# 6-1-decompose-routing.R
# Counterfactual routing for accessibility decomposition
#
# Output:
#   decompose/networks/{base}/district_{code}/
#   decompose/routing/ttm_{mode}_{base}net_nb{target}_{code}.parquet
#
# Usage: Rscript scripts/6-1-decompose-routing.R [MAX_DISTRICTS]
#
# Run from the repository root. Machine-specific values are environment variables:
#   ACCESS_DECLINE_ROOT       project root (default: current directory)
#   ACCESS_DECLINE_BASE_YEARS comma-separated base years (default: 2021,2022,2023)
#   ACCESS_DECLINE_THREADS    R5 threads (default: 4)
#   ACCESS_DECLINE_JVM_HEAP   Java heap flag (default: -Xmx10G)
# JAVA_HOME must point to a Java 21 installation before this script starts.

# === Machine Config ===========================================================

PROJECT_ROOT <- normalizePath(
  Sys.getenv("ACCESS_DECLINE_ROOT", unset = "."),
  winslash = "/", mustWork = TRUE
)
if (!file.exists(file.path(PROJECT_ROOT, ".gitignore"))) {
  stop("Run from the repository root or set ACCESS_DECLINE_ROOT.")
}

JAVA_HOME_PATH <- Sys.getenv("JAVA_HOME", unset = "")
if (!nzchar(JAVA_HOME_PATH) || !dir.exists(JAVA_HOME_PATH)) {
  stop("JAVA_HOME must point to an existing Java 21 installation.")
}

BASE_YEARS <- as.integer(strsplit(
  Sys.getenv("ACCESS_DECLINE_BASE_YEARS", unset = "2021,2022,2023"),
  ",", fixed = TRUE
)[[1]])
if (length(BASE_YEARS) == 0L || anyNA(BASE_YEARS) ||
    any(!BASE_YEARS %in% c(2021L, 2022L, 2023L))) {
  stop("ACCESS_DECLINE_BASE_YEARS must contain only 2021, 2022, and/or 2023.")
}

N_THREADS <- suppressWarnings(as.integer(
  Sys.getenv("ACCESS_DECLINE_THREADS", unset = "4")
))
if (is.na(N_THREADS) || N_THREADS < 1L) {
  stop("ACCESS_DECLINE_THREADS must be a positive integer.")
}

JVM_HEAP <- Sys.getenv("ACCESS_DECLINE_JVM_HEAP", unset = "-Xmx10G")

# ==============================================================================

args <- commandArgs(trailingOnly = TRUE)
MAX_DISTRICTS <- if (length(args) >= 1) as.integer(args[1]) else 9999L

Sys.setenv(JAVA_HOME = JAVA_HOME_PATH)
old_path <- Sys.getenv("PATH")
Sys.setenv(PATH = paste(file.path(Sys.getenv("JAVA_HOME"), "bin"), old_path, sep = .Platform$path.sep))
rm(old_path)
options(java.parameters = c(JVM_HEAP, "-XX:+UseG1GC"))

suppressMessages({
  library(tidyverse); library(sf); library(glue); library(fs)
  library(tidytransit); library(arrow); library(r5r); library(jsonlite)
  library(DBI)
})

DATA_ROOT     <- path(PROJECT_ROOT, "data")
DECOMPOSE_DIR <- path(PROJECT_ROOT, "workflows/results/decompose")
OUT_DIR       <- path(DECOMPOSE_DIR, "routing")
dir_create(OUT_DIR)
source(path(PROJECT_ROOT, "scripts", "_bank_locations.R"), local = FALSE)

TARGET_CRS     <- 5186
WGS84          <- 4326
BUFFER_DIST_KM <- 25.0
BANK_PERIOD    <- "h2"
ALL_YEARS      <- c(2021, 2022, 2023)

YEAR_PAIRS <- expand.grid(base_year = BASE_YEARS, target_year = ALL_YEARS,
                          stringsAsFactors = FALSE) |>
  filter(base_year != target_year) |>
  arrange(match(base_year, BASE_YEARS))

MODE_PARAMS <- list(
  CAR = list(max_trip_duration = 45, time_slots = "12:00:00"),
  TRANSIT = list(
    max_trip_duration = 60, max_walk_time = 30, time_window = 60,
    time_slots = c("07:00:00", "09:00:00", "12:00:00",
                   "14:00:00", "17:00:00", "19:00:00")
  )
)

IS_WINDOWS <- Sys.info()["sysname"] == "Windows"
FAIL_LOG   <- path(DECOMPOSE_DIR, "failed_districts.log")
LOG_FILE   <- path(DECOMPOSE_DIR, "routing.log")

# === Helpers ==================================================================

log_msg <- function(fmt, ...) {
  msg <- sprintf("[%s] %s", format(Sys.time(), "%H:%M:%S"), sprintf(fmt, ...))
  message(msg)
  cat(msg, "\n", file = LOG_FILE, append = TRUE, sep = "")
}

win_to_wsl <- function(p) {
  p <- gsub("\\\\", "/", as.character(p))
  paste0("/mnt/", tolower(substr(p, 1, 1)), "/", substr(p, 4, nchar(p)))
}

safe_write_parquet <- function(df, final_path) {
  tmp <- tempfile(tmpdir = dirname(final_path), fileext = ".parquet")
  write_parquet(df, tmp, compression = "snappy")
  if (file_exists(final_path)) file_delete(final_path)
  file_move(tmp, final_path)
}

clean_r5_cache <- function(dir) {
  stale <- dir_ls(dir, regexp = "\\.(dat|dat\\.p|mapdb|mapdb\\.p)$")
  if (length(stale) > 0) file_delete(stale)
}

format_eta <- function(mins) {
  if (mins >= 60) sprintf("%.0fh%02.0fm", mins %/% 60, mins %% 60)
  else sprintf("%.0fm", mins)
}

ensure_renumbered <- function(base_year) {
  yr_short <- substr(as.character(base_year), 3, 4)
  source <- path(DATA_ROOT, "osm/prep", glue("korea-{yr_short}0101.osm.pbf"))
  renum  <- path(DATA_ROOT, "osm/prep", glue("korea-{yr_short}0101-renum.osm.pbf"))

  if (file_exists(renum)) return(renum)

  log_msg("Renumbering OSM %s (one-time)...", basename(source))
  if (IS_WINDOWS) {
    cmd <- glue("wsl osmium renumber '{win_to_wsl(source)}' -o '{win_to_wsl(renum)}'")
  } else {
    cmd <- glue("osmium renumber '{source}' -o '{renum}'")
  }
  ret <- system(cmd)
  if (ret != 0) stop("osmium renumber failed for ", basename(source))
  log_msg("  Renumbered: %s", basename(renum))
  renum
}

cat("\n")
message(strrep("=", 60))
message(glue("DECOMPOSE ROUTING"))
message(glue("  {nrow(YEAR_PAIRS)} year-pairs | {N_THREADS} threads | {JVM_HEAP}"))
message(glue("  max {MAX_DISTRICTS} districts"))
message(strrep("=", 60))

# === Load Spatial Data ========================================================

log_msg("Loading spatial data...")

districts_sf <- st_read(path(DATA_ROOT, "census/sgg_bnd.gpkg"), quiet = TRUE) |>
  st_transform(WGS84)

census_centroids <- st_read(path(DATA_ROOT, "census/oa_bnd.gpkg"), quiet = TRUE) |>
  select(id = TOT_REG_CD, district_code = SIGUNGU_CD) |>
  st_transform(TARGET_CRS) |> st_centroid() |> st_transform(WGS84)

buffered_districts <- districts_sf |>
  st_transform(TARGET_CRS) |>
  st_buffer(BUFFER_DIST_KM * 1000) |>
  st_transform(WGS84) |>
  select(SIGUNGU_CD)

log_msg("  %d districts, %d census tracts",
        nrow(districts_sf), nrow(census_centroids))

# === Bank Matching ============================================================

log_msg("Matching banks...")

bank_raw <- read_canonical_bank_data(DATA_ROOT)

make_bank_sf <- function(ref) {
  make_bank_locations(bank_raw, ref) |>
    st_as_sf(coords = c("longitude", "latitude"), crs = WGS84) |>
    select(location_id, bank_name)
}

banks_by_year <- list()
for (yr in ALL_YEARS) {
  ref_str <- paste0(yr - 1, BANK_PERIOD)
  banks_by_year[[as.character(yr)]] <- make_bank_sf(ref_str)
  log_msg("  %d: %d banks", yr, nrow(banks_by_year[[as.character(yr)]]))
}

diff_banks <- list()
for (i in seq_len(nrow(YEAR_PAIRS))) {
  by <- YEAR_PAIRS$base_year[i]
  ty <- YEAR_PAIRS$target_year[i]
  base_location_ids <- banks_by_year[[as.character(by)]]$location_id
  target_sf <- banks_by_year[[as.character(ty)]]
  diff <- target_sf[!target_sf$location_id %in% base_location_ids, ] |>
    select(location_id, bank_name)
  pair_key <- glue("{by}_{ty}")
  diff_banks[[pair_key]] <- diff
  log_msg("  %d->%d: %d differential", by, ty, nrow(diff))
}

# === Main Loop ================================================================

for (by in unique(YEAR_PAIRS$base_year)) {
  target_years <- YEAR_PAIRS$target_year[YEAR_PAIRS$base_year == by]

  cat("\n")
  message(strrep("-", 60))
  log_msg("BASE %d  (targets: %s)", by, paste(target_years, collapse = ", "))
  message(strrep("-", 60))

  data_dir <- path(DECOMPOSE_DIR, "networks", as.character(by))
  dir_create(data_dir)

  all_diff <- bind_rows(lapply(target_years, function(ty) {
    diff_banks[[glue("{by}_{ty}")]] |> mutate(target_year = ty)
  }))

  if (nrow(all_diff) == 0) {
    log_msg("  No differential banks -- skipping")
    next
  }

  affected <- suppressMessages(
    st_join(all_diff, buffered_districts, join = st_intersects)
  ) |>
    st_drop_geometry() |>
    pull(SIGUNGU_CD) |>
    unique() |>
    as.character()

  log_msg("  %d affected districts", length(affected))

  # --- Batch OSM clipping ---
  need_osm <- affected[
    !file_exists(path(data_dir, glue("district_{affected}"), "network.osm.pbf"))
  ]

  if (length(need_osm) > 0) {
    log_msg("OSM batch extract: %d districts...", length(need_osm))

    source_pbf <- ensure_renumbered(by)

    extracts <- lapply(need_osm, function(dc) {
      dir_create(path(data_dir, glue("district_{dc}")))
      bbox <- buffered_districts |> filter(SIGUNGU_CD == dc) |> st_bbox()
      list(
        output = glue("district_{dc}/network.osm.pbf"),
        bbox = unname(as.numeric(bbox[c("xmin", "ymin", "xmax", "ymax")]))
      )
    })

    config_file <- path(data_dir, "osmium_config.json")
    if (IS_WINDOWS) {
      config <- list(
        directory = paste0(win_to_wsl(data_dir), "/"),
        extracts = extracts
      )
      write_json(config, config_file, auto_unbox = TRUE, pretty = TRUE)
      cmd <- glue(
        "wsl osmium extract -s simple ",
        "-c '{win_to_wsl(config_file)}' ",
        "'{win_to_wsl(source_pbf)}' --overwrite"
      )
    } else {
      config <- list(
        directory = paste0(as.character(data_dir), "/"),
        extracts = extracts
      )
      write_json(config, config_file, auto_unbox = TRUE, pretty = TRUE)
      cmd <- glue(
        "osmium extract -s simple ",
        "-c '{config_file}' '{source_pbf}' --overwrite"
      )
    }

    t_osm <- Sys.time()
    ret <- system(cmd)
    osm_secs <- round(as.numeric(difftime(Sys.time(), t_osm, units = "secs")))
    if (ret != 0) log_msg("  WARNING: osmium exit code %d", ret)
    log_msg("  OSM done: %d districts in %ds", length(need_osm), osm_secs)
  }

  # --- GTFS clipping ---
  need_gtfs <- affected[
    !file_exists(path(data_dir, glue("district_{affected}"), "network.zip"))
  ]

  if (length(need_gtfs) > 0) {
    log_msg("GTFS clipping: %d districts...", length(need_gtfs))
    t_gtfs <- Sys.time()

    gtfs_full <- suppressMessages(
      read_gtfs(path(DATA_ROOT, "gtfs/prep", glue("gtfs_{by}.zip")))
    )

    for (j in seq_along(need_gtfs)) {
      dc <- need_gtfs[j]
      district_dir <- path(data_dir, glue("district_{dc}"))
      dir_create(district_dir)

      district_bbox <- buffered_districts |>
        filter(SIGUNGU_CD == dc) |>
        st_bbox()
      gf <- filter_feed_by_area(gtfs_full, district_bbox)
      suppressMessages(write_gtfs(gf, path(district_dir, "network.zip")))

      if (j %% 50 == 0 || j == length(need_gtfs))
        log_msg("    GTFS %d/%d", j, length(need_gtfs))
    }

    rm(gtfs_full)
    gc(verbose = FALSE)

    gtfs_secs <- round(as.numeric(difftime(Sys.time(), t_gtfs, units = "secs")))
    log_msg("  GTFS done: %d districts in %ds", length(need_gtfs), gtfs_secs)
  }

  # --- Routing ----------------------------------------------------------------
  expected_files <- function(dc) {
    as.vector(outer(
      c("car", "transit"), target_years,
      \(m, y) path(OUT_DIR, glue("ttm_{m}_{by}net_nb{y}_{dc}.parquet"))
    ))
  }
  is_valid_existing <- function(file, origin_ids, destination_ids) {
    if (!file_exists(file)) return(FALSE)
    endpoints <- tryCatch(
      read_parquet(file, col_select = c(from_id, to_id)),
      error = function(e) NULL
    )
    if (is.null(endpoints)) return(FALSE)
    normalized <- tryCatch(
      normalize_ttm_orientation(
        endpoints, origin_ids, destination_ids,
        context = paste("existing", path_file(file))
      ),
      error = function(e) NULL
    )
    !is.null(normalized) &&
      identical(as.character(endpoints$from_id), normalized$from_id) &&
      identical(as.character(endpoints$to_id), normalized$to_id)
  }
  is_done <- function(dc) {
    files <- expected_files(dc)
    if (!all(file_exists(files))) return(FALSE)
    origins <- census_centroids |> filter(district_code == dc)
    origin_ids <- origins |> st_drop_geometry() |> pull(id)
    buffer <- st_bbox(
      st_buffer(st_transform(origins, TARGET_CRS), BUFFER_DIST_KM * 1000)
    ) |> st_as_sfc() |> st_transform(WGS84)
    for (ty in target_years) {
      destination_ids <- suppressWarnings(
        st_crop(diff_banks[[glue("{by}_{ty}")]], buffer)
      ) |> st_drop_geometry() |> pull(location_id)
      pair_files <- path(
        OUT_DIR,
        glue("ttm_{c('car', 'transit')}_{by}net_nb{ty}_{dc}.parquet")
      )
      valid <- vapply(
        pair_files, is_valid_existing, logical(1),
        origin_ids = origin_ids, destination_ids = destination_ids
      )
      if (!all(valid)) {
        stop("Existing TTM set has wrong pair IDs or schema for district ", dc)
      }
    }
    TRUE
  }

  todo <- affected[!vapply(affected, is_done, logical(1))]
  n_done_prev <- length(affected) - length(todo)
  log_msg("%d districts: %d done, %d remaining",
          length(affected), n_done_prev, length(todo))

  todo <- head(todo, MAX_DISTRICTS)
  if (length(todo) == 0) next

  analysis_date <- paste0(by, "-03-15")
  t_start <- Sys.time()
  completed <- 0
  errors <- character()

  for (i in seq_along(todo)) {
    dc <- todo[i]
    district_name <- districts_sf |> filter(SIGUNGU_CD == dc) |> pull(SIGUNGU_NM)
    t_district <- Sys.time()
    log_msg("  TRYING [%d/%d] %s %s ...",
            n_done_prev + i, length(affected), dc, district_name)

    tryCatch({
      district_dir <- path(data_dir, glue("district_{dc}"))
      osm_file <- path(district_dir, "network.osm.pbf")
      if (!file_exists(osm_file)) {
        log_msg("  [%d/%d] %s %s  SKIP:no-network",
                n_done_prev + i, length(affected), dc, district_name)
        next
      }

      osm_size <- file_size(osm_file)
      if (osm_size < 1000) {
        log_msg("  [%d/%d] %s %s  SKIP:corrupt-osm (%s bytes, deleting)",
                n_done_prev + i, length(affected), dc, district_name,
                scales::comma(osm_size))
        file_delete(osm_file)
        clean_r5_cache(district_dir)
        next
      }

      origins <- census_centroids |>
        filter(district_code == dc) |>
        mutate(id = as.character(id)) |>
        select(id)
      if (nrow(origins) == 0) {
        log_msg("  [%d/%d] %s %s  SKIP:no-origins",
                n_done_prev + i, length(affected), dc, district_name)
        next
      }

      buf <- st_bbox(
        st_buffer(st_transform(origins, TARGET_CRS), BUFFER_DIST_KM * 1000)
      ) |> st_as_sfc() |> st_transform(WGS84)

      clean_r5_cache(district_dir)
      invisible(capture.output(
        net <- build_network(district_dir, verbose = FALSE),
        type = "message"
      ))

      mode_tags <- c()

      for (ty in target_years) {
        nb <- diff_banks[[glue("{by}_{ty}")]]
        dest <- suppressWarnings(st_crop(nb, buf)) |>
          transmute(id = as.character(location_id))
        yr_tag <- substr(as.character(ty), 3, 4)

        for (mode in c("CAR", "TRANSIT")) {
          ml <- tolower(mode)
          out_file <- path(OUT_DIR, glue("ttm_{ml}_{by}net_nb{ty}_{dc}.parquet"))

          if (file_exists(out_file)) {
            existing <- read_parquet(out_file)
            normalized <- normalize_ttm_orientation(
              existing,
              origin_ids = origins |> st_drop_geometry() |> pull(id),
              destination_ids = dest |> st_drop_geometry() |> pull(id),
              context = glue("existing {mode} {by} network/{ty} banks district {dc}")
            )
            if (!identical(as.character(existing$from_id), normalized$from_id) ||
                !identical(as.character(existing$to_id), normalized$to_id)) {
              stop("Existing TTM is not already normalized: ", out_file)
            }
            mode_tags <- c(mode_tags, glue("{ml}{yr_tag}:skip"))
            next
          }

          if (nrow(dest) == 0) {
            safe_write_parquet(
              tibble(from_id = character(), to_id = character(),
                     travel_time_p50 = integer(), departure_time = character()),
              out_file)
            mode_tags <- c(mode_tags, glue("{ml}{yr_tag}:0dest"))
            next
          }

          p <- MODE_PARAMS[[mode]]
          ttm <- {
            if (mode == "CAR") {
              travel_time_matrix(
                net, origins = origins, destinations = dest, mode = "CAR",
                departure_datetime = as.POSIXct(paste(analysis_date, "12:00:00")),
                max_trip_duration = p$max_trip_duration,
                time_window = 1, n_threads = N_THREADS,
                verbose = FALSE, progress = FALSE
              ) |> mutate(departure_time = "12")
            } else {
              bind_rows(lapply(p$time_slots, function(ts) {
                travel_time_matrix(
                  net, origins = origins, destinations = dest,
                  mode = c("WALK", "TRANSIT"),
                  departure_datetime = as.POSIXct(paste(analysis_date, ts)),
                  time_window = p$time_window,
                  max_trip_duration = p$max_trip_duration,
                  max_walk_time = p$max_walk_time,
                  n_threads = N_THREADS,
                  verbose = FALSE, progress = FALSE
                ) |> mutate(departure_time = substr(ts, 1, 2))
              }))
            }
          }

          if (is.null(ttm) || nrow(ttm) == 0) {
            safe_write_parquet(
              tibble(from_id = character(), to_id = character(),
                     travel_time_p50 = integer(), departure_time = character()),
              out_file)
            mode_tags <- c(mode_tags, glue("{ml}{yr_tag}:empty"))
          } else {
            nr <- nrow(ttm)
            ttm <- normalize_ttm_orientation(
              ttm,
              origin_ids = origins |> st_drop_geometry() |> pull(id),
              destination_ids = dest |> st_drop_geometry() |> pull(id),
              context = glue("{mode} {by} network/{ty} banks district {dc}")
            )
            ttm <- ttm |> transmute(
              from_id = as.character(from_id), to_id = as.character(to_id),
              travel_time_p50, departure_time)
            safe_write_parquet(ttm, out_file)
            mode_tags <- c(mode_tags, glue("{ml}{yr_tag}:{scales::comma(nr)}"))
          }
        }
      }

      invisible(capture.output(stop_r5(net), type = "message"))
      suppressMessages(rJava::.jgc(R.gc = TRUE))
      clean_r5_cache(district_dir)
      completed <- completed + 1

      secs <- round(as.numeric(difftime(Sys.time(), t_district, units = "secs")))
      avg_secs <- as.numeric(difftime(Sys.time(), t_start, units = "secs")) / completed
      eta_mins <- avg_secs * (length(todo) - i) / 60
      log_msg("  [%d/%d] %s %s  %s  %ds  ETA:%s",
              n_done_prev + i, length(affected), dc, district_name,
              paste(mode_tags, collapse = "  "), secs, format_eta(eta_mins))

    }, error = function(e) {
      errors <<- c(errors, dc)
      log_msg("  [%d/%d] %s %s  ERROR: %s",
              n_done_prev + i, length(affected), dc, district_name,
              conditionMessage(e))
      cat(sprintf("%s base=%d %s %s: %s\n",
                  format(Sys.time()), by, dc, district_name, conditionMessage(e)),
          file = FAIL_LOG, append = TRUE)
      tryCatch(
        invisible(capture.output(stop_r5(net), type = "message")),
        error = function(e2) NULL)
      district_dir <- path(data_dir, glue("district_{dc}"))
      clean_r5_cache(district_dir)
      osm_file <- path(district_dir, "network.osm.pbf")
      if (file_exists(osm_file)) {
        file_delete(osm_file)
        log_msg("    Deleted corrupt OSM for %s (will re-extract on next run)", dc)
      }
    })

    gc(verbose = FALSE)
  }

  elapsed <- round(as.numeric(difftime(Sys.time(), t_start, units = "mins")), 1)
  log_msg("Base %d: %d completed, %d errors, %.1f min total",
          by, completed, length(errors), elapsed)
    if (length(errors) > 0) log_msg("  Failed: %s", paste(errors, collapse = ", "))
    if (length(errors) > 0) {
      stop("Routing failed for ", length(errors),
           " district(s); refusing to combine incomplete counterfactual TTMs")
    }
}

# === Combine ==================================================================

cat("\n")
log_msg("Combining parquet files...")

for (by in unique(YEAR_PAIRS$base_year)) {
  target_years <- YEAR_PAIRS$target_year[YEAR_PAIRS$base_year == by]
  for (mode in c("car", "transit")) {
    for (ty in target_years) {
      inventory <- dir_ls(OUT_DIR, type = "file")
      file_pattern <- glue(
        "^ttm_{mode}_{by}net_nb{ty}_[0-9]{{5}}\\.parquet$"
      )
      files <- inventory[str_detect(path_file(inventory), file_pattern)]
      expected_files <- c(`2021` = 242L, `2022` = 246L, `2023` = 247L)[as.character(by)]
      if (length(files) != expected_files) {
        stop("Incomplete combine manifest for ", mode, " ", by, "->", ty,
             ": expected ", expected_files, ", found ", length(files))
      }

      pattern  <- path(OUT_DIR, glue("ttm_{mode}_{by}net_nb{ty}_*.parquet"))
      out_file <- path(DECOMPOSE_DIR, glue("ttm_{mode}_{by}net_newbanks_{ty}.parquet"))

      con <- dbConnect(duckdb::duckdb())
      tryCatch({
        dbExecute(con, glue("
          COPY (
            SELECT from_id, to_id, travel_time_p50,
                   COALESCE(departure_time, '12') AS departure_time
            FROM read_parquet('{pattern}')
            WHERE from_id IS NOT NULL
          ) TO '{out_file}' (FORMAT PARQUET, CODEC 'SNAPPY')
        "))
        n <- dbGetQuery(con, glue(
          "SELECT COUNT(*) AS n FROM read_parquet('{out_file}')"))$n
        log_msg("  %s %d->%d: %s rows (%d files)",
                mode, by, ty, scales::comma(n), length(files))
      }, error = function(e) {
        try(dbDisconnect(con, shutdown = TRUE), silent = TRUE)
        stop(glue("Error combining {mode} {by}->{ty}: {e$message}"), call. = FALSE)
      })
      dbDisconnect(con, shutdown = TRUE)
    }
  }
}

cat("\n")
message(strrep("=", 60))
log_msg("DONE")
log_msg("Next: Rscript scripts/6-2-decompose-analyze.R")
message(strrep("=", 60))
