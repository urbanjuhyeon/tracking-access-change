# 1. Setup & Configuration -----------------------------------------------------

pacman::p_load(
  tidyverse, sf, glue, fs, tictoc,
  tidytransit,
  future, furrr, progressr,
  arrow, duckdb
)

tic("Total Workflow")

## 1.1. Environment Configuration ----

PROJECT_ROOT <- normalizePath(
  Sys.getenv("ACCESS_DECLINE_ROOT", unset = "."),
  winslash = "/", mustWork = TRUE
)
if (!file.exists(file.path(PROJECT_ROOT, ".gitignore"))) {
  stop("Run from the repository root or set ACCESS_DECLINE_ROOT.")
}

DATA_ROOT <- file.path(PROJECT_ROOT, "data")
OS_TYPE <- if (Sys.info()["sysname"] == "Windows") "windows" else "linux"
TOTAL_RAM_GB <- suppressWarnings(as.numeric(Sys.getenv(
  "ACCESS_DECLINE_RAM_GB",
  unset = if (OS_TYPE == "windows") "32" else "64"
)))
SYSTEM_BUFFER <- suppressWarnings(as.numeric(Sys.getenv(
  "ACCESS_DECLINE_SYSTEM_BUFFER_GB",
  unset = if (OS_TYPE == "windows") "8" else "6"
)))
if (is.na(TOTAL_RAM_GB) || is.na(SYSTEM_BUFFER) ||
    TOTAL_RAM_GB <= SYSTEM_BUFFER) {
  stop("Invalid ACCESS_DECLINE_RAM_GB or ACCESS_DECLINE_SYSTEM_BUFFER_GB.")
}

source(path(PROJECT_ROOT, "scripts", "_bank_locations.R"), local = FALSE)

# Memory allocation
TARGET_MEM_PER_WORKER_GB <- 8
AVAILABLE_RAM <- TOTAL_RAM_GB - SYSTEM_BUFFER
N_WORKERS <- floor(AVAILABLE_RAM / TARGET_MEM_PER_WORKER_GB)

# Workflow directories
WORKFLOW_DIR <- path(PROJECT_ROOT, "workflows")
DIR_DATA_BASE <- path(WORKFLOW_DIR, "data")
DIR_RESULTS_BASE <- path(WORKFLOW_DIR, "results")

# Create output directories
DIR_BY_DISTRICT <- path(DIR_RESULTS_BASE, "by_district")
dir_create(DIR_BY_DISTRICT)

message("=" |> strrep(60))
message("MULTIMODAL TRAVEL TIME ROUTING")
message("=" |> strrep(60))
message(glue("System: {OS_TYPE} | RAM: {TOTAL_RAM_GB}GB | Workers: {N_WORKERS}"))
message(glue("Output: {DIR_RESULTS_BASE}"))

## 1.2. Parameters ----

TARGET_CRS <- 5186
WGS84_CRS <- 4326
BUFFER_DIST_KM <- 25.0

# =============================================================================
# EXECUTION SETTINGS - Modify these to control what gets processed
# =============================================================================

TARGET_YEARS <- c(2021, 2022, 2023, 2024)
TARGET_MODES <- c("CAR", "TRANSIT", "WALK", "BICYCLE")  # Options: CAR, TRANSIT, WALK, BICYCLE

IS_SAMPLE <- FALSE      # FALSE = all districts, or integer for testing
AUTO_SHUTDOWN <- TRUE   # Shutdown AWS after completion
CLIP_DONE <- TRUE       # Skip network clipping if already done

# =============================================================================

# Mode-specific parameters
MODE_PARAMS <- list(
  CAR = list(
    max_trip_duration = 45,
    time_slots = "12:00:00"  # Single time slot
  ),
  TRANSIT = list(
    max_trip_duration = 60,
    max_walk_time = 30,
    time_window = 60,
    time_slots = c("07:00:00", "09:00:00", "12:00:00",
                   "14:00:00", "17:00:00", "19:00:00")
  ),
  WALK = list(
    max_trip_duration = 60,
    speed = 3.6  # km/h
  ),
  BICYCLE = list(
    max_trip_duration = 60,
    speed = 12,   # km/h
    max_lts = 2   # Level of Traffic Stress: residential streets, bike lanes (R5R default)
  )
)

# Bank data reference period
BANK_PERIOD <- "h2"


# 2. Load Static Data ----------------------------------------------------------

message("\n[2] Loading static data...")

districts_sf <- st_read(path(DATA_ROOT, "census/sgg_bnd.gpkg"), quiet = TRUE) |>
  st_transform(WGS84_CRS)

census_sf <- st_read(path(DATA_ROOT, "census/oa_bnd.gpkg"), quiet = TRUE) |>
  select(id = TOT_REG_CD, district_code = SIGUNGU_CD)

census_centroids <- census_sf |>
  st_transform(TARGET_CRS) |>
  st_centroid() |>
  st_transform(WGS84_CRS)

message(glue("  Districts: {nrow(districts_sf)}"))
message(glue("  Census units: {nrow(census_centroids)}"))


# 3. Helper Functions ----------------------------------------------------------

## 3.1. Network Slicing Function ----

prep_network_subset <- function(
    district_code, target_year, gtfs, stops_sf, data_dir) {

  t_start <- Sys.time()

  library(fs)
  library(glue)
  library(dplyr)
  library(sf)
  library(tidytransit)

  if (Sys.info()["sysname"] == "Linux") {
    Sys.setenv(JAVA_HOME = "/usr/lib/jvm/java-21-openjdk-amd64")
  }

  district_dir <- path(data_dir, glue("district_{district_code}"))

  osm_file  <- path(district_dir, "network.osm.pbf")
  gtfs_file <- path(district_dir, "network.zip")

  if (file_exists(osm_file) && file_exists(gtfs_file)) {
    return(0)
  }

  if (dir_exists(district_dir)) {
    dir_delete(district_dir)
  }
  dir_create(district_dir)

  # Bbox setup
  district_poly <- districts_sf |> filter(SIGUNGU_CD == district_code)
  bbox_poly <- district_poly |>
    st_transform(TARGET_CRS) |>
    st_buffer(BUFFER_DIST_KM * 1000) |>
    st_transform(WGS84_CRS)
  bbox <- st_bbox(bbox_poly)

  # Slice OSM
  bbox_str <- paste(bbox, collapse = ",")

  if (OS_TYPE == "windows") {
    source_win <- path(DATA_ROOT, "osm/prep", glue("korea-{substr(target_year, 3, 4)}0101.osm.pbf"))
    out_win    <- path(district_dir, "network.osm.pbf")

    wsl_source <- paste0("/mnt/", tolower(substr(source_win, 1, 1)), "/", substr(source_win, 4, nchar(source_win)))
    wsl_out    <- paste0("/mnt/", tolower(substr(out_win, 1, 1)), "/", substr(out_win, 4, nchar(out_win)))

    cmd <- glue("wsl osmium extract -b {bbox_str} '{wsl_source}' -o '{wsl_out}' --overwrite")
  } else {
    source_path <- path(DATA_ROOT, "osm/prep", glue("korea-{substr(target_year, 3, 4)}0101.osm.pbf"))
    out_path    <- path(district_dir, "network.osm.pbf")
    cmd <- glue("osmium extract -b {bbox_str} '{source_path}' -o '{out_path}' --overwrite")
  }

  system(cmd, ignore.stdout = TRUE)

  # Filter GTFS
  bbox_coords <- st_bbox(bbox_poly)
  stops_in_bbox <- stops_sf |>
    filter(
      between(st_coordinates(geometry)[,1], bbox_coords["xmin"], bbox_coords["xmax"]),
      between(st_coordinates(geometry)[,2], bbox_coords["ymin"], bbox_coords["ymax"])
    ) |>
    st_filter(bbox_poly)

  stop_ids <- stops_in_bbox$stop_id

  gtfs_filtered <- list()
  gtfs_filtered$stops <- gtfs$stops |> filter(stop_id %in% stop_ids)
  gtfs_filtered$stop_times <- gtfs$stop_times |> filter(stop_id %in% stop_ids)

  trip_ids <- unique(gtfs_filtered$stop_times$trip_id)
  gtfs_filtered$trips <- gtfs$trips |> filter(trip_id %in% trip_ids)

  route_ids <- unique(gtfs_filtered$trips$route_id)
  gtfs_filtered$routes <- gtfs$routes |> filter(route_id %in% route_ids)

  if (!is.null(gtfs$calendar)) {
    service_ids <- unique(gtfs_filtered$trips$service_id)
    gtfs_filtered$calendar <- gtfs$calendar |> filter(service_id %in% service_ids)
  }
  if (!is.null(gtfs$calendar_dates)) {
    service_ids <- unique(gtfs_filtered$trips$service_id)
    gtfs_filtered$calendar_dates <- gtfs$calendar_dates |> filter(service_id %in% service_ids)
  }
  if (!is.null(gtfs$agency)) {
    agency_ids <- unique(gtfs_filtered$routes$agency_id)
    gtfs_filtered$agency <- gtfs$agency |> filter(agency_id %in% agency_ids)
  }
  if (!is.null(gtfs$shapes)) {
    shape_ids <- unique(gtfs_filtered$trips$shape_id)
    gtfs_filtered$shapes <- gtfs$shapes |> filter(shape_id %in% shape_ids)
  }

  class(gtfs_filtered) <- class(gtfs)
  write_gtfs(gtfs_filtered, path(district_dir, "network.zip"))

  gc()

  t_end <- Sys.time()
  elapsed <- as.numeric(difftime(t_end, t_start, units = "secs"))

  return(elapsed)
}


## 3.2. Unified Routing Function ----

calc_ttm_single_mode <- function(
    district_code, target_year, target_mode, bank_sf, data_dir, result_dir) {

  library(glue)
  library(dplyr)
  library(sf)
  library(fs)
  library(arrow)

  mode_lower <- tolower(target_mode)
  out_file <- path(result_dir, glue("ttm_{mode_lower}_{target_year}_{district_code}.parquet"))

  if (file_exists(out_file)) {
    return(list(status = "skipped", elapsed = 0))
  }

  tryCatch({
    t_start <- Sys.time()

    if (Sys.info()["sysname"] == "Linux") {
      Sys.setenv(JAVA_HOME = "/usr/lib/jvm/java-21-openjdk-amd64")
    }
    options(java.parameters = c(glue("-Xmx{TARGET_MEM_PER_WORKER_GB}G"), "-XX:+UseG1GC"))

    library(r5r)

    # Try new naming first, fallback to old naming
    district_dir <- path(data_dir, glue("district_{district_code}"))
    if (!dir_exists(district_dir)) {
      district_dir <- path(data_dir, glue("sgg_{district_code}"))
    }

    analysis_date <- glue("{target_year}-03-15")

    if (!file_exists(path(district_dir, "network.osm.pbf"))) {
      message(glue("  No network for {district_code}, skipping..."))
      return(list(status = "no_network", elapsed = 0))
    }

    # Origins & Destinations
    origins <- census_centroids |>
      filter(district_code == !!district_code) |>
      mutate(id = as.character(id)) |>
      select(id)

    district_bbox <- st_bbox(st_buffer(st_transform(origins, TARGET_CRS), dist = BUFFER_DIST_KM * 1000))
    destinations <- bank_sf |>
      st_crop(st_transform(st_as_sfc(district_bbox), WGS84_CRS)) |>
      transmute(id = as.character(location_id))

    if (nrow(destinations) == 0) {
      message(glue("  No banks for {district_code}, skipping..."))
      return(list(status = "no_banks", elapsed = 0))
    }

    # Build network
    r5r_network <- build_network(district_dir, verbose = FALSE)

    # Get mode parameters
    params <- MODE_PARAMS[[target_mode]]

    # Route based on mode
    if (target_mode == "CAR") {
      ttm <- travel_time_matrix(
        r5r_network,
        origins = origins,
        destinations = destinations,
        mode = "CAR",
        departure_datetime = as.POSIXct(paste(analysis_date, "12:00:00")),
        max_trip_duration = params$max_trip_duration,
        time_window = 1,
        n_threads = 1,
        verbose = FALSE
      )

    } else if (target_mode == "TRANSIT") {
      # Multi-temporal routing for transit
      results_by_time <- list()

      for (time_str in params$time_slots) {
        current_time <- as.POSIXct(paste(analysis_date, time_str))
        time_label <- substr(time_str, 1, 2)

        ttm_slot <- travel_time_matrix(
          r5r_network,
          origins = origins,
          destinations = destinations,
          mode = c("WALK", "TRANSIT"),
          departure_datetime = current_time,
          time_window = params$time_window,
          max_trip_duration = params$max_trip_duration,
          max_walk_time = params$max_walk_time,
          n_threads = 1,
          verbose = FALSE
        ) |> mutate(departure_time = time_label)

        results_by_time[[time_label]] <- ttm_slot
      }

      ttm <- bind_rows(results_by_time)

    } else if (target_mode == "WALK") {
      ttm <- travel_time_matrix(
        r5r_network,
        origins = origins,
        destinations = destinations,
        mode = "WALK",
        departure_datetime = as.POSIXct(paste(analysis_date, "12:00:00")),
        max_trip_duration = params$max_trip_duration,
        walk_speed = params$speed,
        time_window = 1,
        n_threads = 1,
        verbose = FALSE
      )

    } else if (target_mode == "BICYCLE") {
      ttm <- travel_time_matrix(
        r5r_network,
        origins = origins,
        destinations = destinations,
        mode = "BICYCLE",
        departure_datetime = as.POSIXct(paste(analysis_date, "12:00:00")),
        max_trip_duration = params$max_trip_duration,
        bike_speed = params$speed,
        max_lts = params$max_lts,
        time_window = 1,
        n_threads = 1,
        verbose = FALSE
      )
    }

    stop_r5(r5r_network)
    rJava::.jgc(R.gc = TRUE)

    # Add metadata
    result <- ttm |>
      mutate(
        mode = mode_lower,
        year = target_year
      )

    # Add departure_time for non-transit modes (transit already has it from Line 336)
    if (target_mode != "TRANSIT" && !"departure_time" %in% names(result)) {
      result <- result |> mutate(departure_time = "12")
    }

    # r5r can reverse individual rows for bidirectional modes. Validate every
    # endpoint and persist one contract: census origin -> stable bank location.
    result <- normalize_ttm_orientation(
      result,
      origin_ids = origins |> st_drop_geometry() |> pull(id),
      destination_ids = destinations |> st_drop_geometry() |> pull(id),
      context = glue("{target_mode} {target_year} district {district_code}")
    )

    # Save
    write_parquet(result, out_file, compression = "snappy")

    t_end <- Sys.time()
    elapsed <- as.numeric(difftime(t_end, t_start, units = "secs"))

    district_name <- districts_sf |> filter(SIGUNGU_CD == district_code) |> pull(SIGUNGU_NM)
    message(glue("  {target_mode} {target_year} {district_name}: {round(elapsed, 1)}s"))

    return(list(status = "completed", elapsed = elapsed))

  }, error = function(e) {
    message(glue("  Error {district_code}: {e$message}"))
    return(list(status = "error", elapsed = 0, error = e$message))
  })
}


# 4. Main Execution ------------------------------------------------------------

message("\n[4] Starting routing...")

## 4.1. Network Preparation (if needed) ----

if (!CLIP_DONE) {
  message("\n[4.1] Network Preparation...")

  # Only needed for CAR/TRANSIT modes (they require GTFS)
  if (any(c("CAR", "TRANSIT") %in% TARGET_MODES)) {

    for (target_year in TARGET_YEARS) {
      message(glue("\n  Clipping networks for {target_year}..."))

      dir_data_year <- path(DIR_DATA_BASE, as.character(target_year))
      dir_create(dir_data_year)

      # Load GTFS
      gtfs_path <- path(DATA_ROOT, "gtfs/prep", glue("gtfs_{target_year}.zip"))
      if (!file_exists(gtfs_path)) {
        stop(glue("GTFS file not found: {gtfs_path}"))
      }

      gtfs_full <- read_gtfs(gtfs_path)
      stops_all_sf <- st_as_sf(gtfs_full$stops, coords = c("stop_lon", "stop_lat"), crs = WGS84_CRS)

      # Get district codes
      if (isFALSE(IS_SAMPLE)) {
        district_codes <- districts_sf |> st_drop_geometry() |> pull(SIGUNGU_CD) |> as.character()
      } else {
        set.seed(123)
        district_codes <- districts_sf |> st_drop_geometry() |> slice_sample(n = IS_SAMPLE) |> pull(SIGUNGU_CD) |> as.character()
      }

      # Parallel clipping
      plan(multisession, workers = N_WORKERS)

      future_walk(
        district_codes,
        function(dc) prep_network_subset(dc, target_year, gtfs_full, stops_all_sf, dir_data_year),
        .options = furrr_options(
          seed = TRUE,
          globals = c("prep_network_subset", "districts_sf", "TARGET_CRS", "WGS84_CRS",
                      "BUFFER_DIST_KM", "OS_TYPE", "PROJECT_ROOT", "target_year",
                      "gtfs_full", "stops_all_sf", "dir_data_year")
        ),
        .progress = TRUE
      )

      plan(sequential)
      rm(gtfs_full, stops_all_sf)
      gc()
    }
  }
}

## 4.2. Build Task List ----

message("\n[4.2] Building task list...")

# Get district codes
if (isFALSE(IS_SAMPLE)) {
  district_codes <- districts_sf |> st_drop_geometry() |> pull(SIGUNGU_CD) |> as.character()
} else {
  set.seed(123)
  district_codes <- districts_sf |> st_drop_geometry() |> slice_sample(n = IS_SAMPLE) |> pull(SIGUNGU_CD) |> as.character()
}

# Build task list: year x mode x district
task_list <- expand_grid(
  year = TARGET_YEARS,
  mode = TARGET_MODES,
  district_code = district_codes
)

message(glue("  Total tasks: {nrow(task_list)}"))
message(glue("  Years: {paste(TARGET_YEARS, collapse = ', ')}"))
message(glue("  Modes: {paste(TARGET_MODES, collapse = ', ')}"))
message(glue("  Districts: {length(district_codes)}"))

## 4.3. Load Bank Data ----

message("\n[4.3] Loading bank data...")

bank_data_by_year <- list()
bank_raw <- read_canonical_bank_data(DATA_ROOT)

for (yr in TARGET_YEARS) {
  bank_year_ref <- yr - 1
  ref_date_str <- paste0(bank_year_ref, BANK_PERIOD)

  bank_sf <- make_bank_locations(bank_raw, ref_date_str) |>
    st_as_sf(coords = c("longitude", "latitude"), crs = WGS84_CRS) |>
    select(location_id)

  bank_data_by_year[[as.character(yr)]] <- bank_sf
  message(glue("  {yr}: {nrow(bank_sf)} banks (from {ref_date_str})"))
}

## 4.4. Create Output Directories ----

for (mode in tolower(TARGET_MODES)) {
  for (yr in TARGET_YEARS) {
    dir_create(path(DIR_BY_DISTRICT, mode, as.character(yr)))
  }
}

## 4.5. Run Parallel Routing ----

message("\n[4.5] Running parallel routing...")

plan(multisession, workers = N_WORKERS)

tic("Parallel Routing")

results <- future_pmap(
  list(
    district_code = task_list$district_code,
    target_year = task_list$year,
    target_mode = task_list$mode
  ),
  function(district_code, target_year, target_mode) {
    library(fs)

    data_dir <- path(DIR_DATA_BASE, as.character(target_year))
    result_dir <- path(DIR_BY_DISTRICT, tolower(target_mode), as.character(target_year))

    bank_sf <- bank_data_by_year[[as.character(target_year)]]

    calc_ttm_single_mode(
      district_code = district_code,
      target_year = target_year,
      target_mode = target_mode,
      bank_sf = bank_sf,
      data_dir = data_dir,
      result_dir = result_dir
    )
  },
  .options = furrr_options(
    seed = TRUE,
    globals = c(
      "calc_ttm_single_mode", "census_centroids", "districts_sf",
      "TARGET_CRS", "WGS84_CRS", "BUFFER_DIST_KM",
      "MODE_PARAMS", "TARGET_MEM_PER_WORKER_GB",
      "DIR_DATA_BASE", "DIR_BY_DISTRICT",
      "bank_data_by_year", "normalize_ttm_orientation"
    )
  ),
  .progress = TRUE
)

failed_tasks <- vapply(
  results,
  function(x) !is.null(x$status) && x$status %in% c("error", "no_network"),
  logical(1)
)
if (any(failed_tasks)) {
  stop(
    sum(failed_tasks),
    " routing task(s) failed or had no network; refusing to combine incomplete TTMs"
  )
}

toc()

plan(sequential)
gc()


# 5. Combine Results -----------------------------------------------------------
#
# Output schema (standardized):
#   from_id, to_id, travel_time_p50, mode, departure_time, year
#
# Notes:
#   - Car/Walk/Bicycle: departure_time = '12' (single time slot)
#   - Transit: departure_time = '07', '09', '12', '14', '17', '19' (6 time slots)
#   - All modes get consistent column order and types
#   - from_id = census tract ID, to_id = stable bank location_id
#   - Every row is validated and normalized independently

message("\n[5] Combining results...")

# Get census IDs for validation (from_id should be census, to_id should be bank)
census_ids <- census_sf |> st_drop_geometry() |> pull(id) |> as.character()

for (mode in tolower(TARGET_MODES)) {
  for (yr in TARGET_YEARS) {
    year_dir <- path(DIR_BY_DISTRICT, mode, as.character(yr))
    out_file <- path(DIR_RESULTS_BASE, glue("ttm_{mode}_{yr}.parquet"))

    if (!dir_exists(year_dir)) next

    parquet_files <- dir_ls(year_dir, glob = "*.parquet")
    if (length(parquet_files) == 0) next

    tic(glue("  Combining {mode} {yr}"))

    con <- dbConnect(duckdb::duckdb())

    tryCatch({
      # Validate all rows, not a sample. This also catches stale numeric bank
      # IDs before they can be mixed into a location_id-based output.
      bank_ids <- bank_data_by_year[[as.character(yr)]] |>
        st_drop_geometry() |>
        pull(location_id) |>
        as.character()
      if (length(intersect(census_ids, bank_ids)) > 0) {
        stop("Census and bank ID namespaces overlap for ", yr)
      }
      dbWriteTable(con, "valid_origins", tibble(id = census_ids), temporary = TRUE)
      dbWriteTable(con, "valid_destinations", tibble(id = bank_ids), temporary = TRUE)

      endpoint_stats <- dbGetQuery(con, glue("
        WITH raw AS (
          SELECT CAST(from_id AS VARCHAR) AS from_id,
                 CAST(to_id AS VARCHAR) AS to_id
          FROM read_parquet('{year_dir}/*.parquet')
        ), classified AS (
          SELECT *,
                 COALESCE(from_id IN (SELECT id FROM valid_origins), FALSE) AS from_origin,
                 COALESCE(to_id IN (SELECT id FROM valid_origins), FALSE) AS to_origin,
                 COALESCE(from_id IN (SELECT id FROM valid_destinations), FALSE) AS from_destination,
                 COALESCE(to_id IN (SELECT id FROM valid_destinations), FALSE) AS to_destination
          FROM raw
        )
        SELECT COUNT(*) AS n,
               COUNT(*) FILTER (WHERE NOT (
                 (from_origin AND to_destination) OR
                 (to_origin AND from_destination)
               )) AS invalid,
               COUNT(*) FILTER (WHERE to_origin AND from_destination) AS reversed
        FROM classified
      "))
      if (endpoint_stats$invalid[[1]] != 0) {
        stop(endpoint_stats$invalid[[1]], " row(s) in ", mode, " ", yr,
             " do not contain exactly one valid census origin and one valid bank destination")
      }

      # Step 2: Normalize each row and combine with standardized schema.
      query <- glue("
        COPY (
          WITH raw AS (
            SELECT *,
                   COALESCE(
                     CAST(from_id AS VARCHAR) IN (SELECT id FROM valid_origins),
                     FALSE
                   ) AS from_origin
            FROM read_parquet('{year_dir}/*.parquet')
          )
          SELECT
            CASE WHEN from_origin THEN CAST(from_id AS VARCHAR)
                 ELSE CAST(to_id AS VARCHAR) END AS from_id,
            CASE WHEN from_origin THEN CAST(to_id AS VARCHAR)
                 ELSE CAST(from_id AS VARCHAR) END AS to_id,
            CAST(travel_time_p50 AS DOUBLE) AS travel_time_p50,
            CAST('{mode}' AS VARCHAR) AS mode,
            CAST(COALESCE(departure_time, '12') AS VARCHAR) AS departure_time,
            CAST({yr} AS INTEGER) AS year
          FROM raw
        ) TO '{out_file}' (FORMAT PARQUET, CODEC 'SNAPPY')
      ")

      dbExecute(con, query)

      # Report file stats
      row_count <- dbGetQuery(con, glue("SELECT COUNT(*) AS n FROM read_parquet('{out_file}')"))$n
      file_size_mb <- round(file_size(out_file) / 1024^2, 1)
      message(glue("  {mode} {yr}: {scales::comma(row_count)} rows, {file_size_mb} MB; {scales::comma(endpoint_stats$reversed[[1]])} row-wise reversals normalized -> {path_file(out_file)}"))

    }, error = function(e) {
      try(dbDisconnect(con, shutdown = TRUE), silent = TRUE)
      stop(glue("Error combining {mode} {yr}: {e$message}"), call. = FALSE)
    })

    dbDisconnect(con, shutdown = TRUE)
    toc()
  }
}


# 6. Summary -------------------------------------------------------------------

message("\n[6] Summary")

toc()  # Total workflow

# Count results
for (mode in tolower(TARGET_MODES)) {
  combined_files <- dir_ls(DIR_RESULTS_BASE, glob = glue("ttm_{mode}_*.parquet"))
  message(glue("  {toupper(mode)}: {length(combined_files)} year files"))
}

message("\nRouting completed!")
message(glue("Results saved to: {DIR_RESULTS_BASE}"))
message("  - by_district/: Individual district files")
message("  - ttm_{mode}_{year}.parquet: Combined files")


# 7. Auto-shutdown (AWS) -------------------------------------------------------

if (OS_TYPE == "linux" && isTRUE(AUTO_SHUTDOWN)) {
  instance_id <- tryCatch(
    system("curl -s http://169.254.169.254/latest/meta-data/instance-id", intern = TRUE),
    error = function(e) "Unknown"
  )

  message(glue("\nAuto-shutdown in 10 minutes... (Instance: {instance_id})"))

  for (i in 10:1) {
    cat(glue("\r   {i} minutes remaining...    "))
    Sys.sleep(60)
  }

  system(glue("aws ec2 stop-instances --instance-ids {instance_id} --region ap-northeast-2"),
         ignore.stdout = TRUE, ignore.stderr = TRUE, wait = FALSE)
  Sys.sleep(2)
  system("sudo shutdown -h now", wait = FALSE)
}
