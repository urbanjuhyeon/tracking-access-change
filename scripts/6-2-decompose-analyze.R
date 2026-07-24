# 6-2-decompose-analyze.R
# Decompose accessibility change: Service effect × Network effect × Interaction
#
# Two counterfactuals per (base_year, target_year) pair:
#   Actual_base   = f(net_base, banks_base)         — from 5-2
#   Actual_target = f(net_target, banks_target)      — from 5-2
#   CF-A          = f(net_base, banks_target)        — surviving (base TTM) + new (6-1 fwd)
#   CF-B          = f(net_target, banks_base)        — surviving (target TTM) + closed (6-1 rev)
#
#   ΔService     = CF-A - Actual_base       (bank changes, network held at base)
#   ΔNetwork     = CF-B - Actual_base       (network changes, banks held at base)
#   Interaction  = ΔTotal - ΔService - ΔNetwork
#   ΔTotal       = Actual_target - Actual_base

pacman::p_load(
  tidyverse, sf, arrow, fs, glue, scales,
  data.table, duckdb
)

# === Config ===================================================================

PROJECT_ROOT <- normalizePath(
  Sys.getenv("ACCESS_DECLINE_ROOT", unset = "."),
  winslash = "/", mustWork = TRUE
)
if (!file.exists(file.path(PROJECT_ROOT, ".gitignore"))) {
  stop("Run from the repository root or set ACCESS_DECLINE_ROOT.")
}
DATA_ROOT     <- path(PROJECT_ROOT, "data")
RESULTS_DIR   <- path(PROJECT_ROOT, "workflows/results")
DECOMPOSE_DIR <- path(RESULTS_DIR, "decompose")
ROUTING_DIR   <- path(DECOMPOSE_DIR, "routing")
OUTPUT_DIR    <- path(PROJECT_ROOT, "workflows/analysis")
dir_create(OUTPUT_DIR)

source(path(PROJECT_ROOT, "scripts/_bank_locations.R"))
assert_location_id_ttm_ready(RESULTS_DIR)

YEARS      <- c(2021, 2022, 2023)
YEAR_PAIRS <- expand.grid(base_year = YEARS, target_year = YEARS,
                          stringsAsFactors = FALSE) |>
  filter(base_year != target_year)

BANK_PERIOD <- "h2"
THRESHOLDS  <- list(car = 10, transit = 30)

message("=" |> strrep(60))
message("DECOMPOSE: Service x Network x Interaction")
message(glue("Year pairs: {nrow(YEAR_PAIRS)}"))
message("=" |> strrep(60))


# === Helper ===================================================================

count_reachable <- function(ttm_source, threshold, all_origins, bank_ids = NULL) {
  if (!is.null(bank_ids) && length(bank_ids) == 0) {
    return(tibble(
      origin_id = as.character(all_origins),
      n_banks = 0
    ))
  }

  con <- dbConnect(duckdb::duckdb())
  on.exit(dbDisconnect(con, shutdown = TRUE))

  id_filter <- if (!is.null(bank_ids) && length(bank_ids) > 0) {
    ids_sql <- paste(sql_quote_location_ids(bank_ids), collapse = ", ")
    glue("AND CAST(to_id AS VARCHAR) IN ({ids_sql})")
  } else {
    ""
  }

  query <- glue("
    WITH filtered AS (
      SELECT from_id, to_id, travel_time_p50, departure_time
      FROM read_parquet('{ttm_source}')
      WHERE from_id IS NOT NULL {id_filter}
    ),
    by_dep AS (
      SELECT CAST(from_id AS VARCHAR) AS origin_id, departure_time,
             SUM(CASE WHEN travel_time_p50 <= {threshold} THEN 1 ELSE 0 END) AS n
      FROM filtered GROUP BY from_id, departure_time
    )
    SELECT origin_id, AVG(n) AS n_banks
    FROM by_dep GROUP BY origin_id
  ")
  result <- dbGetQuery(con, query)

  tibble(origin_id = as.character(all_origins)) |>
    left_join(result, by = "origin_id") |>
    mutate(n_banks = replace_na(n_banks, 0))
}

resolve_routing <- function(net_year, bank_year, mode) {
  pattern <- path(ROUTING_DIR,
                  glue("ttm_{mode}_{net_year}net_nb{bank_year}_*.parquet"))
  expected_districts <- c(`2021` = 242L, `2022` = 246L, `2023` = 247L)[
    as.character(net_year)
  ]
  if (is.na(expected_districts)) {
    stop("No counterfactual district manifest for network year ", net_year)
  }
  file_pattern <- glue(
    "^ttm_{mode}_{net_year}net_nb{bank_year}_[0-9]{{5}}\\.parquet$"
  )
  inventory <- dir_ls(ROUTING_DIR, type = "file")
  per_district <- inventory[
    str_detect(path_file(inventory), file_pattern)
  ]
  if (length(per_district) != expected_districts) {
    stop(
      "Incomplete counterfactual routing manifest for ", mode, " ",
      net_year, "->", bank_year, ": expected ", expected_districts,
      ", found ", length(per_district)
    )
  }
  pattern
}


# === Bank Matching ============================================================

message("\n[1] Matching banks across years...")

bank_raw <- read_canonical_bank_data(DATA_ROOT)

make_bank_df <- function(ref_date_str) {
  make_bank_locations(bank_raw, ref_date_str)
}

banks_by_year <- list()
for (yr in YEARS) {
  ref_str <- paste0(yr - 1, BANK_PERIOD)
  banks_by_year[[as.character(yr)]] <- make_bank_df(ref_str)
}

survived_base_ids  <- list()
survived_target_ids <- list()

for (i in seq_len(nrow(YEAR_PAIRS))) {
  by <- YEAR_PAIRS$base_year[i]
  ty <- YEAR_PAIRS$target_year[i]
  pair_key <- glue("{by}_{ty}")

  base_df   <- banks_by_year[[as.character(by)]]
  target_df <- banks_by_year[[as.character(ty)]]

  common_location_ids <- intersect(base_df$location_id, target_df$location_id)
  survived_base_ids[[pair_key]]  <- common_location_ids
  survived_target_ids[[pair_key]] <- common_location_ids

  n_surv   <- length(common_location_ids)
  n_new    <- sum(!target_df$location_id %in% base_df$location_id)
  n_closed <- sum(!base_df$location_id %in% target_df$location_id)
  message(glue("  {by}->{ty}: {n_surv} survived, {n_new} new, {n_closed} closed"))
}


# === Census origins ===========================================================

census_sf <- st_read(path(DATA_ROOT, "census/oa_bnd.gpkg"), quiet = TRUE)
all_origin_ids <- census_sf$TOT_REG_CD


# === Counterfactuals ==========================================================

message("\n[2] Computing counterfactuals...")

cfa <- list()
cfb <- list()

for (i in seq_len(nrow(YEAR_PAIRS))) {
  by <- YEAR_PAIRS$base_year[i]
  ty <- YEAR_PAIRS$target_year[i]
  pair_key <- glue("{by}_{ty}")

  for (mode in c("car", "transit")) {
    key <- glue("{mode}_{pair_key}")
    threshold <- THRESHOLDS[[mode]]

    # --- CF-A = f(base_net, target_banks) ---
    # Surviving banks: base-year TTM filtered to survived base IDs
    base_ttm <- path(RESULTS_DIR, glue("ttm_{mode}_{by}.parquet"))
    surv_base <- as.character(survived_base_ids[[pair_key]])
    cfa_surv <- count_reachable(base_ttm, threshold, all_origin_ids, surv_base)

    # New banks: 6-1 routing (base=by, target=ty) = banks in ty not in by, on by network
    src_fwd <- resolve_routing(by, ty, mode)
    cfa_new <- count_reachable(src_fwd, threshold, all_origin_ids)

    cfa[[key]] <- cfa_surv |>
      rename(n_surviving = n_banks) |>
      left_join(cfa_new |> rename(n_new = n_banks), by = "origin_id") |>
      mutate(n_new = replace_na(n_new, 0), cfa = n_surviving + n_new)

    message(glue("  CF-A {mode} {by}->{ty}: done"))

    # --- CF-B = f(target_net, base_banks) ---
    # Surviving banks: target-year TTM filtered to survived target IDs
    target_ttm <- path(RESULTS_DIR, glue("ttm_{mode}_{ty}.parquet"))
    surv_target <- as.character(survived_target_ids[[pair_key]])
    cfb_surv <- count_reachable(target_ttm, threshold, all_origin_ids, surv_target)

    # Closed banks: 6-1 routing (base=ty, target=by) = banks in by not in ty, on ty network
    src_rev <- resolve_routing(ty, by, mode)
    cfb_closed <- count_reachable(src_rev, threshold, all_origin_ids)

    cfb[[key]] <- cfb_surv |>
      rename(n_surviving = n_banks) |>
      left_join(cfb_closed |> rename(n_closed = n_banks), by = "origin_id") |>
      mutate(n_closed = replace_na(n_closed, 0), cfb = n_surviving + n_closed)

    message(glue("  CF-B {mode} {by}->{ty}: done"))
  }
}


# === Decomposition ============================================================

message("\n[3] Computing decomposition...")

cache_file <- path(OUTPUT_DIR, "access_by_census_location_v1.parquet")
cache_manifest_file <- path(OUTPUT_DIR, "access_by_census_location_v1.manifest.csv")
actual_ttm_manifest <- tidyr::expand_grid(
  mode = c("car", "transit", "walk", "bicycle"),
  year = 2021:2024
) |>
  mutate(file = path(RESULTS_DIR, glue("ttm_{mode}_{year}.parquet")))
cache_inputs <- c(
  actual_ttm_manifest$file,
  path(RESULTS_DIR, "ID_SCHEME_LOCATION_V1")
)
if (!file_exists(cache_file) || !file_exists(cache_manifest_file) ||
    any(!file_exists(cache_inputs)) ||
    file_info(cache_file)$modification_time <
      max(file_info(cache_inputs)$modification_time)) {
  stop(
    "Missing or stale location_id access cache; run ",
    "Rscript scripts/5-2-main-analyze.R --cache-only"
  )
}
cache_manifest <- read_csv(cache_manifest_file, show_col_types = FALSE)
expected_cache_code_sha256 <- sha256_file(
  path(PROJECT_ROOT, "scripts/5-2-main-analyze.R")
)
if (nrow(cache_manifest) != 1L ||
    cache_manifest$id_scheme[[1]] != "location_id_v1" ||
    cache_manifest$analysis_code_sha256[[1]] != expected_cache_code_sha256) {
  stop("Access cache manifest does not match current location_id analysis code")
}
access_actual <- read_parquet(cache_file)
cache_key_counts <- access_actual |> count(mode, year, name = "n")
if (nrow(access_actual) != cache_manifest$n_rows[[1]] ||
    nrow(access_actual) != 1734208L ||
    nrow(cache_key_counts) != 16L ||
    any(cache_key_counts$n != 108388L) ||
    nrow(distinct(access_actual, census_id, mode, year)) != nrow(access_actual)) {
  stop("Access cache rows or compound keys are incomplete")
}
message(glue("  Actual accessibility: {comma(nrow(access_actual))} records"))

pop_total <- YEARS |>
  set_names() |>
  map(\(yr) {
    fread(
      path(DATA_ROOT, "census/stats", glue("2024년기준_{yr}년_인구총괄(총인구).txt")),
      sep = "^", header = FALSE,
      col.names = c("year", "census_id", "stat_code", "value")
    )
  }) |>
  list_rbind() |>
  filter(stat_code == "to_in_001") |>
  transmute(year, census_id = as.character(census_id),
            population = suppressWarnings(as.numeric(value))) |>
  filter(!is.na(population), population > 0)

decomp_results <- list()

for (i in seq_len(nrow(YEAR_PAIRS))) {
  by <- YEAR_PAIRS$base_year[i]
  ty <- YEAR_PAIRS$target_year[i]
  pair_key <- glue("{by}_{ty}")

  for (mode in c("car", "transit")) {
    key <- glue("{mode}_{pair_key}")
    threshold_col <- if (mode == "car") "n_10min" else "n_30min"

    actual_base <- access_actual |>
      filter(mode == !!mode, year == by) |>
      select(census_id, actual_base = !!sym(threshold_col))

    actual_target <- access_actual |>
      filter(mode == !!mode, year == ty) |>
      select(census_id, actual_target = !!sym(threshold_col))

    decomp <- actual_base |>
      inner_join(actual_target, by = "census_id") |>
      inner_join(cfa[[key]] |> select(census_id = origin_id, cfa), by = "census_id") |>
      inner_join(cfb[[key]] |> select(census_id = origin_id, cfb), by = "census_id") |>
      inner_join(
        pop_total |> filter(year == by) |> select(census_id, population),
        by = "census_id") |>
      mutate(
        total_change    = actual_target - actual_base,
        service_effect  = cfa - actual_base,
        network_effect  = cfb - actual_base,
        interaction     = total_change - service_effect - network_effect,
        sgg_cd          = str_sub(census_id, 1, 5),
        mode            = !!mode,
        base_year       = by,
        target_year     = ty
      )

    decomp_results[[key]] <- decomp
  }
}

decomp_all <- bind_rows(decomp_results)


# === Aggregate ================================================================

message("\n[4] Aggregating...")

national_decomp <- decomp_all |>
  group_by(mode, base_year, target_year) |>
  summarize(
    n_tracts        = n(),
    total_pop       = sum(population),
    actual_base     = weighted.mean(actual_base, population),
    actual_target   = weighted.mean(actual_target, population),
    cfa             = weighted.mean(cfa, population),
    cfb             = weighted.mean(cfb, population),
    total_change    = weighted.mean(total_change, population),
    service_effect  = weighted.mean(service_effect, population),
    network_effect  = weighted.mean(network_effect, population),
    interaction     = weighted.mean(interaction, population),
    service_pct     = service_effect / abs(total_change) * 100,
    network_pct     = network_effect / abs(total_change) * 100,
    interact_pct    = interaction / abs(total_change) * 100,
    .groups = "drop"
  )

cat("\n=== National Decomposition ===\n")
national_decomp |>
  mutate(across(
    where(is.numeric) & !c(n_tracts, total_pop, base_year, target_year),
    \(x) round(x, 2))) |>
  print(n = 30)

district_decomp <- decomp_all |>
  group_by(sgg_cd, mode, base_year, target_year) |>
  summarize(
    total_pop       = sum(population),
    actual_base     = weighted.mean(actual_base, population),
    actual_target   = weighted.mean(actual_target, population),
    cfa             = weighted.mean(cfa, population),
    cfb             = weighted.mean(cfb, population),
    total_change    = weighted.mean(total_change, population),
    service_effect  = weighted.mean(service_effect, population),
    network_effect  = weighted.mean(network_effect, population),
    interaction     = weighted.mean(interaction, population),
    .groups = "drop"
  )

write_csv(national_decomp, path(OUTPUT_DIR, "decomp_national.csv"))
write_csv(district_decomp, path(OUTPUT_DIR, "decomp_district.csv"))
write_parquet(decomp_all, path(OUTPUT_DIR, "decomp_census.parquet"))

message(glue("\nSaved: decomp_national.csv"))
message(glue("Saved: decomp_district.csv ({comma(nrow(district_decomp))} rows)"))
message(glue("Saved: decomp_census.parquet ({comma(nrow(decomp_all))} rows)"))

message("\n", strrep("=", 60))
message("DECOMPOSITION COMPLETE")
message(strrep("=", 60))
