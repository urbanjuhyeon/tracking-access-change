# 6-2-decompose-pilot.R
# Pilot decomposition: base=2021 only, CF-A available, CF-B incomplete
#
# What we CAN compute:
#   ΔTotal    = Actual_target - Actual_base
#   ΔService  = CF-A - Actual_base    (bank closures effect)
#   Residual  = ΔTotal - ΔService     (= ΔNetwork + Interaction, inseparable for now)

pacman::p_load(tidyverse, sf, arrow, fs, glue, scales, duckdb)

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

BASE_YEAR   <- 2021
TARGET_YEARS <- c(2022, 2023)
BANK_PERIOD <- "h2"
THRESHOLDS  <- list(car = 10, transit = 30)

message(strrep("=", 60))
message("PILOT DECOMPOSITION: base=2021")
message(strrep("=", 60))

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

# === Bank Matching ============================================================

message("\n[1] Matching banks...")

bank_raw <- read_canonical_bank_data(DATA_ROOT)

make_bank_df <- function(ref_date_str) {
  make_bank_locations(bank_raw, ref_date_str)
}

all_years <- c(2021, 2022, 2023)
banks_by_year <- list()
for (yr in all_years) {
  ref_str <- paste0(yr - 1, BANK_PERIOD)
  banks_by_year[[as.character(yr)]] <- make_bank_df(ref_str)
  message(glue("  {yr}: {nrow(banks_by_year[[as.character(yr)]])} banks"))
}

# === Census origins ===========================================================

census_sf <- st_read(path(DATA_ROOT, "census/oa_bnd.gpkg"), quiet = TRUE)
all_origin_ids <- census_sf$TOT_REG_CD
message(glue("  {comma(length(all_origin_ids))} census tracts"))

# === Population ===============================================================

pop <- data.table::fread(
  path(DATA_ROOT, "census/stats", glue("2024년기준_{BASE_YEAR}년_인구총괄(총인구).txt")),
  sep = "^", header = FALSE,
  col.names = c("year", "census_id", "stat_code", "value")
) |>
  as_tibble() |>
  filter(stat_code == "to_in_001") |>
  transmute(census_id = as.character(census_id),
            population = suppressWarnings(as.numeric(value))) |>
  filter(!is.na(population), population > 0)

message(glue("  Population: {comma(nrow(pop))} tracts with data"))

# === Actual accessibility (from 5-2 cache) ====================================

message("\n[2] Loading actual accessibility...")

access_cache_file <- path(OUTPUT_DIR, "access_by_census_location_v1.parquet")
access_manifest_file <- path(
  OUTPUT_DIR,
  "access_by_census_location_v1.manifest.csv"
)
actual_ttm_files <- tidyr::expand_grid(
  mode = c("car", "transit", "walk", "bicycle"),
  year = 2021:2024
) |>
  transmute(file = path(RESULTS_DIR, glue("ttm_{mode}_{year}.parquet"))) |>
  pull(file)
cache_inputs <- c(
  actual_ttm_files,
  path(RESULTS_DIR, "ID_SCHEME_LOCATION_V1")
)
if (!file_exists(access_cache_file) || !file_exists(access_manifest_file) ||
    any(!file_exists(cache_inputs)) ||
    file_info(access_cache_file)$modification_time <
      max(file_info(cache_inputs)$modification_time)) {
  stop("Missing or stale location_id access cache; run 5-2 with --cache-only")
}
access_manifest <- read_csv(access_manifest_file, show_col_types = FALSE)
expected_code_sha256 <- sha256_file(path(PROJECT_ROOT, "scripts/5-2-main-analyze.R"))
if (nrow(access_manifest) != 1L ||
    access_manifest$id_scheme[[1]] != "location_id_v1" ||
    access_manifest$analysis_code_sha256[[1]] != expected_code_sha256) {
  stop("Access cache manifest does not match current location_id analysis code")
}
access_actual <- read_parquet(access_cache_file)
access_key_counts <- access_actual |> count(mode, year, name = "n")
if (nrow(access_actual) != access_manifest$n_rows[[1]] ||
    nrow(access_actual) != 1734208L ||
    nrow(access_key_counts) != 16L ||
    any(access_key_counts$n != 108388L) ||
    nrow(distinct(access_actual, census_id, mode, year)) != nrow(access_actual)) {
  stop("Access cache rows or compound keys are incomplete")
}

# === Compute CF-A and decompose ===============================================

message("\n[3] Computing decomposition...")

results <- list()

for (ty in TARGET_YEARS) {
  pair_key <- glue("{BASE_YEAR}_{ty}")
  base_df   <- banks_by_year[[as.character(BASE_YEAR)]]
  target_df <- banks_by_year[[as.character(ty)]]

  common_location_ids <- intersect(base_df$location_id, target_df$location_id)
  surv_base_ids <- common_location_ids

  n_surv   <- length(common_location_ids)
  n_new    <- sum(!target_df$location_id %in% base_df$location_id)
  n_closed <- sum(!base_df$location_id %in% target_df$location_id)
  message(glue("\n  {BASE_YEAR}->{ty}: {n_surv} survived, {n_new} new, {n_closed} closed"))

  for (mode in c("car", "transit")) {
    threshold <- THRESHOLDS[[mode]]
    threshold_col <- if (mode == "car") "n_10min" else "n_30min"

    # Actual base & target
    actual_base <- access_actual |>
      filter(mode == !!mode, year == BASE_YEAR) |>
      select(census_id, actual_base = !!sym(threshold_col))

    actual_target <- access_actual |>
      filter(mode == !!mode, year == ty) |>
      select(census_id, actual_target = !!sym(threshold_col))

    # CF-A = f(base_net, target_banks)
    # Part 1: surviving banks on base network
    base_ttm <- path(RESULTS_DIR, glue("ttm_{mode}_{BASE_YEAR}.parquet"))
    cfa_surv <- count_reachable(base_ttm, threshold, all_origin_ids, surv_base_ids)

    # Part 2: new banks on base network (from 6-1)
    fwd_pattern <- path(ROUTING_DIR,
      glue("ttm_{mode}_{BASE_YEAR}net_nb{ty}_*.parquet"))
    cfa_new <- count_reachable(fwd_pattern, threshold, all_origin_ids)

    cfa_total <- cfa_surv$n_banks + cfa_new$n_banks

    # Decompose
    decomp <- actual_base |>
      inner_join(actual_target, by = "census_id") |>
      mutate(
        cfa = cfa_total[match(census_id, cfa_surv$origin_id)],
        cfa = replace_na(cfa, 0)
      ) |>
      inner_join(pop, by = "census_id") |>
      mutate(
        total_change   = actual_target - actual_base,
        service_effect = cfa - actual_base,
        residual       = total_change - service_effect,
        sgg_cd         = str_sub(census_id, 1, 5),
        mode           = !!mode,
        base_year      = BASE_YEAR,
        target_year    = ty
      )

    results[[glue("{mode}_{pair_key}")]] <- decomp
    message(glue("  {mode} {BASE_YEAR}->{ty}: {comma(nrow(decomp))} tracts"))
  }
}

decomp_all <- bind_rows(results)

# === National summary =========================================================

message("\n[4] National summary...")

national <- decomp_all |>
  group_by(mode, base_year, target_year) |>
  summarize(
    n_tracts       = n(),
    total_pop      = sum(population),
    mean_base      = weighted.mean(actual_base, population),
    mean_target    = weighted.mean(actual_target, population),
    mean_cfa       = weighted.mean(cfa, population),
    total_change   = weighted.mean(total_change, population),
    service_effect = weighted.mean(service_effect, population),
    residual       = weighted.mean(residual, population),
    pct_service    = service_effect / total_change * 100,
    pct_residual   = residual / total_change * 100,
    .groups = "drop"
  )

cat("\n")
cat(strrep("=", 70), "\n")
cat("NATIONAL DECOMPOSITION (base=2021, population-weighted)\n")
cat(strrep("=", 70), "\n\n")

for (i in seq_len(nrow(national))) {
  r <- national[i, ]
  cat(glue("{r$mode} {r$base_year}->{r$target_year}"), "\n")
  cat(glue("  Accessibility:  {round(r$mean_base, 2)} -> {round(r$mean_target, 2)}"), "\n")
  cat(glue("  CF-A (base net, target banks): {round(r$mean_cfa, 2)}"), "\n")
  cat(glue("  ΔTotal:    {sprintf('%+.3f', r$total_change)}"), "\n")
  cat(glue("  ΔService:  {sprintf('%+.3f', r$service_effect)}  ({round(r$pct_service, 1)}%)"), "\n")
  cat(glue("  Residual:  {sprintf('%+.3f', r$residual)}  ({round(r$pct_residual, 1)}%)"), "\n")
  cat(glue("  (Residual = ΔNetwork + Interaction, separable when CF-B routing completes)"), "\n\n")
}

# === District summary =========================================================

district <- decomp_all |>
  group_by(sgg_cd, mode, base_year, target_year) |>
  summarize(
    total_pop      = sum(population),
    mean_base      = weighted.mean(actual_base, population),
    mean_target    = weighted.mean(actual_target, population),
    mean_cfa       = weighted.mean(cfa, population),
    total_change   = weighted.mean(total_change, population),
    service_effect = weighted.mean(service_effect, population),
    residual       = weighted.mean(residual, population),
    .groups = "drop"
  )

# === Save =====================================================================

write_csv(national, path(OUTPUT_DIR, "decomp_pilot_national.csv"))
write_csv(district, path(OUTPUT_DIR, "decomp_pilot_district.csv"))
write_parquet(decomp_all, path(OUTPUT_DIR, "decomp_pilot_census.parquet"))

message(glue("\nSaved: decomp_pilot_national.csv"))
message(glue("Saved: decomp_pilot_district.csv ({comma(nrow(district))} rows)"))
message(glue("Saved: decomp_pilot_census.parquet ({comma(nrow(decomp_all))} rows)"))

# === Top movers ===============================================================

cat("\n")
cat(strrep("=", 70), "\n")
cat("TOP 10 DISTRICTS BY SERVICE EFFECT (transit, 2021->2023)\n")
cat(strrep("=", 70), "\n\n")

sgg_names <- st_read(path(DATA_ROOT, "census/sgg_bnd.gpkg"), quiet = TRUE) |>
  st_drop_geometry() |>
  select(sgg_cd = SIGUNGU_CD, sgg_nm = SIGUNGU_NM)

district |>
  filter(mode == "transit", target_year == 2023) |>
  left_join(sgg_names, by = "sgg_cd") |>
  arrange(service_effect) |>
  head(10) |>
  transmute(
    sgg_nm,
    pop = comma(total_pop),
    base = round(mean_base, 1),
    target = round(mean_target, 1),
    total = sprintf("%+.2f", total_change),
    service = sprintf("%+.2f", service_effect),
    residual = sprintf("%+.2f", residual)
  ) |>
  print(n = 10)

cat("\n")

district |>
  filter(mode == "transit", target_year == 2023) |>
  left_join(sgg_names, by = "sgg_cd") |>
  arrange(desc(service_effect)) |>
  head(10) |>
  transmute(
    sgg_nm,
    pop = comma(total_pop),
    base = round(mean_base, 1),
    target = round(mean_target, 1),
    total = sprintf("%+.2f", total_change),
    service = sprintf("%+.2f", service_effect),
    residual = sprintf("%+.2f", residual)
  ) |>
  print(n = 10)

message("\n", strrep("=", 60))
message("PILOT COMPLETE")
message("Note: Full decomposition (ΔNetwork, Interaction) requires")
message("  base=2022 and base=2023 reverse routing to finish.")
message(strrep("=", 60))
