# 7-0-policy-pilot.R
# Policy counterfactual pilot: "last-branch protection" rule.
#
# Scenario: closures (2021 -> 2023) that left an administrative dong with zero
# bank branches are blocked. Two variants:
#   A: one branch per emptied dong survives (minimal bite of the rule)
#   B: all closed branches in emptied dongs survive (upper bound)
#
# No new routing needed: reverse-pair TTMs (2023 network x banks that existed
# only in 2021) already contain travel times to every closed branch.
# Scenario accessibility = actual 2023 + reachable resurrected branches.

pacman::p_load(tidyverse, sf, arrow, fs, glue, data.table, duckdb)

PROJECT_ROOT <- normalizePath(
  Sys.getenv("ACCESS_DECLINE_ROOT", unset = "."),
  winslash = "/", mustWork = TRUE
)
if (!file.exists(file.path(PROJECT_ROOT, ".gitignore"))) {
  stop("Run from the repository root or set ACCESS_DECLINE_ROOT.")
}
DATA_ROOT     <- path(PROJECT_ROOT, "data")
RESULTS_DIR   <- path(PROJECT_ROOT, "workflows/results")
ROUTING_DIR   <- path(RESULTS_DIR, "decompose/routing")
OUTPUT_DIR    <- path(PROJECT_ROOT, "workflows/analysis")

source(path(PROJECT_ROOT, "scripts/_bank_locations.R"))
assert_location_id_ttm_ready(RESULTS_DIR)

BANK_PERIOD <- "h2"
THRESHOLDS  <- list(car = 10, transit = 30)
WGS84       <- 4326

message(strrep("=", 70))
message("POLICY PILOT: last-branch protection rule (2021 -> 2023)")
message(strrep("=", 70))

# === [1] Banks, matched exactly as in 6-2 =====================================

message("\n[1] Rebuilding bank sets (6-2 recipe)...")

bank_raw <- read_canonical_bank_data(DATA_ROOT)

make_bank_df <- function(ref_date_str) {
  make_bank_locations(bank_raw, ref_date_str)
}

banks_2021 <- make_bank_df(paste0(2020, BANK_PERIOD))
banks_2023 <- make_bank_df(paste0(2022, BANK_PERIOD))

closed <- banks_2021 |>
  filter(!location_id %in% banks_2023$location_id)
message(glue("  2021: {nrow(banks_2021)} | 2023: {nrow(banks_2023)} | closed: {nrow(closed)}"))

# === [2] Assign banks to administrative dongs =================================

message("\n[2] Spatial join to dong boundaries...")

dong_sf <- st_read(path(DATA_ROOT, "census/dong_bnd.gpkg"), quiet = TRUE)
code_col <- names(dong_sf)[str_detect(tolower(names(dong_sf)), "cd|code")][1]
message(glue("  dong layer: {nrow(dong_sf)} polygons, code column: {code_col}"))

to_dong <- function(df) {
  pts <- st_as_sf(df, coords = c("longitude", "latitude"), crs = WGS84) |>
    st_transform(st_crs(dong_sf))
  joined <- st_join(pts, dong_sf[code_col], join = st_intersects)
  df$dong_cd <- st_drop_geometry(joined)[[code_col]]
  df
}

banks_2021 <- to_dong(banks_2021)
banks_2023 <- to_dong(banks_2023)
n_na <- sum(is.na(banks_2021$dong_cd)) + sum(is.na(banks_2023$dong_cd))
message(glue("  unmatched points dropped: {n_na}"))

# === [3] Identify emptied dongs and resurrected branches ======================

message("\n[3] Applying last-branch rule...")

cnt_2021 <- banks_2021 |> filter(!is.na(dong_cd)) |> count(dong_cd, name = "n_2021")
cnt_2023 <- banks_2023 |> filter(!is.na(dong_cd)) |> count(dong_cd, name = "n_2023")

emptied <- cnt_2021 |>
  left_join(cnt_2023, by = "dong_cd") |>
  mutate(n_2023 = replace_na(n_2023, 0)) |>
  filter(n_2023 == 0)

closed_dong <- banks_2021 |>
  filter(!location_id %in% banks_2023$location_id, dong_cd %in% emptied$dong_cd)

resurrect_A <- closed_dong |>
  group_by(dong_cd) |>
  slice_min(location_id, n = 1) |>
  ungroup()
resurrect_B <- closed_dong

message(glue("  dongs with banks in 2021: {nrow(cnt_2021)}"))
message(glue("  dongs emptied by 2023:    {nrow(emptied)}"))
message(glue("  resurrected A (1/dong):   {nrow(resurrect_A)}"))
message(glue("  resurrected B (all):      {nrow(resurrect_B)}"))

# === [4] Reachable resurrected branches per tract (existing TTMs) =============

message("\n[4] Counting reachable resurrected branches (DuckDB over rev TTMs)...")

routing_inventory <- dir_ls(ROUTING_DIR, type = "file")
for (mode_value in c("car", "transit")) {
  expected_pattern <- glue(
    "^ttm_{mode_value}_2023net_nb2021_[0-9]{{5}}\\.parquet$"
  )
  observed <- routing_inventory[
    str_detect(path_file(routing_inventory), expected_pattern)
  ]
  if (length(observed) != 247L) {
    stop("Incomplete policy routing manifest for ", mode_value,
         ": expected 247, found ", length(observed))
  }
}

count_gain <- function(mode, ids) {
  if (length(ids) == 0) {
    return(tibble(census_id = character(), gain = numeric()))
  }

  threshold <- THRESHOLDS[[mode]]
  pattern <- path(ROUTING_DIR, glue("ttm_{mode}_2023net_nb2021_*.parquet"))
  ids_sql <- paste(sql_quote_location_ids(ids), collapse = ", ")

  con <- dbConnect(duckdb::duckdb())
  on.exit(dbDisconnect(con, shutdown = TRUE))
  dbGetQuery(con, glue("
    WITH filtered AS (
      SELECT from_id, to_id, travel_time_p50, departure_time
      FROM read_parquet('{pattern}')
      WHERE from_id IS NOT NULL
        AND CAST(to_id AS VARCHAR) IN ({ids_sql})
    ),
    by_dep AS (
      SELECT CAST(from_id AS VARCHAR) AS census_id, departure_time,
             SUM(CASE WHEN travel_time_p50 <= {threshold} THEN 1 ELSE 0 END) AS n
      FROM filtered GROUP BY from_id, departure_time
    )
    SELECT census_id, AVG(n) AS gain FROM by_dep GROUP BY census_id
  ")) |> as_tibble()
}

gains <- list()
for (mode in c("car", "transit")) {
  gains[[glue("{mode}_A")]] <- count_gain(mode, resurrect_A$location_id)
  gains[[glue("{mode}_B")]] <- count_gain(mode, resurrect_B$location_id)
  message(glue("  {mode}: A tracts w/ gain {sum(gains[[glue('{mode}_A')]]$gain > 0)}, ",
               "B tracts w/ gain {sum(gains[[glue('{mode}_B')]]$gain > 0)}"))
}

# === [5] Combine with existing decomposition ==================================

message("\n[5] Scenario accounting vs 2021->2023 decomposition...")

decomp_file <- path(OUTPUT_DIR, "decomp_full_census.parquet")
decomp_inputs <- c(
  path(RESULTS_DIR, "ID_SCHEME_LOCATION_V1"),
  dir_ls(RESULTS_DIR, type = "file", regexp = "ttm_.*\\.parquet$"),
  routing_inventory
)
if (!file_exists(decomp_file) ||
    file_info(decomp_file)$modification_time <
      max(file_info(decomp_inputs)$modification_time)) {
  stop(
    "Missing or stale full decomposition; run ",
    "Rscript scripts/6-2-decompose-full-2021-2023.R"
  )
}
decomp <- read_parquet(decomp_file)
required_decomp_columns <- c(
  "census_id", "population", "actual_base", "actual_target",
  "total_change", "service_effect", "mode", "base_year", "target_year"
)
if (!all(required_decomp_columns %in% names(decomp)) || nrow(decomp) != 413124L) {
  stop("Unexpected full decomposition schema or row count: ", decomp_file)
}
decomp <- decomp |>
  filter(base_year == 2021, target_year == 2023)

scenario <- decomp |>
  left_join(bind_rows(
    gains$car_A     |> mutate(mode = "car",     variant = "A"),
    gains$car_B     |> mutate(mode = "car",     variant = "B"),
    gains$transit_A |> mutate(mode = "transit", variant = "A"),
    gains$transit_B |> mutate(mode = "transit", variant = "B")
  ) |> pivot_wider(names_from = variant, values_from = gain, names_prefix = "gain_"),
  by = c("census_id", "mode")) |>
  mutate(across(c(gain_A, gain_B), \(x) replace_na(x, 0)))

national <- scenario |>
  group_by(mode) |>
  summarize(
    total_pop      = sum(population),
    actual_base    = weighted.mean(actual_base, population),
    actual_target  = weighted.mean(actual_target, population),
    total_change   = weighted.mean(total_change, population),
    service_effect = weighted.mean(service_effect, population),
    averted_A      = weighted.mean(gain_A, population),
    averted_B      = weighted.mean(gain_B, population),
    pct_of_decline_A = averted_A / abs(total_change) * 100,
    pct_of_decline_B = averted_B / abs(total_change) * 100,
    pct_of_service_A = averted_A / abs(service_effect) * 100,
    pct_of_service_B = averted_B / abs(service_effect) * 100,
    pop_benefiting_A = sum(population[gain_A > 0]),
    pop_benefiting_B = sum(population[gain_B > 0]),
    .groups = "drop"
  )

cat("\n=== National summary (pop-weighted) ===\n")
national |> mutate(across(where(is.numeric), \(x) round(x, 3))) |> print(width = Inf)

sgg <- scenario |>
  group_by(sgg_cd, mode) |>
  summarize(
    pop        = sum(population),
    total_change = weighted.mean(total_change, population),
    averted_A  = weighted.mean(gain_A, population),
    averted_B  = weighted.mean(gain_B, population),
    .groups = "drop"
  ) |>
  arrange(desc(averted_A))

cat("\n=== Top 12 districts by averted loss (variant A, transit) ===\n")
sgg |> filter(mode == "transit") |> slice_head(n = 12) |> print()

write_csv(national, path(OUTPUT_DIR, "policy_pilot_lastbranch_national.csv"))
write_csv(sgg, path(OUTPUT_DIR, "policy_pilot_lastbranch_sgg.csv"))
write_csv(
  resurrect_A |>
    select(location_id, bank_name, branch_name, dong_cd, longitude, latitude),
  path(OUTPUT_DIR, "policy_pilot_resurrected_A.csv"))

message("\nSaved: policy_pilot_lastbranch_national.csv / _sgg.csv / resurrected_A.csv")
message(strrep("=", 70))
message("PILOT COMPLETE")
message(strrep("=", 70))
