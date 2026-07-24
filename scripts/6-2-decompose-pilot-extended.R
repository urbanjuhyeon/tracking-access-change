# 6-2-decompose-pilot-extended.R
# Extended pilot: multiple cutoffs, relative change, zero-access analysis

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

BASE_YEAR    <- 2021
TARGET_YEARS <- c(2022, 2023)
BANK_PERIOD  <- "h2"

CAR_CUTOFFS     <- c(10, 15, 20, 30)
TRANSIT_CUTOFFS <- c(15, 30, 45, 60)

message(strrep("=", 60))
message("EXTENDED PILOT: multi-cutoff decomposition")
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
  } else ""

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

# === Data setup ===============================================================

message("\n[1] Loading data...")

bank_raw <- read_canonical_bank_data(DATA_ROOT)

make_bank_df <- function(ref_date_str) {
  make_bank_locations(bank_raw, ref_date_str)
}

all_years <- c(2021, 2022, 2023)
banks_by_year <- list()
for (yr in all_years) {
  banks_by_year[[as.character(yr)]] <- make_bank_df(paste0(yr - 1, BANK_PERIOD))
}

census_sf <- st_read(path(DATA_ROOT, "census/oa_bnd.gpkg"), quiet = TRUE)
all_origin_ids <- census_sf$TOT_REG_CD

sgg_names <- st_read(path(DATA_ROOT, "census/sgg_bnd.gpkg"), quiet = TRUE) |>
  st_drop_geometry() |>
  select(sgg_cd = SIGUNGU_CD, sgg_nm = SIGUNGU_NM)

pop <- data.table::fread(
  path(DATA_ROOT, "census/stats", glue("2024년기준_{BASE_YEAR}년_인구총괄(총인구).txt")),
  sep = "^", header = FALSE,
  col.names = c("year", "census_id", "stat_code", "value")
) |> as_tibble() |>
  filter(stat_code == "to_in_001") |>
  transmute(census_id = as.character(census_id),
            population = suppressWarnings(as.numeric(value))) |>
  filter(!is.na(population), population > 0)

message(glue("  {comma(length(all_origin_ids))} tracts, {comma(nrow(pop))} with population"))

# === Multi-cutoff decomposition ===============================================

message("\n[2] Multi-cutoff decomposition...")

all_results <- list()

for (ty in TARGET_YEARS) {
  base_df   <- banks_by_year[[as.character(BASE_YEAR)]]
  target_df <- banks_by_year[[as.character(ty)]]
  common_location_ids <- intersect(base_df$location_id, target_df$location_id)
  surv_base_ids <- common_location_ids

  for (mode in c("car", "transit")) {
    cutoffs <- if (mode == "car") CAR_CUTOFFS else TRANSIT_CUTOFFS
    base_ttm <- path(RESULTS_DIR, glue("ttm_{mode}_{BASE_YEAR}.parquet"))
    target_ttm <- path(RESULTS_DIR, glue("ttm_{mode}_{ty}.parquet"))
    fwd_pattern <- path(ROUTING_DIR,
      glue("ttm_{mode}_{BASE_YEAR}net_nb{ty}_*.parquet"))

    for (threshold in cutoffs) {
      message(glue("  {mode} {BASE_YEAR}->{ty} cutoff={threshold}min..."))

      actual_base <- count_reachable(base_ttm, threshold, all_origin_ids)
      actual_target <- count_reachable(target_ttm, threshold, all_origin_ids)
      cfa_surv <- count_reachable(base_ttm, threshold, all_origin_ids, surv_base_ids)
      cfa_new  <- count_reachable(fwd_pattern, threshold, all_origin_ids)

      decomp <- tibble(
        census_id    = as.character(all_origin_ids),
        actual_base  = actual_base$n_banks,
        actual_target = actual_target$n_banks,
        cfa          = cfa_surv$n_banks + cfa_new$n_banks
      ) |>
        inner_join(pop, by = "census_id") |>
        mutate(
          total_change   = actual_target - actual_base,
          service_effect = cfa - actual_base,
          residual       = total_change - service_effect,
          sgg_cd         = str_sub(census_id, 1, 5),
          mode           = !!mode,
          base_year      = BASE_YEAR,
          target_year    = ty,
          cutoff         = threshold
        )

      all_results[[glue("{mode}_{ty}_{threshold}")]] <- decomp
    }
  }
}

decomp_all <- bind_rows(all_results)

# === Analysis 1: National by cutoff ==========================================

message("\n[3] Analysis 1: Sensitivity to cutoff...")

national <- decomp_all |>
  group_by(mode, base_year, target_year, cutoff) |>
  summarize(
    mean_base      = weighted.mean(actual_base, population),
    mean_target    = weighted.mean(actual_target, population),
    total_change   = weighted.mean(total_change, population),
    service_effect = weighted.mean(service_effect, population),
    residual       = weighted.mean(residual, population),
    pct_service    = service_effect / total_change * 100,
    .groups = "drop"
  )

cat("\n")
cat(strrep("=", 70), "\n")
cat("SENSITIVITY: How does cutoff affect the decomposition?\n")
cat(strrep("=", 70), "\n\n")

for (m in c("car", "transit")) {
  cat(glue("--- {toupper(m)} ---"), "\n")
  national |>
    filter(mode == m) |>
    transmute(
      period = glue("{base_year}->{target_year}"),
      cutoff = glue("{cutoff}min"),
      base = round(mean_base, 1),
      target = round(mean_target, 1),
      total = sprintf("%+.2f", total_change),
      service = sprintf("%+.2f", service_effect),
      svc_pct = sprintf("%.0f%%", pct_service),
      residual = sprintf("%+.2f", residual)
    ) |>
    print(n = 30)
  cat("\n")
}

# === Analysis 2: Zero-access population ======================================

message("[4] Analysis 2: Zero-access population...")

zero_access <- decomp_all |>
  group_by(mode, target_year, cutoff) |>
  summarize(
    pop_zero_base   = sum(population[actual_base == 0]),
    pop_zero_target = sum(population[actual_target == 0]),
    pop_lost_all    = sum(population[actual_base > 0 & actual_target == 0]),
    pop_total       = sum(population),
    .groups = "drop"
  ) |>
  mutate(
    pct_zero_base   = pop_zero_base / pop_total * 100,
    pct_zero_target = pop_zero_target / pop_total * 100,
    pct_lost_all    = pop_lost_all / pop_total * 100
  )

cat(strrep("=", 70), "\n")
cat("ZERO-ACCESS: Population with NO reachable banks\n")
cat(strrep("=", 70), "\n\n")

zero_access |>
  transmute(
    mode, target_year,
    cutoff = glue("{cutoff}min"),
    zero_2021 = glue("{comma(pop_zero_base)} ({round(pct_zero_base, 1)}%)"),
    zero_target = glue("{comma(pop_zero_target)} ({round(pct_zero_target, 1)}%)"),
    newly_zero = glue("{comma(pop_lost_all)} ({round(pct_lost_all, 1)}%)")
  ) |>
  print(n = 30)

# === Analysis 3: Urban vs Rural decomposition ================================

message("\n[5] Analysis 3: Urban vs Rural...")

urban_codes <- sgg_names |>
  mutate(
    sido = str_sub(sgg_cd, 1, 2),
    type = case_when(
      sido %in% c("11") ~ "Seoul",
      sido %in% c("21", "22", "23", "24", "25", "26") ~ "Metropolitan",
      TRUE ~ "Non-metro"
    )
  ) |>
  select(sgg_cd, type)

urban_decomp <- decomp_all |>
  left_join(urban_codes, by = "sgg_cd") |>
  group_by(type, mode, target_year, cutoff) |>
  summarize(
    total_pop      = sum(population),
    mean_base      = weighted.mean(actual_base, population),
    mean_target    = weighted.mean(actual_target, population),
    total_change   = weighted.mean(total_change, population),
    service_effect = weighted.mean(service_effect, population),
    residual       = weighted.mean(residual, population),
    pct_service    = service_effect / total_change * 100,
    .groups = "drop"
  )

cat("\n")
cat(strrep("=", 70), "\n")
cat("URBAN vs RURAL (transit, 30min cutoff)\n")
cat(strrep("=", 70), "\n\n")

urban_decomp |>
  filter(mode == "transit", cutoff == 30) |>
  transmute(
    type,
    period = glue("{target_year}"),
    pop = comma(total_pop),
    base = round(mean_base, 1),
    target = round(mean_target, 1),
    total = sprintf("%+.2f", total_change),
    service = sprintf("%+.2f", service_effect),
    svc_pct = sprintf("%.0f%%", pct_service),
    residual = sprintf("%+.2f", residual)
  ) |>
  arrange(type, period) |>
  print(n = 20)

cat("\n")
cat("Same table for CAR (10min cutoff):\n\n")

urban_decomp |>
  filter(mode == "car", cutoff == 10) |>
  transmute(
    type,
    period = glue("{target_year}"),
    pop = comma(total_pop),
    base = round(mean_base, 1),
    target = round(mean_target, 1),
    total = sprintf("%+.2f", total_change),
    service = sprintf("%+.2f", service_effect),
    svc_pct = sprintf("%.0f%%", pct_service),
    residual = sprintf("%+.2f", residual)
  ) |>
  arrange(type, period) |>
  print(n = 20)

# === Analysis 4: Relative change by district ==================================

message("\n[6] Analysis 4: Relative change (who lost proportionally most)...")

district_rel <- decomp_all |>
  filter(cutoff == ifelse(mode == "car", 10, 30)) |>
  group_by(sgg_cd, mode, target_year) |>
  summarize(
    total_pop      = sum(population),
    mean_base      = weighted.mean(actual_base, population),
    mean_target    = weighted.mean(actual_target, population),
    total_change   = weighted.mean(total_change, population),
    service_effect = weighted.mean(service_effect, population),
    residual       = weighted.mean(residual, population),
    .groups = "drop"
  ) |>
  mutate(
    pct_total_change   = ifelse(mean_base > 0, total_change / mean_base * 100, NA),
    pct_service_effect = ifelse(mean_base > 0, service_effect / mean_base * 100, NA)
  ) |>
  left_join(sgg_names, by = "sgg_cd")

cat("\n")
cat(strrep("=", 70), "\n")
cat("RELATIVE CHANGE: Transit 30min, 2021->2023\n")
cat("(Districts with base >= 1 bank, sorted by % service decline)\n")
cat(strrep("=", 70), "\n\n")

district_rel |>
  filter(mode == "transit", target_year == 2023, mean_base >= 1) |>
  arrange(pct_service_effect) |>
  head(15) |>
  transmute(
    sgg_nm,
    pop = comma(total_pop),
    base = round(mean_base, 1),
    svc_eff = sprintf("%+.1f", service_effect),
    pct_svc = sprintf("%+.1f%%", pct_service_effect),
    total = sprintf("%+.1f%%", pct_total_change)
  ) |>
  print(n = 15)

cat("\n")
cat("Same for CAR 10min, 2021->2023:\n\n")

district_rel |>
  filter(mode == "car", target_year == 2023, mean_base >= 1) |>
  arrange(pct_service_effect) |>
  head(15) |>
  transmute(
    sgg_nm,
    pop = comma(total_pop),
    base = round(mean_base, 1),
    svc_eff = sprintf("%+.1f", service_effect),
    pct_svc = sprintf("%+.1f%%", pct_service_effect),
    total = sprintf("%+.1f%%", pct_total_change)
  ) |>
  print(n = 15)

# === Analysis 5: Car vs Transit divergence ====================================

message("\n[7] Analysis 5: Car vs Transit divergence...")

car_transit <- district_rel |>
  filter(target_year == 2023) |>
  select(sgg_cd, sgg_nm, mode, total_pop, pct_total_change, pct_service_effect) |>
  pivot_wider(names_from = mode, values_from = c(pct_total_change, pct_service_effect))

cat("\n")
cat(strrep("=", 70), "\n")
cat("CAR vs TRANSIT: Where do they diverge most? (2021->2023)\n")
cat("(Sorted by gap: transit decline much worse than car)\n")
cat(strrep("=", 70), "\n\n")

car_transit |>
  filter(!is.na(pct_total_change_car), !is.na(pct_total_change_transit)) |>
  mutate(gap = pct_total_change_transit - pct_total_change_car) |>
  arrange(gap) |>
  head(15) |>
  transmute(
    sgg_nm,
    pop = comma(total_pop),
    car_pct = sprintf("%+.1f%%", pct_total_change_car),
    transit_pct = sprintf("%+.1f%%", pct_total_change_transit),
    gap = sprintf("%+.1f%%p", gap)
  ) |>
  print(n = 15)

# === Save =====================================================================

write_csv(national, path(OUTPUT_DIR, "pilot_sensitivity.csv"))
write_csv(zero_access, path(OUTPUT_DIR, "pilot_zero_access.csv"))
write_csv(urban_decomp, path(OUTPUT_DIR, "pilot_urban_rural.csv"))
write_parquet(decomp_all, path(OUTPUT_DIR, "pilot_extended_census.parquet"))

message("\n", strrep("=", 60))
message("EXTENDED PILOT COMPLETE")
message(strrep("=", 60))
