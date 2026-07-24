# 6-2-decompose-full-2021-2023.R
# Full decomposition for 2021<->2023: CF-A and CF-B both available
#
# CF-A = f(base_net, target_banks): service effect
# CF-B = f(target_net, base_banks): network effect
# Interaction = ΔTotal - ΔService - ΔNetwork

pacman::p_load(tidyverse, sf, arrow, fs, glue, scales, duckdb, data.table)

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

BANK_PERIOD <- "h2"
THRESHOLDS  <- list(car = 10, transit = 30)
PAIRS <- list(
  list(base = 2021, target = 2023),
  list(base = 2023, target = 2021)
)

message(strrep("=", 60))
message("FULL DECOMPOSITION: 2021 <-> 2023")
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
  message(glue("  {yr}: {nrow(banks_by_year[[as.character(yr)]])} banks"))
}

census_sf <- st_read(path(DATA_ROOT, "census/oa_bnd.gpkg"), quiet = TRUE)
all_origin_ids <- census_sf$TOT_REG_CD

sgg_names <- st_read(path(DATA_ROOT, "census/sgg_bnd.gpkg"), quiet = TRUE) |>
  st_drop_geometry() |>
  select(sgg_cd = SIGUNGU_CD, sgg_nm = SIGUNGU_NM) |>
  mutate(
    sido = str_sub(sgg_cd, 1, 2),
    sido_nm = case_when(
      sido == "11" ~ "서울", sido == "21" ~ "부산", sido == "22" ~ "대구",
      sido == "23" ~ "인천", sido == "24" ~ "광주", sido == "25" ~ "대전",
      sido == "26" ~ "울산", sido == "29" ~ "세종", sido == "31" ~ "경기",
      sido == "32" ~ "강원", sido == "33" ~ "충북", sido == "34" ~ "충남",
      sido == "35" ~ "전북", sido == "36" ~ "전남", sido == "37" ~ "경북",
      sido == "38" ~ "경남", sido == "39" ~ "제주", TRUE ~ "기타"
    ),
    type = case_when(
      sido == "11" ~ "Seoul",
      sido %in% c("21","22","23","24","25","26") ~ "Metropolitan",
      TRUE ~ "Non-metro"
    )
  )

pop <- fread(
  path(DATA_ROOT, "census/stats", "2024년기준_2021년_인구총괄(총인구).txt"),
  sep = "^", header = FALSE,
  col.names = c("year", "census_id", "stat_code", "value")
) |> as_tibble() |>
  filter(stat_code == "to_in_001") |>
  transmute(census_id = as.character(census_id),
            population = suppressWarnings(as.numeric(value))) |>
  filter(!is.na(population), population > 0)

avg_age <- fread(
  path(DATA_ROOT, "census/stats", "2024년기준_2021년_인구총괄(평균나이).txt"),
  sep = "^", header = FALSE,
  col.names = c("year", "census_id", "stat_code", "value")
) |> as_tibble() |>
  transmute(census_id = as.character(census_id),
            avg_age = suppressWarnings(as.numeric(value))) |>
  filter(!is.na(avg_age))

message(glue("  {comma(length(all_origin_ids))} tracts, {comma(nrow(pop))} with population"))

# === Decomposition ============================================================

message("\n[2] Computing full decomposition...")

results <- list()

for (pair in PAIRS) {
  by <- pair$base
  ty <- pair$target

  base_df   <- banks_by_year[[as.character(by)]]
  target_df <- banks_by_year[[as.character(ty)]]
  common_location_ids <- intersect(base_df$location_id, target_df$location_id)

  surv_base_ids   <- common_location_ids
  surv_target_ids <- common_location_ids

  n_surv   <- length(common_location_ids)
  n_new    <- sum(!target_df$location_id %in% base_df$location_id)
  n_closed <- sum(!base_df$location_id %in% target_df$location_id)
  message(glue("\n  {by}->{ty}: {n_surv} survived, {n_new} new, {n_closed} closed"))

  for (mode in c("car", "transit")) {
    threshold <- THRESHOLDS[[mode]]

    base_ttm   <- path(RESULTS_DIR, glue("ttm_{mode}_{by}.parquet"))
    target_ttm <- path(RESULTS_DIR, glue("ttm_{mode}_{ty}.parquet"))

    # Actual
    actual_base   <- count_reachable(base_ttm, threshold, all_origin_ids)
    actual_target <- count_reachable(target_ttm, threshold, all_origin_ids)

    # CF-A = f(base_net, target_banks)
    cfa_surv <- count_reachable(base_ttm, threshold, all_origin_ids, surv_base_ids)
    fwd_pattern <- path(ROUTING_DIR, glue("ttm_{mode}_{by}net_nb{ty}_*.parquet"))
    cfa_new  <- count_reachable(fwd_pattern, threshold, all_origin_ids)
    cfa <- cfa_surv$n_banks + cfa_new$n_banks

    # CF-B = f(target_net, base_banks)
    cfb_surv <- count_reachable(target_ttm, threshold, all_origin_ids, surv_target_ids)
    rev_pattern <- path(ROUTING_DIR, glue("ttm_{mode}_{ty}net_nb{by}_*.parquet"))
    cfb_closed <- count_reachable(rev_pattern, threshold, all_origin_ids)
    cfb <- cfb_surv$n_banks + cfb_closed$n_banks

    decomp <- tibble(
      census_id     = as.character(all_origin_ids),
      actual_base   = actual_base$n_banks,
      actual_target = actual_target$n_banks,
      cfa           = cfa,
      cfb           = cfb
    ) |>
      inner_join(pop, by = "census_id") |>
      inner_join(avg_age, by = "census_id") |>
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

    results[[glue("{mode}_{by}_{ty}")]] <- decomp
    message(glue("  {mode} {by}->{ty}: {comma(nrow(decomp))} tracts"))
  }
}

decomp_all <- bind_rows(results)

# === National summary =========================================================

message("\n[3] National summary...")

national <- decomp_all |>
  group_by(mode, base_year, target_year) |>
  summarize(
    n_tracts       = n(),
    total_pop      = sum(population),
    mean_base      = weighted.mean(actual_base, population),
    mean_target    = weighted.mean(actual_target, population),
    mean_cfa       = weighted.mean(cfa, population),
    mean_cfb       = weighted.mean(cfb, population),
    total_change   = weighted.mean(total_change, population),
    service_effect = weighted.mean(service_effect, population),
    network_effect = weighted.mean(network_effect, population),
    interaction    = weighted.mean(interaction, population),
    .groups = "drop"
  ) |>
  mutate(
    pct_service = service_effect / total_change * 100,
    pct_network = network_effect / total_change * 100,
    pct_interact = interaction / total_change * 100
  )

cat("\n")
cat(strrep("=", 70), "\n")
cat("FULL DECOMPOSITION: 2021 <-> 2023 (population-weighted)\n")
cat(strrep("=", 70), "\n\n")

for (i in seq_len(nrow(national))) {
  r <- national[i, ]
  cat(glue("{r$mode} {r$base_year}->{r$target_year}"), "\n")
  cat(glue("  Base: {round(r$mean_base, 2)}  Target: {round(r$mean_target, 2)}"), "\n")
  cat(glue("  CF-A: {round(r$mean_cfa, 2)}  CF-B: {round(r$mean_cfb, 2)}"), "\n")
  cat(glue("  ΔTotal:       {sprintf('%+.3f', r$total_change)}"), "\n")
  cat(glue("  ΔService:     {sprintf('%+.3f', r$service_effect)}  ({round(r$pct_service, 1)}%)"), "\n")
  cat(glue("  ΔNetwork:     {sprintf('%+.3f', r$network_effect)}  ({round(r$pct_network, 1)}%)"), "\n")
  cat(glue("  Interaction:  {sprintf('%+.3f', r$interaction)}  ({round(r$pct_interact, 1)}%)"), "\n\n")
}

# === Urban/Rural breakdown ====================================================

message("[4] Urban vs Rural...")

urban_decomp <- decomp_all |>
  left_join(sgg_names |> select(sgg_cd, type, sido_nm), by = "sgg_cd") |>
  filter(base_year == 2021, target_year == 2023) |>
  group_by(type, mode) |>
  summarize(
    total_pop      = sum(population),
    mean_base      = weighted.mean(actual_base, population),
    total_change   = weighted.mean(total_change, population),
    service_effect = weighted.mean(service_effect, population),
    network_effect = weighted.mean(network_effect, population),
    interaction    = weighted.mean(interaction, population),
    .groups = "drop"
  )

cat(strrep("=", 70), "\n")
cat("URBAN vs RURAL (2021->2023)\n")
cat(strrep("=", 70), "\n\n")

for (m in c("transit", "car")) {
  cat(glue("--- {toupper(m)} ---"), "\n")
  urban_decomp |>
    filter(mode == m) |>
    transmute(type, pop = comma(total_pop), base = round(mean_base, 1),
              total = sprintf("%+.2f", total_change),
              service = sprintf("%+.2f", service_effect),
              network = sprintf("%+.2f", network_effect),
              interact = sprintf("%+.2f", interaction)) |>
    print(n = 5)
  cat("\n")
}

# === Sido-level ===============================================================

message("[5] Sido-level...")

sido_decomp <- decomp_all |>
  left_join(sgg_names |> select(sgg_cd, sido_nm), by = "sgg_cd") |>
  filter(base_year == 2021, target_year == 2023) |>
  group_by(sido_nm, mode) |>
  summarize(
    total_pop      = sum(population),
    mean_base      = weighted.mean(actual_base, population),
    total_change   = weighted.mean(total_change, population),
    service_effect = weighted.mean(service_effect, population),
    network_effect = weighted.mean(network_effect, population),
    interaction    = weighted.mean(interaction, population),
    .groups = "drop"
  )

cat(strrep("=", 70), "\n")
cat("SIDO-LEVEL (2021->2023, transit 30min)\n")
cat(strrep("=", 70), "\n\n")

sido_decomp |>
  filter(mode == "transit") |>
  arrange(total_change) |>
  transmute(sido_nm, pop = comma(total_pop), base = round(mean_base, 1),
            total = sprintf("%+.2f", total_change),
            service = sprintf("%+.2f", service_effect),
            network = sprintf("%+.2f", network_effect),
            interact = sprintf("%+.2f", interaction)) |>
  print(n = 20)

# === Age quintiles ============================================================

message("\n[6] Aging analysis...")

age_decomp <- decomp_all |>
  filter(base_year == 2021, target_year == 2023) |>
  mutate(age_quintile = ntile(avg_age, 5)) |>
  group_by(age_quintile, mode) |>
  summarize(
    total_pop      = sum(population),
    mean_age       = weighted.mean(avg_age, population),
    mean_base      = weighted.mean(actual_base, population),
    total_change   = weighted.mean(total_change, population),
    service_effect = weighted.mean(service_effect, population),
    network_effect = weighted.mean(network_effect, population),
    interaction    = weighted.mean(interaction, population),
    .groups = "drop"
  )

cat("\n")
cat(strrep("=", 70), "\n")
cat("AGING × DECOMPOSITION (2021->2023, transit 30min)\n")
cat(strrep("=", 70), "\n\n")

age_decomp |>
  filter(mode == "transit") |>
  transmute(Q = age_quintile, age = round(mean_age, 1),
            pop = comma(total_pop), base = round(mean_base, 1),
            total = sprintf("%+.2f", total_change),
            service = sprintf("%+.2f", service_effect),
            network = sprintf("%+.2f", network_effect),
            interact = sprintf("%+.2f", interaction)) |>
  print(n = 5)

cat("\nSame for CAR:\n\n")

age_decomp |>
  filter(mode == "car") |>
  transmute(Q = age_quintile, age = round(mean_age, 1),
            pop = comma(total_pop), base = round(mean_base, 1),
            total = sprintf("%+.2f", total_change),
            service = sprintf("%+.2f", service_effect),
            network = sprintf("%+.2f", network_effect),
            interact = sprintf("%+.2f", interaction)) |>
  print(n = 5)

# === Top districts by each component ==========================================

message("\n[7] Top districts by component...")

district_decomp <- decomp_all |>
  filter(base_year == 2021, target_year == 2023) |>
  group_by(sgg_cd, mode) |>
  summarize(
    total_pop      = sum(population),
    mean_base      = weighted.mean(actual_base, population),
    total_change   = weighted.mean(total_change, population),
    service_effect = weighted.mean(service_effect, population),
    network_effect = weighted.mean(network_effect, population),
    interaction    = weighted.mean(interaction, population),
    .groups = "drop"
  ) |>
  left_join(sgg_names, by = "sgg_cd")

cat("\n")
cat(strrep("=", 70), "\n")
cat("TOP 10: Worst NETWORK effect (transit, 2021->2023)\n")
cat(strrep("=", 70), "\n\n")

district_decomp |>
  filter(mode == "transit") |>
  arrange(network_effect) |>
  head(10) |>
  transmute(sgg_nm, sido_nm, pop = comma(total_pop),
            total = sprintf("%+.1f", total_change),
            service = sprintf("%+.1f", service_effect),
            network = sprintf("%+.1f", network_effect),
            interact = sprintf("%+.1f", interaction)) |>
  print(n = 10)

cat("\nTOP 10: Worst SERVICE effect (transit, 2021->2023)\n\n")

district_decomp |>
  filter(mode == "transit") |>
  arrange(service_effect) |>
  head(10) |>
  transmute(sgg_nm, sido_nm, pop = comma(total_pop),
            total = sprintf("%+.1f", total_change),
            service = sprintf("%+.1f", service_effect),
            network = sprintf("%+.1f", network_effect),
            interact = sprintf("%+.1f", interaction)) |>
  print(n = 10)

cat("\nTOP 10: Largest INTERACTION (transit, 2021->2023)\n\n")

district_decomp |>
  filter(mode == "transit") |>
  arrange(interaction) |>
  head(10) |>
  transmute(sgg_nm, sido_nm, pop = comma(total_pop),
            total = sprintf("%+.1f", total_change),
            service = sprintf("%+.1f", service_effect),
            network = sprintf("%+.1f", network_effect),
            interact = sprintf("%+.1f", interaction)) |>
  print(n = 10)

# === Save =====================================================================

write_csv(national, path(OUTPUT_DIR, "decomp_full_national.csv"))
write_csv(sido_decomp, path(OUTPUT_DIR, "decomp_full_sido.csv"))
write_csv(as.data.frame(district_decomp), path(OUTPUT_DIR, "decomp_full_district.csv"))
write_parquet(decomp_all, path(OUTPUT_DIR, "decomp_full_census.parquet"))

message(glue("\nSaved: decomp_full_national.csv"))
message(glue("Saved: decomp_full_district.csv ({comma(nrow(district_decomp))} rows)"))
message(glue("Saved: decomp_full_census.parquet ({comma(nrow(decomp_all))} rows)"))

message("\n", strrep("=", 60))
message("FULL DECOMPOSITION COMPLETE")
message(strrep("=", 60))
