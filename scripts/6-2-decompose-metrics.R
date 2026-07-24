# 6-2-decompose-metrics.R
# Decomposition with multiple accessibility metrics:
#   1. Nearest bank travel time (min to closest bank)
#   2. Binary access (can reach >=1 bank within threshold?)
#   3. Cumulative opportunities at tight thresholds
# All using 2021<->2023, full decomposition

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

BANK_PERIOD <- "h2"

source(path(PROJECT_ROOT, "scripts/_bank_locations.R"))
assert_location_id_ttm_ready(RESULTS_DIR)

message(strrep("=", 60))
message("MULTI-METRIC DECOMPOSITION: 2021 <-> 2023")
message(strrep("=", 60))

# === Helper: nearest bank =====================================================

nearest_bank <- function(ttm_source, all_origins, bank_ids = NULL) {
  if (!is.null(bank_ids) && length(bank_ids) == 0) {
    return(tibble(
      origin_id = as.character(all_origins),
      nearest_tt = Inf
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
      WHERE from_id IS NOT NULL AND travel_time_p50 IS NOT NULL {id_filter}
    ),
    nearest_by_dep AS (
      SELECT CAST(from_id AS VARCHAR) AS origin_id, departure_time,
             MIN(travel_time_p50) AS min_tt
      FROM filtered GROUP BY from_id, departure_time
    )
    SELECT origin_id, AVG(min_tt) AS nearest_tt
    FROM nearest_by_dep GROUP BY origin_id
  ")
  result <- dbGetQuery(con, query)

  tibble(origin_id = as.character(all_origins)) |>
    left_join(result, by = "origin_id") |>
    mutate(nearest_tt = replace_na(nearest_tt, Inf))
}

# === Helper: count reachable ==================================================

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

banks_2021 <- make_bank_df("2020h2")
banks_2023 <- make_bank_df("2022h2")

common_location_ids <- intersect(banks_2021$location_id, banks_2023$location_id)
surv_base_ids   <- common_location_ids
surv_target_ids <- common_location_ids
n_new    <- sum(!banks_2023$location_id %in% banks_2021$location_id)
n_closed <- sum(!banks_2021$location_id %in% banks_2023$location_id)

message(glue("  2021: {nrow(banks_2021)} banks, 2023: {nrow(banks_2023)} banks"))
message(glue("  {length(common_location_ids)} survived, {n_new} new, {n_closed} closed"))

census_sf <- st_read(path(DATA_ROOT, "census/oa_bnd.gpkg"), quiet = TRUE)
all_origins <- census_sf$TOT_REG_CD

sgg_names <- st_read(path(DATA_ROOT, "census/sgg_bnd.gpkg"), quiet = TRUE) |>
  st_drop_geometry() |>
  select(sgg_cd = SIGUNGU_CD, sgg_nm = SIGUNGU_NM) |>
  mutate(
    sido = str_sub(sgg_cd, 1, 2),
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

# === Metric 1: Nearest bank travel time =======================================

message("\n[2] Metric 1: Nearest bank travel time...")

for (mode in c("car", "transit")) {
  message(glue("  {mode}..."))

  base_ttm   <- path(RESULTS_DIR, glue("ttm_{mode}_2021.parquet"))
  target_ttm <- path(RESULTS_DIR, glue("ttm_{mode}_2023.parquet"))
  fwd_pattern <- path(ROUTING_DIR, glue("ttm_{mode}_2021net_nb2023_*.parquet"))
  rev_pattern <- path(ROUTING_DIR, glue("ttm_{mode}_2023net_nb2021_*.parquet"))

  # Actual: nearest among ALL banks of that year
  act_base   <- nearest_bank(base_ttm, all_origins)
  act_target <- nearest_bank(target_ttm, all_origins)

  # CF-A = f(base_net, target_banks)
  # nearest among: survived (base IDs) + new banks (fwd routing)
  cfa_surv <- nearest_bank(base_ttm, all_origins, surv_base_ids)
  cfa_new  <- nearest_bank(fwd_pattern, all_origins)
  cfa_nearest <- pmin(cfa_surv$nearest_tt, cfa_new$nearest_tt)

  # CF-B = f(target_net, base_banks)
  cfb_surv   <- nearest_bank(target_ttm, all_origins, surv_target_ids)
  cfb_closed <- nearest_bank(rev_pattern, all_origins)
  cfb_nearest <- pmin(cfb_surv$nearest_tt, cfb_closed$nearest_tt)

  assign(glue("nearest_{mode}"), tibble(
    census_id = as.character(all_origins),
    actual_base = act_base$nearest_tt,
    actual_target = act_target$nearest_tt,
    cfa = cfa_nearest,
    cfb = cfb_nearest
  ) |>
    mutate(
      actual_base = ifelse(is.infinite(actual_base), NA_real_, actual_base),
      actual_target = ifelse(is.infinite(actual_target), NA_real_, actual_target),
      cfa = ifelse(is.infinite(cfa), NA_real_, cfa),
      cfb = ifelse(is.infinite(cfb), NA_real_, cfb)
    ))
}

# === Metric 1 Results =========================================================

message("\n[3] Nearest bank results...")

for (mode in c("car", "transit")) {
  df <- get(glue("nearest_{mode}")) |>
    inner_join(pop, by = "census_id") |>
    mutate(sgg_cd = str_sub(census_id, 1, 5)) |>
    left_join(sgg_names, by = "sgg_cd") |>
    filter(!is.na(actual_base), !is.na(actual_target))

  # National
  nat <- df |>
    summarize(
      pop = sum(population),
      base_mean = weighted.mean(actual_base, population),
      target_mean = weighted.mean(actual_target, population),
      cfa_mean = weighted.mean(cfa, population),
      cfb_mean = weighted.mean(cfb, population)
    ) |>
    mutate(
      total_change = target_mean - base_mean,
      service = cfa_mean - base_mean,
      network = cfb_mean - base_mean,
      interaction = total_change - service - network
    )

  cat("\n")
  cat(strrep("=", 70), "\n")
  cat(glue("NEAREST BANK: {toupper(mode)} (minutes, 2021->2023)"), "\n")
  cat(strrep("=", 70), "\n\n")

  cat(glue("  Mean nearest bank: {round(nat$base_mean, 2)} -> {round(nat$target_mean, 2)} min"), "\n")
  cat(glue("  ΔTotal:      {sprintf('%+.2f', nat$total_change)} min"), "\n")
  cat(glue("  ΔService:    {sprintf('%+.2f', nat$service)} min ({round(nat$service/nat$total_change*100, 1)}%)"), "\n")
  cat(glue("  ΔNetwork:    {sprintf('%+.2f', nat$network)} min ({round(nat$network/nat$total_change*100, 1)}%)"), "\n")
  cat(glue("  Interaction: {sprintf('%+.2f', nat$interaction)} min"), "\n")

  # By type
  by_type <- df |>
    group_by(type) |>
    summarize(
      pop = sum(population),
      base = weighted.mean(actual_base, population),
      target = weighted.mean(actual_target, population),
      cfa = weighted.mean(cfa, population),
      cfb = weighted.mean(cfb, population),
      .groups = "drop"
    ) |>
    mutate(
      total = target - base,
      service = cfa - base,
      network = cfb - base,
      interaction = total - service - network
    )

  cat("\n  By region:\n")
  by_type |>
    transmute(type, pop = comma(pop),
              base = sprintf("%.1f", base),
              total = sprintf("%+.2f", total),
              service = sprintf("%+.2f", service),
              network = sprintf("%+.2f", network),
              interact = sprintf("%+.2f", interaction)) |>
    print(n = 5)

  # Median and percentiles
  cat(glue("\n  Median nearest bank: {round(median(df$actual_base, na.rm=T), 1)} -> {round(median(df$actual_target, na.rm=T), 1)} min"), "\n")

  # Population with nearest > X min
  for (cutoff in c(10, 15, 20, 30, 45, 60)) {
    pop_base <- sum(df$population[df$actual_base > cutoff], na.rm = TRUE)
    pop_target <- sum(df$population[df$actual_target > cutoff], na.rm = TRUE)
    total_pop <- sum(df$population)
    cat(glue("  Nearest > {cutoff}min: {comma(pop_base)} ({round(pop_base/total_pop*100,1)}%) -> {comma(pop_target)} ({round(pop_target/total_pop*100,1)}%)"), "\n")
  }
}

# === Metric 2: Binary access ==================================================

message("\n[4] Metric 2: Binary access (>=1 bank within threshold)...")

binary_results <- list()

for (mode in c("car", "transit")) {
  thresholds <- if (mode == "car") c(5, 10, 15) else c(10, 15, 20, 30)

  for (th in thresholds) {
    message(glue("  {mode} {th}min..."))

    base_ttm   <- path(RESULTS_DIR, glue("ttm_{mode}_2021.parquet"))
    target_ttm <- path(RESULTS_DIR, glue("ttm_{mode}_2023.parquet"))
    fwd_pattern <- path(ROUTING_DIR, glue("ttm_{mode}_2021net_nb2023_*.parquet"))
    rev_pattern <- path(ROUTING_DIR, glue("ttm_{mode}_2023net_nb2021_*.parquet"))

    act_b <- count_reachable(base_ttm, th, all_origins)
    act_t <- count_reachable(target_ttm, th, all_origins)
    cfa_s <- count_reachable(base_ttm, th, all_origins, surv_base_ids)
    cfa_n <- count_reachable(fwd_pattern, th, all_origins)
    cfb_s <- count_reachable(target_ttm, th, all_origins, surv_target_ids)
    cfb_c <- count_reachable(rev_pattern, th, all_origins)

    df <- tibble(
      census_id = as.character(all_origins),
      has_base = act_b$n_banks >= 1,
      has_target = act_t$n_banks >= 1,
      has_cfa = (cfa_s$n_banks + cfa_n$n_banks) >= 1,
      has_cfb = (cfb_s$n_banks + cfb_c$n_banks) >= 1
    ) |>
      inner_join(pop, by = "census_id")

    total_pop <- sum(df$population)
    binary_results[[glue("{mode}_{th}")]] <- tibble(
      mode = mode, threshold = th,
      pop_access_base = sum(df$population[df$has_base]),
      pop_access_target = sum(df$population[df$has_target]),
      pop_access_cfa = sum(df$population[df$has_cfa]),
      pop_access_cfb = sum(df$population[df$has_cfb]),
      total_pop = total_pop
    )
  }
}

binary_all <- bind_rows(binary_results) |>
  mutate(
    pct_base = pop_access_base / total_pop * 100,
    pct_target = pop_access_target / total_pop * 100,
    pct_cfa = pop_access_cfa / total_pop * 100,
    pct_cfb = pop_access_cfb / total_pop * 100,
    lost_total = pct_base - pct_target,
    lost_service = pct_base - pct_cfa,
    lost_network = pct_base - pct_cfb
  )

cat("\n")
cat(strrep("=", 70), "\n")
cat("BINARY ACCESS: % population with >=1 bank (2021->2023)\n")
cat(strrep("=", 70), "\n\n")

binary_all |>
  transmute(
    mode, threshold = glue("{threshold}min"),
    base = sprintf("%.1f%%", pct_base),
    target = sprintf("%.1f%%", pct_target),
    lost_total = sprintf("%+.2f%%p", -lost_total),
    lost_service = sprintf("%+.2f%%p", -lost_service),
    lost_network = sprintf("%+.2f%%p", -lost_network)
  ) |>
  print(n = 20)

# === Metric 3: Tight threshold cumulative (transit 10, 15, 20min) =============

message("\n[5] Metric 3: Tight cumulative thresholds...")

tight_results <- list()

for (mode in c("transit")) {
  for (th in c(10, 15, 20)) {
    message(glue("  {mode} {th}min..."))

    base_ttm   <- path(RESULTS_DIR, glue("ttm_{mode}_2021.parquet"))
    target_ttm <- path(RESULTS_DIR, glue("ttm_{mode}_2023.parquet"))
    fwd_pattern <- path(ROUTING_DIR, glue("ttm_{mode}_2021net_nb2023_*.parquet"))
    rev_pattern <- path(ROUTING_DIR, glue("ttm_{mode}_2023net_nb2021_*.parquet"))

    act_b <- count_reachable(base_ttm, th, all_origins)
    act_t <- count_reachable(target_ttm, th, all_origins)
    cfa_s <- count_reachable(base_ttm, th, all_origins, surv_base_ids)
    cfa_n <- count_reachable(fwd_pattern, th, all_origins)
    cfb_s <- count_reachable(target_ttm, th, all_origins, surv_target_ids)
    cfb_c <- count_reachable(rev_pattern, th, all_origins)

    df <- tibble(
      census_id = as.character(all_origins),
      actual_base = act_b$n_banks,
      actual_target = act_t$n_banks,
      cfa = cfa_s$n_banks + cfa_n$n_banks,
      cfb = cfb_s$n_banks + cfb_c$n_banks
    ) |>
      inner_join(pop, by = "census_id") |>
      mutate(
        total = actual_target - actual_base,
        service = cfa - actual_base,
        network = cfb - actual_base,
        interaction = total - service - network
      )

    nat <- df |> summarize(
      total = weighted.mean(total, population),
      service = weighted.mean(service, population),
      network = weighted.mean(network, population),
      interaction = weighted.mean(interaction, population),
      base = weighted.mean(actual_base, population)
    )

    tight_results[[glue("{mode}_{th}")]] <- tibble(
      mode = mode, threshold = th,
      base = nat$base, total = nat$total,
      service = nat$service, network = nat$network,
      interaction = nat$interaction,
      pct_service = nat$service / nat$total * 100,
      pct_network = nat$network / nat$total * 100
    )
  }
}

tight_all <- bind_rows(tight_results)

cat("\n")
cat(strrep("=", 70), "\n")
cat("TIGHT THRESHOLDS: Transit cumulative (2021->2023)\n")
cat(strrep("=", 70), "\n\n")

tight_all |>
  transmute(
    threshold = glue("{threshold}min"),
    base = round(base, 1),
    total = sprintf("%+.2f", total),
    service = sprintf("%+.2f (%0.f%%)", service, pct_service),
    network = sprintf("%+.2f (%0.f%%)", network, pct_network),
    interact = sprintf("%+.3f", interaction)
  ) |>
  print(n = 10)

# === Save =====================================================================

write_csv(binary_all, path(OUTPUT_DIR, "decomp_binary_access.csv"))
write_csv(tight_all, path(OUTPUT_DIR, "decomp_tight_thresholds.csv"))

# Save nearest bank decomposition
for (mode in c("car", "transit")) {
  df <- get(glue("nearest_{mode}")) |>
    inner_join(pop, by = "census_id") |>
    mutate(sgg_cd = str_sub(census_id, 1, 5))
  write_parquet(df, path(OUTPUT_DIR, glue("decomp_nearest_{mode}.parquet")))
}

message("\n", strrep("=", 60))
message("MULTI-METRIC DECOMPOSITION COMPLETE")
message(strrep("=", 60))
