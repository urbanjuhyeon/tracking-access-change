# 6-2-decompose-pilot-aging.R
# Cross-analysis: aging population × accessibility decomposition
# + car ownership proxy (car vs transit gap)
# + sido-level breakdown

pacman::p_load(tidyverse, sf, arrow, fs, glue, scales, data.table)

PROJECT_ROOT <- normalizePath(
  Sys.getenv("ACCESS_DECLINE_ROOT", unset = "."),
  winslash = "/", mustWork = TRUE
)
if (!file.exists(file.path(PROJECT_ROOT, ".gitignore"))) {
  stop("Run from the repository root or set ACCESS_DECLINE_ROOT.")
}
DATA_ROOT    <- path(PROJECT_ROOT, "data")
OUTPUT_DIR   <- path(PROJECT_ROOT, "workflows/analysis")

message(strrep("=", 60))
message("AGING × ACCESSIBILITY DECOMPOSITION")
message(strrep("=", 60))

# === Load decomposition results ===============================================

decomp <- read_parquet(path(OUTPUT_DIR, "pilot_extended_census.parquet"))

# Focus on main cutoffs
decomp_main <- decomp |>
  filter(
    (mode == "car" & cutoff == 10) |
    (mode == "transit" & cutoff == 30)
  )

# === Load aging data ==========================================================

message("\n[1] Loading demographic data...")

avg_age <- fread(
  path(DATA_ROOT, "census/stats", "2024년기준_2021년_인구총괄(평균나이).txt"),
  sep = "^", header = FALSE,
  col.names = c("year", "census_id", "stat_code", "value")
) |> as_tibble() |>
  transmute(census_id = as.character(census_id),
            avg_age = suppressWarnings(as.numeric(value))) |>
  filter(!is.na(avg_age))

aging_index <- fread(
  path(DATA_ROOT, "census/stats", "2024년기준_2021년_인구총괄(노령화지수).txt"),
  sep = "^", header = FALSE,
  col.names = c("year", "census_id", "stat_code", "value")
) |> as_tibble() |>
  transmute(census_id = as.character(census_id),
            aging_idx = suppressWarnings(as.numeric(value))) |>
  filter(!is.na(aging_idx))

message(glue("  Average age: {comma(nrow(avg_age))} tracts"))
message(glue("  Aging index: {comma(nrow(aging_index))} tracts"))

# === SGG names ================================================================

sgg_names <- st_read(path(DATA_ROOT, "census/sgg_bnd.gpkg"), quiet = TRUE) |>
  st_drop_geometry() |>
  select(sgg_cd = SIGUNGU_CD, sgg_nm = SIGUNGU_NM) |>
  mutate(
    sido = str_sub(sgg_cd, 1, 2),
    sido_nm = case_when(
      sido == "11" ~ "서울",
      sido == "21" ~ "부산",
      sido == "22" ~ "대구",
      sido == "23" ~ "인천",
      sido == "24" ~ "광주",
      sido == "25" ~ "대전",
      sido == "26" ~ "울산",
      sido == "29" ~ "세종",
      sido == "31" ~ "경기",
      sido == "32" ~ "강원",
      sido == "33" ~ "충북",
      sido == "34" ~ "충남",
      sido == "35" ~ "전북",
      sido == "36" ~ "전남",
      sido == "37" ~ "경북",
      sido == "38" ~ "경남",
      sido == "39" ~ "제주",
      TRUE ~ "기타"
    )
  )

# === Merge ====================================================================

merged <- decomp_main |>
  left_join(avg_age, by = "census_id") |>
  left_join(aging_index, by = "census_id") |>
  left_join(sgg_names, by = "sgg_cd") |>
  filter(!is.na(avg_age))

# === Analysis 1: Aging quintiles × decomposition ==============================

message("\n[2] Analysis: Aging quintiles × service effect...")

merged_transit_2023 <- merged |>
  filter(mode == "transit", target_year == 2023)

merged_transit_2023 <- merged_transit_2023 |>
  mutate(age_quintile = ntile(avg_age, 5))

age_labels <- merged_transit_2023 |>
  group_by(age_quintile) |>
  summarize(
    age_range = glue("{round(min(avg_age),0)}-{round(max(avg_age),0)}세"),
    .groups = "drop"
  )

quintile_decomp <- merged_transit_2023 |>
  group_by(age_quintile) |>
  summarize(
    total_pop      = sum(population),
    mean_age       = weighted.mean(avg_age, population),
    mean_base      = weighted.mean(actual_base, population),
    mean_target    = weighted.mean(actual_target, population),
    total_change   = weighted.mean(total_change, population),
    service_effect = weighted.mean(service_effect, population),
    residual       = weighted.mean(residual, population),
    pct_service    = service_effect / total_change * 100,
    pct_zero_base  = sum(population[actual_base == 0]) / sum(population) * 100,
    pct_zero_target = sum(population[actual_target == 0]) / sum(population) * 100,
    .groups = "drop"
  ) |>
  left_join(age_labels, by = "age_quintile")

cat("\n")
cat(strrep("=", 70), "\n")
cat("AGING × TRANSIT ACCESSIBILITY (30min, 2021->2023)\n")
cat("Quintile 1 = youngest communities, 5 = oldest\n")
cat(strrep("=", 70), "\n\n")

quintile_decomp |>
  transmute(
    Q = age_quintile,
    age_range,
    pop = comma(total_pop),
    mean_age = round(mean_age, 1),
    base = round(mean_base, 1),
    total = sprintf("%+.2f", total_change),
    service = sprintf("%+.2f", service_effect),
    svc_pct = sprintf("%.0f%%", pct_service),
    zero_21 = sprintf("%.1f%%", pct_zero_base),
    zero_23 = sprintf("%.1f%%", pct_zero_target)
  ) |>
  print(n = 5)

# Same for car
merged_car_2023 <- merged |>
  filter(mode == "car", target_year == 2023) |>
  mutate(age_quintile = ntile(avg_age, 5))

quintile_car <- merged_car_2023 |>
  group_by(age_quintile) |>
  summarize(
    total_pop      = sum(population),
    mean_age       = weighted.mean(avg_age, population),
    mean_base      = weighted.mean(actual_base, population),
    total_change   = weighted.mean(total_change, population),
    service_effect = weighted.mean(service_effect, population),
    residual       = weighted.mean(residual, population),
    .groups = "drop"
  ) |>
  left_join(age_labels, by = "age_quintile")

cat("\nSame for CAR (10min):\n\n")

quintile_car |>
  transmute(
    Q = age_quintile,
    age_range,
    mean_age = round(mean_age, 1),
    base = round(mean_base, 1),
    total = sprintf("%+.2f", total_change),
    service = sprintf("%+.2f", service_effect),
    residual = sprintf("%+.2f", residual)
  ) |>
  print(n = 5)

# === Analysis 2: Sido-level decomposition =====================================

message("\n[3] Analysis: Sido-level decomposition...")

sido_decomp <- merged |>
  filter(target_year == 2023) |>
  group_by(sido_nm, mode) |>
  summarize(
    total_pop      = sum(population),
    mean_base      = weighted.mean(actual_base, population),
    mean_target    = weighted.mean(actual_target, population),
    total_change   = weighted.mean(total_change, population),
    service_effect = weighted.mean(service_effect, population),
    residual       = weighted.mean(residual, population),
    pct_service    = service_effect / total_change * 100,
    mean_age       = weighted.mean(avg_age, population, na.rm = TRUE),
    .groups = "drop"
  )

cat("\n")
cat(strrep("=", 70), "\n")
cat("SIDO-LEVEL DECOMPOSITION (2021->2023)\n")
cat(strrep("=", 70), "\n\n")

cat("--- TRANSIT 30min ---\n")
sido_decomp |>
  filter(mode == "transit") |>
  arrange(service_effect) |>
  transmute(
    sido_nm,
    pop = comma(total_pop),
    age = round(mean_age, 1),
    base = round(mean_base, 1),
    total = sprintf("%+.2f", total_change),
    service = sprintf("%+.2f", service_effect),
    svc_pct = sprintf("%.0f%%", pct_service),
    residual = sprintf("%+.2f", residual)
  ) |>
  print(n = 20)

cat("\n--- CAR 10min ---\n")
sido_decomp |>
  filter(mode == "car") |>
  arrange(service_effect) |>
  transmute(
    sido_nm,
    pop = comma(total_pop),
    age = round(mean_age, 1),
    base = round(mean_base, 1),
    total = sprintf("%+.2f", total_change),
    service = sprintf("%+.2f", service_effect),
    svc_pct = sprintf("%.0f%%", pct_service),
    residual = sprintf("%+.2f", residual)
  ) |>
  print(n = 20)

# === Analysis 3: Car vs Transit gap by aging ==================================

message("\n[4] Analysis: Modal gap × aging...")

modal_gap <- merged |>
  filter(target_year == 2023) |>
  select(census_id, mode, total_change, service_effect, actual_base,
         actual_target, population, avg_age, sgg_cd) |>
  pivot_wider(
    names_from = mode,
    values_from = c(total_change, service_effect, actual_base, actual_target)
  ) |>
  mutate(
    age_quintile = ntile(avg_age, 5),
    modal_gap_total = total_change_transit - total_change_car,
    modal_gap_service = service_effect_transit - service_effect_car,
    pct_change_car = ifelse(actual_base_car > 0,
      total_change_car / actual_base_car * 100, NA),
    pct_change_transit = ifelse(actual_base_transit > 0,
      total_change_transit / actual_base_transit * 100, NA)
  )

gap_by_age <- modal_gap |>
  group_by(age_quintile) |>
  summarize(
    mean_age = weighted.mean(avg_age, population),
    pop = sum(population),
    car_pct = weighted.mean(pct_change_car, population, na.rm = TRUE),
    transit_pct = weighted.mean(pct_change_transit, population, na.rm = TRUE),
    gap = transit_pct - car_pct,
    .groups = "drop"
  )

cat("\n")
cat(strrep("=", 70), "\n")
cat("MODAL GAP BY AGE QUINTILE (% change, 2021->2023)\n")
cat("Negative gap = transit declined more than car\n")
cat(strrep("=", 70), "\n\n")

gap_by_age |>
  transmute(
    Q = age_quintile,
    mean_age = round(mean_age, 1),
    pop = comma(pop),
    car = sprintf("%+.1f%%", car_pct),
    transit = sprintf("%+.1f%%", transit_pct),
    gap = sprintf("%+.1f%%p", gap)
  ) |>
  print(n = 5)

# === Analysis 4: Population newly losing all access ===========================

message("\n[5] Analysis: Newly zero-access by aging...")

newly_zero <- merged |>
  filter(target_year == 2023, mode == "transit") |>
  mutate(
    was_accessible = actual_base > 0,
    now_zero = actual_target == 0,
    lost_all = was_accessible & now_zero,
    age_quintile = ntile(avg_age, 5)
  )

zero_by_age <- newly_zero |>
  group_by(age_quintile) |>
  summarize(
    total_pop = sum(population),
    pop_lost_all = sum(population[lost_all]),
    pct_lost = pop_lost_all / total_pop * 100,
    mean_age = weighted.mean(avg_age, population),
    .groups = "drop"
  )

cat("\n")
cat(strrep("=", 70), "\n")
cat("NEWLY ZERO-ACCESS BY AGE (transit 30min, 2021->2023)\n")
cat("= people who HAD access in 2021 but LOST ALL by 2023\n")
cat(strrep("=", 70), "\n\n")

zero_by_age |>
  transmute(
    Q = age_quintile,
    mean_age = round(mean_age, 1),
    total_pop = comma(total_pop),
    lost_all = comma(pop_lost_all),
    pct = sprintf("%.2f%%", pct_lost)
  ) |>
  print(n = 5)

# === Save =====================================================================

write_csv(quintile_decomp, path(OUTPUT_DIR, "pilot_aging_quintiles.csv"))
write_csv(sido_decomp, path(OUTPUT_DIR, "pilot_sido_decomp.csv"))
write_csv(as.data.frame(gap_by_age), path(OUTPUT_DIR, "pilot_modal_gap_age.csv"))

message("\n", strrep("=", 60))
message("AGING ANALYSIS COMPLETE")
message(strrep("=", 60))
