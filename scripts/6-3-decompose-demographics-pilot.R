# 6-3-decompose-demographics-pilot.R
# Extends the manuscript Q6 demographic approach to decomposition outputs.
#
# Unit: SGG district
# Outcome: total/service/network/interaction change, 2021 -> 2023
# Covariates: elderly share and population density, matching the earlier longitudinal analysis.

pacman::p_load(tidyverse, data.table, fs, glue, broom)

PROJECT_ROOT <- normalizePath(
  Sys.getenv("ACCESS_DECLINE_ROOT", unset = "."),
  winslash = "/", mustWork = TRUE
)
if (!file.exists(file.path(PROJECT_ROOT, ".gitignore"))) {
  stop("Run from the repository root or set ACCESS_DECLINE_ROOT.")
}
DATA_ROOT    <- path(PROJECT_ROOT, "data")
OUTPUT_DIR   <- path(PROJECT_ROOT, "workflows/analysis")

message(strrep("=", 70))
message("DECOMPOSITION x DEMOGRAPHICS PILOT")
message(strrep("=", 70))

# === Demographic variables, same logic as scripts/5-2-main-analyze.R =========

message("\n[1] Loading demographic variables...")

pop_total <- fread(
  path(DATA_ROOT, "census/stats/2024년기준_2021년_인구총괄(총인구).txt"),
  sep = "^", header = FALSE,
  col.names = c("year", "census_id", "stat_code", "value")
) |>
  as_tibble() |>
  filter(stat_code == "to_in_001") |>
  transmute(
    census_id = as.character(census_id),
    population = suppressWarnings(as.numeric(value))
  ) |>
  filter(!is.na(population), population > 0)

age_data <- fread(
  path(DATA_ROOT, "census/stats/2024년기준_2021년_성연령별인구.txt"),
  sep = "^", header = FALSE,
  col.names = c("year", "census_id", "stat_code", "value")
) |>
  as_tibble() |>
  mutate(census_id = as.character(census_id),
         value = suppressWarnings(as.numeric(value))) |>
  filter(!is.na(value))

elderly_codes <- paste0("in_age_0", 14:21)

pct_elderly <- age_data |>
  mutate(
    is_elderly = stat_code %in% elderly_codes,
    is_total = str_detect(stat_code, "^in_age_0[0-2][0-9]$") &
      !str_detect(stat_code, "_남자|_여자")
  ) |>
  summarize(
    pop_elderly = sum(value[is_elderly], na.rm = TRUE),
    pop_total = sum(value[is_total], na.rm = TRUE),
    .by = census_id
  ) |>
  filter(pop_total > 0) |>
  mutate(pct_elderly = pop_elderly / pop_total * 100)

pop_density <- fread(
  path(DATA_ROOT, "census/stats/2024년기준_2021년_인구총괄(인구밀도).txt"),
  sep = "^", header = FALSE,
  col.names = c("year", "census_id", "stat_code", "value")
) |>
  as_tibble() |>
  filter(stat_code == "to_in_003") |>
  transmute(
    census_id = as.character(census_id),
    pop_density = suppressWarnings(as.numeric(value))
  )

demo_sgg <- pct_elderly |>
  left_join(pop_density, by = "census_id") |>
  left_join(pop_total, by = "census_id") |>
  mutate(sgg_cd = str_sub(census_id, 1, 5)) |>
  filter(!is.na(population), population > 0) |>
  summarize(
    pct_elderly = weighted.mean(pct_elderly, population, na.rm = TRUE),
    pop_density = weighted.mean(pop_density, population, na.rm = TRUE),
    demo_pop = sum(population, na.rm = TRUE),
    .by = sgg_cd
  ) |>
  mutate(pop_density_log = log10(pop_density + 1))

message(glue("  Demographic SGGs: {nrow(demo_sgg)}"))

# === Join decomposition =======================================================

message("\n[2] Joining decomposition...")

decomp <- read_csv(path(OUTPUT_DIR, "decomp_full_district.csv"),
                   show_col_types = FALSE) |>
  mutate(sgg_cd = str_pad(as.character(sgg_cd), width = 5, pad = "0")) |>
  left_join(demo_sgg, by = "sgg_cd") |>
  filter(!is.na(pct_elderly), !is.na(pop_density_log)) |>
  mutate(
    total_pct = if_else(mean_base > 0, total_change / mean_base * 100, NA_real_),
    service_pct_base = if_else(mean_base > 0, service_effect / mean_base * 100, NA_real_),
    network_pct_base = if_else(mean_base > 0, network_effect / mean_base * 100, NA_real_),
    interaction_pct_base = if_else(mean_base > 0, interaction / mean_base * 100, NA_real_),
    service_share_loss = if_else(total_change < 0, service_effect / total_change * 100, NA_real_),
    network_share_loss = if_else(total_change < 0, network_effect / total_change * 100, NA_real_)
  )

write_csv(decomp, path(OUTPUT_DIR, "decomp_demo_sgg.csv"))
message(glue("  Joined rows: {nrow(decomp)}"))

# === Correlation table ========================================================

message("\n[3] Correlations...")

outcomes <- c(
  "total_change", "service_effect", "network_effect", "interaction",
  "total_pct", "service_pct_base", "network_pct_base", "interaction_pct_base",
  "mean_base"
)

cor_table <- decomp |>
  select(mode, all_of(outcomes), pct_elderly, pop_density_log) |>
  pivot_longer(cols = all_of(outcomes), names_to = "outcome", values_to = "y") |>
  filter(is.finite(y)) |>
  summarize(
    n = n(),
    r_elderly = cor(pct_elderly, y, use = "complete.obs"),
    r_density = cor(pop_density_log, y, use = "complete.obs"),
    .by = c(mode, outcome)
  ) |>
  arrange(mode, outcome)

write_csv(cor_table, path(OUTPUT_DIR, "decomp_demo_correlations.csv"))
print(cor_table, n = 100)

# === Simple weighted models ===================================================

message("\n[4] Weighted linear models...")

zscore <- function(x) as.numeric(scale(x))

model_data <- decomp |>
  mutate(
    z_elderly = zscore(pct_elderly),
    z_density = zscore(pop_density_log)
  )

fit_one <- function(df, outcome) {
  df <- df |> filter(is.finite(.data[[outcome]]))
  fit <- lm(
    reformulate(c("z_elderly", "z_density", "mean_base"), response = outcome),
    data = df,
    weights = total_pop
  )
  broom::tidy(fit) |>
    mutate(
      outcome = outcome,
      r2 = summary(fit)$r.squared,
      n = nrow(df),
      .before = 1
    )
}

model_table <- model_data |>
  group_split(mode) |>
  map(\(df) {
    map_dfr(c("total_change", "service_effect", "network_effect", "interaction"),
            \(out) fit_one(df, out)) |>
      mutate(mode = unique(df$mode), .before = 1)
  }) |>
  bind_rows()

write_csv(model_table, path(OUTPUT_DIR, "decomp_demo_models.csv"))
print(model_table, n = 100)

# === Typology: which component dominates district losses? =====================

message("\n[5] District typology...")

typology <- decomp |>
  filter(total_change < 0) |>
  mutate(
    service_loss = pmax(-service_effect, 0),
    network_loss = pmax(-network_effect, 0),
    total_loss = -total_change,
    dominant_component = case_when(
      service_loss >= 0.6 * (service_loss + network_loss) ~ "closure-dominant",
      network_loss >= 0.6 * (service_loss + network_loss) ~ "network-dominant",
      service_loss > 0 & network_loss > 0 ~ "mixed",
      TRUE ~ "offset/other"
    )
  ) |>
  summarize(
    n_districts = n(),
    mean_base = weighted.mean(mean_base, total_pop, na.rm = TRUE),
    total_change = weighted.mean(total_change, total_pop, na.rm = TRUE),
    service_effect = weighted.mean(service_effect, total_pop, na.rm = TRUE),
    network_effect = weighted.mean(network_effect, total_pop, na.rm = TRUE),
    pct_elderly = weighted.mean(pct_elderly, total_pop, na.rm = TRUE),
    pop_density_log = weighted.mean(pop_density_log, total_pop, na.rm = TRUE),
    total_pop = sum(total_pop, na.rm = TRUE),
    .by = c(mode, dominant_component)
  ) |>
  arrange(mode, dominant_component)

write_csv(typology, path(OUTPUT_DIR, "decomp_demo_typology.csv"))
print(typology, n = 50)

message("\nSaved:")
message("  workflows/analysis/decomp_demo_sgg.csv")
message("  workflows/analysis/decomp_demo_correlations.csv")
message("  workflows/analysis/decomp_demo_models.csv")
message("  workflows/analysis/decomp_demo_typology.csv")
message("\n", strrep("=", 70))
message("DONE")
message(strrep("=", 70))
