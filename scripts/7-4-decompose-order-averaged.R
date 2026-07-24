# 7-4-decompose-order-averaged.R
# Order-averaged (two-factor Shapley) decomposition: assign half the interaction
# to each component (Kim & Kwon 2026 averaging logic, two-factor case).
# Outputs feed manuscript Sections 3.1, 5.1, and 5.6.

pacman::p_load(tidyverse, fs)

PROJECT_ROOT <- normalizePath(
  Sys.getenv("ACCESS_DECLINE_ROOT", unset = "."),
  winslash = "/", mustWork = TRUE
)
if (!file.exists(file.path(PROJECT_ROOT, ".gitignore"))) {
  stop("Run from the repository root or set ACCESS_DECLINE_ROOT.")
}
OUTPUT_DIR   <- path(PROJECT_ROOT, "workflows/analysis")

# === National: all six ordered pairs =========================================

national <- read_csv(path(OUTPUT_DIR, "decomp_national.csv"),
                     show_col_types = FALSE) |>
  mutate(
    service_avg = service_effect + interaction / 2,
    network_avg = network_effect + interaction / 2,
    service_avg_pct = 100 * service_avg / total_change,
    network_avg_pct = 100 * network_avg / total_change
  )

write_csv(
  national |>
    select(mode, base_year, target_year, total_change,
           service_effect, network_effect, interaction,
           service_avg, network_avg, service_avg_pct, network_avg_pct),
  path(OUTPUT_DIR, "decomp_order_averaged.csv")
)

message("National order-averaged decomposition (2021→2023):")
national |>
  filter(base_year == 2021, target_year == 2023) |>
  select(mode, total_change, service_avg, network_avg,
         service_avg_pct, network_avg_pct) |>
  print()

# === District typology under both decompositions =============================
# Same rule as 6-4-decompose-typology-figures.R.

classify <- function(total_change, service_effect, network_effect) {
  service_loss <- pmax(-service_effect, 0)
  network_loss <- pmax(-network_effect, 0)
  loss_sum <- service_loss + network_loss
  case_when(
    total_change < 0 & loss_sum > 0 & service_loss >= 0.6 * loss_sum ~ "Closure-dominant loss",
    total_change < 0 & loss_sum > 0 & network_loss >= 0.6 * loss_sum ~ "Network-dominant loss",
    total_change < 0 & service_loss > 0 & network_loss > 0 ~ "Mixed loss",
    total_change < 0 ~ "Offset/other loss",
    TRUE ~ "No net loss"
  )
}

district <- read_csv(path(OUTPUT_DIR, "decomp_demo_sgg.csv"),
                     show_col_types = FALSE) |>
  mutate(
    type_directional = classify(total_change, service_effect, network_effect),
    type_averaged = classify(total_change,
                             service_effect + interaction / 2,
                             network_effect + interaction / 2),
    type_changed = type_directional != type_averaged
  )

write_csv(
  district |>
    select(sgg_cd, mode, total_change, service_effect, network_effect,
           interaction, type_directional, type_averaged, type_changed),
  path(OUTPUT_DIR, "decomp_order_averaged_typology.csv")
)

message("Mode-district classifications that change under order averaging:")
district |> count(mode, type_changed) |> print()
district |>
  filter(type_changed) |>
  select(sgg_cd, mode, type_directional, type_averaged) |>
  print(n = 20)

message("Outputs:")
message("  workflows/analysis/decomp_order_averaged.csv")
message("  workflows/analysis/decomp_order_averaged_typology.csv")
