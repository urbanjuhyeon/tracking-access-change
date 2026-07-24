# Post-promotion validation for the stable bank-location ID migration.

suppressPackageStartupMessages({
  library(arrow)
  library(dplyr)
  library(fs)
  library(openssl)
  library(readr)
})

args <- commandArgs(trailingOnly = TRUE)
PROJECT_ROOT <- if (length(args) >= 1) path_abs(args[[1]]) else path_abs(".")
OLD_DECOMP <- if (length(args) >= 2) path_abs(args[[2]]) else NA_character_
RESULTS_DIR <- path(PROJECT_ROOT, "workflows/results")
MIGRATION_DIR <- path(RESULTS_DIR, "id_migration_20260712")

sha256_file <- function(file) {
  connection <- base::file(file, open = "rb")
  on.exit(close(connection), add = TRUE)
  as.character(openssl::sha256(connection))
}

stopifnot(
  file_exists(path(RESULTS_DIR, "ID_SCHEME_LOCATION_V1")),
  !file_exists(path(RESULTS_DIR, "ID_MIGRATION_IN_PROGRESS")),
  sha256_file(path(PROJECT_ROOT, "data/bank/bank_4_fin.csv")) ==
    "9d2db762c12cd1425bbf7c7c9bbb5521bcd3964ed87834a44b2a79d573ca765a"
)

actual_files <- dir_ls(RESULTS_DIR, type = "file")
actual_files <- actual_files[
  grepl("^ttm_(car|transit|walk|bicycle)_20(21|22|23|24)\\.parquet$",
        path_file(actual_files))
]
counterfactual_files <- dir_ls(
  path(RESULTS_DIR, "decompose/routing"),
  type = "file"
)
counterfactual_files <- counterfactual_files[
  grepl("^ttm_(car|transit)_20[0-9]{2}net_nb20[0-9]{2}_[0-9]{5}\\.parquet$",
        path_file(counterfactual_files))
]
stopifnot(length(actual_files) == 16L, length(counterfactual_files) == 2940L)

report <- read_csv(
  path(MIGRATION_DIR, "ttm_staging_validation.csv"),
  show_col_types = FALSE
)
report_exact <- read_csv(
  path(MIGRATION_DIR, "ttm_staging_validation.csv"),
  col_types = cols(.default = col_character()),
  progress = FALSE
)
nonempty_report_rows <- as.numeric(report_exact$staged_n_rows) > 0
stopifnot(
  nrow(report) == 2956L,
  sum(report$kind == "actual") == 16L,
  sum(report$kind == "counterfactual") == 2940L,
  all(report$verified),
  all(report$raw_n_rows == report$staged_n_rows),
  sum(report$raw_n_rows) == 1671751395,
  all(report$n_null_to_id == 0),
  all(report$n_orientation_swaps[report$kind == "counterfactual"] == 0),
  all(grepl("^[0-9a-f]{64}$", report$input_sha256)),
  all(grepl("^[0-9a-f]{64}$", report$staged_sha256)),
  all(grepl("^[0-9]+$", report_exact$input_bytes)),
  all(grepl("^[0-9]+$", report_exact$staged_bytes)),
  all(grepl("^[0-9]+$", report_exact$xor_hash[nonempty_report_rows])),
  all(grepl("^[0-9]+$", report_exact$sum_hash[nonempty_report_rows]))
)

expected_swaps <- c(
  ttm_bicycle_2021.parquet = 1421305,
  ttm_bicycle_2022.parquet = 1459602,
  ttm_bicycle_2023.parquet = 1458830,
  ttm_bicycle_2024.parquet = 1660909,
  ttm_car_2021.parquet = 0,
  ttm_car_2022.parquet = 0,
  ttm_car_2023.parquet = 0,
  ttm_car_2024.parquet = 0,
  ttm_transit_2021.parquet = 0,
  ttm_transit_2022.parquet = 0,
  ttm_transit_2023.parquet = 0,
  ttm_transit_2024.parquet = 0,
  ttm_walk_2021.parquet = 714319,
  ttm_walk_2022.parquet = 721112,
  ttm_walk_2023.parquet = 699521,
  ttm_walk_2024.parquet = 731021
)
actual_report <- report |>
  filter(kind == "actual") |>
  mutate(filename = path_file(input_file)) |>
  arrange(filename)
stopifnot(
  identical(actual_report$filename, sort(names(expected_swaps))),
  identical(
    as.numeric(actual_report$n_orientation_swaps),
    as.numeric(expected_swaps[actual_report$filename])
  )
)

national <- read_csv(
  path(PROJECT_ROOT, "workflows/analysis/decomp_full_national.csv"),
  show_col_types = FALSE
)
expected_forward <- tibble::tribble(
  ~mode, ~total_change, ~service_effect, ~network_effect, ~interaction,
  "car", -10.391412041627914, -11.631928758998379,
    0.6115613974088829, 0.628955319961581,
  "transit", -9.031504128426691, -6.220409723045144,
    -3.1238912508611567, 0.31279684547960823
)
observed_forward <- national |>
  filter(base_year == 2021, target_year == 2023) |>
  arrange(mode) |>
  select(mode, total_change, service_effect, network_effect, interaction)
expected_forward <- arrange(expected_forward, mode)
stopifnot(
  identical(observed_forward$mode, expected_forward$mode),
  max(abs(as.matrix(observed_forward[-1]) - as.matrix(expected_forward[-1]))) < 1e-12,
  max(abs(national$total_change - national$service_effect -
          national$network_effect - national$interaction)) < 1e-12
)

decomp <- read_parquet(path(PROJECT_ROOT, "workflows/analysis/decomp_full_census.parquet"))
core_columns <- c(
  "census_id", "actual_base", "actual_target", "cfa", "cfb",
  "total_change", "service_effect", "network_effect", "interaction",
  "mode", "base_year", "target_year"
)
stopifnot(
  nrow(decomp) == 413124L,
  !anyNA(decomp[core_columns]),
  max(abs(decomp$total_change - decomp$service_effect -
          decomp$network_effect - decomp$interaction)) < 1e-12,
  nrow(distinct(decomp, census_id, mode, base_year, target_year)) == nrow(decomp)
)

forward <- decomp |>
  filter(base_year == 2021, target_year == 2023) |>
  select(census_id, mode, ends_with("_base"), ends_with("_target"),
         cfa, cfb, interaction)
reverse <- decomp |>
  filter(base_year == 2023, target_year == 2021) |>
  select(census_id, mode, ends_with("_base"), ends_with("_target"),
         cfa, cfb, interaction)
symmetry <- inner_join(forward, reverse, by = c("census_id", "mode"),
                       suffix = c("_fwd", "_rev"))
stopifnot(
  nrow(symmetry) == 206562L,
  max(abs(symmetry$actual_base_fwd - symmetry$actual_target_rev)) == 0,
  max(abs(symmetry$actual_target_fwd - symmetry$actual_base_rev)) == 0,
  max(abs(symmetry$cfa_fwd - symmetry$cfb_rev)) == 0,
  max(abs(symmetry$cfb_fwd - symmetry$cfa_rev)) == 0,
  max(abs(symmetry$interaction_fwd - symmetry$interaction_rev)) < 1e-12
)

if (!is.na(OLD_DECOMP) && file_exists(OLD_DECOMP)) {
  old <- read_parquet(OLD_DECOMP)
  comparison <- inner_join(
    select(decomp, census_id, mode, base_year, target_year,
           actual_base, actual_target, total_change, cfa, cfb),
    select(old, census_id, mode, base_year, target_year,
           actual_base, actual_target, total_change, cfa, cfb),
    by = c("census_id", "mode", "base_year", "target_year"),
    suffix = c("_new", "_old")
  )
  stopifnot(
    nrow(comparison) == nrow(decomp),
    max(abs(comparison$actual_base_new - comparison$actual_base_old)) == 0,
    max(abs(comparison$actual_target_new - comparison$actual_target_old)) == 0,
    max(abs(comparison$total_change_new - comparison$total_change_old)) == 0
  )
  message(
    "Counterfactual rows changed after correct ID mapping: ",
    sum(comparison$cfa_new != comparison$cfa_old |
        comparison$cfb_new != comparison$cfb_old),
    " / ", nrow(comparison)
  )
}

message("Location-ID migration and decomposition validation passed")
