# Reproduce the one-time legacy row-ID -> stable location_id crosswalk.
#
# This script intentionally depends on the two historical snapshots because it
# documents exactly how their row-order IDs were assigned. After the migration
# is promoted, the generated CSVs remain the durable provenance artifacts and
# normal analysis must not call this script or depend on either dated input.

suppressPackageStartupMessages({
  library(dplyr)
  library(fs)
  library(openssl)
  library(readr)
  library(tidyr)
})

args <- commandArgs(trailingOnly = TRUE)
PROJECT_ROOT <- if (length(args) >= 1) path_abs(args[[1]]) else path_abs(".")
OUTPUT_DIR <- if (length(args) >= 2) {
  path_abs(args[[2]])
} else {
  path(PROJECT_ROOT, "workflows/results/id_migration_20260712")
}

source(path(PROJECT_ROOT, "scripts/_bank_locations.R"))

sha256_file <- function(file) {
  connection <- base::file(file, open = "rb")
  on.exit(close(connection), add = TRUE)
  as.character(openssl::sha256(connection))
}

legacy_inputs <- tibble::tribble(
  ~id_scheme, ~filename, ~expected_sha256,
  "counterfactual_ttm_to_id", "bank_4_fin_251124.csv",
  "8f13ea6b1593e2837f9beea05f7c99260ac11b85eda1759558918d9e18b72760",
  "actual_ttm_to_id", "bank_4_fin_251225.csv",
  "9d2db762c12cd1425bbf7c7c9bbb5521bcd3964ed87834a44b2a79d573ca765a"
) |>
  mutate(file = path(PROJECT_ROOT, "data/bank", filename))

if (any(!file_exists(legacy_inputs$file))) {
  stop("A required historical input is missing: ",
       paste(legacy_inputs$file[!file_exists(legacy_inputs$file)], collapse = ", "))
}

legacy_inputs <- legacy_inputs |>
  rowwise() |>
  mutate(observed_sha256 = sha256_file(file)) |>
  ungroup()

if (any(legacy_inputs$observed_sha256 != legacy_inputs$expected_sha256)) {
  stop("Historical bank input SHA-256 mismatch; refusing to build crosswalk")
}

read_legacy_locations <- function(file, reference_date_value, id_column) {
  locations <- read_csv(file, show_col_types = FALSE) |>
    make_bank_locations(reference_date_value) |>
    mutate(legacy_id = as.character(row_number())) |>
    select(reference_date, location_id, bank_name, branch_name,
           longitude, latitude, legacy_id)
  names(locations)[names(locations) == "legacy_id"] <- id_column
  locations
}

actual_file <- legacy_inputs$file[legacy_inputs$id_scheme == "actual_ttm_to_id"]
counterfactual_file <- legacy_inputs$file[
  legacy_inputs$id_scheme == "counterfactual_ttm_to_id"
]
reference_dates <- c("2020h2", "2021h2", "2022h2", "2023h2")

actual <- bind_rows(lapply(reference_dates, function(reference_date_value) {
  read_legacy_locations(actual_file, reference_date_value, "actual_ttm_to_id")
}))
counterfactual <- bind_rows(lapply(reference_dates[1:3], function(reference_date_value) {
  read_legacy_locations(
    counterfactual_file,
    reference_date_value,
    "counterfactual_ttm_to_id"
  ) |>
    select(reference_date, location_id, counterfactual_ttm_to_id)
}))

set_check <- full_join(
  distinct(actual, reference_date, location_id) |>
    filter(reference_date != "2023h2") |>
    mutate(in_actual = TRUE),
  distinct(counterfactual, reference_date, location_id) |>
    mutate(in_counterfactual = TRUE),
  by = c("reference_date", "location_id")
)
if (anyNA(set_check$in_actual) || anyNA(set_check$in_counterfactual)) {
  stop("Historical location sets differ; row-ID translation is not one-to-one")
}

crosswalk <- actual |>
  left_join(
    counterfactual,
    by = c("reference_date", "location_id"),
    relationship = "one-to-one"
  ) |>
  select(reference_date, location_id, bank_name, branch_name,
         longitude, latitude, actual_ttm_to_id, counterfactual_ttm_to_id)

if (nrow(crosswalk) != 23406L ||
    anyDuplicated(crosswalk[c("reference_date", "location_id")])) {
  stop("Unexpected crosswalk size or duplicate stable IDs")
}

summary <- crosswalk |>
  group_by(reference_date) |>
  summarise(
    n_locations = n(),
    n_id_differences = if (all(is.na(counterfactual_ttm_to_id))) {
      NA_integer_
    } else {
      sum(actual_ttm_to_id != counterfactual_ttm_to_id, na.rm = TRUE)
    },
    .groups = "drop"
  )

expected_summary <- tibble::tribble(
  ~reference_date, ~n_locations, ~n_id_differences,
  "2020h2", 6238L, 2621L,
  "2021h2", 5932L, 2526L,
  "2022h2", 5648L, 2427L,
  "2023h2", 5588L, NA_integer_
)
if (!identical(summary, expected_summary)) {
  stop("Crosswalk summary differs from the independently verified expectation")
}

dir_create(OUTPUT_DIR, recurse = TRUE)
write_csv(crosswalk, path(OUTPUT_DIR, "bank_ttm_id_crosswalk.csv"))
write_csv(summary, path(OUTPUT_DIR, "bank_ttm_id_summary.csv"))
write_csv(
  legacy_inputs |>
    transmute(id_scheme, filename, sha256 = observed_sha256),
  path(OUTPUT_DIR, "legacy_bank_source_manifest.csv")
)

message("Crosswalk reproduced: ", nrow(crosswalk), " rows")
message("Output: ", OUTPUT_DIR)
