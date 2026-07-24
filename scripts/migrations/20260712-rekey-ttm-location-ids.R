# One-time, read-preserving migration of existing TTM destination IDs.
#
# Actual TTMs were routed with row IDs from the 2025-12-25 bank CSV.
# Counterfactual TTMs were routed with row IDs from the 2025-11-24 bank CSV.
# This script translates both schemes to the canonical, row-order-invariant
# location_id and writes a fully validated staging copy. It never overwrites an
# original TTM.

suppressPackageStartupMessages({
  library(DBI)
  library(duckdb)
  library(fs)
  library(readr)
  library(dplyr)
  library(stringr)
  library(tibble)
  library(sf)
  library(openssl)
})

args <- commandArgs(trailingOnly = TRUE)
PROJECT_ROOT <- if (length(args) >= 1) path_abs(args[[1]]) else path_abs(".")
STAGE_ROOT <- if (length(args) >= 2) {
  path_abs(args[[2]])
} else {
  path("C:/tmp", "uad_ttm_stable_stage_20260712")
}

RESULTS_DIR <- path(PROJECT_ROOT, "workflows/results")
ROUTING_DIR <- path(RESULTS_DIR, "decompose/routing")
MIGRATION_DIR <- path(RESULTS_DIR, "id_migration_20260712")
CROSSWALK_FILE <- path(MIGRATION_DIR, "bank_ttm_id_crosswalk.csv")
REPORT_FILE <- path(MIGRATION_DIR, "ttm_staging_validation.csv")
RUN_SCOPE <- if (length(args) >= 3) args[[3]] else "full"
if (!RUN_SCOPE %in% c("full", "smoke")) {
  stop("Third argument must be 'full' or 'smoke'")
}

if (!file_exists(CROSSWALK_FILE)) {
  stop("Crosswalk not found: ", CROSSWALK_FILE)
}

dir_create(path(STAGE_ROOT, "actual"), recurse = TRUE)
dir_create(path(STAGE_ROOT, "counterfactual"), recurse = TRUE)

crosswalk <- read_csv(CROSSWALK_FILE, show_col_types = FALSE) |>
  mutate(
    actual_ttm_to_id = as.character(actual_ttm_to_id),
    counterfactual_ttm_to_id = as.character(counterfactual_ttm_to_id)
  )

if (anyNA(crosswalk$reference_date) || anyNA(crosswalk$location_id)) {
  stop("Crosswalk has missing reference_date or location_id")
}
if (anyDuplicated(crosswalk[c("reference_date", "location_id")])) {
  stop("Crosswalk has duplicate reference_date + location_id")
}

assert_legacy_id <- function(column) {
  values <- crosswalk |>
    filter(!is.na(.data[[column]]), nzchar(.data[[column]])) |>
    transmute(reference_date, legacy_id = .data[[column]])
  if (any(!grepl("^[0-9]+$", values$legacy_id))) {
    stop(column, " contains non-numeric legacy IDs")
  }
  if (anyDuplicated(values[c("reference_date", "legacy_id")])) {
    stop(column, " is not unique within reference_date")
  }
}
assert_legacy_id("actual_ttm_to_id")
assert_legacy_id("counterfactual_ttm_to_id")

con <- dbConnect(duckdb())
on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)
dbWriteTable(con, "bank_id_crosswalk", crosswalk, overwrite = TRUE)

origin_file <- path(PROJECT_ROOT, "data/census/oa_bnd.gpkg")
if (!file_exists(origin_file)) stop("Census origin file not found: ", origin_file)
valid_origins <- st_read(origin_file, quiet = TRUE) |>
  st_drop_geometry() |>
  transmute(from_id = as.character(TOT_REG_CD)) |>
  distinct()
if (anyNA(valid_origins$from_id) || anyDuplicated(valid_origins$from_id)) {
  stop("Census origin IDs must be non-missing and unique")
}
dbWriteTable(con, "valid_origin_ids", valid_origins, overwrite = TRUE)

sql_path <- function(x) gsub("'", "''", path_abs(x), fixed = TRUE)

actual_manifest <- tidyr::expand_grid(
  mode = c("car", "transit", "walk", "bicycle"),
  analysis_year = 2021:2024
) |>
  mutate(file = path(RESULTS_DIR, paste0("ttm_", mode, "_", analysis_year,
                                         ".parquet")))
actual_files <- actual_manifest$file
if (length(actual_files) != 16L || any(!file_exists(actual_files))) {
  stop("Actual TTM manifest is incomplete; expected exactly 16 files")
}

counterfactual_manifest <- tibble::tribble(
  ~network_year, ~bank_year, ~expected_districts,
  2021L, 2022L, 242L,
  2021L, 2023L, 242L,
  2022L, 2021L, 246L,
  2022L, 2023L, 246L,
  2023L, 2021L, 247L,
  2023L, 2022L, 247L
) |>
  tidyr::crossing(mode = c("car", "transit"))

all_counterfactual_files <- dir_ls(
  ROUTING_DIR,
  type = "file",
  glob = "*.parquet"
)
counterfactual_files <- character()
for (i in seq_len(nrow(counterfactual_manifest))) {
  item <- counterfactual_manifest[i, ]
  pattern <- paste0(
    "^ttm_", item$mode, "_", item$network_year,
    "net_nb", item$bank_year, "_[0-9]{5}\\.parquet$"
  )
  # fs::dir_ls(regexp=) applies the pattern to full paths. Filtering the base
  # name explicitly keeps the anchored manifest regex honest on every machine.
  files <- all_counterfactual_files[
    str_detect(path_file(all_counterfactual_files), pattern)
  ]
  if (length(files) != item$expected_districts) {
    stop(
      "Counterfactual manifest mismatch for ", item$mode, " ",
      item$network_year, "->", item$bank_year, ": expected ",
      item$expected_districts, ", found ", length(files)
    )
  }
  counterfactual_files <- c(counterfactual_files, files)
}
if (length(counterfactual_files) != 2940L ||
    anyDuplicated(counterfactual_files)) {
  stop("Counterfactual manifest must contain 2,940 unique files")
}

counterfactual_inventory <- tibble(file = counterfactual_files) |>
  mutate(
    name = path_file(file),
    mode = str_match(name, "^ttm_([a-z]+)_")[, 2],
    network_year = as.integer(str_match(name, "_([0-9]{4})net_nb")[, 2]),
    bank_year = as.integer(str_match(name, "net_nb([0-9]{4})_")[, 2]),
    district = str_match(name, "_([0-9]{5})\\.parquet$")[, 2]
  )
if (anyNA(counterfactual_inventory)) {
  stop("Counterfactual inventory contains an unparseable file name")
}
for (network_year_value in 2021:2023) {
  groups <- counterfactual_inventory |>
    filter(network_year == network_year_value) |>
    group_by(mode, bank_year) |>
    summarize(districts = list(sort(district)), .groups = "drop")
  reference_set <- groups$districts[[1]]
  if (!all(vapply(groups$districts, identical, logical(1), reference_set))) {
    stop("District suffix sets differ within network year ", network_year_value)
  }
}

locations_by_year <- crosswalk |>
  transmute(
    analysis_year = as.integer(substr(reference_date, 1, 4)) + 1L,
    location_id
  )
allowed_differential <- bind_rows(lapply(2021:2023, function(network_year) {
  bind_rows(lapply(setdiff(2021:2023, network_year), function(bank_year) {
    target <- locations_by_year |>
      filter(analysis_year == bank_year) |>
      pull(location_id)
    base <- locations_by_year |>
      filter(analysis_year == network_year) |>
      pull(location_id)
    tibble(
      network_year = network_year,
      bank_year = bank_year,
      location_id = setdiff(target, base)
    )
  }))
}))
if (anyDuplicated(allowed_differential)) {
  stop("Allowed differential location table contains duplicates")
}
dbWriteTable(con, "allowed_differential", allowed_differential, overwrite = TRUE)

if (RUN_SCOPE == "smoke") {
  actual_files <- path(RESULTS_DIR, "ttm_bicycle_2021.parquet")
  empty_cf <- counterfactual_files[file_size(counterfactual_files) < 2000][[1]]
  nonempty_cf <- counterfactual_files[file_size(counterfactual_files) >= 2000][[1]]
  counterfactual_files <- c(empty_cf, nonempty_cf)
  REPORT_FILE <- path(MIGRATION_DIR, "ttm_staging_validation_smoke.csv")
}

parse_actual <- function(file) {
  m <- str_match(path_file(file), "^ttm_([a-z]+)_([0-9]{4})\\.parquet$")
  if (anyNA(m)) stop("Cannot parse actual TTM name: ", file)
  list(
    mode = m[, 2],
    analysis_year = as.integer(m[, 3]),
    reference_date = paste0(as.integer(m[, 3]) - 1L, "h2")
  )
}

parse_counterfactual <- function(file) {
  m <- str_match(
    path_file(file),
    "^ttm_(car|transit)_([0-9]{4})net_nb([0-9]{4})_([0-9]{5})\\.parquet$"
  )
  if (anyNA(m)) stop("Cannot parse counterfactual TTM name: ", file)
  list(
    mode = m[, 2],
    network_year = as.integer(m[, 3]),
    bank_year = as.integer(m[, 4]),
    district = m[, 5],
    reference_date = paste0(as.integer(m[, 4]) - 1L, "h2")
  )
}

fingerprint_query <- function(source_sql, columns) {
  hash_expr <- paste(columns, collapse = ", ")
  sprintf(
    paste0(
      "SELECT COUNT(*) AS n_rows, ",
      "COUNT(DISTINCT to_id) AS n_destinations, ",
      "COUNT(*) FILTER (WHERE to_id IS NULL) AS n_null_to_id, ",
      "CAST(bit_xor(hash(%s)) AS VARCHAR) AS xor_hash, ",
      "CAST(sum(CAST(hash(%s) AS HUGEINT)) AS VARCHAR) AS sum_hash ",
      "FROM (%s)"
    ),
    hash_expr, hash_expr, source_sql
  )
}

sha256_file <- function(file) {
  connection <- base::file(file, open = "rb")
  on.exit(close(connection), add = TRUE)
  as.character(openssl::sha256(connection))
}

convert_one <- function(input_file, output_file, reference_date,
                        legacy_column, kind,
                        network_year = NA_integer_, bank_year = NA_integer_) {
  input_sql_path <- sql_path(input_file)
  output_sql_path <- sql_path(output_file)
  temp_file <- path_ext_set(output_file, "part.parquet")
  temp_sql_path <- sql_path(temp_file)

  if (file_exists(output_file) || file_exists(temp_file)) {
    stop("Staging output already exists; refusing to overwrite: ", output_file)
  }

  schema <- dbGetQuery(
    con,
    sprintf("DESCRIBE SELECT * FROM read_parquet('%s')", input_sql_path)
  )
  expected_columns <- if (kind == "actual") {
    c("from_id", "to_id", "travel_time_p50", "mode", "departure_time", "year")
  } else {
    c("from_id", "to_id", "travel_time_p50", "departure_time")
  }
  if (!identical(schema$column_name, expected_columns)) {
    stop("Unexpected schema in ", input_file, ": ",
         paste(schema$column_name, collapse = ", "))
  }

  invalid_orientation_rows <- dbGetQuery(con, sprintf(
    paste0(
      "SELECT COUNT(*) AS n FROM read_parquet('%s') t ",
      "LEFT JOIN bank_id_crosswalk mt ",
      "ON mt.reference_date = '%s' ",
      "AND CAST(t.to_id AS VARCHAR) = CAST(mt.%s AS VARCHAR) ",
      "LEFT JOIN bank_id_crosswalk mf ",
      "ON mf.reference_date = '%s' ",
      "AND CAST(t.from_id AS VARCHAR) = CAST(mf.%s AS VARCHAR) ",
      "WHERE (mt.location_id IS NULL AND mf.location_id IS NULL) ",
      "OR (mt.location_id IS NOT NULL AND mf.location_id IS NOT NULL)"
    ),
    input_sql_path, reference_date, legacy_column,
    reference_date, legacy_column
  ))$n
  if (invalid_orientation_rows != 0) {
    stop("Rows without exactly one mapped bank endpoint in ", input_file,
         ": ", invalid_orientation_rows)
  }

  n_orientation_swaps <- dbGetQuery(con, sprintf(
    paste0(
      "SELECT COUNT(*) AS n FROM read_parquet('%s') t ",
      "JOIN bank_id_crosswalk mf ",
      "ON mf.reference_date = '%s' ",
      "AND CAST(t.from_id AS VARCHAR) = CAST(mf.%s AS VARCHAR)"
    ),
    input_sql_path, reference_date, legacy_column
  ))$n

  select_columns <- if (kind == "actual") {
    paste0(
      "CAST(CASE WHEN mt.location_id IS NOT NULL THEN t.from_id ",
      "ELSE t.to_id END AS VARCHAR) AS from_id, ",
      "CAST(COALESCE(mt.location_id, mf.location_id) AS VARCHAR) AS to_id, ",
      "t.travel_time_p50, t.mode, t.departure_time, t.year"
    )
  } else {
    paste0(
      "CAST(CASE WHEN mt.location_id IS NOT NULL THEN t.from_id ",
      "ELSE t.to_id END AS VARCHAR) AS from_id, ",
      "CAST(COALESCE(mt.location_id, mf.location_id) AS VARCHAR) AS to_id, ",
      "t.travel_time_p50, t.departure_time"
    )
  }

  source_with_stable_id <- sprintf(
    paste0(
      "SELECT %s FROM read_parquet('%s') t ",
      "LEFT JOIN bank_id_crosswalk mt ",
      "ON mt.reference_date = '%s' ",
      "AND CAST(t.to_id AS VARCHAR) = CAST(mt.%s AS VARCHAR) ",
      "LEFT JOIN bank_id_crosswalk mf ",
      "ON mf.reference_date = '%s' ",
      "AND CAST(t.from_id AS VARCHAR) = CAST(mf.%s AS VARCHAR) ",
      "WHERE (mt.location_id IS NOT NULL) != (mf.location_id IS NOT NULL)"
    ),
    select_columns, input_sql_path, reference_date, legacy_column,
    reference_date, legacy_column
  )

  raw_count <- dbGetQuery(
    con,
    sprintf("SELECT COUNT(*) AS n FROM read_parquet('%s')", input_sql_path)
  )$n
  canonical_count <- dbGetQuery(
    con,
    sprintf("SELECT COUNT(*) AS n FROM (%s)", source_with_stable_id)
  )$n
  if (raw_count != canonical_count) {
    stop("Canonicalized source row count differs from raw input: ", input_file)
  }

  if (kind == "counterfactual") {
    unexpected_differential <- dbGetQuery(con, sprintf(
      paste0(
        "SELECT COUNT(DISTINCT s.to_id) AS n FROM (%s) s ",
        "LEFT JOIN allowed_differential a ",
        "ON a.network_year = %d AND a.bank_year = %d ",
        "AND s.to_id = a.location_id ",
        "WHERE a.location_id IS NULL"
      ),
      source_with_stable_id, network_year, bank_year
    ))$n
    if (unexpected_differential != 0) {
      stop("Unexpected bank locations for counterfactual pair in ", input_file)
    }
  }

  dbExecute(con, sprintf(
    "COPY (%s) TO '%s' (FORMAT PARQUET, CODEC 'SNAPPY')",
    source_with_stable_id, temp_sql_path
  ))

  staged_schema <- dbGetQuery(
    con,
    sprintf("DESCRIBE SELECT * FROM read_parquet('%s')", temp_sql_path)
  )
  if (!identical(schema$column_name, staged_schema$column_name) ||
      !identical(schema$column_type, staged_schema$column_type)) {
    stop("Non-ID schema changed after staging: ", input_file)
  }

  invalid_staged_origins <- dbGetQuery(con, sprintf(
    paste0(
      "SELECT COUNT(*) AS n FROM (",
      "SELECT DISTINCT CAST(from_id AS VARCHAR) AS from_id ",
      "FROM read_parquet('%s')",
      ") s LEFT JOIN valid_origin_ids o USING (from_id) ",
      "WHERE o.from_id IS NULL"
    ),
    temp_sql_path
  ))$n
  if (invalid_staged_origins != 0) {
    stop("Staged file contains non-census origins: ", input_file)
  }

  fingerprint_columns <- if (kind == "actual") {
    c("from_id", "to_id", "travel_time_p50", "mode", "departure_time", "year")
  } else {
    c("from_id", "to_id", "travel_time_p50", "departure_time")
  }
  original_fp <- dbGetQuery(
    con,
    fingerprint_query(source_with_stable_id, fingerprint_columns)
  )
  staged_source <- sprintf(
    "SELECT * FROM read_parquet('%s')",
    temp_sql_path
  )
  staged_fp <- dbGetQuery(
    con,
    fingerprint_query(staged_source, fingerprint_columns)
  )

  compare_columns <- c(
    "n_rows", "n_destinations", "n_null_to_id", "xor_hash", "sum_hash"
  )
  if (!identical(original_fp[compare_columns], staged_fp[compare_columns])) {
    stop("Fingerprint mismatch after staging: ", input_file)
  }
  if (staged_fp$n_rows != raw_count) {
    stop("Staged row count differs from raw input: ", input_file)
  }

  file_move(temp_file, output_file)

  input_sha256 <- sha256_file(input_file)
  staged_sha256 <- sha256_file(output_file)

  tibble(
    kind = kind,
    input_file = path_rel(input_file, PROJECT_ROOT),
    staged_file = path_abs(output_file),
    reference_date = reference_date,
    legacy_column = legacy_column,
    input_bytes = as.numeric(file_size(input_file)),
    staged_bytes = as.numeric(file_size(output_file)),
    raw_n_rows = raw_count,
    staged_n_rows = staged_fp$n_rows,
    n_destinations = original_fp$n_destinations,
    n_null_to_id = original_fp$n_null_to_id,
    n_orientation_swaps = n_orientation_swaps,
    xor_hash = original_fp$xor_hash,
    sum_hash = original_fp$sum_hash,
    input_sha256 = input_sha256,
    staged_sha256 = staged_sha256,
    verified = TRUE
  )
}

reports <- list()
report_index <- 0L

for (file in sort(actual_files)) {
  meta <- parse_actual(file)
  output <- path(STAGE_ROOT, "actual", path_file(file))
  message("[actual] ", path_file(file), " -> ", meta$reference_date)
  report_index <- report_index + 1L
  reports[[report_index]] <- convert_one(
    file, output, meta$reference_date, "actual_ttm_to_id", "actual"
  )
}

for (file in sort(counterfactual_files)) {
  meta <- parse_counterfactual(file)
  output <- path(STAGE_ROOT, "counterfactual", path_file(file))
  message("[counterfactual] ", path_file(file), " -> ", meta$reference_date)
  report_index <- report_index + 1L
  reports[[report_index]] <- convert_one(
    file, output, meta$reference_date,
    "counterfactual_ttm_to_id", "counterfactual",
    meta$network_year, meta$bank_year
  )
}

report <- bind_rows(reports)
if (nrow(report) != length(actual_files) + length(counterfactual_files)) {
  stop("Validation report row count does not match input file count")
}
if (!all(report$verified)) stop("One or more staged files failed validation")
if (any(report$raw_n_rows != report$staged_n_rows) ||
    any(report$n_null_to_id != 0)) {
  stop("Staged row counts or destination null counts failed final validation")
}

# Independent measurements made before the migration. This catches a mistaken
# orientation rule even if the same mistake were repeated in both sides of a
# source-vs-staging fingerprint comparison.
if (RUN_SCOPE == "full") {
  actual_swap_expectation <- tibble::tribble(
    ~filename, ~expected_swaps,
    "ttm_bicycle_2021.parquet", 1421305,
    "ttm_bicycle_2022.parquet", 1459602,
    "ttm_bicycle_2023.parquet", 1458830,
    "ttm_bicycle_2024.parquet", 1660909,
    "ttm_car_2021.parquet", 0,
    "ttm_car_2022.parquet", 0,
    "ttm_car_2023.parquet", 0,
    "ttm_car_2024.parquet", 0,
    "ttm_transit_2021.parquet", 0,
    "ttm_transit_2022.parquet", 0,
    "ttm_transit_2023.parquet", 0,
    "ttm_transit_2024.parquet", 0,
    "ttm_walk_2021.parquet", 714319,
    "ttm_walk_2022.parquet", 721112,
    "ttm_walk_2023.parquet", 699521,
    "ttm_walk_2024.parquet", 731021
  )
  actual_swap_check <- report |>
    filter(kind == "actual") |>
    mutate(filename = path_file(input_file)) |>
    select(filename, observed_swaps = n_orientation_swaps) |>
    left_join(actual_swap_expectation, by = "filename", relationship = "one-to-one")
  if (nrow(actual_swap_check) != 16L ||
      anyNA(actual_swap_check$expected_swaps) ||
      any(actual_swap_check$observed_swaps != actual_swap_check$expected_swaps)) {
    stop("Actual TTM orientation swaps differ from independent measurements")
  }
  if (any(report$n_orientation_swaps[report$kind == "counterfactual"] != 0)) {
    stop("Counterfactual TTMs unexpectedly required orientation swaps")
  }
}

write_csv(report, REPORT_FILE)
message("Validated staging complete: ", nrow(report), " files")
message("Stage: ", STAGE_ROOT)
message("Report: ", REPORT_FILE)
