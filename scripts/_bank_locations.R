# Canonical bank-location identifiers used by every routing and analysis script.
#
# The identifier is derived from the same institution + rounded-coordinate rule
# that the project has always used to deduplicate routing destinations. Unlike
# row_number(), it is invariant to CSV row order.

CANONICAL_BANK_FILENAME <- "bank_4_fin.csv"

sha256_file <- function(file) {
  if (!requireNamespace("openssl", quietly = TRUE)) {
    stop("The openssl package is required for SHA-256 verification")
  }
  connection <- base::file(file, open = "rb")
  on.exit(close(connection), add = TRUE)
  tolower(unname(as.character(openssl::sha256(connection))))
}

format_bank_coordinate <- function(x) {
  formatC(round(as.numeric(x), 4), format = "f", digits = 4,
          decimal.mark = ".")
}

make_bank_location_id <- function(bank_name, longitude, latitude) {
  bank_name <- as.character(bank_name)
  longitude <- suppressWarnings(as.numeric(longitude))
  latitude <- suppressWarnings(as.numeric(latitude))
  if (anyNA(bank_name) || any(!nzchar(trimws(bank_name)))) {
    stop("bank_name must be non-missing and non-blank")
  }
  if (any(!is.finite(longitude)) || any(!is.finite(latitude))) {
    stop("longitude and latitude must be finite numeric values")
  }
  if (any(grepl("|", bank_name, fixed = TRUE), na.rm = TRUE)) {
    stop("bank_name contains the reserved '|' delimiter")
  }

  paste(
    bank_name,
    format_bank_coordinate(longitude),
    format_bank_coordinate(latitude),
    sep = "|"
  )
}

read_canonical_bank_data <- function(data_root) {
  bank_file <- file.path(data_root, "bank", CANONICAL_BANK_FILENAME)
  if (!file.exists(bank_file)) {
    stop("Canonical bank file not found: ", bank_file)
  }
  readr::read_csv(bank_file, show_col_types = FALSE)
}

assert_location_id_ttm_ready <- function(
    results_root,
    verify_sha256 = !identical(Sys.getenv("TTM_SKIP_SHA256"), "1")) {
  in_progress <- file.path(results_root, "ID_MIGRATION_IN_PROGRESS")
  ready <- file.path(results_root, "ID_SCHEME_LOCATION_V1")
  if (file.exists(in_progress)) {
    stop(
      "TTM location_id migration is still in progress; analysis is blocked: ",
      in_progress
    )
  }
  if (!file.exists(ready)) {
    stop(
      "TTM location_id readiness marker is missing; analysis is blocked: ",
      ready
    )
  }

  marker <- readLines(ready, warn = FALSE)
  required_marker_lines <- c(
    "scheme=location_id_v1",
    "actual_files=16",
    "counterfactual_files=2940"
  )
  if (!all(required_marker_lines %in% marker)) {
    stop("TTM readiness marker has unexpected or incomplete contents: ", ready)
  }

  project_root <- normalizePath(
    file.path(results_root, "../.."),
    winslash = "/",
    mustWork = TRUE
  )
  canonical_file <- file.path(project_root, "data/bank", CANONICAL_BANK_FILENAME)
  expected_canonical_sha256 <-
    "9d2db762c12cd1425bbf7c7c9bbb5521bcd3964ed87834a44b2a79d573ca765a"
  if (!file.exists(canonical_file)) {
    stop("Canonical bank file is missing: ", canonical_file)
  }
  canonical_sha256 <- sha256_file(canonical_file)
  if (!isTRUE(canonical_sha256 == expected_canonical_sha256)) {
    stop("Canonical bank CSV SHA-256 differs from the 2025-12-25 source")
  }

  report_file <- file.path(
    results_root,
    "id_migration_20260712/ttm_staging_validation.csv"
  )
  if (!file.exists(report_file)) {
    stop("TTM migration validation report is missing: ", report_file)
  }
  report <- utils::read.csv(report_file, stringsAsFactors = FALSE)
  if (nrow(report) != 2956L ||
      sum(report$kind == "actual") != 16L ||
      sum(report$kind == "counterfactual") != 2940L ||
      any(!report$verified) ||
      any(report$raw_n_rows != report$staged_n_rows)) {
    stop("TTM migration validation report failed its inventory checks")
  }

  expected_files <- file.path(
    project_root,
    gsub("/", .Platform$file.sep, report$input_file, fixed = TRUE)
  )
  if (any(!file.exists(expected_files))) {
    stop("One or more promoted TTM files are missing")
  }
  observed_sizes <- file.info(expected_files)$size
  if (any(observed_sizes != as.numeric(report$staged_bytes))) {
    stop("One or more promoted TTM file sizes differ from the validated staging set")
  }

  actual_inventory <- list.files(
    results_root,
    pattern = "^ttm_(car|transit|walk|bicycle)_20(21|22|23|24)\\.parquet$",
    full.names = TRUE
  )
  counterfactual_inventory <- list.files(
    file.path(results_root, "decompose/routing"),
    pattern = "^ttm_(car|transit)_20[0-9]{2}net_nb20[0-9]{2}_[0-9]{5}\\.parquet$",
    full.names = TRUE
  )
  observed_inventory <- normalizePath(
    c(actual_inventory, counterfactual_inventory),
    winslash = "/",
    mustWork = TRUE
  )
  expected_inventory <- normalizePath(
    expected_files,
    winslash = "/",
    mustWork = TRUE
  )
  if (!setequal(observed_inventory, expected_inventory)) {
    stop("Active TTM inventory differs from the validated 2,956-file manifest")
  }

  if (verify_sha256) {
    observed_sha256 <- vapply(expected_files, sha256_file, character(1))
    if (any(observed_sha256 != report$staged_sha256)) {
      stop("One or more promoted TTM SHA-256 values failed verification")
    }
  }
  invisible(TRUE)
}

make_bank_locations <- function(bank_raw, reference_date_value) {
  required <- c("bank_name", "branch_name", "reference_date",
                "latitude", "longitude")
  missing <- setdiff(required, names(bank_raw))
  if (length(missing) > 0) {
    stop("Canonical bank data is missing columns: ",
         paste(missing, collapse = ", "))
  }

  selected <- bank_raw |>
    dplyr::filter(
      reference_date == reference_date_value,
      !is.na(longitude),
      !is.na(latitude)
    ) |>
    dplyr::mutate(
      longitude = suppressWarnings(as.numeric(longitude)),
      latitude = suppressWarnings(as.numeric(latitude))
    )

  if (nrow(selected) == 0) {
    stop("No bank locations found for reference_date=", reference_date_value)
  }
  if (anyNA(selected$bank_name) ||
      any(!nzchar(trimws(as.character(selected$bank_name))))) {
    stop("Missing or blank bank_name in ", reference_date_value)
  }
  if (any(!is.finite(selected$longitude)) ||
      any(!is.finite(selected$latitude))) {
    stop("Non-numeric or non-finite coordinates in ", reference_date_value)
  }
  if (any(selected$longitude < 120 | selected$longitude > 135) ||
      any(selected$latitude < 30 | selected$latitude > 40)) {
    stop("Coordinates outside the South Korea validation bounds in ",
         reference_date_value)
  }

  result <- selected |>
    dplyr::mutate(
      # Keep R's rounding semantics: the historical TTMs were created in R.
      longitude = round(longitude, 4),
      latitude = round(latitude, 4)
    ) |>
    dplyr::distinct(bank_name, longitude, latitude, .keep_all = TRUE) |>
    dplyr::mutate(
      location_id = make_bank_location_id(bank_name, longitude, latitude)
    )

  if (anyNA(result$location_id) || anyDuplicated(result$location_id)) {
    stop("location_id must be non-missing and unique within ",
         reference_date_value)
  }

  result
}

sql_quote_location_ids <- function(x) {
  paste0("'", gsub("'", "''", as.character(x), fixed = TRUE), "'")
}

bank_reference_date <- function(analysis_year, period = "h2") {
  paste0(as.integer(analysis_year) - 1L, period)
}

# r5r can return origin/destination columns in either direction, including a
# mixture of directions within one result.  Validate every row against the
# exact origin and destination ID sets and standardize the persisted contract.
normalize_ttm_orientation <- function(ttm, origin_ids, destination_ids,
                                      context = "TTM") {
  required <- c("from_id", "to_id")
  missing <- setdiff(required, names(ttm))
  if (length(missing) > 0) {
    stop(context, " is missing columns: ", paste(missing, collapse = ", "))
  }

  origin_ids <- unique(as.character(origin_ids))
  destination_ids <- unique(as.character(destination_ids))
  if (anyNA(origin_ids) || anyNA(destination_ids) ||
      any(!nzchar(origin_ids)) || any(!nzchar(destination_ids))) {
    stop(context, " received missing or blank endpoint IDs")
  }
  overlap <- intersect(origin_ids, destination_ids)
  if (length(overlap) > 0) {
    stop(context, " origin and destination ID namespaces overlap: ",
         paste(utils::head(overlap, 3), collapse = ", "))
  }

  from_id <- as.character(ttm$from_id)
  to_id <- as.character(ttm$to_id)
  normal <- from_id %in% origin_ids & to_id %in% destination_ids
  reversed <- to_id %in% origin_ids & from_id %in% destination_ids
  valid <- xor(normal, reversed)

  if (any(!valid)) {
    bad <- which(!valid)
    examples <- paste0("(", from_id[bad], " -> ", to_id[bad], ")")
    stop(
      context, " has ", length(bad), " row(s) that do not contain exactly ",
      "one known census origin and one known bank destination. Examples: ",
      paste(utils::head(examples, 3), collapse = ", ")
    )
  }

  ttm$from_id <- ifelse(reversed, to_id, from_id)
  ttm$to_id <- ifelse(reversed, from_id, to_id)
  ttm
}
