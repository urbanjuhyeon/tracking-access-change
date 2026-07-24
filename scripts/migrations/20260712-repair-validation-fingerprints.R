# Recompute exact fingerprint strings after an accidental numeric CSV round-trip.

suppressPackageStartupMessages({
  library(DBI)
  library(duckdb)
  library(fs)
  library(readr)
})

args <- commandArgs(trailingOnly = TRUE)
PROJECT_ROOT <- if (length(args) >= 1) path_abs(args[[1]]) else path_abs(".")
REPORT_FILE <- path(
  PROJECT_ROOT,
  "workflows/results/id_migration_20260712/ttm_staging_validation.csv"
)
TEMP_FILE <- path_ext_set(REPORT_FILE, "repaired.csv")
LOSSY_BACKUP <- path("C:/tmp", "uad_ttm_validation_report_lossy_20260712.csv")

if (file_exists(TEMP_FILE) || file_exists(LOSSY_BACKUP)) {
  stop("Repair temp or backup already exists; refusing to overwrite")
}

report <- read_csv(
  REPORT_FILE,
  col_types = cols(.default = col_character()),
  progress = FALSE
)
if (nrow(report) != 2956L) stop("Unexpected report row count")

con <- dbConnect(duckdb())
on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)

sql_path <- function(x) gsub("'", "''", path_abs(x), fixed = TRUE)

for (i in seq_len(nrow(report))) {
  active_file <- path(PROJECT_ROOT, report$input_file[[i]])
  if (!file_exists(active_file)) stop("Missing active file: ", active_file)
  columns <- if (report$kind[[i]] == "actual") {
    c("from_id", "to_id", "travel_time_p50", "mode", "departure_time", "year")
  } else {
    c("from_id", "to_id", "travel_time_p50", "departure_time")
  }
  hash_expr <- paste(columns, collapse = ", ")
  result <- dbGetQuery(con, sprintf(
    paste0(
      "SELECT CAST(COUNT(*) AS VARCHAR) AS n_rows, ",
      "CAST(COUNT(DISTINCT to_id) AS VARCHAR) AS n_destinations, ",
      "CAST(COUNT(*) FILTER (WHERE to_id IS NULL) AS VARCHAR) AS n_null_to_id, ",
      "CAST(bit_xor(hash(%s)) AS VARCHAR) AS xor_hash, ",
      "CAST(sum(CAST(hash(%s) AS HUGEINT)) AS VARCHAR) AS sum_hash ",
      "FROM read_parquet('%s')"
    ),
    hash_expr, hash_expr, sql_path(active_file)
  ))
  if (result$n_rows[[1]] != report$staged_n_rows[[i]] ||
      result$n_destinations[[1]] != report$n_destinations[[i]] ||
      result$n_null_to_id[[1]] != report$n_null_to_id[[i]]) {
    stop("Fingerprint companion counts changed for ", active_file)
  }
  report$xor_hash[[i]] <- result$xor_hash[[1]]
  report$sum_hash[[i]] <- result$sum_hash[[1]]
  if (i %% 100L == 0L) message("Recomputed ", i, " / ", nrow(report))
}

nonempty <- as.numeric(report$staged_n_rows) > 0
if (any(!grepl("^[0-9]+$", report$xor_hash[nonempty])) ||
    any(!grepl("^[0-9]+$", report$sum_hash[nonempty]))) {
  stop("Recomputed fingerprints contain a non-decimal value")
}

write_csv(report, TEMP_FILE, na = "")
roundtrip <- read_csv(
  TEMP_FILE,
  col_types = cols(.default = col_character()),
  progress = FALSE
)
if (!identical(report$xor_hash, roundtrip$xor_hash) ||
    !identical(report$sum_hash, roundtrip$sum_hash)) {
  stop("Fingerprint strings changed during repaired CSV round-trip")
}

file_move(REPORT_FILE, LOSSY_BACKUP)
file_move(TEMP_FILE, REPORT_FILE)
message("Exact fingerprints restored for ", nrow(report), " files")
message("Lossy report retained at ", LOSSY_BACKUP)
