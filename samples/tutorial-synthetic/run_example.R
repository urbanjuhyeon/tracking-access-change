# Synthetic fixture data: CC0-1.0. Analysis code: MIT.

options(java.parameters = "-Xmx2G")

suppressPackageStartupMessages({
  library(data.table)
  library(r5r)
})

script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
if (length(script_arg) != 1L) {
  stop("Run this file with Rscript so its fixture directory can be resolved.")
}

fixture_dir <- dirname(normalizePath(sub("^--file=", "", script_arg)))
generator <- file.path(fixture_dir, "generate_fixture.py")
build_dir <- file.path(fixture_dir, "build")
network_dir <- file.path(build_dir, "network")
results_dir <- file.path(build_dir, "results")

python <- Sys.which("python")
if (!nzchar(python)) {
  stop("Python is required to generate the synthetic fixture.")
}

generator_output <- system2(
  python,
  args = shQuote(generator),
  stdout = TRUE,
  stderr = TRUE
)
if (!is.null(attr(generator_output, "status"))) {
  stop(paste(c("Fixture generation failed:", generator_output), collapse = "\n"))
}

dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)

network <- build_network(
  data_path = network_dir,
  verbose = FALSE,
  overwrite = TRUE
)
on.exit(stop_r5(), add = TRUE)

street_layers <- street_network_to_sf(network)
transit_layers <- transit_network_to_sf(network)
if (nrow(street_layers$vertices) < 40L || !any(street_layers$edges$walk)) {
  stop("Synthetic street network was pruned or has no walkable edges.")
}
if (nrow(transit_layers$stops) != 4L || nrow(transit_layers$routes) < 1L) {
  stop("Synthetic GTFS did not produce the expected stops and route.")
}

origins <- fread(file.path(build_dir, "origins.csv"))
destinations <- fread(file.path(build_dir, "destinations.csv"))
departure <- as.POSIXct("2026-01-05 08:00:00", tz = "Etc/UTC")

route_mode <- function(mode_label, mode_value) {
  result <- travel_time_matrix(
    r5r_network = network,
    origins = origins[, .(id, lon, lat)],
    destinations = destinations[, .(id, lon, lat)],
    mode = mode_value,
    departure_datetime = departure,
    time_window = 1L,
    percentiles = 50L,
    draws_per_minute = 1L,
    max_trip_duration = 60L,
    n_threads = 1L,
    progress = FALSE
  )
  result[, mode := mode_label]
  result[]
}

walk <- route_mode("walk", "WALK")
transit <- route_mode("transit", c("WALK", "TRANSIT"))
results <- rbindlist(list(walk, transit), use.names = TRUE, fill = TRUE)

travel_column <- grep("^travel_time", names(results), value = TRUE)
if (length(travel_column) != 1L) {
  stop("Expected exactly one travel-time column; found: ", paste(travel_column, collapse = ", "))
}

expected_pairs <- nrow(origins) * nrow(destinations)
counts <- results[, .N, by = mode]
if (nrow(counts) != 2L || any(counts$N != expected_pairs)) {
  print(counts)
  expected_grid <- CJ(from_id = origins$id, to_id = destinations$id)
  observed_grid <- unique(results[, .(from_id, to_id)])
  print(fsetdiff(expected_grid, observed_grid))
  stop("Expected ", expected_pairs, " reachable OD pairs for each of two modes.")
}
if (any(!is.finite(results[[travel_column]])) || any(results[[travel_column]] < 0)) {
  stop("Travel times must be finite and non-negative.")
}

setorderv(results, c("mode", "from_id", "to_id"))
fwrite(results, file.path(results_dir, "travel_times.csv"))

summary <- results[
  , .(
    od_pairs = .N,
    min_minutes = min(get(travel_column)),
    median_minutes = median(get(travel_column)),
    max_minutes = max(get(travel_column))
  ),
  by = mode
]
setorder(summary, mode)
fwrite(summary, file.path(results_dir, "summary.csv"))

cat("fixture_smoke_test=passed\n")
print(summary)
