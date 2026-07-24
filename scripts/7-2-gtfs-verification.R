# 7-2-gtfs-verification.R
# Confirms the §5.6 input-verification numbers (Busan, Daegu) and saves them
# as durable artifacts for Supplementary S3.
#
# Paper claims to verify (currently from session memory):
#   Busan 2021->2023: scheduled trips on common routes ~ -12%,
#                     scheduled speed (min/stop) ~ unchanged (+0.3%)
#   Daegu 2021->2023: served bus stops net increase (~ +65),
#                     trips on common routes ~ +9.2%

pacman::p_load(tidyverse, tidytransit, fs, glue, scales)

PROJECT_ROOT <- normalizePath(
  Sys.getenv("ACCESS_DECLINE_ROOT", unset = "."),
  winslash = "/", mustWork = TRUE
)
if (!file.exists(file.path(PROJECT_ROOT, ".gitignore"))) {
  stop("Run from the repository root or set ACCESS_DECLINE_ROOT.")
}
DATA_ROOT    <- path(PROJECT_ROOT, "data")
OUTPUT_DIR   <- path(PROJECT_ROOT, "workflows/analysis")

CITIES <- list(
  busan = c(xmin = 128.7, ymin = 34.9, xmax = 129.3, ymax = 35.4),
  daegu = c(xmin = 128.4, ymin = 35.7, xmax = 128.8, ymax = 36.0)
)

message(strrep("=", 70))
message("GTFS INPUT VERIFICATION: Busan / Daegu, 2021 vs 2023")
message(strrep("=", 70))

route_tables <- list()
city_year <- list()

for (yr in c(2021, 2023)) {
  message(glue("\n[{yr}] Loading national GTFS..."))
  gtfs <- read_gtfs(path(DATA_ROOT, "gtfs/prep", glue("gtfs_{yr}.zip")))

  st <- gtfs$stop_times |>
    mutate(arr_sec = as.numeric(arrival_time), dep_sec = as.numeric(departure_time))

  first_stops <- st |>
    group_by(trip_id) |>
    slice_min(stop_sequence, n = 1) |>
    ungroup()

  trips_info  <- gtfs$trips |> select(trip_id, route_id)
  routes_info <- gtfs$routes |> select(route_id, route_short_name, route_type)

  for (city in names(CITIES)) {
    bb <- CITIES[[city]]
    stops_bbox <- gtfs$stops |>
      filter(!is.na(stop_lon), !is.na(stop_lat),
             stop_lon >= bb["xmin"], stop_lon <= bb["xmax"],
             stop_lat >= bb["ymin"], stop_lat <= bb["ymax"])

    city_trip_ids <- first_stops |>
      filter(stop_id %in% stops_bbox$stop_id) |>
      pull(trip_id)

    city_st <- st |> filter(trip_id %in% city_trip_ids)

    trip_dur <- city_st |>
      group_by(trip_id) |>
      summarize(
        n_stops = n(),
        duration_min = (max(arr_sec, na.rm = TRUE) - min(dep_sec, na.rm = TRUE)) / 60,
        .groups = "drop"
      ) |>
      filter(duration_min > 0, duration_min < 600) |>
      left_join(trips_info, by = "trip_id") |>
      left_join(routes_info, by = "route_id") |>
      filter(route_type == 3)

    served_stop_ids <- city_st |>
      semi_join(trip_dur, by = "trip_id") |>
      distinct(stop_id) |>
      filter(stop_id %in% stops_bbox$stop_id)

    city_year[[glue("{city}_{yr}")]] <- tibble(
      city = city, year = yr,
      n_served_bus_stops = nrow(served_stop_ids),
      n_bus_trips = nrow(trip_dur),
      n_bus_routes = n_distinct(trip_dur$route_id)
    )

    route_tables[[glue("{city}_{yr}")]] <- trip_dur |>
      group_by(route_id, route_short_name) |>
      summarize(
        n_trips = n(),
        mean_dur = mean(duration_min),
        mean_stops = mean(n_stops),
        .groups = "drop"
      ) |>
      mutate(city = city, year = yr)

    message(glue("  {city}: {nrow(stops_bbox)} stops in bbox, ",
                 "{nrow(served_stop_ids)} served bus stops, ",
                 "{comma(nrow(trip_dur))} bus trips"))
  }
  rm(gtfs, st, first_stops); gc(verbose = FALSE)
}

# === Common-route comparison ==================================================

summary_rows <- list()

for (city in names(CITIES)) {
  r21 <- route_tables[[glue("{city}_2021")]]
  r23 <- route_tables[[glue("{city}_2023")]]

  common <- inner_join(
    r21 |> select(route_id, route_short_name, n_trips_21 = n_trips,
                  dur_21 = mean_dur, stops_21 = mean_stops),
    r23 |> select(route_id, route_short_name, n_trips_23 = n_trips,
                  dur_23 = mean_dur, stops_23 = mean_stops),
    by = c("route_id", "route_short_name")
  ) |>
    mutate(
      speed_21 = dur_21 / stops_21,
      speed_23 = dur_23 / stops_23
    )

  cy21 <- city_year[[glue("{city}_2021")]]
  cy23 <- city_year[[glue("{city}_2023")]]

  s <- tibble(
    city = city,
    n_common_routes   = nrow(common),
    trips_common_21   = sum(common$n_trips_21),
    trips_common_23   = sum(common$n_trips_23),
    trips_pct_change  = (sum(common$n_trips_23) / sum(common$n_trips_21) - 1) * 100,
    mean_dur_pct      = mean((common$dur_23 - common$dur_21) / common$dur_21) * 100,
    mean_speed_pct    = mean((common$speed_23 - common$speed_21) / common$speed_21) * 100,
    served_stops_21   = cy21$n_served_bus_stops,
    served_stops_23   = cy23$n_served_bus_stops,
    served_stops_diff = cy23$n_served_bus_stops - cy21$n_served_bus_stops,
    all_trips_21      = cy21$n_bus_trips,
    all_trips_23      = cy23$n_bus_trips,
    all_trips_pct     = (cy23$n_bus_trips / cy21$n_bus_trips - 1) * 100
  )
  summary_rows[[city]] <- s

  cat("\n", strrep("=", 66), "\n", sep = "")
  cat(toupper(city), "2021 -> 2023\n")
  cat(strrep("=", 66), "\n")
  cat(glue("  Common bus routes:            {s$n_common_routes}"), "\n")
  cat(glue("  Trips on common routes:       {comma(s$trips_common_21)} -> {comma(s$trips_common_23)}  ({sprintf('%+.1f%%', s$trips_pct_change)})"), "\n")
  cat(glue("  All bus trips (bbox):         {comma(s$all_trips_21)} -> {comma(s$all_trips_23)}  ({sprintf('%+.1f%%', s$all_trips_pct)})"), "\n")
  cat(glue("  Mean route duration change:   {sprintf('%+.1f%%', s$mean_dur_pct)}"), "\n")
  cat(glue("  Mean speed (min/stop) change: {sprintf('%+.1f%%', s$mean_speed_pct)}"), "\n")
  cat(glue("  Served bus stops:             {comma(s$served_stops_21)} -> {comma(s$served_stops_23)}  ({sprintf('%+d', s$served_stops_diff)})"), "\n")
}

verif <- bind_rows(summary_rows)
write_csv(verif, path(OUTPUT_DIR, "gtfs_verification_busan_daegu.csv"))
write_csv(bind_rows(route_tables), path(OUTPUT_DIR, "gtfs_verification_routes.csv"))

message("\nSaved: workflows/analysis/gtfs_verification_busan_daegu.csv")
message("Saved: workflows/analysis/gtfs_verification_routes.csv")
message(strrep("=", 70))
message("VERIFICATION COMPLETE")
message(strrep("=", 70))
