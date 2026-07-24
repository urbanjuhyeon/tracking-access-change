# 7-3-gtfs-slot-frequencies.R
# Trips departing at the routing departure slots (07,09,12,14,17,19h),
# which is the service the accessibility computation actually samples.
# Complements 7-2 (all-day totals). Busan + Daegu, 2021 vs 2023.

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
SLOTS <- c(7, 9, 12, 14, 17, 19)

rows <- list()

for (yr in c(2021, 2023)) {
  message(glue("[{yr}] Loading national GTFS..."))
  gtfs <- read_gtfs(path(DATA_ROOT, "gtfs/prep", glue("gtfs_{yr}.zip")))

  first_stops <- gtfs$stop_times |>
    mutate(dep_sec = as.numeric(departure_time)) |>
    group_by(trip_id) |>
    slice_min(stop_sequence, n = 1) |>
    ungroup()

  trips_info  <- gtfs$trips |> select(trip_id, route_id)
  routes_info <- gtfs$routes |> select(route_id, route_type)

  for (city in names(CITIES)) {
    bb <- CITIES[[city]]
    stops_bbox <- gtfs$stops |>
      filter(!is.na(stop_lon), !is.na(stop_lat),
             stop_lon >= bb["xmin"], stop_lon <= bb["xmax"],
             stop_lat >= bb["ymin"], stop_lat <= bb["ymax"])

    active_stop_ids <- gtfs$stop_times |>
      distinct(stop_id) |>
      inner_join(stops_bbox, by = "stop_id")

    city_first <- first_stops |>
      filter(stop_id %in% stops_bbox$stop_id) |>
      left_join(trips_info, by = "trip_id") |>
      left_join(routes_info, by = "route_id") |>
      mutate(dep_hour = floor(dep_sec / 3600))

    bus_slot  <- city_first |> filter(route_type == 3, dep_hour %in% SLOTS)
    rail_slot <- city_first |> filter(route_type != 3, dep_hour %in% SLOTS)

    per_slot <- bus_slot |> count(dep_hour) |> mutate(city = city, year = yr)

    rows[[glue("{city}_{yr}")]] <- tibble(
      city = city, year = yr,
      stops_in_feed  = nrow(stops_bbox),
      active_stops   = nrow(active_stop_ids),
      bus_trips_slots  = nrow(bus_slot),
      rail_trips_slots = nrow(rail_slot),
      per_slot = list(per_slot)
    )
    message(glue("  {city}: feed stops {nrow(stops_bbox)}, active {nrow(active_stop_ids)}, ",
                 "bus trips@slots {comma(nrow(bus_slot))}, rail@slots {comma(nrow(rail_slot))}"))
  }
  rm(gtfs, first_stops); gc(verbose = FALSE)
}

flat <- bind_rows(lapply(rows, \(r) r |> select(-per_slot)))

cat("\n", strrep("=", 66), "\n", sep = "")
for (city in names(CITIES)) {
  a <- flat |> filter(city == !!city, year == 2021)
  b <- flat |> filter(city == !!city, year == 2023)
  cat(toupper(city), "2021 -> 2023 (routing departure slots)\n")
  cat(glue("  Stops in feed:   {comma(a$stops_in_feed)} -> {comma(b$stops_in_feed)}  ({sprintf('%+d', b$stops_in_feed - a$stops_in_feed)})"), "\n")
  cat(glue("  Active stops:    {comma(a$active_stops)} -> {comma(b$active_stops)}  ({sprintf('%+d', b$active_stops - a$active_stops)})"), "\n")
  cat(glue("  Bus trips@slots: {comma(a$bus_trips_slots)} -> {comma(b$bus_trips_slots)}  ({sprintf('%+.1f%%', (b$bus_trips_slots/a$bus_trips_slots - 1)*100)})"), "\n")
  cat(glue("  Rail trips@slots:{comma(a$rail_trips_slots)} -> {comma(b$rail_trips_slots)}  ({sprintf('%+.1f%%', (b$rail_trips_slots/a$rail_trips_slots - 1)*100)})"), "\n\n")
}

per_slot_all <- bind_rows(lapply(rows, \(r) r$per_slot[[1]]))
per_slot_wide <- per_slot_all |>
  pivot_wider(names_from = year, values_from = n, names_prefix = "y") |>
  mutate(pct = (y2023 / y2021 - 1) * 100)

cat("Per-slot bus trips (2021 -> 2023):\n")
per_slot_wide |>
  mutate(line = glue("  {city} {sprintf('%02d', dep_hour)}:00  {y2021} -> {y2023}  ({sprintf('%+.1f%%', pct)})")) |>
  pull(line) |> walk(cat, "\n")

write_csv(flat, path(OUTPUT_DIR, "gtfs_slot_frequencies.csv"))
write_csv(per_slot_wide, path(OUTPUT_DIR, "gtfs_slot_frequencies_hourly.csv"))
message("\nSaved: gtfs_slot_frequencies.csv / _hourly.csv")
