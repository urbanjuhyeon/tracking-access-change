# Setup ----

## Packages ----
options(java.parameters = "-Xmx4G")  # Must be set before loading r5r

pacman::p_load(
  sf, r5r, ggplot2, ggspatial, patchwork, ggrepel,
  dplyr, tidyr, scales, fs
)

## Paths ----
project_root <- normalizePath(
  Sys.getenv("ACCESS_DECLINE_ROOT", unset = "."),
  winslash = "/", mustWork = TRUE
)
if (!file.exists(file.path(project_root, ".gitignore"))) {
  stop("Run from the repository root or set ACCESS_DECLINE_ROOT.")
}
setwd(project_root)

pilot_input_dir   <- "workflows/pilot/input"
pilot_network_dir <- "workflows/pilot/network"
pilot_output_dir  <- "workflows/pilot/output"
docs_figures_dir  <- "docs/assets/figures/ch3"

# Clean and recreate output directory for fresh run
if (dir_exists(pilot_output_dir)) dir_delete(pilot_output_dir)
dir_create(path(pilot_output_dir, "figures"), recurse = TRUE)
dir_create(path(pilot_output_dir, "data"), recurse = TRUE)
dir_create(docs_figures_dir, recurse = TRUE)

## Parameters ----
departure <- as.POSIXct("2024-03-15 14:00:00")
max_walk_time <- 15
max_trip_duration <- 120

# Bicycle parameters
bicycle_speed <- 12      # km/h (conservative for general population)
bicycle_max_lts <- 2     # Level of Traffic Stress: residential streets, bike lanes (R5R default)

## Build Network ----
cat("=== Building R5R Network ===\n")

# Clean cache files (skip if locked)
cache_files <- dir_ls(pilot_network_dir, regexp = "\\.(dat|mapdb|mapdb\\.p|log|json)$")
for (f in cache_files) {
  tryCatch(file_delete(f), error = function(e) NULL)
}

r5r_core <- r5r::build_network(data_path = pilot_network_dir, verbose = FALSE)

## Load Data ----
landmarks <- read.csv(path(pilot_input_dir, "landmarks.csv"))
print(landmarks)

paldal_tracts <- st_read(path(pilot_input_dir, "census_tracts.gpkg"), quiet = TRUE)

origins <- paldal_tracts |>
  st_transform(5179) |>
  st_centroid() |>
  st_transform(4326) |>
  mutate(lon = st_coordinates(geom)[, 1],
         lat = st_coordinates(geom)[, 2]) |>
  st_drop_geometry() |>
  select(id = TOT_REG_CD, lon, lat)

cat("Loaded", nrow(landmarks), "landmarks,", nrow(origins), "census tracts\n")


# detailed_itineraries() ----
cat("\n=== detailed_itineraries() ===\n")

## Define OD Pairs ----
od_pairs <- tibble(
  origin_id = c("suwon_station", "central_library", "paldal_office"),
  dest_id   = c("central_library", "paldal_office", "paldal_health"),
  file_tag  = c("station_to_library", "library_to_office", "office_to_health")
) |>
  left_join(landmarks |> select(origin_id = id, origin_name = name,
                                origin_lon = lon, origin_lat = lat), by = "origin_id") |>
  left_join(landmarks |> select(dest_id = id, dest_name = name,
                                dest_lon = lon, dest_lat = lat), by = "dest_id")

## Calculate Routes ----
routes_list <- list()

for (i in 1:nrow(od_pairs)) {
  od <- od_pairs[i, ]
  origin_df <- data.frame(id = od$origin_id, lon = od$origin_lon, lat = od$origin_lat)
  dest_df   <- data.frame(id = od$dest_id, lon = od$dest_lon, lat = od$dest_lat)

  # Car
  car_route <- detailed_itineraries(
    r5r_core, origins = origin_df, destinations = dest_df,
    mode = "CAR", departure_datetime = departure,
    max_trip_duration = max_trip_duration, shortest_path = TRUE
  )
  if (!is.null(car_route) && nrow(car_route) > 0) {
    car_route$mode_type <- "car"
    car_route$od_pair <- od$file_tag
    car_route$od_label <- paste(od$origin_name, "->", od$dest_name)
    routes_list[[length(routes_list) + 1]] <- car_route
  }

  # Transit
  transit_route <- detailed_itineraries(
    r5r_core, origins = origin_df, destinations = dest_df,
    mode = c("WALK", "TRANSIT"), departure_datetime = departure,
    max_walk_time = max_walk_time, max_trip_duration = max_trip_duration,
    shortest_path = TRUE
  )
  if (!is.null(transit_route) && nrow(transit_route) > 0) {
    transit_route$mode_type <- "transit"
    transit_route$od_pair <- od$file_tag
    transit_route$od_label <- paste(od$origin_name, "->", od$dest_name)
    routes_list[[length(routes_list) + 1]] <- transit_route
  }

  # Walk
  walk_route <- detailed_itineraries(
    r5r_core, origins = origin_df, destinations = dest_df,
    mode = "WALK", departure_datetime = departure,
    max_walk_time = max_trip_duration, max_trip_duration = max_trip_duration,
    shortest_path = TRUE
  )
  if (!is.null(walk_route) && nrow(walk_route) > 0) {
    walk_route$mode_type <- "walk"
    walk_route$od_pair <- od$file_tag
    walk_route$od_label <- paste(od$origin_name, "->", od$dest_name)
    routes_list[[length(routes_list) + 1]] <- walk_route
  }

  # Bicycle
  bicycle_route <- detailed_itineraries(
    r5r_core, origins = origin_df, destinations = dest_df,
    mode = "BICYCLE", departure_datetime = departure,
    max_trip_duration = max_trip_duration, shortest_path = TRUE,
    bike_speed = bicycle_speed, max_lts = bicycle_max_lts
  )
  if (!is.null(bicycle_route) && nrow(bicycle_route) > 0) {
    bicycle_route$mode_type <- "bicycle"
    bicycle_route$od_pair <- od$file_tag
    bicycle_route$od_label <- paste(od$origin_name, "->", od$dest_name)
    routes_list[[length(routes_list) + 1]] <- bicycle_route
  }

  cat("  Calculated:", od$file_tag, "\n")
}

transit_route |>
  st_drop_geometry() |>
  select(mode, segment, segment_duration, route, distance)

## Save first OD pair route segments for qmd ----
# Extract station_to_library routes for qmd examples
first_od <- od_pairs[1, ]  # station_to_library
origin_first <- data.frame(id = first_od$origin_id, lon = first_od$origin_lon, lat = first_od$origin_lat)
dest_first   <- data.frame(id = first_od$dest_id, lon = first_od$dest_lon, lat = first_od$dest_lat)

# Car route for qmd
car_route_first <- detailed_itineraries(
  r5r_core, origins = origin_first, destinations = dest_first,
  mode = "CAR", departure_datetime = departure,
  max_trip_duration = max_trip_duration, shortest_path = TRUE
)
car_route_csv <- car_route_first |>
  st_drop_geometry() |>
  select(mode, segment, segment_duration, distance)
cat("\n--- detailed_itineraries() Output (for qmd) ---\n")
cat("\n# Car (Suwon Station -> Central Library)\n")
print(car_route_csv)
write.csv(car_route_csv, path(pilot_output_dir, "data", "route_car_segments.csv"), row.names = FALSE)

# Transit route for qmd
transit_route_first <- detailed_itineraries(
  r5r_core, origins = origin_first, destinations = dest_first,
  mode = c("WALK", "TRANSIT"), departure_datetime = departure,
  max_walk_time = max_walk_time, max_trip_duration = max_trip_duration,
  shortest_path = TRUE
)
transit_route_csv <- transit_route_first |>
  st_drop_geometry() |>
  select(mode, segment, segment_duration, route, distance)
cat("\n# Transit (Suwon Station -> Central Library)\n")
print(transit_route_csv)
write.csv(transit_route_csv, path(pilot_output_dir, "data", "route_transit_segments.csv"), row.names = FALSE)
cat("  Saved: route_car_segments.csv, route_transit_segments.csv\n")

routes_sf <- bind_rows(routes_list) |>
  group_by(od_pair, mode_type) |>
  mutate(
    total_time = round(first(total_duration), 1),
    facet_label = case_when(
      mode_type == "car" ~ paste0("(a) Car - ", total_time, " min"),
      mode_type == "transit" ~ paste0("(b) Transit - ", total_time, " min"),
      mode_type == "walk" ~ paste0("(c) Walk - ", total_time, " min"),
      mode_type == "bicycle" ~ paste0("(d) Bicycle - ", total_time, " min")
    )
  ) |>
  ungroup()

## Save Routes to CSV (for qmd consumption) ----
routes_csv <- routes_sf |>
  st_drop_geometry() |>
  select(od_pair, mode_type, mode, segment, segment_duration, distance,
         route, total_duration, total_time, facet_label, od_label)
write.csv(routes_csv, path(pilot_output_dir, "data", "routes_detailed.csv"), row.names = FALSE)
cat("  Saved: routes_detailed.csv\n")

# Also save route geometries as GeoPackage for spatial analysis
st_write(routes_sf, path(pilot_output_dir, "data", "routes_detailed.gpkg"),
         layer = "routes", delete_dsn = TRUE, quiet = TRUE)
cat("  Saved: routes_detailed.gpkg\n")

## Walk vs Bicycle Route Comparison ----
cat("\n--- Walk vs Bicycle Route Analysis ---\n")

# Compare routes for each OD pair
for (tag in unique(od_pairs$file_tag)) {
  cat("\n", tag, ":\n", sep = "")

  walk_data <- routes_sf |>
    filter(od_pair == tag, mode_type == "walk") |>
    st_drop_geometry()

  bicycle_data <- routes_sf |>
    filter(od_pair == tag, mode_type == "bicycle") |>
    st_drop_geometry()

  if (nrow(walk_data) > 0 && nrow(bicycle_data) > 0) {
    # Total distance comparison
    walk_dist <- sum(walk_data$distance, na.rm = TRUE)
    bicycle_dist <- sum(bicycle_data$distance, na.rm = TRUE)

    cat("  Walk:    ", round(walk_dist, 0), "m in",
        round(sum(walk_data$segment_duration), 1), "min\n")
    cat("  Bicycle: ", round(bicycle_dist, 0), "m in",
        round(sum(bicycle_data$segment_duration), 1), "min\n")
    cat("  Distance diff: ", round(bicycle_dist - walk_dist, 0), "m (",
        round((bicycle_dist / walk_dist - 1) * 100, 1), "%)\n", sep = "")

    if (abs(bicycle_dist - walk_dist) > 50) {  # More than 50m difference
      cat("  -> Routes differ! Bicycle takes a different path.\n")
      cat("     Likely due to LTS restriction (avoiding high-traffic roads)\n")
    } else {
      cat("  -> Routes are similar (same path, different speeds)\n")
    }
  }
}

### Not shortest_path Example ----
origin_ex <- data.frame(id = od$origin_id, lon = od$origin_lon, lat = od$origin_lat)
dest_ex   <- data.frame(id = od$dest_id, lon = od$dest_lon, lat = od$dest_lat)

transit_route <- detailed_itineraries(
    r5r_core, origins = origin_ex, destinations = dest_ex,
    mode = c("WALK", "TRANSIT"), departure_datetime = departure,
    max_walk_time = max_walk_time, max_trip_duration = max_trip_duration,
    suboptimal_minutes = 3, shortest_path = FALSE
  )

transit_route |>
  st_drop_geometry() |>
  select(mode, segment, segment_duration, route, distance)

## Visualize Routes ----
for (tag in unique(od_pairs$file_tag)) {
  route_data <- routes_sf |> filter(od_pair == tag)
  od_info <- od_pairs |> filter(file_tag == tag)

  # Create location labels
  origin_label <- paste0(od_info$origin_name, "  ->")
  dest_label <- paste0(od_info$dest_name)

  od_points <- tibble(
    location = factor(c(origin_label, dest_label),
                      levels = c(origin_label, dest_label)),
    lon = c(od_info$origin_lon, od_info$dest_lon),
    lat = c(od_info$origin_lat, od_info$dest_lat)
  ) |>
    st_as_sf(coords = c("lon", "lat"), crs = 4326)

  # Reorder mode factor for legend: Car, Bus, Subway, Walk, Bicycle
  route_data <- route_data |>
    mutate(mode = factor(mode, levels = c("CAR", "BUS", "SUBWAY", "WALK", "BICYCLE")))

  # 4 modes: 2x2 layout
  n_cols <- 2

  p <- ggplot() +
    annotation_map_tile(type = "cartolight", zoomin = -1, progress = "none") +
    geom_sf(data = route_data, aes(color = mode), linewidth = 1.5, alpha = 0.8) +
    geom_sf(data = od_points, aes(shape = location, fill = location),
            size = 4, stroke = 0.5, color = "black") +
    scale_color_manual(name = NULL,
                       values = c("CAR" = "#2C3E50", "BUS" = "#FF6B6B",
                                  "SUBWAY" = "#4ECDC4", "WALK" = "#00AA00",
                                  "BICYCLE" = "#9B59B6"),
                       labels = c("CAR" = "Car", "BUS" = "Bus",
                                  "SUBWAY" = "Subway", "WALK" = "Walk",
                                  "BICYCLE" = "Bicycle")) +
    scale_shape_manual(name = NULL, values = c(21, 23)) +
    scale_fill_manual(name = NULL,
                      values = c("#00AA00", "#FF0000")) +
    facet_wrap(~ facet_label, ncol = n_cols) +
    labs(title = "Route Comparison by Mode",
         x = NULL, y = NULL) +
    guides(shape = guide_legend(order = 1),
           fill = guide_legend(order = 1),
           color = guide_legend(order = 2)) +
    theme_bw() +
    theme(plot.title = element_text(face = "bold", hjust = 0),
          strip.text = element_text(face = "italic"),
          legend.position = "top",
          legend.justification = "left",
          legend.box = "vertical",
          legend.box.just = "left",
          legend.box.spacing = unit(0.1, "cm"),
          legend.spacing.y = unit(0.1, "cm"),
          legend.margin = margin(0, 0, 2, 0),
          axis.text = element_blank(), axis.ticks = element_blank())

  # Save
  ggsave(path(path(pilot_output_dir, "figures"), paste0("validation_routes_", tag, ".png")),
         p, width = 8, height = 7, dpi = 300, bg = "white")
  file_copy(path(path(pilot_output_dir, "figures"), paste0("validation_routes_", tag, ".png")),
            path(docs_figures_dir, paste0("validation_routes_", tag, ".png")),
            overwrite = TRUE)
  cat("  Saved: validation_routes_", tag, "\n", sep = "")
}

## Visualize Timelines ----

# Load GTFS routes for route_short_name lookup
gtfs_routes <- read.csv(
  unz(path(pilot_network_dir, "gtfs.zip"), "routes.txt"),
  stringsAsFactors = FALSE
)

## Multiple Routes (shortest_path = FALSE) ----
# When shortest_path = FALSE, r5r returns multiple route options
# This is useful for comparing alternatives (e.g., different bus routes)

cat("\n--- Multiple Route Options ---\n")

origin_multi <- landmarks |> filter(id == "suwon_station") |> select(id, lon, lat)
dest_multi   <- landmarks |> filter(id == "central_library") |> select(id, lon, lat)

multi_routes <- detailed_itineraries(
  r5r_core, origins = origin_multi, destinations = dest_multi,
  mode = c("WALK", "TRANSIT"), departure_datetime = departure,
  max_walk_time = max_walk_time, max_trip_duration = max_trip_duration,
  shortest_path = FALSE,
  suboptimal_minutes = 10  # Include routes up to 10 min slower than optimal
)

if (!is.null(multi_routes) && nrow(multi_routes) > 0) {
  # Summarize each option
  route_summary <- multi_routes |>
    st_drop_geometry() |>
    left_join(gtfs_routes |> select(route_id, route_short_name),
              by = c("route" = "route_id")) |>
    group_by(option) |>
    summarise(
      total_time = round(first(total_duration), 1),
      n_segments = n(),
      modes_used = paste(unique(mode[mode != "WALK"]), collapse = ", "),
      route_names = paste(unique(na.omit(route_short_name)), collapse = ", "),
      .groups = "drop"
    ) |>
    arrange(total_time)

  cat("Found", nrow(route_summary), "route options:\n")
  print(route_summary)

  ## Save multi_routes summary for qmd ----
  # qmd shows: option, total_time, modes (e.g., "WALK+BUS")
  multi_routes_qmd <- multi_routes |>
    st_drop_geometry() |>
    group_by(option) |>
    summarise(
      total_time = round(first(total_duration), 1),
      modes = paste(unique(mode), collapse = "+"),
      .groups = "drop"
    ) |>
    arrange(option)
  cat("\n# multi_routes summary (for qmd)\n")
  print(multi_routes_qmd)
  write.csv(multi_routes_qmd, path(pilot_output_dir, "data", "multi_routes_summary.csv"), row.names = FALSE)
  cat("  Saved: multi_routes_summary.csv\n")
}

for (tag in unique(od_pairs$file_tag)) {
  route_data <- routes_sf |> filter(od_pair == tag)
  od_info <- od_pairs |> filter(file_tag == tag)
  od_label <- paste0(od_info$origin_name, " -> ", od_info$dest_name)

  # Get walk time as x-axis max (exact value, no rounding)
  walk_time <- route_data |>
    st_drop_geometry() |>
    filter(mode_type == "walk") |>
    summarise(total = sum(segment_duration)) |>
    pull(total)
  x_max <- walk_time

  timeline_plots <- list()

  for (m in c("car", "transit", "walk", "bicycle")) {
    mode_data <- route_data |>
      filter(mode_type == m) |>
      st_drop_geometry() |>
      left_join(gtfs_routes |> select(route_id = route_id, route_short_name),
                by = c("route" = "route_id")) |>
      mutate(
        duration = segment_duration,
        start_time = cumsum(lag(duration, default = 0)),
        end_time = start_time + duration,
        # Build bar label: "Bus #11" or "Walk"
        bar_label = case_when(
          mode == "WALK" ~ "Walk",
          mode == "CAR" ~ "Car",
          mode == "BICYCLE" ~ "Bicycle",
          mode == "BUS" & !is.na(route_short_name) ~ paste0("Bus #", route_short_name),
          mode == "SUBWAY" & !is.na(route_short_name) ~ paste0("Subway ", route_short_name),
          mode == "BUS" ~ "Bus",
          mode == "SUBWAY" ~ "Subway",
          TRUE ~ mode
        )
      )

    if (nrow(mode_data) == 0) next

    total_time <- max(mode_data$end_time)

    # Build subtitle with segment sequence using arrows
    segment_summary <- mode_data |>
      mutate(seg_text = paste0(bar_label, " ", round(duration, 1), " min")) |>
      pull(seg_text) |>
      paste(collapse = " -> ")

    panel_label <- case_when(
      m == "car" ~ "(a) Car",
      m == "transit" ~ "(b) Transit",
      m == "walk" ~ "(c) Walk",
      m == "bicycle" ~ "(d) Bicycle"
    )

    # Only show text label if segment >= 3 min
    mode_data <- mode_data |>
      mutate(show_label = duration >= 3)

    # X-axis breaks: only show ticks up to the mode's total time (floor to 5 min intervals)
    mode_x_breaks <- seq(0, floor(total_time / 5) * 5, by = 5)

    timeline_plots[[m]] <- ggplot(mode_data, aes(xmin = start_time, xmax = end_time,
                                                  ymin = 0, ymax = 1, fill = mode)) +
      geom_rect(color = "white", linewidth = 1) +
      geom_text(data = mode_data |> filter(show_label),
                aes(x = (start_time + end_time) / 2, y = 0.5,
                    label = paste0(bar_label, "\n", round(duration, 1), " min")),
                size = 3, color = "white") +
      scale_fill_manual(values = c("WALK" = "#1B7A1B", "BUS" = "#D63031",
                                   "SUBWAY" = "#00B894", "CAR" = "#2C3E50",
                                   "BICYCLE" = "#9B59B6")) +
      coord_cartesian(xlim = c(0, x_max)) +
      scale_x_continuous(breaks = mode_x_breaks,
                         labels = \(x) paste0(x, " min")) +
      labs(title = panel_label,
           subtitle = paste0("Total ", round(total_time, 1), " min (", segment_summary, ")"),
           x = NULL, y = NULL) +
      theme_minimal() +
      theme(axis.text.y = element_blank(), legend.position = "none",
            panel.grid = element_blank(),
            axis.ticks.x = element_line(color = "grey50"),
            plot.title = element_text(face = "bold.italic"))
  }

  if (length(timeline_plots) > 0) {
    combined <- wrap_plots(timeline_plots, ncol = 1) +
      plot_annotation(title = "Travel Time Breakdown by Mode",
                      subtitle = od_label,
                      theme = theme(plot.title = element_text(face = "bold")))

    ggsave(path(path(pilot_output_dir, "figures"), paste0("timeline_", tag, ".png")),
           combined, width = 8, height = 6, dpi = 300, bg = "white")
    file_copy(path(path(pilot_output_dir, "figures"), paste0("timeline_", tag, ".png")),
              path(docs_figures_dir, paste0("timeline_", tag, ".png")),
              overwrite = TRUE)
    cat("  Saved: timeline_", tag, "\n", sep = "")
  }
}


# travel_time_matrix() ----
cat("\n=== travel_time_matrix() ===\n")

## Basic TTM (default parameters) ----
destination <- landmarks |> filter(id == "suwon_station") |> select(id, lon, lat)

# Default: time_window = 10 (simulates 10 departures), percentiles = 50 (median)
ttm_car <- travel_time_matrix(
  r5r_core, origins = origins, destinations = destination,
  mode = "CAR", departure_datetime = departure,
  max_trip_duration = max_trip_duration
)

ttm_transit <- travel_time_matrix(
  r5r_core, origins = origins, destinations = destination,
  mode = c("WALK", "TRANSIT"), departure_datetime = departure,
  max_walk_time = max_walk_time, max_trip_duration = max_trip_duration
)

ttm_walk <- travel_time_matrix(
  r5r_core, origins = origins, destinations = destination,
  mode = "WALK", departure_datetime = departure,
  max_trip_duration = max_trip_duration,
  walk_speed = 3.6
)

ttm_bicycle <- travel_time_matrix(
  r5r_core, origins = origins, destinations = destination,
  mode = "BICYCLE", departure_datetime = departure,
  max_trip_duration = max_trip_duration,
  bike_speed = bicycle_speed, max_lts = bicycle_max_lts
)

cat("  Car coverage:", percent(mean(!is.na(ttm_car$travel_time_p50))), "\n")
cat("  Transit coverage:", percent(mean(!is.na(ttm_transit$travel_time_p50))), "\n")
cat("  Walk coverage:", percent(mean(!is.na(ttm_walk$travel_time_p50))), "\n")
cat("  Bicycle coverage:", percent(mean(!is.na(ttm_bicycle$travel_time_p50))), "\n")

## Print head for qmd verification ----
cat("\n--- TTM Head Output (for qmd) ---\n")
cat("\n# Car\n")
print(head(ttm_car, 3))
cat("\n# Transit\n")
print(head(ttm_transit, 3))
cat("\n# Walk\n")
print(head(ttm_walk, 3))
cat("\n# Bicycle\n")
print(head(ttm_bicycle, 3))

## Save individual TTM heads to CSV for qmd ----
write.csv(head(ttm_car, 3), path(pilot_output_dir, "data", "ttm_car_head.csv"), row.names = FALSE)
write.csv(head(ttm_transit, 3), path(pilot_output_dir, "data", "ttm_transit_head.csv"), row.names = FALSE)
write.csv(head(ttm_walk, 3), path(pilot_output_dir, "data", "ttm_walk_head.csv"), row.names = FALSE)
write.csv(head(ttm_bicycle, 3), path(pilot_output_dir, "data", "ttm_bicycle_head.csv"), row.names = FALSE)
cat("\n  Saved: ttm_*_head.csv (4 files)\n")

## Fix Walk/Bicycle column swap if needed ----
origin_ids <- origins$id

# Check and fix Walk
ttm_walk_fixed <- ttm_walk
if (!any(ttm_walk$from_id %in% origin_ids)) {
  ttm_walk_fixed <- ttm_walk |>
    rename(from_id = to_id, to_id = from_id) |>
    select(from_id, to_id, everything())
}

# Check and fix Bicycle
ttm_bicycle_fixed <- ttm_bicycle
if (!any(ttm_bicycle$from_id %in% origin_ids)) {
  ttm_bicycle_fixed <- ttm_bicycle |>
    rename(from_id = to_id, to_id = from_id) |>
    select(from_id, to_id, everything())
}

cat("\n# Walk (after swap fix)\n")
print(head(ttm_walk_fixed, 3))
cat("\n# Bicycle (after swap fix)\n")
print(head(ttm_bicycle_fixed, 3))

write.csv(head(ttm_walk_fixed, 3), path(pilot_output_dir, "data", "ttm_walk_fixed_head.csv"), row.names = FALSE)
write.csv(head(ttm_bicycle_fixed, 3), path(pilot_output_dir, "data", "ttm_bicycle_fixed_head.csv"), row.names = FALSE)
cat("  Saved: ttm_walk_fixed_head.csv, ttm_bicycle_fixed_head.csv\n")

## Save TTM to CSV ----
ttm_combined <- bind_rows(
  ttm_car |> mutate(mode = "car"),
  ttm_transit |> mutate(mode = "transit"),
  ttm_walk |> mutate(mode = "walk"),
  ttm_bicycle |> mutate(mode = "bicycle")
)
write.csv(ttm_combined, path(pilot_output_dir, "data", "travel_time_matrix.csv"), row.names = FALSE)
cat("  Saved: travel_time_matrix.csv\n")

## Variation: time_window and multiple percentiles ----
# Larger time_window captures more schedule variability
# Multiple percentiles show uncertainty range (25th = optimistic, 75th = pessimistic)
ttm_transit_var <- travel_time_matrix(
  r5r_core, origins = origins, destinations = destination,
  mode = c("WALK", "TRANSIT"), departure_datetime = departure,
  time_window = 60,
  percentiles = c(25, 50, 75),
  max_walk_time = max_walk_time, max_trip_duration = max_trip_duration
)

ttm_transit_var

# Show percentile spread for a sample of tracts
cat("\n  Percentile variation (sample):\n")
ttm_transit_var_head <- ttm_transit_var |>
  filter(!is.na(travel_time_p50)) |>
  head(5)
print(ttm_transit_var_head)

## Save ttm_transit_var head for qmd ----
write.csv(ttm_transit_var_head, path(pilot_output_dir, "data", "ttm_transit_var_head.csv"), row.names = FALSE)
cat("  Saved: ttm_transit_var_head.csv\n")

## Visualize TTM (4 modes) ----
# Prepare data with quantile bins using cut_number (equal count per bin)
#
# Helper: determine which column contains tract IDs (from_id or to_id)
# r5r internally reverses origin/destination for WALK and BICYCLE modes
# when origins > destinations (performance optimization via one-to-many algorithm).
# This means the column containing tract IDs may be from_id or to_id depending on mode.
# See: https://ipeagit.github.io/r5r/news/ (reverse_if_direct_mode optimization)
get_tract_col <- function(ttm, tract_ids) {
  if (any(ttm$from_id %in% tract_ids)) "from_id" else "to_id"
}
tract_ids <- paldal_tracts$TOT_REG_CD

# Helper: format cut labels as integers (e.g., "(4,7]" instead of "(4.2,6.8]")
format_int_labels <- function(x) {
  # Extract numeric bounds from factor levels and round to integers
  lvls <- levels(x)
  new_lvls <- sapply(lvls, function(lbl) {
    nums <- as.numeric(regmatches(lbl, gregexpr("[0-9.]+", lbl))[[1]])
    if (length(nums) == 2) {
      paste0("(", round(nums[1]), ",", round(nums[2]), "]")
    } else {
      lbl
    }
  })
  levels(x) <- new_lvls
  x
}

ttm_car_sf <- paldal_tracts |>
  select(TOT_REG_CD, geom) |>
  inner_join(ttm_car, by = setNames(get_tract_col(ttm_car, tract_ids), "TOT_REG_CD")) |>
  mutate(time_bin = format_int_labels(cut_number(travel_time_p50, n = 5)))

ttm_transit_sf <- paldal_tracts |>
  select(TOT_REG_CD, geom) |>
  inner_join(ttm_transit, by = setNames(get_tract_col(ttm_transit, tract_ids), "TOT_REG_CD")) |>
  mutate(time_bin = format_int_labels(cut_number(travel_time_p50, n = 5)))

ttm_walk_sf <- paldal_tracts |>
  select(TOT_REG_CD, geom) |>
  inner_join(ttm_walk, by = setNames(get_tract_col(ttm_walk, tract_ids), "TOT_REG_CD")) |>
  mutate(time_bin = format_int_labels(cut_number(travel_time_p50, n = 5)))

ttm_bicycle_sf <- paldal_tracts |>
  select(TOT_REG_CD, geom) |>
  inner_join(ttm_bicycle, by = setNames(get_tract_col(ttm_bicycle, tract_ids), "TOT_REG_CD")) |>
  mutate(time_bin = format_int_labels(cut_number(travel_time_p50, n = 5)))

dest_sf <- st_as_sf(destination, coords = c("lon", "lat"), crs = 4326)

# Find tracts where car has biggest advantage over transit
comparison <- ttm_car_sf |>
  st_drop_geometry() |>
  select(TOT_REG_CD, car_time = travel_time_p50) |>
  inner_join(
    ttm_transit_sf |> st_drop_geometry() |>
      select(TOT_REG_CD, transit_time = travel_time_p50),
    by = "TOT_REG_CD"
  ) |>
  mutate(ratio = transit_time / car_time) |>
  filter(!is.na(ratio), is.finite(ratio))

cat("  Comparison stats: n =", nrow(comparison),
    "| ratio range:", round(min(comparison$ratio), 2), "-", round(max(comparison$ratio), 2), "\n")

# Top 2 tracts with highest ratio (biggest car advantage)
symbols <- c("*", "^")
top_car_advantage <- comparison |>
  slice_max(ratio, n = 2, with_ties = FALSE) |>
  mutate(label = symbols[row_number()])

cat("  Biggest car advantage:\n")
print(top_car_advantage)

# Get centroids for markers (keep same CRS as map data)
outlier_sf <- ttm_car_sf |>
  filter(TOT_REG_CD %in% top_car_advantage$TOT_REG_CD) |>
  st_centroid() |>
  left_join(top_car_advantage |> select(TOT_REG_CD, label), by = "TOT_REG_CD")

# Helper function to create travel time map
create_ttm_map <- function(data, title, show_outliers = FALSE) {
  p <- ggplot() +
    annotation_map_tile(type = "cartolight", zoomin = 0, progress = "none") +
    geom_sf(data = data, aes(fill = time_bin),
            color = "grey40", linewidth = 0.15, alpha = 0.85) +
    geom_sf(data = dest_sf, shape = 23, fill = "red", size = 3, stroke = 0.8) +
    scale_fill_brewer(name = "minutes", palette = "YlGnBu", direction = 1,
                      na.value = "grey80") +
    guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
    labs(title = title, x = NULL, y = NULL) +
    theme_bw() +
    theme(axis.text = element_blank(), axis.ticks = element_blank(),
          legend.position = "bottom",
          legend.key.size = unit(0.4, "cm"),
          plot.title = element_text(size = 10))

  if (show_outliers) {
    # White stroke behind black text
    p <- p +
      geom_sf_text(data = outlier_sf, aes(label = label),
                   size = 5, fontface = "bold", color = "white") +
      geom_sf_text(data = outlier_sf, aes(label = label),
                   size = 4, fontface = "bold", color = "black")
  }

  p
}

# Create 4 maps
p_car <- create_ttm_map(ttm_car_sf, "(a) Car", show_outliers = TRUE)
p_transit <- create_ttm_map(ttm_transit_sf, "(b) Transit", show_outliers = TRUE)
p_walk <- create_ttm_map(ttm_walk_sf, "(c) Walk")
p_bicycle <- create_ttm_map(ttm_bicycle_sf, "(d) Bicycle")

# Caption for outlier markers: ^ first, then *
# Format as natural sentence: "Note: ^ takes X min by car but Y min by transit; * takes ..."
outlier_sorted <- top_car_advantage |>
  arrange(desc(label))  # ^ comes before * in descending order

outlier_note <- paste0(
  "Note: ", outlier_sorted$label[1], " takes ", outlier_sorted$car_time[1],
  " min by car but ", outlier_sorted$transit_time[1], " min by transit; ",
  outlier_sorted$label[2], " takes ", outlier_sorted$car_time[2],
  " min by car but ", outlier_sorted$transit_time[2], " min by transit."
)

# Add caption to (b) Transit panel only
p_transit <- p_transit +
  labs(caption = outlier_note) +
  theme(plot.caption = element_text(size = 7, color = "grey30", hjust = 1))

# Combine with patchwork (2x2 layout)
ttm_map <- (p_car + p_transit) / (p_walk + p_bicycle) +
  plot_annotation(
    title = "Travel Time to Suwon Station",
    subtitle = paste0("From ", nrow(origins), " census tract centroids | \u25C7 Suwon Station (destination)"),
    theme = theme(plot.title = element_text(face = "bold", size = 12),
                  plot.subtitle = element_text(size = 9, color = "grey40"))
  )

ggsave(path(path(pilot_output_dir, "figures"), "travel_time_matrix.png"),
       ttm_map, width = 8, height = 8, dpi = 300, bg = "white")
file_copy(path(path(pilot_output_dir, "figures"), "travel_time_matrix.png"),
          path(docs_figures_dir, "travel_time_matrix.png"),
          overwrite = TRUE)
cat("  Saved: travel_time_matrix\n")


# accessibility() ----
cat("\n=== accessibility() ===\n")

# This demonstrates n x m scale:
# - n origins (census tract centroids)
# - m destinations (restaurants within ~20km)
# The function internally computes travel times and applies decay,
# more efficient than storing full OD matrix for large datasets.

## Load Restaurants ----
restaurants <- read.csv(path(pilot_input_dir, "restaurants.csv"))
cat("  n x m scale:", nrow(origins), "origins x", nrow(restaurants), "destinations\n")

## Calculate Accessibility ----
# Each restaurant = 1 opportunity
destinations <- restaurants |>
  mutate(opportunities = 1)

# Step function (cumulative): count opportunities within cutoff
# Different cutoffs reflect realistic travel expectations per mode
car_cutoff <- 10       # Car: 10-min radius
transit_cutoff <- 20   # Transit: 20-min radius (includes walk + wait + ride)
walk_cutoff <- 15      # Walk: 15-min radius
bicycle_cutoff <- 10   # Bicycle: 10-min radius (similar speed range as car in urban)

access_car <- accessibility(
  r5r_core, origins = origins, destinations = destinations,
  mode = "CAR", departure_datetime = departure,
  cutoffs = car_cutoff, decay_function = "step",
  max_trip_duration = 30
)

access_transit <- accessibility(
  r5r_core, origins = origins, destinations = destinations,
  mode = c("WALK", "TRANSIT"), departure_datetime = departure,
  cutoffs = transit_cutoff, decay_function = "step",
  max_walk_time = max_walk_time, max_trip_duration = 30
)

access_walk <- accessibility(
  r5r_core, origins = origins, destinations = destinations,
  mode = "WALK", departure_datetime = departure,
  cutoffs = walk_cutoff, decay_function = "step",
  max_trip_duration = 30, walk_speed = 3.6
)

access_bicycle <- accessibility(
  r5r_core, origins = origins, destinations = destinations,
  mode = "BICYCLE", departure_datetime = departure,
  cutoffs = bicycle_cutoff, decay_function = "step",
  max_trip_duration = 30, bike_speed = bicycle_speed, max_lts = bicycle_max_lts
)

## Save Accessibility to CSV ----
access_combined <- bind_rows(
  access_car |> mutate(mode = "car", cutoff = car_cutoff),
  access_transit |> mutate(mode = "transit", cutoff = transit_cutoff),
  access_walk |> mutate(mode = "walk", cutoff = walk_cutoff),
  access_bicycle |> mutate(mode = "bicycle", cutoff = bicycle_cutoff)
)
write.csv(access_combined, path(pilot_output_dir, "data", "accessibility.csv"), row.names = FALSE)
cat("  Saved: accessibility.csv\n")

## Print and save accessibility heads for qmd ----
cat("\n--- Accessibility Head Output (for qmd) ---\n")
cat("\n# access_car (head 5)\n")
print(head(access_car, 5))
cat("\n# access_transit (head 5)\n")
print(head(access_transit, 5))

write.csv(head(access_car, 5), path(pilot_output_dir, "data", "access_car_head.csv"), row.names = FALSE)
write.csv(head(access_transit, 5), path(pilot_output_dir, "data", "access_transit_head.csv"), row.names = FALSE)
cat("  Saved: access_car_head.csv, access_transit_head.csv\n")

## Summary ----
cat("\n  Accessibility summary:\n")
cat("    Car (", car_cutoff, " min):\n", sep = "")
print(summary(access_car$accessibility))
cat("    Transit (", transit_cutoff, " min):\n", sep = "")
print(summary(access_transit$accessibility))
cat("    Walk (", walk_cutoff, " min):\n", sep = "")
print(summary(access_walk$accessibility))
cat("    Bicycle (", bicycle_cutoff, " min):\n", sep = "")
print(summary(access_bicycle$accessibility))

## Visualize: Accessibility Concept Figure ----
# Show reachable restaurants with route segments from origin tract
cat("\n--- Creating accessibility concept figure ---\n")

# Prepare restaurant points
restaurants_sf <- st_as_sf(restaurants, coords = c("lon", "lat"), crs = 4326)

# Pick a central tract to highlight (one with moderate accessibility)
# Use median accessibility tract for demonstration
median_access <- median(access_car$accessibility, na.rm = TRUE)
highlight_tract_id <- access_car |>
  mutate(diff = abs(accessibility - median_access)) |>
  slice_min(diff, n = 1, with_ties = FALSE) |>
  pull(id)

highlight_tract <- paldal_tracts |>
  filter(TOT_REG_CD == highlight_tract_id)

# Get centroid of highlighted tract
highlight_centroid <- highlight_tract |>
  st_centroid() |>
  st_transform(4326)

origin_coords <- data.frame(
  id = highlight_tract_id,
  lon = st_coordinates(highlight_centroid)[1],
  lat = st_coordinates(highlight_centroid)[2]
)

# Calculate which restaurants are reachable within 10 min by car from this tract
ttm_highlight <- travel_time_matrix(
  r5r_core,
  origins = origin_coords,
  destinations = restaurants |> mutate(id = as.character(row_number())) |> select(id, lon, lat),
  mode = "CAR", departure_datetime = departure,
  max_trip_duration = 30
)

reachable_ids <- ttm_highlight |>
  filter(travel_time_p50 <= car_cutoff) |>
  pull(to_id) |>
  as.integer()

restaurants_sf <- restaurants_sf |>
  mutate(row_id = row_number(),
         reachable = row_id %in% reachable_ids)

n_reachable <- sum(restaurants_sf$reachable)
cat("  Highlighted tract:", highlight_tract_id, "\n")
cat("  Restaurants reachable within", car_cutoff, "min:", n_reachable, "\n")

# Calculate routes to ALL reachable restaurants
# We'll aggregate overlapping segments for visualization
reachable_dest <- restaurants_sf |>
  filter(reachable) |>
  mutate(
    id = as.character(row_id),
    lon = st_coordinates(geometry)[, 1],
    lat = st_coordinates(geometry)[, 2]
  ) |>
  st_drop_geometry() |>
  select(id, lon, lat)

routes_to_restaurants <- detailed_itineraries(
  r5r_core, origins = origin_coords, destinations = reachable_dest,
  mode = "CAR", departure_datetime = departure,
  max_trip_duration = 30, shortest_path = TRUE
)

cat("  Routes calculated:", nrow(routes_to_restaurants), "segments to", n_reachable, "restaurants\n")

# Aggregate overlapping route segments
# Convert to line segments and count how many routes use each road
routes_lines <- routes_to_restaurants |>
  st_cast("LINESTRING") |>
  # Create a simplified geometry key for grouping (round coordinates)
  mutate(geom_key = st_as_text(st_transform(geometry, 5179) |>
                                 st_simplify(dTolerance = 10) |>
                                 st_transform(4326))) |>
  group_by(geom_key) |>
  summarise(
    n_routes = n(),
    geometry = st_union(geometry),
    .groups = "drop"
  ) |>
  # Normalize for visualization
  mutate(width_scaled = scales::rescale(n_routes, to = c(0.3, 2.5)))

cat("  Aggregated to", nrow(routes_lines), "unique segments\n")
cat("  Route overlap range:", min(routes_lines$n_routes), "-", max(routes_lines$n_routes), "\n")

# Zoom: reachable restaurants 범위 + padding
reachable_only <- restaurants_sf |> filter(reachable)
reachable_bbox <- st_bbox(reachable_only)

cat("  Reachable bbox:", reachable_bbox["xmin"], reachable_bbox["xmax"],
    reachable_bbox["ymin"], reachable_bbox["ymax"], "\n")

padding_x <- (reachable_bbox["xmax"] - reachable_bbox["xmin"]) * 0.05
padding_y <- (reachable_bbox["ymax"] - reachable_bbox["ymin"]) * 0.05

xlim_range <- c(as.numeric(reachable_bbox["xmin"] - padding_x),
                as.numeric(reachable_bbox["xmax"] + padding_x))
ylim_range <- c(as.numeric(reachable_bbox["ymin"] - padding_y),
                as.numeric(reachable_bbox["ymax"] + padding_y))

cat("  Zoom xlim:", xlim_range[1], "-", xlim_range[2], "\n")
cat("  Zoom ylim:", ylim_range[1], "-", ylim_range[2], "\n")

# Create concept figure with zoomed view and routes
# Line width varies by how many routes share each segment
# Ensure all layers are in WGS84 for proper coord_sf alignment
routes_lines_4326 <- st_transform(routes_lines, 4326)
restaurants_unreachable_4326 <- st_transform(restaurants_sf |> filter(!reachable), 4326)
reachable_only_4326 <- st_transform(reachable_only, 4326)
highlight_centroid_4326 <- st_transform(highlight_centroid, 4326)
paldal_tracts_4326 <- st_transform(paldal_tracts, 4326)
paldal_boundary_4326 <- st_transform(st_union(paldal_tracts), 4326)

p_concept <- ggplot() +
  annotation_map_tile(type = "cartolight", zoomin = 0, alpha = 0.5, progress = "none") +
  # District boundary (thick)
  geom_sf(data = paldal_boundary_4326, fill = NA, color = "grey30", linewidth = 1.2) +
  # Census tracts (thin)
  geom_sf(data = paldal_tracts_4326, fill = NA, color = "grey60", linewidth = 0.3) +
  # Route segments with varying width (thicker = more routes share this road)
  geom_sf(data = routes_lines_4326, aes(linewidth = width_scaled),
          color = "#3498DB", alpha = 0.35) +
  scale_linewidth_identity() +
  # Unreachable restaurants (gray) - within view but beyond cutoff
  geom_sf(data = restaurants_unreachable_4326,
          color = "grey60", size = 1.5, alpha = 0.4) +
  # Reachable restaurants (red)
  geom_sf(data = reachable_only_4326,
          color = "#E74C3C", size = 2, alpha = 0.7) +
  # Origin tract centroid (larger, blue)
  geom_sf(data = highlight_centroid_4326, shape = 21, fill = "#3498DB",
          color = "white", size = 6, stroke = 2) +
  # Zoom to reachable area
  coord_sf(xlim = xlim_range, ylim = ylim_range, crs = 4326) +
  labs(
    title = "Accessibility: Counting Reachable Destinations",
    subtitle = paste0("From origin (blue): ", n_reachable,
                      " restaurants (red) reachable within ", car_cutoff, " min by car"),
    caption = "Gray points: beyond threshold | Line thickness: route overlap",
    x = NULL, y = NULL
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(color = "grey30"),
    plot.caption = element_text(color = "grey50", hjust = 0),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "none"
  )

ggsave(path(path(pilot_output_dir, "figures"), "accessibility_concept.png"),
       p_concept, width = 8, height = 7, dpi = 300, bg = "white")
file_copy(path(path(pilot_output_dir, "figures"), "accessibility_concept.png"),
          path(docs_figures_dir, "accessibility_concept.png"),
          overwrite = TRUE)
cat("  Saved: accessibility_concept.png\n")

## Visualize: Map of accessibility (4 modes) ----
# Join accessibility to tract geometry
access_car_sf <- paldal_tracts |>
  select(TOT_REG_CD, geom) |>
  inner_join(access_car |> select(id, accessibility), by = c("TOT_REG_CD" = "id")) |>
  mutate(access_bin = format_int_labels(cut_number(accessibility, n = 5)))

access_transit_sf <- paldal_tracts |>
  select(TOT_REG_CD, geom) |>
  inner_join(access_transit |> select(id, accessibility), by = c("TOT_REG_CD" = "id")) |>
  mutate(access_bin = format_int_labels(cut_number(accessibility, n = 5)))

access_walk_sf <- paldal_tracts |>
  select(TOT_REG_CD, geom) |>
  inner_join(access_walk |> select(id, accessibility), by = c("TOT_REG_CD" = "id")) |>
  mutate(access_bin = format_int_labels(cut_number(accessibility, n = 5)))

access_bicycle_sf <- paldal_tracts |>
  select(TOT_REG_CD, geom) |>
  inner_join(access_bicycle |> select(id, accessibility), by = c("TOT_REG_CD" = "id")) |>
  mutate(access_bin = format_int_labels(cut_number(accessibility, n = 5)))

# Helper function for accessibility maps
create_access_map <- function(data, title) {
  ggplot() +
    annotation_map_tile(type = "cartolight", zoomin = 0, progress = "none") +
    geom_sf(data = data, aes(fill = access_bin),
            color = "grey40", linewidth = 0.15, alpha = 0.85) +
    scale_fill_brewer(name = "count", palette = "YlOrRd", direction = 1,
                      na.value = "grey80") +
    guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
    labs(title = title, x = NULL, y = NULL) +
    theme_bw() +
    theme(axis.text = element_blank(), axis.ticks = element_blank(),
          legend.position = "bottom", legend.key.size = unit(0.4, "cm"),
          plot.title = element_text(size = 10))
}

# Create 4 accessibility maps
p_access_car <- create_access_map(access_car_sf, paste0("(a) Car: within ", car_cutoff, " min"))
p_access_transit <- create_access_map(access_transit_sf, paste0("(b) Transit: within ", transit_cutoff, " min"))
p_access_walk <- create_access_map(access_walk_sf, paste0("(c) Walk: within ", walk_cutoff, " min"))
p_access_bicycle <- create_access_map(access_bicycle_sf, paste0("(d) Bicycle: within ", bicycle_cutoff, " min"))

# Combine (2x2 layout)
access_map <- (p_access_car + p_access_transit) / (p_access_walk + p_access_bicycle) +
  plot_annotation(
    title = "Restaurants Reachable from Each Census Tract",
    subtitle = paste0(nrow(origins), " tract centroids -> ", nrow(restaurants), " restaurants"),
    theme = theme(plot.title = element_text(face = "bold", size = 12),
                  plot.subtitle = element_text(size = 9, color = "grey40"))
  )

ggsave(path(path(pilot_output_dir, "figures"), "accessibility_map.png"),
       access_map, width = 8, height = 8, dpi = 300, bg = "white")
file_copy(path(path(pilot_output_dir, "figures"), "accessibility_map.png"),
          path(docs_figures_dir, "accessibility_map.png"),
          overwrite = TRUE)
cat("  Saved: accessibility_map\n")


# Cleanup ----
stop_r5(r5r_core)
gc()

cat("\n=== COMPLETE ===\n")
cat("Outputs saved to:\n")
cat("  workflows/pilot/output/\n")
cat("    figures/  - PNG visualizations\n")
cat("    data/     - CSV and GeoPackage files\n")
cat("  docs/assets/figures/ch3/  - Copied for Quarto book\n")
