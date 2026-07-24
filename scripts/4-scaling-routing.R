# 1. Setup =====================================================================

options(java.parameters = c("-Xmx8G", "-XX:+UseG1GC"))

pacman::p_load(
  tidyverse, sf, r5r, arrow, fs, glue, scales
)

PROJECT_ROOT <- normalizePath(
  Sys.getenv("ACCESS_DECLINE_ROOT", unset = "."),
  winslash = "/", mustWork = TRUE
)
if (!file.exists(file.path(PROJECT_ROOT, ".gitignore"))) {
  stop("Run from the repository root or set ACCESS_DECLINE_ROOT.")
}
DATA_ROOT    <- path(PROJECT_ROOT, "data")
setwd(PROJECT_ROOT)

# Directories
WORKFLOW_DIR <- "workflows/scaling_sample"
RESULT_DIR <- path(WORKFLOW_DIR, "results")

# RESULT_DIR 초기화 (기존 결과물 모두 삭제 후 재생성)
unlink(RESULT_DIR, recursive = TRUE)
dir_create(RESULT_DIR)
message(glue("Results directory initialized: {RESULT_DIR}"))


# Sample districts (5 diverse contexts)
DISTRICT_CODES <- c("26010", "32010", "32030", "38600", "39010")

DISTRICT_INFO <- tribble(
  ~district_cd, ~district_name, ~context,
  "26010", "Ulsan Jung-gu", "Dense metropolitan core",
  "32010", "Chuncheon", "Provincial capital",
  "32030", "Gangneung", "Coastal mid-size city",
  "38600", "Hapcheon", "Rural, low-density county",
  "39010", "Jeju City", "Island, isolated network"
)

# Analysis date (must be within GTFS validity period)
ANALYSIS_DATE <- "2023-03-15"

# Routing parameters
MAX_CAR_MIN <- 45
MAX_TRANSIT_MIN <- 60
MAX_WALK_MIN <- 60
MAX_BICYCLE_MIN <- 60

WALK_SPEED <- 3.6
BICYCLE_SPEED <- 12
BICYCLE_LTS <- 2

TIME_WINDOW <- 60
TIME_SLOTS <- c("08:00:00", "12:00:00", "18:00:00")

BUFFER_KM <- 25

message("=" |> strrep(60))
message("CHAPTER 4: SCALING ROUTING (4 MODES)")
message("=" |> strrep(60))
message(glue("  Districts: {length(DISTRICT_CODES)}"))


# 2. Load Pre-prepared Data ====================================================

message("\n[2] Loading pre-prepared data...")

# Origins (census polygons with population)
origins_poly <- st_read(path(WORKFLOW_DIR, "origins/census.gpkg"), quiet = TRUE)
message(glue("  Origins: {nrow(origins_poly)} census tracts"))

# Destinations (banks)
destinations_df <- read_csv(path(WORKFLOW_DIR, "destinations/banks.csv"),
                            show_col_types = FALSE)
message(glue("  Destinations: {nrow(destinations_df)} banks"))

# Compute centroids for routing
origins_centroid <- origins_poly |>
  st_transform(5179) |>
  st_centroid() |>
  st_transform(4326)

# Extract coordinates from centroids
coords <- st_coordinates(origins_centroid)
origins_df <- origins_centroid |>
  st_drop_geometry() |>
  mutate(lon = coords[, 1], lat = coords[, 2])


# 3. Routing ===================================================================

message("\n[3] Running 4-mode routing...")

for (dc in DISTRICT_CODES) {

  out_file <- path(RESULT_DIR, glue("{dc}.parquet"))

  # Skip if already done
  if (file_exists(out_file)) {
    message(glue("  Skip {dc} (results exist)"))
    next
  }

  district_name <- DISTRICT_INFO |> filter(district_cd == dc) |> pull(district_name)
  message(glue("\n  Processing: {dc} ({district_name})"))

  tryCatch({

    # Filter origins for this district
    # NOTE: Must convert to data.frame explicitly for r5r to preserve IDs correctly
    district_origins <- origins_df |>
      filter(district_cd == dc) |>
      mutate(id = as.character(id)) |>
      select(id, lon, lat) |>
      as.data.frame()

    if (nrow(district_origins) == 0) {
      message("    No origins, skipping...")
      next
    }

    # Filter destinations within buffer
    origins_union <- origins_poly |>
      filter(district_cd == dc) |>
      st_transform(5179) |>
      st_union() |>
      st_buffer(BUFFER_KM * 1000) |>
      st_transform(4326)

    district_destinations <- destinations_df |>
      st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
      st_filter(origins_union) |>
      mutate(
        id = as.character(id),
        lon = st_coordinates(geometry)[, 1],
        lat = st_coordinates(geometry)[, 2]
      ) |>
      st_drop_geometry() |>
      select(id, lon, lat) |>
      as.data.frame()

    if (nrow(district_destinations) == 0) {
      message("    No destinations in range, skipping...")
      next
    }

    message(glue("    Origins: {nrow(district_origins)}, Destinations: {nrow(district_destinations)}"))

    # Build network
    network_dir <- path(WORKFLOW_DIR, "districts", dc)
    r5r_core <- setup_r5(network_dir, verbose = FALSE)

    # Single departure for time-invariant modes
    departure_static <- as.POSIXct(paste(ANALYSIS_DATE, "12:00:00"), tz = "Asia/Seoul")

    # --- CAR ---
    message("    Car...")
    ttm_car <- travel_time_matrix(
      r5r_core,
      origins = district_origins,
      destinations = district_destinations,
      mode = "CAR",
      departure_datetime = departure_static,
      max_trip_duration = MAX_CAR_MIN,
      time_window = 1,
      verbose = FALSE
    ) |>
      mutate(mode = "car")

    # --- WALK ---
    message("    Walk...")
    ttm_walk <- travel_time_matrix(
      r5r_core,
      origins = district_origins,
      destinations = district_destinations,
      mode = "WALK",
      departure_datetime = departure_static,
      max_trip_duration = MAX_WALK_MIN,
      walk_speed = WALK_SPEED,
      time_window = 1,
      verbose = FALSE
    ) |>
      mutate(mode = "walk")

    # --- BICYCLE ---
    message("    Bicycle...")
    ttm_bicycle <- travel_time_matrix(
      r5r_core,
      origins = district_origins,
      destinations = district_destinations,
      mode = "BICYCLE",
      departure_datetime = departure_static,
      max_trip_duration = MAX_BICYCLE_MIN,
      bike_speed = BICYCLE_SPEED,
      max_lts = BICYCLE_LTS,
      time_window = 1,
      verbose = FALSE
    ) |>
      mutate(mode = "bicycle")

    # --- TRANSIT (multiple time slots) ---
    message("    Transit...")
    ttm_transit <- map(TIME_SLOTS, function(time_str) {
      departure <- as.POSIXct(paste(ANALYSIS_DATE, time_str), tz = "Asia/Seoul")

      result <- tryCatch({
        travel_time_matrix(
          r5r_core,
          origins = district_origins,
          destinations = district_destinations,
          mode = c("WALK", "TRANSIT"),
          departure_datetime = departure,
          time_window = TIME_WINDOW,
          max_trip_duration = MAX_TRANSIT_MIN,
          max_walk_time = 30,
          verbose = FALSE
        )
      }, error = function(e) {
        message(glue("      Transit error at {time_str}: {e$message}"))
        return(NULL)
      })

      if (is.null(result) || nrow(result) == 0) {
        return(NULL)
      }

      result |>
        mutate(mode = "transit", departure_time = substr(time_str, 1, 2))
    }) |>
      compact() |>
      bind_rows()

    # Combine all modes
    results <- bind_rows(ttm_car, ttm_walk, ttm_bicycle, ttm_transit) |>
      mutate(district_cd = dc)

    # Cleanup
    stop_r5(r5r_core)
    rJava::.jgc(R.gc = TRUE)

    # Save
    write_parquet(results, out_file, compression = "snappy")
    message(glue("    Saved: {comma(nrow(results))} rows"))

  }, error = function(e) {
    message(glue("    Error: {e$message}"))
  })
}


# 4. Compute Accessibility =====================================================

message("\n[4] Computing accessibility...")

# Load all results
result_files <- dir_ls(RESULT_DIR, glob = "*.parquet")

if (length(result_files) == 0) {
  stop("No result files found. Run routing first.")
}

ttm_all <- open_dataset(result_files) |> collect()
message(glue("  Total TTM rows: {comma(nrow(ttm_all))}"))

# Detect origin column per district-mode (r5r may swap for WALK/BICYCLE)
origin_map <- ttm_all |>
  group_by(district_cd, mode) |>
  summarize(sample_from = as.character(from_id[1]), .groups = "drop") |>
  mutate(
    use_to_id = !startsWith(sample_from, as.character(district_cd))
  ) |>
  select(district_cd, mode, use_to_id)

# Log swapped cases
swapped <- origin_map |> filter(use_to_id)
if (nrow(swapped) > 0) {
  message("  Swapped from_id/to_id detected:")
  for (i in seq_len(nrow(swapped))) {
    message(glue("    - {swapped$district_cd[i]} / {swapped$mode[i]}"))
  }
}

# Add origin_id column based on detection
ttm_all <- ttm_all |>
  left_join(origin_map, by = c("district_cd", "mode")) |>
  mutate(
    origin_id = if_else(use_to_id, as.character(to_id), as.character(from_id))
  )

# Compute accessibility per mode
compute_accessibility <- function(ttm_data, mode_name) {

  has_time_slots <- "departure_time" %in% names(ttm_data) &&
    n_distinct(ttm_data$departure_time) > 1

  if (has_time_slots) {
    result <- ttm_data |>
      group_by(district_cd, origin_id, departure_time) |>
      summarize(
        n_15min = sum(travel_time_p50 <= 15, na.rm = TRUE),
        n_30min = sum(travel_time_p50 <= 30, na.rm = TRUE),
        n_45min = sum(travel_time_p50 <= 45, na.rm = TRUE),
        n_60min = sum(travel_time_p50 <= 60, na.rm = TRUE),
        .groups = "drop"
      ) |>
      group_by(district_cd, origin_id) |>
      summarize(
        n_15min = mean(n_15min),
        n_30min = mean(n_30min),
        n_45min = mean(n_45min),
        n_60min = mean(n_60min),
        .groups = "drop"
      )
  } else {
    result <- ttm_data |>
      group_by(district_cd, origin_id) |>
      summarize(
        n_15min = sum(travel_time_p50 <= 15, na.rm = TRUE),
        n_30min = sum(travel_time_p50 <= 30, na.rm = TRUE),
        n_45min = sum(travel_time_p50 <= 45, na.rm = TRUE),
        n_60min = sum(travel_time_p50 <= 60, na.rm = TRUE),
        .groups = "drop"
      )
  }

  result |>
    rename(census_id = origin_id) |>
    mutate(mode = mode_name)
}

access_car <- ttm_all |> filter(mode == "car") |> compute_accessibility("car")
access_transit <- ttm_all |> filter(mode == "transit") |> compute_accessibility("transit")
access_walk <- ttm_all |> filter(mode == "walk") |> compute_accessibility("walk")
access_bicycle <- ttm_all |> filter(mode == "bicycle") |> compute_accessibility("bicycle")

access_all <- bind_rows(access_car, access_transit, access_walk, access_bicycle)

# Zero-fill: origins with no reachable destination count as 0, not dropped
pop_lookup <- origins_df |>
  transmute(census_id = as.character(id), district_cd, population)

access_pop <- expand_grid(
  census_id = pop_lookup$census_id,
  mode = c("car", "transit", "walk", "bicycle")
) |>
  left_join(
    access_all |>
      mutate(census_id = as.character(census_id)) |>
      select(-district_cd),
    by = c("census_id", "mode")
  ) |>
  mutate(across(starts_with("n_"), ~ replace_na(.x, 0))) |>
  left_join(pop_lookup, by = "census_id")

write_csv(access_pop, path(RESULT_DIR, "accessibility_by_tract.csv"))
message(glue("  Saved: accessibility_by_tract.csv"))


# 5. District Summary ==========================================================

message("\n[5] Creating summaries...")

district_summary <- access_pop |>
  filter(population > 0) |>
  group_by(district_cd, mode) |>
  summarize(
    n_tracts = n(),
    total_pop = sum(population, na.rm = TRUE),
    mean_15min = weighted.mean(n_15min, population, na.rm = TRUE),
    mean_30min = weighted.mean(n_30min, population, na.rm = TRUE),
    .groups = "drop"
  ) |>
  left_join(DISTRICT_INFO, by = "district_cd") |>
  arrange(district_cd, mode)

write_csv(district_summary, path(RESULT_DIR, "district_summary.csv"))

# Wide format
mode_wide <- district_summary |>
  select(district_name, context, mode, mean_30min) |>
  mutate(mean_30min = round(mean_30min, 1)) |>
  pivot_wider(names_from = mode, values_from = mean_30min)

write_csv(mode_wide, path(RESULT_DIR, "mode_comparison_wide.csv"))

# Copy to docs
DOCS_OUTPUT_DIR <- "docs/assets/data/ch4"
DOCS_FIG_DIR <- "docs/assets/figures/ch4"
dir_create(DOCS_OUTPUT_DIR)
dir_create(DOCS_FIG_DIR)

file_copy(path(RESULT_DIR, "district_summary.csv"),
          path(DOCS_OUTPUT_DIR, "district_summary.csv"), overwrite = TRUE)
file_copy(path(RESULT_DIR, "mode_comparison_wide.csv"),
          path(DOCS_OUTPUT_DIR, "mode_comparison_wide.csv"), overwrite = TRUE)


# 6. Visualization =============================================================

message("\n[6] Creating visualizations...")

library(ggplot2)

# Prepare data for plotting
plot_data <- district_summary |>
  pivot_longer(
    cols = c(mean_15min, mean_30min),
    names_to = "threshold",
    values_to = "banks"
  ) |>
  mutate(
    threshold = case_when(
      threshold == "mean_15min" ~ "15 min",
      threshold == "mean_30min" ~ "30 min"
    ),
    threshold = factor(threshold, levels = c("15 min", "30 min")),
    mode = factor(mode, levels = c("car", "transit", "bicycle", "walk"))
  )

# Plot: threshold panels, districts on x-axis, grouped by mode
p <- ggplot(plot_data, aes(x = district_name, y = banks, fill = mode)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(
    aes(label = round(banks, 0)),
    position = position_dodge(width = 0.8),
    vjust = -0.3,
    size = 2.5
  ) +
  facet_wrap(~threshold, ncol = 2) +
  scale_fill_manual(
    values = c("car" = "#e41a1c", "transit" = "#377eb8",
               "bicycle" = "#4daf4a", "walk" = "#984ea3"),
    labels = c("Car", "Transit", "Bicycle", "Walk")
  ) +
  labs(
    x = NULL,
    y = NULL,
    fill = "Mode"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    strip.background = element_rect(fill = "grey90")
  )

ggsave(
  path(DOCS_FIG_DIR, "district_accessibility_comparison.png"),
  p, width = 8, height = 4, dpi = 300
)
message("  Saved: district_accessibility_comparison.png")


# 7. Summary ===================================================================

message("\n" |> paste0(strrep("=", 60)))
message("COMPLETE")
message(strrep("=", 60))

cat("\n--- Banks within 30 min (4 modes) ---\n")
print(mode_wide)

cat("\nOutput files:\n")
dir_ls(RESULT_DIR) |> path_file() |> cat(sep = "\n  ")
