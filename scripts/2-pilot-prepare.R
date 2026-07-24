# Setup ----
pacman::p_load(
  data.table, fs, tictoc, scales,
  tidyverse, glue, sf, patchwork,
  showtext, tidytransit, ggrepel, osmdata
)

rm(list = ls())  # Clear workspace
graphics.off()   # Clear graphics

# Font setup
showtext_opts(dpi = 300)
showtext_auto()
font_add_google("Source Sans Pro", "Source Sans Pro")
theme_set(theme_bw(base_family = "Source Sans Pro"))

# Path setup
PROJECT_ROOT <- normalizePath(
  Sys.getenv("ACCESS_DECLINE_ROOT", unset = "."),
  winslash = "/", mustWork = TRUE
)
if (!file.exists(file.path(PROJECT_ROOT, ".gitignore"))) {
  stop("Run from the repository root or set ACCESS_DECLINE_ROOT.")
}
DATA_ROOT    <- path(PROJECT_ROOT, "data")
setwd(PROJECT_ROOT)

to_wsl <- function(win_path) {
  paste0("/mnt/", tolower(substr(win_path, 1, 1)), "/", substr(win_path, 4, nchar(win_path)))
}

# Parameters ----
## Pilot region definition ----
PILOT_REGION  <- "수원시 팔달구"  # Pilot region name (Korean)
PILOT_YEAR    <- 2024             # Analysis year
region_code   <- "paldal"         # Output filename code

## Buffer strategy ----
# Target: 30-minute transit accessibility
#   - Walking to/from stops: 10 min (~800m each way) = 1.5 km
#   - Wait + transfer time: 5 min
#   - In-vehicle time: 15 min @ 20 km/h = 5 km
#   - Total transit catchment: 5 + 1.5 = 6.5 km → Round to 7 km
# 
# References:
#   - Neutens et al. (2010) J. Transport Geography: 20-30 min for daily services
#   - Benenson et al. (2011) J. Transport Geography: 15-20 min for banks
#   - Korea Household Travel Survey (2021): 15-25 min for banking trips

BASIC_BUFFER_METERS    <- 1500   # Walking catchment (access to stops)
EXTENDED_BUFFER_METERS <- 7000   # Transit catchment (30-min accessibility)
TOTAL_BUFFER_METERS    <- BASIC_BUFFER_METERS + EXTENDED_BUFFER_METERS  # 8.5 km

## Directory structure ----
pilot_root        <- path(PROJECT_ROOT, "workflows/pilot")
pilot_data_dir    <- path(pilot_root, "data")           # Final outputs (GTFS, OSM)
pilot_results_dir <- path(pilot_root, "figures")        # Working figures
docs_figures_dir  <- path(PROJECT_ROOT, "docs/assets/figures/ch2")  # Chapter 2 figures

# Create directories
dir_create(pilot_data_dir, recurse = TRUE)
dir_create(pilot_results_dir, recurse = TRUE)
dir_create(docs_figures_dir, recurse = TRUE)

## Output file paths ----
gtfs_out_file <- path(pilot_data_dir, glue("gtfs_{PILOT_YEAR}_{region_code}.zip"))
osm_out_file  <- path(pilot_data_dir, glue("osm_{PILOT_YEAR}_{region_code}.osm.pbf"))


# PART 1: Define Pilot Region Spatial Extent ----
message("\n", strrep("=", 70))
message("PART 1: Pilot Region Boundary Definition")
message(strrep("=", 70))

## Load census tract boundaries ----
census_tract_sf <- st_read(path(DATA_ROOT, "census/oa_bnd.gpkg"), quiet = TRUE)

## Extract pilot region ----
pilot_tract_sf <- census_tract_sf %>%
  filter(SIGUNGU_NM == PILOT_REGION)

message("✅ Pilot region: ", PILOT_REGION)
message("   Census tracts: ", nrow(pilot_tract_sf))

## Create buffered bounding box ----
# Buffer strategy:
#   - Basic buffer (1500m): Walking access to transit stops
#   - Extended buffer (7000m): 30-minute transit catchment
#   - Total: 8500m covers full accessibility range

pilot_bbox <- pilot_tract_sf %>%
  st_transform(5186) %>%  # Korea 2000 projection (meters)
  st_union() %>%          # Merge to single polygon
  st_buffer(TOTAL_BUFFER_METERS) %>%  # Apply total buffer (8.5 km)
  st_transform(4326) %>%  # WGS84 for GTFS/OSM
  st_bbox()

# Convert bbox to polygon for gtfstools
pilot_bbox_polygon <- pilot_bbox %>%
  st_as_sfc() %>%
  st_as_sf()

message("\n📍 Buffer Strategy:")
message("   Target: 30-minute transit journey")
message("   Basic buffer (walking):    ", BASIC_BUFFER_METERS, " m")
message("   Extended buffer (transit): ", EXTENDED_BUFFER_METERS, " m")
message("   Total buffer:              ", TOTAL_BUFFER_METERS, " m (", 
        round(TOTAL_BUFFER_METERS/1000, 1), " km)")
message("\n📐 Bounding Box Coordinates:")
message("   xmin (minlon): ", round(pilot_bbox["xmin"], 5))
message("   ymin (minlat): ", round(pilot_bbox["ymin"], 5))
message("   xmax (maxlon): ", round(pilot_bbox["xmax"], 5))
message("   ymax (maxlat): ", round(pilot_bbox["ymax"], 5))
message(strrep("=", 70), "\n")


# PART 2: Filter GTFS Data ----
message("\n", strrep("=", 70))
message("PART 2: GTFS Data Filtering")
message(strrep("=", 70))
tic("GTFS Processing")

## Load national GTFS ----
message("📦 Loading national GTFS data...")
gtfs_full <- read_gtfs(path(DATA_ROOT, "gtfs/prep", glue("gtfs_{PILOT_YEAR}.zip")))
message("   ✅ Loaded: ", length(gtfs_full), " tables")

## Spatial filtering ----
# Filter stops within bounding box, then extract related trips
message("\n🔍 Filtering by spatial extent...")
message("   Step 1: Filter stops in bbox")
message("   Step 2: Get trips passing through those stops")
message("   Step 3: Get complete stop_times for those trips")

# Step 1: Filter stops
stops_filtered <- gtfs_full$stops %>%
  filter(
    stop_lon >= pilot_bbox["xmin"] & 
    stop_lon <= pilot_bbox["xmax"] &
    stop_lat >= pilot_bbox["ymin"] & 
    stop_lat <= pilot_bbox["ymax"]
  )

message("   → Stops in bbox: ", nrow(stops_filtered))

# Step 2: Filter stop_times (trips passing through bbox)
stop_times_filtered <- gtfs_full$stop_times %>%
  semi_join(stops_filtered, by = "stop_id")

# Step 3: Filter related tables
trips_filtered <- gtfs_full$trips %>%
  semi_join(stop_times_filtered, by = "trip_id")

routes_filtered <- gtfs_full$routes %>%
  semi_join(trips_filtered, by = "route_id")

# Keep agency and calendar as-is
agency_filtered <- gtfs_full$agency
calendar_filtered <- gtfs_full$calendar

# Reconstruct filtered GTFS object
gtfs_filtered <- gtfs_full  # Copy structure
gtfs_filtered$stops <- stops_filtered
gtfs_filtered$stop_times <- stop_times_filtered
gtfs_filtered$trips <- trips_filtered
gtfs_filtered$routes <- routes_filtered
gtfs_filtered$agency <- agency_filtered
gtfs_filtered$calendar <- calendar_filtered

## Summary statistics ----
message("\n✅ GTFS Filtering Results:")
message("   Stops:      ", nrow(gtfs_filtered$stops), 
        " (", nrow(gtfs_full$stops), " → ", nrow(gtfs_filtered$stops), ")")
message("   Routes:     ", nrow(gtfs_filtered$routes))
message("   Trips:      ", nrow(gtfs_filtered$trips))
message("   Stop times: ", nrow(gtfs_filtered$stop_times))

## Save filtered GTFS ----
message("\n💾 Writing GTFS...")
write_gtfs(gtfs_filtered, gtfs_out_file)
gtfs_size_mb <- file.size(gtfs_out_file) / 1024^2

toc()
message("✅ GTFS saved: ", basename(gtfs_out_file))
message("   File size: ", round(gtfs_size_mb, 2), " MB\n")


# PART 3: Prepare OSM Data ----
message("\n", strrep("=", 70))
message("PART 3: OSM Data Extraction")
message(strrep("=", 70))
tic("OSM Processing")

## Use pilot region bbox for OSM ----
# Reuse the same bounding box used for GTFS filtering
# This ensures:
#   - Consistent spatial extent for GTFS and OSM
#   - Walking access covered (1.5km buffer)
#   - Transit network covered (7km buffer)

message("🔍 Using pilot region bbox for OSM extraction...")

osm_bbox <- pilot_bbox  # Same as GTFS bbox (8.5km buffer)

# Verify coverage
stops_in_osm_bbox <- gtfs_filtered$stops %>%
  filter(
    stop_lon >= osm_bbox["xmin"] & 
    stop_lon <= osm_bbox["xmax"] &
    stop_lat >= osm_bbox["ymin"] & 
    stop_lat <= osm_bbox["ymax"]
  )

message("   ✅ OSM bbox = GTFS bbox (", TOTAL_BUFFER_METERS/1000, " km total buffer)")
message("   ✅ Covers ", nrow(stops_in_osm_bbox), "/", nrow(gtfs_filtered$stops), " stops")
message("   📐 Coordinates: [", paste(round(osm_bbox, 5), collapse = ", "), "]")

# Prepare file paths
osm_raw_file     <- path(DATA_ROOT, "osm/prep",
                         glue("korea-{substr(PILOT_YEAR, 3, 4)}0101.osm.pbf"))
wsl_osm_raw_file <- to_wsl(osm_raw_file)
wsl_osm_out_file <- to_wsl(osm_out_file)

## Extract OSM using osmium ----
bbox_str <- paste(round(osm_bbox, 5), collapse = ",")

message("\n🔧 Running osmium extract...")
message("   Input:  ", basename(osm_raw_file))
message("   Output: ", basename(osm_out_file))
message("   BBox:   ", bbox_str)

cmd_extract <- glue(
  "wsl osmium extract -b {bbox_str} ",
  "'{wsl_osm_raw_file}' -o '{wsl_osm_out_file}' --overwrite"
)

result <- system(cmd_extract, intern = FALSE, wait = TRUE)

if (result != 0) {
  stop("❌ osmium execution failed (exit code: ", result, ")")
}

if (!file_exists(osm_out_file)) {
  stop("❌ OSM output file not created: ", osm_out_file)
}

## Cleanup ----
system("wsl --shutdown")

osm_size_mb <- file.size(osm_out_file) / 1024^2

toc()
message("✅ OSM saved: ", basename(osm_out_file))
message("   File size: ", round(osm_size_mb, 2), " MB\n")


# Summary ----
message("\n", strrep("=", 70))
message("✅ Pilot Data Preparation Complete!")
message(strrep("=", 70))
message("Output directory: ", pilot_data_dir)
message("  📦 GTFS: ", basename(gtfs_out_file), " (", round(gtfs_size_mb, 2), " MB)")
message("  🗺️  OSM:  ", basename(osm_out_file), " (", round(osm_size_mb, 2), " MB)")
message("\nFiltering method: Buffered spatial extent")
message("  - Bounding box: 8.5 km buffer around pilot region")
message("  - GTFS: Stops in bbox → trips passing through → complete stop_times")
message("  - OSM: Same bbox as GTFS for consistent coverage")
message(strrep("=", 70), "\n")


# PART 4: Visualization ----
message("\n", strrep("=", 70))
message("PART 4: Data Visualization")
message(strrep("=", 70))

## Prepare spatial objects ----
# Pilot region boundary
pilot_region_union <- pilot_tract_sf %>%
  st_transform(5186) %>%
  st_union() %>%
  st_transform(4326) %>%
  st_as_sf() %>%
  mutate(label = PILOT_REGION)

# Study area bbox (for visualization)
bbox_for_plot <- pilot_bbox %>%
  st_as_sfc() %>%
  st_as_sf() %>%
  mutate(label = glue("Study area\n({PILOT_REGION})"))

## GTFS Visualization ----
message("\n[GTFS Visualization]")

# Full national network (for context)
stops_full_sf <- gtfs_full$stops %>%
  st_as_sf(coords = c("stop_lon", "stop_lat"), crs = 4326)

# Filtered network
stops_filtered_sf <- gtfs_filtered$stops %>%
  st_as_sf(coords = c("stop_lon", "stop_lat"), crs = 4326)

# Create route linestrings for visualization
route_lines_sf <- gtfs_filtered$stop_times %>%
  left_join(gtfs_filtered$trips %>% select(trip_id, route_id), by = "trip_id") %>%
  left_join(gtfs_filtered$stops, by = "stop_id") %>%
  arrange(trip_id, stop_sequence) %>%
  group_by(trip_id) %>%
  filter(n() >= 2) %>%
  summarise(
    route_id = first(route_id),
    geometry = st_sfc(st_linestring(cbind(stop_lon, stop_lat)), crs = 4326),
    .groups = "drop"
  ) %>%
  st_as_sf()

# Sample one route per route_id (for clarity)
route_lines_sample <- route_lines_sf %>%
  group_by(route_id) %>%
  slice(1) %>%
  ungroup()

# Label anchor point
label_data <- data.frame(
  lon = pilot_bbox["xmax"],
  lat = pilot_bbox["ymax"],
  label = "Study area"
)

# Panel (a): National context
p_gtfs1 <- ggplot() +
  geom_sf(data = stops_full_sf |> 
    # sample 10% for visualization clarity
    sample_frac(size = 0.50, replace = FALSE), size = 0.01, alpha = 0.25, color = "gray50") +
  geom_sf(data = bbox_for_plot, fill = NA, color = "red", linewidth = 1) +
  ggrepel::geom_label_repel(
    data = label_data,
    aes(x = lon, y = lat, label = label),
    color = "red",
    fill = "white",
    family = "Source Sans Pro",
    fontface = "bold.italic",
    size = 3.5,
    lineheight = 0.85,
    label.size = 0.5,
    label.padding = unit(0.4, "lines"),
    xlim = c(pilot_bbox["xmax"], NA),
    ylim = c(pilot_bbox["ymax"], NA),
    box.padding = unit(1, "lines"),
    segment.color = "red",
    segment.size = 1,
    arrow = arrow(length = unit(0.03, "npc"), type = "closed")
  ) +
  labs(title = "(a) National coverage", x = NULL, y = NULL) +
  theme(
    plot.title = element_text(face = "italic", size = 14, hjust = 0),
    panel.grid.major = element_line(color = "gray90", linewidth = 0.3),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )

# Panel (b): Study area detail
p_gtfs2 <- ggplot() +
  geom_sf(data = bbox_for_plot, fill = NA, color = "red", 
          linewidth = 0.8, linetype = "dashed") +
  geom_sf(data = route_lines_sample, aes(color = route_id), 
          linewidth = 0.5, alpha = 0.4) +
  geom_sf(data = stops_filtered_sf, size = 0.8, alpha = 0.7, color = "#2E86AB") +
  geom_sf(data = pilot_region_union, fill = "gray95", color = "gray20", 
          linewidth = 1.5, alpha = 0.3) +
  labs(title = "(b) Study area", x = NULL, y = NULL) +
  theme(
    plot.title = element_text(face = "italic", size = 14, hjust = 0),
    legend.position = "none",
    panel.grid.major = element_line(color = "gray90", linewidth = 0.3),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.margin = margin(l = 15)
  )

# Combined figure
p_gtfs_combined <- p_gtfs1 + p_gtfs2

ggsave(
  filename = path(pilot_results_dir, "gtfs_visualization_pilot.png"),
  plot = p_gtfs_combined,
  width = 7, height = 3.5, dpi = 300, units = "in"
)

message("✅ GTFS visualization saved\n")

## OSM Visualization ----
message("[OSM Visualization]")

# Load national OSM roads (including residential roads)
message("  Loading national OSM roads...")

pacman::p_load(osmextract)

osm_full_road_sf <- oe_read(
  file_path = osm_raw_file,
  layer = "lines",
  extra_tags = c("highway"),
  query = "SELECT * FROM lines WHERE highway IN ('motorway', 'trunk', 'primary', 'secondary', 'tertiary', 'motorway_link', 'trunk_link', 'primary_link', 'secondary_link', 'tertiary_link', 'residential', 'unclassified')"
)


# Load pilot region OSM roads
message("  Loading pilot region OSM roads...")
osm_pilot_roads_sf <- st_read(dsn = osm_out_file, layer = "lines", quiet = TRUE)

# No sampling - use all roads for better visualization
osm_full_road_sf_sample <- osm_full_road_sf |> 
  sample_frac(size = 0.2, replace = FALSE)

# Panel (a): National context
p_osm1 <- ggplot() +
  geom_sf(data = osm_full_road_sf_sample, color = "gray30", size = 0.3, alpha = 0.5) +
  geom_sf(data = bbox_for_plot, fill = NA, color = "red", linewidth = 1) +
  ggrepel::geom_label_repel(
    data = label_data, 
    aes(x = lon, y = lat, label = label),
    color = "red",
    fill = "white",
    fontface = "bold.italic",
    size = 3.5,
    lineheight = 0.85,
    label.size = 0.5,
    label.padding = unit(0.4, "lines"),
    xlim = c(pilot_bbox["xmax"], NA),
    ylim = c(pilot_bbox["ymax"], NA),
    box.padding = unit(1, "lines"),
    segment.color = "red",
    segment.size = 1,
    family = "Source Sans Pro",
    arrow = arrow(length = unit(0.03, "npc"), type = "closed")
  ) +
  labs(title = "(a) National coverage", x = NULL, y = NULL) +
  theme(
    plot.title = element_text(face = "italic", size = 14, hjust = 0),
    panel.grid.major = element_line(color = "gray90", linewidth = 0.3),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )

# Panel (b): Study area detail (slightly zoomed out beyond red bbox)
expand_frac <- 0.10  # 10% margin around bbox (adjustable)

xmin <- as.numeric(pilot_bbox["xmin"])
xmax <- as.numeric(pilot_bbox["xmax"])
ymin <- as.numeric(pilot_bbox["ymin"])
ymax <- as.numeric(pilot_bbox["ymax"])

x_range <- xmax - xmin
y_range <- ymax - ymin

xlim_exp <- c(xmin - expand_frac * x_range, xmax + expand_frac * x_range)
ylim_exp <- c(ymin - expand_frac * y_range, ymax + expand_frac * y_range)

p_osm2 <- ggplot() +
  geom_sf(data = bbox_for_plot, fill = NA, color = "red",
          linewidth = 0.8, linetype = "dashed") +
  geom_sf(data = osm_pilot_roads_sf, color = "#39393972",
          linewidth = 0.20, alpha = 0.6) +
  geom_sf(data = pilot_region_union, fill = alpha("gray95", 0.5),
          color = "gray10", linewidth = 1.2) +
  coord_sf(xlim = xlim_exp, ylim = ylim_exp, expand = FALSE) +
  labs(title = "(b) Study area", x = NULL, y = NULL) +
  theme(
    plot.title = element_text(face = "italic", size = 14, hjust = 0),
    legend.position = "none",
    panel.grid.major = element_line(color = "gray90", linewidth = 0.3),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.margin = margin(l = 15)
  )

# Combined figure
p_osm_combined <- p_osm1 + p_osm2

ggsave(
  filename = path(pilot_results_dir, "osm_visualization_pilot.png"),
  plot = p_osm_combined,
  width = 7, height = 3.5, dpi = 300, units = "in"
)

message("✅ OSM visualization saved\n")

## Copy figures to docs ----
message("[Copying figures to docs]")

figure_files <- list(
  "gtfs_visualization_pilot.png" = "gtfs_visualization.png",
  "osm_visualization_pilot.png" = "osm_visualization.png"
)

for (src_name in names(figure_files)) {
  dst_name <- figure_files[[src_name]]
  src <- path(pilot_results_dir, src_name)
  dst <- path(docs_figures_dir, dst_name)
  if (file_exists(src)) {
    file_copy(src, dst, overwrite = TRUE)
    message("  ✅ ", src_name, " → docs/assets/figures/ch2/", dst_name)
  } else {
    message("  ⚠️ ", src_name, " not found")
  }
}

# Summary for documentation
message("\n", strrep("=", 70))
message("📊 SUMMARY FOR DOCS")
message(strrep("=", 70))
message("GTFS: ", scales::comma(nrow(gtfs_full$stops)), " stops → ", 
        scales::comma(nrow(gtfs_filtered$stops)), " stops")
message("OSM:  ", round(file.size(osm_raw_file)/1e6, 1), " MB → ",
        round(file.size(osm_out_file)/1e6, 1), " MB")
