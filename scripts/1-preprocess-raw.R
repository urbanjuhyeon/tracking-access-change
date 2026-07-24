# 1-preprocess-raw.R
# Preprocess raw GTFS and OSM data by year (2021-2024)

# Setup -------------------------------------------------------------------
pacman::p_load(fs, archive, tidyverse, data.table, glue, zip)

# Path configuration ------------------------------------------------------
PROJECT_ROOT <- normalizePath(
  Sys.getenv("ACCESS_DECLINE_ROOT", unset = "."),
  winslash = "/", mustWork = TRUE
)
if (!file.exists(file.path(PROJECT_ROOT, ".gitignore"))) {
  stop("Run from the repository root or set ACCESS_DECLINE_ROOT.")
}
DATA_ROOT    <- path(PROJECT_ROOT, "data")

gtfs_raw  <- path(DATA_ROOT, "gtfs/raw")
gtfs_work <- path(DATA_ROOT, "gtfs/work")
gtfs_prep <- path(DATA_ROOT, "gtfs/prep")

osm_raw  <- path(DATA_ROOT, "osm/raw")
osm_work <- path(DATA_ROOT, "osm/work")
osm_prep <- path(DATA_ROOT, "osm/prep")

# WSL paths for osmium (derives from Windows paths)
to_wsl <- function(win_path) {
  paste0("/mnt/", tolower(substr(win_path, 1, 1)), "/", substr(win_path, 4, nchar(win_path)))
}

dir_create(c(gtfs_work, gtfs_prep, osm_work, osm_prep))

# Year configuration ------------------------------------------------------
year_config <- tribble(
  ~year, ~gtfs_filename,                                     ~osm_filename,
  2021,  "2022-TM-PT-GTFS 대중교통GTFS(2021년 기준).zip",    "asia-210101.osm.pbf",
  2022,  "2023-TM-PT-GTFS 대중교통GTFS(2022년 기준).ZIP",    "asia-220101.osm.pbf",
  2023,  "2024-TM-PT-GTFS 대중교통GTFS(2023년 기준).zip",    "asia-230101.osm.pbf",
  2024,  "2025-TM-PT-GTFS 대중교통GTFS(2024년 기준).zip",    "asia-240101.osm.pbf"
)

# Filter to process only the latest year (modify as needed)
year_config <- year_config %>% filter(year == max(year))

# GTFS preprocessing function ---------------------------------------------
process_gtfs <- function(year, gtfs_filename) {
  message(glue("\n[GTFS {year}] {gtfs_filename}"))

  # Initialize work directory
  if (dir_exists(gtfs_work)) dir_delete(gtfs_work)
  dir_create(gtfs_work)

  # Extract ZIP (handles nested archives)
  archive_extract(path(gtfs_raw, gtfs_filename), dir = gtfs_work)
  inner_zips <- dir_ls(gtfs_work, glob = "*.zip")
  walk(inner_zips, ~archive_extract(.x, dir = gtfs_work))

  # Move files from subdirectory if needed
  agency_path <- dir_ls(gtfs_work, recurse = TRUE, glob = "*agency.txt")[1]
  actual_dir <- path_dir(agency_path)
  if (path_norm(actual_dir) != path_norm(gtfs_work)) {
    file_move(dir_ls(actual_dir, glob = "*.txt"), gtfs_work)
  }

  # (1) Agency: fix timezone
  agency_file <- path(gtfs_work, "agency.txt")
  fread(agency_file) |>
    mutate(agency_timezone = "Asia/Seoul") |>
    fwrite(agency_file, bom = TRUE)

  # (2) Calendar: set validity period to March of the year
  calendar_file <- path(gtfs_work, "calendar.txt")
  fread(calendar_file) |>
    mutate(
      start_date = as.integer(paste0(year, "0301")),
      end_date = as.integer(paste0(year, "0331"))
    ) |>
    fwrite(calendar_file, bom = TRUE)
  message(glue("  Calendar: {year}-03-01 ~ {year}-03-31"))

  # (3) Routes: standardize route_type (1=Subway, 2=Rail, 3=Bus)
  routes_file <- path(gtfs_work, "routes.txt")
  routes_tbl <- fread(routes_file) |>
    filter(route_type %in% c(0, 1, 2, 3, 4)) |>
    mutate(route_type = case_when(
      route_type %in% c(0, 2, 3) ~ 3L,  # Bus
      route_type == 1 ~ 1L,              # Subway
      route_type == 4 ~ 2L,              # Rail
      TRUE ~ 3L
    ))
  fwrite(routes_tbl, routes_file, bom = TRUE)
  message(glue("  Routes: {nrow(routes_tbl)} (Subway:{sum(routes_tbl$route_type==1)}, Rail:{sum(routes_tbl$route_type==2)}, Bus:{sum(routes_tbl$route_type==3)})"))

  # (4) Trips: keep only valid routes
  trips_file <- path(gtfs_work, "trips.txt")
  trips_tbl <- fread(trips_file) |>
    semi_join(routes_tbl, by = "route_id")
  fwrite(trips_tbl, trips_file, bom = TRUE)

  # (5) Stop_times: keep only valid trips + sanitize pickup/drop_off_type
  stop_times_file <- path(gtfs_work, "stop_times.txt")
  stop_times_tbl <- fread(stop_times_file) |>
    semi_join(trips_tbl, by = "trip_id") |>
    mutate(
      pickup_type = fifelse(pickup_type %in% c(0, 1, 2, 3), as.integer(pickup_type), 0L),
      drop_off_type = fifelse(drop_off_type %in% c(0, 1, 2, 3), as.integer(drop_off_type), 0L)
    )
  fwrite(stop_times_tbl, stop_times_file, bom = TRUE)
  message(glue("  Stop_times: {scales::comma(nrow(stop_times_tbl))}"))

  # (6) Stops: keep only used stops
  stops_file <- path(gtfs_work, "stops.txt")
  stops_tbl <- fread(stops_file) |>
    filter(stop_id %in% unique(stop_times_tbl$stop_id))
  fwrite(stops_tbl, stops_file, bom = TRUE)

  # Package as ZIP (exclude transfers.txt to prevent orphan references when spatial filtering)
  out_path <- path(gtfs_prep, glue("gtfs_{year}.zip"))
  if (file_exists(out_path)) file_delete(out_path)
  gtfs_files <- dir_ls(gtfs_work, glob = "*.txt")
  gtfs_files <- gtfs_files[!grepl("transfers", gtfs_files)]
  zip::zipr(out_path, gtfs_files, include_directories = FALSE)
  message(glue("  -> gtfs_{year}.zip"))
}

# OSM preprocessing function ----------------------------------------------
process_osm <- function(year, osm_filename) {
  message(glue("\n[OSM {year}] {osm_filename}"))

  target_name <- glue("korea-{substr(year, 3, 4)}0101.osm.pbf")
  korea_bbox <- "124.0,32.5,132.0,39.0"

  wsl_raw  <- to_wsl(path(osm_raw, osm_filename))
  wsl_work <- to_wsl(path(osm_work, target_name))
  wsl_prep <- to_wsl(path(osm_prep, target_name))

  # Clip to Korea extent
  system(glue("wsl osmium extract -b {korea_bbox} '{wsl_raw}' -o '{wsl_work}' --overwrite"),
         ignore.stdout = TRUE, ignore.stderr = TRUE)

  # Filter highway features only
  system(glue("wsl osmium tags-filter '{wsl_work}' w/highway -o '{wsl_prep}' --overwrite"),
         ignore.stdout = TRUE, ignore.stderr = TRUE)

  message(glue("  -> {target_name}"))
}

# Execute -----------------------------------------------------------------
message(glue("\n{strrep('=', 60)}"))
message("GTFS & OSM Preprocessing")
message(glue("{strrep('=', 60)}"))

pwalk(year_config, ~process_gtfs(..1, ..2))
pwalk(year_config, ~process_osm(..1, ..3))

message("\nDone!")
