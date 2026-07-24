# Setup ========================================================================

pacman::p_load(
  tidyverse, sf, arrow, fs, glue, scales,
  data.table, patchwork, ggtext, shadowtext, ggh4x, cowplot
)

PROJECT_ROOT <- normalizePath(
  Sys.getenv("ACCESS_DECLINE_ROOT", unset = "."),
  winslash = "/", mustWork = TRUE
)
if (!file.exists(file.path(PROJECT_ROOT, ".gitignore"))) {
  stop("Run from the repository root or set ACCESS_DECLINE_ROOT.")
}
DATA_ROOT    <- path(PROJECT_ROOT, "data")
RESULTS_DIR  <- path(PROJECT_ROOT, "workflows/results")
OUTPUT_DIR   <- path(PROJECT_ROOT, "workflows/analysis")
FIGURE_DIR   <- path(OUTPUT_DIR, "figures")

source(path(PROJECT_ROOT, "scripts/_bank_locations.R"))
assert_location_id_ttm_ready(RESULTS_DIR)

dir_create(OUTPUT_DIR)
dir_create(FIGURE_DIR)

TARGET_YEARS <- c(2021, 2022, 2023, 2024)
CACHE_ONLY <- "--cache-only" %in% commandArgs(trailingOnly = TRUE)

# Colors
GRAY_DARK  <- "#4B5563"
GRAY_MED   <- "#9CA3AF"
GRAY_LIGHT <- "#E5E7EB"

MODE_COLORS <- c(
  "car"     = "#c0392b",  # Brick red (primary)
  "transit" = "#2980b9",  # Steel blue (primary)
  "bicycle" = "#7f8c8d",  # Medium grey (secondary)
  "walk"    = "#bdc3c7"   # Light grey (secondary)
)

# Province mapping
SIDO_EN <- tribble(
  ~sido_nm, ~sido_en,

"서울특별시", "Seoul",
"부산광역시", "Busan",
  "대구광역시", "Daegu",
  "인천광역시", "Incheon",
  "광주광역시", "Gwangju",
  "대전광역시", "Daejeon",
  "울산광역시", "Ulsan",
  "세종특별자치시", "Sejong",
  "경기도", "Gyeonggi",
  "강원특별자치도", "Gangwon",
  "충청북도", "Chungbuk",
  "충청남도", "Chungnam",
  "전북특별자치도", "Jeonbuk",
  "전라남도", "Jeonnam",
  "경상북도", "Gyeongbuk",
  "경상남도", "Gyeongnam",
  "제주특별자치도", "Jeju"
)


message("\n[Setup] Loading data...")

sido_sf <- st_read(path(DATA_ROOT, "census/sido_bnd.gpkg"), quiet = TRUE)
sgg_sf  <- st_read(path(DATA_ROOT, "census/sgg_bnd.gpkg"), quiet = TRUE)

sido_lookup <- sido_sf |>
  st_drop_geometry() |>
  select(sido_cd = SIDO_CD, sido_nm = SIDO_NM)

sgg_lookup <- sgg_sf |>
  st_drop_geometry() |>
  select(sgg_cd = SIGUNGU_CD, sgg_nm = SIGUNGU_NM) |>
  mutate(sido_cd = str_sub(sgg_cd, 1, 2)) |>
  left_join(sido_lookup, by = "sido_cd")

POP_YEARS <- c(2021, 2022, 2023)

pop_total <- POP_YEARS |>
  set_names() |>
  map(\(yr) {
    fread(
      path(DATA_ROOT, "census/stats", glue("2024년기준_{yr}년_인구총괄(총인구).txt")),
      sep = "^", header = FALSE,
      col.names = c("year", "census_id", "stat_code", "value")
    )
  }) |>
  list_rbind() |>
  filter(stat_code == "to_in_001") |>
  transmute(year, census_id = as.character(census_id), population = suppressWarnings(as.numeric(value)))

# 2024 uses 2023 population
pop_total <- bind_rows(
  pop_total,
  pop_total |> filter(year == 2023) |> mutate(year = 2024)
)

elderly_codes <- paste0("in_age_", sprintf("%03d", 65:100))

pop_elderly <- POP_YEARS |>
  set_names() |>
  map(\(yr) {
    fread(
      path(DATA_ROOT, "census/stats", glue("2024년기준_{yr}년_성연령별인구.txt")),
      sep = "^", header = FALSE,
      col.names = c("year", "census_id", "stat_code", "value")
    )
  }) |>
  list_rbind() |>
  filter(stat_code %in% elderly_codes) |>
  mutate(census_id = as.character(census_id), value = suppressWarnings(as.numeric(value))) |>
  summarize(elderly = sum(value, na.rm = TRUE), .by = c(year, census_id))

pop_elderly <- bind_rows(
  pop_elderly,
  pop_elderly |> filter(year == 2023) |> mutate(year = 2024)
)

pop_all <- pop_total |>
  left_join(pop_elderly, by = c("year", "census_id")) |>
  mutate(
    elderly = replace_na(elderly, 0),
    elderly_share = elderly / population,
    sgg_cd = str_sub(census_id, 1, 5)
  ) |>
  filter(population > 0)

bank_raw <- read_canonical_bank_data(DATA_ROOT)

ttm_manifest <- tidyr::expand_grid(
  mode = c("car", "transit", "walk", "bicycle"),
  year = TARGET_YEARS
) |>
  mutate(file = path(RESULTS_DIR, glue("ttm_{mode}_{year}.parquet")))
ttm_files <- ttm_manifest$file
if (nrow(ttm_manifest) != 16L || any(!file_exists(ttm_files))) {
  stop("Expected exactly 16 actual TTM files for the location_id cache")
}

cache_file <- path(OUTPUT_DIR, "access_by_census_location_v1.parquet")
cache_manifest_file <- path(OUTPUT_DIR, "access_by_census_location_v1.manifest.csv")
scheme_marker <- path(RESULTS_DIR, "ID_SCHEME_LOCATION_V1")
cache_inputs <- c(ttm_files, scheme_marker)
analysis_code_sha256 <- sha256_file(
  path(PROJECT_ROOT, "scripts/5-2-main-analyze.R")
)
cache_manifest_is_current <- FALSE
if (file_exists(cache_manifest_file)) {
  cache_manifest <- read_csv(cache_manifest_file, show_col_types = FALSE)
  cache_manifest_is_current <- nrow(cache_manifest) == 1L &&
    identical(cache_manifest$id_scheme[[1]], "location_id_v1") &&
    identical(cache_manifest$analysis_code_sha256[[1]], analysis_code_sha256)
}
cache_is_fresh <- file_exists(cache_file) &&
  cache_manifest_is_current &&
  file_info(cache_file)$modification_time >=
    max(file_info(cache_inputs)$modification_time)

validate_access_cache <- function(cache, expected_n_rows) {
  required <- c(
    "census_id", "mode", "year",
    paste0("n_", sprintf("%02d", seq(5, 60, 5)), "min")
  )
  if (!all(required %in% names(cache)) || nrow(cache) != expected_n_rows) {
    stop("Unexpected location_id access cache schema or row count")
  }
  key_counts <- cache |>
    count(mode, year, name = "n")
  if (nrow(key_counts) != 16L || any(key_counts$n != 108388L) ||
      nrow(distinct(cache, census_id, mode, year)) != nrow(cache)) {
    stop("Location_id access cache has incomplete or duplicate keys")
  }
  invisible(TRUE)
}

if (cache_is_fresh) {
  access_all_modes <- read_parquet(cache_file)
  validate_access_cache(
    access_all_modes,
    as.integer(cache_manifest$n_rows[[1]])
  )
  message(glue("  Loaded cache: {comma(nrow(access_all_modes))} records"))
} else {
  message("  Cache missing or stale. Generating from 16 location_id TTM files...")

  # All routing uses same census IDs (2024 census boundaries)
  census_sf <- st_read(path(DATA_ROOT, "census/oa_bnd.gpkg"), quiet = TRUE)
  all_origin_ids <- census_sf$TOT_REG_CD

  message(glue("  Origin IDs for 0-fill: {comma(length(all_origin_ids))}"))

  access_all_modes <- ttm_files |>
    map(\(f) {
      meta <- str_match(path_file(f), "ttm_([a-z]+)_(\\d{4})")
      file_year <- as.integer(meta[3])
      if (!file_year %in% TARGET_YEARS) return(NULL)

      # Use Arrow for lazy evaluation - only collect at the end
      open_dataset(f) |>
        mutate(
          # Stable-ID TTMs have one enforced orientation: census -> bank.
          origin_id = as.character(from_id)
        ) |>
        # Step 1: Count destinations within each threshold, per time slot
        group_by(origin_id, departure_time) |>
        summarize(
          n_05min = sum(travel_time_p50 <= 5, na.rm = TRUE),
          n_10min = sum(travel_time_p50 <= 10, na.rm = TRUE),
          n_15min = sum(travel_time_p50 <= 15, na.rm = TRUE),
          n_20min = sum(travel_time_p50 <= 20, na.rm = TRUE),
          n_25min = sum(travel_time_p50 <= 25, na.rm = TRUE),
          n_30min = sum(travel_time_p50 <= 30, na.rm = TRUE),
          n_35min = sum(travel_time_p50 <= 35, na.rm = TRUE),
          n_40min = sum(travel_time_p50 <= 40, na.rm = TRUE),
          n_45min = sum(travel_time_p50 <= 45, na.rm = TRUE),
          n_50min = sum(travel_time_p50 <= 50, na.rm = TRUE),
          n_55min = sum(travel_time_p50 <= 55, na.rm = TRUE),
          n_60min = sum(travel_time_p50 <= 60, na.rm = TRUE),
          .groups = "drop"
        ) |>
        collect() |>
        # Step 2: Average across time slots (for Transit with multiple departures)
        summarize(
          across(starts_with("n_"), \(x) mean(x, na.rm = TRUE)),
          .by = origin_id
        ) |>
        rename(census_id = origin_id) |>
        right_join(tibble(census_id = all_origin_ids), by = "census_id") |>
        mutate(
          across(starts_with("n_"), \(x) replace_na(x, 0)),
          mode = meta[2],
          year = file_year
        )
    }, .progress = TRUE) |>
    list_rbind()

  validate_access_cache(access_all_modes, 1734208L)
  cache_temp <- path_ext_set(cache_file, "part.parquet")
  manifest_temp <- path_ext_set(cache_manifest_file, "part.csv")
  if (file_exists(cache_temp) || file_exists(manifest_temp)) {
    stop("Cache temp output already exists; refusing to overwrite")
  }
  write_parquet(access_all_modes, cache_temp)
  validate_access_cache(read_parquet(cache_temp), 1734208L)
  write_csv(
    tibble(
      id_scheme = "location_id_v1",
      analysis_code_sha256 = analysis_code_sha256,
      n_rows = nrow(access_all_modes)
    ),
    manifest_temp
  )
  if (file_exists(cache_file)) file_delete(cache_file)
  if (file_exists(cache_manifest_file)) file_delete(cache_manifest_file)
  file_move(cache_temp, cache_file)
  file_move(manifest_temp, cache_manifest_file)
  message(glue("  Saved: {comma(nrow(access_all_modes))} records"))
}

if (CACHE_ONLY) {
  message("  Cache-only validation complete")
  quit(save = "no", status = 0)
}

access_pop <- access_all_modes |>
  left_join(pop_all, by = c("year", "census_id")) |>
  filter(!is.na(population))

access_long <- access_pop |>
  pivot_longer(
    cols = starts_with("n_"),
    names_to = "threshold",
    names_prefix = "n_",
    values_to = "n_banks"
  )

sgg_summary <- access_long |>
  summarize(
    n_census = n(),
    total_pop = sum(population),
    mean_access = weighted.mean(n_banks, w = population),
    median_access = median(n_banks),
    pct_zero = mean(n_banks == 0) * 100,
    elderly_share = weighted.mean(elderly_share, w = population),
    .by = c(year, mode, threshold, sgg_cd)
  ) |>
  left_join(sgg_lookup, by = "sgg_cd")

national_summary <- access_long |>
  summarize(
    total_pop = sum(population),
    mean_access = weighted.mean(n_banks, w = population),
    pct_zero = mean(n_banks == 0) * 100,
    .by = c(year, mode, threshold)
  ) |>
  arrange(year, mode, threshold)

message(glue("  Done: {comma(nrow(access_pop))} census records"))


# Q1: Supply ===================================================================
# Bank branches are declining nationally

message("\n[Q1] Supply: Bank Decline")

file_delete(dir_ls(OUTPUT_DIR, glob = "*q1*", fail = FALSE))
file_delete(dir_ls(FIGURE_DIR, glob = "*q1*", fail = FALSE))

## National trend --------------------------------------------------------------

tbl_q1_national <- bank_raw |>
  filter(!is.na(longitude)) |>
  mutate(reference_date = as.character(reference_date)) |>
  distinct(reference_date, bank_name,
           longitude = round(longitude, 4),
           latitude = round(latitude, 4)) |>
  count(reference_date, name = "n_banks") |>
  arrange(reference_date) |>
  mutate(
    year = as.integer(str_sub(reference_date, 1, 4)),
    half = str_sub(reference_date, 5, 6),
    change = n_banks - lag(n_banks),
    pct_change = change / lag(n_banks) * 100,
    in_study = reference_date %in% c("2020h2", "2021h2", "2022h2", "2023h2")
  )

write_csv(tbl_q1_national, path(OUTPUT_DIR, "tbl_q1_national.csv"))

n_first <- first(tbl_q1_national$n_banks)
n_last  <- last(tbl_q1_national$n_banks)
total_closed <- n_first - n_last
total_pct <- (n_last / n_first - 1) * 100
years_spanned <- max(tbl_q1_national$year) - min(tbl_q1_national$year)

## Provincial trend ------------------------------------------------------------

tbl_q1_provincial <- bank_raw |>
  filter(!is.na(longitude)) |>
  mutate(reference_date = as.character(reference_date)) |>
  filter(reference_date %in% c("2019h1", "2025h1")) |>
  mutate(year = as.integer(str_sub(reference_date, 1, 4))) |>
  distinct(year, bank_name,
           longitude = round(longitude, 4),
           latitude = round(latitude, 4)) |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
  st_transform(st_crs(sgg_sf)) |>
  st_join(sgg_sf |> select(sgg_cd = SIGUNGU_CD)) |>
  st_drop_geometry() |>
  filter(!is.na(sgg_cd)) |>
  mutate(sido_cd = str_sub(sgg_cd, 1, 2)) |>
  count(year, sido_cd, name = "n_banks") |>
  left_join(sido_lookup, by = "sido_cd") |>
  left_join(SIDO_EN, by = "sido_nm") |>
  pivot_wider(names_from = year, values_from = n_banks, names_prefix = "y") |>
  mutate(
    change = y2025 - y2019,
    pct_change = change / y2019 * 100
  ) |>
  arrange(pct_change)

write_csv(tbl_q1_provincial, path(OUTPUT_DIR, "tbl_q1_provincial.csv"))

y_min <- 5000

plot_national <- tbl_q1_national |>
  mutate(
    x_label = case_when(
      half == "h1" & year == 2025 ~ "H1<br>2025",
      half == "h1" ~ "H1<br> ",
      half == "h2" & in_study ~ paste0("**H2**<br>**", year, "**"),
      TRUE ~ paste0("H2<br>", year)
    ),
    x_pos = row_number(),
    y_adjusted = n_banks - y_min,
    y_label_pos = y_adjusted - 80
  )

year_dividers <- seq(2.5, max(plot_national$x_pos), by = 2)
max_banks <- max(plot_national$n_banks)

fig_q1_national <- ggplot(plot_national, aes(x = x_pos)) +
  geom_vline(xintercept = year_dividers, color = "gray80", linewidth = 0.3, linetype = "dashed") +
  geom_col(aes(y = y_adjusted, fill = in_study), width = 0.7) +
  scale_fill_manual(values = c("FALSE" = GRAY_MED, "TRUE" = GRAY_DARK), guide = "none") +
  geom_text(data = \(d) filter(d, !in_study),
            aes(y = y_label_pos, label = comma(n_banks)),
            color = "white", size = 2.5, vjust = 0.15) +
  geom_text(data = \(d) filter(d, in_study),
            aes(y = y_label_pos, label = comma(n_banks)),
            color = "white", size = 3, fontface = "bold", vjust = 0.15) +
  scale_x_continuous(
    breaks = plot_national$x_pos,
    labels = plot_national$x_label,
    expand = expansion(add = 0.5)
  ) +
  scale_y_continuous(
    limits = c(0, (max_banks - y_min) * 1.12),
    breaks = seq(0, 2000, 500),
    labels = \(x) comma(x + y_min),
    expand = expansion(mult = c(0, 0))
  ) +
  labs(
    title = "Bank Branches in South Korea (2019-2025)",
    subtitle = glue("{comma(total_closed)} branches closed over {years_spanned} years ({round(abs(total_pct), 1)}% decline)"),
    x = NULL, y = NULL,
    caption = "Dark bars indicate study time points. Y-axis starts at 5,000."
  ) +
  theme_bw(base_size = 11) +
  theme(
    axis.text.x = element_markdown(size = 9, lineheight = 1.2, halign = 0.5),
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "gray40", size = 11, margin = margin(b = 8)),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.caption = element_text(hjust = 0, color = "gray50", size = 9)
  )

ggsave(path(FIGURE_DIR, "fig_q1_national.png"), fig_q1_national,
       width = 7.5, height = 4, dpi = 300, bg = "white")

plot_provincial <- tbl_q1_provincial |>
  mutate(
    sido_en = fct_reorder(sido_en, -pct_change),
    decline_pct = abs(pct_change),
    rank = row_number(pct_change)
  )

max_decline <- slice_max(plot_provincial, decline_pct, n = 1)
min_decline <- slice_min(plot_provincial, decline_pct, n = 1)

fig_q1_provincial <- ggplot(plot_provincial, aes(x = decline_pct, y = sido_en)) +
  geom_col(aes(fill = rank), width = 0.7, show.legend = FALSE) +
  scale_fill_gradient(low = GRAY_DARK, high = GRAY_MED) +
  geom_text(aes(label = sprintf("-%.1f%%", decline_pct)),
            hjust = 1.1, size = 3, color = "white", fontface = "bold") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.02))) +
  labs(
    title = "Bank Branch Change by Province (2019-2025)",
    subtitle = glue("{round(max_decline$decline_pct, 1)}% in {max_decline$sido_en} to {round(min_decline$decline_pct, 1)}% in {min_decline$sido_en}"),
    x = NULL, y = NULL
  ) +
  theme_bw(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "gray40", size = 11, margin = margin(b = 8)),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )

ggsave(path(FIGURE_DIR, "fig_q1_provincial.png"), fig_q1_provincial,
       width = 7.5, height = 6, dpi = 300, bg = "white")

message(glue("  {comma(total_closed)} branches closed ({round(abs(total_pct), 1)}%)"))


# Q2: Baseline =================================================================
# 4-mode accessibility in 2021 → derive comparable thresholds

message("\n[Q2] Baseline: 2021 Accessibility")

file_delete(dir_ls(OUTPUT_DIR, glob = "*q2*", fail = FALSE))
file_delete(dir_ls(FIGURE_DIR, glob = "*q2*", fail = FALSE))

tbl_q2_national <- access_long |>
  filter(year == 2021) |>
  group_by(mode, threshold) |>
  summarise(
    n_tracts = n(),
    total_pop_M = sum(population) / 1e6,
    mean_banks = weighted.mean(n_banks, w = population),
    pct_coverage = sum(population[n_banks >= 1]) / sum(population) * 100,
    .groups = "drop"
  ) |>
  mutate(threshold_min = as.integer(str_extract(threshold, "\\d+"))) |>
  arrange(mode, threshold_min)

write_csv(tbl_q2_national, path(OUTPUT_DIR, "tbl_q2_national.csv"))

# Key thresholds
car_10_cov <- tbl_q2_national |> filter(mode == "car", threshold == "10min") |> pull(pct_coverage)
transit_30_cov <- tbl_q2_national |> filter(mode == "transit", threshold == "30min") |> pull(pct_coverage)
car_10_banks <- tbl_q2_national |> filter(mode == "car", threshold == "10min") |> pull(mean_banks)
transit_30_banks <- tbl_q2_national |> filter(mode == "transit", threshold == "30min") |> pull(mean_banks)

walk_60_cov <- tbl_q2_national |> filter(mode == "walk", threshold == "60min") |> pull(pct_coverage)
bike_60_cov <- tbl_q2_national |> filter(mode == "bicycle", threshold == "60min") |> pull(pct_coverage)

## Coverage curve --------------------------------------------------------------

fig_q2_data <- tbl_q2_national |>
  mutate(
    mode_label = factor(
      case_match(mode, "car" ~ "Car", "transit" ~ "Transit", "walk" ~ "Walk", "bicycle" ~ "Bicycle"),
      levels = c("Car", "Transit", "Bicycle", "Walk")
    )
  )

fig_q2_coverage <- ggplot(fig_q2_data, aes(x = threshold_min, y = pct_coverage,
                                            color = mode_label, group = mode_label)) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 90, ymax = 95,
           fill = "gray70", alpha = 0.35) +
  geom_hline(yintercept = c(90, 95), linetype = "dashed", color = "gray50", linewidth = 0.5) +
  annotate("text", x = 22.5, y = 92.5, label = "90-95%", size = 3, color = "gray40", fontface = "italic") +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.5) +
  # Mark thresholds
  geom_segment(x = 10, xend = 10, y = 0, yend = car_10_cov,
               linetype = "dotted", color = MODE_COLORS["car"], linewidth = 0.7) +
  geom_segment(x = 30, xend = 30, y = 0, yend = transit_30_cov,
               linetype = "dotted", color = MODE_COLORS["transit"], linewidth = 0.7) +
  geom_point(data = tibble(
    mode_label = factor(c("Car", "Transit"), levels = c("Car", "Transit", "Bicycle", "Walk")),
    threshold_min = c(10, 30),
    pct_coverage = c(car_10_cov, transit_30_cov)
  ), size = 4, shape = 21, fill = "white", stroke = 1.5) +
  geom_shadowtext(aes(x = 10, y = car_10_cov - 7.5,
                      label = glue("10min\n({round(car_10_cov, 0)}%)")),
                  color = MODE_COLORS["car"], bg.color = "white", bg.r = 0.15,
                  size = 3.4, fontface = "bold", lineheight = 0.85,
                  data = data.frame(x = 1), inherit.aes = FALSE) +
  geom_shadowtext(aes(x = 30, y = transit_30_cov - 7.5,
                      label = glue("30min\n({round(transit_30_cov, 0)}%)")),
                  color = MODE_COLORS["transit"], bg.color = "white", bg.r = 0.15,
                  size = 3.4, fontface = "bold", lineheight = 0.85,
                  data = data.frame(x = 1), inherit.aes = FALSE) +
  scale_x_continuous(breaks = seq(5, 60, 5), labels = \(x) paste0(x, "min")) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 10), labels = \(x) paste0(x, "%")) +
  scale_color_manual(values = c(
    "Car" = unname(MODE_COLORS["car"]),
    "Transit" = unname(MODE_COLORS["transit"]),
    "Walk" = unname(MODE_COLORS["walk"]),
    "Bicycle" = unname(MODE_COLORS["bicycle"])
  )) +
  labs(
    title = "Travel Time to Banks (2021)",
    subtitle = "Share of population with access to at least one bank",
    x = NULL, y = NULL, color = NULL,
    caption = "Shaded band shows 90-95% coverage. Circled points are thresholds used in subsequent analysis."
  ) +
  theme_bw(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "gray40", size = 11, margin = margin(b = 3)),
    panel.grid.minor = element_blank(),
    legend.position = "top",
    legend.justification = "left",
    legend.margin = margin(t = 0, b = 0),
    legend.box.margin = margin(t = -1.5, b = -7.5),
    plot.caption = element_text(hjust = 0, color = "gray50", size = 9)
  ) +
  guides(color = guide_legend(override.aes = list(size = 1.2, shape = NA)))

ggsave(path(FIGURE_DIR, "fig_q2_coverage.png"), fig_q2_coverage,
       width = 7, height = 5, dpi = 300, bg = "white")

## Choice distribution ---------------------------------------------------------

fig_q2_choice_data <- access_long |>
  filter(year == 2021) |>
  filter(
    (mode == "car" & threshold == "10min") |
    (mode == "transit" & threshold == "30min")
  ) |>
  mutate(mode_label = if_else(mode == "car", "Car (10min)", "Transit (30min)"))

choice_stats <- fig_q2_choice_data |>
  group_by(mode_label) |>
  summarise(
    mean_banks = weighted.mean(n_banks, w = population),
    median_banks = median(n_banks),
    .groups = "drop"
  )

median_car <- choice_stats |> filter(mode_label == "Car (10min)") |> pull(median_banks)
median_transit <- choice_stats |> filter(mode_label == "Transit (30min)") |> pull(median_banks)

fig_q2_choice <- ggplot(fig_q2_choice_data, aes(x = n_banks, weight = population)) +
  geom_density(aes(fill = mode_label), alpha = 0.4, color = NA) +
  geom_density(aes(color = mode_label), fill = NA, linewidth = 0.8) +
  geom_vline(xintercept = median_car, linetype = "dashed", color = "#8B0000", linewidth = 0.7) +
  geom_vline(xintercept = median_transit, linetype = "dashed", color = "#00008B", linewidth = 0.7) +
  scale_x_continuous(
    limits = c(0, 400),
    breaks = sort(c(0, round(median_transit), round(median_car), 100, 200, 300, 400)),
    labels = \(x) case_when(
      x == round(median_car) ~ glue("<b style='color:#8B0000;font-size:12pt'>{x}</b>"),
      x == round(median_transit) ~ glue("<b style='color:#00008B;font-size:12pt'>{x}</b>"),
      TRUE ~ as.character(x)
    )
  ) +
  scale_fill_manual(values = c(
    "Car (10min)" = unname(MODE_COLORS["car"]),
    "Transit (30min)" = unname(MODE_COLORS["transit"])
  )) +
  scale_color_manual(values = c("Car (10min)" = "#8B0000", "Transit (30min)" = "#00008B")) +
  labs(
    title = "Number of Banks Reachable by Mode (2021)",
    subtitle = glue("Median: {round(median_car)} by car (10min), {round(median_transit)} by transit (30min)"),
    x = "Number of banks", y = NULL,
    fill = NULL, color = NULL,
    caption = "Thresholds yield ~90% population coverage. Weighted by census tract population. X-axis truncated at 400."
  ) +
  theme_bw(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "gray40", size = 11, margin = margin(b = 3)),
    panel.grid.minor = element_blank(),
    legend.position = "top",
    legend.justification = "left",
    legend.margin = margin(t = 0, b = 0),
    legend.box.margin = margin(t = 2, b = -5),
    # legend symbol 크기
    legend.key.size = unit(1, "lines"),
    axis.text.x = element_markdown(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    plot.caption = element_text(hjust = 0, color = "gray50", size = 9)
  ) +
  guides(fill = guide_legend(override.aes = list(alpha = 0.6)), color = "none")

ggsave(path(FIGURE_DIR, "fig_q2_choice.png"), fig_q2_choice,
       width = 7, height = 4.5, dpi = 300, bg = "white")


message(glue("  Car 10min: {round(car_10_cov, 1)}% | Transit 30min: {round(transit_30_cov, 1)}%"))


# Q3: Change ===================================================================
# How did accessibility change over time?

message("\n[Q3] Change: Temporal Trends")

file_delete(dir_ls(OUTPUT_DIR, glob = "*q3*", fail = FALSE))
file_delete(dir_ls(FIGURE_DIR, glob = "*q3*", fail = FALSE))

tbl_q3_trend <- access_long |>
  filter(
    (mode == "car" & threshold == "10min") |
    (mode == "transit" & threshold == "30min")
  ) |>
  summarize(
    total_pop_M = sum(population) / 1e6,
    median_banks = median(n_banks),
    pct_coverage = sum(population[n_banks >= 1]) / sum(population) * 100,
    .by = c(year, mode, threshold)
  ) |>
  arrange(year, mode)

write_csv(tbl_q3_trend, path(OUTPUT_DIR, "tbl_q3_trend.csv"))

## Trend figure ------------------------------------------------------

# Calculate changes for subtitle
car_banks_2021 <- tbl_q3_trend |> filter(mode == "car", year == 2021) |> pull(median_banks)
car_banks_2024 <- tbl_q3_trend |> filter(mode == "car", year == 2024) |> pull(median_banks)
transit_banks_2021 <- tbl_q3_trend |> filter(mode == "transit", year == 2021) |> pull(median_banks)
transit_banks_2024 <- tbl_q3_trend |> filter(mode == "transit", year == 2024) |> pull(median_banks)

car_banks_change <- round((car_banks_2024 / car_banks_2021 - 1) * 100)
transit_banks_change <- round((transit_banks_2024 / transit_banks_2021 - 1) * 100)

# Long format for faceting
fig_q3_data <- tbl_q3_trend |>
  mutate(
    mode_label = if_else(mode == "car", "Car (10min)", "Transit (30min)"),
    mode_label = factor(mode_label, levels = c("Car (10min)", "Transit (30min)"))
  ) |>
  pivot_longer(
    cols = c(pct_coverage, median_banks),
    names_to = "metric",
    values_to = "value"
  ) |>
  mutate(
    metric = factor(
      case_match(metric,
        "pct_coverage" ~ "(a) Population with Access",
        "median_banks" ~ "(b) Median Banks Reachable"
      ),
      levels = c("(a) Population with Access", "(b) Median Banks Reachable")
    ),
    label = if_else(
      metric == "(a) Population with Access",
      paste0(round(value, 1), "%"),
      as.character(round(value))
    )
  )

# Single faceted plot with ggh4x for different Y-axis scales
fig_q3_trend <- ggplot(fig_q3_data, aes(x = year, y = value, color = mode_label)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  geom_label(
    aes(label = label),
    size = 3.5, fontface = "bold", show.legend = FALSE,
    label.size = 0.3, label.padding = unit(0.15, "lines"), fill = "white"
  ) +
  facet_wrap(~metric, scales = "free_y") +
  facetted_pos_scales(
    y = list(
      `(a) Population with Access` = scale_y_continuous(
        limits = c(86, 94), breaks = seq(86, 94, 2), labels = \(x) paste0(x, "%")
      ),
      `(b) Median Banks Reachable` = scale_y_continuous(
        limits = c(0, 70), breaks = seq(0, 60, 20)
      )
    )
  ) +
  scale_x_continuous(breaks = TARGET_YEARS, expand = expansion(mult = 0.15)) +
  scale_color_manual(values = c(
    "Car (10min)" = unname(MODE_COLORS["car"]),
    "Transit (30min)" = unname(MODE_COLORS["transit"])
  )) +
  labs(
    title = "Changes in Bank Access (2021–2024)",
    subtitle = glue("Population with access stable; median banks down {abs(car_banks_change)}% (car), {abs(transit_banks_change)}% (transit)"),
    x = NULL, y = NULL, color = NULL
  ) +
  theme_bw(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "gray40", size = 11, margin = margin(b = 8)),
    panel.grid.minor = element_blank(),
    panel.spacing = unit(1.2, "lines"),
    strip.text = element_text(face = "bold", size = 11),
    legend.position = "top",
    legend.justification = "left",
    legend.margin = margin(t = 0, b = 0),
    legend.box.margin = margin(t = -2.5, b = -5)
  ) +
  guides(color = guide_legend(override.aes = list(shape = NA, linewidth = 1.2)))

ggsave(path(FIGURE_DIR, "fig_q3_trend.png"), fig_q3_trend,
       width = 7, height = 4, dpi = 300, bg = "white")

message("  Trend analysis complete")


# Q4: Spatial Change ============================================================
# SGG and Sido level datasets with geometry

message("\n[Q4] Spatial Change")

file_delete(dir_ls(OUTPUT_DIR, glob = "*q4*", fail = FALSE))
file_delete(dir_ls(FIGURE_DIR, glob = "*q4*", fail = FALSE))

## SGG bank counts --------------------------------------------------------------

sgg_bank_counts <- bank_raw |>
  filter(!is.na(longitude)) |>
  mutate(reference_date = as.character(reference_date)) |>
  filter(reference_date %in% c("2021h2", "2022h2", "2023h2")) |>
  mutate(year = as.integer(str_sub(reference_date, 1, 4))) |>
  distinct(year, bank_name,
           longitude = round(longitude, 4),
           latitude = round(latitude, 4)) |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
  st_transform(st_crs(sgg_sf)) |>
  st_join(sgg_sf |> select(sgg_cd = SIGUNGU_CD)) |>
  st_drop_geometry() |>
  filter(!is.na(sgg_cd)) |>
  count(year, sgg_cd, name = "n_banks") |>
  pivot_wider(names_from = year, values_from = n_banks, names_prefix = "banks_") |>
  mutate(across(starts_with("banks_"), \(x) replace_na(x, 0)))

## SGG dataset -----------------------------------------------------------------

tbl_q4_sgg <- sgg_summary |>
  filter(
    year %in% c(2021, 2022, 2023),
    (mode == "car" & threshold == "10min") |
    (mode == "transit" & threshold == "30min")
  ) |>
  select(sgg_cd, sgg_nm, sido_nm, year, mode, mean_access, total_pop) |>
  pivot_wider(
    names_from = year,
    values_from = c(mean_access, total_pop),
    names_sep = "_"
  ) |>
  mutate(
    access_change = mean_access_2023 - mean_access_2021,
    access_pct_change = if_else(
      mean_access_2021 > 0,
      access_change / mean_access_2021 * 100,
      NA_real_
    )
  ) |>
  left_join(sgg_bank_counts, by = "sgg_cd") |>
  mutate(
    bank_change = banks_2023 - banks_2021,
    bank_pct_change = if_else(
      banks_2021 > 0,
      bank_change / banks_2021 * 100,
      NA_real_
    )
  ) |>
  left_join(SIDO_EN, by = "sido_nm") |>
  left_join(
    sgg_sf |>
      mutate(sgg_cd = SIGUNGU_CD) |>
      select(sgg_cd, geom),
    by = "sgg_cd"
  ) |>
  st_as_sf() |>
  select(
    sgg_cd, sgg_nm, sido_nm, sido_en, mode,
    mean_access_2021, mean_access_2022, mean_access_2023,
    access_change, access_pct_change,
    total_pop_2021, total_pop_2022, total_pop_2023,
    banks_2021, banks_2022, banks_2023,
    bank_change, bank_pct_change,
    geom
  )

# CSV (without geometry) + GPKG (with geometry)
tbl_q4_sgg |>
  st_drop_geometry() |>
  write_csv(path(OUTPUT_DIR, "tbl_q4_sgg.csv"))

st_write(tbl_q4_sgg, path(OUTPUT_DIR, "tbl_q4_sgg.gpkg"), delete_dsn = TRUE, quiet = TRUE)

## Sido dataset ----------------------------------------------------------------

# Sido geometry (dissolve from SGG)
sido_geom <- sgg_sf |>
  mutate(sido_cd = str_sub(SIGUNGU_CD, 1, 2)) |>
  group_by(sido_cd) |>
  summarize(geom = st_union(geom), .groups = "drop")

tbl_q4_sido <- tbl_q4_sgg |>
  st_drop_geometry() |>
  mutate(sido_cd = str_sub(sgg_cd, 1, 2)) |>
  summarize(
    mean_access_2021 = weighted.mean(mean_access_2021, total_pop_2021, na.rm = TRUE),
    mean_access_2022 = weighted.mean(mean_access_2022, total_pop_2022, na.rm = TRUE),
    mean_access_2023 = weighted.mean(mean_access_2023, total_pop_2023, na.rm = TRUE),
    total_pop_2021 = sum(total_pop_2021, na.rm = TRUE),
    total_pop_2022 = sum(total_pop_2022, na.rm = TRUE),
    total_pop_2023 = sum(total_pop_2023, na.rm = TRUE),
    banks_2021 = sum(banks_2021, na.rm = TRUE),
    banks_2022 = sum(banks_2022, na.rm = TRUE),
    banks_2023 = sum(banks_2023, na.rm = TRUE),
    .by = c(sido_cd, sido_nm, sido_en, mode)
  ) |>
  mutate(
    access_change = mean_access_2023 - mean_access_2021,
    access_pct_change = access_change / mean_access_2021 * 100,
    bank_change = banks_2023 - banks_2021,
    bank_pct_change = bank_change / banks_2021 * 100
  ) |>
  left_join(sido_geom, by = "sido_cd") |>
  st_as_sf() |>
  select(
    sido_cd, sido_nm, sido_en, mode,
    mean_access_2021, mean_access_2022, mean_access_2023,
    access_change, access_pct_change,
    total_pop_2021, total_pop_2022, total_pop_2023,
    banks_2021, banks_2022, banks_2023,
    bank_change, bank_pct_change,
    geom
  ) |>
  arrange(sido_cd, mode)

# CSV (without geometry) + GPKG (with geometry)
tbl_q4_sido |>
  st_drop_geometry() |>
  write_csv(path(OUTPUT_DIR, "tbl_q4_sido.csv"))

st_write(tbl_q4_sido, path(OUTPUT_DIR, "tbl_q4_sido.gpkg"), delete_dsn = TRUE, quiet = TRUE)

## Three-panel map: Bank Closures vs Accessibility Change -----------------------
# (a) Bank Closures: where banks physically closed
# (b) Accessibility Δ: absolute change in reachable banks (transit)
# (c) Accessibility Δ%: relative change in reachable banks (transit)
#
# Key insights:
# - (a) shows supply-side change (distributed across country)
# - (b) concentrates in metros (floor effect: high baseline = large absolute decline)
# - (c) shows peripheral areas with low baseline suffering large relative losses
# - Only 4 of 252 SGGs appear in all three panels

# Sido boundaries for overlay
sido_border <- sido_geom |> st_transform(st_crs(tbl_q4_sgg))

# Bounding box for mainland (exclude Jeju and remote islands for cleaner aspect ratio)
MAINLAND_XLIM <- c(125.6, 129.8)
MAINLAND_YLIM <- c(33.8, 38.7)

# Prepare transit data with all three metrics
map_data_q4 <- tbl_q4_sgg |>
  filter(mode == "transit") |>
  st_drop_geometry() |>
  select(sgg_cd, bank_change, access_change, access_pct_change) |>
  left_join(
    sgg_sf |> mutate(sgg_cd = SIGUNGU_CD) |> select(sgg_cd, geom),
    by = "sgg_cd"
  ) |>
  st_as_sf()

# Calculate top 20% thresholds (most decline = lowest values)
q20_bank <- quantile(map_data_q4$bank_change, 0.20, na.rm = TRUE)
q20_abs <- quantile(map_data_q4$access_change, 0.20, na.rm = TRUE)
q20_pct <- quantile(map_data_q4$access_pct_change, 0.20, na.rm = TRUE)

message(glue("  Thresholds: bank={q20_bank}, abs={round(q20_abs,1)}, pct={round(q20_pct,1)}%"))

# Add top 20% flags
map_data_q4 <- map_data_q4 |>
  mutate(
    top20_bank = bank_change <= q20_bank,
    top20_abs = access_change <= q20_abs,
    top20_pct = access_pct_change <= q20_pct
  )

# Transform to WGS84
map_data_q4_wgs84 <- st_transform(map_data_q4, 4326)
sido_wgs84 <- st_transform(sido_border, 4326)

# Colors: red for top 20%, light gray for rest
highlight_colors <- c("TRUE" = "#b2182b", "FALSE" = "#f5f5f5")

# Helper for 3-panel maps
create_q4c_panel <- function(data, sido_data, fill_var, panel_label) {
  ggplot() +
    geom_sf(data = data, aes(fill = .data[[fill_var]]),
            color = "gray70", linewidth = 0.08) +
    geom_sf(data = sido_data, fill = NA, color = "gray30", linewidth = 0.3) +
    coord_sf(xlim = MAINLAND_XLIM, ylim = MAINLAND_YLIM, expand = FALSE) +
    scale_fill_manual(values = highlight_colors, guide = "none", na.value = "gray90") +
    annotate("text", x = 125.7, y = 38.5, label = panel_label,
             fontface = "bold.italic", size = 4.5, hjust = 0) +
    theme_void(base_size = 9) +
    theme(legend.position = "none")
}

# Overlap analysis (calculate before plot for subtitle)
n_total <- nrow(map_data_q4)
n_all_three <- sum(map_data_q4$top20_bank & map_data_q4$top20_abs & map_data_q4$top20_pct, na.rm = TRUE)
n_a_only <- sum(map_data_q4$top20_bank & !map_data_q4$top20_abs & !map_data_q4$top20_pct, na.rm = TRUE)
n_b_only <- sum(!map_data_q4$top20_bank & map_data_q4$top20_abs & !map_data_q4$top20_pct, na.rm = TRUE)
n_c_only <- sum(!map_data_q4$top20_bank & !map_data_q4$top20_abs & map_data_q4$top20_pct, na.rm = TRUE)

message(glue("  Overlap: all_three={n_all_three}, (a)_only={n_a_only}, (b)_only={n_b_only}, (c)_only={n_c_only}"))

# Create 3 panels
fig_q4_bank <- create_q4c_panel(map_data_q4_wgs84, sido_wgs84, "top20_bank", "(a)")
fig_q4_abs <- create_q4c_panel(map_data_q4_wgs84, sido_wgs84, "top20_abs", "(b)")
fig_q4_pct <- create_q4c_panel(map_data_q4_wgs84, sido_wgs84, "top20_pct", "(c)")

# Combine 3 panels (with spacing between panels)
fig_q4_threepanel <- (fig_q4_bank | fig_q4_abs | fig_q4_pct) +
  plot_annotation(
    title = "Spatial Patterns of Bank Access Decline (2021-2023)",
    subtitle = "(a) branch closures, (b) transit access (Δ), (c) transit access (Δ%); bottom 20% in red",
    theme = theme(
      plot.title = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(color = "gray40", size = 11, margin = margin(b = 8))
    )
  ) &
  theme(plot.margin = margin(l = 9, r = 9))

ggsave(path(FIGURE_DIR, "fig_q4_threepanel.png"), fig_q4_threepanel,
       width = 7.5, height = 4, dpi = 300, bg = "white")

message("  Three-panel map complete")


# Q5: Modal Divergence =========================================================
# Did car and transit show the same spatial patterns of decline?
# Key question: Do the worst-hit areas differ by mode?

message("\n[Q5] Modal Divergence Analysis")

file_delete(dir_ls(OUTPUT_DIR, glob = "*q5*", fail = FALSE))
file_delete(dir_ls(FIGURE_DIR, glob = "*q5*", fail = FALSE))

## Prepare modal comparison data -----------------------------------------------

# Pivot tbl_q4_sgg to wide format (car and transit side by side)
modal_wide <- tbl_q4_sgg |>
  st_drop_geometry() |>
  select(sgg_cd, sgg_nm, sido_nm, sido_en, mode,
         mean_access_2021, mean_access_2023,
         access_change, access_pct_change,
         total_pop_2021, bank_change) |>
  pivot_wider(
    names_from = mode,
    values_from = c(mean_access_2021, mean_access_2023, access_change, access_pct_change),
    names_sep = "_"
  ) |>
  filter(!is.na(access_pct_change_car) & !is.na(access_pct_change_transit))

n_districts <- nrow(modal_wide)
n_bottom <- round(n_districts * 0.2)  # Bottom 20%

message(glue("  {n_districts} districts with both car and transit data"))
message(glue("  Bottom quintile = {n_bottom} districts"))

## Identify bottom 20% for each mode -------------------------------------------

# Get bottom 20% by car
car_bottom_ids <- modal_wide |>
  slice_min(access_pct_change_car, n = n_bottom) |>
  pull(sgg_cd)

# Get bottom 20% by transit
transit_bottom_ids <- modal_wide |>
  slice_min(access_pct_change_transit, n = n_bottom) |>
  pull(sgg_cd)

# Calculate overlap
overlap_ids <- intersect(car_bottom_ids, transit_bottom_ids)
only_car_ids <- setdiff(car_bottom_ids, transit_bottom_ids)
only_transit_ids <- setdiff(transit_bottom_ids, car_bottom_ids)

n_overlap <- length(overlap_ids)
n_only_car <- length(only_car_ids)
n_only_transit <- length(only_transit_ids)
jaccard <- n_overlap / length(union(car_bottom_ids, transit_bottom_ids))

message(glue("  Overlap: {n_overlap} districts ({round(jaccard * 100, 1)}% Jaccard)"))
message(glue("  Only in car bottom: {n_only_car}"))
message(glue("  Only in transit bottom: {n_only_transit}"))

## Add divergence categories ---------------------------------------------------

modal_divergence <- modal_wide |>
  mutate(
    in_car_bottom = sgg_cd %in% car_bottom_ids,
    in_transit_bottom = sgg_cd %in% transit_bottom_ids,
    divergence_category = case_when(
      in_car_bottom & in_transit_bottom ~ "Both bottom 20%",
      in_car_bottom & !in_transit_bottom ~ "Car only",
      !in_car_bottom & in_transit_bottom ~ "Transit only",
      TRUE ~ "Neither"
    )
  )

# Summary table
divergence_summary <- modal_divergence |>
  count(divergence_category, name = "n_districts") |>
  mutate(pct = round(n_districts / sum(n_districts) * 100, 1)) |>
  arrange(desc(n_districts))

message("\n  Divergence categories:")
print(divergence_summary)

## Examples of divergent districts ---------------------------------------------

# Districts where car was worst-hit but transit was not
car_only_examples <- modal_divergence |>
  filter(divergence_category == "Car only") |>
  slice_min(access_pct_change_car, n = 5) |>
  select(sgg_nm, sido_en, access_pct_change_car, access_pct_change_transit)

message("\n  Car-only worst (car bad, transit not as bad):")
print(car_only_examples)

# Districts where transit was worst-hit but car was not
transit_only_examples <- modal_divergence |>
  filter(divergence_category == "Transit only") |>
  slice_min(access_pct_change_transit, n = 5) |>
  select(sgg_nm, sido_en, access_pct_change_car, access_pct_change_transit)

message("\n  Transit-only worst (transit bad, car not as bad):")
print(transit_only_examples)

## Save tables -----------------------------------------------------------------

write_csv(modal_divergence, path(OUTPUT_DIR, "tbl_q5_modal_divergence.csv"))
write_csv(divergence_summary, path(OUTPUT_DIR, "tbl_q5_divergence_summary.csv"))

# Save key statistics for manuscript
divergence_stats <- tibble(
  metric = c("n_districts", "n_bottom_quintile", "n_overlap",
             "n_only_car", "n_only_transit", "jaccard_similarity"),
  value = c(n_districts, n_bottom, n_overlap,
            n_only_car, n_only_transit, round(jaccard, 3))
)
write_csv(divergence_stats, path(OUTPUT_DIR, "tbl_q5_divergence_stats.csv"))

message(glue("\n  Saved: tbl_q5_modal_divergence.csv ({nrow(modal_divergence)} districts)"))
message("  Saved: tbl_q5_divergence_summary.csv")
message("  Saved: tbl_q5_divergence_stats.csv")

## Visualization: Scatterplot + Map --------------------------------------------

# Bottom 20% approach:
# - Identify bottom 20% for car and transit separately
# - Show where they overlap vs diverge
# - Key message: "Single-mode analysis misses different vulnerable areas"

n_bottom <- round(n_districts * 0.2)  # Bottom 20%

# Identify bottom 20% for each mode
car_bottom_ids <- modal_divergence |>
  slice_min(access_pct_change_car, n = n_bottom) |>
  pull(sgg_cd)

transit_bottom_ids <- modal_divergence |>
  slice_min(access_pct_change_transit, n = n_bottom) |>
  pull(sgg_cd)

# Calculate overlap statistics
n_overlap <- length(intersect(car_bottom_ids, transit_bottom_ids))
n_car_only <- length(setdiff(car_bottom_ids, transit_bottom_ids))
n_transit_only <- length(setdiff(transit_bottom_ids, car_bottom_ids))
jaccard <- n_overlap / length(union(car_bottom_ids, transit_bottom_ids))

message(glue("  Bottom 20%: {n_bottom} districts each"))
message(glue("  Overlap: {n_overlap} ({round(jaccard * 100)}% Jaccard)"))
message(glue("  Car only: {n_car_only}, Transit only: {n_transit_only}"))

# Categorize districts
modal_divergence <- modal_divergence |>
  mutate(
    in_car_bottom = sgg_cd %in% car_bottom_ids,
    in_transit_bottom = sgg_cd %in% transit_bottom_ids,
    bottom_category = case_when(
      in_car_bottom & in_transit_bottom ~ "Both",
      in_car_bottom & !in_transit_bottom ~ "Car only",
      !in_car_bottom & in_transit_bottom ~ "Transit only",
      TRUE ~ "Neither"
    ),
    bottom_category = factor(bottom_category,
                             levels = c("Both", "Car only", "Transit only", "Neither"))
  )

# Color palette
bottom_colors <- c(
  "Both" = "#7570b3",         # Purple
  "Car only" = "#d95f02",     # Orange
  "Transit only" = "#1b9e77", # Teal
  "Neither" = "#e0e0e0"       # Light gray
)

# Panel A: Scatterplot with confusion matrix annotation
fig_q5_scatter <- ggplot(modal_divergence,
                         aes(x = access_pct_change_car,
                             y = access_pct_change_transit)) +
  # Background points (Neither)
  geom_point(data = filter(modal_divergence, bottom_category == "Neither"),
             aes(size = total_pop_2021),
             color = bottom_colors["Neither"], alpha = 0.6) +
  # Highlighted points (bottom 20%)
  geom_point(data = filter(modal_divergence, bottom_category != "Neither"),
             aes(size = total_pop_2021, color = bottom_category),
             alpha = 0.85) +
  # Reference lines
  geom_hline(yintercept = 0, color = "gray50", linewidth = 0.3) +
  geom_vline(xintercept = 0, color = "gray50", linewidth = 0.3) +
  # Diagonal = equal decline
  geom_abline(slope = 1, intercept = 0, linetype = "dashed",
              color = "gray40", linewidth = 0.4) +
  # Panel label (top-left corner)
  annotate("text", x = -48, y = 18, label = "(a)",
           fontface = "bold.italic", size = 4.5, hjust = 0) +
  # Scales - keep legend for colors
  scale_color_manual(values = bottom_colors, name = NULL) +
  scale_size_continuous(range = c(1.5, 6), guide = "none") +
  # Labels (no title - will be in subtitle of combined plot)
  labs(
    x = "Car accessibility change (%)",
    y = "Transit accessibility change (%)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = c(0.88, 0.18),
    legend.background = element_rect(fill = "white", color = "gray80", linewidth = 0.3),
    legend.text = element_text(size = 9),
    legend.key.size = unit(0.9, "lines"),
    panel.grid.minor = element_blank()
  ) +
  coord_cartesian(xlim = c(-50, 25), ylim = c(-45, 20)) +
  guides(color = guide_legend(override.aes = list(size = 4)))

# Panel B: Map
map_data_q5 <- modal_divergence |>
  select(sgg_cd, bottom_category) |>
  left_join(
    sgg_sf |> mutate(sgg_cd = SIGUNGU_CD) |> select(sgg_cd, geom),
    by = "sgg_cd"
  ) |>
  st_as_sf() |>
  st_transform(4326)

sido_wgs84_q5 <- sgg_sf |>
  mutate(sido_cd = str_sub(SIGUNGU_CD, 1, 2)) |>
  group_by(sido_cd) |>
  summarize(geom = st_union(geom), .groups = "drop") |>
  st_transform(4326)

fig_q5_map <- ggplot() +
  geom_sf(data = map_data_q5, aes(fill = bottom_category),
          color = "white", linewidth = 0.05) +
  geom_sf(data = sido_wgs84_q5, fill = NA, color = "gray40", linewidth = 0.3) +
  # Panel label (top-left corner) - aligned with (a) y-position
  annotate("text", x = 125.75, y = 38.55, label = "(b)",
           fontface = "bold.italic", size = 4.5, hjust = 0) +
  scale_fill_manual(values = bottom_colors, name = NULL) +
  coord_sf(xlim = c(125.9, 129.5), ylim = c(34.3, 38.7)) +
  theme_void(base_size = 11) +
  theme(legend.position = "none")

# Combine - following Q4 style
fig_q5_divergence <- fig_q5_scatter + fig_q5_map +
  plot_layout(widths = c(1.3, 1)) +
  plot_annotation(
    title = "Where Access Declined Depends on Mode (2021-2023)",
    subtitle = "(a) accessibility change by mode, (b) spatial distribution of bottom 20%",
    caption = glue("Note: Bottom 20% ({n_bottom} districts) selected separately for each mode; ",
                   "{n_overlap} appear in both, {n_car_only} in car only, {n_transit_only} in transit only ",
                   "(Jaccard similarity: {round(jaccard * 100)}%)."),
    theme = theme(
      plot.title = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(color = "gray40", size = 11, margin = margin(b = 8)),
      plot.caption = element_text(color = "gray50", size = 7, hjust = 0, margin = margin(t = 8))
    )
  )

ggsave(path(FIGURE_DIR, "fig_q5_divergence.png"), fig_q5_divergence,
       width = 7, height = 4.2, dpi = 300, bg = "white")

message(glue("\n  Both modes: {n_overlap}"))
message(glue("  Car only: {n_car_only}"))
message(glue("  Transit only: {n_transit_only}"))
message(glue("  Jaccard overlap: {round(jaccard * 100)}%"))
message("\n  Saved: fig_q5_divergence.png")
message("  Modal divergence analysis complete")



# Q6: Demographics ==============================================================
# Do accessibility changes correlate with demographic characteristics?
# Variables: mean age, aging index, population density

message("\n[Q6] Demographic Correlates")

file_delete(dir_ls(OUTPUT_DIR, glob = "*q6*", fail = FALSE))
file_delete(dir_ls(FIGURE_DIR, glob = "*q6*", fail = FALSE))

## Demographic data ------------------------------------------------------------
# Key variables:
#   - pct_elderly: 65세 이상 인구 비율 (from 성연령별인구)
#   - pop_density: 인구밀도 (from 인구총괄)

# 65+ population ratio (from age-sex data)
# Codes: in_age_014 to in_age_021 = 65-69, 70-74, ..., 100+
age_data <- fread(
  path(DATA_ROOT, "census/stats/2024년기준_2021년_성연령별인구.txt"),
  sep = "^", header = FALSE,
  col.names = c("year", "census_id", "stat_code", "value")
) |>
  mutate(
    census_id = as.character(census_id),
    value = as.numeric(value)
  ) |>
  filter(!is.na(value))

# Calculate 65+ ratio per census tract
elderly_codes <- paste0("in_age_0", 14:21)  # 65-69 to 100+

pct_elderly <- age_data |>
  mutate(
    is_elderly = stat_code %in% elderly_codes,
    is_total = str_detect(stat_code, "^in_age_0[0-2][0-9]$") & !str_detect(stat_code, "_남자|_여자")
  ) |>
  summarize(
    pop_elderly = sum(value[is_elderly], na.rm = TRUE),
    pop_total = sum(value[is_total], na.rm = TRUE),
    .by = census_id
  ) |>
  filter(pop_total > 0) |>
  mutate(pct_elderly = pop_elderly / pop_total * 100)

# Population density
pop_density <- fread(
  path(DATA_ROOT, "census/stats/2024년기준_2021년_인구총괄(인구밀도).txt"),
  sep = "^", header = FALSE,
  col.names = c("year", "census_id", "stat_code", "value")
) |>
  filter(stat_code == "to_in_003") |>
  transmute(census_id = as.character(census_id), pop_density = as.numeric(value))

# Combine demographic variables
demographics <- pct_elderly |>
  left_join(pop_density, by = "census_id") |>
  mutate(sgg_cd = str_sub(census_id, 1, 5))

# Aggregate to SGG level (population-weighted)
demo_sgg <- demographics |>
  left_join(pop_total |> filter(year == 2021), by = "census_id") |>
  filter(!is.na(population), population > 0) |>
  summarize(
    pct_elderly = weighted.mean(pct_elderly, population, na.rm = TRUE),
    pop_density = weighted.mean(pop_density, population, na.rm = TRUE),
    total_pop = sum(population, na.rm = TRUE),
    .by = sgg_cd
  )

message(glue("  Demographic data: {nrow(demo_sgg)} SGGs"))
message(glue("  Elderly ratio range: {round(min(demo_sgg$pct_elderly), 1)}% - {round(max(demo_sgg$pct_elderly), 1)}%"))

## Scatter plots ---------------------------------------------------------------
# 2 variables (% elderly, pop density) × 2 modes (Car, Transit)

# Prepare data: both Car and Transit with demographics
q6_car <- tbl_q4_sgg |>
  st_drop_geometry() |>
  filter(mode == "car") |>
  left_join(demo_sgg, by = "sgg_cd") |>
  filter(!is.na(pct_elderly), !is.na(access_pct_change)) |>
  mutate(mode_label = "Car (\u226410min)")

q6_transit <- tbl_q4_sgg |>
  st_drop_geometry() |>
  filter(mode == "transit") |>
  left_join(demo_sgg, by = "sgg_cd") |>
  filter(!is.na(pct_elderly), !is.na(access_pct_change)) |>
  mutate(mode_label = "Transit (\u226430min)")

q6_both <- bind_rows(q6_car, q6_transit) |>
  mutate(
    mode_label = factor(mode_label, levels = c("Car (\u226410min)", "Transit (\u226430min)")),
    pop_density_log = log10(pop_density + 1)
  )

# Calculate correlations for each mode × variable (all data)
cor_table <- q6_both |>
  summarize(
    r_elderly = cor(pct_elderly, access_pct_change, use = "complete.obs"),
    r_density = cor(pop_density_log, access_pct_change, use = "complete.obs"),
    .by = mode_label
  )

message("  Correlations by mode (all data):")
print(cor_table)

# Filter outliers for visualization
# - |change| > 100% excluded
# - pop_density_log < 2.5 excluded (very low density outliers)
q6_plot <- q6_both |>
  filter(
    abs(access_pct_change) <= 100,
    pop_density_log >= 2.5
  )

n_excluded <- nrow(q6_both) - nrow(q6_plot)
message(glue("  Excluded {n_excluded} outliers for visualization"))

# Correlations without outliers
cor_table_filtered <- q6_plot |>
  summarize(
    r_elderly = cor(pct_elderly, access_pct_change, use = "complete.obs"),
    r_density = cor(pop_density_log, access_pct_change, use = "complete.obs"),
    .by = mode_label
  )

message("  Correlations by mode (excl. outliers):")
print(cor_table_filtered)

# Pivot to long format for faceting (2 variables × 2 modes)
q6_long <- q6_plot |>
  select(sgg_cd, mode_label, access_pct_change, total_pop_2021, pct_elderly, pop_density_log) |>
  pivot_longer(
    cols = c(pct_elderly, pop_density_log),
    names_to = "variable",
    values_to = "value"
  ) |>
  mutate(
    variable_label = case_match(
      variable,
      "pct_elderly" ~ "Population 65+ (%)",
      "pop_density_log" ~ "Population Density (log)"
    ),
    # Create combined facet label for proper ordering
    facet_label = paste(variable_label, "-", mode_label),
    facet_label = factor(facet_label, levels = c(
      "Population 65+ (%) - Car (\u226410min)",
      "Population 65+ (%) - Transit (\u226430min)",
      "Population Density (log) - Car (\u226410min)",
      "Population Density (log) - Transit (\u226430min)"
    ))
  )

# Panel labels and correlation annotations
# Clockwise order: (a) top-left, (b) top-right, (c) bottom-left, (d) bottom-right
panel_labels <- q6_long |>
  summarize(
    r = cor(value, access_pct_change, use = "complete.obs"),
    .by = c(mode_label, variable_label, facet_label)
  ) |>
  arrange(facet_label) |>
  mutate(
    panel = c("(a)", "(b)", "(c)", "(d)"),
    cor_label = sprintf("italic(r) == %.2f", r),
    # Color for correlation text (darker versions of mode colors)
    cor_color = case_match(
      mode_label,
      "Car (\u226410min)" ~ "#922b21",
      "Transit (\u226430min)" ~ "#1a5276"
    )
  )

# Create 2×2 scatter using facet_wrap (allows free scales per panel)
fig_q6_scatter <- ggplot(q6_long, aes(x = value, y = access_pct_change)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_point(
    aes(size = total_pop_2021 / 1e5, color = mode_label),
    alpha = 0.5
  ) +
  geom_smooth(
    aes(color = mode_label),
    method = "lm", se = TRUE, linewidth = 0.8
  ) +
  # Panel label (a), (b), etc. at top-left
  geom_text(
    data = panel_labels,
    aes(x = -Inf, y = Inf, label = panel),
    hjust = -0.3, vjust = 1.5, size = 5, fontface = "bold"
  ) +
  # Correlation at top-right with italic r, darker mode colors
  geom_text(
    data = panel_labels,
    aes(x = Inf, y = Inf, label = cor_label),
    color = panel_labels$cor_color,
    hjust = 1.1, vjust = 1.5, size = 4, fontface = "bold", parse = TRUE
  ) +
  facet_wrap(~ facet_label, ncol = 2, scales = "free_x") +
  scale_color_manual(
    values = c("Car (\u226410min)" = "#c0392b", "Transit (\u226430min)" = "#2980b9"),
    guide = "none"
  ) +
  scale_size_continuous(range = c(0.5, 5), guide = "none") +
  labs(
    title = "Demographics and Access Decline (2021-2023)",
    subtitle = "Denser districts lost more; transit shows stronger correlation than car",
    x = NULL,
    y = "Accessibility change (%)"
  ) +
  theme_bw(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "gray40", size = 10, margin = margin(b = 8)),
    strip.text = element_text(face = "bold", size = 10),
    strip.background = element_rect(fill = "gray95"),
    panel.spacing = unit(1, "lines")
  )

ggsave(path(FIGURE_DIR, "fig_q6_demographics.png"), fig_q6_scatter,
       width = 7, height = 5, dpi = 300, bg = "white")

message("  Demographic scatter complete")



# Summary ======================================================================

message("\n", strrep("=", 60))
message("ANALYSIS COMPLETE")
message(strrep("=", 60))

cat("\n--- Key Findings ---\n")
cat(glue("Q1: {comma(total_closed)} banks closed ({round(abs(total_pct), 1)}% decline)\n"))
cat(glue("Q2: Car 10min = {round(car_10_cov, 1)}% | Transit 30min = {round(transit_30_cov, 1)}%\n"))
cat(glue("    Walk/Bicycle 60min: {round(walk_60_cov, 1)}% / {round(bike_60_cov, 1)}%\n"))
cat(glue("Q3-Q4: See figures in {FIGURE_DIR}\n"))

cat("\n--- Outputs ---\n")
cat("Tables:\n")
dir_ls(OUTPUT_DIR, glob = "*.csv") |> path_file() |> sort() |> cat(sep = "\n  ")
cat("\n\nFigures:\n")
dir_ls(FIGURE_DIR, glob = "*.png") |> path_file() |> sort() |> cat(sep = "\n  ")
cat("\n")


# Export to docs ==============================================================
# Copy figures and data to docs/ for Quarto book (Chapter 5)

message("\n[Export] Copying to docs/...")

DOCS_FIG_DIR  <- path(PROJECT_ROOT, "docs/assets/figures/ch5")
DOCS_DATA_DIR <- path(PROJECT_ROOT, "docs/assets/data/ch5")

dir_create(DOCS_FIG_DIR)
dir_create(DOCS_DATA_DIR)

# Copy figures (Q1-Q3 for now)
fig_files <- dir_ls(FIGURE_DIR, glob = "*.png")
for (f in fig_files) {

  file_copy(f, path(DOCS_FIG_DIR, path_file(f)), overwrite = TRUE)
}
message(glue("  Figures: {length(fig_files)} files -> {DOCS_FIG_DIR}"))

# Copy key tables
tbl_files <- dir_ls(OUTPUT_DIR, glob = "*.csv")
for (f in tbl_files) {
  file_copy(f, path(DOCS_DATA_DIR, path_file(f)), overwrite = TRUE)
}
message(glue("  Tables: {length(tbl_files)} files -> {DOCS_DATA_DIR}"))

message("Export complete!")


# Manuscript Figures ===========================================================
# Clean versions without title/subtitle for journal submission
# Captions will be in the manuscript text instead

message("\n[Manuscript] Creating clean figures...")

MANUSCRIPT_FIG_DIR <- path(PROJECT_ROOT, "manuscript/figures")
dir_create(MANUSCRIPT_FIG_DIR)

## fig_q3_trend -------------------------------------------

fig_q3_trend_ms <- ggplot(fig_q3_data, aes(x = year, y = value, color = mode_label)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  geom_label(
    aes(label = label),
    size = 3.5, fontface = "bold", show.legend = FALSE,
    label.size = 0.3, label.padding = unit(0.15, "lines"), fill = "white"
  ) +
  facet_wrap(~metric, scales = "free_y") +
  facetted_pos_scales(
    y = list(
      `(a) Population with Access` = scale_y_continuous(
        limits = c(86, 94), breaks = seq(86, 94, 2), labels = \(x) paste0(x, "%")
      ),
      `(b) Median Banks Reachable` = scale_y_continuous(
        limits = c(0, 70), breaks = seq(0, 60, 20)
      )
    )
  ) +
  scale_x_continuous(breaks = TARGET_YEARS, expand = expansion(mult = 0.15)) +
  scale_color_manual(values = c(
    "Car (10min)" = unname(MODE_COLORS["car"]),
    "Transit (30min)" = unname(MODE_COLORS["transit"])
  )) +
  labs(x = NULL, y = NULL, color = NULL) +
  theme_bw(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    panel.spacing = unit(1.2, "lines"),
    # legned 크기
    legend.text = element_text(size = 10),
    strip.text = element_text(face = "bold", size = 11),
    legend.position = "bottom",
    legend.justification = "center",
    legend.margin = margin(t = -1, b = -0.5)
  ) +
  guides(color = guide_legend(override.aes = list(shape = NA, linewidth = 1.2)))

ggsave(path(MANUSCRIPT_FIG_DIR, "fig_trend.png"), fig_q3_trend_ms,
       width = 7, height = 3.75, dpi = 300, bg = "white")

## fig_q4_threepanel --------------------------------------

fig_q4_threepanel_ms <- (fig_q4_bank | fig_q4_abs | fig_q4_pct) &
  theme(plot.margin = margin(l = 9, r = 9))

ggsave(path(MANUSCRIPT_FIG_DIR, "fig_spatial.png"), fig_q4_threepanel_ms,
       width = 7.5, height = 3.4, dpi = 300, bg = "white")

## fig_q5_divergence --------------------------------------
# Clean version without title/subtitle/caption for manuscript

fig_q5_divergence_ms <- fig_q5_scatter + fig_q5_map +
  plot_layout(widths = c(1.3, 1)) +
  plot_annotation(theme = theme(plot.margin = margin(5, 5, 5, 5)))

ggsave(path(MANUSCRIPT_FIG_DIR, "fig_divergence.png"), fig_q5_divergence_ms,
       width = 8, height = 4, dpi = 300, bg = "white")

## fig_q6_demographics ------------------------------------

fig_q6_scatter_ms <- ggplot(q6_long, aes(x = value, y = access_pct_change)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_point(
    aes(size = total_pop_2021 / 1e5, color = mode_label),
    alpha = 0.5
  ) +
  geom_smooth(
    aes(color = mode_label),
    method = "lm", se = TRUE, linewidth = 0.8
  ) +
  geom_text(
    data = panel_labels,
    aes(x = -Inf, y = Inf, label = panel),
    hjust = -0.3, vjust = 1.5, size = 5, fontface = "bold.italic"
  ) +
  geom_text(
    data = panel_labels,
    aes(x = Inf, y = Inf, label = cor_label),
    color = panel_labels$cor_color,
    hjust = 1.1, vjust = 1.5, size = 4, fontface = "bold", parse = TRUE
  ) +
  facet_wrap(~ facet_label, ncol = 2, scales = "free_x") +
  scale_color_manual(
    values = c("Car (\u226410min)" = "#c0392b", "Transit (\u226430min)" = "#2980b9"),
    guide = "none"
  ) +
  scale_size_continuous(range = c(0.5, 5), guide = "none") +
  labs(x = NULL, y = "Accessibility change (%)") +
  theme_bw(base_size = 11) +
  theme(
    strip.text = element_text(face = "bold", size = 10),
    strip.background = element_rect(fill = "gray95"),
    panel.spacing = unit(1, "lines")
  )

ggsave(path(MANUSCRIPT_FIG_DIR, "fig_demographics.png"), fig_q6_scatter_ms,
       width = 7.5, height = 5.2, dpi = 300, bg = "white")

# Supplementary Figures ===========================================================
## fig_q2_coverage ----------------------------------------------

fig_q2_coverage_ms <- ggplot(fig_q2_data, aes(x = threshold_min, y = pct_coverage,
                                              color = mode_label, group = mode_label)) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 90, ymax = 95,
           fill = "gray70", alpha = 0.35) +
  geom_hline(yintercept = c(90, 95), linetype = "dashed", color = "gray50", linewidth = 0.5) +
  annotate("text", x = 22.5, y = 92.5, label = "90-95%", size = 3, color = "gray40", fontface = "italic") +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.5) +
  geom_segment(x = 10, xend = 10, y = 0, yend = car_10_cov,
               linetype = "dotted", color = MODE_COLORS["car"], linewidth = 0.7) +
  geom_segment(x = 30, xend = 30, y = 0, yend = transit_30_cov,
               linetype = "dotted", color = MODE_COLORS["transit"], linewidth = 0.7) +
  geom_point(data = tibble(
    mode_label = factor(c("Car", "Transit"), levels = c("Car", "Transit", "Bicycle", "Walk")),
    threshold_min = c(10, 30),
    pct_coverage = c(car_10_cov, transit_30_cov)
  ), size = 4, shape = 21, fill = "white", stroke = 1.5) +
  geom_shadowtext(aes(x = 10, y = car_10_cov - 7.5,
                      label = glue("10min\n({round(car_10_cov, 0)}%)")),
                  color = MODE_COLORS["car"], bg.color = "white", bg.r = 0.15,
                  size = 3.4, fontface = "bold", lineheight = 0.85,
                  data = data.frame(x = 1), inherit.aes = FALSE) +
  geom_shadowtext(aes(x = 30, y = transit_30_cov - 7.5,
                      label = glue("30min\n({round(transit_30_cov, 0)}%)")),
                  color = MODE_COLORS["transit"], bg.color = "white", bg.r = 0.15,
                  size = 3.4, fontface = "bold", lineheight = 0.85,
                  data = data.frame(x = 1), inherit.aes = FALSE) +
  scale_x_continuous(breaks = seq(5, 60, 5), labels = \(x) paste0(x, "min")) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 10), labels = \(x) paste0(x, "%")) +
  scale_color_manual(values = c(
    "Car" = unname(MODE_COLORS["car"]),
    "Transit" = unname(MODE_COLORS["transit"]),
    "Walk" = unname(MODE_COLORS["walk"]),
    "Bicycle" = unname(MODE_COLORS["bicycle"])
  )) +
  labs(x = NULL, y = NULL, color = NULL) +
  theme_bw(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    legend.justification = "center",
    legend.text = element_text(size = 10),
    legend.margin = margin(t = -1, b = 0),
    legend.box.margin = margin(t = 0, b = 2),
  ) +
  guides(color = guide_legend(override.aes = list(size = 1.2, shape = NA)))

ggsave(path(MANUSCRIPT_FIG_DIR, "fig_supp_coverage.png"), fig_q2_coverage_ms,
       width = 7, height = 4.5, dpi = 300, bg = "white")

## fig_q2_choice ------------------------------------------------

fig_q2_choice_ms <- ggplot(fig_q2_choice_data, aes(x = n_banks, weight = population)) +
  geom_density(aes(fill = mode_label), alpha = 0.4, color = NA) +
  geom_density(aes(color = mode_label), fill = NA, linewidth = 0.8) +
  geom_vline(xintercept = median_car, linetype = "dashed", color = "#8B0000", linewidth = 0.7) +
  geom_vline(xintercept = median_transit, linetype = "dashed", color = "#00008B", linewidth = 0.7) +
  scale_x_continuous(
    limits = c(0, 400),
    breaks = sort(c(0, round(median_transit), round(median_car), 100, 200, 300, 400)),
    labels = \(x) case_when(
      x == round(median_car) ~ glue("<b style='color:#8B0000;font-size:12pt'>{x}</b>"),
      x == round(median_transit) ~ glue("<b style='color:#00008B;font-size:12pt'>{x}</b>"),
      TRUE ~ as.character(x)
    )
  ) +
  scale_fill_manual(values = c(
    "Car (10min)" = unname(MODE_COLORS["car"]),
    "Transit (30min)" = unname(MODE_COLORS["transit"])
  )) +
  scale_color_manual(values = c("Car (10min)" = "#8B0000", "Transit (30min)" = "#00008B")) +
  labs(x = "Number of banks", y = NULL, fill = NULL, color = NULL) +
  theme_bw(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    legend.justification = "center",
    legend.margin = margin(t = 0, b = 0),
    legend.box.margin = margin(t = 0, b = 2),
    legend.key.size = unit(1, "lines"),
    legend.text = element_text(size = 10),
    axis.text.x = element_markdown(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  guides(fill = guide_legend(override.aes = list(alpha = 0.6)), color = "none")

ggsave(path(MANUSCRIPT_FIG_DIR, "fig_supp_choice.png"), fig_q2_choice_ms,
       width = 7, height = 4.5, dpi = 300, bg = "white")

## fig_q1_national -------------------------

fig_q1_national_ms <- ggplot(plot_national, aes(x = x_pos)) +
  geom_vline(xintercept = year_dividers, color = "gray80", linewidth = 0.3, linetype = "dashed") +
  geom_col(aes(y = y_adjusted, fill = in_study), width = 0.7) +
  scale_fill_manual(values = c("FALSE" = GRAY_MED, "TRUE" = GRAY_DARK), guide = "none") +
  geom_text(data = \(d) filter(d, !in_study),
            aes(y = y_label_pos, label = comma(n_banks)),
            color = "white", size = 2.5, vjust = 0.15) +
  geom_text(data = \(d) filter(d, in_study),
            aes(y = y_label_pos, label = comma(n_banks)),
            color = "white", size = 3, fontface = "bold", vjust = 0.15) +
  scale_x_continuous(
    breaks = plot_national$x_pos,
    labels = plot_national$x_label,
    expand = expansion(add = 0.5)
  ) +
  scale_y_continuous(
    limits = c(0, (max_banks - y_min) * 1.12),
    breaks = seq(0, 2000, 500),
    labels = \(x) comma(x + y_min),
    expand = expansion(mult = c(0, 0))
  ) +
  labs(x = NULL, y = NULL) +
  theme_bw(base_size = 11) +
  theme(
    axis.text.x = element_markdown(size = 9, lineheight = 1.2, halign = 0.5),
    panel.grid.minor = element_blank(),
    legend.text = element_text(size = 10),
    panel.grid.major.x = element_blank()
  )

ggsave(path(MANUSCRIPT_FIG_DIR, "fig_supp_national.png"), fig_q1_national_ms,
       width = 7.5, height = 4, dpi = 300, bg = "white")

message(glue("  Manuscript figures saved to {MANUSCRIPT_FIG_DIR}"))
message("  - fig_trend.png")
message("  - fig_spatial.png")
message("  - fig_demographics.png")
message("  - fig_supp_coverage.png (S1)")
message("  - fig_supp_choice.png (S2)")
message("  - fig_supp_national.png (S3)")
