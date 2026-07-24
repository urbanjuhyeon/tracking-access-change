# 6-3-measurement-audit.R
#
# Provisional measurement audit for the 2021 -> 2023 decomposition.
# Reuses stored actual and hybrid travel-time matrices; no routing is performed.
#
# Outputs:
#   - threshold_decomposition.csv
#   - kth_branch_decomposition.csv
#   - kth_branch_validity.csv
#   - accessibility_transitions.csv
#   - transition_summary.csv
#   - measurement_audit_origin.parquet
#   - three diagnostic PNG figures
#   - measurement_audit_summary.md

suppressPackageStartupMessages({
  library(arrow)
  library(data.table)
  library(DBI)
  library(dplyr)
  library(duckdb)
  library(fs)
  library(ggplot2)
  library(glue)
  library(readr)
  library(scales)
  library(sf)
  library(stringr)
  library(tidyr)
})

PROJECT_ROOT <- normalizePath(
  Sys.getenv("ACCESS_DECLINE_ROOT", unset = "."),
  winslash = "/", mustWork = TRUE
)
if (!file.exists(file.path(PROJECT_ROOT, ".gitignore"))) {
  stop("Run from the repository root or set ACCESS_DECLINE_ROOT.")
}

DATA_ROOT   <- path(PROJECT_ROOT, "data")
RESULTS_DIR <- path(PROJECT_ROOT, "workflows/results")
ROUTING_DIR <- path(RESULTS_DIR, "decompose/routing")
OUTPUT_DIR  <- path(PROJECT_ROOT, "workflows/analysis/measurement_audit")
TEMP_DIR    <- path(OUTPUT_DIR, "duckdb_temp")

dir_create(OUTPUT_DIR)
dir_create(TEMP_DIR)

source(path(PROJECT_ROOT, "scripts/_bank_locations.R"))
assert_location_id_ttm_ready(RESULTS_DIR)

thresholds <- c(5L, 10L, 15L, 20L, 25L, 30L)
k_values <- c(1L, 3L, 5L)
departure_slots <- c("07", "09", "12", "14", "17", "19")
mode_caps <- c(car = 45, transit = 60)

message(strrep("=", 72))
message("MEASUREMENT AUDIT: 2021 -> 2023")
message(strrep("=", 72))

# ---- Population and bank-location maps --------------------------------------

pop <- fread(
  path(DATA_ROOT, "census/stats", "2024년기준_2021년_인구총괄(총인구).txt"),
  sep = "^", header = FALSE,
  col.names = c("year", "census_id", "stat_code", "value")
) |>
  as_tibble() |>
  filter(stat_code == "to_in_001") |>
  transmute(
    origin_id = as.character(census_id),
    population = suppressWarnings(as.numeric(value))
  ) |>
  filter(!is.na(population), population > 0) |>
  distinct(origin_id, .keep_all = TRUE)

origin_prefixes <- sort(unique(str_sub(pop$origin_id, 1, 2)))

bank_raw <- read_canonical_bank_data(DATA_ROOT)
banks_2021 <- make_bank_locations(bank_raw, "2020h2") |>
  st_drop_geometry() |>
  transmute(location_id = as.character(location_id), bank_name = as.character(bank_name))
banks_2023 <- make_bank_locations(bank_raw, "2022h2") |>
  st_drop_geometry() |>
  transmute(location_id = as.character(location_id), bank_name = as.character(bank_name))

bank_map <- bind_rows(banks_2021, banks_2023) |>
  distinct(location_id, bank_name)
if (bank_map |> count(location_id) |> filter(n > 1) |> nrow() > 0) {
  stop("A stable location_id maps to multiple bank names.")
}

survivor_ids <- intersect(banks_2021$location_id, banks_2023$location_id)

bank_inventory <- bind_rows(
  banks_2021 |> mutate(year = 2021L),
  banks_2023 |> mutate(year = 2023L)
) |>
  count(year, bank_name, name = "branches") |>
  arrange(year, desc(branches), bank_name)
write_csv(bank_inventory, path(OUTPUT_DIR, "bank_name_inventory.csv"))

message(glue("Population origins: {comma(nrow(pop))}"))
message(glue(
  "Banks: {comma(nrow(banks_2021))} (2021), ",
  "{comma(nrow(banks_2023))} (2023); ",
  "{comma(length(survivor_ids))} stable locations"
))
message(glue(
  "Institutions: {n_distinct(banks_2021$bank_name)} (2021), ",
  "{n_distinct(banks_2023$bank_name)} (2023)"
))

# ---- DuckDB setup ------------------------------------------------------------

con <- dbConnect(duckdb())
on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)

dbExecute(con, "SET threads = 4")
dbExecute(con, "SET memory_limit = '4GB'")
dbExecute(con, "SET preserve_insertion_order = false")
dbExecute(con, glue("SET temp_directory = '{path_abs(TEMP_DIR)}'"))
dbWriteTable(con, "origin_ids", pop |> select(origin_id), overwrite = TRUE)
dbWriteTable(
  con, "departure_slots",
  bind_rows(
    tibble(mode = "car", departure_time = "12"),
    tibble(mode = "transit", departure_time = departure_slots)
  ),
  overwrite = TRUE
)
dbWriteTable(con, "bank_map", bank_map, overwrite = TRUE)
dbWriteTable(
  con, "survivor_ids",
  tibble(location_id = survivor_ids), overwrite = TRUE
)

sql_path <- function(x) str_replace_all(path_abs(x), fixed("\\"), "/")
th_label <- function(x) sprintf("%02d", x)

count_sql <- function(prefix, distinct_brand = FALSE) {
  vapply(
    thresholds,
    function(th) {
      if (distinct_brand) {
        glue(
          "COUNT(DISTINCT CASE WHEN tt <= {th} THEN bank_name END) ",
          "AS {prefix}_{th_label(th)}"
        )
      } else {
        glue(
          "COUNT(*) FILTER (WHERE tt <= {th}) ",
          "AS {prefix}_{th_label(th)}"
        )
      }
    },
    character(1)
  )
}

mean_sql <- function(prefix) {
  vapply(
    thresholds,
    function(th) {
      nm <- glue("{prefix}_{th_label(th)}")
      glue("AVG(COALESCE(m.{nm}, 0)) AS {nm}")
    },
    character(1)
  )
}

make_source_sql <- function(
    actual_file, origin_prefix, survivor_only = FALSE, extra_pattern = NULL) {
  actual_file <- sql_path(actual_file)
  origin_filter <- glue("CAST(from_id AS VARCHAR) LIKE '{origin_prefix}%'")

  if (!survivor_only) {
    return(glue("(
      SELECT CAST(from_id AS VARCHAR) AS origin_id,
             CAST(to_id AS VARCHAR) AS location_id,
             CAST(departure_time AS VARCHAR) AS departure_time,
             travel_time_p50 AS tt
      FROM read_parquet('{actual_file}')
      WHERE from_id IS NOT NULL
        AND to_id IS NOT NULL
        AND travel_time_p50 IS NOT NULL
        AND {origin_filter}
    )"))
  }

  if (is.null(extra_pattern)) {
    stop("Hybrid states require an extra counterfactual pattern.")
  }
  extra_pattern <- sql_path(extra_pattern)

  glue("(
    SELECT CAST(t.from_id AS VARCHAR) AS origin_id,
           CAST(t.to_id AS VARCHAR) AS location_id,
           CAST(t.departure_time AS VARCHAR) AS departure_time,
           t.travel_time_p50 AS tt
    FROM read_parquet('{actual_file}') t
    INNER JOIN survivor_ids s
      ON CAST(t.to_id AS VARCHAR) = s.location_id
    WHERE t.from_id IS NOT NULL
      AND t.to_id IS NOT NULL
      AND t.travel_time_p50 IS NOT NULL
      AND CAST(t.from_id AS VARCHAR) LIKE '{origin_prefix}%'

    UNION ALL

    SELECT CAST(from_id AS VARCHAR) AS origin_id,
           CAST(to_id AS VARCHAR) AS location_id,
           CAST(departure_time AS VARCHAR) AS departure_time,
           travel_time_p50 AS tt
    FROM read_parquet('{extra_pattern}')
    WHERE from_id IS NOT NULL
      AND to_id IS NOT NULL
      AND travel_time_p50 IS NOT NULL
      AND {origin_filter}
  )")
}

compute_state_chunk <- function(
    mode, state, actual_file, origin_prefix,
    survivor_only = FALSE, extra_pattern = NULL) {
  cap <- unname(mode_caps[[mode]])
  source_sql <- make_source_sql(
    actual_file, origin_prefix, survivor_only, extra_pattern
  )

  branch_counts <- paste(count_sql("branch", FALSE), collapse = ",\n             ")
  brand_counts <- paste(count_sql("brand", TRUE), collapse = ",\n             ")
  branch_means <- paste(mean_sql("branch"), collapse = ",\n           ")
  brand_means <- paste(mean_sql("brand"), collapse = ",\n           ")

  query <- glue("
    WITH raw AS (
      SELECT s.origin_id, s.location_id, s.departure_time, s.tt, b.bank_name
      FROM {source_sql} s
      INNER JOIN bank_map b ON s.location_id = b.location_id
    ),
    metrics_by_departure AS (
      SELECT origin_id, departure_time,
             {branch_counts},
             {brand_counts},
             list_extract(min(tt, 5), 1) AS kbranch_1,
             list_extract(min(tt, 5), 3) AS kbranch_3,
             list_extract(min(tt, 5), 5) AS kbranch_5
      FROM raw
      GROUP BY origin_id, departure_time
    ),
    origin_departure_grid AS (
      SELECT o.origin_id, d.departure_time
      FROM origin_ids o CROSS JOIN departure_slots d
      WHERE o.origin_id LIKE '{origin_prefix}%'
        AND d.mode = '{mode}'
    )
    SELECT g.origin_id,
           {branch_means},
           {brand_means},
           AVG(COALESCE(m.kbranch_1, {cap})) AS kbranch_1,
           AVG(COALESCE(m.kbranch_3, {cap})) AS kbranch_3,
           AVG(COALESCE(m.kbranch_5, {cap})) AS kbranch_5,
           AVG(CASE WHEN m.kbranch_1 IS NULL THEN 0 ELSE 1 END) AS kvalid_1,
           AVG(CASE WHEN m.kbranch_3 IS NULL THEN 0 ELSE 1 END) AS kvalid_3,
           AVG(CASE WHEN m.kbranch_5 IS NULL THEN 0 ELSE 1 END) AS kvalid_5
    FROM origin_departure_grid g
    LEFT JOIN metrics_by_departure m
      ON g.origin_id = m.origin_id
     AND g.departure_time = m.departure_time
    GROUP BY g.origin_id
    ORDER BY g.origin_id
  ")

  dbGetQuery(con, query) |>
    as_tibble() |>
    mutate(mode = mode, state = state, .before = 1)
}

compute_state_metrics <- function(
    mode, state, actual_file, survivor_only = FALSE, extra_pattern = NULL) {
  message(glue("  [{mode}] {state}: querying stored matrices by province..."))
  started <- Sys.time()
  chunks <- vector("list", length(origin_prefixes))

  for (i in seq_along(origin_prefixes)) {
    prefix <- origin_prefixes[[i]]
    chunks[[i]] <- tryCatch(
      compute_state_chunk(
        mode, state, actual_file, prefix, survivor_only, extra_pattern
      ),
      error = function(e) {
        if (!str_detect(conditionMessage(e), regex("memory|allocation", TRUE))) {
          stop(e)
        }

        district_prefixes <- pop |>
          filter(str_starts(origin_id, prefix)) |>
          transmute(prefix = str_sub(origin_id, 1, 5)) |>
          distinct(prefix) |>
          pull(prefix) |>
          sort()
        message(glue(
          "    {prefix}: province chunk exceeded memory; retrying ",
          "{length(district_prefixes)} district chunks"
        ))
        bind_rows(lapply(
          district_prefixes,
          function(district_prefix) compute_state_chunk(
            mode, state, actual_file, district_prefix,
            survivor_only, extra_pattern
          )
        ))
      }
    )

    if (i %% 4 == 0 || i == length(origin_prefixes)) {
      message(glue(
        "    [{mode}] {state}: {i}/{length(origin_prefixes)} province groups"
      ))
    }
  }

  out <- bind_rows(chunks) |>
    arrange(origin_id)
  message(glue(
    "  [{mode}] {state}: {comma(nrow(out))} origins in ",
    "{round(as.numeric(difftime(Sys.time(), started, units = 'mins')), 2)} min"
  ))
  out
}

# State notation:
#   BB: base network + base banks
#   YY: target network + target banks
#   BY: base network + target banks
#   YB: target network + base banks

state_results <- list()

for (mode in c("car", "transit")) {
  base_actual <- path(RESULTS_DIR, glue("ttm_{mode}_2021.parquet"))
  target_actual <- path(RESULTS_DIR, glue("ttm_{mode}_2023.parquet"))
  forward_extra <- path(ROUTING_DIR, glue("ttm_{mode}_2021net_nb2023_*.parquet"))
  reverse_extra <- path(ROUTING_DIR, glue("ttm_{mode}_2023net_nb2021_*.parquet"))

  state_results[[glue("{mode}_BB")]] <- compute_state_metrics(
    mode, "BB", base_actual
  )
  state_results[[glue("{mode}_YY")]] <- compute_state_metrics(
    mode, "YY", target_actual
  )
  state_results[[glue("{mode}_BY")]] <- compute_state_metrics(
    mode, "BY", base_actual, TRUE, forward_extra
  )
  state_results[[glue("{mode}_YB")]] <- compute_state_metrics(
    mode, "YB", target_actual, TRUE, reverse_extra
  )
}

origin_metrics <- bind_rows(state_results)
write_parquet(
  origin_metrics,
  path(OUTPUT_DIR, "measurement_audit_origin.parquet"),
  compression = "zstd"
)

# ---- National decomposition -------------------------------------------------

metric_pattern <- paste0(
  "^(branch_(05|10|15|20|25|30)|",
  "brand_(05|10|15|20|25|30)|",
  "kbranch_(1|3|5))$"
)

national_state <- origin_metrics |>
  inner_join(pop, by = "origin_id") |>
  pivot_longer(
    cols = matches(metric_pattern),
    names_to = "metric",
    values_to = "value"
  ) |>
  group_by(mode, state, metric) |>
  summarize(
    value = weighted.mean(value, population),
    population = sum(population),
    .groups = "drop"
  ) |>
  mutate(
    metric_family = case_when(
      str_starts(metric, "branch_") ~ "reachable branches",
      str_starts(metric, "brand_") ~ "reachable institutions",
      str_starts(metric, "kbranch_") ~ "k-th branch travel time",
      TRUE ~ NA_character_
    ),
    threshold = case_when(
      str_starts(metric, "branch_") ~ as.integer(str_remove(metric, "branch_")),
      str_starts(metric, "brand_") ~ as.integer(str_remove(metric, "brand_")),
      TRUE ~ NA_integer_
    ),
    k = if_else(
      str_starts(metric, "kbranch_"),
      as.integer(str_remove(metric, "kbranch_")),
      NA_integer_
    )
  )

decompose_national <- function(df) {
  df |>
    select(mode, metric, metric_family, threshold, k, state, value) |>
    pivot_wider(names_from = state, values_from = value) |>
    transmute(
      mode, metric, metric_family, threshold, k,
      base = BB,
      target = YY,
      counterfactual_base_network = BY,
      counterfactual_target_network = YB,
      total = YY - BB,
      service_directional = BY - BB,
      network_directional = YB - BB,
      interaction = (YY - BB) - (BY - BB) - (YB - BB),
      service = service_directional + interaction / 2,
      network = network_directional + interaction / 2
    ) |>
    mutate(
      service_share = if_else(
        abs(total) > 1e-9, service / total * 100, NA_real_
      ),
      network_share = if_else(
        abs(total) > 1e-9, network / total * 100, NA_real_
      )
    )
}

national_decomp <- decompose_national(national_state)

threshold_decomp <- national_decomp |>
  filter(metric_family %in% c("reachable branches", "reachable institutions")) |>
  arrange(metric_family, mode, threshold)
write_csv(threshold_decomp, path(OUTPUT_DIR, "threshold_decomposition.csv"))

kth_decomp <- national_decomp |>
  filter(metric_family == "k-th branch travel time") |>
  arrange(mode, k)
write_csv(kth_decomp, path(OUTPUT_DIR, "kth_branch_decomposition.csv"))

kth_validity <- origin_metrics |>
  inner_join(pop, by = "origin_id") |>
  pivot_longer(
    cols = matches("^kvalid_(1|3|5)$"),
    names_to = "metric", values_to = "valid_fraction"
  ) |>
  mutate(k = as.integer(str_remove(metric, "kvalid_"))) |>
  group_by(mode, state, k) |>
  summarize(
    pct_population_departures_observed =
      weighted.mean(valid_fraction, population) * 100,
    .groups = "drop"
  ) |>
  arrange(mode, k, state)
write_csv(kth_validity, path(OUTPUT_DIR, "kth_branch_validity.csv"))

# ---- Accessibility transitions ---------------------------------------------

actual_wide <- origin_metrics |>
  filter(state %in% c("BB", "YY")) |>
  select(-matches("^kvalid_")) |>
  pivot_wider(
    names_from = state,
    values_from = matches("^(branch|brand|kbranch)_"),
    names_glue = "{.value}_{state}"
  ) |>
  inner_join(pop, by = "origin_id")

transition_rows <- list()
transition_index <- 1L

for (mode_i in c("car", "transit")) {
  mode_df <- actual_wide |> filter(mode == mode_i)

  for (th in thresholds) {
    suffix <- th_label(th)
    margin_specs <- list(
      any_branch = list(prefix = "branch", cutoff = 1),
      at_least_three_branches = list(prefix = "branch", cutoff = 3),
      at_least_two_institutions = list(prefix = "brand", cutoff = 2)
    )

    for (margin_name in names(margin_specs)) {
      spec <- margin_specs[[margin_name]]
      base_col <- glue("{spec$prefix}_{suffix}_BB")
      target_col <- glue("{spec$prefix}_{suffix}_YY")
      base_access <- mode_df[[base_col]] >= spec$cutoff
      target_access <- mode_df[[target_col]] >= spec$cutoff

      transition <- case_when(
        base_access & target_access ~ "retained",
        base_access & !target_access ~ "lost",
        !base_access & target_access ~ "gained",
        TRUE ~ "never"
      )

      transition_rows[[transition_index]] <- tibble(
        mode = mode_i,
        threshold = th,
        margin = margin_name,
        transition = transition,
        population = mode_df$population
      ) |>
        group_by(mode, threshold, margin, transition) |>
        summarize(population = sum(population), .groups = "drop") |>
        mutate(
          total_population = sum(population),
          pct_population = population / total_population * 100
        )
      transition_index <- transition_index + 1L
    }
  }
}

transitions <- bind_rows(transition_rows) |>
  arrange(margin, mode, threshold, transition)
write_csv(transitions, path(OUTPUT_DIR, "accessibility_transitions.csv"))

transition_summary <- transitions |>
  select(mode, threshold, margin, transition, pct_population) |>
  pivot_wider(names_from = transition, values_from = pct_population, values_fill = 0) |>
  mutate(
    gross_churn = lost + gained,
    net_change = gained - lost
  ) |>
  arrange(margin, mode, threshold)
write_csv(transition_summary, path(OUTPUT_DIR, "transition_summary.csv"))

# ---- Diagnostic figures -----------------------------------------------------

component_labels <- c(
  total = "Observed total",
  service = "Branch-side",
  network = "Network-side"
)
component_colors <- c(
  "Observed total" = "#202124",
  "Branch-side" = "#C44E52",
  "Network-side" = "#4C72B0"
)

threshold_plot_data <- threshold_decomp |>
  select(mode, metric_family, threshold, total, service, network) |>
  pivot_longer(
    cols = c(total, service, network),
    names_to = "component", values_to = "change"
  ) |>
  mutate(
    component = recode(component, !!!component_labels),
    mode = recode(mode, car = "Car", transit = "Transit"),
    metric_family = recode(
      metric_family,
      `reachable branches` = "Reachable branches",
      `reachable institutions` = "Reachable institutions"
    )
  )

p_threshold <- ggplot(
  threshold_plot_data,
  aes(threshold, change, color = component, linetype = component)
) +
  geom_hline(yintercept = 0, color = "grey75", linewidth = 0.35) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2) +
  facet_grid(mode ~ metric_family, scales = "free_y") +
  scale_color_manual(values = component_colors) +
  scale_linetype_manual(values = c("solid", "dashed", "dotdash")) +
  scale_x_continuous(breaks = thresholds) +
  labs(
    title = "Accessibility loss and attribution across common time thresholds",
    subtitle = "Order-averaged branch and network components, 2021-2023",
    x = "Door-to-door travel-time threshold (minutes)",
    y = "Change in reachable opportunities",
    color = NULL, linetype = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold")
  )

ggsave(
  path(OUTPUT_DIR, "fig_threshold_attribution.png"),
  p_threshold, width = 11, height = 6.8, dpi = 220
)

k_plot_data <- kth_decomp |>
  select(mode, k, total, service, network) |>
  pivot_longer(
    cols = c(total, service, network),
    names_to = "component", values_to = "change_minutes"
  ) |>
  mutate(
    component = recode(component, !!!component_labels),
    mode = recode(mode, car = "Car", transit = "Transit")
  )

p_k <- ggplot(
  k_plot_data,
  aes(factor(k), change_minutes, color = component, group = component)
) +
  geom_hline(yintercept = 0, color = "grey75", linewidth = 0.35) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2.4) +
  facet_wrap(~mode, scales = "free_y") +
  scale_color_manual(values = component_colors) +
  labs(
    title = "Change in travel time to the 1st, 3rd, and 5th branch",
    subtitle = "Travel times are conservatively capped at the routing horizon (45 min car; 60 min transit)",
    x = "k-th nearest branch", y = "Change in travel time (minutes)", color = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold")
  )

ggsave(
  path(OUTPUT_DIR, "fig_kth_branch_attribution.png"),
  p_k, width = 9, height = 4.8, dpi = 220
)

transition_plot_data <- transition_summary |>
  filter(margin %in% c("any_branch", "at_least_two_institutions")) |>
  select(mode, threshold, margin, lost, gained) |>
  pivot_longer(c(lost, gained), names_to = "direction", values_to = "pct") |>
  mutate(
    signed_pct = if_else(direction == "lost", -pct, pct),
    direction = recode(direction, lost = "Lost access", gained = "Gained access"),
    mode = recode(mode, car = "Car", transit = "Transit"),
    margin = recode(
      margin,
      any_branch = "At least one branch",
      at_least_two_institutions = "At least two institutions"
    )
  )

p_transition <- ggplot(
  transition_plot_data,
  aes(threshold, signed_pct, fill = direction, group = direction)
) +
  geom_hline(yintercept = 0, color = "grey45", linewidth = 0.35) +
  geom_col(position = "identity", width = 3.5, alpha = 0.9) +
  facet_grid(mode ~ margin, scales = "free_y") +
  scale_x_continuous(breaks = thresholds) +
  scale_fill_manual(values = c("Lost access" = "#C44E52", "Gained access" = "#55A868")) +
  labs(
    title = "Gross access losses and gains hidden by net coverage",
    subtitle = "Population transitions between 2021 and 2023",
    x = "Door-to-door travel-time threshold (minutes)",
    y = "Population share (percentage points; losses shown below zero)",
    fill = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold")
  )

ggsave(
  path(OUTPUT_DIR, "fig_access_transitions.png"),
  p_transition, width = 11, height = 6.8, dpi = 220
)

# ---- Short generated audit note ---------------------------------------------

fmt <- function(x, digits = 2) formatC(x, format = "f", digits = digits)

headline_thresholds <- tibble(
  mode = c("car", "transit"),
  threshold = c(10L, 30L)
)

headline_threshold <- threshold_decomp |>
  inner_join(headline_thresholds, by = c("mode", "threshold")) |>
  arrange(mode, metric_family)

headline_k <- kth_decomp |>
  arrange(mode, k)

headline_transition <- transition_summary |>
  inner_join(headline_thresholds, by = c("mode", "threshold")) |>
  filter(margin %in% c("any_branch", "at_least_two_institutions")) |>
  arrange(mode, margin)

threshold_row <- function(r) {
  paste0(
    "| ", r[["mode"]], " | ", r[["metric_family"]], " | ", r[["threshold"]], " | ",
    fmt(as.numeric(r[["base"]])), " | ", fmt(as.numeric(r[["target"]])), " | ",
    fmt(as.numeric(r[["total"]])), " | ", fmt(as.numeric(r[["service"]])), " | ",
    fmt(as.numeric(r[["network"]])), " |"
  )
}

k_row <- function(r) {
  paste0(
    "| ", r[["mode"]], " | ", r[["k"]], " | ",
    fmt(as.numeric(r[["base"]])), " | ", fmt(as.numeric(r[["target"]])), " | ",
    fmt(as.numeric(r[["total"]])), " | ", fmt(as.numeric(r[["service"]])), " | ",
    fmt(as.numeric(r[["network"]])), " |"
  )
}

transition_row <- function(r) {
  paste0(
    "| ", r[["mode"]], " | ", r[["margin"]], " | ",
    fmt(as.numeric(r[["lost"]]), 3), " | ",
    fmt(as.numeric(r[["gained"]]), 3), " | ",
    fmt(as.numeric(r[["net_change"]]), 3), " |"
  )
}

note_lines <- c(
  "# Provisional measurement audit (2021-2023)",
  "",
  "All estimates reuse stored actual and hybrid travel-time matrices; no routing was rerun.",
  "Service and network effects below are order-averaged (the interaction is split equally).",
  "",
  "## Mode-specific headline thresholds",
  "",
  "| Mode | Metric | Threshold | Base | Target | Total | Service | Network |",
  "|---|---|---:|---:|---:|---:|---:|---:|",
  apply(headline_threshold, 1, threshold_row),
  "",
  "## Capped travel time to the k-th branch",
  "",
  "| Mode | k | Base min | Target min | Total | Service | Network |",
  "|---|---:|---:|---:|---:|---:|---:|",
  apply(headline_k, 1, k_row),
  "",
  "## Gross transitions at the headline thresholds",
  "",
  "| Mode | Margin | Lost % | Gained % | Net pp |",
  "|---|---|---:|---:|---:|",
  apply(headline_transition, 1, transition_row),
  "",
  "## Interpretation guardrails",
  "",
  "- Reachable branch counts measure branch availability, not realized use.",
  "- Reachable institutions reduce duplicate-branch inflation but still do not identify a customer's own bank.",
  "- k-th travel times are capped at the stored routing horizon and should be read with kth_branch_validity.csv.",
  "- Threshold curves are the primary diagnostic; the 10/30-minute values are illustrative summaries."
)

writeLines(note_lines, path(OUTPUT_DIR, "measurement_audit_summary.md"), useBytes = TRUE)

message(strrep("=", 72))
message("MEASUREMENT AUDIT COMPLETE")
message(glue("Outputs: {path_abs(OUTPUT_DIR)}"))
message(strrep("=", 72))
