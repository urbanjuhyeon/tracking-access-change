# 6-4-decompose-typology-figures.R
# Quadrant and k-means plots for decomposition mechanisms.

pacman::p_load(tidyverse, fs, glue, scales)

PROJECT_ROOT <- normalizePath(
  Sys.getenv("ACCESS_DECLINE_ROOT", unset = "."),
  winslash = "/", mustWork = TRUE
)
if (!file.exists(file.path(PROJECT_ROOT, ".gitignore"))) {
  stop("Run from the repository root or set ACCESS_DECLINE_ROOT.")
}
OUTPUT_DIR   <- path(PROJECT_ROOT, "workflows/analysis")
FIGURE_DIR   <- path(OUTPUT_DIR, "figures")
dir_create(FIGURE_DIR)

set.seed(20260623)

message(strrep("=", 70))
message("DECOMPOSITION TYPOLOGY FIGURES")
message(strrep("=", 70))

decomp <- read_csv(path(OUTPUT_DIR, "decomp_demo_sgg.csv"),
                   show_col_types = FALSE) |>
  mutate(
    service_loss = pmax(-service_effect, 0),
    network_loss = pmax(-network_effect, 0),
    loss_sum = service_loss + network_loss,
    dominant_component = case_when(
      total_change < 0 & loss_sum > 0 & service_loss >= 0.6 * loss_sum ~ "Closure-dominant loss",
      total_change < 0 & loss_sum > 0 & network_loss >= 0.6 * loss_sum ~ "Network-dominant loss",
      total_change < 0 & service_loss > 0 & network_loss > 0 ~ "Mixed loss",
      total_change < 0 ~ "Offset/other loss",
      TRUE ~ "No net loss"
    ),
    quadrant = case_when(
      service_effect < 0 & network_effect < 0 ~ "Both reduce access",
      service_effect < 0 & network_effect >= 0 ~ "Closures reduce, network buffers",
      service_effect >= 0 & network_effect < 0 ~ "Network reduces, service buffers",
      TRUE ~ "Both buffer/improve"
    ),
    mode_label = case_match(
      mode,
      "car" ~ "Car (10 min)",
      "transit" ~ "Transit (30 min)"
    ),
    label_flag = case_when(
      mode == "transit" &
        (rank(network_effect, ties.method = "first") <= 6 |
           rank(service_effect, ties.method = "first") <= 6) ~ TRUE,
      mode == "car" &
        (rank(network_effect, ties.method = "first") <= 4 |
           rank(service_effect, ties.method = "first") <= 4) ~ TRUE,
      TRUE ~ FALSE
    )
  )

dominant_levels <- c(
  "Closure-dominant loss",
  "Network-dominant loss",
  "Mixed loss",
  "Offset/other loss",
  "No net loss"
)

dominant_colors <- c(
  "Closure-dominant loss" = "#C0392B",
  "Network-dominant loss" = "#2878B5",
  "Mixed loss" = "#7B4EA3",
  "Offset/other loss" = "#8A8F98",
  "No net loss" = "#2E8B57"
)

decomp <- decomp |>
  mutate(dominant_component = factor(dominant_component, dominant_levels))

write_csv(
  decomp |>
    count(mode, dominant_component, quadrant, name = "n_districts") |>
    arrange(mode, dominant_component, quadrant),
  path(OUTPUT_DIR, "decomp_typology_counts.csv")
)

# === Figure 1: rule-based quadrant typology ==================================

fig_quadrant <- ggplot(
  decomp,
  aes(x = service_effect, y = network_effect)
) +
  annotate("rect", xmin = -Inf, xmax = 0, ymin = -Inf, ymax = 0,
           fill = "#F5F1FA", alpha = 0.55) +
  annotate("rect", xmin = -Inf, xmax = 0, ymin = 0, ymax = Inf,
           fill = "#FCEDEA", alpha = 0.55) +
  annotate("rect", xmin = 0, xmax = Inf, ymin = -Inf, ymax = 0,
           fill = "#EAF3FB", alpha = 0.55) +
  annotate("rect", xmin = 0, xmax = Inf, ymin = 0, ymax = Inf,
           fill = "#EDF7EF", alpha = 0.55) +
  geom_hline(yintercept = 0, color = "gray35", linewidth = 0.45) +
  geom_vline(xintercept = 0, color = "gray35", linewidth = 0.45) +
  geom_point(
    aes(size = total_pop, color = dominant_component),
    alpha = 0.72
  ) +
  geom_text(
    data = \(d) filter(d, label_flag),
    aes(label = sgg_nm),
    size = 2.6,
    check_overlap = TRUE,
    vjust = -0.75,
    color = "gray15"
  ) +
  facet_wrap(~ mode_label, scales = "free") +
  scale_color_manual(values = dominant_colors, drop = FALSE) +
  scale_size_continuous(
    range = c(1.4, 7),
    breaks = c(1e5, 5e5, 1e6, 5e6),
    labels = label_number(scale_cut = cut_short_scale())
  ) +
  labs(
    x = "Service effect: holding network fixed",
    y = "Network effect: holding bank locations fixed",
    color = NULL,
    size = "Population"
  ) +
  theme_bw(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold"),
    legend.position = "bottom",
    legend.box = "vertical",
    legend.margin = margin(t = -2),
    plot.margin = margin(8, 8, 8, 8)
  )

ggsave(
  path(FIGURE_DIR, "fig_decomp_quadrant_typology.png"),
  fig_quadrant,
  width = 8.5, height = 5.5, dpi = 300, bg = "white"
)

# === K-means clustering on service/network components ========================

kmeans_by_mode <- decomp |>
  group_by(mode) |>
  group_modify(\(df, key) {
    x <- df |>
      select(service_effect, network_effect) |>
      scale()
    km <- kmeans(x, centers = 4, nstart = 100)
    df |>
      mutate(kmeans_cluster = paste0("Cluster ", km$cluster))
  }) |>
  ungroup()

cluster_summary <- kmeans_by_mode |>
  summarize(
    n_districts = n(),
    service_effect = weighted.mean(service_effect, total_pop, na.rm = TRUE),
    network_effect = weighted.mean(network_effect, total_pop, na.rm = TRUE),
    total_change = weighted.mean(total_change, total_pop, na.rm = TRUE),
    pct_elderly = weighted.mean(pct_elderly, total_pop, na.rm = TRUE),
    pop_density_log = weighted.mean(pop_density_log, total_pop, na.rm = TRUE),
    total_pop = sum(total_pop, na.rm = TRUE),
    .by = c(mode, kmeans_cluster)
  ) |>
  arrange(mode, kmeans_cluster)

write_csv(kmeans_by_mode, path(OUTPUT_DIR, "decomp_kmeans_sgg.csv"))
write_csv(cluster_summary, path(OUTPUT_DIR, "decomp_kmeans_summary.csv"))

centers <- cluster_summary |>
  mutate(
    mode_label = case_match(
      mode,
      "car" ~ "Car (10 min)",
      "transit" ~ "Transit (30 min)"
    )
  )

fig_kmeans <- ggplot(
  kmeans_by_mode |>
    mutate(mode_label = case_match(
      mode,
      "car" ~ "Car (10 min)",
      "transit" ~ "Transit (30 min)"
    )),
  aes(x = service_effect, y = network_effect)
) +
  geom_hline(yintercept = 0, color = "gray35", linewidth = 0.45) +
  geom_vline(xintercept = 0, color = "gray35", linewidth = 0.45) +
  geom_point(aes(size = total_pop, color = kmeans_cluster), alpha = 0.68) +
  geom_point(
    data = centers,
    aes(color = kmeans_cluster),
    size = 5.5,
    shape = 4,
    stroke = 1.3
  ) +
  geom_text(
    data = centers,
    aes(label = kmeans_cluster),
    size = 3,
    vjust = -1.1,
    fontface = "bold"
  ) +
  facet_wrap(~ mode_label, scales = "free") +
  scale_size_continuous(
    range = c(1.4, 7),
    breaks = c(1e5, 5e5, 1e6, 5e6),
    labels = label_number(scale_cut = cut_short_scale())
  ) +
  labs(
    x = "Service effect: holding network fixed",
    y = "Network effect: holding bank locations fixed",
    color = NULL,
    size = "Population"
  ) +
  theme_bw(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold"),
    legend.position = "bottom",
    legend.box = "vertical",
    legend.margin = margin(t = -2),
    plot.margin = margin(8, 8, 8, 8)
  )

ggsave(
  path(FIGURE_DIR, "fig_decomp_kmeans_clusters.png"),
  fig_kmeans,
  width = 8.5, height = 5.5, dpi = 300, bg = "white"
)

message("\nSaved:")
message("  workflows/analysis/figures/fig_decomp_quadrant_typology.png")
message("  workflows/analysis/figures/fig_decomp_kmeans_clusters.png")
message("  workflows/analysis/decomp_typology_counts.csv")
message("  workflows/analysis/decomp_kmeans_sgg.csv")
message("  workflows/analysis/decomp_kmeans_summary.csv")
message("\n", strrep("=", 70))
message("DONE")
message(strrep("=", 70))
