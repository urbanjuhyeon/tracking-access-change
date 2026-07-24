# 7-1-manuscript-figures.R
# Manuscript figures 1, 2, 3, 5, 6 (Fig 4 = existing quadrant typology, copied).
# House style follows 6-4-decompose-typology-figures.R:
#   theme_bw(base_size = 11), bold strips, bottom legend, 300 dpi, white bg.

pacman::p_load(tidyverse, sf, fs, glue, scales, patchwork)

PROJECT_ROOT <- normalizePath(
  Sys.getenv("ACCESS_DECLINE_ROOT", unset = "."),
  winslash = "/", mustWork = TRUE
)
if (!file.exists(file.path(PROJECT_ROOT, ".gitignore"))) {
  stop("Run from the repository root or set ACCESS_DECLINE_ROOT.")
}
ANALYSIS_DIR <- path(PROJECT_ROOT, "workflows/analysis")
FIG_DIR      <- path(PROJECT_ROOT, "manuscript/figures")
dir_create(FIG_DIR)

component_colors <- c(
  "Total"       = "#4D4D4D",
  "Service"     = "#C0392B",
  "Network"     = "#2878B5",
  "Interaction" = "#8A8F98"
)

dominant_colors <- c(
  "Closure-dominant loss" = "#C0392B",
  "Network-dominant loss" = "#2878B5",
  "Mixed loss"            = "#7B4EA3",
  "Offset/other loss"     = "#8A8F98",
  "No net loss"           = "#2E8B57"
)

mode_labels <- c(car = "Car (10 min)", transit = "Transit (30 min)")

theme_house <- function(base_size = 11) {
  theme_bw(base_size = base_size) +
    theme(
      panel.grid.minor = element_blank(),
      strip.text = element_text(face = "bold"),
      legend.position = "bottom",
      plot.margin = margin(8, 8, 8, 8)
    )
}

national <- read_csv(path(ANALYSIS_DIR, "decomp_national.csv"), show_col_types = FALSE)

message("[Fig 1] framework schematic")

tr <- national |> filter(mode == "transit", base_year == 2021, target_year == 2023)
v <- list(
  a = sprintf("%.1f", tr$actual_base), b = sprintf("%.1f", tr$cfa),
  c = sprintf("%.1f", tr$cfb),         d = sprintf("%.1f", tr$actual_target),
  svc = sprintf("%+.1f", tr$service_effect), net = sprintf("%+.1f", tr$network_effect),
  tot = sprintf("%+.1f", tr$total_change),   int = sprintf("%+.1f", tr$interaction)
)

box <- function(xmin, xmax, ymin, ymax, fill, color, linetype = "solid") {
  annotate("rect", xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
           fill = fill, color = color, linetype = linetype, linewidth = 0.6)
}
btxt <- function(x, y, title, formula, value) {
  list(
    annotate("text", x = x, y = y + 0.62, label = title, fontface = "bold", size = 3.6),
    annotate("text", x = x, y = y, label = formula, size = 3.1, color = "gray25"),
    annotate("text", x = x, y = y - 0.62, label = value, size = 3.6, color = "gray10")
  )
}

fig1 <- ggplot() +
  xlim(0, 10) + ylim(0.6, 10.4) +
  annotate("text", x = 2.3, y = 9.95, label = "Banks as of 2021",
           fontface = "bold", size = 3.7, color = "gray20") +
  annotate("text", x = 7.7, y = 9.95, label = "Banks as of 2023",
           fontface = "bold", size = 3.7, color = "gray20") +
  annotate("text", x = 0.12, y = 7.75, label = "Network 2021", angle = 90,
           fontface = "bold", size = 3.7, color = "gray20") +
  annotate("text", x = 0.12, y = 2.85, label = "Network 2023", angle = 90,
           fontface = "bold", size = 3.7, color = "gray20") +
  box(0.6, 4.0, 6.6, 8.9, "white", "gray25") +
  btxt(2.3, 7.75, "Observed 2021", "f(N[2021], S[2021])", glue("{v$a} banks")) +
  box(6.0, 9.4, 6.6, 8.9, "#FCEDEA", "#C0392B", "dashed") +
  btxt(7.7, 7.75, "Counterfactual A", "f(N[2021], S[2023])", glue("{v$b} banks")) +
  box(0.6, 4.0, 1.7, 4.0, "#EAF3FB", "#2878B5", "dashed") +
  btxt(2.3, 2.85, "Counterfactual B", "f(N[2023], S[2021])", glue("{v$c} banks")) +
  box(6.0, 9.4, 1.7, 4.0, "white", "gray25") +
  btxt(7.7, 2.85, "Observed 2023", "f(N[2023], S[2023])", glue("{v$d} banks")) +
  annotate("segment", x = 4.15, xend = 5.85, y = 7.75, yend = 7.75,
           color = "#C0392B", linewidth = 0.9,
           arrow = arrow(length = unit(0.16, "cm"), type = "closed")) +
  annotate("text", x = 5.0, y = 8.25, label = glue("Service effect {v$svc}"),
           color = "#C0392B", fontface = "bold", size = 3.4) +
  annotate("text", x = 5.0, y = 7.35, label = "banks change,\nnetwork held", size = 2.8, color = "gray35") +
  annotate("segment", x = 2.3, xend = 2.3, y = 6.45, yend = 4.15,
           color = "#2878B5", linewidth = 0.9,
           arrow = arrow(length = unit(0.16, "cm"), type = "closed")) +
  annotate("text", x = 2.48, y = 5.3, label = glue("Network effect {v$net}"),
           color = "#2878B5", fontface = "bold", size = 3.4, hjust = 0) +
  annotate("text", x = 2.48, y = 4.75, label = "network changes,\nbanks held", size = 2.8,
           color = "gray35", hjust = 0) +
  annotate("segment", x = 4.15, xend = 5.85, y = 6.45, yend = 4.15,
           color = "gray25", linewidth = 0.9,
           arrow = arrow(length = unit(0.16, "cm"), type = "closed")) +
  annotate("text", x = 5.55, y = 5.5, label = glue("Total {v$tot}"),
           color = "gray15", fontface = "bold", size = 3.4, hjust = 0) +
  annotate("text", x = 5.0, y = 1.05,
           label = glue("Interaction = Total − Service − Network = {v$int}    ·    values: transit, banks reachable within 30 min, population-weighted"),
           size = 2.9, color = "gray35") +
  theme_void()

ggsave(path(FIG_DIR, "fig1_framework.png"), fig1,
       width = 7.6, height = 5.0, dpi = 300, bg = "white")

message("[Fig 2] national decomposition, 2021 -> 2023")

fig2_data <- national |>
  filter(base_year == 2021, target_year == 2023) |>
  transmute(
    mode_label = mode_labels[mode],
    Total = total_change, Service = service_effect,
    Network = network_effect, Interaction = interaction
  ) |>
  pivot_longer(-mode_label, names_to = "component", values_to = "value") |>
  mutate(component = factor(component, names(component_colors)))

fig2 <- ggplot(fig2_data, aes(component, value, fill = component)) +
  geom_col(width = 0.62) +
  geom_hline(yintercept = 0, color = "gray35", linewidth = 0.45) +
  geom_text(aes(label = sprintf("%+.1f", value),
                vjust = if_else(value < 0, 1.35, -0.5)),
            size = 3.3, fontface = "bold", color = "gray15") +
  facet_wrap(~ mode_label) +
  scale_fill_manual(values = component_colors, guide = "none") +
  scale_y_continuous(expand = expansion(mult = c(0.18, 0.15))) +
  labs(x = NULL, y = "Change in reachable banks, 2021→2023") +
  theme_house()

ggsave(path(FIG_DIR, "fig2_decomp_national.png"), fig2,
       width = 8.5, height = 4.2, dpi = 300, bg = "white")

message("[Fig 3] chained year pairs")

fig3_data <- national |>
  filter((base_year == 2021 & target_year == 2022) |
         (base_year == 2022 & target_year == 2023)) |>
  mutate(pair = glue("{base_year}→{target_year}"),
         mode_label = mode_labels[mode]) |>
  select(mode_label, pair, Service = service_effect,
         Network = network_effect, Interaction = interaction,
         Total = total_change)

fig3_bars <- fig3_data |>
  pivot_longer(c(Service, Network, Interaction),
               names_to = "component", values_to = "value") |>
  mutate(component = factor(component, c("Service", "Network", "Interaction")))

fig3 <- ggplot(fig3_bars, aes(pair, value, fill = component)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.62) +
  geom_hline(yintercept = 0, color = "gray35", linewidth = 0.45) +
  geom_point(data = fig3_data, aes(pair, Total),
             inherit.aes = FALSE, shape = 18, size = 3.2, color = "gray10") +
  geom_line(data = fig3_data, aes(pair, Total, group = 1),
            inherit.aes = FALSE, linewidth = 0.5, color = "gray10", linetype = "42") +
  geom_text(aes(label = sprintf("%+.1f", value),
                vjust = if_else(value < 0, 1.4, -0.55)),
            position = position_dodge(width = 0.7),
            size = 2.9, color = "gray15") +
  facet_wrap(~ mode_label) +
  scale_fill_manual(values = component_colors[c("Service", "Network", "Interaction")]) +
  scale_y_continuous(expand = expansion(mult = c(0.2, 0.18))) +
  labs(x = NULL, y = "Change in reachable banks",
       fill = NULL,
       caption = "Diamonds: total observed change per pair.") +
  theme_house()

ggsave(path(FIG_DIR, "fig3_decomp_temporal.png"), fig3,
       width = 8.5, height = 4.4, dpi = 300, bg = "white")

message("[Fig 5] dominant-driver map")

sgg <- st_read(path(PROJECT_ROOT, "data/census/sgg_bnd.gpkg"), quiet = TRUE) |>
  select(sgg_cd = SIGUNGU_CD)

demo <- read_csv(path(ANALYSIS_DIR, "decomp_demo_sgg.csv"), show_col_types = FALSE) |>
  mutate(
    sgg_cd = sprintf("%05d", as.integer(sgg_cd)),
    service_loss = pmax(-service_effect, 0),
    network_loss = pmax(-network_effect, 0),
    loss_sum = service_loss + network_loss,
    dominant_component = case_when(
      total_change < 0 & loss_sum > 0 & service_loss >= 0.6 * loss_sum ~ "Closure-dominant loss",
      total_change < 0 & loss_sum > 0 & network_loss >= 0.6 * loss_sum ~ "Network-dominant loss",
      total_change < 0 & service_loss > 0 & network_loss > 0 ~ "Mixed loss",
      total_change < 0 ~ "Offset/other loss",
      TRUE ~ "No net loss"
    ) |> factor(names(dominant_colors)),
    mode_label = mode_labels[mode]
  )

map_data <- sgg |>
  left_join(demo, by = "sgg_cd") |>
  filter(!is.na(mode_label))

fig5 <- ggplot(map_data) +
  geom_sf(aes(fill = dominant_component), color = "white", linewidth = 0.08) +
  facet_wrap(~ mode_label) +
  scale_fill_manual(values = dominant_colors, drop = TRUE, name = NULL) +
  guides(fill = guide_legend(nrow = 2)) +
  theme_void(base_size = 11) +
  theme(
    strip.text = element_text(face = "bold", size = 11, margin = margin(b = 4)),
    legend.position = "bottom",
    plot.margin = margin(8, 8, 8, 8)
  )

ggsave(path(FIG_DIR, "fig5_typology_map.png"), fig5,
       width = 8.5, height = 5.4, dpi = 300, bg = "white")

message("[Fig 6] policy scenario")

pol_nat <- read_csv(path(ANALYSIS_DIR, "policy_pilot_lastbranch_national.csv"),
                    show_col_types = FALSE)

fig6a_data <- pol_nat |>
  transmute(
    mode_label = mode_labels[mode],
    `Observed loss` = abs(total_change),
    `Under last-branch rule` = abs(total_change) - averted_A,
    pct = pct_of_decline_A
  ) |>
  pivot_longer(c(`Observed loss`, `Under last-branch rule`),
               names_to = "scenario", values_to = "loss")

fig6a <- ggplot(fig6a_data, aes(mode_label, loss, fill = scenario)) +
  geom_col(position = position_dodge(width = 0.66), width = 0.6) +
  geom_text(aes(label = if_else(scenario == "Under last-branch rule",
                                sprintf("%.1f (−%.1f%%)", loss, pct),
                                sprintf("%.1f", loss))),
            position = position_dodge(width = 0.66),
            vjust = -0.5, size = 3.1, fontface = "bold", color = "gray15") +
  scale_fill_manual(values = c("Observed loss" = "#8A8F98",
                               "Under last-branch rule" = "#2E8B57"), name = NULL) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.14))) +
  labs(x = NULL, y = "Loss in reachable banks, 2021→2023") +
  theme_house()

pol_sgg <- read_csv(path(ANALYSIS_DIR, "policy_pilot_lastbranch_sgg.csv"),
                    show_col_types = FALSE) |>
  filter(mode == "transit") |>
  mutate(sgg_cd = sprintf("%05d", as.integer(sgg_cd)))

fig6b <- sgg |>
  left_join(pol_sgg, by = "sgg_cd") |>
  ggplot() +
  geom_sf(aes(fill = averted_A), color = "white", linewidth = 0.08) +
  scale_fill_gradient(low = "grey96", high = "#2E8B57", trans = "sqrt",
                      name = "Averted transit loss\n(banks, variant A)") +
  theme_void(base_size = 11) +
  theme(legend.position = "bottom",
        plot.margin = margin(8, 8, 8, 8))

fig6 <- fig6a + fig6b + plot_layout(widths = c(1, 1.15)) +
  plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")")

ggsave(path(FIG_DIR, "fig6_policy_lastbranch.png"), fig6,
       width = 9.2, height = 4.9, dpi = 300, bg = "white")

message("[Fig 4] copy existing quadrant typology")
file_copy(path(ANALYSIS_DIR, "figures/fig_decomp_quadrant_typology.png"),
          path(FIG_DIR, "fig4_quadrant_typology.png"), overwrite = TRUE)

message("\nSaved to manuscript/figures/:")
walk(dir_ls(FIG_DIR, glob = "*.png"), \(p) message("  ", path_file(p)))
message("DONE")
