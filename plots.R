library(tidyverse)
library(ggrepel)
library(patchwork)

rapm        <- read_csv("datasets/rapm.csv")
four_factor <- read_csv("datasets/4factor_rapm.csv")
usage_rapm  <- read_csv("datasets/usage_rapm.csv")

dir.create("plots", showWarnings = FALSE)

MIN_POSS <- 1000

p1 <- rapm %>%
  filter(poss >= MIN_POSS) %>%
  mutate(
    quadrant = case_when(
      orapm >= 0 & drapm >= 0 ~ "Two-Way (+/+)",
      orapm >= 0 & drapm <  0 ~ "Offensive Specialist",
      orapm <  0 & drapm >= 0 ~ "Defensive Specialist",
      TRUE                    ~ "Below Average"
    ),
    label = if_else(abs(rapm) >= 3.5 | drapm > 3 | orapm > 4, player, NA_character_)
  ) %>%
  ggplot(aes(orapm, drapm, color = quadrant, size = poss)) +
  annotate("rect", xmin = 0, xmax = Inf,  ymin = 0,    ymax = Inf,
           fill = "#1a9641", alpha = 0.04) +
  annotate("rect", xmin = -Inf, xmax = 0, ymin = -Inf, ymax = 0,
           fill = "#d7191c", alpha = 0.04) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.6) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.6) +
  geom_point(alpha = 0.65) +
  geom_label_repel(
    aes(label = label), size = 2.7, max.overlaps = 25,
    show.legend = FALSE, fill = alpha("white", 0.8),
    label.padding = 0.15, box.padding = 0.3
  ) +
  scale_color_manual(
    values = c(
      "Two-Way (+/+)"        = "#1a9641",
      "Offensive Specialist" = "#2b83ba",
      "Defensive Specialist" = "#d7191c",
      "Below Average"        = "gray60"
    )
  ) +
  scale_size_continuous(range = c(1.5, 5), guide = "none") +
  labs(
    title    = "Offensive and Defensive RAPM (2023–25)",
    subtitle = "Point size = possessions played · Players with ≥ 1,000 possessions",
    x        = "Offensive RAPM  (pts / 100 poss, higher = better)",
    y        = "Defensive RAPM  (pts / 100 poss, higher = better)",
    color    = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"), legend.position = "top")

top20 <- rapm %>%
  filter(poss >= MIN_POSS) %>%
  slice_max(rapm, n = 20) %>%
  mutate(player = fct_reorder(player, rapm))

p2 <- top20 %>%
  pivot_longer(cols = c(orapm, drapm), names_to = "component", values_to = "value") %>%
  mutate(component = recode(component, orapm = "Offense", drapm = "Defense")) %>%
  ggplot(aes(value, player, fill = component)) +
  geom_col(position = "stack", width = 0.65, alpha = 0.7) +
  geom_col(
    data = top20,
    aes(x = rapm, y = player),
    inherit.aes = FALSE, fill = "transparent", color = "black",
    linewidth = 0.6, width = 0.65
  ) +
  geom_text(
    data = top20,
    aes(x = pmax(rapm, drapm) + 0.35, y = player, label = sprintf("%.1f", rapm)),
    inherit.aes = FALSE, size = 3, hjust = 0, color = "gray25"
  ) +
  scale_fill_manual(values = c(Offense = "#2b83ba", Defense = "#d7191c")) +
  scale_x_continuous(expand = expansion(mult = c(0.04, 0.12))) +
  labs(
    title    = "RAPM Top 20 (2023–25)",
    subtitle = "Black outline = total RAPM · Filled bars show the offensive and defensive share",
    x        = "RAPM component (pts / 100 poss)",
    y        = NULL,
    fill     = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"), legend.position = "top")

top20_off_names <- rapm %>%
  filter(poss >= MIN_POSS) %>%
  slice_max(orapm, n = 20) %>%
  arrange(desc(orapm)) %>%
  pull(player)

top20_def_names <- rapm %>%
  filter(poss >= MIN_POSS) %>%
  slice_max(drapm, n = 20) %>%
  arrange(desc(drapm)) %>%
  pull(player)

make_ff_panel <- function(player_names, side, title_str) {
  if (side == "offense") {
    ff_data <- four_factor %>%
      filter(player %in% player_names) %>%
      transmute(
        player,
        `eFG%`      = oefg_rapm,
        `TOV Avoid` = -otov_rapm,   # negated: higher = fewer turnovers
        `Off Reb`   = oorb_rapm,
        `FT Rate`   = oftr_rapm
      )
    factor_levels <- c("eFG%", "TOV Avoid", "Off Reb", "FT Rate")
  } else {
    ff_data <- four_factor %>%
      filter(player %in% player_names) %>%
      transmute(
        player,
        `eFG Supp`   = defg_rapm,    # higher = better (suppresses opponent eFG)
        `TOV Forced` = -dtov_rapm,   # negated: higher = forces more turnovers = better
        `Stop OReb`  = dorb_rapm,    # higher = better (prevents opponent ORBs)
        `Limit FTA`  = dftr_rapm     # higher = better (limits opponent FTAs)
      )
    factor_levels <- c("eFG Supp", "TOV Forced", "Stop OReb", "Limit FTA")
  }

  ff_data %>%
    pivot_longer(-player, names_to = "factor", values_to = "value") %>%
    mutate(
      player = factor(player, levels = rev(player_names)),
      factor = factor(factor, levels = factor_levels)
    ) %>%
    filter(!is.na(value)) %>%
    ggplot(aes(value, player, fill = value > 0)) +
    geom_col(width = 0.7, alpha = 0.85) +
    geom_vline(xintercept = 0, color = "gray30", linewidth = 0.4) +
    facet_wrap(~factor, scales = "free_x", nrow = 1) +
    scale_fill_manual(
      values = c(`TRUE` = "#2b83ba", `FALSE` = "#d7191c"),
      guide  = "none"
    ) +
    labs(
      title = title_str,
      x     = "pts / 100 poss  (positive = helps team)",
      y     = NULL
    ) +
    theme_minimal(base_size = 10.5) +
    theme(
      plot.title    = element_text(face = "bold", size = 11),
      strip.text    = element_text(face = "bold"),
      panel.spacing = unit(1.2, "lines")
    )
}

p3_off <- make_ff_panel(
  top20_off_names, "offense",
  "Offensive Four-Factor — Top 20 by Offensive RAPM"
)
p3_def <- make_ff_panel(
  top20_def_names, "defense",
  "Defensive Four-Factor — Top 20 by Defensive RAPM"
)

p3 <- (p3_off / p3_def) +
  plot_annotation(
    title   = "Four-Factor RAPM Breakdown (2023–25)",
    caption = "Blue = positive contribution · Red = negative · All axes: positive = helps team · Negated columns: TOV Avoid (offense) and TOV Forced (defense)",
    theme   = theme(
      plot.title   = element_text(face = "bold", size = 13),
      plot.caption = element_text(size = 9, color = "gray45")
    )
  )

featured_players <- c(
  "Nikola Jokic", "Nikola Jokić", "Shai Gilgeous-Alexander", "Giannis Antetokounmpo", "Jayson Tatum",
  "Draymond Green", "Herbert Jones", "Isaiah Hartenstein", "Dorian Finney-Smith"
)

p4_data <- usage_rapm %>%
  filter(player %in% featured_players) %>%
  select(player, o1_rapm, o2_rapm, o3_rapm, o1_poss, o2_poss, o3_poss) %>%
  pivot_longer(
    -player,
    names_to  = c("tier_key", ".value"),
    names_sep = "_(?=[^_]+$)"   # split at last underscore: "o1_rapm" → "o1" + "rapm"
  ) %>%
  mutate(
    tier = factor(
      tier_key,
      levels = c("o1", "o2", "o3"),
      labels = c("Primary\n(High Usage)", "Secondary\n(Mid Usage)", "Tertiary\n(Low Usage)")
    ),
    archetype = if_else(
      player %in% c("Nikola Jokic", "Nikola Jokić", "Shai Gilgeous-Alexander",
                    "Giannis Antetokounmpo", "Jayson Tatum"),
      "Star", "Role Player"
    )
  )

p4 <- p4_data %>%
  ggplot(aes(tier, rapm, group = player, color = archetype)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray55", linewidth = 0.5) +
  geom_line(linewidth = 1.1, alpha = 0.85) +
  geom_point(aes(size = poss)) +
  scale_size_continuous(range = c(2, 7), guide = "none") +
  geom_label_repel(
    data        = p4_data %>% filter(tier == "Tertiary\n(Low Usage)"),
    aes(label   = player),
    size        = 2.7, nudge_x = 0.2,
    show.legend = FALSE, fill = alpha("white", 0.8),
    label.padding = 0.15, max.overlaps = 20
  ) +
  scale_color_manual(values = c(
    "Star"        = "#d7191c",
    "Role Player" = "#2b83ba"
  )) +
  labs(
    title    = "Offensive RAPM by lineup-level usage tier (2023–25)",
    subtitle = "Point size = possessions played in that tier",
    x        = NULL,
    y        = "Offensive RAPM (pts / 100 poss)",
    color    = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"), legend.position = "top")

ggsave("plots/p1_two_way_quadrant.png", p1, width = 10, height = 8,  dpi = 150)
ggsave("plots/p2_top20_stacked.png",    p2, width = 9,  height = 8,  dpi = 150)
ggsave("plots/p3_four_factor.png",      p3, width = 14, height = 16, dpi = 150)
ggsave("plots/p4_usage_tiers.png",      p4, width = 10, height = 7,  dpi = 150)

