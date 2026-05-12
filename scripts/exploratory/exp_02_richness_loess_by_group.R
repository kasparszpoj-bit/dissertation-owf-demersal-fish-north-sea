################################################################################
# exp_02_richness_loess_by_group.R
#
# PURPOSE (EXPLORATORY):
#   Descriptive plots of species richness vs distance to OWF, by commercial
#   importance group. Same structure as exp_01 but for the richness response.
#
#   Superseded by Fig. 1 in scripts/09_figures.R.
#
# INPUTS:
#   - data/processed/groundfish_data_with_commercial_groups.csv
#   - data/processed/gns_hauls_dist_to_owf.csv
#
# OUTPUT:
#   - outputs/figures/exploratory/exp_richness_loess_by_group.png
################################################################################

source(here::here("scripts", "00_setup.R"))

# ── 1. LOAD ───────────────────────────────────────────────────────────────────
groundfish_data <- read_csv(
  here("data", "processed", "groundfish_data_with_commercial_groups.csv"),
  show_col_types = FALSE
) %>%
  clean_names()

gns_hauls_dist_to_owf <- read_csv(
  here("data", "processed", "gns_hauls_dist_to_owf.csv"),
  show_col_types = FALSE
) %>%
  clean_names()

# ── 2. RICHNESS PER HAUL × GROUP ──────────────────────────────────────────────
richness_by_group <- groundfish_data %>%
  filter(!is.na(commercial_group)) %>%
  group_by(haul_id, year, shoot_long_degdec, shoot_lat_degdec, commercial_group) %>%
  summarise(species_richness = n_distinct(sci_name), .groups = "drop")

# ── 3. JOIN DISTANCE ──────────────────────────────────────────────────────────
richness_dist <- richness_by_group %>%
  left_join(gns_hauls_dist_to_owf, by = "haul_id") %>%
  filter(!is.na(owf_dist_km))

# ── 4. PLOT SETTINGS ──────────────────────────────────────────────────────────
exp_colours <- c("High" = "red", "Low" = "green", "Medium" = "blue")

x_breaks <- c(0, 1, 5, 10, 20, 50, 100, 200, 300)
x_labels <- as.character(x_breaks)
x_limits <- c(0, 300)
y_limits <- range(richness_dist$species_richness, na.rm = TRUE)

# ── 5. PANELS ─────────────────────────────────────────────────────────────────
p_all_groups <- richness_dist %>%
  filter(owf_dist_km <= 300) %>%
  ggplot(aes(x = owf_dist_km, y = species_richness, colour = commercial_group)) +
  geom_point(alpha = 0.25) +
  geom_smooth(method = "loess", se = TRUE) +
  scale_colour_manual(values = exp_colours, breaks = c("High", "Low", "Medium")) +
  scale_x_continuous(trans = "log1p",
                     breaks = x_breaks, labels = x_labels, limits = x_limits) +
  coord_cartesian(ylim = y_limits) +
  labs(title = "Species richness vs distance to nearest OWF by commercial importance",
       x = "Distance to nearest OWF (km)",
       y = "Species richness",
       colour = "Commercial importance") +
  theme_minimal()

panel_for_group <- function(grp) {
  richness_dist %>%
    filter(commercial_group == grp, owf_dist_km <= 300) %>%
    ggplot(aes(x = owf_dist_km, y = species_richness)) +
    geom_point(alpha = 0.25, colour = exp_colours[[grp]]) +
    geom_smooth(method = "loess", se = TRUE, colour = "black") +
    scale_x_continuous(trans = "log1p",
                       breaks = x_breaks, labels = x_labels, limits = x_limits) +
    coord_cartesian(ylim = y_limits) +
    labs(title = paste(grp, "commercial importance"),
         x = "Distance to nearest OWF (km)",
         y = "Species richness") +
    theme_minimal()
}

p_low    <- panel_for_group("Low")
p_medium <- panel_for_group("Medium")
p_high   <- panel_for_group("High")

# ── 6. ASSEMBLE AND SAVE ──────────────────────────────────────────────────────
fig <- (p_all_groups | p_low) / (p_medium | p_high)

ggsave(
  here("outputs", "figures", "exploratory", "exp_richness_loess_by_group.png"),
  plot = fig, width = 12, height = 9, dpi = 300, bg = "white"
)
cat("Saved: outputs/figures/exploratory/exp_richness_loess_by_group.png\n")
