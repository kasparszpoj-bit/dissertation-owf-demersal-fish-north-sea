################################################################################
# exp_01_cms_loess_by_group.R
#
# PURPOSE (EXPLORATORY):
#   Quick descriptive look at CMS vs distance to OWF, by commercial importance
#   group. Scales the x-axis with a log1p transform to spread out the
#   short-distance hauls where most of the action is.
#
#   Superseded by Fig. 1 in scripts/09_figures.R, which is the version used
#   in the dissertation. Kept here to document the exploratory step.
#
# INPUTS:
#   - data/processed/groundfish_data_with_commercial_groups.csv
#   - data/processed/gns_hauls_dist_to_owf.csv
#
# OUTPUT:
#   - outputs/figures/exploratory/exp_cms_loess_by_group.png
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

# ── 2. CMS PER HAUL × GROUP ───────────────────────────────────────────────────
cms_by_group <- groundfish_data %>%
  filter(!is.na(commercial_group)) %>%
  mutate(length_x_biomass = fish_length_cm * dens_biom_kg_sqkm) %>%
  group_by(haul_id, year, shoot_long_degdec, shoot_lat_degdec, commercial_group) %>%
  summarise(
    cms = sum(length_x_biomass, na.rm = TRUE) /
          sum(dens_biom_kg_sqkm, na.rm = TRUE),
    .groups = "drop"
  )

# ── 3. JOIN DISTANCE ──────────────────────────────────────────────────────────
cms_dist <- cms_by_group %>%
  left_join(gns_hauls_dist_to_owf, by = "haul_id") %>%
  filter(!is.na(owf_dist_km))

# ── 4. PLOT SETTINGS ──────────────────────────────────────────────────────────
exp_colours <- c("High" = "red", "Low" = "green", "Medium" = "blue")

x_breaks <- c(0, 1, 5, 10, 20, 50, 100, 200, 300)
x_labels <- as.character(x_breaks)
x_limits <- c(0, 300)
y_limits <- c(0, 110)

# ── 5. PANELS ─────────────────────────────────────────────────────────────────
p_all_groups <- cms_dist %>%
  filter(owf_dist_km <= 300) %>%
  ggplot(aes(x = owf_dist_km, y = cms, colour = commercial_group)) +
  geom_point(alpha = 0.25) +
  geom_smooth(method = "loess", se = TRUE) +
  scale_colour_manual(values = exp_colours, breaks = c("High", "Low", "Medium")) +
  scale_x_continuous(trans = "log1p",
                     breaks = x_breaks, labels = x_labels, limits = x_limits) +
  coord_cartesian(ylim = y_limits) +
  labs(title = "CMS vs distance to nearest OWF by commercial importance",
       x = "Distance to nearest OWF (km)",
       y = "Community Mean Size (CMS)",
       colour = "Commercial importance") +
  theme_minimal()

panel_for_group <- function(grp) {
  cms_dist %>%
    filter(commercial_group == grp, owf_dist_km <= 300) %>%
    ggplot(aes(x = owf_dist_km, y = cms)) +
    geom_point(alpha = 0.25, colour = exp_colours[[grp]]) +
    geom_smooth(method = "loess", se = TRUE, colour = "black") +
    scale_x_continuous(trans = "log1p",
                       breaks = x_breaks, labels = x_labels, limits = x_limits) +
    coord_cartesian(ylim = y_limits) +
    labs(title = paste(grp, "commercial importance"),
         x = "Distance to nearest OWF (km)",
         y = "Community Mean Size (CMS)") +
    theme_minimal()
}

p_low    <- panel_for_group("Low")
p_medium <- panel_for_group("Medium")
p_high   <- panel_for_group("High")

# ── 6. ASSEMBLE AND SAVE ──────────────────────────────────────────────────────
fig <- (p_all_groups | p_low) / (p_medium | p_high)

ggsave(
  here("outputs", "figures", "exploratory", "exp_cms_loess_by_group.png"),
  plot = fig, width = 12, height = 9, dpi = 300, bg = "white"
)
cat("Saved: outputs/figures/exploratory/exp_cms_loess_by_group.png\n")
