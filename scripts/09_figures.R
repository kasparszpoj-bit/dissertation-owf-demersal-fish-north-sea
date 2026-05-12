################################################################################
# 09_figures.R
#
# PURPOSE:
#   Build the three figures used in the dissertation Results section.
#
#     Figure 1 — Raw patterns: CMS and richness across OWF distance
#     Figure 2 — RQ1: covariate-adjusted linear model coefficients
#     Figure 3 — RQ2: GAM smooths
#
#   This script re-fits the linear models and GAMs internally so it is
#   self-contained — running 09 alone (after 06, 07, 08 have run once)
#   reproduces every figure.
#
# INPUTS:
#   - data/processed/haul_metrics_owf_with_covariates.csv     (full dataset)
#   - data/processed/haul_metrics_with_covariates_clean.csv   (≤50 km subset)
#   - outputs/tables/GAM_summary_table.csv                    (for annotations)
#
# OUTPUTS:
#   - outputs/figures/Fig1_raw_patterns.png
#   - outputs/figures/Fig2_linear_coefficients.png
#   - outputs/figures/Fig3_GAM_smooths.png
################################################################################

source(here::here("scripts", "00_setup.R"))

# ── 1. LOAD DATA ──────────────────────────────────────────────────────────────
haul_all <- read_csv(
  here("data", "processed", "haul_metrics_owf_with_covariates.csv"),
  show_col_types = FALSE
) %>%
  mutate(
    commercial_group = factor(commercial_group, levels = c("Low", "Medium", "High")),
    dist_band        = factor(dist_band, levels = dist_band_levels)
  )

haul_50 <- read_csv(
  here("data", "processed", "haul_metrics_with_covariates_clean.csv"),
  show_col_types = FALSE
) %>%
  mutate(
    commercial_group = factor(commercial_group, levels = c("Low", "Medium", "High")),
    dist_band        = factor(dist_band, levels = dist_band_levels),
    depth_sc         = scale(depth)[, 1],
    sbt_sc           = scale(sbt)[, 1]
  )

################################################################################
# ── FIGURE 1: RAW PATTERNS ────────────────────────────────────────────────────
#   A — CMS vs continuous distance (≤50 km), LOESS + linear
#   B — CMS by distance band, boxplot, faceted by group
#   C — Richness vs continuous distance (≤50 km), LOESS + linear
#   D — Richness by distance band, boxplot, faceted by group
################################################################################

# Panel A
pA <- haul_50 %>%
  filter(!is.na(CMS)) %>%
  ggplot(aes(x = owf_dist_km, y = CMS)) +
  geom_point(alpha = 0.15, size = 0.7, colour = "steelblue") +
  geom_smooth(method = "loess", span = 0.6,
              colour = "firebrick", fill = "firebrick",
              alpha = 0.2, linewidth = 0.9) +
  geom_smooth(method = "lm", colour = "grey40",
              linetype = "dashed", se = FALSE, linewidth = 0.7) +
  labs(title = "A",
       x = "Distance to nearest OWF (km)",
       y = "Community Mean Size (cm)")

# Panel B
pB <- haul_all %>%
  filter(!is.na(dist_band), !is.na(CMS)) %>%
  ggplot(aes(x = dist_band, y = CMS,
             fill = commercial_group, colour = commercial_group)) +
  geom_boxplot(alpha = 0.6, outlier.size = 0.4,
               outlier.alpha = 0.3, linewidth = 0.45) +
  facet_wrap(~ commercial_group, nrow = 1) +
  scale_fill_manual(values = group_colours) +
  scale_colour_manual(values = group_colours) +
  labs(title = "B",
       x = "Distance band from nearest OWF",
       y = "Community Mean Size (cm)") +
  theme(axis.text.x = element_text(angle = 35, hjust = 1, size = 8))

# Panel C
pC <- haul_50 %>%
  filter(!is.na(species_richness)) %>%
  ggplot(aes(x = owf_dist_km, y = species_richness)) +
  geom_point(alpha = 0.15, size = 0.7, colour = "steelblue") +
  geom_smooth(method = "loess", span = 0.6,
              colour = "firebrick", fill = "firebrick",
              alpha = 0.2, linewidth = 0.9) +
  geom_smooth(method = "lm", colour = "grey40",
              linetype = "dashed", se = FALSE, linewidth = 0.7) +
  labs(title = "C",
       x = "Distance to nearest OWF (km)",
       y = "Species richness (n species)")

# Panel D
pD <- haul_all %>%
  filter(!is.na(dist_band), !is.na(species_richness)) %>%
  ggplot(aes(x = dist_band, y = species_richness,
             fill = commercial_group, colour = commercial_group)) +
  geom_boxplot(alpha = 0.6, outlier.size = 0.4,
               outlier.alpha = 0.3, linewidth = 0.45) +
  facet_wrap(~ commercial_group, nrow = 1) +
  scale_fill_manual(values = group_colours) +
  scale_colour_manual(values = group_colours) +
  labs(title = "D",
       x = "Distance band from nearest OWF",
       y = "Species richness (n species)") +
  theme(axis.text.x = element_text(angle = 35, hjust = 1, size = 8))

fig1 <- (pA | pB) / (pC | pD) +
  plot_annotation(
    title   = "Figure 1. Raw patterns in demersal fish community metrics across OWF distance gradients",
    caption = paste0(
      "LOESS smooth (red) and linear fit (dashed grey) shown for continuous plots. ",
      "Boxes show median ± IQR; whiskers = 1.5×IQR.\n",
      "Greater North Sea · NE Atlantic Groundfish Survey · OWFs from EMODnet"
    ),
    theme = theme(
      plot.title   = element_text(face = "bold", size = 11),
      plot.caption = element_text(colour = "grey50", size = 8)
    )
  )

ggsave(
  here("outputs", "figures", "Fig1_raw_patterns.png"),
  plot = fig1, width = 13, height = 10, dpi = 300, bg = "white"
)
cat("Saved: outputs/figures/Fig1_raw_patterns.png\n")

################################################################################
# ── FIGURE 2: LINEAR MODEL COEFFICIENTS (RQ1) ─────────────────────────────────
################################################################################

# CMS coefficients
cms_lm <- haul_50 %>%
  filter(!is.na(CMS)) %>%
  group_by(commercial_group) %>%
  group_map(function(df, key) {
    mod <- lm(CMS ~ owf_dist_km + depth_sc + sbt_sc, data = df)
    s   <- summary(mod)
    tibble(
      commercial_group = key$commercial_group,
      slope            = coef(mod)[["owf_dist_km"]],
      se               = coef(s)["owf_dist_km", "Std. Error"],
      p_value          = coef(s)["owf_dist_km", "Pr(>|t|)"],
      response         = "Community Mean Size (cm)"
    )
  }) %>%
  bind_rows()

# Richness coefficients
rich_lm <- haul_50 %>%
  filter(!is.na(species_richness)) %>%
  group_by(commercial_group) %>%
  group_map(function(df, key) {
    mod <- lm(species_richness ~ owf_dist_km + depth_sc + sbt_sc, data = df)
    s   <- summary(mod)
    tibble(
      commercial_group = key$commercial_group,
      slope            = coef(mod)[["owf_dist_km"]],
      se               = coef(s)["owf_dist_km", "Std. Error"],
      p_value          = coef(s)["owf_dist_km", "Pr(>|t|)"],
      response         = "Species richness"
    )
  }) %>%
  bind_rows()

coef_dat <- bind_rows(cms_lm, rich_lm) %>%
  mutate(
    ci_lo            = slope - 1.96 * se,
    ci_hi            = slope + 1.96 * se,
    sig              = ifelse(p_value < 0.05, "p < 0.05", "p ≥ 0.05"),
    commercial_group = factor(commercial_group, levels = c("Low", "Medium", "High")),
    response         = factor(response,
                              levels = c("Community Mean Size (cm)", "Species richness"))
  )

pA2 <- coef_dat %>%
  filter(response == "Community Mean Size (cm)") %>%
  ggplot(aes(x = commercial_group, y = slope,
             ymin = ci_lo, ymax = ci_hi,
             colour = commercial_group, shape = sig)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey60") +
  geom_pointrange(size = 0.9, linewidth = 1) +
  scale_colour_manual(values = group_colours, guide = "none") +
  scale_shape_manual(values = c("p < 0.05" = 16, "p ≥ 0.05" = 1), name = NULL) +
  labs(title = "A  Community Mean Size",
       x = "Commercial importance group",
       y = "Slope: change in CMS per km from OWF (cm)") +
  theme(legend.position = "bottom")

pB2 <- coef_dat %>%
  filter(response == "Species richness") %>%
  ggplot(aes(x = commercial_group, y = slope,
             ymin = ci_lo, ymax = ci_hi,
             colour = commercial_group, shape = sig)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey60") +
  geom_pointrange(size = 0.9, linewidth = 1) +
  scale_colour_manual(values = group_colours, guide = "none") +
  scale_shape_manual(values = c("p < 0.05" = 16, "p ≥ 0.05" = 1), name = NULL) +
  labs(title = "B  Species richness",
       x = "Commercial importance group",
       y = "Slope: change in richness per km from OWF") +
  theme(legend.position = "bottom")

fig2 <- (pA2 | pB2) +
  plot_annotation(
    title   = "Figure 2. Covariate-adjusted effects of OWF distance on community metrics by commercial importance group",
    caption = paste0(
      "Linear models: response ~ owf_dist_km + depth + SBT  ·  ",
      "Hauls within 50 km  ·  Points show slope ± 95% CI\n",
      "Filled circles = p < 0.05; open circles = p ≥ 0.05"
    ),
    theme = theme(
      plot.title   = element_text(face = "bold", size = 11),
      plot.caption = element_text(colour = "grey50", size = 8)
    )
  )

ggsave(
  here("outputs", "figures", "Fig2_linear_coefficients.png"),
  plot = fig2, width = 10, height = 5, dpi = 300, bg = "white"
)
cat("Saved: outputs/figures/Fig2_linear_coefficients.png\n")

################################################################################
# ── FIGURE 3: GAM SMOOTHS (RQ2) ───────────────────────────────────────────────
################################################################################

# Re-fit GAMs
cms_gams <- haul_50 %>%
  filter(!is.na(CMS)) %>%
  group_by(commercial_group) %>%
  group_map(function(df, key) {
    mod <- gam(CMS ~ s(owf_dist_km, k = 5) + depth_sc + sbt_sc,
               data = df, method = "REML")
    list(model = mod, group = as.character(key$commercial_group), data = df)
  })
names(cms_gams) <- c("Low", "Medium", "High")

richness_gams <- haul_50 %>%
  filter(!is.na(species_richness)) %>%
  group_by(commercial_group) %>%
  group_map(function(df, key) {
    mod <- gam(species_richness ~ s(owf_dist_km, k = 5) + depth_sc + sbt_sc,
               data = df, method = "REML")
    list(model = mod, group = as.character(key$commercial_group), data = df)
  })
names(richness_gams) <- c("Low", "Medium", "High")

# Helper: extract the smooth on the response scale.
# Handles different gratia versions (column names changed between releases).
get_smooth <- function(gam_obj, group_name) {
  sm      <- smooth_estimates(gam_obj$model, smooth = "s(owf_dist_km)")
  est_col <- intersect(names(sm), c(".estimate", "est", "smooth"))[1]
  se_col  <- intersect(names(sm), c(".se", "se", "std.error"))[1]
  x_col   <- "owf_dist_km"
  sm %>%
    mutate(
      owf_dist_km      = .data[[x_col]],
      estimate         = .data[[est_col]],
      lower_ci         = .data[[est_col]] - 1.96 * .data[[se_col]],
      upper_ci         = .data[[est_col]] + 1.96 * .data[[se_col]],
      commercial_group = group_name
    ) %>%
    select(owf_dist_km, estimate, lower_ci, upper_ci, commercial_group)
}

# Load GAM summary for annotation labels
gam_results <- read_csv(
  here("outputs", "tables", "GAM_summary_table.csv"),
  show_col_types = FALSE
)

make_gam_label <- function(results_df, response_name, group_name) {
  row <- results_df %>%
    filter(response == response_name, commercial_group == group_name)
  paste0(
    "edf = ", round(row$edf, 2),
    "\np ", ifelse(row$smooth_p < 0.001, "< 0.001",
                   paste0("= ", round(row$smooth_p, 3)))
  )
}

# Panel builders
make_cms_panel <- function(group_name, panel_label) {
  sm  <- get_smooth(cms_gams[[group_name]], group_name)
  lbl <- make_gam_label(gam_results, "Community Mean Size (cm)", group_name)

  ggplot(sm, aes(x = owf_dist_km, y = estimate)) +
    geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci),
                fill = group_colours[[group_name]], alpha = 0.25) +
    geom_line(colour = group_colours[[group_name]], linewidth = 1) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
    annotate("text", x = 45, y = Inf, label = lbl,
             vjust = 1.4, hjust = 1, size = 3, colour = "grey30") +
    labs(title = paste0(panel_label, "  ", group_name),
         x = "Distance to nearest OWF (km)",
         y = "Partial effect on CMS (cm)")
}

make_rich_panel <- function(group_name, panel_label) {
  sm  <- get_smooth(richness_gams[[group_name]], group_name)
  lbl <- make_gam_label(gam_results, "Species richness", group_name)

  ggplot(sm, aes(x = owf_dist_km, y = estimate)) +
    geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci),
                fill = group_colours[[group_name]], alpha = 0.25) +
    geom_line(colour = group_colours[[group_name]], linewidth = 1) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
    annotate("text", x = 45, y = Inf, label = lbl,
             vjust = 1.4, hjust = 1, size = 3, colour = "grey30") +
    labs(title = paste0(panel_label, "  ", group_name),
         x = "Distance to nearest OWF (km)",
         y = "Partial effect on richness")
}

p_cms_low   <- make_cms_panel("Low",    "A")
p_cms_med   <- make_cms_panel("Medium", "B")
p_cms_high  <- make_cms_panel("High",   "C")
p_rich_low  <- make_rich_panel("Low",    "D")
p_rich_med  <- make_rich_panel("Medium", "E")
p_rich_high <- make_rich_panel("High",   "F")

fig3 <- (p_cms_low | p_cms_med | p_cms_high) /
        (p_rich_low | p_rich_med | p_rich_high) +
  plot_annotation(
    title   = "Figure 3. GAM partial effects of OWF distance on community metrics by commercial importance group",
    caption = paste0(
      "GAM: response ~ s(owf_dist_km, k=5) + depth + SBT  ·  Hauls within 50 km\n",
      "Shaded band = 95% CI  ·  Dashed line = zero effect  ·  ",
      "edf and p-value refer to the OWF distance smooth term"
    ),
    theme = theme(
      plot.title   = element_text(face = "bold", size = 11),
      plot.caption = element_text(colour = "grey50", size = 8)
    )
  )

ggsave(
  here("outputs", "figures", "Fig3_GAM_smooths.png"),
  plot = fig3, width = 13, height = 9, dpi = 300, bg = "white"
)
cat("Saved: outputs/figures/Fig3_GAM_smooths.png\n")

cat("\n── All figures saved ──\n")
cat("  outputs/figures/Fig1_raw_patterns.png\n")
cat("  outputs/figures/Fig2_linear_coefficients.png\n")
cat("  outputs/figures/Fig3_GAM_smooths.png\n")
