################################################################################
# 07_linear_models.R
#
# PURPOSE:
#   Address RQ1 — does community structure vary linearly with distance to the
#   nearest OWF?
#
#   Fits covariate-adjusted linear models for two responses (CMS, species
#   richness), separately for each commercial-importance group, and compares
#   them against unadjusted models so the impact of covariate control is
#   transparent.
#
# INPUT:
#   - data/processed/haul_metrics_owf_with_covariates.csv   (from script 06)
#
# OUTPUTS:
#   - data/processed/haul_metrics_with_covariates_clean.csv (≤50 km subset)
#   - outputs/tables/linear_model_coefficients_CMS.csv
#   - outputs/tables/linear_model_coefficients_richness.csv
#   - outputs/tables/linear_model_unadjusted_vs_adjusted.csv
#   - outputs/figures/covariate_vs_owf_distance.png
#
# MODEL STRUCTURE:
#   response ~ owf_dist_km + depth_sc + sbt_sc + sst_trend_sc + dist_coast_sc
#
#   - owf_dist_km is left in raw km units so its slope is interpretable
#     ("change per km from nearest OWF").
#   - The four covariates are scaled to mean 0, sd 1 so their coefficients are
#     directly comparable across covariates.
#   - Models are fit separately per commercial group because body-size and
#     richness responses to environmental gradients are expected to differ.
#
# WHY ≤ 50 km?
#   The OWF signal is strongest within ~50 km. Beyond that, hauls are
#   effectively "far from any OWF" and mostly add noise. The full distance
#   range is retained for the descriptive figures in script 09.
#
# RICHNESS NOTE:
#   Strictly, richness is a count and a Poisson GLM would be more appropriate.
#   For these sample sizes and near-normal distributions, lm() is a defensible
#   approximation. Discuss this caveat in the write-up.
################################################################################

source(here::here("scripts", "00_setup.R"))

# ── 1. LOAD ───────────────────────────────────────────────────────────────────
haul_dat <- read_csv(
  here("data", "processed", "haul_metrics_owf_with_covariates.csv"),
  show_col_types = FALSE
) %>%
  mutate(
    commercial_group = factor(commercial_group, levels = c("Low", "Medium", "High")),
    dist_band        = factor(dist_band, levels = dist_band_levels)
  )

# ── 2. COVARIATE CHECKS ───────────────────────────────────────────────────────
cat("── Covariate missingness ──\n")
haul_dat %>%
  summarise(
    n_total         = n(),
    miss_sbt        = sum(is.na(sbt)),
    miss_sst_trend  = sum(is.na(sst_trends)),
    miss_depth      = sum(is.na(depth)),
    miss_dist_coast = sum(is.na(dist_coast)),
    miss_CMS        = sum(is.na(CMS)),
    miss_richness   = sum(is.na(species_richness))
  ) %>%
  print()

# Scale continuous covariates. owf_dist_km is also scaled here for any
# sensitivity analyses that want it on a comparable scale, but the main
# models below use raw owf_dist_km.
haul_dat <- haul_dat %>%
  mutate(
    depth_sc      = scale(depth)[, 1],
    sbt_sc        = scale(sbt)[, 1],
    sst_trend_sc  = scale(sst_trends)[, 1],
    dist_coast_sc = scale(dist_coast)[, 1],
    owf_dist_sc   = scale(owf_dist_km)[, 1]
  )

# ── 3. RESTRICT TO ≤ 50 KM AND SAVE ───────────────────────────────────────────
haul_50 <- haul_dat %>%
  filter(owf_dist_km <= 50)

cat("\n── Rows within 50 km, by commercial group ──\n")
haul_50 %>%
  filter(!is.na(CMS)) %>%
  count(commercial_group) %>%
  print()

write_csv(
  haul_50,
  here("data", "processed", "haul_metrics_with_covariates_clean.csv")
)
cat("Saved: data/processed/haul_metrics_with_covariates_clean.csv\n\n")

################################################################################
# ── 4. ADJUSTED LINEAR MODELS — CMS ───────────────────────────────────────────
################################################################################

cat("── Adjusted linear models: CMS ~ OWF distance + covariates (≤50 km) ──\n\n")

cms_models_adj <- haul_50 %>%
  filter(!is.na(CMS)) %>%
  group_by(commercial_group) %>%
  group_map(function(df, key) {

    mod <- lm(
      CMS ~ owf_dist_km + depth_sc + sbt_sc + sst_trend_sc + dist_coast_sc,
      data = df
    )
    s        <- summary(mod)
    coef_tab <- coef(s)

    tibble(
      commercial_group = key$commercial_group,
      n                = nobs(mod),
      owf_slope        = coef_tab["owf_dist_km",   "Estimate"],
      owf_se           = coef_tab["owf_dist_km",   "Std. Error"],
      owf_p            = coef_tab["owf_dist_km",   "Pr(>|t|)"],
      depth_coef       = coef_tab["depth_sc",      "Estimate"],
      depth_p          = coef_tab["depth_sc",      "Pr(>|t|)"],
      sbt_coef         = coef_tab["sbt_sc",        "Estimate"],
      sbt_p            = coef_tab["sbt_sc",        "Pr(>|t|)"],
      sst_trend_coef   = coef_tab["sst_trend_sc",  "Estimate"],
      sst_trend_p      = coef_tab["sst_trend_sc",  "Pr(>|t|)"],
      dist_coast_coef  = coef_tab["dist_coast_sc", "Estimate"],
      dist_coast_p     = coef_tab["dist_coast_sc", "Pr(>|t|)"],
      r_squared        = s$r.squared,
      adj_r_squared    = s$adj.r.squared
    )
  }) %>%
  bind_rows()

print(cms_models_adj, digits = 4, width = 120)

cat("\n── Full model summaries: CMS ──\n")
haul_50 %>%
  filter(!is.na(CMS)) %>%
  group_by(commercial_group) %>%
  group_walk(function(df, key) {
    cat("\nCommercial group:", as.character(key$commercial_group), "\n")
    mod <- lm(
      CMS ~ owf_dist_km + depth_sc + sbt_sc + sst_trend_sc + dist_coast_sc,
      data = df
    )
    print(summary(mod))
  })

write_csv(
  cms_models_adj,
  here("outputs", "tables", "linear_model_coefficients_CMS.csv")
)
cat("\nSaved: outputs/tables/linear_model_coefficients_CMS.csv\n")

################################################################################
# ── 5. ADJUSTED LINEAR MODELS — SPECIES RICHNESS ──────────────────────────────
################################################################################

cat("\n── Adjusted linear models: richness ~ OWF distance + covariates (≤50 km) ──\n\n")

richness_models_adj <- haul_50 %>%
  filter(!is.na(species_richness)) %>%
  group_by(commercial_group) %>%
  group_map(function(df, key) {

    mod <- lm(
      species_richness ~ owf_dist_km + depth_sc + sbt_sc + sst_trend_sc + dist_coast_sc,
      data = df
    )
    s        <- summary(mod)
    coef_tab <- coef(s)

    tibble(
      commercial_group = key$commercial_group,
      n                = nobs(mod),
      owf_slope        = coef_tab["owf_dist_km",   "Estimate"],
      owf_se           = coef_tab["owf_dist_km",   "Std. Error"],
      owf_p            = coef_tab["owf_dist_km",   "Pr(>|t|)"],
      depth_coef       = coef_tab["depth_sc",      "Estimate"],
      depth_p          = coef_tab["depth_sc",      "Pr(>|t|)"],
      sbt_coef         = coef_tab["sbt_sc",        "Estimate"],
      sbt_p            = coef_tab["sbt_sc",        "Pr(>|t|)"],
      sst_trend_coef   = coef_tab["sst_trend_sc",  "Estimate"],
      sst_trend_p      = coef_tab["sst_trend_sc",  "Pr(>|t|)"],
      dist_coast_coef  = coef_tab["dist_coast_sc", "Estimate"],
      dist_coast_p     = coef_tab["dist_coast_sc", "Pr(>|t|)"],
      r_squared        = s$r.squared,
      adj_r_squared    = s$adj.r.squared
    )
  }) %>%
  bind_rows()

print(richness_models_adj, digits = 4, width = 120)

cat("\n── Full model summaries: species richness ──\n")
haul_50 %>%
  filter(!is.na(species_richness)) %>%
  group_by(commercial_group) %>%
  group_walk(function(df, key) {
    cat("\nCommercial group:", as.character(key$commercial_group), "\n")
    mod <- lm(
      species_richness ~ owf_dist_km + depth_sc + sbt_sc + sst_trend_sc + dist_coast_sc,
      data = df
    )
    print(summary(mod))
  })

write_csv(
  richness_models_adj,
  here("outputs", "tables", "linear_model_coefficients_richness.csv")
)
cat("\nSaved: outputs/tables/linear_model_coefficients_richness.csv\n")

################################################################################
# ── 6. COMPARISON: WITH vs WITHOUT COVARIATES ─────────────────────────────────
# If the owf_dist_km slope changes substantially after covariate adjustment,
# part of the raw distance signal was driven by environmental gradients.
################################################################################

cat("\n── Unadjusted: CMS ~ owf_dist_km ──\n\n")

cms_models_raw <- haul_50 %>%
  filter(!is.na(CMS)) %>%
  group_by(commercial_group) %>%
  group_map(function(df, key) {
    mod <- lm(CMS ~ owf_dist_km, data = df)
    s   <- summary(mod)
    tibble(
      commercial_group = key$commercial_group,
      n                = nobs(mod),
      owf_slope_raw    = coef(mod)[["owf_dist_km"]],
      owf_se_raw       = coef(s)["owf_dist_km", "Std. Error"],
      owf_p_raw        = coef(s)["owf_dist_km", "Pr(>|t|)"],
      r_squared_raw    = s$r.squared
    )
  }) %>%
  bind_rows()

print(cms_models_raw, digits = 4)

cat("\n── Unadjusted: richness ~ owf_dist_km ──\n\n")

richness_models_raw <- haul_50 %>%
  filter(!is.na(species_richness)) %>%
  group_by(commercial_group) %>%
  group_map(function(df, key) {
    mod <- lm(species_richness ~ owf_dist_km, data = df)
    s   <- summary(mod)
    tibble(
      commercial_group = key$commercial_group,
      n                = nobs(mod),
      owf_slope_raw    = coef(mod)[["owf_dist_km"]],
      owf_se_raw       = coef(s)["owf_dist_km", "Std. Error"],
      owf_p_raw        = coef(s)["owf_dist_km", "Pr(>|t|)"],
      r_squared_raw    = s$r.squared
    )
  }) %>%
  bind_rows()

print(richness_models_raw, digits = 4)

# Joined comparison: unadjusted vs adjusted
cms_compare <- left_join(
  cms_models_raw %>% select(commercial_group, owf_slope_raw, owf_p_raw, r_squared_raw),
  cms_models_adj %>% select(commercial_group,
                            owf_slope_adj = owf_slope,
                            owf_p_adj     = owf_p,
                            r_squared_adj = adj_r_squared),
  by = "commercial_group"
) %>%
  mutate(response = "Community Mean Size (cm)")

richness_compare <- left_join(
  richness_models_raw %>% select(commercial_group, owf_slope_raw, owf_p_raw, r_squared_raw),
  richness_models_adj %>% select(commercial_group,
                                 owf_slope_adj = owf_slope,
                                 owf_p_adj     = owf_p,
                                 r_squared_adj = adj_r_squared),
  by = "commercial_group"
) %>%
  mutate(response = "Species richness")

unadj_vs_adj <- bind_rows(cms_compare, richness_compare) %>%
  select(response, everything())

cat("\n── Slope comparison (unadjusted vs adjusted) ──\n")
print(unadj_vs_adj, digits = 4)

write_csv(
  unadj_vs_adj,
  here("outputs", "tables", "linear_model_unadjusted_vs_adjusted.csv")
)
cat("\nSaved: outputs/tables/linear_model_unadjusted_vs_adjusted.csv\n")

################################################################################
# ── 7. COVARIATE-VS-DISTANCE DIAGNOSTIC FIGURE ────────────────────────────────
# Are covariates themselves correlated with OWF distance? If yes, that
# justifies the covariate adjustment. Worth referencing in the Methods.
################################################################################

cov_long <- haul_50 %>%
  select(owf_dist_km, depth, sbt, sst_trends, dist_coast) %>%
  pivot_longer(-owf_dist_km, names_to = "covariate", values_to = "value") %>%
  mutate(covariate = recode(covariate,
                            "depth"      = "Depth (m)",
                            "sbt"        = "Sea-bottom temperature (°C)",
                            "sst_trends" = "SST trend (°C / decade)",
                            "dist_coast" = "Distance to coast (km)"))

p_cov <- ggplot(cov_long, aes(x = owf_dist_km, y = value)) +
  geom_point(alpha = 0.15, size = 0.6, colour = "steelblue") +
  geom_smooth(method = "loess", span = 0.7,
              colour = "firebrick", fill = "firebrick",
              alpha = 0.2, linewidth = 0.8) +
  facet_wrap(~ covariate, scales = "free_y", ncol = 2) +
  labs(
    title    = "Environmental covariates vs distance to nearest OWF",
    subtitle = "Hauls within 50 km  |  LOESS smooth",
    x        = "Distance to nearest OWF (km)",
    y        = "Covariate value"
  )

ggsave(
  here("outputs", "figures", "covariate_vs_owf_distance.png"),
  plot = p_cov, width = 9, height = 7, dpi = 300, bg = "white"
)
cat("Saved: outputs/figures/covariate_vs_owf_distance.png\n")

cat("\n── Script complete ──\n")
