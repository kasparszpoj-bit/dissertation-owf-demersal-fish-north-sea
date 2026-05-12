################################################################################
# 08_gam_models.R
#
# PURPOSE:
#   Address RQ2 — is the relationship between OWF distance and community
#   metrics non-linear or non-monotonic? Does CMS or species richness peak,
#   dip, or plateau at a particular distance?
#
#   GAMs allow a flexible smooth in OWF distance, so a hump, threshold, or
#   inflection that a linear model would miss can be detected. Depth and SBT
#   are included as parametric (linear) terms so we control for them while
#   isolating the OWF-distance smooth.
#
# INPUT:
#   - data/processed/haul_metrics_with_covariates_clean.csv   (≤50 km subset)
#
# OUTPUTS:
#   - outputs/tables/GAM_summary_table.csv
#   - outputs/tables/AIC_comparison_GAM_vs_linear.csv
#
#   Figures are produced by script 09 to keep modelling and plotting separate.
#
# MODEL STRUCTURE:
#   response ~ s(owf_dist_km, k = 5) + depth_sc + sbt_sc
#
#   - s(owf_dist_km): thin-plate regression spline with REML smoothness
#     selection.
#   - k = 5: caps the smooth's flexibility. Conservative for these sample
#     sizes; defensible against overfitting.
#   - depth_sc, sbt_sc: linear parametric terms — we control for them, not
#     model their shape.
#
# READING THE OUTPUT:
#   edf (effective degrees of freedom):
#     ≈ 1   the smooth has collapsed to roughly a straight line
#     > 1   curvature is present
#     >> 1  strong non-linearity
################################################################################

source(here::here("scripts", "00_setup.R"))

# ── 1. LOAD ───────────────────────────────────────────────────────────────────
haul_50 <- read_csv(
  here("data", "processed", "haul_metrics_with_covariates_clean.csv"),
  show_col_types = FALSE
) %>%
  mutate(
    commercial_group = factor(commercial_group, levels = c("Low", "Medium", "High")),
    # Re-scale here: scaling is dataset-specific so re-derive on the loaded data.
    depth_sc         = scale(depth)[, 1],
    sbt_sc           = scale(sbt)[, 1]
  )

cat("Rows loaded:", nrow(haul_50), "\n")
cat("Commercial groups:", levels(haul_50$commercial_group), "\n\n")

################################################################################
# ── 2. FIT GAMs — CMS ─────────────────────────────────────────────────────────
################################################################################

cat("── Fitting GAMs: CMS ──\n")

cms_gams <- haul_50 %>%
  filter(!is.na(CMS)) %>%
  group_by(commercial_group) %>%
  group_map(function(df, key) {
    cat("  Fitting:", as.character(key$commercial_group), "\n")
    mod <- gam(
      CMS ~ s(owf_dist_km, k = 5) + depth_sc + sbt_sc,
      data   = df,
      method = "REML"
    )
    list(model = mod, group = as.character(key$commercial_group), data = df)
  })
names(cms_gams) <- c("Low", "Medium", "High")

cat("\n── GAM summaries: CMS ──\n")
for (grp in names(cms_gams)) {
  cat("\nCommercial group:", grp, "\n")
  print(summary(cms_gams[[grp]]$model))
}

################################################################################
# ── 3. FIT GAMs — SPECIES RICHNESS ────────────────────────────────────────────
################################################################################

cat("\n── Fitting GAMs: Species Richness ──\n")

richness_gams <- haul_50 %>%
  filter(!is.na(species_richness)) %>%
  group_by(commercial_group) %>%
  group_map(function(df, key) {
    cat("  Fitting:", as.character(key$commercial_group), "\n")
    mod <- gam(
      species_richness ~ s(owf_dist_km, k = 5) + depth_sc + sbt_sc,
      data   = df,
      method = "REML"
    )
    list(model = mod, group = as.character(key$commercial_group), data = df)
  })
names(richness_gams) <- c("Low", "Medium", "High")

cat("\n── GAM summaries: Species Richness ──\n")
for (grp in names(richness_gams)) {
  cat("\nCommercial group:", grp, "\n")
  print(summary(richness_gams[[grp]]$model))
}

################################################################################
# ── 4. KEY RESULTS TABLE ──────────────────────────────────────────────────────
################################################################################

extract_gam_results <- function(gam_list, response_name) {
  map_dfr(gam_list, function(x) {
    s          <- summary(x$model)
    smooth_tab <- s$s.table
    par_tab    <- s$p.table

    tibble(
      response         = response_name,
      commercial_group = x$group,
      n                = nobs(x$model),
      edf              = smooth_tab["s(owf_dist_km)", "edf"],
      smooth_F         = smooth_tab["s(owf_dist_km)", "F"],
      smooth_p         = smooth_tab["s(owf_dist_km)", "p-value"],
      depth_coef       = par_tab["depth_sc", "Estimate"],
      depth_p          = par_tab["depth_sc", "Pr(>|t|)"],
      sbt_coef         = par_tab["sbt_sc",   "Estimate"],
      sbt_p            = par_tab["sbt_sc",   "Pr(>|t|)"],
      dev_explained    = s$dev.expl * 100,
      r_sq_adj         = s$r.sq
    )
  })
}

gam_results <- bind_rows(
  extract_gam_results(cms_gams,      "Community Mean Size (cm)"),
  extract_gam_results(richness_gams, "Species richness")
)

cat("\n── GAM results summary ──\n")
print(gam_results, digits = 3, width = 120)

write_csv(
  gam_results,
  here("outputs", "tables", "GAM_summary_table.csv")
)
cat("Saved: outputs/tables/GAM_summary_table.csv\n")

# Also save inside data/processed/ for backwards compatibility with downstream
# scripts that previously read it from there.
write_csv(
  gam_results,
  here("data", "processed", "GAM_summary_table.csv")
)

################################################################################
# ── 5. AIC COMPARISON: GAM vs LINEAR ──────────────────────────────────────────
# Lower AIC = better fit. A delta > 2 in favour of the GAM is treated as
# evidence that the non-linear smooth is a meaningful improvement on a line.
# This is the direct test of RQ2.
################################################################################

cat("\n── AIC comparison: GAM vs linear model ──\n\n")

compare_aic <- function(gam_list, response_var) {
  map_dfr(gam_list, function(x) {
    df         <- x$data %>% filter(!is.na(.data[[response_var]]))
    mod_gam    <- x$model
    formula_lm <- as.formula(
      paste(response_var, "~ owf_dist_km + depth_sc + sbt_sc")
    )
    mod_lm <- lm(formula_lm, data = df)

    tibble(
      response         = response_var,
      commercial_group = x$group,
      AIC_linear       = AIC(mod_lm),
      AIC_GAM          = AIC(mod_gam),
      delta_AIC        = AIC_linear - AIC_GAM,
      GAM_preferred    = AIC_linear - AIC_GAM > 2
    )
  })
}

aic_cms      <- compare_aic(cms_gams,      "CMS")
aic_richness <- compare_aic(richness_gams, "species_richness")

aic_comparison <- bind_rows(aic_cms, aic_richness)
print(aic_comparison, digits = 3)

write_csv(
  aic_comparison,
  here("outputs", "tables", "AIC_comparison_GAM_vs_linear.csv")
)
cat("\nSaved: outputs/tables/AIC_comparison_GAM_vs_linear.csv\n")

################################################################################
# ── 6. PLAIN-LANGUAGE SUMMARY ─────────────────────────────────────────────────
# Console output to guide interpretation in the Results section.
################################################################################

cat("\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
cat("── Plain-language GAM results summary ──\n")
cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n")

for (i in 1:nrow(gam_results)) {
  row  <- gam_results[i, ]
  sig  <- ifelse(row$smooth_p < 0.05, "SIGNIFICANT", "non-significant")
  mono <- ifelse(row$edf < 1.5, "approximately linear",
                 "non-linear / potentially non-monotonic")
  cat(sprintf(
    "%s — %s group:\n  Smooth %s (edf = %.2f, F = %.2f, p = %.4f)\n  Shape: %s\n  Deviance explained: %.1f%%\n\n",
    row$response, row$commercial_group,
    sig, row$edf, row$smooth_F, row$smooth_p,
    mono, row$dev_explained
  ))
}

cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
cat("AIC comparison (positive delta = GAM better than linear):\n\n")
print(aic_comparison %>%
        select(response, commercial_group, delta_AIC, GAM_preferred),
      digits = 3)

cat("\n── Script complete ──\n")
