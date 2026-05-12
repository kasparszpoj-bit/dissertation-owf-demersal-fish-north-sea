################################################################################
# 05_build_haul_metrics.R
#
# PURPOSE:
#   Build the master haul-level analysis table by joining:
#     - CMS per haul × commercial group         (from script 04)
#     - Species richness per haul × group       (from script 04)
#     - Distance to nearest contemporary OWF    (from script 02)
#   and then cutting OWF distance into discrete bands for plotting.
#
# INPUTS:
#   - data/processed/cms_by_haul.csv
#   - data/processed/richness_by_haul.csv
#   - data/processed/gns_hauls_dist_to_owf.csv
#
# OUTPUT:
#   - data/processed/haul_metrics_owf.csv
#
#   This is the dataset that script 06 enriches with environmental covariates,
#   and that scripts 07–09 then model and visualise.
#
# DISTANCE BANDS:
#   0–5, 5–10, 10–20, 20–50, >50 km. Defined once in 00_setup.R as
#   dist_band_levels.
#
# COLUMN NAMING NOTE:
#   The two community-metric inputs were saved with snake_case via janitor
#   (haul_id, year, shoot_long_degdec, shoot_lat_degdec). The OWF distance
#   file was saved by script 02 in CamelCase (HaulID). We rename HaulID to
#   haul_id on the way in so all joins use a consistent key.
################################################################################

source(here::here("scripts", "00_setup.R"))

# ── 1. LOAD ───────────────────────────────────────────────────────────────────
cms_by_haul <- read_csv(
  here("data", "processed", "cms_by_haul.csv"),
  show_col_types = FALSE
)

richness_by_haul <- read_csv(
  here("data", "processed", "richness_by_haul.csv"),
  show_col_types = FALSE
)

dist_to_owf <- read_csv(
  here("data", "processed", "gns_hauls_dist_to_owf.csv"),
  show_col_types = FALSE
) %>%
  rename(haul_id = HaulID) %>%
  select(haul_id, owf_dist_km)

# ── 2. JOIN CMS + RICHNESS ────────────────────────────────────────────────────
# Both tables share the same key columns (haul_id, year, lat, long,
# commercial_group), so this is a straightforward inner join on all of them.
haul_metrics <- cms_by_haul %>%
  inner_join(
    richness_by_haul,
    by = c("haul_id", "year", "shoot_long_degdec",
           "shoot_lat_degdec", "commercial_group")
  )

# ── 3. JOIN OWF DISTANCE ──────────────────────────────────────────────────────
haul_metrics_owf <- haul_metrics %>%
  left_join(dist_to_owf, by = "haul_id") %>%
  filter(!is.na(owf_dist_km))

# ── 4. CUT INTO DISTANCE BANDS ────────────────────────────────────────────────
haul_metrics_owf <- haul_metrics_owf %>%
  mutate(
    dist_band = cut(
      owf_dist_km,
      breaks         = c(0, 5, 10, 20, 50, Inf),
      labels         = dist_band_levels,
      include.lowest = TRUE,
      right          = FALSE
    ),
    commercial_group = factor(commercial_group,
                              levels = c("Low", "Medium", "High"))
  )

# ── 5. CHECKS ─────────────────────────────────────────────────────────────────
cat("Rows in master haul-metrics table:", nrow(haul_metrics_owf), "\n\n")

cat("Rows by commercial group:\n")
print(table(haul_metrics_owf$commercial_group, useNA = "ifany"))

cat("\nRows by distance band:\n")
print(table(haul_metrics_owf$dist_band, useNA = "ifany"))

cat("\nRows by group × band:\n")
print(table(haul_metrics_owf$commercial_group,
            haul_metrics_owf$dist_band))

# Rename to match HaulID convention used by the covariate script
haul_metrics_owf <- haul_metrics_owf %>%
  rename(HaulID = haul_id)

# ── 6. SAVE ───────────────────────────────────────────────────────────────────
write_csv(
  haul_metrics_owf,
  here("data", "processed", "haul_metrics_owf.csv")
)
cat("\nSaved: data/processed/haul_metrics_owf.csv\n")
