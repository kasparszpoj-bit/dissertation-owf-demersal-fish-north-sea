################################################################################
# 06_add_covariates.R
#
# PURPOSE:
#   Add four environmental covariates to the haul metrics table:
#     - sea-surface temperature trend (sst_trends)
#     - sea-bottom temperature        (sbt)
#     - depth                         (from bathymetry raster)
#     - distance to coast             (from same raster)
#
#   These covariates are needed because depth, temperature, and coastal
#   distance all vary systematically across the North Sea and could confound
#   a raw distance-to-OWF signal.
#
# INPUTS:
#   - data/processed/haul_metrics_owf.csv               (from script 05)
#   - data/raw/trawl/temperature_matched_hauls.csv      (sst_trends, sbt)
#   - data/raw/depth/bathy_dist_coast.tif               (depth, dist_coast)
#
# OUTPUT:
#   - data/processed/haul_metrics_owf_with_covariates.csv
################################################################################

source(here::here("scripts", "00_setup.R"))

# ── 1. LOAD HAUL METRICS ──────────────────────────────────────────────────────
haul_metrics <- read_csv(
  here("data", "processed", "haul_metrics_owf.csv"),
  show_col_types = FALSE
)

# ── 2. JOIN TEMPERATURE COVARIATES ────────────────────────────────────────────
# temperature_matched_hauls.csv carries both sst_trends and sbt at the haul
# level. We deduplicate on HaulID to avoid blow-up if multiple species rows
# exist per haul.
temp_data <- read_csv(
  here("data", "raw", "trawl", "temperature_matched_hauls.csv"),
  show_col_types = FALSE
) %>%
  select(HaulID, sst_trends, sbt) %>%
  distinct(HaulID, .keep_all = TRUE)

haul_metrics_with_temp <- haul_metrics %>%
  left_join(temp_data, by = "HaulID")

# ── 3. EXTRACT DEPTH AND DISTANCE-TO-COAST FROM BATHYMETRY RASTER ─────────────
# Build a spatial points layer for the haul locations, extract raster values
# at each point, then convert depth to a positive number.
haul_locations_sf <- read_csv(
  here("data", "raw", "trawl", "temperature_matched_hauls.csv"),
  show_col_types = FALSE
) %>%
  st_as_sf(
    coords = c("ShootLong_degdec", "ShootLat_degdec"),
    remove = FALSE,
    crs    = 4326
  )

bathy <- rast(
  here("data", "raw", "depth", "bathy_dist_coast.tif")
)

bathy_extracted <- terra::extract(bathy, haul_locations_sf, bind = TRUE) %>%
  as_tibble()

depth_data <- bathy_extracted %>%
  select(HaulID, depth = bathymetry_mean, dist_coast) %>%
  mutate(depth = abs(depth)) %>%
  distinct(HaulID, .keep_all = TRUE)

# ── 4. JOIN DEPTH + DIST_COAST ────────────────────────────────────────────────
haul_metrics_with_covariates <- haul_metrics_with_temp %>%
  left_join(depth_data, by = "HaulID")

# ── 5. CHECKS ─────────────────────────────────────────────────────────────────
cat("Columns in final dataset:\n")
print(names(haul_metrics_with_covariates))

cat("\nMissing values per covariate:\n")
cat("  sst_trends:", sum(is.na(haul_metrics_with_covariates$sst_trends)), "\n")
cat("  sbt:       ", sum(is.na(haul_metrics_with_covariates$sbt)),        "\n")
cat("  depth:     ", sum(is.na(haul_metrics_with_covariates$depth)),      "\n")
cat("  dist_coast:", sum(is.na(haul_metrics_with_covariates$dist_coast)), "\n")

cat("\nCovariate summaries:\n")
print(summary(select(
  haul_metrics_with_covariates,
  sst_trends, sbt, depth, dist_coast
)))

# ── 6. SAVE ───────────────────────────────────────────────────────────────────
write_csv(
  haul_metrics_with_covariates,
  here("data", "processed", "haul_metrics_owf_with_covariates.csv")
)
cat("\nSaved: data/processed/haul_metrics_owf_with_covariates.csv\n")
