################################################################################
# 02_haul_owf_distance.R
#
# PURPOSE:
#   For every haul in the NE Atlantic Groundfish Survey, compute the distance
#   (km) to the nearest offshore wind farm that was either operational or
#   under construction at the time of the haul.
#
#   The match is year-aware: a 2010 haul cannot be matched to a farm that did
#   not begin construction until 2018.
#
# INPUTS:
#   - data/processed/owf_polygons_with_dates.rds   (from script 01)
#   - data/raw/trawl/temperature_matched_hauls.csv
#
# OUTPUT:
#   - data/processed/gns_hauls_dist_to_owf.csv
#
# RUNTIME:
#   Around 5 minutes for ~25.5K Greater North Sea hauls. Use the slice() line
#   inside the call to test on a small subset first.
#
# NOTE:
#   The get_nearest_windfarm() function below is adapted from a script
#   provided by Tom Webb (Sheffield).
################################################################################

source(here::here("scripts", "00_setup.R"))

# ── 1. LOAD OWF POLYGONS ──────────────────────────────────────────────────────
owf <- readRDS(
  here("data", "processed", "owf_polygons_with_dates.rds")
) %>%
  st_transform(4326)

# ── 2. LOAD HAULS, MAKE SPATIAL, RESTRICT TO GNS ──────────────────────────────
# The haul CSV contains records from surveys outside the Greater North Sea.
# We intersect with the GNS polygon to keep only hauls inside the ecoregion.
ices_ecoregions <- read_sf(
  here("data", "raw", "ICES_ecoregions",
       "ICES_ecoregions_20171207_erase_ESRI.shp")
)
gns <- ices_ecoregions %>%
  filter(Ecoregion == "Greater North Sea")

gns_hauls <- read_csv(
  here("data", "raw", "trawl", "temperature_matched_hauls.csv"),
  show_col_types = FALSE
) %>%
  st_as_sf(coords = c("ShootLong_degdec", "ShootLat_degdec"),
           crs = 4326) %>%
  st_intersection(st_make_valid(gns))

cat("Hauls inside the Greater North Sea:", nrow(gns_hauls), "\n")

# ── 3. NEAREST-WINDFARM FUNCTION ──────────────────────────────────────────────
# For a single haul, find the nearest OWF that existed at the time of the haul.
#
# Args:
#   haul_dat     — sf object, single row, must contain HaulID and Year columns
#   owf_dat      — sf object of OWF polygons with construction_start_year and
#                  operation_start_year columns
#   in_operation — TRUE: restrict to fully operational farms
#                  FALSE: include farms under construction (used here)
#
# Returns: a one-row tibble with HaulID, owf_dist_km, and OWF metadata.
get_nearest_windfarm <- function(haul_dat,
                                 owf_dat = owf,
                                 in_operation = FALSE) {

  yr <- pull(haul_dat, Year)

  if (in_operation == FALSE) {
    owf_dat <- owf_dat %>%
      filter(!is.na(construction_start_year),
             construction_start_year <= yr)
  } else {
    owf_dat <- owf_dat %>%
      filter(!is.na(operation_start_year),
             operation_start_year <= yr)
  }

  # Index of the nearest OWF feature
  nearest_owf <- st_nearest_feature(haul_dat, owf_dat)

  # Distance to that nearest feature, in km
  dist_to_nearest_owf_km <- st_distance(haul_dat, owf_dat[nearest_owf, ]) / 1000
  dist_to_nearest_owf_km <- as.vector(dist_to_nearest_owf_km)

  tibble(
    HaulID      = pull(haul_dat, HaulID),
    owf_dist_km = dist_to_nearest_owf_km
  ) %>%
    bind_cols(st_drop_geometry(owf_dat[nearest_owf, ]))
}

# ── 4. APPLY OVER ALL HAULS ───────────────────────────────────────────────────
# Uncomment the slice() line below to test on a small subset before running
# the full ~5 minute computation.
gns_hauls_dist_to_owf <- gns_hauls %>%
  # slice(1:3) %>%
  mutate(grp_var = HaulID) %>%
  group_by(grp_var) %>%
  group_map(~ get_nearest_windfarm(haul_dat = ., in_operation = FALSE)) %>%
  bind_rows()

# ── 5. SAVE ───────────────────────────────────────────────────────────────────
write_csv(
  gns_hauls_dist_to_owf,
  here("data", "processed", "gns_hauls_dist_to_owf.csv")
)
cat("Saved: data/processed/gns_hauls_dist_to_owf.csv\n")
