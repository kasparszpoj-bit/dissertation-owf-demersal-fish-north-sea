################################################################################
# 01_owf_polygons.R
#
# PURPOSE:
#   Take the raw EMODnet offshore wind farm shapefile, clip it to the ICES
#   Greater North Sea ecoregion, apply quality-control filters, and join it
#   to a manually curated metadata CSV (containing construction and operation
#   start years).
#
# INPUTS:
#   - data/raw/offshore_wind/EMODnet_HA_Energy_WindFarms_pg_*.shp
#   - data/raw/ICES_ecoregions/ICES_ecoregions_20171207_erase_ESRI.shp
#   - data/processed/owf_master_metadata_template_EDITED.csv
#
# OUTPUT:
#   - data/processed/owf_polygons_with_dates.rds
#
# NOTE:
#   The QC filter logic and the function pattern below are adapted from a
#   script provided by Tom Webb (Sheffield) — kept here with attribution.
################################################################################

source(here::here("scripts", "00_setup.R"))

# ── 1. FILE PATHS ─────────────────────────────────────────────────────────────
raw_shp <- here(
  "data", "raw", "offshore_wind",
  "EMODnet_HA_Energy_WindFarms_pg_20260127.shp"
)

ices_shp <- here(
  "data", "raw", "ICES_ecoregions",
  "ICES_ecoregions_20171207_erase_ESRI.shp"
)

edited_csv <- here(
  "data", "processed",
  "owf_master_metadata_template_EDITED.csv"
)

# ── 2. LOAD AND CLIP TO GREATER NORTH SEA ─────────────────────────────────────
# Convert to WGS84, fix any invalid geometries, then crop to the ICES
# Greater North Sea ecoregion. Cropping via terra::crop() rather than sf
# because sf::st_intersection() throws errors on this dataset.
owf_raw <- read_sf(raw_shp, quiet = TRUE)

owf_wgs <- owf_raw %>%
  st_transform(4326) %>%
  st_make_valid()

ices_ecoregions <- read_sf(ices_shp)
gns <- ices_ecoregions %>%
  filter(Ecoregion == "Greater North Sea")

owf_gns <- owf_wgs %>%
  as_spatvector() %>%
  crop(as_spatvector(gns)) %>%
  st_as_sf() %>%
  as_tibble() %>%
  st_as_sf()

# ── 3. QUALITY-CONTROL FILTERING ──────────────────────────────────────────────
# Keep only farms that are actually built or under construction with a real
# capacity. Drop met masts, OFTOs (offshore transmission operators), and
# tidal/wave installations that share the dataset.
owf_qc <- owf_gns %>%
  clean_names() %>%
  mutate(
    status  = str_to_lower(str_trim(status)),
    name    = str_squish(name),
    country = str_squish(country)
  ) %>%
  filter(!status %in% c("planned", "approved")) %>%
  filter(!str_detect(str_to_lower(name), "met mast")) %>%
  filter(!str_detect(str_to_lower(name), "ofto")) %>%
  filter(!str_detect(str_to_lower(name), "tidal|wave")) %>%
  filter(
    (status %in% c("production", "construction") &
       !is.na(power_mw) & power_mw > 0) |
      (status == "dismantled" &
         !is.na(area_sqkm) & area_sqkm >= 0.05)
  )

# ── 4. JOIN MANUALLY CURATED METADATA ─────────────────────────────────────────
# The CSV adds construction_start_year and operation_start_year, both required
# downstream so a haul is only matched to OWFs that existed at the time of the
# tow.
owf_meta <- read_csv(edited_csv, show_col_types = FALSE) %>%
  mutate(
    name    = str_squish(name),
    country = str_squish(country)
  )

# Sanity check: report any metadata rows that won't match a polygon
unmatched <- anti_join(
  owf_meta,
  st_drop_geometry(owf_qc),
  by = c("name", "country")
)
if (nrow(unmatched) > 0) {
  message("Rows in edited CSV that did not match polygons:")
  print(unmatched %>% select(name, country), n = 30)
}

# Drop polygon attribute columns before the join so we don't end up with
# duplicate columns in the joined output.
owf_final <- owf_qc %>%
  select(name, country) %>%
  left_join(owf_meta, by = c("name", "country"))

# ── 5. POST-JOIN CHECKS ───────────────────────────────────────────────────────
owf_final %>%
  st_drop_geometry() %>%
  summarise(
    n                    = n(),
    missing_construction = sum(is.na(construction_start_year)),
    missing_operation    = sum(is.na(operation_start_year))
  ) %>%
  print()

# ── 6. SAVE ───────────────────────────────────────────────────────────────────
saveRDS(
  owf_final,
  here("data", "processed", "owf_polygons_with_dates.rds")
)
cat("Saved: data/processed/owf_polygons_with_dates.rds\n")
