################################################################################
# 04_community_metrics.R
#
# PURPOSE:
#   Compute the two community-level response metrics used in the analysis,
#   per haul × commercial group:
#
#     - Community Mean Size (CMS): biomass-weighted mean fish length, in cm
#     - Species richness: number of distinct species
#
# INPUT:
#   - data/processed/groundfish_data_with_commercial_groups.csv
#
# OUTPUTS:
#   - data/processed/cms_by_haul.csv
#   - data/processed/richness_by_haul.csv
#
#   These two intermediate tables are the inputs to script 05, which combines
#   them with OWF distance and distance bands into the master haul-level table.
#
# DEFINITION OF CMS:
#   CMS_h = sum_i (length_i * biomass_i) / sum_i (biomass_i)
#
#   where i indexes species records within haul h. Biomass weighting means
#   abundant species contribute more than rare ones.
################################################################################

source(here::here("scripts", "00_setup.R"))

# ── 1. LOAD ───────────────────────────────────────────────────────────────────
groundfish_data <- read_csv(
  here("data", "processed", "groundfish_data_with_commercial_groups.csv"),
  show_col_types = FALSE
) %>%
  clean_names()

# ── 2. CLEAN ──────────────────────────────────────────────────────────────────
# Drop rows missing length, biomass, or location. CMS undefined where biomass
# is zero or negative.
groundfish_clean <- groundfish_data %>%
  filter(
    !is.na(haul_id),
    !is.na(shoot_long_degdec),
    !is.na(shoot_lat_degdec),
    !is.na(fish_length_cm),
    !is.na(dens_biom_kg_sqkm),
    dens_biom_kg_sqkm > 0
  )

# ── 3. CMS PER HAUL × COMMERCIAL GROUP ────────────────────────────────────────
cms_by_haul <- groundfish_clean %>%
  filter(!is.na(commercial_group)) %>%
  mutate(length_x_biomass = fish_length_cm * dens_biom_kg_sqkm) %>%
  group_by(haul_id, year, shoot_long_degdec, shoot_lat_degdec,
           commercial_group) %>%
  summarise(
    CMS           = sum(length_x_biomass,   na.rm = TRUE) /
                    sum(dens_biom_kg_sqkm,  na.rm = TRUE),
    total_biomass = sum(dens_biom_kg_sqkm, na.rm = TRUE),
    .groups = "drop"
  )

# ── 4. SPECIES RICHNESS PER HAUL × COMMERCIAL GROUP ───────────────────────────
richness_by_haul <- groundfish_clean %>%
  filter(!is.na(commercial_group)) %>%
  group_by(haul_id, year, shoot_long_degdec, shoot_lat_degdec,
           commercial_group) %>%
  summarise(
    species_richness = n_distinct(sci_name),
    .groups = "drop"
  )

# ── 5. CHECKS ─────────────────────────────────────────────────────────────────
cat("CMS rows by group:\n")
print(table(cms_by_haul$commercial_group, useNA = "ifany"))

cat("\nRichness rows by group:\n")
print(table(richness_by_haul$commercial_group, useNA = "ifany"))

cat("\nCMS summary by group (cm):\n")
cms_by_haul %>%
  group_by(commercial_group) %>%
  summarise(
    n        = n(),
    mean_cms = mean(CMS, na.rm = TRUE),
    median   = median(CMS, na.rm = TRUE),
    .groups  = "drop"
  ) %>%
  print()

# ── 6. SAVE ───────────────────────────────────────────────────────────────────
write_csv(cms_by_haul,
          here("data", "processed", "cms_by_haul.csv"))
cat("\nSaved: data/processed/cms_by_haul.csv\n")

write_csv(richness_by_haul,
          here("data", "processed", "richness_by_haul.csv"))
cat("Saved: data/processed/richness_by_haul.csv\n")
