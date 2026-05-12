################################################################################
# exp_04_richness_all_taxa.R
#
# PURPOSE (EXPLORATORY):
#   Compute species richness three ways at the haul level:
#
#     - richness_total : every taxon, fish + invertebrates
#     - richness_fish  : demersal + pelagic fish only (DEM + PEL)
#     - richness_invert: invertebrates only (dempel == "Other")
#
#   The main analysis uses richness over the FishBase-classified fish only
#   (script 04 onwards). This script is kept to confirm that the patterns
#   are not artefacts of the fish-only filtering.
#
# INPUTS:
#   - data/raw/NEAtlanticGroundfish/*haul by spp*.csv
#   - data/processed/gns_hauls_dist_to_owf.csv
#
# OUTPUT:
#   - data/processed/richness_dist_to_owf.csv  (used by no other script)
################################################################################

source(here::here("scripts", "00_setup.R"))

# ── 1. LOAD ───────────────────────────────────────────────────────────────────
groundfish_data <- list.files(
  here("data", "raw", "NEAtlanticGroundfish"),
  pattern = "haul by spp",
  full.names = TRUE
) %>%
  purrr::map_dfr(read_csv, show_col_types = FALSE) %>%
  clean_names() %>%
  mutate(sci_name = str_squish(str_trim(sci_name)))

gns_hauls_dist_to_owf <- read_csv(
  here("data", "processed", "gns_hauls_dist_to_owf.csv"),
  show_col_types = FALSE
) %>%
  clean_names()

# ── 2. THREE RICHNESS METRICS ─────────────────────────────────────────────────
richness_total <- groundfish_data %>%
  group_by(haul_id) %>%
  summarise(richness_total = n_distinct(sci_name), .groups = "drop")

richness_fish <- groundfish_data %>%
  filter(dempel %in% c("DEM", "PEL")) %>%
  group_by(haul_id) %>%
  summarise(richness_fish = n_distinct(sci_name), .groups = "drop")

richness_invert <- groundfish_data %>%
  filter(dempel == "Other") %>%
  group_by(haul_id) %>%
  summarise(richness_invert = n_distinct(sci_name), .groups = "drop")

# ── 3. COMBINE ────────────────────────────────────────────────────────────────
richness_combined <- richness_total %>%
  left_join(richness_fish,   by = "haul_id") %>%
  left_join(richness_invert, by = "haul_id")

richness_dist <- richness_combined %>%
  left_join(gns_hauls_dist_to_owf, by = "haul_id") %>%
  filter(!is.na(owf_dist_km))

# ── 4. CHECKS ─────────────────────────────────────────────────────────────────
cat("Hauls with richness data:", nrow(richness_dist), "\n\n")

cat("Mean richness:\n")
richness_dist %>%
  summarise(
    mean_total  = mean(richness_total, na.rm = TRUE),
    mean_fish   = mean(richness_fish,  na.rm = TRUE),
    mean_invert = mean(richness_invert, na.rm = TRUE)
  ) %>%
  print()

cat("\nDistance to OWF (km):\n")
print(summary(richness_dist$owf_dist_km))

# ── 5. SAVE ───────────────────────────────────────────────────────────────────
write_csv(
  richness_dist,
  here("data", "processed", "richness_dist_to_owf.csv")
)
cat("\nSaved: data/processed/richness_dist_to_owf.csv\n")
