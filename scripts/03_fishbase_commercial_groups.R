################################################################################
# 03_fishbase_commercial_groups.R
#
# PURPOSE:
#   Classify every fish species in the trawl dataset by its FishBase
#   commercial-importance category, then collapse those categories into three
#   analysis groups: Low / Medium / High.
#
#   The output is the master fish-only species table used by every downstream
#   script for community-level analysis.
#
# INPUTS:
#   - data/raw/NEAtlanticGroundfish/*haul by spp*.csv   (one or more files)
#
# OUTPUT:
#   - data/processed/groundfish_data_with_commercial_groups.csv
#
# CLASSIFICATION RULES:
#   FishBase "highly commercial"  → High
#   FishBase "commercial"         → Medium
#   FishBase "minor commercial"   → Low
#   anything else                 → NA (excluded from group analyses)
#
# WHY FISH ONLY?
#   Community Mean Size (mean fish length) is not meaningful when invertebrates
#   are mixed in. Fish are filtered using the dempel column (DEM = demersal,
#   PEL = pelagic). Invertebrate-inclusive richness is computed separately in
#   scripts/exploratory/exp_04_richness_all_taxa.R.
################################################################################

source(here::here("scripts", "00_setup.R"))

# rfishbase is heavy; loaded only here, where it is used.
library(rfishbase)

# ── 1. LOAD GROUNDFISH DATA ───────────────────────────────────────────────────
# Reads all haul-by-species files in the directory and binds them.
groundfish_data <- list.files(
  here("data", "raw", "NEAtlanticGroundfish"),
  pattern = "haul by spp",
  full.names = TRUE
) %>%
  purrr::map_dfr(read_csv, show_col_types = FALSE) %>%
  clean_names() %>%
  mutate(sci_name = str_squish(str_trim(sci_name)))

# ── 2. FILTER TO FISH ONLY ────────────────────────────────────────────────────
# Default: demersal + pelagic fish.
# To restrict to demersal only, change to filter(dempel == "DEM").
fish_data <- groundfish_data %>%
  filter(dempel %in% c("DEM", "PEL"))

# ── 3. QUERY FISHBASE FOR COMMERCIAL IMPORTANCE ───────────────────────────────
species_list <- unique(fish_data$sci_name)

fishbase_traits <- rfishbase::species(
  species_list,
  fields = c("SpecCode", "Species", "Importance")
) %>%
  mutate(
    sci_name   = str_squish(str_trim(Species)),
    Importance = str_squish(str_trim(Importance))
  ) %>%
  select(sci_name, Importance)

# ── 4. ASSIGN COMMERCIAL GROUPS ───────────────────────────────────────────────
fish_data <- fish_data %>%
  left_join(fishbase_traits, by = "sci_name") %>%
  mutate(
    commercial_group = case_when(
      Importance == "highly commercial" ~ "High",
      Importance == "commercial"        ~ "Medium",
      Importance == "minor commercial"  ~ "Low",
      TRUE                              ~ NA_character_
    ),
    commercial_group = factor(
      commercial_group,
      levels = c("Low", "Medium", "High")
    )
  )

# ── 5. QUALITY CHECKS ─────────────────────────────────────────────────────────
cat("FishBase importance categories:\n")
print(table(fish_data$Importance, useNA = "ifany"))

cat("\nCommercial groups assigned:\n")
print(table(fish_data$commercial_group, useNA = "ifany"))

cat("\nSpecies with no FishBase importance match:\n")
fish_data %>%
  filter(is.na(Importance)) %>%
  distinct(sci_name, dempel) %>%
  arrange(sci_name) %>%
  print(n = 50)

# ── 6. SAVE ───────────────────────────────────────────────────────────────────
write_csv(
  fish_data,
  here("data", "processed", "groundfish_data_with_commercial_groups.csv")
)
cat("\nSaved: data/processed/groundfish_data_with_commercial_groups.csv\n")
