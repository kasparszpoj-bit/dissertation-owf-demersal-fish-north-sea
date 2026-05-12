################################################################################
# exp_03_fishing_score_by_distance.R
#
# PURPOSE (EXPLORATORY):
#   An alternative to the Low/Medium/High commercial groups: collapse FishBase
#   importance into a continuous 0–3 score, then compute a biomass-weighted
#   mean score per haul. This indexes how commercially valuable the catch
#   composition is, on average.
#
#   Not used in the main analysis. Useful as a sense-check on the categorical
#   grouping.
#
# SCORE MAPPING:
#   "highly commercial"      → 3
#   "commercial"             → 2
#   "minor commercial"       → 1
#   "subsistence fisheries"  → 1
#   anything else            → 0
#
# INPUTS:
#   - data/raw/NEAtlanticGroundfish/*haul by spp*.csv
#   - data/processed/gns_hauls_dist_to_owf.csv
#
# OUTPUT:
#   - outputs/figures/exploratory/exp_fishing_score_by_distance.png
################################################################################

source(here::here("scripts", "00_setup.R"))

library(rfishbase)

# ── 1. LOAD GROUNDFISH ────────────────────────────────────────────────────────
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

# ── 2. FISHBASE → CONTINUOUS SCORE ────────────────────────────────────────────
species_list <- unique(groundfish_data$sci_name)

fishbase_traits <- rfishbase::species(
  species_list,
  fields = c("SpecCode", "Genus", "Species", "Importance")
) %>%
  mutate(
    sci_name         = str_squish(str_trim(Species)),
    importance_clean = str_to_lower(str_trim(Importance)),
    fishing_score    = case_when(
      importance_clean == "highly commercial"     ~ 3,
      importance_clean == "commercial"            ~ 2,
      importance_clean == "minor commercial"      ~ 1,
      importance_clean == "subsistence fisheries" ~ 1,
      TRUE                                        ~ 0
    )
  ) %>%
  select(sci_name, Importance, fishing_score)

groundfish_data <- groundfish_data %>%
  left_join(fishbase_traits, by = "sci_name")

cat("Fishing score distribution:\n")
print(table(groundfish_data$fishing_score, useNA = "ifany"))

# ── 3. BIOMASS-WEIGHTED MEAN SCORE PER HAUL ───────────────────────────────────
fishing_index_by_haul <- groundfish_data %>%
  mutate(score_x_biomass = fishing_score * dens_biom_kg_sqkm) %>%
  group_by(haul_id, year, shoot_long_degdec, shoot_lat_degdec) %>%
  summarise(
    total_biomass         = sum(dens_biom_kg_sqkm, na.rm = TRUE),
    mean_commercial_score = sum(score_x_biomass, na.rm = TRUE) /
                            sum(dens_biom_kg_sqkm, na.rm = TRUE),
    .groups = "drop"
  )

fishing_dist <- fishing_index_by_haul %>%
  left_join(gns_hauls_dist_to_owf, by = "haul_id") %>%
  filter(!is.na(owf_dist_km))

# ── 4. PANELS AT FOUR DISTANCE CUT-OFFS ───────────────────────────────────────
N_all <- nrow(fishing_dist)
N_50  <- fishing_dist %>% filter(owf_dist_km <= 50) %>% nrow()
N_20  <- fishing_dist %>% filter(owf_dist_km <= 20) %>% nrow()
N_10  <- fishing_dist %>% filter(owf_dist_km <= 10) %>% nrow()

panel <- function(df, title_str) {
  ggplot(df, aes(x = owf_dist_km, y = mean_commercial_score)) +
    geom_point(alpha = 0.25) +
    geom_smooth(method = "loess", se = TRUE) +
    labs(title = title_str,
         x = "Distance to nearest OWF (km)",
         y = "Mean commercial score") +
    theme_minimal()
}

p_all <- panel(fishing_dist,
               sprintf("Mean commercial score vs distance to nearest OWF (N = %d)", N_all))
p_50  <- panel(fishing_dist %>% filter(owf_dist_km <= 50),
               sprintf("≤ 50 km (N = %d)", N_50))
p_20  <- panel(fishing_dist %>% filter(owf_dist_km <= 20),
               sprintf("≤ 20 km (N = %d)", N_20))
p_10  <- panel(fishing_dist %>% filter(owf_dist_km <= 10),
               sprintf("≤ 10 km (N = %d)", N_10))

fishing_multi <- (p_all | p_50) / (p_20 | p_10)

ggsave(
  here("outputs", "figures", "exploratory", "exp_fishing_score_by_distance.png"),
  plot = fishing_multi, width = 12, height = 9, dpi = 300, bg = "white"
)
cat("Saved: outputs/figures/exploratory/exp_fishing_score_by_distance.png\n")
