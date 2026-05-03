# BSc Dissertation: North Sea Fisheries and Offshore Wind Farms

Geostatistical analysis of offshore wind farm influence on demersal 
fish community structure in the Greater North Sea.

**Author:** Kaspar Szpojnarowicz  
**Institution:** University of Sheffield 
**Year:** 2026

## Data Sources

- **Trawl survey data:** GNSIntOT1 NE Atlantic Groundfish Survey 
  (Lynam & Ribeiro, 2022) — available via the Cefas Data Portal
- **OWF polygons:** EMODnet Human Activities offshore wind farm 
  dataset — available at https://emodnet.ec.europa.eu
- **Species classifications:** FishBase, accessed via the rfishbase 
  R package

## Repository Structure

- `data/raw/` — raw data files (not hosted; see sources above)
- `data/processed/` — processed outputs generated during analysis
- `scripts/` — R scripts for data processing and analysis
- `outputs/` — figures and tables generated from analysis

## Dependencies

R packages: tidyverse, sf, terra, rfishbase, mgcv, janitor, 
ggplot2, patchwork
