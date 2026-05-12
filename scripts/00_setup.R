################################################################################
# 00_setup.R
#
# Central setup file. Sourced at the top of every analysis script so that
# library loads, the plotting theme, and the shared colour palette are defined
# in exactly one place.
#
# Usage:
#   source(here::here("scripts", "00_setup.R"))
################################################################################

# ── LIBRARIES ─────────────────────────────────────────────────────────────────
# Spatial + data manipulation
library(sf)
library(terra)
library(tidyverse)
library(here)
library(janitor)

# Modelling
library(mgcv)
library(gratia)

# Plotting
library(ggplot2)
library(patchwork)

# Note: rfishbase is loaded only inside 03_fishbase_commercial_groups.R because
# it is only needed for the species classification step. Keeping it out of the
# common setup avoids a slow load on every script run.

# ── PLOTTING THEME ────────────────────────────────────────────────────────────
theme_owf <- theme_bw(base_size = 11) +
  theme(
    strip.background  = element_rect(fill = "grey92", colour = "grey70"),
    strip.text        = element_text(face = "bold"),
    panel.grid.minor  = element_blank(),
    legend.position   = "none",
    plot.title        = element_text(face = "bold", size = 12),
    plot.subtitle     = element_text(colour = "grey40", size = 10),
    axis.title        = element_text(size = 10),
    axis.text         = element_text(size = 9)
  )

theme_set(theme_owf)

# ── COLOURS ───────────────────────────────────────────────────────────────────
# Used across all figures so commercial groups have consistent colours.
group_colours <- c(
  "Low"    = "#4E9AC6",
  "Medium" = "#F4A82A",
  "High"   = "#D95F2B"
)

# ── DISTANCE BAND LEVELS ──────────────────────────────────────────────────────
# Single source of truth for distance band ordering across all scripts.
dist_band_levels <- c("0–5 km", "5–10 km", "10–20 km", "20–50 km", ">50 km")
