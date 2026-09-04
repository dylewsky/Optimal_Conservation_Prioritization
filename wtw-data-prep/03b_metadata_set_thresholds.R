#
# Authors: Marc Edwards
#
# Date: April 24th, 2023
#
# Description: Initialize the meta data table using all tifs and .gdb feature
# classes in the root folder. Attempts to guess the metadata values where
# possible. User needs to QC and complete the metadata table manually before
# proceeding.
#
# Inputs:  1. project name for output csv
#          2. file paths to themes, includes, excludes and weights data
#             all input data should be placed in the correct folder and be in
#             .tif format for rasters, or as feature classes within a .gdb for
#             vectors. For theme data, all data for a given theme should be in
#             a subfolder where the subfolder name is the Theme name (e.g.
#             Regional/Themes/Forest)
#          3. The areal units and area value for planning units
#             (e.g. for a 10 km x 10 km grid we would use 'km2' and '10')
#
# Outputs: 1. Meta data csv
#
# ===============================================================================


# ONCE INITIALIZED, MANUALLY COMPLETE THE METADATA TABLE


library(sf)
library(dplyr)
library(readr)
library(tools)
source("functions/fct_init_metadata.R")
source("functions/fct_get_dirs.R")

# Setup ------------------------------------------------------------------------

metadata_set_thresholds <- function(project_params) {
  list2env(project_params, environment())

  # Read-in metadata csv
  metadata_path <- file.path(project_folder, paste0("WTW/metadata/", proj_name, "_metadata.csv"))
  metadata <- read.csv(metadata_path)

  grid <- read_sf(file.path(project_folder, "PU/PU.shp"))

  fields <- metadata$unique_id[metadata$Unit != ""]

  for (field in fields) {
    this_md <- subset(metadata, unique_id == field)
    this_unit <- this_md$Unit
    grid_areas <- as.numeric(set_units(set_units(st_area(grid), m^2), this_unit, mode = "standard"))

    if ((max(grid_areas) - min(grid_areas)) / mean(grid_areas) > 1e-6) {
      print("Nonuniform grid areas")
    } else {
      grid_area <- mean(grid_areas)
    }
    metadata$threshold[metadata$unique_id == field] <- grid_area * binary_area_threshold
  }


  write_csv(metadata, metadata_path, append = FALSE, na = "")
}
