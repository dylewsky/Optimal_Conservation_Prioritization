#
# Authors: Marc Edwards
#
# Date: April 24th, 2023
#
# Description: Script to sum raster values within each planning unit
#
# Two workflows are run in this script:
# 1) Sum the area of binary rasters
#    Source rasters in this case are binary and have values of 1 representing
#    presence of the feature.
#    This involves summing the raster values in each planning unit then
#    converting from the source cell area into the desired area units defined in
#    the meta data table for that layer.
# 2) Sum raster values without converting to an area.
#    In this case rasters have continuous values representing a non-area
#    attribute e.g. tonnes of carbon per pixel
#
# In this example we use the metadata 'Unit' column to split the rasters into
# two lists that feed into each of these workflows. Datasets with units
# in metadata matching the areal units defined in the script will be processed as
# areas, otherwise the values will be summed.

# If calculations other than the sum of values are required, a separate custom
# script will be needed, or this script can be edited to accommodate other
# calculations.

# Note: this script assumes the source data has a prj with units of meters

# Inputs:  1. QC'd meta data csv
#          2. Area units used in metadata (used to determine if raster represents
#             an area)
#          3. Planning unit shapefile to hold values (i.e. the output from
#             05a_vector_variables_to_grid.R)
#
# Outputs: 1. Planning unit shapefile with new columns added where column names
#             match unique_id values from metadata
# ===============================================================================

library(raster)
library(sf)
library(readr)
library(exactextractr)
library(units)

raster_variables_to_grid <- function(project_params) {
  list2env(project_params, environment())

  # Setup ------------------------------------------------------------------------
  # Read the meta data table
  metadata <- read_csv(file.path(project_folder, paste0("WTW/metadata/", proj_name, "_metadata.csv")))

  # Which units in metadata represent binary rasters?
  area_units <- c("km2", "ha")

  # Get list of binary rasters representing areas
  raster_list_sum_areas <- metadata$source[grepl(".tif$", metadata$source) & metadata$Unit %in% area_units]

  # Get list of rasters NOT representing areas
  raster_list_sum <- metadata$source[grepl(".tif$", metadata$source) & !metadata$Unit %in% area_units]

  # Read-in planning units - here we'll use pu's that we've already joined the
  # vector data to
  pu_path <- file.path(project_folder, "Regional/Extractions/planning_unit_values.shp")
  grid <- read_sf(pu_path)

  round_digits <- 2

  # Processing ------------------------------------------------------------------------

  # Process area rasters
  for (raster_path in raster_list_sum_areas) {
    # open raster
    r <- raster(raster_path)

    # get unique_id to use as col name
    unique_id <- metadata$unique_id[metadata$source == raster_path]
    metadata_units <- metadata$Unit[metadata$source == raster_path] # the area units to use for the raster values, defined in meta data table (note these can be different for different layers)

    # extract values to column in grid
    grid[[unique_id]] <- exact_extract(r, grid, fun = "sum")

    # convert to area units
    unit_conversion <- as.numeric(set_units(set_units(prod(res(r)), m^2), metadata_units, mode = "standard")) # get area of source cell in requested units, assumes source crs is in m
    grid[[unique_id]] <- round(grid[[unique_id]] * unit_conversion, round_digits)
  }

  # Process sum rasters
  for (raster_path in raster_list_sum) {
    # open raster
    r <- raster(raster_path)

    # get unique_id to use as col name
    unique_id <- metadata$unique_id[metadata$source == raster_path]

    # extract values to column in grid
    grid[[unique_id]] <- round(exact_extract(r, grid, fun = "sum"), round_digits)
  }

  # convert NA to zero otherwise WTW throws error when importing as shp
  for (i in names(grid)[grepl("ID_", names(grid))]) {
    grid[[i]][is.na(grid[[i]])] <- 0
  }

  # Write to disk
  write_sf(grid, pu_path, append = FALSE)
}
