#
# Authors: Dan Wismer
#
# Date: Jan, 2023
#
# Description: Script to convert the extracted values in a planning unit grid
# to rasters to be passed to WTW.
#
# Inputs:  1. Planning unit shapefile where each row is a planning unit and
#             each column is a dataset named using the unique_id from the meta
#             data table. Values represent the amount of the feature in the
#             planning unit (in units matching the meta data units).
#          2. Meta data table listing all datasets and their attributes.
#          3. The PU tif where all PU values are zero's (i.e. PU/PU0.tif)
#
# Outputs: 1. Converts the planning units to a raster for each column, and saves
#             into the Tiffs folder.
#
# Threshold: Note that if a value is entered in the meta data column 'threshold'
# then all cells where the threshold value is met or exceeded will be converted
# to have a value of the full cell area. Cells missing the threshold become zero.
# This can be used for finer control of things like 'Includes' data where we may
# want to lock in cells with >=50% coverage of protected areas. Without doing
# this the default behavior of 'Including' any cell with a value >0 would be
# used which might not be what we want.
# ===============================================================================

library(sf)
library(dplyr)
library(raster)
library(fasterize)

grid_to_raster <- function(project_params) {
  list2env(project_params, environment())

  # Setup ------------------------------------------------------------------------

  # Read the meta data table
  metadata <- read_csv(file.path(project_folder, paste0("WTW/metadata/", proj_name, "_metadata.csv")))

  # Read vector grid with extracted variables
  vector_grid <- read_sf(file.path(project_folder, "Regional/Extractions/planning_unit_values.shp"))

  # Read-in raster grid
  raster_grid <- raster(file.path(project_folder, "PU/PU0.tif"))


  # Processing -------------------------------------------------------------------

  # Get field names to rasterize
  fields <- colnames(vector_grid)

  # Remove fields that are not in metadata.csv
  fields <- fields[fields %in% metadata$unique_id]

  # Rasterize variables
  for (i in seq_along(fields)) {
    print(paste0("Rasterize ", i, " of ", length(fields), ": ", fields[i]))
    ## extract row index from metadata csv
    idx <- which(metadata$unique_id == fields[i])
    ## extract shp name
    tiff_name <- metadata[idx, ]$File
    ## extract type
    threshold <- metadata[idx, ]$threshold

    ## rasterize variable
    x <- fasterize(vector_grid, raster_grid, field = fields[i])

    print("Name: ")
    print(tiff_name)
    print("Max: ")
    print(cellStats(x, "max"))

    ## back fill 0 values
    tiff <- raster::mosaic(x, raster_grid, fun = "max")
    ## create binary variable
    if (!is.na(threshold)) {
      tiff[tiff < threshold] <- 0
      tiff[tiff >= threshold] <- 10
    }

    ## write to disk if there are values in raster
    if (tiff@data@max > 0) {
      writeRaster(tiff, paste0(file.path(project_folder, "Tiffs/", tiff_name)), overwrite = TRUE)
    } else {
      warning(paste0("Layer: ", tiff_name, " has no values, change threshold or remove the layer row from metadata."))
    }
  }
}
