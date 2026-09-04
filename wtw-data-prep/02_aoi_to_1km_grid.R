#
# Author: Dan Wismer
#
# Date: October 2nd, 2024
#
# Description: Generates vector and raster 1km PU's from a aoi shapefile.
#              Outputs take the extent of the input shapefile aoi.
#
# Inputs:  1. NAT_1KM data
#          2. Input shapefile aoi
#          3. Output folder
#
# Outputs: 1. 1km vector grid with NCCID and PUID
#          2. 1km raster grid (values are all 1)
#          3. 1km raster grid (values are all 0)
#
# Tested on R Versions: 4.4.1
#
# ===============================================================================


# 1.0 Load packages ------------------------------------------------------------

library(terra)
library(sf)
library(dplyr)

# 2.0 Set up -------------------------------------------------------------------

aoi_to_grid <- function(project_params) {
  list2env(project_params, environment())

  # Start timer
  start_time <- Sys.time()

  # Input boundary shapefile (aoi) path
  AOI <- aoi_shp

  # Output folder
  OUTPUT <- file.path(project_folder, "PU") # <- CHANGE TO YOUR OUTPUT FOLDER PATH. POINT TO "PU" FOLDER.

  # Read-in 1km index grid
  IDX_PATH <- reference_rast
  IDX <- rast(IDX_PATH)

  # 3.0 Processing ---------------------------------------------------------------

  # Read-in boundary shapefile
  aoi <- read_sf(AOI) |>
    st_transform(crs = st_crs(IDX)) # project to Canada_Albers_WGS_1984

  # Rasterize boundary polygon: 4700 rows, 5700 cols, 26790000 cells
  pu_1km <- aoi |>
    mutate(BURN = 1) |>
    st_buffer(1000) |> # buffer by 1km
    rasterize(IDX, "BURN")

  # Raster 1km grid, cell values are NCC indexes, mask values to aoi
  r_pu <- mask((pu_1km * IDX), vect(aoi))

  # Vector 1km grid
  v_pu <- st_as_sf(as.polygons(r_pu)) |>
    rename(EBD_ID = lyr1) |>
    mutate(PUID = row_number()) |>
    write_sf(file.path(OUTPUT, "PU.shp"), overwrite = TRUE)

  # Create raster template matching vector grid extent
  r_pu_template <- rast(vect(v_pu), res = 1000)

  # Rasterize vector grid, values are all 1
  r_pu <- rasterize(vect(v_pu), r_pu_template, 1) |>
    writeRaster(file.path(OUTPUT, "PU.tif"), datatype = "INT1U", overwrite = TRUE)

  # Convert all cell values to 0
  r_pu[r_pu > 0] <- 0
  writeRaster(r_pu, file.path(OUTPUT, "PU0.tif"), datatype = "INT1U", overwrite = TRUE)

  # End timer
  end_time <- Sys.time()
  end_time - start_time

  # Remove objects
  rm(list = ls())
  gc()
}
