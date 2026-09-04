#
# Authors: Marc Edwards
#
# Date: April 4th, 2023
#
# Description: This script sets up the folder structure for generating input
#              data for a Where To Work project
#
# Inputs:  1. The project folder path
#          2. An AOI shapefile
#          3. The data type for the project (National or regional)
#
# Outputs: 1. Project folder structure
#
# ===============================================================================


# See https://github.com/NCC-CNC/wtw-data-prep for an explanation of the
# various workflows

library(sf)

initiate_project <- function(project_params) {
  # 01 Set parameters ------------------------------------------------------------

  list2env(project_params, environment())

  project_data_type <- "REGIONAL" # NATIONAL or REGIONAL or BOTH


  # 02 Processing ----------------------------------------------------------------

  # create folder structure
  dir.create(file.path(project_folder, "PU"), recursive = TRUE)
  dir.create(file.path(project_folder, "scripts"), recursive = TRUE)
  dir.create(file.path(project_folder, "Tiffs"), recursive = TRUE)
  dir.create(file.path(project_folder, "WTW/metadata"), recursive = TRUE)

  if (project_data_type == "NATIONAL") {
    dir.create(file.path(project_folder, "National"), recursive = TRUE)
  }
  if (project_data_type == "REGIONAL") {
    dir.create(file.path(project_folder, "Regional"), recursive = TRUE)
  }
  if (project_data_type == "BOTH") {
    dir.create(file.path(project_folder, "Regional"), recursive = TRUE)
    dir.create(file.path(project_folder, "National"), recursive = TRUE)
  }

  # Copy AOI into PU folder
  x <- st_read(aoi_shp)
  st_write(x, file.path(project_folder, "PU/AOI.shp"), append = FALSE)
}
