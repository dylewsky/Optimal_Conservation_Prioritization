#
# Authors: Marc Edwards
#
# Date: April 24th, 2023
#
# Description: Script to calculate the area, length and count of polygon, line
# and point data that has been intersected with the planning units.
#
# This script automates the calculation of:
#   area per planning unit - for polygon data
#   length per planning unit - for line data
#   count per planning unit - for point data
#
# Any other data summaries need to be done with a separate script, see script
# 05b_complex_vector_variables_to_grid.R for an example.
#
# Data values are summed per planning unit and added to the planning unit vector
# file. The planning unit file contains one column per input dataset and one row
# per planning unit ID. Columns are named using the unique_id value specified in
# the meta data table.
#
# Inputs:  1. QC'd meta data csv
#          2. Shapefile of planning units
#          3. .gdb containing all vector datasets intersected to planning units
#          4. Area and length units to use in calculations
#             (using units::set_units())
#          5. Output path for shapefile containing all calculated values
#
# Outputs: 1. Shapefile of planning units with columns containing all calculated
#             values. Columns are named using the unique_id value from the
#             metadata table.
#
# ===============================================================================

library(sf)
library(dplyr)
library(units)
library(tools)

# Setup ------------------------------------------------------------------------

vector_variables_to_grid <- function(project_params) {
  list2env(project_params, environment())

  # Read-in metadata csv
  metadata <- read.csv(file.path(project_folder, paste0("WTW/metadata/", proj_name, "_metadata.csv")))

  # Read-in vector grid
  grid <- read_sf(file.path(project_folder, "PU/PU.shp"))

  # Set intersections path
  intersections_gdb <- file.path(project_folder, "Regional/intersections.gdb")

  # Set units to calculate area and lengths
  area_unit <- "km^2" # e.g. "hectares", "ha", "km^2", "km2"
  length_unit <- "km" # e.g. "km", "kilometers", "m"

  # Set out path for grid containing values as columns
  out_shp <- file.path(project_folder, "Regional/Extractions/planning_unit_values.shp")


  # Read-in intersection layer metadata
  layer_meta <- st_layers(intersections_gdb)
  layer_names <- layer_meta$name # <--- Edit this list of names to change the layers that will be processed (e.g. remove any layers where required value is not a simple area/length/count)


  # Processing -------------------------------------------------------------------

  # Extract to the grid
  for (i in seq_along(layer_names)) {
    print(paste0("Extracting ", i, " of ", length(layer_names), ": ", layer_names[i]))

    # extract row index from metadata csv
    idx <- which(file_path_sans_ext(metadata$File) == layer_names[i])
    ## extract unique_id
    unique_id <- metadata[idx, ]$unique_id

    ## read-in feature class
    print("... reading-in feature class")
    fc <- st_read(intersections_gdb, layer_names[i], quiet = TRUE)

    ## calculate area length or count
    if (layer_meta$geomtype[[i]] %in% c("Multi Polygon", "3D Measured Multi Polygon")) {
      fc$metric <- as.numeric(set_units(st_area(fc), area_unit, mode = "standard"))
    } else if (identical(layer_meta$geomtype[[i]], "Multi Line String")) {
      fc$metric <- as.numeric(set_units(st_length(fc), length_unit, mode = "standard"))
    }

    print("... summarizing metric")
    if (identical(layer_meta$geomtype[[i]], "Point")) {
      ### point count (count all observations)
      metric_to_join <- fc |>
        st_drop_geometry() |>
        group_by(PUID) |>
        summarise(!!unique_id := n())
    } else {
      ### polygons and lines
      metric_to_join <- fc |>
        st_drop_geometry() |>
        group_by(PUID) |>
        dplyr::summarise(!!unique_id := round(sum(metric), 2))
    }

    ## join metric back to the grid
    print("... joining metric to the grid")
    grid <- left_join(grid, metric_to_join)
  }

  # convert NA to zero otherwise WTW throws error when importing as shp
  for (i in names(grid)[grepl("ID_", names(grid))]) {
    grid[[i]][is.na(grid[[i]])] <- 0
  }


  # Write extracted grid to disk
  if (!dir.exists(dirname(out_shp))) {
    dir.create(dirname(out_shp))
  }
  write_sf(grid, out_shp)
}
