#
# Authors: Marc Edwards
#
# Date: April 24th, 2023
#
# Description: Example script for summarizing non-standard data into the planning unit grids.
# i.e. a data type that does not require simply summing the area, length or count of
# polygon, line, or point data.

# In this example we will calculate the weighted average of stream strahler order per
# planning unit grid cell. This will be used a weight in WTW.

# The goal is to add a new column with the calculated values to the planning units
# grid that we already added extracted values to. The column name should link
# to the layers meta data in the metadata table.

# Once complete, we need to fill out the new row in the metadata table for our
# new layer.
#
# ===============================================================================

library(sf)
library(dplyr)

complex_vector_variables_to_grid <- function(project_params) {
  list2env(project_params, environment())

  # Setup ------------------------------------------------------------------------

  # Read in dataset - note that this data has already been intersected with the
  # planning unit grid using intersect_regional_vectors.py
  in_data <- read_sf(file.path(project_folder, "Regional/Intersections.gdb", "T_NBHN_Watercourses"))

  # Read-in metadata csv
  metadata <- read.csv(file.path(project_folder, paste0("WTW/metadata/", proj_name, "_metadata.csv")))

  # Set out path for grid containing values as columns
  pu_path <- file.path(project_folder, "Regional/Extractions/planning_unit_values.shp")
  vector_grid <- st_read(pu_path)


  # Summarize data ---------------------------------------------------------------

  # First calculate the length of each order value in each planning unit
  x <- in_data |>
    mutate(stream_length_m = as.numeric(st_length(.))) |>
    group_by(PUID, STREAMORDE) |>
    summarise(length_m = sum(stream_length_m))

  # Then calculate weighted average per PUID
  x <- x |>
    st_drop_geometry() |>
    group_by(PUID) |>
    summarise(wgt_av_order = round(sum(STREAMORDE * (length_m / sum(length_m))), 1))


  # Edit metadata ----------------------------------------------------------------

  # Add a new row to the metadata with a new unique_id
  unique_id <- paste0("ID_", max(as.numeric(gsub("ID_", "", metadata$unique_id))) + 1)

  metadata <- rbind(metadata, rep(NA, ncol(metadata)))
  metadata$unique_id[nrow(metadata)] <- unique_id

  # save
  write_csv(metadata, metadata_path, append = FALSE, na = "")

  # Join result to planning units ------------------------------------------------

  # Use unique ID as column name to link to metadata
  names(x) <- c("PUID", unique_id)
  vector_grid <- left_join(vector_grid, x)

  # save
  write_sf(vector_grid, pu_path)


  ### USER NOW NEEDS TO FILL IN THE LAYER INFO IN THE METADATA TABLE -------------
}
