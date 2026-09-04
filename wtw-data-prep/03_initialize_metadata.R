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

initialize_metadata <- function(project_params) {
  list2env(project_params, environment())

  themes_dir <- file.path(data_folder, "Regional/Themes") # <--- Themes data folder
  includes_dir <- file.path(data_folder, "Regional/Includes") # <--- Includes data folder
  excludes_dir <- file.path(data_folder, "Regional/Excludes") # <--- Excludes data folder
  weights_dir <- file.path(data_folder, "Regional/Weights") # <--- Weights data folder

  pu_units <- "km2" # <--- SET DESIRED UNITS
  pu_cell_area <- 1 # <--- SET PLANNING UNIT AREA IN units

  # Build vectors of data paths --------------------------------------------------

  themes_list <- get_all_tifs_gdbs(themes_dir)
  includes_list <- get_all_tifs_gdbs(includes_dir)
  excludes_list <- get_all_tifs_gdbs(excludes_dir)
  weights_list <- get_all_tifs_gdbs(weights_dir)

  # Fill table -------------------------------------------------------------------

  # make empty table
  df <- init_metadata()

  # Add Regional specific columns
  df$unique_id <- as.character()
  df$threshold <- as.character()
  df$source <- as.character()

  for (x in c(themes_list, includes_list, excludes_list, weights_list)) {
    # Get Type
    type <- case_when(
      x %in% themes_list ~ "theme",
      x %in% includes_list ~ "include",
      x %in% excludes_list ~ "exclude",
      x %in% weights_list ~ "weight"
    )

    # Get Theme
    theme <- case_when(x %in% themes_list ~ basename(get_parent_dir(x)),
      .default = ""
    )

    layer_name <- file_path_sans_ext(basename(x))

    # Get final file name
    file <- case_when(
      x %in% themes_list ~ paste0("T_", layer_name, ".tif"),
      x %in% includes_list ~ paste0("I_", layer_name, ".tif"),
      x %in% excludes_list ~ paste0("E_", layer_name, ".tif"),
      x %in% weights_list ~ paste0("W_", layer_name, ".tif")
    )

    # Guess legend
    legend <- case_when(x %in% themes_list ~ "continuous", # usually continuous
      x %in% includes_list ~ "manual", # usually binary data
      x %in% excludes_list ~ "manual", # usually binary data
      x %in% weights_list ~ "continuous", # usually continuous
      .default = "continuous"
    )

    # Guess values
    values <- case_when(legend == "manual" ~ paste0("0, ", pu_cell_area),
      .default = ""
    )

    # Guess colour
    # color <- case_when(legend == "manual" ~ "#00000000, #fb9a99",
    #                    .default = "")
    color <- case_when(legend == "manual" ~ "#00000000, #84dc53",
      .default = ""
    )


    # Guess units
    unit <- case_when(legend == "manual" ~ pu_units,
      .default = ""
    )

    # Get name
    name <- gsub("_", " ", layer_name)

    # Set theme goals
    goal <- case_when(type == "theme" ~ "0.2",
      .default = ""
    )

    provenance <- "regional"
    order <- ""
    labels <- ""
    threshold <- ""
    visible <- FALSE
    hidden <- FALSE
    source <- x
    id <- ""

    # add row
    new_row <- c(
      type, theme, file, name, legend, values, color, labels, unit,
      provenance, order, visible, hidden, goal, id, threshold, source
    )

    df <- structure(rbind(df, new_row), .Names = names(df))
  }

  # populate continuous colors, same color for each theme
  themes <- unique(df$Theme)
  theme_colours <- sample(c("Greens", "Reds", "viridis", "YlOrBr", "Blues", "mako", "PuBuGn", "rocket"), length(themes), replace = TRUE)
  for (i in seq_len(nrow(df))) {
    if (df$Legend[i] == "continuous") {
      df$Color[i] <- theme_colours[which(themes == df$Theme[i])]
    }
  }

  # populate unique ID
  df$unique_id <- paste0("ID_", seq(1:nrow(df)))

  # save
  out_csv <- file.path(project_folder, paste0("WTW/metadata/", proj_name, "_metadata.csv"))
  write_csv(df, out_csv, append = FALSE, na = "")
}
