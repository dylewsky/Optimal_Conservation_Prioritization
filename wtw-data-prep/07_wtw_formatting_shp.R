#
# Authors: Dan Wismer & Jeffrey Hanson
#
# Date: September 30th, 2022
#
# Description: This script generates the 4 mandatory files required to
#              upload project data into Where To Work
#
# Inputs:  1. A metadata.csv that defines attributes of each raster layer
#          2. A folder of rasters (themes, weights, includes and excludes)
#             that all have the same spatial properties (cell size, extent, CRS)
#          3. Required R libraries
#
# Outputs: 1. Configuration.yaml
#          2. Spatial.tif
#          3. Attribute.csv.gz
#          3. Boundary.csv.gz
#
#
# 1.0 Load packages ------------------------------------------------------------

wtw_formatting_shp <- function(project_params) {
  list2env(project_params, environment())

  ## Package names
  packages <- c("raster", "dplyr")

  ## Install packages not yet installed
  installed_packages <- packages %in% rownames(installed.packages())
  if (any(installed_packages == FALSE)) {
    install.packages(packages[!installed_packages])
  }

  ## Install wheretowork if not yet installed
  if (!require(wheretowork)) {
    if (!require(remotes)) install.packages("remotes")
    remotes::install_github("NCC-CNC/wheretowork")
  }

  ## Load packages
  library(raster)
  library(dplyr)
  library(wheretowork)

  # 2.0 Set up -------------------------------------------------------------------

  ## Set path where a QC'd metadata.csv version is located
  metadata_path <- file.path(project_folder, paste0("WTW/metadata/", proj_name, "_metadata.csv"))

  ## Set output variables
  prj_name <- project_title
  prj_file_name <- proj_name
  author_name <- "Daniel Dylewsky"
  author_email <- "DanielDylewsky@cunet.carleton.ca"

  ## Import formatted csv (metadata) as tibble
  metadata <- tibble::as_tibble(
    utils::read.table(
      metadata_path,
      stringsAsFactors = FALSE, sep = ",", header = TRUE,
      comment.char = "", quote = "\""
    )
  )

  metadata$Labels[is.na(metadata$Labels)] <- ""


  ## Assort by order column - optional
  metadata <- dplyr::arrange(metadata, Order)

  ## Validate metadata
  assertthat::assert_that(
    all(metadata$Type %in% c("theme", "include", "weight", "exclude"))
  )

  # Import planning units shp
  shp_data <- st_read(file.path(project_folder, "Regional/Extractions/planning_unit_values.shp")) # <--- CHANGE PATH HERE FOR NEW PROJECT

  # 4.0 Pre-processing -----------------------------------------------------------

  ## Prepare theme inputs ----
  theme_data <- shp_data[metadata$unique_id[which(metadata$Type == "theme")]] |> st_drop_geometry()
  theme_names <- metadata$Name[metadata$Type == "theme"]
  theme_groups <- metadata$Theme[metadata$Type == "theme"]
  theme_colors <- metadata$Color[metadata$Type == "theme"]
  theme_units <- metadata$Unit[metadata$Type == "theme"]
  theme_visible <- metadata$Visible[metadata$Type == "theme"]
  theme_provenance <- metadata$Provenance[metadata$Type == "theme"]
  theme_hidden <- metadata$Hidden[metadata$Type == "theme"]
  theme_legend <- metadata$Legend[metadata$Type == "theme"]
  theme_labels <- metadata$Labels[metadata$Type == "theme"]
  theme_values <- metadata$Values[metadata$Type == "theme"]
  theme_goals <- metadata$Goal[metadata$Type == "theme"]

  ## Prepare weight inputs ----
  weight_data <- shp_data[metadata$unique_id[which(metadata$Type == "weight")]] |> st_drop_geometry()
  weight_names <- metadata$Name[metadata$Type == "weight"]
  weight_colors <- metadata$Color[metadata$Type == "weight"]
  weight_units <- metadata$Unit[metadata$Type == "weight"]
  weight_visible <- metadata$Visible[metadata$Type == "weight"]
  weight_hidden <- metadata$Hidden[metadata$Type == "weight"]
  weight_provenance <- metadata$Provenance[metadata$Type == "weight"]
  weight_legend <- metadata$Legend[metadata$Type == "weight"]
  weight_labels <- metadata$Labels[metadata$Type == "weight"]
  weight_values <- metadata$Values[metadata$Type == "weight"]

  ## Prepare include inputs ----
  include_data <- shp_data[metadata$unique_id[which(metadata$Type == "include")]] |> st_drop_geometry()
  for (i in names(st_drop_geometry(include_data))) {
    this_thresh <- metadata$threshold[metadata$unique_id == i]
    include_data[[i]] <- ifelse(include_data[[i]] > this_thresh, 1, 0) # set includes to 0/1 based on threshold
  }
  include_names <- metadata$Name[metadata$Type == "include"]
  include_colors <- metadata$Color[metadata$Type == "include"]
  include_units <- metadata$Unit[metadata$Type == "include"]
  include_visible <- metadata$Visible[metadata$Type == "include"]
  include_provenance <- metadata$Provenance[metadata$Type == "include"]
  include_legend <- metadata$Legend[metadata$Type == "include"]
  include_labels <- metadata$Labels[metadata$Type == "include"]
  include_hidden <- metadata$Hidden[metadata$Type == "include"]

  ## Prepare exclude inputs ----
  exclude_data <- shp_data[metadata$unique_id[which(metadata$Type == "exclude")]] |> st_drop_geometry()
  if (length(st_drop_geometry(exclude_data)) > 0) {
    for (i in names(st_drop_geometry(exclude_data))) {
      this_thresh <- metadata$threshold[metadata$unique_id == i]
      exclude_data[[i]] <- ifelse(exclude_data[[i]] > this_thresh, 1, 0) # set excludes to 0/1 based on threshold
    }
    exclude_names <- metadata$Name[metadata$Type == "exclude"]
    exclude_colors <- metadata$Color[metadata$Type == "exclude"]
    exclude_units <- metadata$Unit[metadata$Type == "exclude"]
    exclude_visible <- metadata$Visible[metadata$Type == "exclude"]
    exclude_provenance <- metadata$Provenance[metadata$Type == "exclude"]
    exclude_legend <- metadata$Legend[metadata$Type == "exclude"]
    exclude_labels <- metadata$Labels[metadata$Type == "exclude"]
    exclude_hidden <- metadata$Hidden[metadata$Type == "exclude"]
  }

  # 5.0 Build wheretowork objects ------------------------------------------------

  ## Create data set ----
  dataset <- new_dataset_from_auto(
    cbind(shp_data["PUID"], theme_data, weight_data, include_data, exclude_data)
  )

  ## Create themes ----

  ### loop over unique theme groups (ex. Endemic Species, Species at Risk, etc.)
  themes <- lapply(seq_along(unique(theme_groups)), function(i) {
    #### store temp variables associated with group (i)
    curr_theme_groups <- unique(theme_groups)[i]
    curr_theme_data <- theme_data[which(theme_groups == curr_theme_groups)]
    curr_theme_data_names <- names(curr_theme_data)
    curr_theme_names <- theme_names[theme_groups == curr_theme_groups]
    curr_theme_colors <- theme_colors[theme_groups == curr_theme_groups]
    curr_theme_labels <- theme_labels[theme_groups == curr_theme_groups]
    curr_theme_units <- theme_units[theme_groups == curr_theme_groups]
    curr_theme_visible <- theme_visible[theme_groups == curr_theme_groups]
    curr_theme_hidden <- theme_hidden[theme_groups == curr_theme_groups]
    curr_theme_provenance <- theme_provenance[theme_groups == curr_theme_groups]
    curr_theme_legend <- theme_legend[theme_groups == curr_theme_groups]
    curr_theme_values <- theme_values[theme_groups == curr_theme_groups]
    curr_theme_goals <- theme_goals[theme_groups == curr_theme_groups]

    #### create list of features (j) associated with group
    curr_features <- lapply(seq_along(curr_theme_names), function(j) {
      #### create variable (if manual legend)
      if (identical(curr_theme_legend[j], "manual")) {
        v <- new_variable(
          dataset = dataset,
          index = curr_theme_data_names[j],
          units = curr_theme_units[j],
          total = raster::cellStats(curr_theme_data[[j]], "sum"),
          legend = new_manual_legend(
            values = c(as.numeric(trimws(unlist(strsplit(curr_theme_values[j], ","))))),
            colors = c(trimws(unlist(strsplit(curr_theme_colors[j], ",")))),
            labels = c(trimws(unlist(strsplit(curr_theme_labels[j], ","))))
          ),
          provenance = new_provenance_from_source(curr_theme_provenance[j])
        )

        #### create variable (if continuous legend)
      } else if (identical(curr_theme_legend[j], "continuous")) {
        v <- new_variable_from_auto(
          dataset = dataset,
          index = curr_theme_data_names[j],
          units = curr_theme_units[j],
          type = "continuous",
          # total = sum(curr_theme_data[[j]]), # this line absent from original code
          colors = curr_theme_colors[j],
          provenance = curr_theme_provenance[j],
          labels = "missing",
          hidden = curr_theme_hidden[j]
        )

        #### create variable (if null legend)
      } else if (identical(curr_theme_legend[j], "null")) {
        v <- new_variable(
          dataset = dataset,
          index = curr_theme_data_names[j],
          units = " ",
          total = raster::cellStats(curr_theme_data[[j]], "sum"),
          legend = new_null_legend(),
          provenance = new_provenance_from_source("missing")
        )
      }

      #### create new feature
      new_feature(
        name = curr_theme_names[j],
        goal = curr_theme_goals[j],
        current = 0,
        limit_goal = 0,
        visible = curr_theme_visible[j],
        hidden = curr_theme_hidden[j],
        variable = v
      )
    })

    #### create theme from list of features
    curr_theme <- new_theme(curr_theme_groups, curr_features)

    #### return theme
    curr_theme
  })


  ## Create weights ----

  ### loop over each raster in weight_data
  weights <- lapply(seq_along(weight_names), function(i) {
    #### prepare variable (if manual legend)
    if (identical(weight_legend[i], "manual")) {
      v <- new_variable_from_auto(
        dataset = dataset,
        index = names(weight_data)[i],
        units = weight_units[i],
        type = "manual",
        colors = trimws(unlist(strsplit(weight_colors[i], ","))),
        provenance = weight_provenance[i],
        labels = trimws(unlist(strsplit(weight_labels[i], ",")))
      )

      #### prepare variable (if null legend)
    } else if (identical(weight_legend[i], "null")) {
      v <- new_variable(
        dataset = dataset,
        index = names(weight_data)[i],
        units = " ",
        total = sum(weight_data[[i]]),
        legend = new_null_legend(),
        provenance = new_provenance_from_source("missing")
      )

      ### prepare variable (if continuous legend)
    } else if (identical(weight_legend[i], "continuous")) {
      v <- new_variable_from_auto(
        dataset = dataset,
        index = names(weight_data)[i],
        units = weight_units[i],
        type = "continuous",
        colors = weight_colors[i],
        provenance = weight_provenance[i]
      )
    }

    #### create weight
    new_weight(
      name = weight_names[i], variable = v, visible = weight_visible[i],
      hidden = weight_hidden[i]
    )
  })


  ## Create includes ----

  ### loop over each column in include_data
  includes <- lapply(seq_along(include_names), function(i) {
    ### Only include if there are 1's in include_data
    if (sum(include_data[[i]]) > 0) {
      ### build legend
      if (identical(include_legend[i], "null")) {
        legend <- new_null_legend()
      } else {
        these_colors <- trimws(unlist(strsplit(include_colors[i], ",")))
        these_labels <- unlist(strsplit(include_labels[i], ","))
        if (length(these_labels) == 0) {
          these_labels <- rep("", length(these_colors))
        }

        legend <- new_manual_legend(
          values = c(0, 1),
          colors = these_colors,
          labels = these_labels
        )
      }

      ### build include
      new_include(
        name = include_names[i],
        visible = include_visible[i],
        hidden = include_hidden[i],
        variable = new_variable(
          dataset = dataset,
          index = names(include_data)[i],
          units = " ",
          total = sum(include_data[[i]]),
          legend = legend,
          provenance = new_provenance_from_source(include_provenance[i])
        )
      )
    }
  })
  includes <- includes[!sapply(includes, is.null)] # drop the NULL list elements that have no 1's

  ## Create excludes ----

  ## Create excludes ----
  ### loop over each column in exclude_data
  excludes <- lapply(seq_along(names(st_drop_geometry(exclude_data))), function(i) {
    ### Only include if there are 1's in exclude_data
    if (sum(exclude_data[[i]]) > 0) {
      ### build legend
      if (identical(exclude_legend[i], "null")) {
        legend <- new_null_legend()
      } else {
        these_colors <- trimws(unlist(strsplit(exclude_colors[i], ",")))
        these_labels <- unlist(strsplit(exclude_labels[i], ","))
        if (length(these_labels) == 0) {
          these_labels <- rep("", length(these_colors))
        }

        legend <- new_manual_legend(
          values = c(0, 1),
          colors = these_colors,
          labels = these_labels
        )
      }

      ### build exclude
      new_exclude(
        name = exclude_names[i],
        visible = exclude_visible[i],
        hidden = exclude_hidden[i],
        variable = new_variable(
          dataset = dataset,
          index = names(exclude_data)[i],
          units = " ",
          total = sum(exclude_data[[i]]),
          legend = legend,
          provenance = new_provenance_from_source(exclude_provenance[i])
        )
      )
    }
  })
  excludes <- excludes[!sapply(excludes, is.null)] # drop the NULL list elements that have no 1's

  # 6.0  Export Where To Work objects --------------------------------------------
  print(paste0("Writing to disk: ", file.path(project_folder, paste0("WTW/", prj_file_name))))
  ## Save project to disk ---- <--- CHANGE FOR NEW PROJECT
  write_project(
    x = append(append(themes, append(includes, weights)), excludes),
    dataset = dataset,
    name = prj_name,
    path = file.path(project_folder, paste0("WTW/", prj_file_name, ".yaml")),
    spatial_path = file.path(project_folder, paste0("WTW/", prj_file_name, ".shp")),
    attribute_path = file.path(project_folder, paste0("WTW/", prj_file_name, "_attribute.csv.gz")),
    boundary_path = file.path(project_folder, paste0("WTW/", prj_file_name, "_boundary.csv.gz")),
    mode = "advanced",
    author_name = author_name,
    author_email = author_email
  )

  # 7.0 Clear R environment ------------------------------------------------------


  ## Comment these lines below to keep all the objects in the R session
  rm(list = ls())
  gc()
}
