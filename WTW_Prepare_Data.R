# Set path of Python environment with arcpy installed

arc_py3_path <- file.path("C:", "Program Files", "ArcGIS", "Pro", "bin", "Python", "envs", "arcgispro-py3")
if (!dir.exists(arc_py3_path)){
  arc_py3_path <- file.path("C:", "Users", "AppData", "Local", "Programs", "ArcGIS", "Pro", "bin", "Python", "envs", "arcgispro-py3")
}

if (!dir.exists(arc_py3_path)){
  arc_py3_path <- NA
  stop('ArcGIS installation not found: Set arc_py3_path manually to a directory containing a Python distribution with ArcPy installed')
}

Sys.setenv(RETICULATE_PYTHON = arc_py3_path)

library(reticulate)
library(stringr)

data_prep_dir <- 'wtw-data-prep'

source(file.path(data_prep_dir,"01_initiate_project.R"))
source(file.path(data_prep_dir,"02_aoi_to_custom_grid.R"))
source(file.path(data_prep_dir,"02_aoi_to_raster_grid.R"))
source(file.path(data_prep_dir,"02_aoi_to_custom_PU.R"))
source(file.path(data_prep_dir,"03_initialize_metadata.R"), chdir=TRUE)
source(file.path(data_prep_dir,"03b_metadata_set_thresholds.R"), chdir=TRUE)
source_python(file.path(data_prep_dir,"04a_check_vectors.py"))
source_python(file.path(data_prep_dir,"04b_intersect_regional_vectors.py"))
source(file.path(data_prep_dir,"05a_vector_variables_to_grid.R"))
source(file.path(data_prep_dir,"05b_complex_vector_variables_to_grid.R"))
source(file.path(data_prep_dir,"05c_raster_variables_to_grid.R"))
source(file.path(data_prep_dir,"06_grid_to_raster.R"))
source(file.path(data_prep_dir,"07_wtw_formatting.R"))
source(file.path(data_prep_dir,"07_wtw_formatting_shp.R"))
source_python(file.path(data_prep_dir,"08a_combined_TIF_metadata.py"))


# proj_name <- 'EHJV_Watersheds'
# proj_name <- 'EHJV_eBird_grid'
proj_name <- "EHJV_10km_hex_grid"


project_params <- list()

project_params[["proj_name"]] <- proj_name


if (proj_name == "EHJV_Watersheds") {
  project_params[["project_title"]] <- "EHJV: Watershed Planning Units"
  project_params[["project_folder"]] <- file.path(data_prep_dir,"WTW", proj_name)
  project_params[["data_folder"]] <- file.path(data_prep_dir,"WTW", "EHJV_base")
  project_params[["aoi_shp"]] <- file.path(data_prep_dir,"Data", "bcr_ehjv_2025.shp")
  project_params[["aoi_gpkg"]] <- file.path(data_prep_dir,"Data", "bcr_ehjv_2025.gpkg")
  project_params[["pu_dir"]] <- file.path(data_prep_dir,"Data", "Canada_Watersheds")
  project_params[["pu_type"]] <- "custom"
  project_params[["reference_rast"]] <- file.path(data_prep_dir,"Data", "spatialData", "eBird", "2023", "ambduc", "seasonal", "ambduc_abundance_seasonal_mean_3km_2023.tif")
  project_params[["grid_size"]] <- res(rast(project_params[["reference_rast"]]))[[1]]
  project_params[["project_crs"]] <- crs(rast(project_params[["reference_rast"]]))

  if (!dir.exists(file.path("Data", "Canada_Watersheds"))) {
    stop("Watershed boundary data not present. Download from ESRI Canada and extract to Data/Canada_Watersheds directory: https://www.arcgis.com/home/item.html?id=c735874e9a5b4d04bbc3d2ca13ff6f6f")
  }
} else if (proj_name == "EHJV_eBird_grid") {
  project_params[["project_title"]] <- "EHJV: 3km eBird Grid Units"
  project_params[["project_folder"]] <- file.path(data_prep_dir,"WTW", proj_name)
  project_params[["data_folder"]] <- file.path(data_prep_dir,"WTW", "EHJV_base")
  project_params[["aoi_shp"]] <- file.path(data_prep_dir,"Data", "bcr_ehjv_2025.shp")
  project_params[["aoi_gpkg"]] <- file.path(data_prep_dir,"Data", "bcr_ehjv_2025.gpkg")
  project_params[["pu_dir"]] <- NA
  project_params[["pu_type"]] <- "raster_grid"
  project_params[["reference_rast"]] <- file.path(data_prep_dir,"Data", "spatialData", "eBird", "2023", "ambduc", "seasonal", "ambduc_abundance_seasonal_mean_3km_2023.tif")
  project_params[["grid_size"]] <- res(rast(project_params[["reference_rast"]]))[[1]]
  project_params[["project_crs"]] <- crs(rast(project_params[["reference_rast"]]))
} else if (endsWith(proj_name, "hex_grid")) {
  grid_size_km <- str_extract_all(proj_name, "[0-9]+km")[[1]] |>
    strsplit("\\D+") |>
    as.numeric()

  grid_size_m <- 1000 * grid_size_km

  project_params[["project_title"]] <- "EHJV: 10km Hex Grid Units"
  project_params[["project_folder"]] <- file.path(data_prep_dir,"WTW", proj_name)
  project_params[["data_folder"]] <- file.path(data_prep_dir,"WTW", "EHJV_base")
  project_params[["aoi_shp"]] <- file.path(data_prep_dir,"Data", "bcr_ehjv_2025.shp")
  project_params[["aoi_gpkg"]] <- file.path(data_prep_dir,"Data", "bcr_ehjv_2025.gpkg")
  project_params[["pu_dir"]] <- NA
  project_params[["pu_type"]] <- "hex_grid"
  project_params[["reference_rast"]] <- file.path(data_prep_dir,"Data", "spatialData", "eBird", "2023", "ambduc", "seasonal", "ambduc_abundance_seasonal_mean_3km_2023.tif")
  project_params[["grid_size"]] <- grid_size_m
  project_params[["project_crs"]] <- crs(rast(project_params[["reference_rast"]]))
}

if (project_params[["pu_type"]] %in% c("hex_grid", "square_grid")) {
  project_params[["binary_area_threshold"]] <- 0.5 # fraction of PU area
}


initiate_project(project_params)

if (project_params[["pu_type"]] == "custom") {
  aoi_to_custom_PU(project_params)
} else if (project_params[["pu_type"]] == "raster_grid") {
  aoi_to_raster_grid(project_params)
} else if (project_params[["pu_type"]] == "hex_grid") {
  aoi_to_custom_grid(project_params)
}

initialize_metadata(project_params)
metadata_set_thresholds(project_params)

# check_vectors(project_params[['project_folder']],project_params[['data_folder']])
intersect_regional_vectors(project_params[["project_folder"]], project_params[["data_folder"]])

vector_variables_to_grid(project_params)

raster_variables_to_grid(project_params)

grid_to_raster(project_params)

if ((project_params[["pu_type"]] == "custom") || (project_params[["pu_type"]] == "hex_grid")) {
  wtw_formatting_shp(project_params)
} else if ((project_params[["pu_type"]] == "raster_grid")) {
  wtw_formatting(project_params)
  combine_TIF_metadata(project_params[["project_folder"]], project_params[["proj_name"]], project_params[["reference_rast"]])
}
