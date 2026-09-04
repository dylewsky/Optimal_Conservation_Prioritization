library(dplyr)
library(sf)
library(fs)
library(terra)
library(tidyterra)
library(ggplot2)
library(stringr)
library(prioritizr)
library(exactextractr)

source("WTW_Prioritizr_Utils.R")

project_dir <- file.path("wtw-data-prep", "WTW", "EHJV_10km_hex_grid")

processing_dir <- file.path(project_dir, "WTW", "Processing")
plot_dir <- file.path(processing_dir, "Plots")


spec_file <- file.path("wtw-data-prep", "Data", "IBP_Species_Abbreviations.csv")
spec_names <- read.csv(spec_file, header = TRUE)

WTW_data <- read_WTW(project_dir)

spatial_data <- WTW_data$spatial_data
attribute_data <- WTW_data$attribute_data
boundary_data <- WTW_data$boundary_data
metadata <- WTW_data$metadata

ehjv_file <- file.path("wtw-data-prep", "Data", "bcr_ehjv_2025.gpkg")
boundary <- read_sf(ehjv_file) |>
  st_transform(crs = crs(attribute_data)) |>
  st_buffer(10000)

FM_data_path <- file.path("wtw-data-prep", "Data", "Map of Forest Management in Canada", "Canada_MFv2020.tif")
if (!file.exists(FM_data_path)) {
  stop('Public/private lands data from the Canadian Forest Service not present. Download and extract to directory "wtw-data-prep/Data/Map of Forest Management in Canada" : https://open.canada.ca/data/en/dataset/d8fa9a38-c4df-442a-8319-9bbcbdc29060/resource/02db308a-3320-44b8-8b8e-48199f837f17')
}

FM_data <- rast(FM_data_path) |>
  project(crs(attribute_data), method = "near") |>
  crop(boundary, mask = TRUE)

FM_classes <- data.frame(value = c(0, 11, 12, 13, 20, 31, 32, 33, 40, 50, 100), class = c("Null", "Long-Term", "Short-Term", "Other", "Protected", "Federal Reserve", "Indian Reserve", "Restricted", "Treaty", "Private", "Water"))

levels(FM_data) <- list(FM_classes)

class_fracs <- exact_extract(FM_data, spatial_data, function(df) {
  df |>
    mutate(frac_total = coverage_fraction / sum(coverage_fraction)) |>
    group_by(X_index, value) |>
    summarize(freq = sum(frac_total))
}, summarize_df = TRUE, include_cols = "X_index", progress = FALSE)


pdf(file.path(plot_dir, paste0("private_land_coverage_distribution.pdf")))
hist(subset(class_fracs, value == 50)$freq, n = 64, main = "Fraction of Cell Designated Private")
title("Fraction of Cell Designated Private")
dev.off()

class_fracs_sf <- st_sf(merge(class_fracs, spatial_data, by = "X_index", all.y = TRUE))
class_fracs_sf <- merge(class_fracs_sf, FM_classes, by = "value", all.x = TRUE) |> arrange(X_index, value)

st_write(class_fracs_sf, file.path(processing_dir, "FM_coverage_fracs.shp"))
