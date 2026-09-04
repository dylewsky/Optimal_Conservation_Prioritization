# load packages
library(dplyr)
library(prioritizr)
library(prioritizrdata)
library(terra)
library(sf)
library(fs)
library(tidyterra)
library(ggplot2)
library(RStoolbox)
library(ebirdst)

set_ebirdst_access_key("YOUR EBIRD ACCESS KEY HERE", overwrite = TRUE)


ehjv_file <- file.path("wtw-data-prep", "Data", "bcr_ehjv_2025.gpkg")
ehjv <- read_sf(ehjv_file)

spec_file <- file.path("wtw-data-prep", "Data", "IBP_Species_Abbreviations.csv")
spec_names <- read.csv(spec_file, header = TRUE)

target_species <- subset(spec_names, spec_names$Priority != "")

download_dir <- file.path("wtw-data-prep", "Data", "spatialData", "eBird")
if (!dir.exists(download_dir)) {
  dir.create(download_dir, recursive = TRUE)
}

# list available files
ebirdst_download_status(target_species$eBird_Abbrev[1],
  path = download_dir,
  download_ranges = TRUE,
  dry_run = TRUE
)

# Download seasonal abundances + full year

for (sj in seq_len(nrow(target_species))) {
  # Download range
  tryCatch(
    {
      ebirdst_download_status(target_species$eBird_Abbrev[sj],
        pattern = "range_raw_9km_",
        path = download_dir,
        download_ranges = TRUE
      )
    },
    error = function(e) {
      print(paste0("Range data not found for ", target_species$eBird_Abbrev[sj]))
    }
  )

  # Download seasonal abundance
  tryCatch(
    {
      ebirdst_download_status(target_species$eBird_Abbrev[sj],
        pattern = "abundance_seasonal_mean_3km_",
        path = download_dir
      )
    },
    error = function(e) {
      print(paste0("Seasonal data not found for ", target_species$eBird_Abbrev[sj]))
    }
  )


  # Download full-year data
  tryCatch(
    {
      ebirdst_download_status(target_species$eBird_Abbrev[sj],
        pattern = "abundance_full-year_mean_3km_",
        path = download_dir,
        force = FALSE # if true, download + overwrite existing files
      )
    },
    error = function(e) {
      print(paste0("Full year data not found for ", target_species$eBird_Abbrev[sj]))
    }
  )
}


# # Download weekly abundances
#
# for (sj in seq_len(nrow(target_species))){
#   for (metric in c("median","lower","upper")){
#     ebirdst_download_status(target_species$eBird_Abbrev[sj], pattern = paste0('.*abundance_',metric,'.*3km.*'), path = download_dir)
#   }
#
# }


# Organize data to be read into WTW processing pipeline (Crop to EHJV area and separate by season)

project_folder <- file.path("wtw-data-prep", "WTW", "EHJV_base")
out_dir <- file.path(project_folder, "Regional/Themes") # <--- Themes data folder
if (!dir.exists(out_dir)) {
  dir.create(out_dir, recursive = TRUE)
}

spec_file <- file.path("wtw-data-prep", "Data", "IBP_Species_Abbreviations.csv")
spec_names <- read.csv(spec_file, header = TRUE)

source_dir <- download_dir

source_rasts <- list.files(path = source_dir, pattern = ".*_seasonal_.*.tif$", full.names = TRUE, recursive = TRUE)

reference_rast <- source_rasts[1]

boundary <- read_sf(ehjv_file) |>
  st_transform(crs = crs(rast(reference_rast)))

min_rast_thresh <- 0.01

priority_scheme <- "EHJV"

for (srast in source_rasts) {
  this_rast <- rast(srast) |> crop(boundary, mask = TRUE)

  for (sample_season in c("breeding", "nonbreeding", "prebreeding_migration", "postbreeding_migration")) {
    outfile <- basename(srast)
    outfile <- gsub("seasonal", sample_season, outfile)

    this_species <- strsplit(outfile, "_")[[1]][1]

    if (priority_scheme == "EHJV") {
      spec_priority <- subset(spec_names, eBird_Abbrev == this_species)$Priority
    } else if (priority_scheme == "NAWCA") {
      spec_priority <- subset(spec_names, eBird_Abbrev == this_species)$NAWCA_Priority
    }

    if (spec_priority == "") {
      next # skip species not present in the priority scheme
    }

    if (sample_season %in% names(this_rast)) {
      season_rast <- subset(this_rast, sample_season)
    } else if ("resident" %in% names(this_rast)) {
      season_rast <- subset(this_rast, "resident")
    } else {
      print(paste0("No ", sample_season, " season data available for ", srast))
    }

    if (global(season_rast, "max", na.rm = TRUE) < min_rast_thresh) {
      print(paste0("Species not present during ", sample_season, " season: ", srast))
      print(paste0("[Max value = ", global(season_rast, "max", na.rm = TRUE), "]"))
    } else {
      this_out_dir <- file.path(out_dir, paste0(sample_season, "_abundance_eBird_", spec_priority))

      if (!dir.exists(this_out_dir)) {
        dir.create(this_out_dir, recursive = TRUE)
      }

      if (!file.exists(file.path(this_out_dir, outfile))) {
        print(paste0("Writing ", outfile))
        writeRaster(season_rast, file.path(this_out_dir, outfile), overwrite = TRUE)
      }
    }
  }
}
