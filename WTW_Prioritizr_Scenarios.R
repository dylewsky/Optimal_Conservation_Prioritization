library(dplyr)
library(sf)
library(fs)
library(tidyterra)
library(ggplot2)
library(stringr)
library(prioritizr)

source("WTW_Prioritizr_Utils.R")

project_dir <- file.path("wtw-data-prep", "WTW", "EHJV_10km_hex_grid")
processing_dir <- file.path(project_dir, "WTW", "Processing")
plot_dir <- file.path(processing_dir, "Plots")
if (!dir.exists(plot_dir)) {
  dir.create(plot_dir, recursive = TRUE)
}

spec_file <- file.path("wtw-data-prep", "Data", "IBP_Species_Abbreviations.csv")
spec_names <- read.csv(spec_file, header = TRUE)

spec_names <- get_season_data(spec_names)

spec_bottleneck_file <- file.path("wtw-data-prep", "Data", "Species_Season_Bottlenecks.csv")
spec_bottlenecks <- read.csv(spec_bottleneck_file, header = TRUE, check.names = FALSE) |>
  mutate(species = str_to_title(species)) |>
  mutate(across(c("breeding", "nonbreeding", "prebreeding migration", "postbreeding migration"), function(x) {
    ifelse(x == "X", 1, 0)
  })) |>
  select(c("species", "breeding", "nonbreeding", "prebreeding migration", "postbreeding migration")) |>
  rename_with(~ paste0("bottleneck_", sub(" ", "_", .x)), .cols = c("breeding", "nonbreeding", "prebreeding migration", "postbreeding migration"))

spec_names <- spec_names |>
  mutate(Common_Name = str_to_title(Common_Name)) |>
  left_join(spec_bottlenecks, by = join_by("Common_Name" == "species"))

WTW_data <- read_WTW(project_dir)

spatial_data <- WTW_data$spatial_data
attribute_data <- WTW_data$attribute_data
boundary_data <- WTW_data$boundary_data
metadata <- WTW_data$metadata


theme_ids <- subset(metadata, Type == "theme")["unique_id"]$unique_id

pu_data <- spatial_data |> mutate(cost = as.vector(10**(-6) * st_area(spatial_data["geometry"])))
feature_data <- attribute_data |> dplyr::select(theme_ids)
feature_metadata <- metadata[metadata$unique_id %in% theme_ids, ]


parse_source <- function(name) {
  tokens <- strsplit(name, "_")[[1]]
  tokens[which(tokens == "abundance") + 1]
}

feature_metadata$Source <- sapply(feature_metadata$Theme, parse_source, USE.NAMES = FALSE)


FL_to_EB <- function(FL) {
  this_spec_name <- subset(spec_names, Four_Letter == FL)$eBird_Abbrev
  if (length(this_spec_name) == 0) NA_character_ else this_spec_name
}

feature_metadata <- feature_metadata |>
  mutate(
    Species = word(Name, 1),
    Species = if_else(
      Source == "Frei",
      purrr::map_chr(Species, FL_to_EB),
      Species
    ),
    Season = case_when(
      Source == "eBird" ~ word(Name, 3),
      Source %in% c("Frei", "Adde") ~ "breeding",
      TRUE ~ ""
    ),
    Season = if_else((Season == "prebreeding" | Season == "postbreeding"),
      paste0(Season, " migration"),
      Season
    )
  )


feature_metadata <- merge(x = feature_metadata, y = spec_names, by.x = "Species", by.y = "eBird_Abbrev", all.x = TRUE)

feature_metadata <- feature_metadata |>
  mutate(bottleneck_include = ifelse((Season == "breeding" & bottleneck_breeding == 1) | (Season == "nonbreeding" & bottleneck_nonbreeding == 1) | (Season == "prebreeding migration" & bottleneck_prebreeding_migration == 1) | (Season == "postbreeding migration" & bottleneck_postbreeding_migration == 1),
    1, 0
  ))

season_duration_means <- colMeans(feature_metadata[c("breeding_duration", "nonbreeding_duration", "postbreeding_migration_duration", "prebreeding_migration_duration")], na.rm = TRUE)
season_duration_fracs <- season_duration_means / sum(season_duration_means)
for (ssn in c("breeding", "nonbreeding", "postbreeding migration", "prebreeding migration")) {
  dur_col <- paste0(gsub(" ", "_", ssn), "_duration")
  this_metadata <- subset(feature_metadata, (is_resident == TRUE) & (Season == ssn))

  these_durations <- this_metadata$resident_duration * as.numeric(season_duration_fracs[dur_col])

  feature_metadata[(feature_metadata$is_resident == TRUE) & (feature_metadata$Season == ssn), dur_col] <- these_durations
}

season_to_duration <- function(season) {
  paste0(gsub(" ", "_", Season), "_duration")
}

feature_metadata <- feature_metadata |> mutate(this_season_duration = case_when(
  Season == "breeding" ~ breeding_duration,
  Season == "nonbreeding" ~ nonbreeding_duration,
  Season == "prebreeding migration" ~ prebreeding_migration_duration,
  Season == "postbreeding migration" ~ postbreeding_migration_duration
))

problem_data <- st_join(pu_data, feature_data, join = st_equals)


# Only used if abundance data from Frei et. al. (2024) is available:

# Frei_IDs <- subset(feature_metadata, (Source == 'Frei') & (Priority == 'Waterfowl'))[['unique_id']]
# Frei_spatial_mask <- problem_data['geometry']
# Frei_spatial_mask['Present'] <- rowSums(problem_data[Frei_IDs] |> st_drop_geometry())
# Frei_spatial_mask <- Frei_spatial_mask |> mutate(Present = Present > 0)


# Sweep over preset scenarios

for (scenario in seq(1, 27)) {
  uniform_target_weight <- TRUE
  scen_source <- "eBird"
  scen_spatial_mask <- problem_data["geometry"] |> mutate(Present = 1)
  if (scenario == 1) {
    scen_label <- "Waterfowl | Breeding"
    scen_ids <- subset(feature_metadata, (Source == scen_source) & (Priority == "Waterfowl") & (Season == "breeding"))[["unique_id"]]
  } else if (scenario == 2) {
    scen_label <- "Waterfowl | Wintering"
    scen_ids <- subset(feature_metadata, (Source == scen_source) & (Priority == "Waterfowl") & (Season == "nonbreeding"))[["unique_id"]]
  } else if (scenario == 3) {
    scen_label <- "Waterfowl | Spring + Fall Migration"
    scen_ids <- subset(feature_metadata, (Source == scen_source) & (Priority == "Waterfowl") & (Season %in% c("prebreeding migration", "postbreeding migration")))[["unique_id"]]
  } else if (scenario == 4) {
    scen_label <- "Non-Waterfowl | Breeding"
    scen_ids <- subset(feature_metadata, (Source == scen_source) & (Priority == "Nonwaterfowl") & (Season == "breeding"))[["unique_id"]]
  } else if (scenario == 5) {
    scen_label <- "All Species | All Seasons"
    scen_ids <- subset(feature_metadata, (Source == scen_source) & (!is.na(Priority)))[["unique_id"]]
  } else if (scenario == 6) {
    scen_label <- "All Species | Breeding"
    scen_ids <- subset(feature_metadata, (Source == scen_source) & (!is.na(Priority)) & (Season == "breeding"))[["unique_id"]]
  } else if (scenario == 7) {
    scen_label <- "Waterfowl | All Seasons (Equally Weighted)"
    scen_ids <- subset(feature_metadata, (Source == scen_source) & (Priority == "Waterfowl"))[["unique_id"]]
  } else if (scenario == 8) {
    scen_label <- "Waterfowl | Pre-Breeding Migration"
    scen_ids <- subset(feature_metadata, (Source == scen_source) & (Priority == "Waterfowl") & (Season == "prebreeding migration"))[["unique_id"]]
  } else if (scenario == 9) {
    scen_label <- "Waterfowl | Post-Breeding Migration"
    scen_ids <- subset(feature_metadata, (Source == scen_source) & (Priority == "Waterfowl") & (Season == "postbreeding migration"))[["unique_id"]]
  } else if (scenario == 10) {
    scen_label <- "Non-Waterfowl | Wintering"
    scen_ids <- subset(feature_metadata, (Priority == "Nonwaterfowl") & (Season == "nonbreeding"))[["unique_id"]]
  } else if (scenario == 11) {
    scen_label <- "Non-Waterfowl | Pre-Breeding Migration"
    scen_ids <- subset(feature_metadata, (Source == scen_source) & (Priority == "Nonwaterfowl") & (Season == "prebreeding migration"))[["unique_id"]]
  } else if (scenario == 12) {
    scen_label <- "Non-Waterfowl | Post-Breeding Migration"
    scen_ids <- subset(feature_metadata, (Source == scen_source) & (Priority == "Nonwaterfowl") & (Season == "postbreeding migration"))[["unique_id"]]
  } else if (scenario == 13) {
    scen_label <- "Waterfowl | All Seasons (Proportionally Weighted)"
    scen_ids <- subset(feature_metadata, (Source == scen_source) & (Priority == "Waterfowl"))[["unique_id"]]
    scen_weights <- subset(feature_metadata, (Source == scen_source) & (Priority == "Waterfowl"))[["this_season_duration"]]
    scen_weights <- scen_weights / median(scen_weights)
    scen_weights <- matrix(scen_weights, nrow = length(scen_ids), ncol = 1, dimnames = list(scen_ids, "weight"))
    uniform_target_weight <- FALSE
  } else if (scenario == 14) {
    scen_label <- "Non-Waterfowl | All Seasons (Equally Weighted)"
    scen_ids <- subset(feature_metadata, (Source == scen_source) & (Priority == "Nonwaterfowl"))[["unique_id"]]
  } else if (scenario == 15) {
    scen_label <- "Non-Waterfowl | All Seasons (Proportionally Weighted)"
    scen_ids <- subset(feature_metadata, (Source == scen_source) & (Priority == "Nonwaterfowl"))[["unique_id"]]
    scen_weights <- subset(feature_metadata, (Source == scen_source) & (Priority == "Nonwaterfowl"))[["this_season_duration"]]
    scen_weights <- scen_weights / median(scen_weights)
    scen_weights <- matrix(scen_weights, nrow = length(scen_ids), ncol = 1, dimnames = list(scen_ids, "weight"))
    uniform_target_weight <- FALSE
  } else if (scenario == 16) {
    scen_label <- "Frei | Waterfowl | Breeding"
    scen_source <- "Frei"
    scen_spatial_mask <- Frei_spatial_mask
    scen_ids <- subset(feature_metadata, (Source == scen_source) & (Priority == "Waterfowl"))[["unique_id"]]
  } else if (scenario == 17) {
    scen_source <- "eBird"
    scen_species <- unique(subset(feature_metadata, Source == "Frei")$Species)
    scen_label <- "eBird Priority Waterfowl | Breeding | Frei Species"
    scen_ids <- subset(feature_metadata, (Source == scen_source) & (Species %in% scen_species) & (Season == "breeding"))[["unique_id"]]
  } else if (scenario == 18) {
    scen_source <- "eBird"
    scen_spatial_mask <- Frei_spatial_mask
    scen_species <- unique(subset(feature_metadata, Source == "Frei")$Species)
    scen_label <- "eBird Priority Waterfowl | Breeding | Frei Species + Spatial Domain"
    scen_ids <- subset(feature_metadata, (Source == scen_source) & (Species %in% scen_species) & (Season == "breeding"))[["unique_id"]]
  } else if (scenario == 19) {
    scen_label <- "Waterfowl | All Bottleneck Seasons"
    scen_ids <- subset(feature_metadata, (Source == scen_source) & (Priority == "Waterfowl") & (bottleneck_include == 1))[["unique_id"]]
  } else if (scenario == 20) {
    scen_label <- "Nonwaterfowl | All Bottleneck Seasons"
    scen_ids <- subset(feature_metadata, (Source == scen_source) & (Priority == "Nonwaterfowl") & (bottleneck_include == 1))[["unique_id"]]
  } else if (scenario == 21) {
    scen_label <- "Wetland Habitat Group | All Bottleneck Seasons"
    scen_ids <- subset(feature_metadata, (Source == scen_source) & (Habitat_Group == "Wetland") & (bottleneck_include == 1))[["unique_id"]]
  } else if (scenario == 22) {
    scen_label <- "Forest Habitat Group | All Bottleneck Seasons"
    scen_ids <- subset(feature_metadata, (Source == scen_source) & (Habitat_Group == "Forest") & (bottleneck_include == 1))[["unique_id"]]
  } else if (scenario == 23) {
    scen_label <- "Riparian Habitat Group | All Bottleneck Seasons"
    scen_ids <- subset(feature_metadata, (Source == scen_source) & (Habitat_Group == "Riparian") & (bottleneck_include == 1))[["unique_id"]]
  } else if (scenario == 24) {
    scen_label <- "Herbaceous Habitat Group | All Bottleneck Seasons"
    scen_ids <- subset(feature_metadata, (Source == scen_source) & (Habitat_Group == "Herbaceous") & (bottleneck_include == 1))[["unique_id"]]
  } else if (scenario == 25) {
    scen_label <- "Coastal Habitat Group | All Bottleneck Seasons"
    scen_ids <- subset(feature_metadata, (Source == scen_source) & (Habitat_Group == "Coastal") & (bottleneck_include == 1))[["unique_id"]]
  } else if (scenario == 26) {
    scen_label <- "Frei | Waterfowl | All Bottleneck Seasons"
    scen_source <- "Frei"
    scen_spatial_mask <- Frei_spatial_mask
    scen_ids <- subset(feature_metadata, (Source == scen_source) & (Priority == "Waterfowl") & (bottleneck_include == 1))[["unique_id"]]
  } else if (scenario == 27) {
    scen_source <- "eBird"
    scen_spatial_mask <- Frei_spatial_mask
    scen_species <- unique(subset(feature_metadata, Source == "Frei")$Species)
    scen_label <- "eBird Priority Waterfowl | All Bottleneck Seasons | Frei Species + Spatial Domain"
    scen_ids <- subset(feature_metadata, (Source == scen_source) & (Species %in% scen_species) & (bottleneck_include == 1))[["unique_id"]]
  }


  scen_targets <- matrix(1, nrow = length(scen_ids), ncol = 1, dimnames = list(scen_ids, "target"))

  if (uniform_target_weight) {
    scen_weights <- matrix(1, nrow = length(scen_ids), ncol = 1, dimnames = list(scen_ids, "weight"))
  }

  # sweep_type <- 'linear'
  # sweep_bounds <- c(0,1)
  #
  # sweep_type <- 'exponential'
  # sweep_bounds <- c(-7,0)

  sweep_type <- "lin_exp"
  sweep_bounds <- c(-7, 0)

  problem_data_mask <- st_join(problem_data, scen_spatial_mask, join = st_equals) |>
    filter(Present > 0)

  problem_data_mask["Present"] <- NULL

  sweep_sol <- target_sweep(problem_data_mask, scen_ids, scen_targets, scen_weights, n = 49, n_portfolio = 100, sweep_type = sweep_type, sweep_bounds = sweep_bounds)

  if (!dir.exists(file.path(plot_dir, "plot_data"))) {
    dir.create(file.path(plot_dir, "plot_data"), recursive = TRUE)
  }

  st_write(sweep_sol["mean"], file.path(plot_dir, "plot_data", paste0("target_sweep_", scenario, ".shp")), append = FALSE)

  # Plot result
  pdf(file.path(plot_dir, paste0("target_sweep_", scenario, ".pdf")), width = 16, height = 12)
  plot(sweep_sol["mean"], main = NULL)
  title(scen_label)
  dev.off()
}
