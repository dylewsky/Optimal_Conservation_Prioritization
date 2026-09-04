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
plot_dir <- file.path(processing_dir, "Plots", "Chunked")
plot_data_dir <- file.path(plot_dir, "plot_data")
if (!dir.exists(plot_data_dir)) {
  dir.create(plot_data_dir, recursive = TRUE)
}

spec_file <- file.path("wtw-data-prep", "Data", "IBP_Species_Abbreviations.csv")
spec_names <- read.csv(spec_file, header = TRUE)

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
include_ids <- subset(metadata, Type == "include")["unique_id"]$unique_id
exclude_ids <- subset(metadata, (Type == "exclude") & (File != "E_lpr_000b16a_e.tif"))["unique_id"]$unique_id

pu_data <- spatial_data |> mutate(cost = as.vector(10**(-6) * st_area(spatial_data["geometry"])))
feature_data <- attribute_data |> dplyr::select(theme_ids)
feature_metadata <- metadata[metadata$unique_id %in% theme_ids, ]


include_data <- attribute_data |> dplyr::select(include_ids)
include_metadata <- metadata[metadata$unique_id %in% include_ids, ]
include_data["include"] <- FALSE
for (ex_id in include_ids) {
  this_thresh <- subset(include_metadata, unique_id == ex_id)$threshold
  include_data <- include_data |> mutate(!!rlang::sym("include") := ifelse((!!rlang::sym(ex_id) > this_thresh), TRUE, include))
}

exclude_data <- attribute_data |> dplyr::select(exclude_ids)
exclude_metadata <- metadata[metadata$unique_id %in% exclude_ids, ]
exclude_data["exclude"] <- FALSE
for (ex_id in exclude_ids) {
  this_thresh <- subset(exclude_metadata, unique_id == ex_id)$threshold
  exclude_data <- exclude_data |> mutate(!!rlang::sym("exclude") := ifelse((!!rlang::sym(ex_id) > this_thresh), TRUE, exclude))
}

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
    Season = if_else(Source == "eBird",
      word(Name, 3),
      ""
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


problem_data <- st_join(pu_data, feature_data, join = st_equals)

problem_data <- st_join(problem_data, exclude_data["exclude"], join = st_equals)
problem_data <- st_join(problem_data, include_data["include"], join = st_equals)


# target sweep parameters
# sweep_bounds <- c(0,0.5)
n_sweep <- 49
n_portfolio <- 100

# sweep_type <- 'linear'
# sweep_bounds <- c(0,1)
#
# sweep_type <- 'exponential'
# sweep_bounds <- c(-7,0)

sweep_type <- "lin_exp"
sweep_bounds <- c(-7, 0)

# chunk_by <- 'province'
# chunk_by <- 'BCR'
# chunk_by <- 'FM_class'

FM_threshold <- 0.1 # fractional coverage required for a cell to be included in class

province_list <- c("Ontario", "Quebec", "New Brunswick", "Nova Scotia", "Prince Edward Island", "Newfoundland and Labrador")
BCR_list <- as.character(c(3, 7, 8, 12, 13, 14))
FM_list <- c("Private", "Non-Private")

for (chunk_by in c("province", "FM_class")) {
  if (chunk_by == "province") {
    # Aggregate by province


    province_data_file <- file.path(processing_dir, "provinces_join.shp")

    if (file.exists(province_data_file)) {
      spatial_province_data <- st_read(province_data_file)
    } else {
      province_geometry_file <- file.path("wtw-data-prep", "Data", "Canada_Provinces_Territories")

      if (!file.exists(province_geometry_file)) {
        stop("Province boundary data file not available. Download and extract to the wtw-data-prep/Data/Canada_Provinces_Territories directory: https://open.canada.ca/data/en/dataset/a883eb14-0c0e-45c4-b8c4-b54c4a819edb/resource/12c03de6-c3f7-4f5f-bb5c-d479f2332842")
      }

      provinces <- st_read(province_geometry_file) |>
        st_transform(crs = st_crs(spatial_data))


      spatial_province_data <- st_join(spatial_data, provinces, join = st_intersects, largest = TRUE, left = TRUE)

      st_write(spatial_province_data, province_data_file)
    }

    problem_data <- st_join(problem_data, spatial_province_data["PRENAME"], join = st_equals, left = TRUE)

    chunk_list <- province_list
  } else if (chunk_by == "BCR") {
    bcr_data_file <- file.path(processing_dir, "BCR_join.shp")

    if (file.exists(bcr_data_file)) {
      spatial_bcr_data <- st_read(bcr_data_file)
    } else {
      BCRs <- st_read(dsn = file.path("wtw-data-prep", "Data"), layer = "bcr_ehjv_2025") |>
        st_transform(crs = st_crs(spatial_data))

      BCRs <- BCRs |> mutate(bcr_num = unlist(str_extract_all(bcr_label, "\\(?[0-9,.]+\\)?")))

      BCR_dissolve <- BCRs |>
        group_by(bcr_num) |>
        summarize()


      spatial_bcr_data <- st_join(spatial_data, BCR_dissolve, join = st_intersects, largest = TRUE, left = TRUE)

      st_write(spatial_bcr_data, bcr_data_file)
    }

    problem_data <- st_join(problem_data, spatial_bcr_data["bcr_num"], join = st_equals, left = TRUE)

    chunk_list <- BCR_list
  } else if (chunk_by == "FM_class") {
    FM_data_file <- file.path(processing_dir, "FM_join.shp")

    if (file.exists(FM_data_file)) {
      spatial_FM_data <- st_read(FM_data_file)
    } else {
      FM_data <- st_read(file.path(processing_dir, "FM_coverage_fracs.shp"))
      FM_private <- FM_data |>
        subset(class == "Private") |>
        mutate(private = case_when(
          class == "Private" ~ (freq > FM_threshold),
          class != "Private" ~ 0
        )) |>
        mutate(FM_class = case_when(
          private == 1 ~ "Private",
          private == 0 ~ "Non-Private"
        ))

      FM_protected <- FM_data |>
        subset(class == "Protected") |>
        mutate(protected = case_when(
          class == "Protected" ~ (freq > 0.5),
          class != "Protected" ~ 0
        ))

      FM_indigenous <- FM_data |>
        subset(class == "Indian Reserve" | class == "Treaty") |>
        mutate(indigenous = (freq > 0.5))

      FM_federal <- FM_data |>
        subset(class == "Federal Reserve") |>
        mutate(federal = (freq > 0.5))

      spatial_FM_data <- st_join(spatial_data["geometry"], FM_private, join = st_equals, left = TRUE) |>
        mutate(private = ifelse(is.na(private), 0, private)) |>
        mutate(FM_class = ifelse(is.na(FM_class), "Non-Private", FM_class))

      spatial_FM_data <- st_join(spatial_FM_data, FM_protected["protected"], join = st_equals, left = TRUE) |>
        mutate(protected = ifelse(is.na(protected), 0, protected))

      spatial_FM_data <- st_join(spatial_FM_data, FM_indigenous["indigenous"], join = st_equals, left = TRUE) |>
        mutate(indigenous = ifelse(is.na(indigenous), 0, indigenous))

      spatial_FM_data <- st_join(spatial_FM_data, FM_federal["federal"], join = st_equals, left = TRUE) |>
        mutate(federal = ifelse(is.na(federal), 0, federal))

      spatial_FM_data["class"] <- NULL


      st_write(spatial_FM_data, FM_data_file, append = FALSE)
    }

    problem_data <- st_join(problem_data, spatial_FM_data["FM_class"], join = st_equals, left = TRUE)

    chunk_list <- FM_list
  }

  for (priority in c("Waterfowl", "Nonwaterfowl")) {
    for (season in c("bottlenecks",'breeding')) {
      for (problem_IE in c('IE_off', 'I_on')){
        uniform_target_weight <- TRUE

        scen_source <- "eBird"

        if (season == "bottlenecks") {
          scen_ids <- subset(feature_metadata, (Source == scen_source) & (Priority == priority) & (bottleneck_include == 1))[["unique_id"]]
        } else {
          scen_ids <- subset(feature_metadata, (Source == scen_source) & (Priority == priority) & (Season == season))[["unique_id"]]
        }

        scen_targets <- matrix(1, nrow = length(scen_ids), ncol = 1, dimnames = list(scen_ids, "target"))

        if (uniform_target_weight) {
          scen_weights <- matrix(1, nrow = length(scen_ids), ncol = 1, dimnames = list(scen_ids, "weight"))
        } else {
          stop("implement")
        }

        scen_label <- paste0(priority, " | ", season, " | By ", chunk_by)

        if (problem_IE == "IE_on") {
          use_includes <- TRUE
          use_excludes <- TRUE
        } else if (problem_IE == "IE_off") {
          use_includes <- FALSE
          use_excludes <- FALSE
        } else if (problem_IE == "I_on") {
          use_includes <- TRUE
          use_excludes <- FALSE
        }

        # Compute full (unchunked) solution
        full_sol <- target_sweep(problem_data, scen_ids, scen_targets, scen_weights,
          sweep_type = sweep_type, sweep_bounds = sweep_bounds, n = n_sweep, n_portfolio = n_portfolio,
          use_includes = use_includes, use_excludes = use_excludes
        )

        pdf(file.path(plot_dir, paste0("full", "_", priority, "_", season, "_", problem_IE, ".pdf")), width = 16, height = 12)
        plot(full_sol["mean"], main = NULL)
        title(paste0(priority, " | ", season, " | Full"))
        dev.off()

        full_sol <- full_sol |> mutate(solution = mean)

        if (problem_IE %in% c("IE_on", "I_on")) {
          full_sol <- st_join(full_sol, problem_data[c("include", "exclude")], join = st_equals, left = TRUE)
          st_write(full_sol[c("solution", "include", "exclude")], file.path(plot_data_dir, paste0("full", "_", priority, "_", season, "_", problem_IE, ".shp")), append = FALSE)
        } else {
          st_write(full_sol["solution"], file.path(plot_data_dir, paste0("full", "_", priority, "_", season, "_", problem_IE, ".shp")), append = FALSE)
        }


        # Compute chunked solution

        sol_sf <- problem_data["geometry"]

        for (chunk in chunk_list) {
          if (chunk_by == "province") {
            prov_prob_data <- problem_data |> filter(PRENAME == chunk)
          } else if (chunk_by == "BCR") {
            prov_prob_data <- problem_data |> filter(bcr_num == chunk)
          } else if (chunk_by == "FM_class") {
            prov_prob_data <- problem_data |> filter(FM_class == chunk)
          }

          chunk_use_includes <- use_includes
          if (!any(prov_prob_data["include"] |> st_drop_geometry())) {
            chunk_use_includes <- FALSE
          }
          chunk_use_excludes <- use_excludes
          if (!any(prov_prob_data["exclude"] |> st_drop_geometry())) {
            chunk_use_excludes <- FALSE
          }

          prov_sol <- target_sweep(prov_prob_data, scen_ids, scen_targets, scen_weights,
            sweep_type = sweep_type, sweep_bounds = sweep_bounds, n = n_sweep, n_portfolio = n_portfolio,
            use_includes = chunk_use_includes, use_excludes = chunk_use_excludes
          )
          prov_sol <- prov_sol |> rename(!!chunk := mean) # rename 'mean' column

          sol_sf <- st_join(sol_sf, prov_sol[chunk], join = st_equals, left = TRUE)
        }
        stopifnot(all(rowSums(!is.na(sol_sf)) <= 2)) # assert that no grid cell has non-NA value in more than one province

        na_cells <- (rowSums(!is.na(sol_sf)) == 1)

        sol_sf["knit"] <- rowSums(st_drop_geometry(sol_sf), na.rm = TRUE)
        sol_sf[na_cells, "knit"] <- NA

        pdf(file.path(plot_dir, paste0(chunk_by, "_", priority, "_", season, "_", problem_IE, ".pdf")), width = 16, height = 12)
        plot(sol_sf["knit"], main = NULL)
        title(scen_label)
        dev.off()

        sol_sf <- sol_sf |> mutate(solution = knit)

        if (problem_IE %in% c("IE_on", "I_on")) {
          sol_sf <- st_join(sol_sf, problem_data[c("include", "exclude")], join = st_equals, left = TRUE)
          st_write(sol_sf[c("solution", "include", "exclude")], file.path(plot_data_dir, paste0(chunk_by, "_", priority, "_", season, "_", problem_IE, ".shp")), append = FALSE)
        } else {
          st_write(sol_sf["solution"], file.path(plot_data_dir, paste0(chunk_by, "_", priority, "_", season, "_", problem_IE, ".shp")), append = FALSE)
        }
      }
    }
  }
}
