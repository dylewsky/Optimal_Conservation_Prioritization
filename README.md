# EHJV Decision Support Tool

Code accompanying the paper "Optimal Conservation Prioritization for Waterfowl and Other Birds in Eastern Canada" by Daniel Dylewsky, Matthew E. Dyson, James E. Patterson, Richard Schuster, and Joseph R. Bennett.

## Description

Code pipeline to compute conservation prioritization maps from species abundance data. Designed primarily for use with 2023 eBird Status & Trends data products, but can easily be adapted for use with any species abundance layers in raster format. To download the eBird data, [obtain an access key from eBird](https://ebird.org/st/request) and paste it into `eBird_download_abundance_data.R`.


### Dependencies

In addition to a number of widely used GIS libraries in R, this code makes use of the `prioritizr` package for optimization computations: https://prioritizr.net/

The data processing pipeline is designed to integrate with the Where to Work tool managed by the Nature Conservancy of Canada (https://github.com/NCC-CNC/wheretowork/). While this tool is not directly used in our code, preprocessed data is formatted to be compatible with it. Users are encouraged to explore prioritization results interactively by uploading these processed data files into their online interface at https://ncc.carleton.ca/login

To achieve this compatibility, our code makes use of the published Where to Work data preparation repository available at https://github.com/NCC-CNC/wtw-data-prep/ . A modified version of this preprocessing pipeline is reproduced in our repository in the wtw-data-prep directory with the permission of the original authors. 

## Executing Code

### Data Preparation

Analysis can be performed using either 1) the same planning grid and species data used for the Eastern Habitat Joint Venture in the paper, or 2) alternative grid parameters, study region, or data sources.

#### Using existing data/parameters
Simply extract `EHJV_Example_Data.zip` to `/wtw-data-prep/WTW/EHJV_10km_hex_grid`.

#### Using custom data/parameters
Obtain necessary data files. To use eBird abundance layers, run `eBird_download_abundance_data.R`. By default, it will download data for EHJV priority species as designated in `/wtw-data-prep/Data/IBP_Species_Abbreviations.csv`. To use other abundance layers, extract the relevant raster files (in .tif format) to /data/spatialData/your_data_source_name. 

Some code functionality depends on additional data files. To prepare geometries derived from the Canadian Forest Service's Map of Forest Management in Canada, run `process_forest_management_data.R`. You will be prompted to download the relevant files if they are not present.

Data preprocessing is carried out using the files in the /wtw-data-prep/ directory. `WTW_Prepare_Data.R` serves as a wrapper around this processing pipeline so it can be run in a single line. For more details on customizing run parameters or troubleshooting this pipeline, refer to its [original documentation](https://github.com/NCC-CNC/wtw-data-prep/). Some scripts are implemented in Python using the ArcPy package, which requires an active license.

### Data Analysis

Data analysis is carried out using:

1. `WTW_Prioritizr_Scenarios.R` (to compute region-wide prioritizations using pre-specified subsets of available abundance data)
2. `WTW_Prioritizr_Patchwork.R` (to compute prioritizations which divide the study area into subregions)
