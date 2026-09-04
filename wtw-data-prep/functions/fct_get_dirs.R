get_gdb_layers <- function(gdb_path) {
  layers <- st_layers(gdb_path)$name

  file.path(gdb_path, layers)
}

get_all_tifs_gdbs <- function(search_dir) {
  tifs <- list.files(search_dir, full.names = TRUE, recursive = TRUE, pattern = ".tif$|.tiff$")

  gdbs_paths <- list.dirs(search_dir)[grepl(".gdb$", list.dirs(search_dir))]

  gdb_layers <- unlist(
    lapply(gdbs_paths, function(x) {
      get_gdb_layers(x)
    })
  )

  c(tifs, gdb_layers)
}

get_parent_dir <- function(file_path) {
  # if tif, return one up
  if (grepl(".tif$|.tiff$", file_path)) {
    return(dirname(file_path))
  }

  # if gdb feature class, return two up
  if (grepl(".gdb/", file_path)) {
    return(dirname(dirname(file_path)))
  }

  # if gdb, return one up
  if (grepl(".gdb$", file_path)) {
    return(dirname(file_path))
  }

  stop("Must be file path ending in .tif, .tiff, .gdb/* or .gdb")
}
