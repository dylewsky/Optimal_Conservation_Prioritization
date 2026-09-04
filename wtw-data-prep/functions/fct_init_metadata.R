#' Creates an empty meta data table with the initial columns
#'

init_metadata <- function() {
  data.frame(
    Type = character(),
    Theme = character(),
    File = character(),
    Name = character(),
    Legend = character(),
    Values = character(),
    Color = character(),
    Labels = character(),
    Unit = character(),
    Provenance = character(),
    Order = character(),
    Visible = character(),
    Hidden = character(),
    Goal = character()
  )
}
