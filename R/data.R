#' @title Example state obesity dataset
#' @format A tibble with 52 rows and 3 columns of data.
#' @description The \code{example_US_stateObesity} dataset provides a small
#' state dataset to use in code examples. The code for creating it provides an
#' example for how to create a dataest that is compatible with \code{stateMap()}.
#' 
#' This dataset was generatedon 2020-06-09 by running:
#'
#' \preformatted{
#' library(dplyr)
#' library(MazamaSpatialUtils)
#' 
#' initializeMazamaSpatialUtils()
#' 
#' fileUrl <- paste0("http://data-lakecountyil.opendata.arcgis.com/datasets/",
#'                   "3e0c1eb04e5c48b3be9040b0589d3ccf_8.csv")
#' 
#' col_names <- c("FID", "stateName", "obesityRate", "SHAPE_Length", "SHAPE_Area")
#' col_types = "icddd"
#' 
#' outputColumns <- c("stateCode", "stateName", "obesityRate")
#' 
#' # A couple of iterations and I end up with this:
#' 
#' example_US_stateObesity <-
#'   readr::read_csv(
#'     file = fileUrl,
#'     skip = 1,                    # Skip the header line
#'     col_names = col_names,
#'     col_types = col_types
#'   ) %>%
#'   dplyr::mutate(
#'     stateCode = MazamaSpatialUtils::US_stateNameToCode(stateName),
#'     stateName = stateName
#'   ) %>%
#'   dplyr::select(!!outputColumns)
#' 
#' save(example_US_stateObesity, file = "data/example_US_stateObesity.rda")
#' }
#'
#' @seealso example_pas_raw
#' @source https://www.purpleair.com/json
"example_US_stateObesity"
