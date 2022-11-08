#' @title Example state obesity dataset
#' @format A tibble with 52 rows and 3 columns of data.
#' @description The \code{example_US_stateObesity} dataset provides a small
#' state dataset to use in code examples. The code for creating it demonstrates
#' creation of a dataest that is compatible with \code{stateMap()}.
#'
#' This dataset was generated on 2020-06-09 by running:
#'
#' \preformatted{
#' library(dplyr)
#' library(MazamaSpatialUtils)
#'
#' fileUrl <- paste0("http://data-lakecountyil.opendata.arcgis.com/datasets/",
#'                   "3e0c1eb04e5c48b3be9040b0589d3ccf_8.csv")
#'
#' col_names <- c("FID", "stateName", "obesityRate", "SHAPE_Length", "SHAPE_Area")
#' col_types = "icddd"
#'
#' outputColumns <- c("stateCode", "stateName", "obesityRate")
#'
#' # After a little trial and error, the following works well:
#'
#' example_US_stateObesity <-
#'   readr::read_csv(
#'     file = fileUrl,
#'     skip = 1,                    # Skip the header line
#'     col_names = col_names,
#'     col_types = col_types
#'   ) \%>\%
#'   dplyr::mutate(
#'     stateCode = MazamaSpatialUtils::US_stateNameToCode(stateName)
#'   ) \%>\%
#'   dplyr::select(!!outputColumns)
#'
#' save(example_US_stateObesity, file = "data/example_US_stateObesity.rda")
#' }
#'
"example_US_stateObesity"


#' @title Example county Covid dataset
#' @format A tibble with 52 rows and 3 columns of data.
#' @description The \code{example_US_countyCovid} dataset provides a small
#' county dataset to use in code examples. The code for creating it demonstrates
#' creation of a dataest that is compatible with \code{countyMap()}.
#'
#' This dataset was generated on 2020-06-12 by running:
#'
#' \preformatted{
#' library(dplyr)
#' library(MazamaSpatialUtils)
#'
#' fileUrl <- "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv"
#'
#' col_names <- c("date", "countyName", "stateName", "countyFIPS", "cases", "deaths")
#' col_types = "Dcccii"
#'
#' outputColumns <- c("stateCode", "stateName", "countyFIPS", "countyName", "cases", "deaths")
#'
#' # After a little trial and error, the following works well:
#'
#' example_US_countyCovid <-
#'   readr::read_csv(
#'     file = fileUrl,
#'     skip = 1,                    # Skip the header line
#'     col_names = col_names,
#'     col_types = col_types
#'   ) \%>\%
#'   dplyr::mutate(
#'     stateCode = MazamaSpatialUtils::US_stateNameToCode(stateName),
#'   ) \%>\%
#'   dplyr::filter(.data$date == lubridate::ymd("2020-06-01")) \%>\%
#'   dplyr::select(!!outputColumns)
#'
#' save(example_US_countyCovid, file = "data/example_US_countyCovid.rda")
#' }
#'
"example_US_countyCovid"


#' @title US Census State simple features data frame
#' @format A simple features data frame (SFDF) with 52 observations and 8 variables.
#' @description The \code{USCensusStates_02} dataset provides a SFDF of US states to
#' use in code examples. It is created by converting a US state borders
#' shapefile to a simple features data frame with additional columns of data. The
#' code for creating it demonstrates creation of a SFDF that is
#' compatible with \code{stateMap()}. See the \pkg{MazamaSpatialUtils} package for the
#' function \code{convertUSCensusStates()} that creates this SFDF.
#'
#' This dataset was generated on 2022-11-007 by running:
#'
#' \preformatted{
#' library(MazamaSpatialUtils)
#'
#' setSpatialDataDir("~/Data/Spatial_0.8")
#'
#' MazamaSpatialUtils::convertUSCensusStates()
#'
#' MazamaSpatialUtils::loadSpatialData("USCensusStates_02")
#'
#' save(USCensusStates_02, file = "data/USCensusStates_02.rda")
#' }
#'
"USCensusStates_02"


#' @title US Census Counties simple features data frame
#' @format A simple features data frame (SFDF) with 3169 observations and 9 variables.
#' @description The \code{USCensusCounties_02} dataset provides a SFDF of US counties to
#' use in code examples. It is created from converting a US county borders
#' shapefile to a simple features data frame with additional columns of data. The
#' code for creating it demonstrates creation of a SFDF that is
#' compatible with \code{countyMap()}. See the \pkg{MazamaSpatialUtils} package for the
#' function \code{convertUSCensusCounties()} that creates this SFDF.
#'
#' This dataset was generated on 2022-11-07 by running:
#'
#' \preformatted{
#' library(MazamaSpatialUtils)
#'
#' setSpatialDataDir("~/Data/Spatial_0.8")
#'
#' MazamaSpatialUtils::convertUSCensusCounties()
#'
#' MazamaSpatialUtils::loadSpatialData("USCensusCounties_02")
#'
#' save(USCensusCounties_02, file = "data/USCensusCounties_02.rda")
#' }
#'
"USCensusCounties_02"
