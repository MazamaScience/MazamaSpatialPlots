
#' @title Initialize with MazamaScience standard directories
#' @description Convenience function to initialize spatial data for US state
#' and county maps. Wraps the following setup lines:
#'
#' \preformatted{
#' MazamaSpatialUtils::setSpatialDataDir(spatialDataDir)
#'
#' MazamaSpatialUtils::loadSpatialData("USCensusCounties_02")
#' MazamaSpatialUtils::loadSpatialData("USCensusStates_02")

#' }
#' @param spatialDataDir Directory where spatial datasets are found,
#' Default: "~/Data/Spatial"
#' @return No return value.
#' @rdname mazama_initialize
#' @examples
#' \donttest{
#' library(MazamaSpatialPlots)
#'
#' # Set up directory for spatial data
#' spatialDataDir <- tempdir() # typically "~/Data/Spatial"
#' MazamaSpatialUtils::setSpatialDataDir(spatialDataDir)
#'
#' exists("USCensusStates_02")
#' mazama_initialize(spatialDataDir)
#' exists("USCensusStates_02")
#' class(USCensusStates_02)
#' }
#' @export
#' @importFrom MazamaSpatialUtils setSpatialDataDir loadSpatialData
#'
mazama_initialize <- function(
  spatialDataDir = "~/Data/Spatial"
) {

  # Is everything already initialized?
  result <- try({
    validateMazamaSpatialUtils() # swallow warning messages
  }, silent = TRUE)

  if ( "try-error" %in% class(result) ) {

    # Not initialized, so try to initialize
    result <- try({
      MazamaSpatialUtils::setSpatialDataDir(spatialDataDir)
      # Install if not found
      MazamaSpatialUtils::installSpatialData("USCensusCounties_02")
      MazamaSpatialUtils::installSpatialData("USCensusStates_02")
      # Now load
      MazamaSpatialUtils::loadSpatialData("USCensusCounties_02")
      MazamaSpatialUtils::loadSpatialData("USCensusStates_02")

    }, silent = TRUE)

    if ( "try-error" %in% class(result) ) {
      # Installation failed so spit out warning messages
      validateMazamaSpatialUtils()
    }

  }

}
