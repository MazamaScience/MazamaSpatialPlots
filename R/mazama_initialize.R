
#' @title Initialize with MazamaScience standard directories
#' @description Convenience function to initialize spatial data. Wraps the 
#' following setup lines:
#' 
#' \preformatted{
#' MazamaSpatialUtils::setSpatialDataDir(spatialDataDir)
#' 
#' MazamaSpatialUtils::loadSpatialData("EEZCountries")
#' MazamaSpatialUtils::loadSpatialData("OSMTimezones")
#' MazamaSpatialUtils::loadSpatialData("NaturalEarthAdm1")
#' MazamaSpatialUtils::loadSpatialData("USCensusCounties")
#' MazamaSpatialUtils::loadSpatialData("USCensusStates")

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
#' # Install core spatial datasets (168 MB download)
#' MazamaSpatialUtils::installSpatialData()
#' 
#' exists("NaturalEarthAdm1")
#' mazama_initialize(spatialDataDir)
#' exists("NaturalEarthAdm1")
#' class(NaturalEarthAdm1)
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
      MazamaSpatialUtils::loadSpatialData("EEZCountries")
      MazamaSpatialUtils::loadSpatialData("OSMTimezones")
      MazamaSpatialUtils::loadSpatialData("NaturalEarthAdm1")
      MazamaSpatialUtils::loadSpatialData("USCensusCounties")
      MazamaSpatialUtils::loadSpatialData("USCensusStates")
      
    }, silent = TRUE)
    
    if ( "try-error" %in% class(result) ) {
      # Installation failed so spit out warning messages
      validateMazamaSpatialUtils()
    }
   
  }
  
}