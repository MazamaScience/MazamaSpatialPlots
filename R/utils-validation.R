#' @title Validate proper setup of MazamaSpatialUtils
#' @description The \pkg{MazamaSpatialUtils} package mus be properly installed
#' and initialized before using functions from the \pkg{MazamaSpatialPlots}
#' package.
#'
#' This helper function is useful when building automated plot-generation systems.
#' @return Invisibly returns \code{TRUE} if no error message has been generated.
#' @rdname validateMazamaSpatialUtils
#' @export
#'
validateMazamaSpatialUtils <- function() {

  if ( !exists("USCensusCounties_02") ||
       !exists("USCensusStates_02") ) {

    stop(paste0(
      "\n\nYou must have the MazamaSpatialUtils package ",
      "as well as core datasets installed.\n\n",
      "Install core datasets with:\n\n",
      "  MazamaSpatialUtils::setSpatialDataDir(\"YOUR_DATA_DIR\")\n",
      "  MazamaSpatialUtils::installSpatialData()\n\n",
      "Once installed, initialize spatial data with:\n\n",
      "  MazamaSpatialUtils::setSpatialDataDir(\"YOUR_DATA_DIR\")\n",
      "  MazamaSpatialUtils::loadSpatialData(\"USCensusCounties_02\")\n",
      "  MazamaSpatialUtils::loadSpatialData(\"USCensusStates_02\")\n"

    ))

  }

  return(invisible(TRUE))

}
