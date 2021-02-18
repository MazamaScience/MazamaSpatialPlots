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


#' @title Validate longitude and latitude vectors
#' @description Longitude and latitude vectors are validated to be parseable as numeric
#' and within the bounds -180:180 and -90:90. If validation fails, an error is
#' generated.
#'
#' This helper function is useful when building automated plot-generation systems.
#' @param longitude Vector of longitudes in decimal degrees E, Default: NULL
#' @param latitude Vector of latitudes in decimal degrees N, Default: NULL
#' @return Invisibly returns \code{TRUE} if no error message has been generated.
#' @rdname validateLonsLats
#' @export
#'
validateLonsLats <- function(
  longitude = NULL,
  latitude = NULL
) {

  MazamaCoreUtils::stopIfNull(longitude)
  MazamaCoreUtils::stopIfNull(latitude)

  if ( length(longitude) != length(latitude) ) {
    stop(paste0(
      "longitude and latitude must have the same length"
    ))
  }

  longitude <- as.numeric(longitude)
  if ( is.na(longitude) || longitude < -180 || longitude > 180 )
    stop("longitudes must be valid values between -180 and 180")

  latitude <- as.numeric(latitude)
  if ( is.na(latitude) || latitude < -180 || latitude > 180 )
    stop("latitudes must be a valid values between -180 and 180")

  return(invisible(TRUE))

}


#' @title Validate longitude and latitude values
#' @description Longitude and latitude are validated to be parseable as numeric
#' and within the bounds -180:180 and -90:90. If validation fails, an error is
#' generated.
#'
#' This helper function is useful when building automated plot-generation systems.
#' @param longitude Single longitude in decimal degrees E, Default: NULL
#' @param latitude Single latitude in decimal degrees N, Default: NULL
#' @return Invisibly returns \code{TRUE} if no error message has been generated.
#' @rdname validateLonLat
#' @export
#'
validateLonLat <- function(
  longitude = NULL,
  latitude = NULL
) {

  MazamaCoreUtils::stopIfNull(longitude)
  MazamaCoreUtils::stopIfNull(latitude)

  if ( length(longitude) > 1 || length(latitude) > 1 ) {
    stop(paste0(
      "longitude and latitude must be single values"
    ))
  }

  longitude <- as.numeric(longitude)
  if ( is.na(longitude) || longitude < -180 || longitude > 180 )
    stop("longitude must be a valid value between -180 and 180")

  latitude <- as.numeric(latitude)
  if ( is.na(latitude) || latitude < -180 || latitude > 180 )
    stop("latitude must be a valid value between -180 and 180")

  return(invisible(TRUE))

}
