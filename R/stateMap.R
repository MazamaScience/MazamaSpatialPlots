#' @title State level thematic map
#' @description Uses the \pkg{tmap} package to generate a thematic map at the
#' state level. Input consists of a dataframe with \code{stateCode} identifiers.
#'
#' Data to plot is specified with \code{parameter} argument. If \code{parameter}
#' is mult-valued, mutliple plots will be generated and displayed in a
#' "small multiples" matrix.
#'
#' The returned object is a \pkg{tmap} ggplot object which can be further
#' modified with ggplot options.
#'
#' @details See \code{tmap::tm_fill()} for a more detailed description of
#' the following parameters:
#'
#' \itemize{
#' \item{\code{palette}}
#' \item{\code{breaks}}
#' }
#'
#' @param data Dataframe containing values to plot. This dataframe
#' must contain a column named \code{stateCode} with the 2-character state code.
#' @param parameter Name of the column in \code{data} to use for coloring the map.
#' @param state_SPDF SpatialPolygonsDataFrame with US states. It's data
#' \code{@slot} must contain a column named \code{stateCode} with the
#' 2-character state code.
#' @param palette Palette name or a vector of colors based on RColorBrewer.
#' @param breaks Numeric vector of break points.
#' @param conusOnly Logical specifying Continental US state codes. Ignored when
#' the \code{stateCode} argument is specified.
#' @param stateCode Vector of state codes to include on the map.
#' @param projection Specified method to represent surface of Earth.
#' @param stateBorderColor Color used for state borders.
#' @param title Vector of text strings to use as individual plot titles.
#' This must be the same length as 'parameter'.
#' @param main.title Text string to use as an overall title for all plots.
#' @return A ggplot object.
#'
#' @rdname stateMap
#'
#' @examples
#' library(MazamaSpatialPlots)
#'
#' stateMap(
#'   data = example_US_stateObesity,
#'   parameter = "obesityRate",
#'   palette = "BuPu",
#'   stateBorderColor = "white",
#'   main.title = "2018 Obesity by State"
#' )
#'
#' # Example of customization using tm_layout and breaks parameter
#' stateMap(
#'   data = example_US_stateObesity,
#'   parameter = "obesityRate",
#'   breaks = seq(20,38,3),
#'   stateBorderColor = 'black'
#' ) +
#'   tmap::tm_layout(
#'     frame = TRUE,
#'     frame.double.line = TRUE,
#'     main.title = 'Obesity Rate by State',
#'     main.title.position = c("center", "top"),
#'     fontfamily = "serif",
#'     bg.color = "grey85",
#'     inner.margins  = .05
#'   )
#'
#' # Example using stateCode
#' stateMap(
#'   data = example_US_stateObesity,
#'   parameter = "obesityRate",
#'   stateCode = c('ME', 'NH', 'VT', 'MA', 'RI', 'CT'),
#'   stateBorderColor = 'black',
#'   title = 'Obesity Rates in New England'
#' ) +
#'   tmap::tm_layout(
#'     frame = TRUE,
#'     frame.double.line = TRUE,
#'     title.size = 1.2,
#'     title.fontface = 2,
#'     fontfamily = "serif",
#'     bg.color = "grey85",
#'     inner.margins  = .08
#'   )
#' @export
#' @importFrom sp CRS
#' @importFrom rlang .data
#' @importFrom tmap tm_shape tm_fill tm_polygons tm_layout
#'

stateMap <- function(
  data = NULL,
  parameter = NULL,
  state_SPDF = "USCensusStates_02",
  palette = "YlOrBr",
  breaks = NULL,
  conusOnly = TRUE,
  stateCode = NULL,
  projection = NULL,
  stateBorderColor = "gray50",
  title = NULL,
  main.title = NULL
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(data)
  MazamaCoreUtils::stopIfNull(parameter)

  # Check if data exists
  if ( !exists("data") ) {
    stop("Parameter 'data' is empty.")
  }

  # * Validate 'data' -----

  # Does 'parameter' exist in the incoming dataframe?
  if ( !all(parameter %in% colnames(data)) ) {
    stop(sprintf("'%s' is not found in the incoming 'data' dataframe.", parameter))
  }

  # Does 'data' have the required columns?
  requiredFields <- c("stateCode")
  missingFields <- setdiff(requiredFields, names(data))
  if ( length(missingFields) > 0 ) {
    if ( "state name" %in% tolower(names(data)) ) {
      columnIndex <- which("state name" == tolower(names(data)))
      data$stateCode <- MazamaSpatialUtils::US_stateNameToCode(data[, columnIndex])
    } else {
      stop(paste0("Missing fields in 'data': ",
                  paste0(missingFields, collapse = ", ")))
      }
    }

  # * Validate 'SPDF' -----

  # Accept state SPDF as character string or as object
  if ( is.character(state_SPDF) ) {
    if ( exists(state_SPDF) ) {
      state_SPDF <- get(state_SPDF)
    } else {
      stop(sprintf("State dataset '%s' is not loaded.
  Please load it with MazamaSpatialtUtils::loadSpatialData()",
                   state_SPDF
      ))
    }
  }

  # Does 'state_SPDF' have the required columns?
  requiredSPDFFields <- c("stateCode")
  missingSPDFFields <- setdiff(requiredSPDFFields, names(state_SPDF@data))
  if ( length(missingSPDFFields) > 0 ) {
    if ( "state name" %in% tolower(names(state_SPDF@data)) ) {  # if statecode not found, try to create it
      columnIndex <- which("state name" == tolower(names(state_SPDF@data)))
      state_SPDF@data$stateCode <- MazamaSpatialUtils::US_stateNameToCode(state_SPDF@data[, columnIndex])
    } else {
      stop(paste0("Missing fields in 'state_SPDF': ",
                  paste0(missingSPDFFields, collapse = ", ")))
    }
  }

  # * Validate other -----

  # Validate breaks
  if ( !is.null(breaks) && !is.numeric(breaks) ) {
    stop("Parameter 'breaks' must be a numeric vector.")
  }

  # Guarantee that conusOnly is logical
  if ( !is.logical(conusOnly) ) {
    conusOnly <- TRUE
  }

  # Validate incoming stateCode vector if any
  if ( !is.null(stateCode) ) {
    if ( !all(stateCode %in% MazamaSpatialUtils::US_52) ) {
      invalidStateCodes <- setdiff(stateCode, MazamaSpatialUtils::US_52)
      stop(sprintf("Invalid state codes found:  %s",
                   paste0(invalidStateCodes, collapse = ", ")))
    }
  }

  # Convert projection to a CRS object if necessary
  if ( !is.null(projection) ) {
    if ( is.character(projection) ) {
      projection <- sp::CRS(projection)
    } else if ( "CRS" %in% class(projection) ) {
      # leave it alone
    } else {
      stop(paste0("Parameter 'projection' must be a sp::CRS object or a valid ",
                  "projection string."))
    }
  }

  # Validate length of 'parameter' and 'title'
  if ( !is.null(title) && length(parameter) != length(title) )
    stop("The lengths of 'parameter' and 'title' must be equal.")

  # ----- Subset the SPDF ------------------------------------------------------

  if ( !is.null(stateCode) ) {

    # NOTE:  Subset doesn't work properly unless we change the name here
    incomingStateCode <- stateCode
    state_SPDF <- subset(state_SPDF, state_SPDF$stateCode %in% incomingStateCode)
    data <- data %>% dplyr::filter(.data$stateCode %in% incomingStateCode)

  } else if ( conusOnly ) {

    state_SPDF <- subset(state_SPDF, state_SPDF$stateCode %in% MazamaSpatialUtils::CONUS)
    data <- data %>% dplyr::filter(.data$stateCode %in% MazamaSpatialUtils::CONUS)

  } else {

    # use existing SPDF

  }

  # ----- Create a projection --------------------------------------------------

  # NOTE:  Setting the est most longitude improves plots that include Alaska by
  # NOTE:  assuming the east most longitude cannot be greater than -66.97626
  # NOTE:  (which is the farthest east longitude in the continental US).
  # NOTE:  This wouldn't work if someone wants to include Guam but this function
  # NOTE:  is designed primarily for CONUS.

  if ( is.null(projection) ) {

    # NOTE:  Specifying stateCode takes precedence over specifying conusOnly
    if ( !is.null(stateCode) ) {

      # 1) Get boundaries from stateSPDF
      bbox <- sp::bbox(state_SPDF)

      # 2) Calculate lat lo/mid/hi and lon mid
      lat_1 <- bbox[2]
      lat_2 <- bbox[4]
      lat_0 <- (lat_1 + lat_2)/2
      lon_1 <- bbox[1]
      lon_2 <- min(bbox[3], -66.97626) # this is the east most longitude to use
      lon_0 <- (lon_1 + lon_2)/2

      # 3) Create the proj4string text
      projString <- sprintf("+proj=aea +lat_1=%.1f +lat_2=%.1f +lat_0=%.1f +lon_0=%.1f +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs",
                            lat_1, lat_2, lat_0, lon_0)
      projection <- sp::CRS(projString)

    } else if ( conusOnly ) {

      projection <- sp::CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")

    } else {

      # TODO:  The 'conusOnly = FALSE' option needs to be replaced with the
      # TODO:  artifical projection that places Alaska and Hawaii in the lower
      # TODO:  left corner. Jon has seen such a projection in an example but
      # TODO:  can't remember exactly where.

      projection <- sp::CRS("+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs")

    }

  } else {

    # Use projection found in SPDF

  }

  # ----- Merge data with SPDF -------------------------------------------------

  # NOTE:  We can use left_join() because 'stateCode' is guaranteed to be in
  # NOTE:  both dataframes. We use "matrix style" subsetting of 'data' to
  # NOTE:  specify that we want all rows and just the columns with "stateCode"
  # NOTE:  and the parameter of interest.

  # Add the incoming data$parameter to 'state_SPDF@data'.
  state_SPDF@data <-
    dplyr::left_join(
      state_SPDF@data,
      data[, c("stateCode", parameter)],
      by = "stateCode"
    )

  # ----- Create plot ----------------------------------------------------------

  gg <-
    tmap::tm_shape(state_SPDF, projection = projection) +
    tmap::tm_fill(
      col = parameter,
      palette = palette,
      breaks = breaks
    ) +
    tmap::tm_shape(state_SPDF, projection = projection) +
    tmap::tm_polygons(
      alpha = 0,
      border.col = stateBorderColor
    ) +
    tmap::tm_layout(
      main.title = main.title,
      title = title,
      main.title.size = .9,
      main.title.position = c("center", "top"),
      title.position = c("center", 'top'),
      frame = FALSE
    )

  # ----- Return ---------------------------------------------------------------

  return(gg)

}

# ===== DEBUGGING ==============================================================

if ( FALSE ) {

  # NOTE:  Good idea to clear out the Global environment before running this.

  library(MazamaSpatialPlots)
  mazama_initialize()

  state_SPDF <- USCensusStates_02

  # Set up required variables so we can walk through the code
  data = example_US_stateObesity
  parameter = "obesityRate"
  palette = "YlOrBr"
  breaks = NULL
  conusOnly = TRUE
  stateCode = NULL
  projection = NULL
  stateBorderColor = "white"
  title <- "Obesity Rate by state"

  # Run the code above and then start walking through the lines of code in the
  # function.
  #
  # You can also source this file and run the function now

  stateMap(
    data = data,
    parameter = parameter,
    state_SPDF = state_SPDF,
    palette = palette,
    breaks = breaks,
    conusOnly = conusOnly,
    stateCode = stateCode,
    projection = projection,
    stateBorderColor = stateBorderColor,
    title = title
  )

  ##############################################################################
  # Here is how I would use this function in real life:

  stateMap(
    data,
    state_SPDF = state_SPDF,
    parameter = "obesityRate"
  )

  # Not bad

  stateMap(
    data,
    state_SPDF = state_SPDF,
    parameter = "obesityRate",
    palette = "BuPu",
    stateBorderColor = "black"
  )

  # Better but still need a title

  stateMap(
    data,
    state_SPDF = state_SPDF,
    parameter = "obesityRate",
    palette = "BuPu",
    stateBorderColor = "black"
  ) +
    tmap::tm_layout(
      title = "Obesity rate by state",
      title.size = 2,
      title.fontface = "bold",
      frame = TRUE
    )

  # Very nice!

  ##############################################################################
  # More examples

  # Example using tmap style "classic"
  stateMap(
    data = example_US_stateObesity,
    parameter = "obesityRate",
    breaks = seq(20,40,4),
  ) +
    tmap::tm_style(
      'classic'
    ) +
    tmap::tm_layout(
      title = 'Obesity Rate by State',
      title.position = c("center", "top"),
      title.size = 1.2,
      inner.margins  = .08
    ) +
    tmap::tm_compass()

  # Example using tmap style "cobalt"
  stateMap(
    data = example_US_stateObesity,
    parameter = "obesityRate",
    palette = "BuPu",
    breaks = seq(20,40,4)
  ) +
    tmap::tm_style(
      'cobalt'
    ) +
    tmap::tm_layout(
      title = 'Obesity Rate by State',
      title.position = c("center", "top"),
      title.size = 1.2,
      inner.margins  = .08
    ) +
    tmap::tm_compass()

  # the previous two examples show how to leverage a preexisting style to create
  # nice plots with few manual specifications

  # Example with conusOnly=FALSE
  stateMap(
    data = example_US_stateObesity,
    parameter = "obesityRate",
    breaks = seq(20,38,3), #increasing color detail
    conusOnly = FALSE ,
    stateBorderColor = 'black',
  ) +
    tmap::tm_layout(
      frame = TRUE,
      main.title = 'Obesity Rates in U.S. States and Territories',
      main.title.position = c("center", "top"),
      title.fontface = 2,
      fontfamily = "serif",
      bg.color = "grey85",
      inner.margins  = .05,
      legend.position = c('right', 'top')
    )

}
