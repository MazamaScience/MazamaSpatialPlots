#' @title County level thematic map
#' @description Uses the \pkg{tmap} package to generate a thematic map at the
#' county level. Input consists of a dataframe with \code{countyFIPS} identifiers.
#'
#' Data to plot is specified with \code{parameter} argument. If \code{parameter}
#' is mult-valued, mutliple plots will be generated and displayed as "facets".
#'
#' The returned object is a \pkg{tmap} ggplot object which can be further
#' modified with tmap or ggplot options.
#'
#' @param data Dataframe containing values to plot. This dataframe
#' must contain a column named \code{countyFIPS} with the 5-digit FIPS code.
#' @param parameter Name of the column in \code{data} to use for coloring the map.
#' @param state_SFDF simple features data frame with US states. It's data
#' \code{@slot} must contain a column named \code{stateCode} if either
#' \code{conusOnly = TRUE} or the \code{stateCode} argument is specified.
#' @param county_SFDF simple features data frame with US counties. It's data
#' \code{@slot} must always contain a column named and \code{countyFIPS} and a
#' column named \code{stateCode} if either \code{conusOnly = TRUE} or the
#' \code{stateCode} argument is specified.
#' @param breaks Numeric vector of break points.
#' @param palette A vector of colors or palette name from the \pkg{cols4all} package
#' (see \code{\link[cols4all:c4a]{cols4all::c4a}}).
#' @param conusOnly Logical specifying Continental US state codes. Ignored when
#' the \code{stateCode} argument is specified.
#' @param stateCode Vector of state codes to include on the map.
#' @param projection Named projection, \emph{e.g.} "EPSG:4326" or "WGS84" or proj4string.
#' @param stateBorderColor Color used for state borders.
#' @param countyBorderColor Color used for county borders.
#' @param title Vector of text strings to use as individual plot titles.
#' This must be the same length as 'parameter'.
#' @param showLegend Logical specifying whether or not to show the legend.
#' @param legendTitle Text string to use as the legend title.
#' @param legendOrientation Orientation of the legend. Either "portrait" or "landscape".
#' @param legendPosition A \emph{tm_pos} object generated with
#' \code{\link[tmap:tm_pos_in]{tmap::tm_pos_in()}} or
#' \code{\link[tmap:tm_pos_out]{tmap::tm_pos_out()}}.
#'
#' @return A ggplot object.
#'
#' @rdname countyMap
#'
#' @examples
#' \donttest{
#' library(MazamaSpatialPlots)
#' mazama_initialize()
#'
#' countyMap(
#'   data = example_US_countyCovid,
#'   parameter = "cases",
#'   breaks = c(0,100,200,500,1000,2000,5000,10000,20000,50000,1e6),
#'   title = "COVID-19 Cases on June 01 2020"
#' )
#'
#' countyMap(
#'   data = example_US_countyCovid,
#'   parameter = "deaths",
#'   state_SFDF = USCensusStates_02,
#'   county_SFDF = USCensusCounties_02,
#'   breaks = c(0, 1, 50, 100, 250, 500, 1000, 2500, 3000),
#'   palette = "brewer.or_rd",
#'   stateCode = c( "NY", "PA", "MD", "NJ", "DE"),
#'   stateBorderColor = "black",
#'   countyBorderColor = 'grey70'
#' ) +
#'   tmap::tm_layout(
#'     attr.color = 'white',
#'     bg.color = "dodgerblue4"
#'   ) +
#'   tmap::tm_title(
#'     text = "COVID-19 Deaths* in the Mid Atlantic",
#'     size = 2.0,
#'     color = "white",
#'   ) +
#'   tmap::tm_credits("*as of June 01, 2020", col = "white", position = "left")
#' }
#' @export
#' @importFrom sf st_bbox st_crs
#' @importFrom rlang .data
#' @importFrom tmap tm_shape tm_fill tm_polygons tm_layout
#'

countyMap <- function(
  data = NULL,
  parameter = NULL,
  state_SFDF = "USCensusStates_02",
  county_SFDF = "USCensusCounties_02",
  breaks = NULL,
  palette = "brewer.blues",
  conusOnly = TRUE,
  stateCode = NULL,
  projection = NULL,
  stateBorderColor = "gray50",
  countyBorderColor = "white",
  title = NULL,
  showLegend = TRUE,
  legendTitle = NULL,
  legendOrientation = c("portrait", "landscape"),
  legendPosition = NULL
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(data)
  MazamaCoreUtils::stopIfNull(parameter)

  legendOrientation <- match.arg(legendOrientation)

  # Check if data exists
  if ( !exists("data") ) {
    stop("Parameter 'data' is empty.")
  }

  # * Validate 'data' -----

  # Does 'parameter' exist in the incoming dataframe?
  if ( !parameter %in% colnames(data) ) {
    stop(sprintf("'%s' is not found in the incoming 'data' dataframe.", parameter))
  }

  # Does 'data' have the required columns?
  requiredFields <- c("countyFIPS")
  missingFields <- setdiff(requiredFields, names(data))
  if ( length(missingFields) > 0 ) {
    stop(paste0("Missing fields in 'data': ",
                paste0(missingFields, collapse = ", ")))
  }

  # * Validate state_SFDF -----

  # Accept state SFDF as character string or as object
  if ( is.character(state_SFDF) ) {
    if ( exists(state_SFDF) ) {
      state_SFDF <- get(state_SFDF)
    } else {
      stop(sprintf("State dataset '%s' is not loaded.
  Please load it with MazamaSpatialtUtils::loadSpatialData()",
                   state_SFDF
      ))
    }
  }

  # Does 'state_SFDF' have the required columns?
  # NOTE: state_SFDF does not require stateCode column if conusOnly = FALSE
  # NOTE: and stateCode argument is not specified
  if ( conusOnly == TRUE | !is.null(stateCode) ){
    requiredSFDFFields <- c("stateCode")
    missingSFDFFields <- setdiff(requiredSFDFFields, names(state_SFDF))
    if ( length(missingSFDFFields) > 0 ) {
      stop(paste0("Missing fields in 'state_SFDF': ",
                  paste0(missingSFDFFields, collapse = ", ")))
    }
  }

  # * Validate county_SFDF -----

  # Accept county SFDF as character string or as object
  if ( is.character(county_SFDF) ) {
    if ( exists(county_SFDF) ) {
      county_SFDF <- get(county_SFDF)
    } else {
      stop(sprintf("County dataset '%s' is not loaded.
  Please load it with MazamaSpatialtUtils::loadSpatialData()",
                   county_SFDF
      ))
    }
  }

  # Does 'county_SFDF' have the required columns?
  if ( conusOnly == TRUE | !is.null(stateCode) ){
    requiredSFDFFields <- c("countyFIPS", "stateCode")
  } else {
    requiredSFDFFields <- c("countyFIPS")
  }

  missingSFDFFields <- setdiff(requiredSFDFFields, names(county_SFDF))
  if ( length(missingSFDFFields) > 0 ) {
    stop(paste0("Missing fields in 'county_SFDF': ",
                paste0(missingSFDFFields, collapse = ", ")))
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
      projection <- sf::st_crs(projection)
    } else if ( "CRS" %in% class(projection) ) {
      # leave it alone
    } else {
      stop(paste0("Parameter 'projection' must be a sf::st_crs object or a valid ",
                  "projection string."))
    }
  }

  # Validate legendPosition
  if ( !is.null(legendPosition) ) {
    if ( !"tm_pos" %in% class(legendPosition) ) {
      stop("Parameter 'legendPosition' must be generated with tmap::tmap_pos_in() or tmap::tmap_pos_out()")
    }
  }

  # ----- Subset the SFDF ------------------------------------------------------

  if ( !is.null(stateCode) ) {

    # NOTE:  Subset doesn't work properly unless we change the name here
    incomingStateCode <- stateCode
    state_SFDF <- subset(state_SFDF, state_SFDF$stateCode %in% incomingStateCode)
    county_SFDF <- subset(county_SFDF, county_SFDF$stateCode %in% incomingStateCode)
    data <- data %>% dplyr::filter(.data$stateCode %in% incomingStateCode)

  } else if ( conusOnly ) {

    state_SFDF <- subset(state_SFDF, state_SFDF$stateCode %in% MazamaSpatialUtils::CONUS)
    county_SFDF <- subset(county_SFDF, county_SFDF$stateCode %in% MazamaSpatialUtils::CONUS)
    data <- data %>% dplyr::filter(.data$stateCode %in% MazamaSpatialUtils::CONUS)

  } else {

    # use existing SFDF

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

      # 1) Get boundaries from stateSFDF
      bbox <- sf::st_bbox(state_SFDF)

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
      projection <- sf::st_crs(projString)

    } else if ( conusOnly ) {

      projection <- sf::st_crs("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")

    } else {

      # TODO:  The 'conusOnly = FALSE' option needs to be replaced with the
      # TODO:  artifical projection that places Alaska and Hawaii in the lower
      # TODO:  left corner. Jon has seen such a projection in an example but
      # TODO:  can't remember exactly where.

      projection <- sf::st_crs("+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs")

    }

  } else {

    # Use projection found in SFDF

  }

  # ----- Merge data with SFDF -------------------------------------------------

  # NOTE:  We can use left_join() because 'countyFIPS' is guaranteed to be in
  # NOTE:  both dataframes. We use "matrix style" subsetting of 'data' to
  # NOTE:  specify that we want all rows and just the columns with "countyFIPS"
  # NOTE:  and the parameter of interest.

  # Add the incoming data$parameter to 'county_SFDF'.
  county_SFDF <-
    dplyr::left_join(
      county_SFDF,
      data[, c("countyFIPS", parameter)],
      by = "countyFIPS"
    )

  # ----- Create plot ----------------------------------------------------------

  gg <-
    tmap::tm_shape(county_SFDF, crs = projection) +
    tmap::tm_fill(
      fill = parameter,
      fill.scale = tmap::tm_scale_intervals(
        breaks = breaks,
        values = palette
      ),
      fill.legend = tmap::tm_legend(
        title = legendTitle,
        show = showLegend,
        orientation = legendOrientation,
        position = legendPosition
      )
    ) +
    tmap::tm_shape(county_SFDF, crs = projection) +
    tmap::tm_polygons(
      fill_alpha = 0,
      col = countyBorderColor
    ) +
    tmap::tm_shape(state_SFDF, crs = projection) +
    tmap::tm_polygons(
      fill_alpha = 0,
      col = stateBorderColor
    ) +
    tmap::tm_layout(
      frame = FALSE,
    ) +
    tmap::tm_title(
      text = title,
      size = .9,
      position = tmap::tm_pos_out("center", "top")
    )

  # ----- Return ---------------------------------------------------------------

  return(gg)

}

# ===== DEBUGGING ==============================================================

if ( FALSE ) {

  # NOTE:  Good idea to clear out the Global environment before running this.

  library(MazamaSpatialPlots)
  mazama_initialize()

  state_SFDF <- USCensusStates_02
  county_SFDF <- USCensusCounties_02

  # Set up required variables so we can walk through the code
  data = example_US_countyCovid
  parameter = "cases"
  breaks = NULL
  palette = "brewer.blues"
  conusOnly = TRUE
  stateCode = NULL
  projection = NULL
  stateBorderColor = "white"
  countyBorderColor = "gray90"
  title = "Covid cases by county -- June 01, 2020"
  showLegend = TRUE
  legendTitle = NULL
  legendOrientation = "portrait"
  legendPosition = NULL



  # Run the code above and then start walking through the lines of code in the
  # function.
  #
  # You can also source this file and run the function now

  countyMap(
    data = data,
    parameter = parameter,
    state_SFDF = state_SFDF,
    county_SFDF = county_SFDF,
    breaks = breaks,
    palette = palette,
    conusOnly = conusOnly,
    stateCode = stateCode,
    projection = projection,
    stateBorderColor = stateBorderColor,
    countyBorderColor = countyBorderColor,
    title = title
  )

  ##############################################################################
  # Here is how I would use this function in real life:

  countyMap(
    data,
    parameter = "cases",
    state_SFDF = state_SFDF,
    county_SFDF = county_SFDF
  )

  # Not bad but, because we are plotting raw numbers rather than the rate,
  # we should probably use an exponential set of breaks.

  countyMap(
    data,
    parameter = "cases",
    state_SFDF = state_SFDF,
    county_SFDF = county_SFDF,
    breaks = c(0,100,200,500,1000,2000,5000,10000,20000,50000,1e6)
  )

  # A few more tweaks with manual tmap additions
  # See: ?tmap::tm_layout

  countyMap(
    data,
    parameter = "cases",
    state_SFDF = state_SFDF,
    county_SFDF = county_SFDF,
    breaks = c(0,100,200,500,1000,2000,5000,10000,20000,50000,1e6)
  ) +
    tmap::tm_title(
      text = "Covid cases by county -- June 01, 2020",
      size = 2,
      fontface = "bold"
    )

  # Very nice!

}
