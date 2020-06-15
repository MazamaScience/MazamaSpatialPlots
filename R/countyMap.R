#' @title County level thematic map
#' @description Uses the \pkg{tmap} package to generate a thematic map at the
#' state level. Input consists of a dataframe with \code{countyFIPS} identifiers.
#' 
#' @details See \code{tmap::tm_fill()} for a more detailed description of
#' the following parameters:
#' 
#' \itemize{
#' \item{\code{palette}}
#' \item{\code{n}}
#' \item{\code{breaks}}
#' }
#' 
#' @param data Dataframe containing values to plot. This dataframe
#' must contain a column named \code{countyFIPS} with the 5-digit FIPS code.
#' @param parameter Name of the column of data in \code{county_SPDF} to use for
#' coloring the map.
#' @param state_SPDF SpatialPolygonsDataFrame with US states. 
#' @param county_SPDF SpatialPolygonsDataFrame with US counties. 
#' @param palette Palette name or a vector of colors based on RColorBrewer.
#' @param breaks Numeric vector of break points. Must be 1 greater than \code{n}. 
#' @param conusOnly Logical specifying CONtinental US state codes.  
#' @param stateCode Vector of state codes.
#' @param projection Specified method to represent surface of Earth.
#' @param stateBorderColor Color used for state borders.
#' @param countyBorderColor Color used for county borders.
#' @param title Text string to use as the plot title.
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
#'   title = "Covid cases by county -- June 01, 2020"
#' )
#' }
#' @export 
#' @importFrom sp CRS
#' @importFrom stringr str_length
#' @importFrom rlang .data
#' @importFrom tmap tm_shape tm_fill tm_polygons tm_layout
#' 

countyMap <- function(
  data = NULL,
  parameter = NULL,
  state_SPDF = "USCensusStates_02",
  county_SPDF = "USCensusCounties_02",
  palette = "YlOrBr",
  breaks = NULL,
  conusOnly = TRUE,
  stateCode = NULL,
  projection = NULL,
  stateBorderColor = "gray50",
  countyBorderColor = "white",
  title = NULL
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(data)
  MazamaCoreUtils::stopIfNull(parameter)
  
  # Check if data exists
  if ( !exists("data") ) {
    stop("Parameter 'data' is empty.")
  }
  
  # Does 'parameter' exist in the incoming dataframe?
  if ( !parameter %in% colnames(data) ) {
    stop(sprintf("'%s' is not found in the incoming 'data' dataframe.", parameter))
  }
  
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
  
  # Accept county SPDF as character string or as object
  if ( is.character(county_SPDF) ) {
    if ( exists(county_SPDF) ) {
      county_SPDF <- get(county_SPDF)
    } else {
      stop(sprintf("County dataset '%s' is not loaded.
  Please load it with MazamaSpatialtUtils::loadSpatialData()",
                   county_SPDF
      ))
    }
  }
  
  # Does 'data' have the required columns? 
  requiredFields <- c("countyFIPS")
  missingFields <- setdiff(requiredFields, names(data))
  if ( length(missingFields) > 0 ) {
    stop(paste0("Missing fields in 'data': ", 
                paste0(missingFields, collapse = ", ")))
  }
  
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
  
  # ----- Prepare data ---------------------------------------------------------
  
  # * Subset data -----
  
  if ( !is.null(stateCode) ) {
    # NOTE:  Subset doesn't work properly unless we change the name here
    incomingStateCode <- stateCode
    state_SPDF <- subset(state_SPDF, state_SPDF$stateCode %in% incomingStateCode)
    county_SPDF <- subset(county_SPDF, county_SPDF$stateCode %in% incomingStateCode)
    data <- data %>% dplyr::filter(.data$stateCode %in% incomingStateCode)
  } else if ( conusOnly ) {
    state_SPDF <- subset(state_SPDF, state_SPDF$stateCode %in% MazamaSpatialUtils::CONUS)
    county_SPDF <- subset(county_SPDF, county_SPDF$stateCode %in% MazamaSpatialUtils::CONUS)
    data <- data %>% dplyr::filter(.data$stateCode %in% MazamaSpatialUtils::CONUS)
  } else {
    # use existing 
  }
  
  # * Project data -----
  
  if ( is.null(projection) ) {
    if ( !is.null(stateCode) ) {
      # TODO:  1) Get boundaries from stateSPDF
      bbox <- sp::bbox(state_SPDF)
      # TODO:  2) Calculate lat lo/mid/hi and lon mid
      lat_1 <- 40 # TODO:  git it from bbox
      lat_2 <- 50 # TODO:  git it from bbox
      lat_0 <- 45 # TODO:  git it from bbox
      lon_0 <- -122 # TODO:  git it from bbox
      # TODO:  3) Create the proj4string text from these using sprintf()
      projString <- sprintf("+proj=aea +lat_1=%.1f +lat_2=%.1f +lat_0=%.1f +lon_0=%.1f +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs",
                            lat_1, lat_2, lat_0, lon_0)
      projection <- sp::CRS(projString)
    } else if ( conusOnly ) {
      projection <- sp::CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")
    } else {
      # Use native projection
      projection <- state_SPDF@proj4string
    }
  } else {
    # use as provided
  }
  
  # ----- Merge data with SPDF -------------------------------------------------
  
  # NOTE:  We can use left_join() because 'countyFIPS' is guaranteed to be in
  # NOTE:  both dataframes. We use "matrix style" subsetting of 'data' to
  # NOTE:  specify that we want all rows and just the columns with "countyFIPS"
  # NOTE:  and the parameter of interest.
  
  # Add the incoming data$parameter to 'county_SPDF@data'.
  county_SPDF@data <-
    dplyr::left_join(
      county_SPDF@data,
      data[, c("countyFIPS", parameter)],
      by = "countyFIPS"
    )
  
  # ----- Create plot ----------------------------------------------------------

  gg <-
    tmap::tm_shape(county_SPDF, projection = projection) +
    tmap::tm_fill(
      col = parameter,
      palette = palette,
      breaks = breaks
    ) +
    tmap::tm_shape(county_SPDF, projection = projection) +
    tmap::tm_polygons(
      alpha = 0,
      border.col = countyBorderColor
    ) +
    tmap::tm_shape(state_SPDF, projection = projection) +
    tmap::tm_polygons(
      alpha = 0,
      border.col = stateBorderColor
    ) +
    tmap::tm_layout(
      title = title,
      title.size = 1.1,
      title.position = c("center", "top"),
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
  county_SPDF <- USCensusCounties_02
  
  # Set up required variables so we can walk through the code
  data = example_US_countyCovid
  parameter = "cases"
  palette = "YlOrBr"
  breaks = NULL
  conusOnly = TRUE
  stateCode = NULL
  projection = NULL
  stateBorderColor = "white"
  countyBorderColor = "gray90"
  title = "Covid cases by county -- June 01, 2020"
  
  # Run the code above and then start walking through the lines of code in the
  # function.
  #
  # You can also source this file and run the function now
  
  countyMap(
    data = data,
    parameter = parameter,
    state_SPDF = state_SPDF,
    county_SPDF = county_SPDF,
    palette = palette,
    breaks = breaks,
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
    state_SPDF = state_SPDF,
    county_SPDF = county_SPDF
  )
  
  # Not bad be, because we are plotting raw numbers rather than the rate,
  # we should probably use an exponential set of breaks.
  
  countyMap(
    data, 
    parameter = "cases", 
    state_SPDF = state_SPDF,
    county_SPDF = county_SPDF,
    breaks = c(0,100,200,500,1000,2000,5000,10000,20000,50000,1e6)
  )
  
  # A few more tweaks with manual tmap additions
  # See: ?tmap::tm_layout
  
  countyMap(
    data, 
    parameter = "cases", 
    state_SPDF = state_SPDF,
    county_SPDF = county_SPDF,
    breaks = c(0,100,200,500,1000,2000,5000,10000,20000,50000,1e6)
  ) +
    tmap::tm_layout(
      title = "Covid cases by county -- June 01, 2020",
      title.size = 2,
      title.fontface = "bold"
    )
  
  # Very nice!
  
}
