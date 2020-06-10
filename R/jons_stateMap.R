#' @title State level thematic map
#' @description Uses the \pkg{tmap} package to generate a thematic map at the
#' county level. Input consists of a dataframe with county identifiers.
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
#' @param data A dataframe containing values to plot a state map. This dataframe
#' must contain a column named \code{stateCode} with the 2-character state code.
#' @param state_SPDF ector of US States. 
#' @param parameter Name of the column of data in \code{state_SPDF} to use fo
#' coloring the map.
#' @param palette A palette name or a vector of colors based on RColorBrewer.
#' @param breaks Numeric vector of break points. Must be 1 greater than \code{n}. 
#' @param conusOnly Logical specifying CONtinental US state codes.  
#' @param stateCode Vector of state codes.
#' @param projection Specified method to represent surface of Earth.
#' @param stateBorderColor The color of the state border to display.
#' @param title Text string to use as the plot title.
#' @return A ggplot object.
#' 
#' @rdname jons_stateMap
#' 
#' @examples
#' \donttest{
#' library(MazamaSpatialPlots)
#' 
#' jons_stateMap(example_US_stateObesity)
#' 
#' }
#' @export 
#' @importFrom sp CRS
#' @importFrom stringr str_length
#' @importFrom rlang .data
#' @importFrom tmap tm_shape tm_fill tm_polygons tm_layout
#' 

jons_stateMap <- function(
  data = NULL,
  state_SPDF = "USCensusStates",
  parameter = NULL,
  palette = "YlOrBr",
  breaks = NULL,
  conusOnly = TRUE,
  stateCode = NULL,
  projection = NULL,
  stateBorderColor = "gray50",
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
    state_SPDF <- get(state_SPDF)
  }
  
  # Does 'data' have the required columns? 
  requiredFields <- c("stateCode")
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
    state_SPDF <- subset(state_SPDF, state_SPDF$stateCode %in% stateCode)
    data <- data %>% dplyr::filter(.data$stateCode %in% stateCode)
  } else if ( conusOnly ) {
    state_SPDF <- subset(state_SPDF, state_SPDF$stateCode %in% MazamaSpatialUtils::CONUS)
    data <- data %>% dplyr::filter(.data$stateCode %in% MazamaSpatialUtils::CONUS)
  } else {
    # use existing 
  }
  
  # * Project data -----
  
  if ( is.null(projection) ) {
    if ( conusOnly ) {
      projection <- sp::CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")
    } else {
      # TODO:  Ideally, we could pick a nice projection based on the bounding box
      # TODO:  of the subset of states.
      # Use native projection
      projection <- state_SPDF@proj4string
    }
  } else {
    # use as provided
  }
  
  # ----- Merge data with SPDF -------------------------------------------------
  
  # TINA:  We can use left_join() because 'stateCode' is guaranteed to be in
  # TINA:  both dataframes. We use "matrix style" subsetting of 'data' to
  # TINA:  specify that we want all rows and just the columns with "stateCode"
  # TINA:  and the parameter of interest.
  
  # Add the incoming data$parameter to 'state_SPDF@data'.
  state_SPDF@data <-
    dplyr::left_join(
      state_SPDF@data,
      data[, c("stateCode", parameter)],
      by = "stateCode"
    )
  
  # ----- Create plot ----------------------------------------------------------

  ##############################################################################  
  # TINA:  I developed this a piece at a time like this:
  # TINA:  Remove this if (FALSE) chunk
  
  if ( FALSE) {
  
    # Attempt 1) get anything plotting
    tmap::tm_shape(state_SPDF) +
      tmap::tm_polygons()
    
    # Attempt 2) get chloropleth map
    tmap::tm_shape(state_SPDF, projection = projection) +
      tmap::tm_fill(
        col = parameter,    # just this at first
        palette = palette,  # then added this
        breaks = breaks     # then added this
      )
    
    # I had tried using the 'n' parameter to tm_fill() but couldn't get it to
    # work so I just bailed on that and let tm_fill() do it's default thing.
    
    # Attempt 3-6 added in the projection, state borders and title to get
    # the code you see below
      
  }
  ##############################################################################  
  
  
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
      title = title,
      title.size = 1.1,
      title.position = c("center", "top"),
      frame = FALSE
    )
  
  # ----- Return ---------------------------------------------------------------
  
  # TINA:  Don't return(invisible(gg)) because we want it to plot by default
  
  return(gg)
  
}

# ===== DEBUGGING ==============================================================

if ( FALSE ) {
  
  # TINA:  Good idea to clear out the Global environment before running this.
  
  library(MazamaSpatialPlots)
  mazama_initialize()
  
  state_SPDF <- subset(NaturalEarthAdm1, countryCode == "US")
  
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
  
  jons_stateMap(
    data = data,
    state_SPDF = state_SPDF,
    parameter = parameter,
    palette = palette,
    breaks = breaks,
    conusOnly = conusOnly,
    stateCode = stateCode,
    projection = projection,
    stateBorderColor = stateBorderColor,
    title = title
  )
  
  # Here is how I would use this function in real life:
  
  jons_stateMap(
    data, 
    parameter = "obesityRate", 
    state_SPDF = state_SPDF
  )
  
  # Not bad
  
  jons_stateMap(
    data, 
    parameter = "obesityRate", 
    state_SPDF = state_SPDF,
    palette = "BuPu",
    stateBorderColor = "black"
  )
  
  # Better but still need a title
  
  jons_stateMap(
    data, 
    parameter = "obesityRate", 
    state_SPDF = state_SPDF,
    palette = "BuPu",
    stateBorderColor = "black",
    title = "2018 Obesity by State"
  )
  
  # Very nice!
  
}
