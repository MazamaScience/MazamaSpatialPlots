#' @title State level thematic map
#' @description Uses the \pkg{tmap} package to generate a thematic map at the
#' county level. Input consists of a dataframe with county identifiers.
#' 
#' @param data A dataframe containing values to plot a county map. At a minimum
#' there will be stateCode, countyName, and countyFIPS.
#' @param county_SPDF Vector of US counties.
#' @param state_SPDF ector of US States. 
#' @param paletteName A palette name or a vector of colors based on RColorBrewer.
#' @param breaks If style is fixed, breaks can be specified. 
#' @param conusOnly Logical specifying CONtinental US state codes.  
#' @param stateCode Vector of state codes.
#' @param projection Specified method to represent surface of Earth.
#' @param stateBorderColor The color of the state border to display.
#' @param countyBorderColor The color of the county borders to display.
#' @return A ggplot object.
#' 
#' @rdname stateMap
#' 
#' @examples
#' \donttest{
#' library(MazamaSpatialPlots)
#' 
#' stateMap(countyDF, "USCensusCounties_05")
#' 
#' }
#' @export 
#' @importFrom sp CRS
#' @importFrom stringr str_length
#' @importFrom rlang .data
#' 

stateMap <- function(
  data = NULL,
  county_SPDF = "USCensusCounties",
  state_SPDF = "USCensusStates",
  paletteName = "YlOrBr",
  #style = ifelse(is.null(breaks), "pretty", "fixed"),
  breaks = NULL,
  conusOnly = TRUE,
  stateCode = NULL,
  projection = NULL,
  stateBorderColor = "gray50",
  countyBorderColor = "white"
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(data)
  
  # Check if data exists
  if ( !exists("data") )
    stop("Parameter 'data' is empty.")
  
  # Accept county SPDF as character string or as object 
  if ( is.character(county_SPDF) ) 
    county_SPDF <- get(county_SPDF)
  
  # Accept state SPDF as character string or as object
  if ( is.character(state_SPDF) ) 
    state_SPDF <- get(state_SPDF)
  
  # Does 'data' have the required columns? 
  requiredFields <- c("stateCode", "countyName", "countyFIPS")
  missingFields <- setdiff(requiredFields, names(data))
  
  if ( length(missingFields) > 0 ) {
    stop(paste0("Missing fields in 'data': ", 
                paste0(missingFields, collapse = ", ")))
  }
  
  if ( !is.logical(conusOnly) ) 
    conusOnly <- TRUE
  
  # ----- Prepare data ---------------------------------------------------------
  
  # * Subset spatial data -----
  
  # Use stateCode if defined
  # Else use conusOnly if defined
  # Else use everything
  
  if ( !is.null(stateCode) ) {
    
    county_SPDF <- subset(county_SPDF, county_SPDF$stateCode %in% stateCode)
    state_SPDF <- subset(state_SPDF, state_SPDF$stateCode %in% stateCode)
    data <- data %>% dplyr::filter(.data$stateCode %in% stateCode)
    
  } else if ( conusOnly ) {
    
    county_SPDF <- subset(county_SPDF, county_SPDF$stateCode %in% MazamaSpatialUtils::CONUS)
    state_SPDF <- subset(state_SPDF, state_SPDF$stateCode %in% MazamaSpatialUtils::CONUS)
    data <- data %>% dplyr::filter(.data$stateCode %in% MazamaSpatialUtils::CONUS)
    
  }
  
  # Else keep all  states
  
  
  # * Project data -----
  
  # Use profj if defined
  # Else use CONUS default projection
  # Else use county_SPDF internal projrection
  
  if ( !is.null(proj) ) {
    
    # TODO:  project county_SPDF and state_SPDF
    
  } else if ( conusOnly ) {
    
    # TODO:
    
  }
  
  
  ##############################################################################  
  ##############################################################################  
  ##############################################################################  
  
  
  
  
  
  
  if ( !is.null(county_SPDF$countyName) ) {
    county_SPDF <- subset(county_SPDF, countyName %in% countyName)
  }
  
  if ( !is.null(state_SPDF$stateCode) ) {
    state_SPDF <- subset(state_SPDF, stateCode %in% stateCode)
  }
  
  # CONUS specification 
  if ( !is.logical(conusOnly) ) {
    conusOnly <- TRUE
    state_SPDF <- subset(state_SPDF, stateCode %in% MazamaSpatialUtils::CONUS)
    conus_proj <- sp::CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")
  }
  
  
  # PROJECTION
  # TODO:   * If 'is.null(projection)', use the projection associated with 'SPDF'.
  if ( !is.null(projection) ) {
    county_SPDF <- projection
    county_SPDF@proj4string <- sp::CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs")
  }
  
  if ( !is.null(projection) ) {
    state_SPDF <- projection
    state_SPDF@proj4string <- sp::CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs")
  }
  
  # Breaks Need to add in style in order to make this work? 
  if ( !is.null(breaks) ) {
    stop("Please specify breaks.")
  }
  
  # ----- Merge data with SPDF -------------------------------------------------
  
  # For US state code, can find if it matches MazamaSpatialUtils::US_52 (Vector of 52 elements) 
  
  all(data$stateCode %in% MazamaSpatialUtils::US_52) 
  
  setdiff(data$countyName, county_SPDF$countyName)
  setdiff(data$countyFIPS, county_SPDF$countyFIPS) # Interesting... length 3 vs length 5
  setdiff(data$stateCode, state_SPDF$stateCode)
  
  # stateCode should be length 2 and countyFIPS should be 3?
  
  #  if ( stringr::str_count(data$stateCode) != 2 )
  #    stop("Length of stateCode should be [2].")
  
  #  if ( stringr::str_count(data$countyFIPS) != "5" )
  #    stop("Length of countyFIPS should be [5].")
  
  # For the below, is Jon asking that
  
  # TODO:  Should have a way to support either 'countyFIPS' or
  # TODO:  'stateCode'+'countyName'. We will soon have utility functions in
  # TODO:  MazamaSpatialUtils to help with this.
  
  # ----- Create plot ----------------------------------------------------------
  
  # TODO:  Using 'local_jon/JHU_deths.R' as an example, put together an 
  # TODO:  attractive map.
  
  # Most basic map
  # tm_shape(data) +
  #  tm_polygons()
  
  gg <-
    tm_shape(data) +
    tm_polygons(
      projection = conus_proj,
      paletteName = "Blues",
      countyBorderColor = "Black"
    ) +
    tm_shape(USCensusStates, projection = conus_proj) +
    tm_polygons(
      alpha = 0,
      border.col = "gray50"
    ) +
    tm_layout(
      title = paste0("Cumulative COVID-19 Deaths per County"),
      title.size = 1.1,
      title.position = c("center", "top"),
      frame = FALSE
    )
  
  tmap_save(gg, "test_county.png")
  
  # ----- Return ---------------------------------------------------------------
  
  return(gg)
  
}

# ===== DEBUGGING ==============================================================

if ( FALSE ) {
  
  library(MazamaSpatialPlots)
  mazama_initialize()
  
  loadSpatialData("USCensusCounties_05")
  
  # Set up required variables so we can walk through the code
  data = USCensusCounties@data
  county_SPDF = "USCensusCounties"
  state_SPDF = "USCensusStates"
  paletteName = "YlOrBr"
  #style = ifelse(is.null(breaks), "pretty", "fixed"),
  breaks = NULL
  conusOnly = TRUE
  stateCode = NULL
  projection = NULL
  stateBorderColor = "gray50"
  countyBorderColor = "white"
  
  # Run the code above and then start walking through the lines of code in the
  # function.
  
}
