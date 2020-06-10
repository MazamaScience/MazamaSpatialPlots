#' @title State level thematic map
#' @description Uses the \pkg{tmap} package to generate a thematic map at the
#' county level. Input consists of a dataframe with county identifiers.
#' 
#' @param data A dataframe containing values to plot a county map. At a minimum
#' there will be stateCode, countyName, and countyFIPS.
#' @param state_SPDF ector of US States. 
#' @param paletteName A palette name or a vector of colors based on RColorBrewer.
#' @param breaks If style is fixed, breaks can be specified. 
#' @param conusOnly Logical specifying CONtinental US state codes.  
#' @param stateCode Vector of state codes.
#' @param projection Specified method to represent surface of Earth.
#' @param stateBorderColor The color of the state border to display.
#' @return A ggplot object.
#' 
#' @rdname stateMap
#' 
#' @examples
#' \donttest{
#' library(MazamaSpatialPlots)
#' 
#' stateMap(stateDF, "USCensusCounties_05")
#' 
#' }
#' @export 
#' @importFrom sp CRS
#' @importFrom stringr str_length
#' @importFrom rlang .data
#' 

  stateMap <- function(
    data = NULL,
    state_SPDF = "USCensusStates",
    paletteName = "YlOrBr",
    breaks = NULL,
    conusOnly = TRUE,
    stateCode = NULL,
    projection = NULL,
    stateBorderColor = "gray50"
  ) {
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(data)
  
  # Check if data exists
  if ( !exists("data") )
    stop("Parameter 'data' is empty.")
  
  # Accept state SPDF as character string or as object
  if ( is.character(state_SPDF) ) 
    state_SPDF <- get(state_SPDF)
  
  # Does 'data' have the required columns? 
  requiredFields <- c("stateCode", "stateName")
  missingFields <- setdiff(requiredFields, names(data))
  
  if ( length(missingFields) > 0 ) {
    stop(paste0("Missing fields in 'data': ", 
                paste0(missingFields, collapse = ", ")))
  }
  
  if ( !is.logical(conusOnly) ) 
    conusOnly <- TRUE
  
  # ----- Prepare data ---------------------------------------------------------
  
  # For US state code, can find if it matches MazamaSpatialUtils::US_52 (Vector of 52 elements) 
  all(data$stateCode %in% MazamaSpatialUtils::US_52) 
  nomatchStateCodes <- setdiff(data$stateCode, state_SPDF$stateCode)
  nomatchStateNames <- setdiff(data$stateName, state_SPDF$stateName)
  
  # Check to see if stateCode is length 2. If not, return non-match.
  if ( !all(stringr::str_count(data$stateCode) == "2") ) {
    stop(paste0("Non-matching state codes: ", paste0(nomatchStateCodes, collapse = ", ")))
  }
  
  # Check if state names match. If not return non-match.
  if ( !all(data$stateName %in% state_SPDF$stateName) ) {
    stop(paste0("Non-matching state names: ", paste0(nomatchStateNames, collapse = ", ")))
  }
  
  if ( !is.null(stateCode) ) {
    state_SPDF <- subset(state_SPDF, state_SPDF$stateCode %in% stateCode)
    data <- data %>% dplyr::filter(.data$stateCode %in% stateCode)
  } else if ( conusOnly ) {
    state_SPDF <- subset(state_SPDF, state_SPDF$stateCode %in% MazamaSpatialUtils::CONUS)
    data <- data %>% dplyr::filter(.data$stateCode %in% MazamaSpatialUtils::CONUS)
  } else {
    state_SPDF <- state_SPDF
    data <- data
  }
  
  # * Project data -----
  
  if ( !is.null(projection) ) {
    state_SPDF@proj4string <- sp::CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs")
    } else if ( conusOnly ) {
      conus_proj <- sp::CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")
    } else {
      state_SPDF@proj4string <- sp::CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs")
    }

  # Breaks Need to add in style in order to make this work? 
  if ( !is.null(breaks) ) {
    stop("Please specify breaks.")
  }
 
  # ----- Merge data with SPDF -------------------------------------------------
  
  # TODO:  Should have a way to support either 'countyFIPS' or
  # TODO:  'stateCode'+'countyName'. We will soon have utility functions in
  # TODO:  MazamaSpatialUtils to help with this.
  
  # ----- Create plot ----------------------------------------------------------
  
  
  
  #############################################################
  # making corrections right now to join datasets - wip 

  state_DF <- dplyr::left_join(
    USCensusStates@data,
    data,
    by = c("stateCode", "stateName")
  )
  state_SPDF@data <- state_DF
  
  # Most basic map
   tm_shape(state_SPDF) +
    tm_polygons() 
  # Error in `$<-.data.frame`(`*tmp*`, "geometry", value = list(list(list( : 
  # replacement has 49 rows, data has 56
  # probably when I did data <- data, it didn't match back up. Let's see. OR somehow 
  # only be able to match back to both state code and state name to SPDF.
  ############################################################# 
   
  stateMap <-
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
  
  #tmap_save(stateMap, "test_county.png")
  
  # ----- Return ---------------------------------------------------------------
  
  return(invisible(stateMap))
  
}

# ===== DEBUGGING ==============================================================

  if ( FALSE ) {
    
    library(MazamaSpatialPlots)
    mazama_initialize()
    
    loadSpatialData("USCensusCounties_05")
    
    # Set up required variables so we can walk through the code
    data = USCensusCounties@data
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
  