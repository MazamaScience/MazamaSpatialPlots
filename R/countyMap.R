#' @title County level thematic map
#' @description Uses the \pkg{tmap} package to generate a thematic map at the
#' county level. Input consists of a dataframe with county identifiers.
#' @param import dplyr tmap MazamaSpatialUtils sp 
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
#' @rdname countyMap
#' @examples
#' \donttest{
#' library(MazamaSpatialPlots)
#' 
#' countyMap(countyDF, "USCensusCounties_05")
#' 
#' }
#' @export 
#' 

countyMap <- function(
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
  
    if(!is.null(sp::proj4string(projection))){
      SPDF <- projection
      SPDF@proj4string <-sp::CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs")
    }
      # can it just be if(!is.null(projection))
      #                   proj4string(SPDF) <- projection 
  
    if(!is.null(stateCode)){
      SPDF <-SPDF[SPDF$stateCode %in% stateCode,]
    }
  
  # TODO:  Accept SPDF as character string or as object
  
  # TODO:  Validate everything and set appropriate defaults. Examples:
  # TODO:   * If 'is.null(projection)', use the projection associated with 'SPDF'.
  # TODO:   * If '!is.null(stateCode)', subset the 'SPDF'.
  
  # Check existence of dataset
  if ( !exists(data) ) {
    stop("Missing dataset. Please loadSpatialData(\"",dataset,"\")",
         call. = FALSE)
  }
  
  # Do we have the required columns? 
  requiredFields <- c("stateCode", "countyName", "countyFIPS")
  missingFields <- setdiff(requiredFields, names(SPDF))
  if ( length(missingFields) > 0 ) {
    stop(paste0('Missing fields in SPDF: ', missingFields))
  }
  
  # ----- Merge data with SPDF -------------------------------------------------
  
  # TODO:  Should have a way to support either 'countyFIPS' or
  # TODO:  'stateCode'+'countyName'. We will soon have utility functions in
  # TODO:  MazamaSpatialUtils to help with this.
  
  # ----- Create plot ----------------------------------------------------------
  
  # TODO:  Using 'local_jon/JHU_deths.R' as an example, put together an 
  # TODO:  attractive map.
  
  # gg <-
  #   tmap::tm_shape(...) +
  #   tmap::tm_polygons(...) + 
  #   ...
  
  # ----- Return ---------------------------------------------------------------
  
  #return(gg)
  
}
