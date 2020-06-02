#' @title County level thematic map
#' @description Uses the \pkg{tmap} package to generate a thematic map at the
#' county level. Input consists of a dataframe with county identifiers.
#' @param data TODO
#' @param countySPDF TODO
#' @param stateSPDF TODO
#' @param paletteName TODO
#' @param breaks TODO
#' @param conusOnly TODO
#' @param stateCode TODO
#' @param projection TODO
#' @param stateBorderColor TODO
#' @param countyBorderColor TODO
#' @return A ggplot object.
#' @rdname countyMap
#' @examples
#' \donttest{
#' library(MazamaSpatialPlots)
#' 
#' # TODO:  Load package internal example_countyDF
#' 
#' countyMap(countyDF, "USCensusCounties_05")
#' 
#' }
#' @export 
#' 

countyMap <- function(
  data = NULL,
  countySPDF = "USCensusCounties",
  stateSPDF = "USCensusStates",
  paletteName = "YlOrBr",
  breaks = NULL,
  conusOnly = TRUE,
  stateCode = NULL,
  projection = NULL,
  stateBorderColor = "gray50",
  countyBorderColor = "white"
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(data)
  
  # TODO:  Accept SPDF as character string or as object
  
  # TODO:  Validate everything and set appropriate defaults. Examples:
  # TODO:   * If 'is.null(projection)', use the projection associated with 'SPDF'.
  # TODO:   * If '!is.null(stateCode)', subset the 'SPDF'.
  
  
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
