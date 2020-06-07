#' @title County level thematic map
#' @description Uses the \pkg{tmap} package to generate a thematic map at the
#' county level. Input consists of a dataframe with county identifiers.
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
#' @importFrom sp CRS
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
  
  # Check if data exists or is null 
  if ( !is.null(data) && !exists("data") )
    stop("Parameter 'data' is empty.")
  
  # TODO:  Accept SPDF as character string or as object
  if ( is.character(county_SPDF) ) 
    county_SPDF <- get(county_SPDF)
  
  # TINA:  As a style issue, I often omit the curly braces for simple, one-line
  # TINA:  error messages. I find it makes the code easier to read.
  
  # TINA:  Please not the use of extra spaces in the 'git diff'. We mostly
  # TINA:  the Hadley Wickham guidelines except that I like an additional space
  # TINA:  around logical statements in "if (<space>...<space>)". This makes it
  # TINA:  easier to copy-paste the logical statement when debugging the code.
  # TINA:  You can have Rstudio alert you about spacing by going to 
  # TINA:  RStudio > Preferences > Code > Diagnostices > [ ] Provide R style diagnostics
  
  # Do we have the required columns? 
  requiredFields <- c("stateCode", "countyName", "countyFIPS")
  missingFields <- setdiff(requiredFields, names(county_SPDF))
  if ( length(missingFields) > 0 )
    stop(paste0("Missing fields in 'county_SPDF': ", paste0(missingFields, collapse = ", ")))
  
  requiredFields <- c("stateCode", "countyName", "countyFIPS")
  missingFields <- setdiff(requiredFields, names(data))
  if ( length(missingFields) > 0 )
    stop(paste0("Missing fields in 'data': ", paste0(missingFields, collapse = ", ")))

  # ----- Prepare data ---------------------------------------------------------
  
  # TODO:   * If 'is.null(projection)', use the projection associated with 'SPDF'.
    if ( !is.null(sp::proj4string(projection)) ) {
      SPDF <- projection
      SPDF@proj4string <- sp::CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs")
    }
      # can it just be if(!is.null(projection))
      #                   proj4string(SPDF) <- projection 
  
  # TODO:   * If '!is.null(stateCode)', subset the 'SPDF'.
    if ( !is.null(stateCode) ) {
      SPDF <- subset(SPDF, stateCode %in% stateCode)
    }

  # Breaks Need to add in style in order to make this work? 
  if ( !is.null(data) ) {
    stop("Please specify breaks.")
  }
  
  if ( conusOnly ) {
    county_SPDF <- subset(county_SPDF, stateCode %in% MazamaSpatialUtils::CONUS)
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
