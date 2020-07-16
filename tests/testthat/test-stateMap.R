# -----------------------------------------------------------------------------
testthat::context("stateMap()")

setup_spatial_data <- function() {
  
  skip_on_cran()
  skip_on_travis()
  
  # try to set up spatial data. Skip if fails.  
  spatialDataDir <- try(getSpatialDataDir(), silent = TRUE)
  
  # load USCensusStates_02 (default state_SPDF)
  if (!exists('USCensusStates_02')) {
    tryCatch(getSpatialDataDir(), 
             error = function(error) {
               setSpatialDataDir("~/Data/Spatial") 
             })
    tryCatch(loadSpatialData("USCensusStates_02"),
             error = function(error) {
               message("Could not load USCensusStates_02")
             })
  }
  if (!exists("USCensusStates_02")) {
    skip("Could not load USCensusStates_02")
  }
  
  #load example_US_stateObesity (example data frame)
  if (!exists('example_US_stateObesity')) {
    tryCatch(getSpatialDataDir(), 
             error = function(error) {
               setSpatialDataDir("~/Data/Spatial") 
             })
    tryCatch(loadSpatialData("example_US_stateObesity"),
             error = function(error) {
               message("Could not load example_US_stateObesity")
             })
  }
  if (!exists("example_US_stateObesity")) {
    skip("Could not load example_US_stateObesity")
  }
  
  return (spatialDataDir)
  
}

testthat::test_that("handles errors correctly", {
  
  skip_on_cran()
  skip_on_travis()
  
  # Setup
  spatialDataDir <- setup_spatial_data()
  
  testthat::expect_error(stateMap())
  testthat::expect_error(stateMap(example_US_stateObesity))
  testthat::expect_error(stateMap(example_US_stateObesity, 'obesityRate', state_SPDF = 'wrongSPDF'),
                         "State dataset 'wrongSPDF' is not loaded.\n  Please load it with MazamaSpatialtUtils::loadSpatialData()"
                         )
  testthat::expect_error(stateMap(example_US_stateObesity, 'wrongParameter'),
                         "'wrongParameter' is not found in the incoming 'data' dataframe."
                         )
  testthat::expect_error(stateMap(example_US_stateObesity[, c("stateName", "obesityRate")], 'obesityRate'),
                         "Missing fields in 'data': stateCode")
  
  #if we add error message in function under parameter validation
  testthat::expect_error(stateMap(example_US_stateObesity, 'obesityRate', state_SPDF =  USCensusStates_02[, c("countryCode", "stateFIPS")]),
                         "Missing fields in 'state_SPDF': stateCode")
  
  testthat::expect_error(stateMap(example_US_stateObesity, 'obesityRate', breaks=c('1','2','3')),
                         "Parameter 'breaks' must be a numeric vector."
                         )
  testthat::expect_error(stateMap(example_US_stateObesity, 'obesityRate', breaks=c(1,2,'a')),
                         "Parameter 'breaks' must be a numeric vector."
                         )
  testthat::expect_error(stateMap(example_US_stateObesity, 'obesityRate', stateCode = c(1,2,3)),
                         "Invalid state codes found:  1, 2, 3"
                         )
  testthat::expect_error(stateMap(example_US_stateObesity, 'obesityRate', stateCode = c('ME','MM')),
                         "Invalid state codes found:  MM"
                         )
  testthat::expect_error(stateMap(example_US_stateObesity, 'obesityRate', stateCode = 3),
                         "Invalid state codes found:  3"
                         )
  testthat::expect_error(stateMap(example_US_stateObesity, 'obesityRate', projection = 2),
                         "Parameter 'projection' must be a sp::CRS object or a valid projection string."
                         )
  # if its a character but not valid it still tries to convert to CRS. 
  #There's still an error bc of this. Is that ok? Or should this be in validate parameters section of stateMap()?
  testthat::expect_error(stateMap(example_US_stateObesity, 'obesityRate', projection = '+bad projection'))
                         
  # Teardown
  if (class(spatialDataDir) == "character") {
    setSpatialDataDir(spatialDataDir)
  } else {
    removeSpatialDataDir()
  }
  
})

testthat::test_that("subsets by stateCode correctly", {
  
  skip_on_cran()
  skip_on_travis()
  
  # Setup
  spatialDataDir <- setup_spatial_data()
  
  stateCodeList <- c('WA','OR')
  plot_states <- stateMap(example_US_stateObesity, 'obesityRate', stateCode = stateCodeList)$tm_shape$shp@data$stateCode
  
  testthat::expect_match(length(plot_states), length(stateCodeList))
  testthat::expect_true('WA' %in% plot_states)
  testthat::expect_true('OR' %in% plot_states)
  
  # Teardown
  if (class(spatialDataDir) == "character") {
    setSpatialDataDir(spatialDataDir)
  } else {
    removeSpatialDataDir()
  }
  
})

testthat::test_that("returns correct object type", {
  
  skip_on_cran()
  skip_on_travis()
  
  # Setup
  spatialDataDir <- setup_spatial_data()
  
  testthat::expect_true(class(stateMap(example_US_stateObesity, 'obesityRate')) == 'tmap')
  
  # Teardown
  if (class(spatialDataDir) == "character") {
    setSpatialDataDir(spatialDataDir)
  } else {
    removeSpatialDataDir()
  }
  
})



