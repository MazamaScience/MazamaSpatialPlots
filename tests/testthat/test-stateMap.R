# -----------------------------------------------------------------------------
testthat::context("stateMap()")

testthat::test_that("make sure data exists", {
  
  skip_on_cran()
  skip_on_travis()
  
  testthat::expect_true(exists("USCensusStates_02"))
  testthat::expect_true(exists("example_US_stateObesity"))

})

testthat::test_that("handles errors correctly", {
  
  skip_on_cran()
  skip_on_travis()
  
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
  
})

testthat::test_that("subsets by stateCode correctly", {
  
  skip_on_cran()
  skip_on_travis()
  
  stateCodeList <- c('WA','OR')
  plottedStates <- stateMap(example_US_stateObesity, 'obesityRate', stateCode = stateCodeList)$tm_shape$shp@data$stateCode
  
  testthat::expect_equal(length(plottedStates), length(stateCodeList))
  testthat::expect_true('WA' %in% plottedStates)
  testthat::expect_true('OR' %in% plottedStates)
  
})

testthat::test_that("returns correct object type", {
  
  skip_on_cran()
  skip_on_travis()
  
  testthat::expect_true(class(stateMap(example_US_stateObesity, 'obesityRate')) == 'tmap')
  
})



