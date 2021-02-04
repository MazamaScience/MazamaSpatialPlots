# -----------------------------------------------------------------------------
testthat::context("countyMap()")

testthat::test_that("make sure data exists", {
  
  testthat::expect_true(exists("USCensusStates_02"))
  testthat::expect_true(exists("USCensusCounties_02"))
  testthat::expect_true(exists("example_US_countyCovid"))

})

testthat::test_that("handles errors correctly", {
  
  testthat::expect_error(countyMap())
  testthat::expect_error(countyMap(example_US_countyCovid))
  testthat::expect_error(countyMap(example_US_countyCovid, 'cases', county_SPDF = 'wrongSPDF'),
                         "County dataset 'wrongSPDF' is not loaded.\n  Please load it with MazamaSpatialtUtils::loadSpatialData()"
                         )
  testthat::expect_error(countyMap(example_US_countyCovid, 'cases', state_SPDF = 'wrongSPDF'),
                         "State dataset 'wrongSPDF' is not loaded.\n  Please load it with MazamaSpatialtUtils::loadSpatialData()"
  )
  
  testthat::expect_error(countyMap(example_US_countyCovid, 'wrongParameter'),
                         "'wrongParameter' is not found in the incoming 'data' dataframe."
                         )
  testthat::expect_error(countyMap(example_US_countyCovid[, c("countyName", "cases")], 'cases'),
                         "Missing fields in 'data': countyFIPS")
  
  #if we add error message in function under parameter validation
  testthat::expect_error(countyMap(example_US_countyCovid, 'cases', county_SPDF = USCensusCounties_02[, c("countryCode", "countyName")]),
                         "Missing fields in 'county_SPDF': countyFIPS")
  
  testthat::expect_error(countyMap(example_US_countyCovid, 'cases', breaks=c('1', '2', '3')),
                         "Parameter 'breaks' must be a numeric vector."
                         )
  testthat::expect_error(countyMap(example_US_countyCovid, 'cases', breaks=c(1, 2, 'a')),
                         "Parameter 'breaks' must be a numeric vector."
                         )
  testthat::expect_error(countyMap(example_US_countyCovid, 'cases', stateCode = c(1, 2, 3)),
                         "Invalid state codes found:  1, 2, 3"
                         )
  testthat::expect_error(countyMap(example_US_countyCovid, 'cases', stateCode = c('ME', 'MM')),
                         "Invalid state codes found:  MM"
                         )
  testthat::expect_error(countyMap(example_US_countyCovid, 'cases', stateCode = 3),
                         "Invalid state codes found:  3"
                         )
  testthat::expect_error(countyMap(example_US_countyCovid, 'cases', projection = 2),
                         "Parameter 'projection' must be a sp::CRS object or a valid projection string."
                         )
  testthat::expect_error(countyMap(example_US_countyCovid, 'cases', projection = '+bad projection'))
  
  
})

testthat::test_that("subsets by stateCode correctly", {
  
  stateCodeList <- c('WA', 'OR')
  countyList <- USCensusCounties_02@data[USCensusCounties_02@data$stateCode %in% stateCodeList,]
  
  plottedStates <- countyMap(example_US_countyCovid, 'cases', stateCode = stateCodeList)$tm_shape$shp@data$stateCode
  
  testthat::expect_equal(length(plottedStates), nrow(countyList))
  testthat::expect_true('WA' %in% plottedStates)
  testthat::expect_true('OR' %in% plottedStates)
  
})

testthat::test_that("returns correct object type", {
  
  testthat::expect_true(class(countyMap(example_US_countyCovid, 'cases')) == 'tmap')
  
})



