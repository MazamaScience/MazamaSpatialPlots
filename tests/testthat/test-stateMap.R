# -----------------------------------------------------------------------------
testthat::context("stateMap()")

testthat::test_that("make sure data exists", {

  testthat::expect_true(exists("USCensusStates_02"))
  testthat::expect_true(exists("example_US_stateObesity"))

})

testthat::test_that("handles errors correctly", {

  testthat::expect_error(stateMap())
  testthat::expect_error(stateMap(example_US_stateObesity))
  testthat::expect_error(stateMap(example_US_stateObesity, 'obesityRate', state_SFDF = 'wrongSFDF'),
                         "State dataset 'wrongSFDF' is not loaded.\n  Please load it with MazamaSpatialtUtils::loadSpatialData()"
                         )
  testthat::expect_error(stateMap(example_US_stateObesity, 'wrongParameter'),
                         "'wrongParameter' is not found in the incoming 'data' dataframe."
                         )
  testthat::expect_error(stateMap(example_US_stateObesity[, c("stateName", "obesityRate")], 'obesityRate'),
                         "Missing fields in 'data': stateCode")

  #if we add error message in function under parameter validation
  testthat::expect_error(stateMap(example_US_stateObesity, 'obesityRate', state_SFDF = USCensusStates_02[, c("countryCode", "stateFIPS")]),
                         "Missing fields in 'state_SFDF': stateCode")

  testthat::expect_error(stateMap(example_US_stateObesity, 'obesityRate', breaks=c('1', '2', '3')),
                         "Parameter 'breaks' must be a numeric vector."
                         )
  testthat::expect_error(stateMap(example_US_stateObesity, 'obesityRate', breaks=c(1, 2, 'a')),
                         "Parameter 'breaks' must be a numeric vector."
                         )
  testthat::expect_error(stateMap(example_US_stateObesity, 'obesityRate', stateCode = c(1, 2, 3)),
                         "Invalid state codes found:  1, 2, 3"
                         )
  testthat::expect_error(stateMap(example_US_stateObesity, 'obesityRate', stateCode = c('ME', 'MM')),
                         "Invalid state codes found:  MM"
                         )
  testthat::expect_error(stateMap(example_US_stateObesity, 'obesityRate', stateCode = 3),
                         "Invalid state codes found:  3"
                         )
  testthat::expect_error(stateMap(example_US_stateObesity, 'obesityRate', projection = 2),
                         "Parameter 'projection' must be a sf::st_crs object or a valid projection string."
                         )
  testthat::expect_error(stateMap(example_US_stateObesity, 'obesityRate', projection = '+bad projection'))

  testthat::expect_error(stateMap(example_US_countyCovid, parameter = c('cases', 'deaths'), title = "Bad title"),
                         "The lengths of 'parameter' and 'title' must be equal."
                         )

})

testthat::test_that("subsets by stateCode correctly", {

  stateCodeList <- c('WA', 'OR')
  plottedStates <- stateMap(example_US_stateObesity, 'obesityRate', stateCode = stateCodeList)$tm_shape$shp$stateCode

  testthat::expect_equal(length(plottedStates), length(stateCodeList))
  testthat::expect_true('WA' %in% plottedStates)
  testthat::expect_true('OR' %in% plottedStates)

})

testthat::test_that("returns correct object type", {

  testthat::expect_true(class(stateMap(example_US_stateObesity, 'obesityRate')) == 'tmap')

})



