# ----------- Test with dataframe with "stateName" variable -----------

# load a dataframe
URL <- "https://www.patriotsoftware.com/blog/accounting/average-cost-living-by-state/"
livingCostData <- MazamaCoreUtils::html_getTable(URL, header = TRUE)

# prepare dataframe with "stateName" but not "stateCode" variable
livingCostData <- livingCostData %>%
  dplyr::mutate(
    'stateName' = .data$"State",
    avgAnnualWage = as.numeric(gsub('[$,]', '', .data$"Annual Mean Wage (All Occupations)")),
    avgMonthlyRent = as.numeric(gsub('[$,]', '', .data$"Median Monthly Rent")),
    rentWagePercent = 100*12*avgMonthlyRent/avgAnnualWage,
    .keep = "none"
  ) %>%
  dplyr::mutate(
    stateCode = MazamaSpatialUtils::US_stateNameToCode(.data$stateName)
  )

# check to make sure "stateName" is included but not "stateCode"
head(livingCostData)

# make sure stateMap works
stateMap(
  data = livingCostData,
  state_SPDF = USCensusStates_02,
  parameter = 'rentWagePercent',
)

# make sure error is there without "stateName" or "stateCode"
stateMap(
  data = livingCostData[, -which(names(livingCostData) %in% "stateName")],
  state_SPDF = USCensusStates_02,
  parameter = 'rentWagePercent'
)

#  ----------- Test with SPDF with "stateName" variable -----------

# load SPDF
SPDF <- USCensusStates_02

# prepare SPDF data with "stateName" but not "stateCode" variable
SPDF@data <- SPDF@data %>%
  dplyr::mutate('stateName' = .data$stateName, .keep = "unused") %>%
  dplyr::select(c(-stateCode))

# check to make sure "stateName" is included but not "stateCode"
head(SPDF@data)

# make sure stateMap works
stateMap(
  data = livingCostData,
  state_SPDF = SPDF,
  parameter = 'rentWagePercent'
)

# make sure error is there without "stateName" or "stateCode"
stateMap(
  data = livingCostData,
  state_SPDF = subset(
    SPDF,
    select = -which(names(SPDF@data) %in% "stateName")
    ),
  parameter = 'rentWagePercent'
)

