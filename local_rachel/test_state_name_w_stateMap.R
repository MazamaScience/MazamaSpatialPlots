# ----------- Test with dataframe with "state name" variable ----------- 

# load a dataframe
URL <- "https://www.patriotsoftware.com/blog/accounting/average-cost-living-by-state/"
livingCostData <- MazamaCoreUtils::html_getTable(URL)

# prepare dataframe with "state name" but not "stateCode" variable
livingCostData <- livingCostData %>%
  dplyr::mutate(
    'state name' = .data$"State",
    avgAnnualWage = as.numeric(gsub('[$,]', '', .data$"Annual Mean Wage (All Occupations)")),
    avgMonthlyRent = as.numeric(gsub('[$,]', '', .data$"Median Monthly Rent")),
    rentWagePercent = 100*12*avgMonthlyRent/avgAnnualWage,
    .keep = "none"
  ) 

# check to make sure "state name" is included but not "stateCode" 
head(livingCostData)

# make sure stateMap works
stateMap(
  data = livingCostData,
  state_SPDF = USCensusStates_02,
  parameter = 'rentWagePercent',
)

# make sure error is there without "state name" or "stateCode"
stateMap(
  data = livingCostData[, -which(names(livingCostData) %in% "state name")],
  state_SPDF = USCensusStates_02,
  parameter = 'rentWagePercent'
)

#  ----------- Test with SPDF with "state name" variable ----------- 

# load SPDF
SPDF <- USCensusStates_02

# prepare SPDF data with "state name" but not "stateCode" variable
SPDF@data <- SPDF@data %>%
  dplyr::mutate('state name' = .data$stateName, .keep = "unused") %>%
  dplyr::select(c(-stateCode))

# check to make sure "state name" is included but not "stateCode"
head(SPDF@data)

# make sure stateMap works
stateMap(
  data = livingCostData,
  state_SPDF = SPDF,
  parameter = 'rentWagePercent'
)

# make sure error is there without "state name" or "stateCode"
stateMap(
  data = livingCostData,
  state_SPDF = subset(
    SPDF, 
    select = -which(names(SPDF@data) %in% "state name")
    ),
  parameter = 'rentWagePercent'
)

