library(dplyr)
library(MazamaSpatialPlots)
library(readr)

# COVID data by US county
URL1 <- "https://raw.githubusercontent.com/OpportunityInsights/EconomicTracker/main/data/COVID%20-%20County%20-%20Daily.csv.gz"
URL2 <- "https://raw.githubusercontent.com/OpportunityInsights/EconomicTracker/main/data/Zearn%20-%20County%20-%20Weekly.csv"
countyCovid <- readr::read_csv(URL1, col_types = cols())
countyMath <- readr::read_csv(URL2, col_types = cols())

# head(countyCovid)
# head(countyMath)

mapDate <- lubridate::parse_date_time("2021 09 14", "y m d", tz = "UTC")

# column 'new_case_rate' is the confirmed covid cases per 100k residents, 7-day rolling average
countyCovid_formatted <- countyCovid %>%
  mutate(
    date = lubridate::parse_date_time(paste(year, month, day), "y m d", tz = "UTC"),
    countyFIPS = stringr::str_pad(countyfips, width = 5, pad = "0"),
    stateCode = US_stateFIPSToCode(substr(countyFIPS, 0, 2)),
    new_case_rate = as.numeric(new_case_rate)) %>%
  filter(date == mapDate) %>%
  group_by(countyFIPS) %>%
  mutate(meanCaseRate = mean(new_case_rate, na.rm = TRUE)) %>%
  select(countyFIPS, stateCode, meanCaseRate)

# ----- Basic map --------------------------------------------------------------

countyMap(
  data = countyCovid_formatted,
  parameter = "meanCaseRate",
  stateCode = "WA",
  breaks = seq(0, 200, 40),
  palette = "YlOrBr",
  style = "cont", # continuous color scale
  legendOrientation = "horizontal",
  legendTitle = "",
  stateBorderColor = "white"
)

# ----- Fancy map --------------------------------------------------------------

countyMap(
  data = countyCovid_formatted,
  parameter = "meanCaseRate",
  state_SPDF = "USCensusStates_05",
  county_SPDF = "USCensusCounties_05",
  stateCode = "WA",
  breaks = c(seq(0, 100, 10), 150, 200),
  palette = "YlOrBr",
  style = "cont",
  legendOrientation = "horizontal",
  legendTitle = "Avg daily cases per 100K",
  stateBorderColor = "white",
  countyBorderColor = "gray95",
  title = "Washington COVID Hot Spots 2021-09-14"
) + tmap::tm_layout(
    legend.position = c("center", "bottom"),
    legend.outside = TRUE,
    legend.outside.position = "bottom",
    legend.outside.size = 0.15,
    main.title.size = 2
)


