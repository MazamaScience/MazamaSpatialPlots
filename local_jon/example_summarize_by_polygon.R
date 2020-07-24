# Summarize by polygon example

library(dplyr)
library(MazamaSpatialUtils)

setSpatialDataDir("~/Data/Spatial")

# Load recent monitoring data
latestData <- monitor_loadLatest()

# Eastern timezone monitors because dailyStatistic() uses local time
EST_monitors <- 
  latestData %>%
  monitor_subsetBy(timezone == "America/New_York")

# Maximum daily average dataframe -- one record per monitor
EST_dailyMax <-
  EST_monitors %>%
  monitor_dailyStatistic() %>%
  monitor_extractData() %>% 
  tidyr::pivot_longer(-datetime, names_to = "monitorID", values_to = "value") %>%
  dplyr::group_by(monitorID) %>% 
  dplyr::summarise(pm25 = max(value, na.rm = TRUE)) %>%
  dplyr::left_join(EST_monitors$meta, by = 'monitorID') %>%
  dplyr::select(c('monitorID', 'stateCode', 'latitude', 'longitude', 'pm25'))

# Load a new SPDF
loadSpatialData("USCensusCBSA_02")

# Subset to relevant states
stateCodes <- sort(unique(EST_dailyMax$stateCode))
EST_CBSA <- subset(USCensusCBSA_02, stateCode %in% stateCodes)

# Validation plot
plot(EST_CBSA)

# Summarize by CBSA
df <- MazamaSpatialUtils::summarizeByPolygon(
  longitude = EST_dailyMax$longitude,
  latitude = EST_dailyMax$latitude,
  value = EST_dailyMax$pm25,
  SPDF = EST_CBSA,
  FUN = mean
)

# What's inside?
head(df)

# Add this to EST_CBSA
EST_CBSA@data$pm25_dailyMax <- df$summaryValue

# Plot
tmap::tm_shape(EST_CBSA) +
  tmap::tm_fill(
    col = "pm25_dailyMax"
  )


