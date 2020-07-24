"
Section 1: Get Data from monitor site
"
library(PWFSLSmoke)

stateCodeList <- c('ME', 'NH', 'VT', 'MA', 'RI', 'CT')
#stateNameList <- c('maine', 'new hampshire', 'vermont', 'massachusetts', 'rhode island', 'connecticut')

# Load New England data from the past 10 days
neMonitor <- monitor_loadLatest() %>%
  monitor_subset(stateCodes = stateCodeList)

# Turn into SPDF 
geojson_file <- tempfile(fileext = ".geojson")
ne_current_geojson <- monitor_writeCurrentStatusGeoJSON(neMonitor, geojson_file)
ne_current_list <- jsonlite::fromJSON(ne_current_geojson)
ne_spdf <- rgdal::readOGR(dsn = geojson_file)

# Daily averages
neDailyMonitor <- monitor_dailyStatistic(neMonitor)

# Maximum daily average for each monitor
monitorMaxDayData <- neDailyMonitor$data %>% 
  tidyr::pivot_longer(-datetime, names_to = "monitorID", values_to = "value") %>%
  dplyr::group_by(monitorID) %>% 
  dplyr::summarise(monitorPMValue = max(value, na.rm=T)) %>%
  dplyr::left_join(neDailyMonitor$meta, by = 'monitorID') %>%
  dplyr::select(c('monitorID','stateCode','latitude','longitude', 'monitorPMValue'))

# Average PM_2.5 value by state
stateAverage <- monitorMaxDayData %>%
  dplyr::group_by(stateCode) %>%
  dplyr::summarise(statePMValue = mean(monitorPMValue, na.rm=T))

ne_spdf@data <- ne_spdf@data %>%
  dplyr::left_join(
    stateAverage[, c("stateCode", 'statePMValue')],
    by = "stateCode"
  ) %>%
  dplyr::left_join(
    monitorMaxDayData[, c("monitorID", "monitorPMValue")], 
    by = "monitorID"
  ) %>%
  dplyr::select(c("monitorID", "longitude", "latitude", "statePMValue", "monitorPMValue"))

"
Section 2: Create map
"

# Create map of average PM_2.5 value by state
stateMap(
  data = stateAverage,
  parameter = "statePMValue", # color states by average PM Value
  stateCode = stateCodeList,
  stateBorderColor = 'white', 
  breaks = seq(0,20,2)
) +
  tmap::tm_shape(ne_spdf) +  # plot points of each monitor location with color 
  tmap::tm_bubbles(          # and size based on PM value reading of the monitor
    size = "monitorPMValue",
    col = "monitorPMValue", 
    border.col = 'black',
    scale = .8,
    legend.size.show = F,
    legend.col.show = F) +
  tmap::tm_layout(
    main.title = 'Air Quality in New England',
    title.fontface = 2,
    fontfamily = "serif",
    bg.color = "lightblue3",
  ) 



# ---------------- random workspace ---------------- 
monitor_dailyBarplot(monitor_loadLatest(), '000106701_01')
x <- monitor_dailyStatistic(monitor)

monitor_map(monitor_loadLatest())

# stateAverage <- MazamaSpatialUtils::summarizeByPolygon(
#   monitorMaxDay$longitude,
#   monitorMaxDay$latitude,
#   monitorMaxDay$PMValue,
#   SPDF = subset(USCensusStates_02, stateCode %in% stateCodeList), 
#   FUN = mean
#   )

# ---------------- random workspace ---------------- 
