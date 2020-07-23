
"
Section 1: Get Data from monitor site
"
library(PWFSLSmoke)

stateCodeList <- c('ME', 'NH', 'VT', 'MA', 'RI', 'CT')

# Load New England data from the past 10 days
neMonitor <- monitor_loadLatest() %>%
  monitor_subset(stateCodes = stateCodeList)

# Daily averages (can prob skip this and use neMonitor$data directly in next step)
neDailyMonitor <- monitor_dailyStatistic(neMonitor)

# Turn into SPDF
geojson_file <- tempfile(fileext = ".geojson")
ne_current_geojson <- monitor_writeCurrentStatusGeoJSON(neMonitor, geojson_file)
ne_current_list <- jsonlite::fromJSON(ne_current_geojson)
ne_spdf <- rgdal::readOGR(dsn = geojson_file)


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
  dplyr::summarise(PMValue = mean(monitorPMValue, na.rm=T))
  
# stateAverage <- MazamaSpatialUtils::summarizeByPolygon(
#   monitorMaxDay$longitude,
#   monitorMaxDay$latitude,
#   monitorMaxDay$PMValue,
#   SPDF = subset(USCensusStates_02, stateCode %in% stateCodeList), 
#   FUN = mean
#   )

monitorMaxDaySF <- st_as_sf(monitorMaxDay, coords = c("longitude", "latitude"), crs = 4326)
tm_shape(tif_sf) + tm_dots() 

# ---------------- random workspace ---------------- 
monitor_dailyBarplot(monitor_loadLatest(), '000106701_01')
x <- monitor_dailyStatistic(monitor)

monitor_map(monitor_loadLatest())

# ---------------- random workspace ---------------- 




"
Section 2: Create map
"
# Create map of average PM_2.5 value by state
x <- stateMap(
  data = stateAverage,
  parameter = "PMValue",
  stateCode = stateCodeList,
  stateBorderColor = 'black'
) +
  tmap::tm_layout(
    main.title = 'Air Quality in New England',
    title.size = 1.2,
    title.fontface = 2,
    fontfamily = "serif",
    bg.color = "lightblue3",
    inner.margins  = .08
  )



"
Section 3: sprinkle points on the map
"


# sprinkle points so show where monitors are located

# color points by PM_2.5 value
