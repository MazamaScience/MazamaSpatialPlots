'"""
Please create the following "article" -- vignettes/articles/Working_with_Locations.Rmd.

This article is more of an internal Mazama Science example for how to include 
monitoring data on top of a plot created with stateMap().

Working on this article may lead you to suggest additional utility functions to 
make it easy to convert monitor data into data frames or other data types that 
play nice with tmap functions.

"""'

library(dplyr)
library(MazamaSpatialUtils)
library(PWFSLSmoke)

# ----------------  Prepare monitoring data

# 1) First load data, there are several options for loading monitor data 

#monitorData <- monitor_load(20170601, 20171001) # from a given time range
#monitor2018Data <- monitor_loadAnnual(2018) # from a given year
#latestDailyData <- monitor_loadDaily() # previous 45 days of daily data (up to the most recent full day)
monitorLatestData <- monitor_loadLatest() # past 10 days of data (up to the current hour) 

# 2) Prepare Data: Subset and summarize based on desired information. 
# output is a dataframe of monitor level data. data is aggregated over time so there is 
# one measurment for each monitor. This works well fr spatial plots but doesnt allow 
# for time series. Maybe future update filter at a point in time so that can be changed dynamically.



# Maximum daily average dataframe -- one record per monitor
dailyMax <-
  monitorLatestData %>%
  monitor_dailyStatistic(minHours = 2) %>% # only need two daily measurement to be included
  monitor_extractData() %>% 
  tidyr::pivot_longer(-datetime, names_to = "monitorID", values_to = "value") %>%
  dplyr::group_by(monitorID) %>% 
  dplyr::summarise(pm25 = max(value, na.rm = TRUE)) %>%
  dplyr::left_join(monitorLatestData$meta, by = 'monitorID') %>%
  dplyr::select(
    c('monitorID', 'stateCode', 'latitude', 'longitude', 'pwfslDataIngestSource', 'pm25'))

# 3) Summarize by polygon

# 4) create SPDF of spatial points:
# This step is necessary if you want to add spatial points to a thematic map in tmap
# RC: come back to this, maybe set the monitor object data. is there a good way to go from 
# dataframe with lat long to spdf? instead of using the monitor object


# ----------------  Plot monitor locations 
# example plot with state boundaries and locations (no colors yet)

# get statecodes and only include continental US states
stateCodeList <- unique(monitorLatestData$meta$stateCode) 
stateCodeList <- subset(
  stateCodeList, stateCodeList %in% MazamaSpatialUtils::CONUS
  )

# method 1 using a state level SPDF and then overlay points from monitor metadata (lat and long)
# NOTE: can use dailyMax or monitorLatestData$meta since both have metadata 
stateSPDF <- subset(USCensusStates_02, stateCode %in% stateCodeList)

plot(stateSPDF)  
points(monitorLatestData$meta$longitude, monitorLatestData$meta$latitude)
points(monitorLatestData$meta$longitude, monitorLatestData$meta$latitude)

# method 2 using monitor_map() from PWFSLSmoke
PWFSLSmoke::monitor_map(monitorLatestData, 
                        countyCol = NA,
                        colors = 'grey50',
                        stateCol = "black",
                        pch = 3,
                        stateLwd = 3)


# ----------------  Plot monitor values -- colored by hourly or daily average values 
#    (plot "airnow" as circles and "arisis|wrcc" as triangles

# turn into SPDF (make function monitor_toSPDF ??)
geojson_file <- tempfile(fileext = ".geojson")
current_geojson <- monitor_writeCurrentStatusGeoJSON(monitorLatestData, geojson_file)
current_list <- jsonlite::fromJSON(current_geojson)
monitorSPDF <- rgdal::readOGR(dsn = geojson_file, verbose = TRUE)

# bring in pm25 field from dailyMax 
monitorSPDF@data <- monitorSPDF@data %>%
  dplyr::left_join(dailyMax[, c("monitorID", "pm25")], by = "monitorID") %>%
  dplyr::mutate(
    plotDataSource = ifelse(
      pwfslDataIngestSource == "AIRNOW", 
      "AIRNOW", 
      "AIRSIS or WRCC")
    ) %>%
  dplyr::select(
    c("monitorID", "stateCode", "longitude", "latitude", "plotDataSource", "pm25")
    )

# all values are between 0 and 50 except 2 (outliers?)
customBreaks <- c(seq(0, 50, 5), 1000)

#createMap
tmap::tm_shape(stateSPDF, 
               bbox = bbox(monitorSPDF) #use monitor bbox 
               ) +
  tmap::tm_polygons(
    col = "grey85",
    border.col = "black") +
  tmap::tm_shape(monitorSPDF) +  # plot points of each monitor location with color 
  tmap::tm_symbols(          # and size based on PM value reading of the monitor
    col = "pm25",
    palette = "Reds",
    breaks = customBreaks,
    shape = "plotDataSource",
    shapes = c(16, 17, 17),
    scale = .4) +
  tmap::tm_layout(
    main.title = "Latest Average Daily Max Monitor Reading",
    main.title.position = c("center", "top"),
    bg.color = "deepskyblue4",
    attr.color = "white",
    legend.position = c("left", "center")
  )

# ----------------  Plot polygon summarized values -- see example in 
#    local_jon/example_summarize_by_polygon.R

# Summarize by State
df <- MazamaSpatialUtils::summarizeByPolygon(
  longitude = dailyMax$longitude,
  latitude = dailyMax$latitude,
  value = dailyMax$pm25,
  SPDF = stateSPDF,
  FUN = mean
)

# add summarized values to stateSPDF@data
stateSPDF@data <- stateSPDF@data %>%
  dplyr::left_join(df, by = "polygonID") %>%
  dplyr::mutate(avgPM25 = summaryValue)

#createMap
tmap::tm_shape(stateSPDF) +
  tmap::tm_polygons(
    col = "avgPM25",
    breaks = c(seq(5, 13, 2), seq(15, 70, 5)),
    border.col = "black") +
  tmap::tm_layout(
    main.title = "Latest Average Daily Max Monitor Reading",
    main.title.position = c("center", "top"),
    bg.color = "deepskyblue4",
    attr.color = "white",
    legend.position = c("right", "bottom")
  )

# 5) Plot stateMap() colored by one thing (obesity? or daily average pm25?) and 
#    a sprinkle of points colored by something else (hourly max pm25?)

#subset SPDFs
selectedStateCodeList <- c('ME', 'NH', 'VT', 'MA', 'RI', 'CT')
selectedStateSPDF <- subset(stateSPDF, stateCode %in% selectedStateCodeList)
selectedMonitorSPDF <- subset(monitorSPDF, stateCode %in% selectedStateCodeList)

# Create map of average PM_2.5 value by state with stateMap
stateMap(
  data = selectedStateSPDF@data,
  parameter = "avgPM25", # color states by average PM Value
  stateCode = selectedStateCodeList,
  stateBorderColor = 'white', 
  breaks = seq(5, 14, 1),
) +
  tmap::tm_shape(selectedMonitorSPDF) +  # plot points of each monitor location with color 
  tmap::tm_symbols(          # and size based on PM value reading of the monitor
    col = "pm25",
    palette = "YlOrBr",
    shape = "plotDataSource",
    shapes = c(21, 24),
    scale = .6,
    legend.col.show = FALSE
  ) +
  tmap::tm_layout(
    main.title = "Latest Average Daily Max Monitor Reading",
    main.title.position = c("center", "top"),
    main.title.size = .9,
    main.title.color = 'white',
    attr.color = 'white',
    bg.color = "deepskyblue4",
    legend.outside = TRUE 
  )
  
# can also do it with just tmap
tmap::tm_shape(selectedStateSPDF) +
  tmap::tm_polygons(
    col = "avgPM25", 
    border.col = "black",
    breaks = seq(5, 14, 1.5),
    legend.show = FALSE) +
  tmap::tm_shape(selectedMonitorSPDF) +  # plot points of each monitor location with color 
  tmap::tm_symbols(          # and size based on PM value reading of the monitor
    col = "pm25",
    palette = "YlOrBr",
    breaks = seq(5, 14, 1),
    shape = "plotDataSource",
    shapes = c(21, 24),
    scale = .6
  ) +
  tmap::tm_layout(
    main.title = "Latest Average Daily Max Monitor Reading",
    main.title.position = c("left", "top"),
    main.title.size = .9,
    bg.color = "deepskyblue4",
    legend.outside = TRUE 
  )


# Now use different size markers since all are AIRNOW in new england
stateMap(
  data = selectedStateSPDF@data,
  parameter = "avgPM25", # color states by average PM Value
  stateCode = selectedStateCodeList,
  stateBorderColor = 'white', 
  breaks = seq(5, 14, 1),
) +
  tmap::tm_shape(selectedMonitorSPDF) +  # plot points of each monitor location with color 
  tmap::tm_bubbles(          # and size based on PM value reading of the monitor
    size = "pm25",
    col = "pm25", 
    border.col = 'black',
    scale = .9,
    legend.size.show = F,
    legend.col.show = F) +
  tmap::tm_layout(
    main.title = 'Air Quality in New England',
    main.title.size = 1.1,
    title.fontface = 2,
    fontfamily = "serif",
    bg.color = "lightblue3",
  ) 

  
  



# ----------- random workspace 

#---

plot(subset(USCensusStates_02, stateCode %in% stateCodeList)) +
  geom_point(dailyMax$longitude, dailyMax$latitude )

ggplot(dailyMax, aes(longitude, latitude)) +
  geom_map()


ggplot(example_US_stateObesity, aes(fill = obesityRate)) +
  geom_map(aes(map_id = stateName), map = map_df) +
  expand_limits(x = map_df$long, y = map_df$lat) +
  scale_fill_gradient(low = "grey95",high = "firebrick3", name="Obesity\nRate") +
  theme_void()  +
  theme(legend.position="none",
        plot.background = element_rect(fill="dodgerblue4")
  ) 


AIRNOW_metaData <- dailyMax[dailyMax$pwfslDataIngestSource == "AIRNOW",]
other_meatData <- dailyMax[dailyMax$pwfslDataIngestSource != "AIRNOW",]

cols <- aqiColors(dailyMax$pm25, bins = AQI$breaks_24, palette = AQI$colors)

plot(subset(USCensusStates_02, stateCode %in% stateCodeList))  
points(AIRNOW_metaData$longitude, AIRNOW_metaData$latitude, pch = 20, col = cols)
points(other_meatData$longitude, other_meatData$latitude, pch = 24, col = cols)



# badmons <- as.vector(dailyMax[dailyMax$pm25==-Inf, "monitorID"])
# 
# badmons <- as.vector(badmons$monitorID)
# 
# t <- dplyr::select(monitorLatestData$data , c(datetime, "840020900040_01", "061072002_01"))
# 
# View(dailyMax$data[,c("datetime", "840020900040_01","061072002_01")])
# 
# chk <- monitor_getDailyMean(monitorLatestData)
