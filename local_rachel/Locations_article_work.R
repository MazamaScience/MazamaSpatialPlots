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
monitorData <- monitor_load(20170601,20171001) # from a given time range
monitor2018Data <- monitor_loadAnnual(2018) # from a given year
latestDailyData <- monitor_loadDaily() # previous 45 days of daily data (up to the most recent full day)
monitorLatestData <- monitor_loadLatest() # past 10 days of data (up to the current hour) 

# 2) Prepare Data: Subset and summarize based on desired information. 
# output is a dataframe of monitor level data. data is aggregated over time so there is 
# one measurment for each monitor. This works well fr spatial plots but doesnt allow 
# for time series. Maybe future update filter at a point in time so that can be changed dynamically.

# Eastern timezone monitors because dailyStatistic() uses local time
EST_monitors <- 
  monitorLatestData %>%
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

# 3) Summarize by polygon

# 4) create SPDF of spatial points:
# This step is necessary if you want to add spatial points to a thematic map in tmap
# RC: come back to this, maybe set the monitor object data. is there a good way to go from 
# dataframe with lat long to spdf? instead of using the monitor object


# ----------------  Plot monitor locations 
# example plot with state boundaries and locations (no colors yet)

stateCodeList <- unique(EST_monitors$meta$stateCode)

# method 1 using monitor_map() from PWFSLSmoke 
PWFSLSmoke::monitor_map(EST_monitors, 
                        countyCol = NA,
                        colors = 'grey50',
                        stateCol = "black",
                        pch = 3,
                        stateLwd = 2)

# method 2 using a state level SPDF and then overlay points from monitor metadata (lat and long)
plot(subset(USCensusStates_02, stateCode %in% stateCodeList))
points(EST_monitors$meta$longitude, EST_monitors$meta$latitude)


# ----------------  Plot monitor values -- colored by hourly or daily average values 
#    (plot "airnow" as circles and "arisis|wrcc" as triangles



                                                               



# ----------------  Plot polygon summarized values -- see example in 
#    local_jon/example_summarize_by_polygon.R




                                                                  
# 5) Plot stateMap() colored by one thing (obesity? or daily average pm25?) and 
#    a sprinkle of points colored by something else (hourly max pm25?)