# Example US County

library(MazamaSpatialPlots)
library(MazamaSpatialUtils)

loadSpatialData("USCensusCounties")
loadSpatialData("USCensusStates")
dataDir <- ("~/Data/Spatial")

url <- 'https://data.chhs.ca.gov/dataset/6882c390-b2d7-4b9a-aefa-2068cee63e47/resource/6cd8d424-dfaa-4bdd-9410-a3d656e1176e/download/covid19data.csv'
filePath <- file.path(dataDir,basename(url))
utils::download.file(url,filePath) #cool, this is just a csv file 
#covid19data <- read.csv(filePath, header = TRUE) # don't use read.csv!!!! 
covid19data <- readr::read_csv(filePath) 



# The input dataset will require: stateCode, countyName, countyFIPS

dplyr::glimpse(covid19data) #this is nicer than using head for some reason

data <- covid19data %>% 
  dplyr::filter(`Most Recent Date` == "06/07/2020") %>%
  # NOTE:  See: WHOA! below
  dplyr::filter(`County Name` != "Unassigned") %>%
  dplyr::mutate(
    countryCode = "US",
    stateCode = "CA",
    countyName = `County Name`
  ) %>%
  select("countryCode", "stateCode", "countyName")

dplyr::glimpse(data) 

ca <- USCensusCounties_05@data %>%
  dplyr::filter(stateCode == "CA") %>%
  select("countryCode", "stateCode", "countyName", "countyFIPS")


data <- left_join(data, ca, by = c("countryCode", "stateCode", "countyName"))


# This is how I'd imagine someone submitting a dataset, but with all the data along with the 
# required fields. 

# Next is to merge with a SPDF? Or at least be able to call from somewhere else to make
# initialization a lot easier. 
