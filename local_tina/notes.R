# Notes + Questions:

#############################################################

# Do we need to have FIPS as one of the necessary columns? Or would stateName + stateCode suffice?

#############################################################
# State Map --------------------------------------------------

library(MazamaSpatialPlots)
library(MazamaSpatialUtils)

loadSpatialData("USCensusCounties_05") # for it to be faster. will fix later 
loadSpatialData("USCensusStates")

data <- example_US_stateObesity











  
# Missing documentation for MazamaSpatialPlots
# Define county_SPDF and state_SPDF - should they have SPDF from USCensusStates + USCensusCounties?
# Have to merge USCensusStates$FIPS with USCensusCounties$countyFIPS! Should be length 5 but we should decide

# Update: countyFIPS should be 5-digits. In that case, we want to add that together 





# I'd imagine we need to set a list of guidelines 
#   For eg., no extra characters for stateCode (Make sure no quotes are between characters -- CA not "CA")
#   countyFIPS is length 5


# ----------------------------------------------------------------------

# Testing with my California covid county data

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


# This is what we'll probably imagine what they'll submit as part of their dataset
# but let's reassign more columns so that I can left join with a SPDF











#data <- covid19data %>% 
#  select("County.Name", "Most.Recent.Date")

# June 8: Some questions cleared up. I think I'm going to actually edit that data abit so that I can 
# use it to test out countyMap. Or at least to check if the errors then makes sense. 
# data contains 2 columns. Right now, there is only one working column and that is "County.Name" but for 
# our countyMap, we need countyName, stateCode, and countyFIPS.

# Checking...1 Currently it reads: 
# Error: Missing fields in 'data': stateCode, countyName, countyFIPS

# Let's change the column name
colnames(data)[1] <- "countyName"

# Checking...2 Currently it reads: 
# Error: Missing fields in 'data': stateCode, countyFIPS
# Awesome! It works. Now let's add in a column for stateCode
data$stateCode <- "CA"
# Error: Missing fields in 'data': countyFIPS
# Great. It works!
# ----------------------------------------------------------------------

# 'USCensusStates' not found. I suppose I should load 'USCensusStates' beforehand?
loadSpatialData("USCensusStates")
loadSpatialData("USCensusCounties")


# Hmm, trying to figure out whether I should match the vectors from US_52 to those in state_SPDF

all(data$stateCode %in% MazamaSpatialUtils::US_52)




# 0. For more information on R packages, visit here: https://r-pkgs.org/intro.html

# ----------------------------------------------------------------------
# 1. Flesh out roxygen2 documentation 

# The primer is here: https://kbroman.org/pkg_primer/pages/docs.html
# Ahh, so every function needs to have source for the documentation (@)
# It precedes the R-file and it also creates the NAMESPACE for you.
# They're like R comments but is preceded by # but you need #' to distinguish it as Roxygen2
# Instead of \item{}, it has @param

# ----------------------------------------------------------------------
# 2. Clean up the function 

# Going to be based on tmap package. 
#Added "style" to specify breaks 



# create countyMap() function 
#local_jon/JHU_deaths.R




# What is !! 

