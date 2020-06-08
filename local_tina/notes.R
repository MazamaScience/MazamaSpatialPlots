# Notes + Questions:
  
# Missing documentation for MazamaSpatialPlots
# Define county_SPDF and state_SPDF
# For stateCode, we need to make sure it's going to follow all the same 
# guidelines as what we have for our stateCodes. 
# For countyFIPS, same thing as above but county.

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
covid19data <- read.csv(filePath, header = TRUE) 
data <- covid19data %>% 
  select("County.Name", "Most.Recent.Date")

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

