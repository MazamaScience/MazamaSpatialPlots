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

# NOTE:  See "DANG!" comment below
###covid19data <- read.csv(filePath, header = TRUE) 
covid19data <- readr::read_csv(filePath) 

################################################################################
# Jon's experiments
#

# > dplyr::glimpse(covid19data)
# Observations: 4,011
# Variables: 8
# $ County.Name                          <fct> Los Angeles, San Bernardino, Orange, Rivers…
# $ Most.Recent.Date                     <fct> 4/1/2020, 4/1/2020, 4/1/2020, 4/1/2020, 4/1…
# $ Total.Count.Confirmed                <int> 3502, 245, 579, 306, 299, 851, 154, 805, 28…
# $ Total.Count.Deaths                   <int> 66, 5, 11, 11, 8, 11, 5, 33, 8, 0, 3, 8, 13…
# $ COVID.19.Positive.Patients           <int> 739, 95, 117, 85, 53, 168, 22, 160, 52, 19,…
# $ Suspected.COVID.19.Positive.Patients <int> 1332, 196, 221, 182, 138, 340, 96, 85, 76, …
# $ ICU.COVID.19.Positive.Patients       <int> 335, 39, 50, 29, 20, 71, 5, 77, 27, 5, 12, …
# $ ICU.COVID.19.Suspected.Patients      <int> 220, 52, 48, 47, 33, 32, 23, 15, 15, 13, 12…

# Recipe to make 'data'
#  * take 'covid19data'
#  * filter for June 1, 2020
#  * create desired columns
#  * select only desired columns

# NOTE:  Names with spaces are retained after switching to readr::read_csv()

data <- 
  covid19data %>% 
  dplyr::filter(`Most Recent Date` == "06/01/2020") %>%
  # NOTE:  See: WHOA! below
  dplyr::filter(`County Name` != "Unassigned") %>%
  dplyr::mutate(
    countryCode = "US",
    stateCode = "CA",
    countyName = `County Name`
  ) %>%
  select("countryCode", "stateCode", "countyName")
  
# > dplyr::glimpse(data)
# Observations: 59
# Variables: 3
# $ countryCode <chr> "US", "US", "US", "US", "US", "US", "US", "US", "US", "US", "US", "U…
# $ stateCode   <chr> "CA", "CA", "CA", "CA", "CA", "CA", "CA", "CA", "CA", "CA", "CA", "C…
# $ countyName  <fct> Alameda, Alpine, Amador, Butte, Calaveras, Colusa, Contra Costa, Del…

# Do the county names match up?

ca <- 
  USCensusCounties_05@data %>%
  dplyr::filter(stateCode == "CA") %>%
  select("countryCode", "stateCode", "countyName", "countyFIPS")

# > dplyr::glimpse(ca)
# Observations: 58
# Variables: 4
# $ countryCode <chr> "US", "US", "US", "US", "US", "US", "US", "US", "US", "US",…
# $ stateCode   <chr> "CA", "CA", "CA", "CA", "CA", "CA", "CA", "CA", "CA", "CA",…
# $ countyName  <chr> "Alameda", "Placer", "Los Angeles", "Solano", "Siskiyou", "…
# $ countyFIPS  <chr> "06001", "06061", "06037", "06095", "06093", "06111", "0611…

# NOTE:  WHOA! was before I fixed things above

# WHOA! Why does 'data' have 59 rows and 'ca' only has 58?

# > setdiff(data$countyName, ca$countyName)
# [1] "Unassigned"

# OK, better filter that out up above

# After fixing:

# > setdiff(data$countyName, ca$countyName)
# character(0)

# Excellent. All county names match. Now lets create 'data$countyFIPS'

# > data <-
#   +   left_join(data, ca, by = c("countryCode", "stateCode", "countyName"))
# Warning message:
#   Column `countyName` joining factor and character vector, coercing into character vector 

# DANG! Stupid factors!!!

# NOTE TO TINA:  Never use 'read.csv()'. Always use 'readr::read_csv()'

# After fixing:

data <-
  left_join(data, ca, by = c("countryCode", "stateCode", "countyName"))

# > head(data)
# # A tibble: 6 x 4
# countryCode stateCode countyName countyFIPS
#   <chr>       <chr>     <chr>      <chr>     
# 1 US          CA        Alameda    06001     
# 2 US          CA        Alpine     06003     
# 3 US          CA        Amador     06005     
# 4 US          CA        Butte      06007     
# 5 US          CA        Calaveras  06009     
# 6 US          CA        Colusa     06011     


#
# END Jon's experiments
################################################################################



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

