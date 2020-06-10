# Get obesityByState dataframe from:
#   https://catalog.data.gov/dataset/national-obesity-by-state-b181b/resource/69b14268-6189-4639-bd9a-61d459006393
# 

library(dplyr)

library(MazamaSpatialPlots)

# Let's explore this dataset

fileUrl <- "http://data-lakecountyil.opendata.arcgis.com/datasets/3e0c1eb04e5c48b3be9040b0589d3ccf_8.csv"

obesity_DF <- readr::read_csv(fileUrl)

# > head(obesity_DF)
# # A tibble: 6 x 5
# FID NAME       Obesity SHAPE_Length SHAPE_Area
# <dbl> <chr>        <dbl>        <dbl>      <dbl>
# 1     1 Texas         32.4         45.0       65.8
# 2     2 California    24.2         40.8       41.7
# 3     3 Kentucky      34.6         20.3       10.7
# 4     4 Georgia       30.7         17.3       14.7
# 5     5 Wisconsin     30.7         22.4       16.5
# 6     6 Oregon        30.1         24.6       28.2

# Now read up on read_csv with
# ?readr::read_csv

# Now do it the right way.

col_names <- c("FID", "stateName", "obesityRate", "SHAPE_Length", "SHAPE_Area")
col_types = "icddd"

outputColumns <- c("countryCode", "stateCode", "stateFIPS", "stateName", 
                   "obesityRate")

# A couple of iterations and I end up with this:

obesity_DF <-
  readr::read_csv(
    file = fileUrl,
    skip = 1,                    # Skip the header line
    col_names = col_names,
    col_types = col_types
  ) %>%
  dplyr::mutate(
    countryCode = "US",
    stateCode = MazamaSpatialUtils::US_stateNameToCode(stateName),
    stateFIPS = MazamaSpatialUtils::US_stateNameToFIPS(stateName),
    stateName = stateName
  ) %>%
  dplyr::select(!!outputColumns)

# > head(obesity_DF)
# # A tibble: 6 x 5
# countryCode stateCode stateFIPS stateName  obesityRate
#   <chr>       <chr>     <chr>     <chr>            <dbl>
# 1 US          TX        48        Texas             32.4
# 2 US          CA        06        California        24.2
# 3 US          KY        21        Kentucky          34.6
# 4 US          GA        13        Georgia           30.7
# 5 US          WI        55        Wisconsin         30.7
# 6 US          OR        41        Oregon            30.1


# Exellent! Now we can use this as the 'data' parameter:

# MazamaSpatialPlots::stateMap(
#   data = obesity_DF,
#   conusOnly = TRUE
# )


