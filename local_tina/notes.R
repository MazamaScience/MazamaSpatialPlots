# Notes + Questions:
  
  #Missing documentation for MazamaSpatialPlots




# Test (?):
# Not sure if this is right... 
dataDir <- ("~/Data/Spatial")
url <- 'https://data.edd.ca.gov/api/geospatial/grn2-ffzq?method=export&format=Shapefile'



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

