[![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version/MazamaSpatialPlots)](https://cran.r-project.org/package=MazamaSpatialPlots)
[![Downloads](https://cranlogs.r-pkg.org/badges/MazamaSpatialPlots)](https://cran.r-project.org/package=MazamaSpatialPlots)

A dedicated Slack channel has been created for announcements, support and to help build a community of practice around this open source package. You may request an invitation to join from jonathan.callahan@dri.com.

# MazamaSpatialPlots

```
A suite of convenience functions for generating thematic maps using datasets
from the MazamaSpatialUtils package.
```

## Background

Maps colored by state and county (aka "chloropleth maps") are among the most common
data graphics and are used to convey everything from vote totals to economic and 
health statistics. Well designed maps involve a careful choice of map projections
and colors for both the state/county fill and boundary lines. Unfortunately,
creating beautiful maps in R is still somewhat involved.

The **MazamaSpatialPlots** package provides plotting functionality to make it as
easy as possible to produce beautiful US state and county level maps. It builds
on the excellent **tmap** package and harnesses datasaets from the 
**MazamaSpatialUtils** package. High-level plotting functions make it easy for 
users to create beautiful chloropleth maps. The underlying code uses **ggplot2**
so users familiar with **ggplot2** can easily enhance the returned plot objects 
to create highly customized plots.

----

This project is supported by Mazama Science.

