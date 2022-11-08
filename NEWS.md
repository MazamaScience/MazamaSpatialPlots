# MazamaSpatialUtils 0.2.0

Version 0.2.x is a refactoring to accommodate **MazamaSpatialUtils** 0.8 which
is based on the **sf** package rather than **sp**. As much as
possible, the suite of functions and arguments will remain the same.

# MazamaSpatialPlots 0.1.2

* Putting `countyMap()` example inside `\donttest{}` to pass CRAN checks.

# MazamaSpatialPlots 0.1.1

* Added "Opportunity Insights" article.

# MazamaSpatialPlots 0.1.0

First user-ready release of the package. While still in development, enough 
functionality exists for outside users to start using the `stateMap()` and
`countyMap()` functions.

* Some refactoring of information in various articles.
* Added unit testing for `countyMap()` function.
* Now requiring **MazamaSpatialUtils** (>= 0.7).
* Added "Creating County Maps" article.

# MazamaSpatialPlots 0.0.10

* Removed dependency on **stringr**.

# MazamaSpatialPlots 0.0.9

* Added dependency on `MazamaCoreUtils (>= 0.4.5)`.

# MazamaSpatialPlots 0.0.8

* Added "Working with Locations" article.

# MazamaSpatialPlots 0.0.7

* Restored improved `countyMap()` function.

# MazamaSpatialPlots 0.0.6

* Added "Creating State Maps" article.
* Added `stateMap()` support for datasets with "State Name" columns.

# MazamaSpatialPlots 0.0.5

* Added `countyMap()` with basic functionality.

# MazamaSpatialPlots 0.0.4

* Added `stateMap()` with basic functionality.
* Removed prototype `countyMap()`.

# MazamaSpatialPlots 0.0.3

* Added `example_US_stateObesity` dataset.
* Added `example_US_countyCovid` dataset.

# MazamaSpatialPlots 0.0.2

* Minor code style changes to `countyMap.R`.
* Added **MazamaSpatialUtils** to the `Depends:` section of the description file.
As an extension to **MazamaSpatialUtils**, it makes sense that we should import
and reexport the entire namespace from **MazamaSpatialutils**.

# MazamaSpatialPlots 0.0.1

* Initial Release

