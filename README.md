![Status](https://img.shields.io/badge/Status-Inactively%20maintained-yellowgreen.svg?style=flat-square)

# fruitr

An [R](https://www.r-project.org/) package evolving with the needs and desires of [Falling Fruit](https://github.com/falling-fruit).

The main functions are described in their own tutorials:

- Formatting location data for import: [vignettes/imports.md](vignettes/imports.md)
- Fast location clustering: [vignettes/clusters.md](vignettes/clusters.md)
- Compiling common name translations: coming soon!

## Status

This package is occasionally used in production for quickly rebuilding the `clusters` database table and for preparing municipal tree inventories for import.

## Installation

The package is not on CRAN. To install in R, use [devtools](https://github.com/hadley/devtools):

```R
install.packages("devtools")
devtools::install_github("falling-fruit/fruitr")
```

The package [`rgdal`](https://cran.r-project.org/web/packages/rgdal/index.html) is not a required dependency, but is needed by `fruitr::read_locations()` for reading [OGR vector formats](http://www.gdal.org/ogr_formats.html).
