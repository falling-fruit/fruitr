% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/clusters.R
\name{expand_geohashes}
\alias{expand_geohashes}
\title{Expand Geohashes}
\usage{
expand_geohashes(geohashes)
}
\arguments{
\item{geohashes}{(character vector) Geohashes to expand.}
}
\value{
Character vector of each geohash in \code{geohashes} followed by its parents.
}
\description{
Expand Geohashes
}
\examples{
expand_geohashes("0011")
expand_geohashes(c("0011", "110011"))
xyz <- xy_to_gridcells(lnglat_to_xy(cbind(123.4567, 12.3456)), 20)
geohashes <- gridcells_to_geohashes(xyz)
exyz <- geohashes_to_gridcells(expand_geohashes(geohashes))
lnglat <- xy_to_lnglat(gridcells_to_xy(exyz))
cbind(exyz, lnglat)
}
