Clusters
================
Ethan Welty
2023-02-03

Falling Fruit uses clusters to summarize the number and location of
types available over large areas. The clusters are computed using a very
fast algorithm described below.

## Convert to Web Mercator

Coordinates are stored in the database as standard WGS 84 longitude and
latitude in decimal degrees (EPSG:4326).

``` r
locations <- data.table::data.table(
  lng = c(-90, 90, -90, 90),
  lat = c(-45, 45, 45, -45)
)
```

Because web mapping applications use the Web Mercator projection, we
first convert our coordinates to Web Mercator (EPSG:3857).

``` r
locations[, c("x", "y") := as.data.frame(
  fruitr::lnglat_to_xy(cbind(lng, lat))
)]
```

    ##    lng lat         x        y
    ## 1: -90 -45 -10018754 -5621521
    ## 2:  90  45  10018754  5621521
    ## 3: -90  45 -10018754  5621521
    ## 4:  90 -45  10018754 -5621521

## Quadtree the Earth

In the Web Mercator projection, if you cut off the poles (at roughly +/-
85° latitude), the surface of the Earth is conveniently represented by a
square (`zoom = 0`). This square Earth can be divided into four smaller
squares (`zoom = 1`), each of which can be divided into four even
smaller squares, and so on. At any given zoom level in this
[quadtree](https://en.wikipedia.org/wiki/Quadtree), the Earth is divided
into 4<sup>zoom</sup> equal-sized squares.

The squares (grid “cells”) at each zoom level can be numbered with a
simple x and y integer index starting from the cell (0, 0) at the bottom
left corner (near -180° longitude, -85° latitude). For speed, we compute
grid cell indices for only the highest supported zoom level, then build
the lower zoom levels from these. Falling Fruit computes clusters to
`zoom = 13`, but for the sake of example, let’s use `zoom = 1`.

``` r
locations[, c("xi", "yi", "zoom") := as.data.frame(
  fruitr::xy_to_gridcells(cbind(x, y), zoom = 1)
)]
```

    ##    lng lat         x        y xi yi zoom
    ## 1: -90 -45 -10018754 -5621521  0  0    1
    ## 2:  90  45  10018754  5621521  1  1    1
    ## 3: -90  45 -10018754  5621521  0  1    1
    ## 4:  90 -45  10018754 -5621521  1  0    1

With grid cell indices assigned to each location, we can now calculate
the total count and center of mass of all locations in each grid cell.

``` r
clusters <- locations[,.(x = mean(x), y = mean(y), count = .N), by = .(xi, yi, zoom)]
```

    ##    xi yi zoom         x        y count
    ## 1:  0  0    1 -10018754 -5621521     1
    ## 2:  1  1    1  10018754  5621521     1
    ## 3:  0  1    1 -10018754  5621521     1
    ## 4:  1  0    1  10018754 -5621521     1

## Geohash the parents

But how do we generate the lower zoom level clusters? As it turns out,
from the grid cell indices we can construct binary
[geohashes](https://en.wikipedia.org/wiki/Geohash) which “magically”
contain the indices of all parent cells!

At `zoom = 0`, one cell encompasses the entire Earth. Counting from
zero, that cell is number zero. In binary, the number zero (0) is “00”,
which is also the x and y grid indices for that cell: (0, 0).

At `zoom = 1`, the four cells are:

- 0: “00” or (0, 0) (bottom left)
- 1: “01” or (0, 1) (top left)
- 2: “10” or (1, 0) (bottom right)
- 3: “11” or (1, 1) (top right)

To construct our geohashes, we convert the x and y grid indices to
binary and interleave the bits. For example, cell (1, 0) - which is
(“01”, “00”) in binary - becomes “00 10”. Therefore, cell “00 10” (1, 0)
at `zoom = 1` is the child of cell “00” (0, 0) at `zoom = 0`. At
`zoom = 2`, the cell (2, 1) is (“010”, “001”) in binary (left-padded
with zeros to account for the higher zoom level). Interleaved, this
becomes “00 10 01”, from which we know that cell “00 10 01” (2, 1) at
`zoom = 2` is the child of cell “00 10” (1, 0) at `zoom = 1` and cell
“00” (0, 0) at `zoom = 0`.

In practice, we compute the geohashes of our base clusters, then
replicate the cluster for each of its parents by expanding its geohash.

``` r
clusters[, geohash := fruitr::gridcells_to_geohashes(cbind(xi, yi, zoom))]
geohashes <- clusters[, fruitr::expand_geohashes(geohash)]
clusters <- clusters[rep(1:.N, nchar(geohash) / 2)]
clusters[, geohash := geohashes]
```

    ##    xi yi zoom         x        y count geohash
    ## 1:  0  0    1 -10018754 -5621521     1    0000
    ## 2:  0  0    1 -10018754 -5621521     1      00
    ## 3:  1  1    1  10018754  5621521     1    0011
    ## 4:  1  1    1  10018754  5621521     1      00
    ## 5:  0  1    1 -10018754  5621521     1    0001
    ## 6:  0  1    1 -10018754  5621521     1      00
    ## 7:  1  0    1  10018754 -5621521     1    0010
    ## 8:  1  0    1  10018754 -5621521     1      00

The total count and center of mass of each parent cluster can then be
calculated from all rows with the same geohash.

``` r
clusters <- clusters[, 
    .(x = weighted.mean(x, count), y = weighted.mean(y, count), count = sum(count)),
    by = geohash
]
```

    ##    geohash         x        y count
    ## 1:    0000 -10018754 -5621521     1
    ## 2:      00         0        0     4
    ## 3:    0011  10018754  5621521     1
    ## 4:    0001 -10018754  5621521     1
    ## 5:    0010  10018754 -5621521     1

## Finalize clusters

The zoom level of each cluster is a simple function of the length of
it’s geohash (\`nchar(geohash) / 2 - 1).

``` r
clusters[, zoom := fruitr::geohashes_to_zoom(geohash)]
```

    ##    geohash         x        y count zoom
    ## 1:    0000 -10018754 -5621521     1    1
    ## 2:      00         0        0     4    0
    ## 3:    0011  10018754  5621521     1    1
    ## 4:    0001 -10018754  5621521     1    1
    ## 5:    0010  10018754 -5621521     1    1

Finally, the centers of mass computed in Web Mercator are converted to
WGS 84 longitude and latitude.

``` r
clusters[, c("lng", "lat") := as.data.frame(
  fruitr::xy_to_lnglat(cbind(x, y))
)]
```

    ##    geohash         x        y count zoom lng lat
    ## 1:    0000 -10018754 -5621521     1    1 -90 -45
    ## 2:      00         0        0     4    0   0   0
    ## 3:    0011  10018754  5621521     1    1  90  45
    ## 4:    0001 -10018754  5621521     1    1 -90  45
    ## 5:    0010  10018754 -5621521     1    1  90 -45

Which we can check against our original locations.

    ##    lng lat
    ## 1: -90 -45
    ## 2:  90  45
    ## 3: -90  45
    ## 4:  90 -45

## Update clusters

As locations are added or removed, the clusters need to be updated. The
geohashes of all cells containing the location are computed (with the
process described above) and used to quickly lookup and increment (or
decrement) all impacted clusters.
