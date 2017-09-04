#' Convert Integers to Strings
#'
#' Converts integers to strings by the given base and character count.
#'
#' @param x Integer vector, or something coercible to this by \code{\link[base]{as.integer}}.
#' @param base Integer between 2 and 11 inclusive.
#' @param nchars Minimum number of characters desired. Shorter strings are left-padded with zeros.
#' @return A character vector of the same length as \code{x}.
#' @export
#' @seealso \code{\link[base]{strtoi}} for the reverse conversion.
#' @examples
#' itostr(123, nchars = 4)
#' strtoi(itostr(123, base = 2), base = 2)
#' itostr(c(0, 1, 2, 10), base = 2)
#' itostr(c(0, 1, 2, 10), base = 2, nchars = 4)
itostr <- function(x, base = 10L, nchars = 0L) {
  binary <- function(x) {
    if (all(x < base)) {
      x
    } else {
      xstr <- rep("0", length(x))
      ind <- x != 0
      xstr[ind] <- paste0(binary(x[ind] %/% base), x[ind] %% base)
      return(xstr)
    }
  }
  if (base < 2 || base > 11) {
   stop(paste("Base", base, "not supported"))
  }
  x <- as.integer(x)
  sprintf(paste0("%0", nchars, "s"), binary(x))
}

#' Convert between WGS84 and Web Mercator Coordinates
#'
#' Converts between geographic WGS84 (\href{http://spatialreference.org/ref/epsg/wgs-84/}{EPSG:4326}) coordinates in decimal degrees and projected Web Mercator (\href{http://spatialreference.org/ref/sr-org/7483/}{EPSG:3857}) coordinates in meters.
#'
#' @param lnglat Rows of coordinates in decimal degrees (longitude, latitude), with longitude: [-180, 180) and latitude: [-85.05112, 85.05112].
#' @export
lnglat_to_xy <- function(lnglat) {
  if (any(lnglat[, 1] < -180 | lnglat[, 1] >= 180)) {
    stop("Longitudes must be in the range [-180, 180).")
  }
  if (any(lnglat[, 2] < -85.05112 | lnglat[, 2] > 85.05112)) {
    stop("Latitudes must be in the range [-85.05112, 85.05112].")
  }
  cbind(
    x = (lnglat[, 1] / 360) * Earth_circumference,
    y = log(tan((lnglat[, 2] + 90) * (pi / 360))) * Earth_radius
  )
}
#' @rdname lnglat_to_xy
#' @param xy Rows of Web Mercator coordinates in meters (x, y).
#' @export
#' @examples
#' lng <- c(-45, -45, 45, 45)
#' lat <- c(-45, 45, 45, -45)
#' lnglat <- cbind(lng, lat)
#' xy_to_lnglat(lnglat_to_xy(lnglat)) - lnglat
xy_to_lnglat <- function(xy) {
  cbind(
    lng = xy[, 1] * (360 / Earth_circumference),
    lat = 90 - (atan2(1, exp(xy[, 2] / Earth_radius)) * (360 / pi))
  )
}

#' Convert between Web Mercator Coordinates and Grid Cell Indices
#'
#' Converts between projected Web Mercator (\href{http://spatialreference.org/ref/sr-org/7483/}{EPSG:3857}) coordinates in meters and quadtree grid cell indices. The conversion back from grid cell indices returns the coordinates of the lower left corner of the cell.
#'
#' @param xy Rows of Web Mercator coordinates in meters (x, y).
#' @param zoom Zoom level(s) of the quadtree dividing the Earth into a square \eqn{2^{zoom}}{2^zoom} grid.
#' @export
xy_to_gridcells <- function(xy, zoom = 0L) {
  # Move origin to bottom left corner
  xy <- xy + (Earth_circumference / 2)
  cell_size <- Earth_circumference / (2 ^ zoom)
  xyi <- floor(xy / cell_size)
  cbind(xyi, zoom)
}
#' @rdname xy_to_gridcells
#' @param xyz Rows of zero-based, integer grid cell indices (x, y, zoom).
#' @export
#' @examples
#' xy <- cbind(x = 0, y = 0)
#' xyz <- xy_to_gridcells(xy, zoom = 1)
#' gridcells_to_xy(xyz)
#' xyz <- xy_to_gridcells(xy, zoom = 0)
#' gridcells_to_xy(xyz)
gridcells_to_xy <- function(xyz) {
  cell_size <- Earth_circumference / (2 ^ xyz[, 3])
  xy <- xyz[, 1:2, drop = FALSE] * cell_size
  # Move origin to center
  xy - (Earth_circumference / 2)
}

#' Convert between Grid Cell Indices and Geohashes
#'
#' Converts between Web Mercator (\href{http://spatialreference.org/ref/sr-org/7483/}{EPSG:3857}) quadtree grid cell indices and binary geohashes.
#'
#' @param xyz Rows of Web Mercator grid cell indices (x, y, zoom).
#' @export
gridcells_to_geohashes <- function(xyz) {
  if (any(xyz[, 1:2] >  2 ^ xyz[, 3] - 1)) {
    stop("x, y indices must be in the range [0, 2^zoom)")
  }
  # Convert indices to binary string representations
  xb <- itostr(xyz[, 1], base = 2, nchars = xyz[, 3] + 1)
  yb <- itostr(xyz[, 2], base = 2, nchars = xyz[, 3] + 1)
  xyb <- cbind(strsplit(xb, ""), strsplit(yb, ""))
  # Build geohashes by interleaving x, y bits
  geohashes <- apply(xyb, 1, function(x) {
    paste0(c(rbind(x[[1]], x[[2]])), collapse = "")
  })
  geohashes
}
#' @rdname gridcells_to_geohashes
#' @param geohashes A character vector of geohashes.
#' @export
#' @examples
#' xi <- c(0, 0, 1, 0)
#' yi <- c(0, 1, 1, 0)
#' zoom <- c(0, 1, 1, 2)
#' xyz <- cbind(xi, yi, zoom)
#' geohashes <- gridcells_to_geohashes(xyz)
#' xyz <- geohashes_to_gridcells(geohashes)
#' geohashes == gridcells_to_geohashes(xyz)
geohashes_to_gridcells <- function(geohashes) {
  # Expand geohashes to binary x, y
  xb <- gsub("(.).", "\\1", geohashes)
  yb <- gsub(".(.)", "\\1", geohashes)
  # Convert binary to integer indices
  xi <- strtoi(xb, base = 2)
  yi <- strtoi(yb, base = 2)
  zoom <- (nchar(geohashes) / 2) - 1
  return(cbind(xi, yi, zoom))
}

#' Expand Geohashes
#'
#' @param geohashes (character vector) Geohashes to expand.
#' @return Character vector of each geohash in \code{geohashes} followed by its parents.
#' @export
#' @examples
#' expand_geohashes("0011")
#' expand_geohashes(c("0011", "110011"))
#' xyz <- xy_to_gridcells(lnglat_to_xy(cbind(123.4567, 12.3456)), 20)
#' geohashes <- gridcells_to_geohashes(xyz)
#' exyz <- geohashes_to_gridcells(expand_geohashes(geohashes))
#' lnglat <- xy_to_lnglat(gridcells_to_xy(exyz))
#' cbind(exyz, lnglat)
expand_geohashes <- function(geohashes) {
  rows <- 1:length(geohashes)
  lengths <- nchar(geohashes)
  subset_lengths <- seq(max(lengths) - 2, 2, by = -2)
  expanded <- c(geohashes, unlist(sapply(subset_lengths, function(subset_length) {
    ind <- which(lengths > subset_length)
    rows <<- c(rows, ind) # WARNING: writing to variable outside apply() scope.
    substr(geohashes[ind], 1, subset_length)
  })))
  expanded[order(rows)]
}

#' Calculate Zoom from Geohashes
#'
#' @param geohashes Geohashes.
#' @export
geohashes_to_zoom <- function(geohashes) {
  as.integer((nchar(geohashes) / 2) - 1)
}
