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
#' @family cluster functions
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
  return(sprintf(paste0("%0", nchars, "s"), binary(x)))
}

#' Convert Coordinates to Grid Cell Indices
#'
#' Converts geographic WGS84 (\href{http://spatialreference.org/ref/epsg/wgs-84/}{EPSG:4326}) coordinates to Web Mercator (\href{http://spatialreference.org/ref/sr-org/7483/}{EPSG:3857}) quadtree grid cell indices.
#'
#' @param lnglat Rows of coordinates in decimal degrees (longitude, latitude), with longitude: [-180, 180) and latitude: [-85.05113, 85.05113).
#' @param zoom Zoom level(s) of the quadtree dividing the Earth into a square \eqn{2^{zoom}}{2^zoom} grid.
#' @return Rows of zero-based, integer grid cell indices (x, y, zoom).
#' @export
#' @family cluster functions
#' @examples
#' lng <- c(-45, -45, 45, 45)
#' lat <- c(-45, 45, 45, -45)
#' lnglat <- cbind(lng, lat)
#' lnglat_to_gridcells(lnglat, zoom = 1)
lnglat_to_gridcells <- function(lnglat, zoom = 0L) {
  if (is.vector(lnglat)) {
    lnglat = t(lnglat[1:2])
  }
  if (any(lnglat[, 1] < -180 | lnglat[, 1] >= 180)) {
    stop("Longitudes must be in the range [-180, 180).")
  }
  if (any(lnglat[, 2] < -85.05113 | lnglat[, 2] > 85.05113)) {
    stop("Latitudes must be in the range [-85.05113, 85.05113).")
  }
  # Convert WGS84 to Web Mercator
  x <- (lnglat[, 1] / 360) * Earth_circumference
  y <- log(tan((lnglat[, 2] + 90) * (pi / 360))) * Earth_radius
  # Move origin to bottom left corner
  xy <- cbind(x, y) + (Earth_circumference / 2)
  # Convert to grid cell indices
  cell_size <- Earth_circumference / (2 ^ zoom)
  xyi <- floor(xy / cell_size)
  # HACK: Snap yi to 0 for minimum latitude
  xyi[lnglat[, 2] == -85.05113, 2] <- 0
  return(cbind(xyi, zoom))
}

#' Convert Grid Cell Indices to Coordinates
#'
#' Converts Web Mercator (\href{http://spatialreference.org/ref/sr-org/7483/}{EPSG:3857}) quadtree grid cell indices to geographic WGS84 (\href{http://spatialreference.org/ref/epsg/wgs-84/}{EPSG:4326}) coordinates.
#'
#' @param xyz Rows of zero-based, integer grid cell indices (x, y, zoom).
#' @return Rows of coordinates in decimal degrees (longitude, latitude) corresponding to the lower-left cell corners.
#' @export
#' @seealso \code{\link{lnglat_to_gridcells}} for the reverse conversion.
#' @family cluster functions
#' @examples
#' xi <- c(0, 0, 1, 0)
#' yi <- c(0, 1, 1, 0)
#' zoom <- c(0, 1, 1, 2)
#' xyz <- cbind(xi, yi, zoom)
#' lnglat <- gridcells_to_lnglat(xyz)
#' xyz == lnglat_to_gridcells(lnglat, zoom)
gridcells_to_lnglat <- function(xyz) {
  if (is.vector(xyz)) {
    xyz = t(xyz[1:3])
  }
  # Convert to meters
  grid_cell_size <- Earth_circumference / (2 ^ xyz[, 3])
  xy <- xyz[, 1:2] * grid_cell_size
  # Move origin to center
  xy <- xy - (Earth_circumference / 2)
  # Web Mercator -> WGS84
  lng <- xy[, 1] * (360 / Earth_circumference)
  lat <- 90 - (atan2(1, exp(xy[, 2] / Earth_radius)) * (360 / pi))
  return(cbind(lng, lat))
}

#' Convert Grid Cell Indices to Geohashes
#'
#' Converts Web Mercator (\href{http://spatialreference.org/ref/sr-org/7483/}{EPSG:3857}) quadtree grid cell indices to binary geohashes.
#'
#' @param grid_cells Rows of Web Mercator grid cell indices (x, y, zoom).
#' @return A character vector of geohashes.
#' @export
#' @family cluster functions
#' @examples
#' xi <- c(0, 0, 1, 0)
#' yi <- c(0, 1, 1, 0)
#' zoom <- c(0, 1, 1, 2)
#' xyz <- cbind(xi, yi, zoom)
#' gridcells_to_geohashes(xyz)
gridcells_to_geohashes <- function(xyz) {
  if (is.vector(xyz)) {
    xyz = t(xyz[1:3])
  }
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
  return(geohashes)
}

#' Convert Geohashes to Grid Cell Indices
#'
#' Converts binary geohashes to Web Mercator (\href{http://spatialreference.org/ref/sr-org/7483/}{EPSG:3857}) quadtree grid cell indices.
#'
#' @param geohashes A character vector of geohashes.
#' @return Rows of Web Mercator grid cell indices (x, y, zoom).
#' @export
#' @family cluster functions
#' @seealso \code{\link{gridcells_to_geohashes}} for the reverse conversion.
#' @examples
#' geohashes <- c("00", "0000", "0001", "0010", "001001")
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
#' @export
#' @family cluster functions
#' @examples
#' expand_geohashes("0011")
#' expand_geohashes(c("0011", "110011"))
#' xyz <- lnglat_to_gridcells(c(123.4567, 12.3456), 31)
#' geohashes <- gridcells_to_geohashes(xyz)
#' exyz <- geohashes_to_gridcells(expand_geohashes(geohashes))
#' lnglat <- gridcells_to_lnglat(exyz)
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
  return(expanded[order(rows)])
}
