#' Falling Fruit Taxonomic ranks
#'
#' Names of the taxonomic ranks returned by Falling Fruit as zero-based indices.
#'
#' @export
#' @family constants
Taxonomic_ranks <- c("Polyphyletic", "Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Multispecies", "Species", "Subspecies")

#' Falling Fruit Categories
#'
#' Names of the categories returned by Falling Fruit as bitmasks.
#'
#' @export
#' @family constants
Categories <- c("forager", "freegan", "honeybee", "grafter")

#' Falling Fruit Location import fields
#'
#' Field names expected by Falling Fruit for locations import.
#'
#' @export
#' @family constants
Location_import_fields <- c("Ids", "Types", "Description", "Lat", "Lng", "Address", "Season Start", "Season Stop", "No Season", "Access", "Unverified", "Yield Rating", "Quality Rating", "Author", "Photo URL")

#' Earth radius
#'
#' The radius of the Earth, in meters.
#'
#' @export
#' @family constants
Earth_radius <- 6378137

#' Earth circumference
#'
#' The circumference of the Earth, in meters.
#'
#' @export
#' @family constants
Earth_circumference <- 2 * pi * Earth_radius

#' Google Custom Search Languages
#'
#' List of the languages supported by Google Custom Search.
#'
#' @source \url{https://developers.google.com/custom-search/docs/ref_languages}
#' @export
#' @family constants
Google_cs_languages <- c("ar", "bg", "ca", "cs", "da", "de", "el", "en", "es", "fi", "fil", "fr", "he", "hi", "hr", "hu", "id", "it", "ja", "ko", "lt", "lv", "nl", "no", "pl", "pt", "ro", "ru", "sk", "sl", "sr", "sv", "th", "tr", "uk", "vi", "zh-Hans", "zh-Hant")
