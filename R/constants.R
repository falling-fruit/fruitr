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
Location_import_fields <- c('Ids', 'Types', 'Description', 'Lat', 'Lng', 'Address', 'Season Start', 'Season Stop', 'No Season', 'Access', 'Unverified', 'Yield Rating', 'Quality Rating', 'Author', 'Photo URL')

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

#' Language codes
#'
#' Comprehensive table of language names and codes.
#'
#' \itemize{
#'   \item locale - Equivalent regional locale, if one exists.
#'   \item variant - Equivalent script variant, if one exists.
#'   \item ISO639.1 - \href{https://en.wikipedia.org/wiki/List_of_ISO_639-1_codes}{ISO 639-1} language code.
#'   \item ISO639.2T - \href{https://en.wikipedia.org/wiki/ISO_639-2}{ISO 639-2T} language code.
#'   \item ISO639.2B - \href{https://en.wikipedia.org/wiki/ISO_639-2}{ISO 639-2B} language code.
#'   \item ISO639.3 - \href{https://en.wikipedia.org/wiki/ISO_639-3}{ISO 639-3} language code.
#'   \item ISO639.6 - \href{https://en.wikipedia.org/wiki/ISO_639-6}{ISO 639-6} language code.
#'   \item wikipedia - \href{https://en.wikipedia.org/wiki/List_of_Wikipedias}{Wikipedia} language code.
#'   \item other - Alternative language code.
#'   \item autonym - Name(s) for the language in the language.
#'   \item en - English name(s) for the language.
#'   \item fr - French name(s) for the language.
#'   \item de - German name(s) for the language.
#'   \item ru - Russian name(s) for the language.
#'   \item it - Italian name(s) for the language.
#'   \item zh - Chinese name(s) for the language.
#'   \item ISO639.3_macro - \href{https://en.wikipedia.org/wiki/ISO_639_macrolanguage}{ISO 639-3 macrolanguage}.
#'   \item scope - Language scope (\code{"individual"}, \code{"macro"}, \code{"collective"}, or \code{"special"}).
#'   \item type - Language type (\code{"living"}, \code{"ancient"}, \code{"historical"}, \code{"extinct"}, \code{"constructed"}, or \code{"special"}).
#'   \item family - Family to which the language belongs.
#'   \item retired - Whether the language has been retired.
#'   \item wikpipedia_url - URL to the wikipedia article for the language.
#'   \item notes - Additional notes.
#' }
#'
#' @format A \code{data.table}.
#' @name Language_codes
#' @family constants
NULL

#' Google Custom Search Languages
#'
#' List of the languages supported by Google Custom Search.
#'
#' @source \url{https://developers.google.com/custom-search/docs/ref_languages}
#' @export
#' @family constants
Google_cs_languages <- c("ar", "bg", "ca", "cs", "da", "de", "el", "en", "es", "fi", "fil", "fr", "he", "hi", "hr", "hu", "id", "it", "ja", "ko", "lt", "lv", "nl", "no", "pl", "pt", "ro", "ru", "sk", "sl", "sr", "sv", "th", "tr", "uk", "vi", "zh-Hans", "zh-Hant")
