#' Get iNaturalist Taxon Search Results
#'
#' Undocumented, but equivalent to the HTML equivalent \url{http://www.inaturalist.org/taxa/search}.
#'
#' @param q (character) Search string.
#' @param is_active Either "true", "false", or "any".
#' @export
#' @family iNaturalist functions
#' @examples
#' s <- get_inaturalist_search("Malus domestica")
#' str(httr::content(s))
get_inaturalist_search <- function(q, is_active = "true") {
  q <- gsub("\\s*x\\s*", " \u00d7 ", q)
  url <- "http://www.inaturalist.org/taxa/search.json"
  query <- mget(c("q", "is_active"))
  return(httr::GET(url, query = query[sapply(query, "!=", "")]))
}

#' Parse iNaturalist Taxon Search Results
#'
#' @param search (response) Result of \code{\link{get_inaturalist_search}}.
#' @param types (character vector) Data types to parse.
#' @param exact (boolean) Whether to only keep results which match the search string exactly.
#' @param scientific_name (boolean) Whether to only keep results which a matching scientific name.
#' @param ignore.case (boolean) Whether to ignore case.
#' @export
#' @family iNaturalist functions
#' @examples
#' s <- get_inaturalist_search("Malus domestica")
#' str(parse_inaturalist_search(s))
parse_inaturalist_search <- function(search, types = c("results", "ids"), exact = TRUE, scientific_name = TRUE, ignore.case = TRUE) {
  result <- list()
  json <- httr::content(search)
  q <- httr::parse_url(search$url)$query$q
  ## Filter results
  if (exact) {
    q <- paste0("^", q, "$")
  }
  ind <- unlist(sapply(json, function(result) {
    is_match <- grepl(q, sapply(result$taxon_names, "[[", "name"), ignore.case = ignore.case)
    is_scientific_name <- sapply(result$taxon_names, "[[", "lexicon") == "Scientific Names"
    if (scientific_name) {
      any(is_match & is_scientific_name)
    } else {
      any(is_match & !is_scientific_name)
    }
  }))
  ## Results
  if ("results" %in% types) {
    result$results <- json[ind]
  }
  ## IDs
  if ("ids" %in% types) {
    result$ids <- sapply(json[ind], "[[", "id")
  }
  ## Return
  return(result)
}

#' Get iNaturalist Taxon Page
#'
#' Undocumented, but equivalent to the HTML equivalent \url{http://www.inaturalist.org/taxa/:id.json}.
#'
#' @param id iNaturalist page ID.
#' @export
#' @family iNaturalist functions
#' @examples
#' s <- get_inaturalist_search("Malus domestica")
#' id <- parse_inaturalist_search(s, "ids")$ids[1]
#' pg <- get_inaturalist_page(id)
#' str(httr::content(pg))
get_inaturalist_page <- function(id) {
  url <- paste0("http://www.inaturalist.org/taxa/", id, ".json")
  return(httr::GET(url))
}

#' Parse iNaturalist Taxon Page
#'
#' @param page (response) Result of \code{\link{get_inaturalist_page}}.
#' @param types (character vector) Data types to parse.
#' @export
#' @family iNaturalist functions
#' @examples
#' s <- get_inaturalist_search("Malus domestica")
#' id <- parse_inaturalist_search(s, "ids")$ids[1]
#' pg <- get_inaturalist_page(id)
#' str(parse_inaturalist_page(pg))
parse_inaturalist_page <- function(page, types = c("scientific_names", "common_names")) {
  result <- list()
  json <- jsonlite::fromJSON(rawToChar(page$content), simplifyVector = FALSE)
  if (!is.null(json$error)) {
    return(result)
  }
  ## Scientific names
  if ("scientific_names" %in% types) {
    scientific_names <- unique(lapply(json$taxon_names, function(x) {
      if (!is.null(x$lexicon) && x$lexicon == "Scientific Names") {
        list(
          name = x$name,
          preferred = ifelse(!is.empty(x$is_valid) && x$is_valid, TRUE, FALSE)
        )
      }
    }))
    result$scientific_names <- scientific_names[!is.empty(scientific_names)]
  }
  ## Common names
  if ("common_names" %in% types) {
    common_names <- unique(lapply(json$taxon_names, function(x) {
      if (!is.null(x$lexicon) && x$lexicon != "" && x$lexicon != "Scientific Names") {
        list(
          name = x$name,
          language = x$lexicon,
          preferred = ifelse(!is.empty(x$is_valid) && x$is_valid, TRUE, FALSE)
        )
      }
    }))
    result$common_names <- common_names[!is.empty(common_names)]
  }
  ## Return
  return(result)
}
