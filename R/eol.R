#' Get Encyclopedia of Life (EOL) Search Results
#'
#' Alterations to default parameter values: exact (FALSE => TRUE). For reliable results, run an exact search (exact = TRUE) with a scientific name search string (q).
#'
#' @param q (character) Search string.
#' @param page (integer) Index of result page to return (each of which includes up to 30 results).
#' @param exact (boolean) Whether to only return taxons whose title, synonym or common name exactly matches \code{q}.
#' @param filter_by_taxon_concept_id (integer) EOL Page ID. Results will be limited to members of that taxonomic group.
#' @param filter_by_hierarchy_entry_id (integer) Hierarchy Entry ID. Results will be limited to members of that taxonomic group.
#' @param filter_by_string (character) Results will be limited to members of the taxonomic group matching the search term.
#' @param cache_ttl (integer) Seconds you wish to have the response cached.
#' @family EOL functions
#' @source \url{http://eol.org/api/docs/search}
#' @export
#' @examples
#' str(httr::content(get_eol_search("Malus domestica"))) # few equivalent results
get_eol_search <- function(q, page = 1, exact = TRUE, filter_by_taxon_concept_id, filter_by_hierarchy_entry_id, filter_by_string, cache_ttl) {
  url <- "http://eol.org/api/search/1.0.json"
  query <- mget(c("q", "page", "exact", "filter_by_taxon_concept_id", "filter_by_hierarchy_entry_id", "filter_by_string", "cache_ttl"))
  return(httr::GET(url, query = query[sapply(query, "!=", "")]))
}

#' Parse Encyclopedia of Life (EOL) Search Results
#'
#' @param search (response) Result of \code{\link{get_eol_search}}.
#' @param types (characte vector) Data types to parse.
#' @family EOL functions
#' @export
#' @examples
#' s <- get_eol_search("Malus domestica")
#' str(parse_eol_search(s)) # few equivalent results
#' s <- get_eol_search("Abelmoschus")
#' str(parse_eol_search(s)) # one result
#' s <- get_eol_search("Forbidden fruit")
#' str(parse_eol_search(s)) # no results
parse_eol_search <- function(search, types = c("results", "ids")) {
  result <- list()
  json <- httr::content(search)
  ## Results
  if ("results" %in% types) {
    result$results <- json$results
  }
  ## IDs
  if ("ids" %in% types) {
    result$ids <- sapply(json$results, "[[", "id")
  }
  ## Return
  return(result)
}

#' Get Encyclopedia of Life (EOL) Page
#'
#' Alterations to default parameter values: common_names (FALSE => TRUE), synonyms: (FALSE => TRUE).
#'
#' @param id (character) EOL Page ID.
#' @param batch (boolean) Whether to return a batch.
#' @param images_per_page (integer) Max image objects to return.
#' @param images_page (integer) Image page number.
#' @param videos_per_page (integer) Max video object to return.
#' @param videos_page (integer) Video page number.
#' @param sounds_per_page (integer) Max sound objects to return.
#' @param sounds_page (integer) Sound page number.
#' @param maps_per_page (integer) Max map objects to return.
#' @param maps_page (integer) Map page number.
#' @param texts_per_page (integer) Max text objects to return.
#' @param texts_page (integer) Text page number.
#' @param subjects (character) Result type: "overview" or "all".
#' @param licenses (character) Pipe-delimited list of licenses: "all" or "cc-by", "cc-by-nc", "cc-by-sa", "cc-by-nc-sa", "pd" (public domain), or "na" (not applicable).
#' @param details (boolean) Whether to return all metadata for data objects.
#' @param common_names (boolean) Whether to return all common names.
#' @param synonyms (boolean) Whether to return all scientific synonyms.
#' @param references (boolean) Whether to return all references.
#' @param taxonomy (boolean) Whether to return taxonomy details from taxon hierarchy providers.
#' @param vetted (integer) Trusted content filter level: 0 (all content), 1 (trusted content), 2 (trusted and unreviewed content), 3 (unreviewed content), or 4 (only untrusted content).
#' @param cache_ttl (integer) Seconds you wish to have the response cached.
#' @param language (character) Response language: "en" or ms, de, es, fr, gl, it, nl, nb, oc, pt-BR, sv, tl, mk, sr, uk, ar, zh-Hans, zh-Hant, or ko.
#' @family EOL functions
#' @source \url{http://eol.org/api/docs/pages}
#' @export
#' @examples
#' s <- get_eol_search("Malus domestica")
#' id <- parse_eol_search(s, "ids")$ids[1]
#' str(httr::content(get_eol_page(id)))
get_eol_page <- function(id, batch = FALSE, images_per_page, images_page = 1, videos_per_page, videos_page = 1, sounds_per_page, sounds_page = 1, maps_per_page, maps_page = 1, texts_per_page, texts_page = 1, subjects = "overview", licenses = "all", details = FALSE, common_names = TRUE, synonyms = TRUE, references = FALSE, taxonomy = TRUE, vetted = 0, cache_ttl, language = "en") {
  url <- paste0("http://eol.org/api/pages/1.0/", id, ".json")
  query <- mget(c("batch", "images_per_page", "images_page", "videos_per_page", "videos_page", "sounds_per_page", "sounds_page", "maps_per_page", "maps_page", "texts_per_page", "texts_page", "subjects", "licenses", "details", "common_names", "synonyms", "references", "taxonomy", "vetted", "cache_ttl", "language"))
  return(httr::GET(url, query = query[sapply(query, "!=", "")]))
}

#' Parse Encyclopedia of Life (EOL) Page
#'
#' @param page (response) Result of \code{\link{get_eol_page}}.
#' @param types (character) Data types to parse.
#' @family EOL functions
#' @export
#' @examples
#' s <- get_eol_search("Malus domestica")
#' id <- parse_eol_search(s, "ids")$ids[1]
#' pg <- get_eol_page(id)
#' str(parse_eol_page(pg))
parse_eol_page <- function(page, types = c("scientific_names", "common_names")) {
  result <- list()
  json <- jsonlite::fromJSON(rawToChar(page$content), simplifyVector = FALSE)
  if (!is.null(unlist(json, recursive = FALSE)$error)) {
    return(result)
  }
  ## Scientific names
  if ("scientific_names" %in% types) {
    canonical_names <- unique(lapply(json$taxonConcepts, function(x) {
      list(
        # eol_id = x$identifier,
        # source = x$nameAccordingTo,
        name = x$canonicalForm,
        rank = x$taxonRank,
        preferred = TRUE
      )
    }))
    synonyms <- unique(lapply(json$synonyms, function(x) {
      list(
        # source = x$resource,
        # relationship = x$relationship,
        name = format_scientific_names(x$synonyms),
        rank = NA,
        preferred = FALSE
      )
    }))
    result$scientific_names <- c(canonical_names, synonyms)
  }
  ## Common names
  if ("common_names" %in% types) {
    common_names <- lapply(json$vernacularNames, function(x) {
      list(
        name = x$vernacularName,
        language = x$language,
        preferred = ifelse(is.null(x$eol_preferred), FALSE, TRUE)
      )
    })
    result$common_names <- common_names
  }
  ## Return
  return(result)
}
