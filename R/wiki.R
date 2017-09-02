# Wiki ----------------

#' Parse Wiki URL
#'
#' @param url (character) URL.
#' @param api (boolean) Whether \code{url} is for an API.
#' @export
#' @family Wiki functions
#' @examples
#' parse_wiki_url("https://en.wikipedia.org/wiki/Malus_domestica")
parse_wiki_url <- function(url, api = FALSE) {
  if (!is.empty(url) && grepl("/w/api.php?", url)) {
    matches <- stringr::str_match(url, "//([^\\.]+).([^\\.]+).[^/]*/w/api\\.php\\?.*page=([^&]+).*$")
  } else {
    matches <- stringr::str_match(url, "//([^\\.]+).([^\\.]+).[^/]*/wiki/([^\\?]+)")
  }
  return(list(
    wiki = matches[2],
    type = matches[3],
    page = matches[4]
  ))
}

#' Build Wiki URL
#'
#' @param wiki (character) Wiki name.
#' @param type (character) Wiki type.
#' @param page (character) Wiki page title.
#' @param url (named list) Alternatively, \code{wiki}, \code{type}, and \code{page} as a list.
#' @export
#' @family Wiki functions
#' @examples
#' build_wiki_url("en", "wikipedia", "Malus domestica")
#' build_wiki_url("commons", "wikimedia", "Malus domestica")
#' build_wiki_url("species", "wikimedia", "Malus domestica")
build_wiki_url <- function(wiki, type, page, url = NULL) {
  if (!is.null(url)) {
    wiki <- url$wiki
    type <- url$type
    page <- url$page
  }
  return(paste0("https://", wiki, ".", type, ".org/wiki/", gsub(" ", "_", page)))
}

#' Get Wiki Page from API
#'
#' @param url (character) Wiki page URL.
#' @param format (character) Response format.
#' @param action (character) Response action.
#' @param redirects (boolean) Whether to follow redirects.
#' @export
#' @family Wiki functions
#' @examples
#' pg <- get_wiki_page("https://en.wikipedia.org/wiki/Malus_domestica")
#' str(httr::content(pg))
get_wiki_page <- function(url, format = "json", action = "parse", redirects = TRUE) {
  url <- utils::URLdecode(url)
  params <- parse_wiki_url(url)
  url <- paste0("https://", params$wiki, ".", params$type, ".org/w/api.php")
  query <- c(page = params$page, mget(c("format", "action", "redirects")))
  return(httr::GET(url, query = query[sapply(query, "!=", "")]))
}

# Wikipedia ----------------

#' Parse Wikipedia Page
#'
#' @param page (response) Result of \code{\link{get_wiki_page}}.
#' @param types (character) Data types to parse.
#' @family Wiki functions
#' @export
#' @examples
#' pg <- get_wiki_page("https://en.wikipedia.org/wiki/Malus_domestica")
#' str(parse_wikipedia_page(pg))
parse_wikipedia_page <- function(page, types = c("common_names", "langlinks")) {
  result <- list()
  json <- jsonlite::fromJSON(rawToChar(page$content), simplifyVector = FALSE)
  if (is.null(json$parse)) {
    return(result)
  }
  ## Language links
  if ("langlinks" %in% types) {
    langlinks <- lapply(json$parse$langlinks, function(x) {
      list(
        language = x$lang,
        url = x$url
      )
    })
    result$langlinks <- langlinks
  }
  ## Common names
  if ("common_names" %in% types) {
    xml <- xml2::read_html(json$parse$text[[1]])
    language = stringr::str_match(page$url, 'http[s]*://([^\\.]*)\\.')[, 2]
    names_xml <- list(
      regular_bolds = xml2::xml_find_all(xml, xpath = "/html/body/p[count(preceding::div[contains(@id, 'toc') or contains(@class, 'toc')]) = 0 and count(preceding::h1) = 0 and count(preceding::h2) = 0 and count(preceding::h3) = 0]//b[not(parent::*[self::i]) and not(i)]"),
      regular_biotabox_header = xml2::xml_find_all(xml, xpath = "(//table[contains(@class, 'infobox biota') or contains(@class, 'infobox_v2 biota')]//th)[1]/b[not(parent::*[self::i]) and not(i)]")
    )
    regular_title <- stats::na.omit(stringr::str_match(json$parse$displaytitle, "^([^<]*)$")[, 2]) # Often unreliable
    names <- unique(c(unlist(sapply(names_xml, xml2::xml_text)), regular_title))
    common_names <- lapply(names, function(name) {list(name = name, language = language)})
    result$common_names <- common_names
  }
  ## Return
  return(result)
}

# Wikimedia ----------------

#' Parse Wikimedia Commons Page
#'
#' @inheritParams parse_wikipedia_page
#' @family Wiki functions
#' @export
#' @examples
#' pg <- get_wiki_page("https://commons.wikimedia.org/wiki/Malus_domestica")
#' str(parse_wikicommons_page(pg))
parse_wikicommons_page = function(page, types = c("common_names")) {
  result <- list()
  json <- jsonlite::fromJSON(rawToChar(page$content), simplifyVector = FALSE)
  if (is.null(json$parse)) {
    return(result)
  }
  ## Common names
  if ("common_names" %in% types) {
    xml <- xml2::read_html(json$parse$text[[1]])
    vernacular_html <- xml2::xml_find_all(xml, xpath = "//bdi[@class='vernacular']")
    # XML formats:
    # <bdi class="vernacular" lang="en"><a href="">name</a></bdi>
    # <bdi class="vernacular" lang="en">name</bdi>
    ## Name formats:
    # name1 / name2
    # name1, name2
    # name (category)
    common_names <- lapply(vernacular_html, function(x) {
      attributes <- xml2::xml_attrs(x)
      language <- attributes[["lang"]]
      name <- trimws(gsub("[ ]*\\(.*\\)", "", xml2::xml_text(x)))
      list(
        name = name,
        language = language
      )
    })
    result$common_names <- common_names
  }
  ## Return
  return(result)
}

#' Parse Wikispecies Page
#'
#' @inheritParams parse_wikipedia_page
#' @family Wiki functions
#' @export
#' @examples
#' pg <- get_wiki_page("https://species.wikimedia.org/wiki/Malus_domestica")
#' str(parse_wikispecies_page(pg))
parse_wikispecies_page <- function(page, types = c("common_names")) {
  result <- list()
  json <- jsonlite::fromJSON(rawToChar(page$content), simplifyVector = FALSE)
  if (is.null(json$parse)) {
    return(result)
  }
  ## Common names
  if ("common_names" %in% types) {
    xml <- xml2::read_html(json$parse$text[[1]])
    # XML formats:
    # <b>language:</b>&nbsp;[name|<a>name</a>]
    # Name formats:
    # name1, name2
    vernacular_html <- xml2::xml_find_all(xml, xpath = "(//h2/span[@id='Vernacular_names']/parent::*/following-sibling::div)[1]")
    languages_html <- xml2::xml_find_all(vernacular_html, xpath = "b")
    languages <- gsub("\\s*:\\s*", "", sapply(languages_html, xml2::xml_text))
    names_html <- xml2::xml_find_all(vernacular_html, xpath = "b[not(following-sibling::*[1][self::a])]/following-sibling::text()[1] | b/following-sibling::*[1][self::a]/text()")
    names <- gsub("^\\s*", "", sapply(names_html, xml2::xml_text))
    common_names <- mapply(list, name = names, language = languages, SIMPLIFY = FALSE, USE.NAMES = FALSE)
    result$common_names <- common_names
  }
  ## Return
  return(result)
}
