#' Count Google Custom Search (CS) Results
#'
#' Free up to 100/day. Maximum 10,000/day. Rate limit 1/s.
#'
#' @param string (character) Search string.
#' @param key (character) API key.
#' @param cx (character) Search engine ID.
#' @param language (character) Language code to search in.
#' @param pause (boolean) Whether to pause 1 s.
#' @export
#' @source \url{https://developers.google.com/custom-search/json-api/v1/overview}
#' @family web search functions
#' @examples
#' count_google_cs_results("'Malus domestica'+'Apfel'", "en")
#' count_google_cs_results("'Malus domestica'+'Apfel'", "de")
count_google_cs_results = function(string, key, language = NULL, pause = FALSE) {
  if (pause) {
    Sys.sleep(1.1)
  }
  url <- "https://www.googleapis.com/customsearch/v1"
  query <- list(key = key, cx = cx, q = string)
  if (!is.empty(language)) {
    if (!(language %in% Google_cs_languages)) {
      warning("Ignored unsupported language (", language, ").")
    } else {
      query <- c(query, lr = paste0("lang_", language))
    }
  }
  json <- httr::content(httr::GET(url, query = query))
  if (is.list(json) && !is.null(json$queries$request[[1]]$totalResults)) {
    return(as.integer(json$queries$request[[1]]$totalResults))
  }
}

#' List supported languages
#'
#' Lists the language codes supported by the Google Cloud Translation API.
#'
#' @param key (character) API key.
#' @source \url{https://cloud.google.com/translate/docs/discovering-supported-languages}
gct_languages <- function(key) {
  url <- "https://translation.googleapis.com/language/translate/v2/languages"
  query <- list(key = key)
  response <- httr::GET(url, query = query)
  json <- jsonlite::fromJSON(rawToChar(response$content), simplifyVector = FALSE)
  if (!is.null(json$error)) {
    stop(json$error$message)
  } else {
    sapply(json$data$languages, "[[", "language")
  }
}

#' Translate text
#'
#' Translates text strings using the Google Cloud Translation API.
#'
#' @param q (character vector) Text strings to translate.
#' @param source (character) Source language code. If \code{NULL}, it is detected automatically.
#' @param target (character) Target language code.
#' @param format (character) Format of \code{q}: "html" or "text".
#' @param key (character) API key
#' @source \url{https://cloud.google.com/translate/docs/translating-text}
gct_translate <- function(q, source = NULL, target, format = "html", key) {
  url <- "https://translation.googleapis.com/language/translate/v2"
  q_list <- as.list(q)
  names(q_list) <- rep("q", length(q))
  query <- c(q_list, target = target, key = key)
  if (!is.null(source)) {
    query <- c(query, source = source)
  }
  response <- httr::GET(url, query = query)
  json <- jsonlite::fromJSON(rawToChar(response$content), simplifyVector = FALSE)
  if (!is.null(json$error)) {
    stop(json$error$message)
  } else {
    sapply(json$data$translations, "[[", "translatedText")
  }
}
