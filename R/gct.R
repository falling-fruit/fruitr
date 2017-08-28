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
#' @param key (character) API key
#' @source \url{https://cloud.google.com/translate/docs/translating-text}
gct_translate <- function(q, source = NULL, target, key) {
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
