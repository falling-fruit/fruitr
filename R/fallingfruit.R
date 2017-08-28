# Types --------------

#' Get Falling Fruit (FF) Types
#'
#' @param categories Categories of types to include.
#' @param uncategorized Whether to include uncategorized types.
#' @param pending Whether to include pending types.
#' @param urls Whether to include URLs.
#' @param locale Locale of the vectorized \code{common_names} field (all available name fields are returned).
#' @return A \code{\link{data.table}} of Falling Fruit types.
#' @export
#' @family Falling Fruit functions
#' @examples
#' ff_types <- get_ff_types()
get_ff_types <- function(categories = c("forager", "freegan", "honeybee", "grafter"), uncategorized = TRUE, pending = TRUE, urls = TRUE, locale = "en") {
  # Retrieve data from API
  url <- "https://fallingfruit.org/api/0.2/types.json"
  query <- list(api_key = "***REMOVED***", c = paste(intersect(Categories, categories), collapse = ","), uncategorized = ifelse(uncategorized, 1, 0), pending = ifelse(pending, 1, 0), locale = locale, urls = ifelse(urls, 1, 0))
  response <- httr::GET(url, query = query)
  # Convert JSON to data.table
  df <- jsonlite::fromJSON(rawToChar(response$content))
  dt <- data.table::as.data.table(df)
  dt[, order := .I]
  data.table::setkey(dt, id)
  # Prepare numeric and named taxonomic ranks
  dt[, taxonomic_rank_order := taxonomic_rank]
  dt[, taxonomic_rank := Taxonomic_ranks[taxonomic_rank_order + 1]]
  # Join synonyms to primary names
  dt[, scientific_names := lapply(Map(c, strsplit(scientific_name, "\\s*,\\s*"), strsplit(scientific_synonyms, "\\s*,\\s*")), na.remove)]
  if (locale == "en") {
    dt[, common_names := lapply(Map(c, strsplit(name, "\\s*,\\s*"), strsplit(synonyms, "\\s*,\\s*")), na.remove)]
  } else {
    dt[, common_names := lapply(strsplit(name, "\\s*,\\s*"), na.remove)]
  }
  # Format names for matching
  is_cultivar <- sapply(lvapply(dt$scientific_names, grepl, pattern = "'[^']+'"), any)
  dt[!is_cultivar, matched_scientific_names := lvapply(scientific_names, format_scientific_names, connecting_terms = FALSE, cultivars = FALSE)]
  dt[is_cultivar, matched_cultivars := lvapply(scientific_names, format_scientific_names, connecting_terms = FALSE, cultivars = TRUE)]
  # Return types as as data.table
  return(dt)
}

# Type Strings --------------

#' Build Type Strings
#'
#' Builds type strings from component parts.
#'
#' @param ids Integer vector.
#' @param common_names Character vector.
#' @param scientific_names Character vector.
#' @param notes Character vector.
#' @param science_in String of two characters in which to display \code{scientific_names}.
#' @return Character vector of type strings.
#' @export
#' @family Falling Fruit functions
#' @examples
#' build_type_strings(1, "Apple")
#' build_type_strings(scientific_names = "Malus domestica")
#' build_type_strings(scientific_names = "Malus domestica", science_in = "()")
#' build_type_strings(c(1, 2), c("Apple", "Pear"), c("Malus domestica", "Pyrus communis"))
#' build_type_strings(1, "Apple", "Malus pumila", "fr: Pommier commun")
build_type_strings <- function(ids = NULL, common_names = NULL, scientific_names = NULL, notes = NULL, science_in = "[]") {
  # Replace empty with blank strings
  ids[is.empty(ids)] <- ""
  common_names[is.empty(common_names)] <- ""
  scientific_names[is.empty(scientific_names)] <- ""
  notes[is.empty(notes)] <- ""
  # Build type strings
  type_strings <- clean_strings(paste0(ids, ": ", common_names, " ", substr(science_in, 1, 1), scientific_names, substr(science_in, 2, 2), " {", notes, "}"))
  type_strings <- gsub("(:\\s*$)|(^: )|(\\s*\\{\\})", "", type_strings)
  return(type_strings)
}

#' Parse Type Strings
#'
#' Parses type strings into their component parts.
#'
#' @param type_strings Character vector of type strings.
#' @return List of lists with id, name, and scientific name.
#' @export
#' @family Falling Fruit functions
#' @examples
#' parse_type_strings("14")
#' parse_type_strings("Apple [Malus pumila]")
#' parse_type_strings("14: Apple [Malus pumila]")
parse_type_strings <- function(type_strings) {
  if (length(type_strings)) {
    substrings <- stringr::str_match(type_strings, "^([0-9]+)?[:\\s]*([^\\[\\{]+?)?[\\s]*(\\[(.+)\\])?[\\s]*(\\{(.+)\\})?$")
    return(Map(list, id = as.numeric(substrings[, 2]), name = substrings[, 3], scientific_name = substrings[, 5]))
  } else {
    return(list())
  }
}

#' Match Type Strings to Types
#'
#' Returns the ids of the Falling Fruit types matching each type string.
#'
#' @param type_strings Character vector of type strings.
#' @param types Falling Fruit types.
#' @return List of Falling Fruit type ids matching each element in \code{type_strings}.
#' @export
#' @family Falling Fruit functions
#' @examples
#' ff_types <- get_ff_types()
#' match_type_strings("Apple", ff_types)
#' match_type_strings(c("Apple [Malus domestica]", "Pear [Pyrus]"), ff_types)
match_type_strings <- function(type_strings, types = get_ff_types(pending = FALSE, urls = FALSE)) {
  ts <- parse_type_strings(type_strings)
  matches <- lapply(ts, function(t) {
    types[(is.na(t$id) | id == t$id) & (is.na(t$name) | name == t$name) & (is.na(t$scientific_name) | scientific_name == t$scientific_name), id]
  })
  return(matches)
}

#' Normalize Type Strings
#'
#' Checks type strings against Falling Fruit types and fills in any missing components.
#'
#' @param type_strings Character vector of type strings as comma-delimited lists.
#' @param types Falling Fruit types
#' @return Character vector of complete type strings as comma-delimited lists.
#' @export
#' @family Falling Fruit functions
#' @examples
#' ff_types <- get_ff_types()
#' \dontrun{
#' normalize_type_strings("Apple", ff_types)
#' }
#' normalize_type_strings(c("14", "Apple [Malus]"), ff_types)
#' normalize_type_strings(c("", " ,", NA), ff_types)
#' normalize_type_strings(c("14: Apple, 14: Apple"), ff_types)
#' normalize_type_strings("Hello World", ff_types)
normalize_type_strings <- function(type_strings, types = get_ff_types(pending = FALSE, urls = FALSE)) {
  # Strip notes
  type_strings <- gsub("\\s*\\{.*\\}", "", type_strings)
  # Verify type strings
  matched_type_strings <- unique(unlist(strsplit(type_strings, "\\s*,\\s*")))
  matched_type_strings <- matched_type_strings[!is.empty(matched_type_strings)]
  if (length(matched_type_strings) < 1) return(rep(NA, length(type_strings)))
  matches <- match_type_strings(matched_type_strings, types)
  n_matches <- sapply(matches, length)
  has_id <- !is.na(sapply(parse_type_strings(matched_type_strings), "[[", "id"))
  has_no_matches <- n_matches == 0
  has_many_matches <- n_matches > 1
  is_invalid <- (has_no_matches & has_id) | has_many_matches
  if (sum(has_no_matches & !has_id) > 0) {
    cat("New types:", sep = "\n")
    cat(matched_type_strings[has_no_matches & !has_id], sep = "\n")
  }
  if (sum(has_no_matches & has_id) > 0) {
    cat("Unrecognized type strings with id:", sep = "\n")
    cat(matched_type_strings[has_no_matches & has_id], sep = "\n")
  }
  if (sum(has_many_matches) > 0) {
    cat("Ambiguous type strings:", sep = "\n")
    cat(paste(matched_type_strings[has_many_matches], "-> Matches", n_matches[has_many_matches], "types"), sep = "\n")
  }
  if (sum(is_invalid) > 0) {
    stop("Invalid type strings found.")
  }
  # Standardize type strings
  ids <- unlist(matches[n_matches == 1])
  if (length(ids) > 0) {
    old_strings <- paste0("(^|,\\s*)", quotemeta(matched_type_strings[n_matches == 1]), "\\s*(,|$)")
    new_strings <- paste0("\\1", build_type_strings(ids, types[.(ids), name], types[.(ids), scientific_name]), "\\2")
    names(new_strings) <- old_strings
    return(stringr::str_replace_all(type_strings, new_strings))
  } else {
    return(type_strings)
  }
}

# Locations --------------

#' Build Location Description
#'
#' Builds a description from its inputs. When \code{merge} is \code{TRUE}, all content is summarized as one group, and all notes not equal throughout are discarded. Otherwise, all content is preserved and only grouped for equal types with all equal notes.
#'
#' @param type_strings Character vector interpreted literally (comma-delimited lists are not split).
#' @param notes List of character vectors, each of the same length.
#' @param merge Whether to merge types, discarding any notes that are not equal for all types.
#' @param type_sep Character string to seperate each element in \code{type_strings} when \code{merge = TRUE}.
#' @param note_sep Character string to seperate each retained element in \code{notes}.
#' @param group_sep Character string to seperate each grouping when \code{merge = FALSE}.
#' @param frequency Whether to display frequency before each element in \code{type_strings}.
#' @param frequency_in String of two characters in which to display the frequencies.
#' @return Character string of the summarized and concatenated values.
#' @export
#' @family Falling Fruit functions
#' @examples
#' type_strings <- c("Apple", "Pear", "Pear")
#' notes <- list(
#'   c("Planted 1999", "Height 10 m"),
#'   c("Planted 1999", "Height 20 m"),
#'   c("Planted 1999", "Height 20 m")
#' )
#' build_location_description(type_strings, notes)
#' build_location_description(type_strings, notes, merge = TRUE)
build_location_description <- function(type_strings, notes = NULL, merge = FALSE, type_sep = ", ", note_sep = ". ", group_sep = "<br>", frequency = TRUE, frequency_in = "[]") {
  if (merge || is.null(notes)) {
    frequencies <- summary(as.factor(unlist(type_strings)))
    if (frequency) {
      description <- paste0(substr(frequency_in, 1, 1), frequencies, "x", substr(frequency_in, 2, 2), " ", attr(frequencies, "names"), collapse = type_sep)
    } else {
      description <- paste0(attr(frequencies, "names"), collapse = type_sep)
    }
    notes <- lapply(do.call(Map, c(base::c, notes)), unique_na)
    notes <- notes[!is.empty(notes)]
    if (length(notes) > 0) {
      description <- paste0(paste(description, paste(notes, collapse = note_sep), sep = note_sep), gsub("\\s*$", "", note_sep))
    }
  } else {
    types <- unique(unlist(type_strings))
    descriptions <- lapply(types, function(type) {
      i_type <- type_strings == type
      if (sum(i_type) == 1) {
        build_location_description(type_strings[i_type], notes[i_type], merge = TRUE, type_sep = type_sep, note_sep = note_sep, frequency = frequency, frequency_in = frequency_in)
      } else {
        note_groups <- unique(notes[i_type])
        temp <- lapply(note_groups, function(note) {
          i_note <- sapply(notes[i_type], function(n) identical(n, note))
          build_location_description(type_strings[i_type][i_note], notes[i_type][i_note], merge = TRUE, type_sep = type_sep, note_sep = note_sep, frequency = frequency, frequency_in = frequency_in)
        })
        paste(temp, collapse = group_sep)
      }
    })
    description <- paste(descriptions, collapse = group_sep)
  }
  return(description)
}

#' Build Location Descriptions
#'
#' A faster, vectorized alternative of \code{\link{build_location_description}} for single-type locations.
#'
#' @param type_strings Character vector of type strings.
#' @param notes List of character vectors, each of the same length.
#' @param note_sep Character string to seperate each element in \code{notes}.
#' @param frequency Whether to display frequency before \code{type_strings}.
#' @param frequency_in String of two characters in which to display the frequencies.
#' @return Character string of the summarized and concatenated values.
#' @export
#' @family Falling Fruit functions
#' @examples
#' type_strings <- c("Apple", "Pear", "Pear")
#' notes <- list(
#'   c("Planted 1999", "Height 10 m"),
#'   c("Planted 1999", "Height 20 m"),
#'   c("Planted 1999", "Height 20 m")
#' )
#' build_location_descriptions(type_strings, notes)
#' notes <- list(c("Planted 1999", NA), c(NA, "Height 20 m"), c(NA, NA))
#' build_location_descriptions(type_strings, notes)
#' build_location_descriptions(type_strings, notes, frequency = FALSE)
build_location_descriptions <- function(type_strings, notes = NULL, note_sep = ". ", frequency = TRUE, frequency_in = "[]") {
  descriptions <- type_strings
  if (frequency) {
    descriptions <- paste0(substr(frequency_in, 1, 1), 1, "x", substr(frequency_in, 2, 2), " ", descriptions)
  }
  if (!is.null(notes) && length(notes) > 1) {
    if (is.list(notes[[1]])) {
      notes <- unlist(notes, recursive = FALSE)
    }
    note_strings <- sapply(notes, function(note) {
      paste(stats::na.omit(note), collapse = note_sep)
    })
    has_note <- note_strings != ""
    descriptions[has_note] <- paste0(paste(descriptions[has_note], note_strings[has_note], sep = ". "), gsub("\\s*$", "", note_sep))
  }
  return(descriptions)
}

# Categories --------------

#' Expand Category Mask to Categories
#'
#' @param mask Integer represention of a binary category mask.
#' @return Vector of category names.
#' @export
#' @family Falling Fruit functions
#' @examples
#' expand_category_mask(0)
#' expand_category_mask(3)
expand_category_mask <- function(mask) {
  Categories[which(as.numeric(intToBits(mask)) == 1)]
}
