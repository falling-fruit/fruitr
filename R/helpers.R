# Object Manipulation --------------

#' Replace Values in List
#'
#' Iterates through a list and replaces values with replacements.
#'
#' @param x List object.
#' @param old List or vector of values to replace.
#' @param new List or vector of replacement values.
#' @return A list with values replaced.
#' @export
#' @family helper functions
#' @examples
#' str(x <- list(NULL, list(NULL, NA, 2)))
#' str(replace_values_in_list(x, NULL, 1))
#' str(replace_values_in_list(x, list(NULL, NA), 1))
#' str(replace_values_in_list(x, list(NULL, NA), list(1, 2)))
replace_values_in_list <- function(x, old, new) {
  if (!is.list(old) || (is.list(old) && length(old) == 0)) {
    if (length(old) > 1) old <- as.list(old)
    else old <- list(old)
  }
  if (!is.list(new) || (is.list(new) && length(new) == 0)) {
    if (length(new) > 1) new <- as.list(new)
    else new <- list(new)
  }
  new <- rep_len(new, length(old))
  for (i in seq_along(old)) {
    x <- lapply(x, function(x_i) {
      if (identical(x_i, old[[i]])) {
        new[[i]]
      } else if (is.list(x_i) && length(x_i) > 0) {
        replace_values_in_list(x_i, old[[i]], new[[i]])
      } else {
        x_i
      }
    })
  }
  return(x)
}

#' Remove Missing Values from Object
#'
#' Silently removes missing values or incomplete cases with \code{\link[base]{NA}}s.
#'
#' @param x Object passed to \code{\link[stats]{na.omit}}.
#' @return \code{x} with \code{\link[base]{NA}}s removed.
#' @export
#' @family helper functions
#' @examples
#' str(stats::na.omit(c(1, NA)))
#' str(na.remove(c(1, NA)))
na.remove <- function(x) {
  y <- stats::na.omit(x)
  attributes(y) <- NULL
  return(y)
}

#' Check if Object is Empty
#'
#' Tests whether each element of an object is (or contains only) empty values (non-existent, zero-length, "", \code{NA}, \code{NULL}).\cr
#'
#' @param x Object.
#' @return Whether each element of \code{x} is empty.
#' @export
#' @family helper functions
#' @examples
#' is.empty(list())
#' is.empty(list(1, NULL, NA, "", c()))
#' is.empty(list(1, NULL, NA, "", c(NA, NA)))
is.empty <- function(x) {
  if (missing(x)) {
    return(TRUE)
  }
  if (any(is.null(x), length(x) == 0, nrow(x) == 0)) {
    return(TRUE)
  }
  results <- sapply(x, function(x_i) {
    suppressWarnings(any(
      is.null(x_i),
      length(x_i) == 0,
      !is.null(nrow(x_i)) && nrow(x_i) == 0,
      all(is.na(x_i)),
      is.character(x_i) && all(x_i[!is.na(x_i)] == "")
    ))
  })
  return(as.vector(results))
}

#' Return Single Unique Value or NA
#'
#' Returns either \code{NA} or the single unique element if it exists.
#'
#' @param x List or atomic vector.
#' @param na.rm Whether missing values should be removed.
#' @export
#' @family helper functions
#' @examples
#' unique_na(c(1, 1))
#' unique_na(c(1, 2))
#' unique_na(c(1, NA), na.rm = FALSE)
#' unique_na(c(1, NA), na.rm = TRUE)
unique_na <- function(x, na.rm = FALSE) {
  if (na.rm) {
    x <- na.remove(x)
  }
  ux <- unique(x)
  if (length(ux) == 1) {
    return(ux)
  } else {
    return(methods::as(NA, class(x)))
  }
}

#' Melt Data Table by List Column
#'
#' Expands a list column in a data.table to an atomic vector, adding rows as necessary.
#'
#' @param dt data.table.
#' @param column Name or index of list column.
#' @return A data.table with rows added to accommodate the expansion of the list column.
#' @export
#' @family helper functions
#' @examples
#' str(dt <- data.table::data.table(label = list(list("a", "b"), list("c")), count = c(1, 2)))
#' melt_by_listcol(dt, "label")
#' melt_by_listcol(dt, 1)
melt_by_listcol <- function(dt, column) {
  values <- dt[, column, with = FALSE][[1]]
  n <- sapply(values, length)
  return(dt[rep(1:nrow(dt), n)][, c(column) := unlist(values)][])
}

#' Apply a Function over a List of Atomic Vectors
#'
#' Provides a fast, vectorized alternative to \code{\link{lapply}} for a list of atomic vectors operated on by a function whose result is the same length as its input.
#'
#' @param X List of atomic vectors.
#' @param FUN Function to be applied to each element of \code{X}.
#' @param ... Arguments passed to \code{FUN}.
#' @return Object of the same length as \code{X}.
#' @family helper functions
#' @export
#' @examples
#' x <- list(c("a", "b"), c(), c("d"))
#' lvapply(x, gsub, pattern = "d", replacement = "e")
lvapply <- function(X, FUN, ...) {
  if (!is.list(X)) {
    stop("X must be a list.")
  }
  FUN <- match.fun(FUN)
  V <- unlist(X, recursive = FALSE, use.names = FALSE)
  v <- FUN(V, ...)
  n <- sapply(X, length)
  is_empty <- n == 0
  n[n == 0] <- 1
  f <- as.factor(rep(1:length(n), n))
  f <- subset(f, f %in% which(!is_empty))
  x <- split(v, f)
  names(x) <- names(X)
  return(x)
}

# String Formatting --------------

#' Quote Regex Metacharacters
#'
#' Escape regular expression metacharacters in strings.
#'
#' @param x Character vector.
#' @return Character vector of the same length as \code{x}.
#' @family helper functions
#' @export
#' @examples
#' quotemeta("^hello world (are you there?)$")
quotemeta <- function(x) {
  stringr::str_replace_all(x, "(\\W)", "\\\\\\1")
}

#' Capitalize Words
#'
#' Capitalizes words in character strings.
#'
#' @param x Character vector.
#' @param strict Whether to lowercase characters not being capitalized.
#' @param first Whether to only capitalize the first word.
#' @return Character vector of the same length as \code{x}.
#' @export
#' @family helper functions
#' @examples
#' x <- "hello wORLd"
#' capitalize_words(x)
#' capitalize_words(x, first = TRUE)
#' capitalize_words(x, strict = TRUE)
#' capitalize_words("3-in-1 pear", first = TRUE)
capitalize_words <- function(x, strict = FALSE, first = FALSE) {
  if (strict) {
    x <- tolower(x)
  }
  if (first) {
    x <- gsub("^([^\\p{L}0-9]*)(\\p{L})", "\\1\\U\\2", x, perl = TRUE)
  } else {
    x <- gsub("(^|\\s)([^\\s\\p{L}0-9]*)(\\p{L})", "\\1\\2\\U\\3", x, perl = TRUE)
  }
  return(x)
}

#' Clean Strings
#'
#' Performs generic string cleaning operations.
#'
#' @param x Character vector.
#' @return Character vector of the same length as \code{x}.
#' @family helper functions
#' @export
#' @examples
#' clean_strings("[](){}")
#' clean_strings("``''\"\"")
#' clean_strings("..,,")
#' clean_strings(" Hello,,  `world`.. how are you?()")
clean_strings <- function(x) {
  start_x <- x
  # Non-empty substitutions
  x <- gsub("`|\\\"", "'", x, perl = TRUE)  # quotes -> '
  x <- gsub("\\s*(\\s*\\.+)+", ".", x, perl = TRUE)  # remove duplicate periods
  x <- gsub("\\s*(\\s*,+)+", ",", x, perl = TRUE)  # remove duplicate commas
  x <- gsub("([\\s,]*\\.+(\\s*,+)*)+", ".", x, perl = TRUE)  # merge punctuation
  x <- gsub("(\\s)+", " ", x, perl = TRUE)  # squish white space
  # Empty substitutions
  remove <- paste(
    "(?:^|\\s+)(NA|N\\/A)(?:$|\\s+)", # NAs in string
    "\\(\\s*\\)|\\[\\s*\\]|\\{\\s*\\}", # empty parentheses and brackets
    "'(\\s*'*\\s*)'", # empty quotes
    "^\\s+|\\s+$|\\s+(?=[\\.|,])", # trailing white space
    "^[,\\.\\s]+|[\\s,]+$", # leading punctuation, trailing commas
    "^[\\-]+|[\\-]+$", # trailing dashes
    sep = "|")
  x <- gsub(remove, "", x, perl = TRUE)
  # Iterate
  changed <- x != start_x
  changed[is.na(changed)] <- FALSE
  if (any(changed)) {
    x[changed] <- clean_strings(x[changed])
  }
  return(x)
}

#' Format Addresses
#'
#' Attempts to clean and format addresses names by the following convention:\cr
#' <Number> <SE|SW|NE|NW> <Street McName> <Ave|*>
#'
#' @param x Character vector.
#' @return Ideally, correctly formatted addresses.
#' @seealso \code{\link{clean_strings}} for generic string cleaning.
#' @family helper functions
#' @export
#' @examples
#' format_addresses("123 SE. MCDONALD AV")
format_addresses <- function(x) {
  start_x <- x
  x <- clean_strings(x)
  x <- capitalize_words(x, strict = TRUE)	# force lowercase, then capitalize each word
  x <- gsub("\\.", "", x)	# remove periods
  x <- gsub("(se|sw|ne|nw)( |$)", "\\U\\1\\2", x, perl = TRUE, ignore.case = TRUE)	# capitalize SE, SW, NE, NW
  x <- gsub("Mc([a-z])", "Mc\\U\\1", x, perl = TRUE, ignore.case = TRUE)	# restore McCaps
  x <- gsub("Av( |$)", "Ave\\1", x, ignore.case = TRUE)	# Av -> Ave
  # Iterate
  changed <- x != start_x
  changed[is.na(changed)] <- FALSE
  if (any(changed)) {
    x[changed] <- format_addresses(x[changed])
  }
  return(x)
}

#' Format Scientific Names
#'
#' Attempts to clean and format scientific names by the following convention:\cr
#' <Genus|Taxon> <species> (<subg|subsp|var|f|subvar|subf>. <subspecies name>) <Author Citation> 'Cultivar'
#'
#' @param x Character vector.
#' @param connecting_terms Whether to retain connecting terms (e.g. "subsp.", "var.").
#' @param cultivars Whether to retain cultivar names (e.g. "'Bradford'").
#' @return Ideally, correctly formatted scientific names.
#' @seealso \code{\link{clean_strings}} for generic string cleaning.
#' @family helper functions
#' @export
#' @examples
#' format_scientific_names("Malus")
#' format_scientific_names("Malus Batt.")
#' format_scientific_names("Tamarix rubella Batt.")
#' format_scientific_names("Tamarix lucronensis Sennen & Elias")
#' format_scientific_names("Malus x domestica var. gala", connecting_terms = TRUE)
#' format_scientific_names("Malus x domestica var. gala", connecting_terms = FALSE)
#' format_scientific_names("Malus pumila 'gala'", cultivars = TRUE)
#' format_scientific_names("Malus pumila 'gala'", cultivars = FALSE)
#' format_scientific_names("Prunus subg amygdalus")
#' format_scientific_names("Malus x", connecting_terms = FALSE)
format_scientific_names <- function(x, connecting_terms = TRUE, cultivars = TRUE) {
  start_x <- x
  x <- clean_strings(x)
  # Connecting terms
  x <- gsub("\\s*\u00d7\\s*", " x ", x, perl = TRUE) # space \u00D7 from other terms
  x <- gsub("\\s+[X\u00d7](\\s+|$)", " x\\1", x, perl = TRUE) # standardize hybrid (Genus \u00d7 species)
  x <- gsub("\\s+(subgenus|subg)(\\.*)(\\s+|$)", " subg.\\3", x, ignore.case = TRUE) # subgenus -> subg.
  x <- gsub("\\s+(species|spp|sp)(\\.*)(\\s+|$)", " sp.\\3", x, ignore.case = TRUE) # species -> sp.
  x <- gsub("\\s+(subspecies|ssp|sspp|subspp|subsp)(\\.*)(\\s+|$)", " subsp.\\3", x, ignore.case = TRUE) # subspecies -> subsp.
  x <- gsub("\\s+(variety|var)(\\.*)(\\s+|$)", " var.\\3", x, ignore.case = TRUE) # variety -> var.
  x <- gsub("\\s+(subvariety|subvar)(\\.*)(\\s+|$)", " subvar.\\3", x, ignore.case = TRUE) # subvariety -> subvar.
  x <- gsub("\\s+(form|forma|f)(\\.*)(\\s+|$)", " f.\\3", x, ignore.case = TRUE) # form -> f.
  x <- gsub("\\s+(subform|subf)(\\.*)(\\s+|$)", " subf.\\3", x, ignore.case = TRUE) # subform -> subf.
  x <- gsub("(auct.|auctt.)+\\s(nec|non|mult.)*", "", x, perl = TRUE) # clear auctorum notation
  # Author citations
  x <- gsub("\\s*(?<!^)(?<!(^[A-Z]{1}[a-z] ))(?<! subg. )([A-Z]{1}[a-z]+\\.*|&)(\\s*|$)+$", "", x, perl = TRUE) # clear author citations (trailing capitalized words, skipping first)
  # Case
  x <- capitalize_words(x, strict = TRUE, first = TRUE) # force lowercase, then capitalize first word
  x <- gsub("(')([a-z])([a-z])", "\\1\\U\\2\\L\\3", x, perl = TRUE)  # capitalize letter after ' if before letter
  x <- gsub("( subg. )([a-z])([a-z])", "\\1\\U\\2\\L\\3", x, perl = TRUE)  # capitalize letter after subg. if before letter
  # Removals
  if (!connecting_terms) {
    x <- gsub(" (subg|sp|subsp|var|subvar|f|subf)\\.*( |$)", "\\2", x) # remove connecting terms
    x <- gsub(" x( |$)", " ", x) # remove hybrid x
  }
  if (!cultivars) {
    x <- gsub("\\s+'.*'", "", x) # remove cultivar
  }
  # Iterate
  changed <- x != start_x
  changed[is.na(changed)] <- FALSE
  if (any(changed)) {
    x[changed] <- format_scientific_names(x[changed], connecting_terms = connecting_terms, cultivars = cultivars)
  }
  return(x)
}

#' Parse Scientific Names
#'
#' Extracts components of scientific names.
#'
#' @param x Character vector.
#' @param parts Parts of the scientific names to extract.
#' @return List of character vectors (for "taxon") and lists of the same length as \code{x}.
#' @family helper functions
#' @export
#' @examples
#' parse_scientific_names(c("Malus pumila", "Prunus persica"), parts = "taxon")
#' parse_scientific_names("Prunus persica var. nucipersica 'Crimson King'")
parse_scientific_names <- function(x, parts = c("taxon", "subtaxons", "connectors", "cultivars")) {
  results <- list()
  if ("taxon" %in% parts) {
    results$taxon <- unlist(stringr::str_extract_all(x, "^([a-zA-Z]+){1}"))
  }
  if ("subtaxons" %in% parts) {
    results$subtaxons <- lapply(stringr::str_match_all(x, "(?: ([a-zA-Z\\-]{2,})(?: |$))"), "[", i = , j = 2)
  }
  if ("connectors" %in% parts) {
    results$connectors <- lapply(stringr::str_match_all(x, "(?: (x|[a-z]+\\.{1}) )"), "[", i = , j = 2)
  }
  if ("cultivars" %in% parts) {
    results$cultivars <- lapply(stringr::str_match_all(x, " '([^']+)'"), "[", i = , j = 2)
  }
  return(results)
}

# Translations --------------

#' Normalize Language
#'
#' Returns the highest level ISO or Wikipedia language code corresponding to the ISO or Wikipedia language code, language name, or autonym provided. If the input matches zero or multiple entries, the input is returned and a warning is raised.
#'
#' @param x Character string.
#' @param types Types of strings to match against, as the corresponding column names in the \link{Language_codes} table.
#' @return Corresponding, highest-level ISO or Wikipedia language code, or \code{x} if no unique match was found.
#' @family helper functions
#' @export
#' @examples
#' normalize_language("spa")
#' normalize_language("Spanish")
#' normalize_language("Espagnol")
normalize_language = function(x, types = c("locale", "variant", "ISO639.1", "ISO639.2T", "ISO639.2B", "ISO639.3", "ISO639.6", "wikipedia", "other", "autonym", "en", "fr", "de", "ru", "es", "it", "zh")) {
  # Prepare input
  if (is.empty(x)) {
    return(NA_character_)
  }
  x <- tolower(x)
  cols <- intersect(types, names(Language_codes))
  # Prioritize code (over name) column matches if both selected
  code_cols <- c("locale", "variant", "ISO639.1", "ISO639.2T", "ISO639.2B", "ISO639.3", "ISO639.6", "wikipedia", "other")
  selected_code_cols <- intersect(cols, code_cols)
  selected_name_cols <- setdiff(cols, code_cols)
  if (length(selected_code_cols) > 0 && length(selected_name_cols) > 0) {
    n_matching_codes <- sum(sapply(selected_code_cols, function(code) {
      any(!is.na(Language_codes[[code]]) & grepl(paste0("(^|,\\s*)", quotemeta(x), "($|,)"), Language_codes[[code]]))
    }))
    if (n_matching_codes > 0) {
      cols <- selected_code_cols
    } else {
      cols <- selected_name_cols
    }
  }
  # Search and filter results
  ind <- unique(unlist(sapply(cols, function(col) {
    which(grepl(paste0("(^|,\\s*)", quotemeta(x), "($|,)"), Language_codes[[col]]))
  })))
  # # Return full result?
  # return(apply(Language_codes[ind], 1, as.list))
  if (length(ind) == 0) {
    warning(paste0("[", x, "] Language not recognized"))
    return(NA_character_)
  } else if (length(ind) > 1) {
    warning(paste0("[", x, "] Language found multiple times"))
    return(NA_character_)
  } else {
    codes <- as.character(Language_codes[ind, code_cols, with = FALSE])
    if (all(is.empty(codes))) {
      warning(paste0("[", x, "] Language does not have a supported code"))
      return(x)
    } else {
      return(codes[which(!is.na(codes))[1]])
    }
  }
}

#' Subset Search Results
#'
#' Allocate values (e.g. search results) based on subsetting of strings (e.g. search strings).
#'
#' @param strings Character vector.
#' @param values Numeric vector of the same length as \code{strings}.
#' @param ignore.case Whether to ignore case when subsetting strings.
#' @return Values, re-allocated based on the subsetting of their strings.
#' @family helper functions
#' @export
#' @examples
#' # Pine: 20, Blue pine: 17
#' strings <- c("Pine", "Blue pine")
#' values <- c(20, 17)
#' subset_search_results(strings, values) # 3, 17
#' # Pine: 20, Blue pine: 17, White blue pine: 10, White: 20
#' strings <- c("Pine", "Blue pine", "White blue pine", "White")
#' values <- c(20, 17, 10, 20)
#' subset_search_results(strings, values) # 3, 7, 10, 10
#' # Pine: 20, Blue pine: 17, Blue pine a: 10, Blue pine b: 5, Blue pine b c: 1
#' strings <- c("Pine", "Blue pine", "Blue pine a", "Blue pine b", "Blue pine b c")
#' values <- c(20, 17, 10, 5, 1)
#' subset_search_results(strings, values) # 3, 2, 10, 4, 1
#' # Pine: 20, Blue: 15, Blue pine: 10, Pine blue: 3
#' strings <- c("Pine", "Blue", "Blue pine", "Pine blue")
#' values <- c(20, 15, 10, 3)
#' subset_search_results(strings, values) # 7, 2, 10, 3
#' # Pine: 200, Blue pine: 100, White pine: 100
subset_search_results <- function(strings, values, ignore.case = TRUE) {
  subsets <- do.call("rbind", lapply(paste0(strings, " | ", strings), grepl, x = strings, ignore.case = ignore.case))
  # Proceed in order of least to most children, most to least parents
  n_children <- rowSums(subsets)
  n_parents <- colSums(subsets)
  node_sequence <- seq_len(length(strings))[order(n_children, -n_parents)]
  for (node in node_sequence) {
    # skip leaf nodes (children == 0)
    if (n_children[node] > 0) {
      is_child <- subsets[node, ]
      is_direct_child <- is_child & n_parents == min(n_parents[is_child])
      values[node] <- values[node] - sum(values[is_child])
      #values[node] <- values[node] - max(values[is_direct_child]) - sum(values[is_child & !is_direct_child])
    }
  }
  return(values)
}

# Datum Conversions --------------

#' Transform Spatial Coordinates
#'
#' @param xy Rows of coordinates (x, y).
#' @param from Initial projection as numeric (EPSG code), character (proj4 string), or \code{\link[sp]{CRS}}.
#' @param to Target projection as numeric (EPSG code), character (proj4 string), or \code{\link[sp]{CRS}}.
#' @param cols Column numbers or names specifying which xy columns are x and y coordinates.
#' @return Transformed coordinates coerced to the same class as \code{xy}.
#' @export
#' @seealso \code{\link[sp]{spTransform}}
#' @family helper functions
#' @examples
#' from <- 21781
#' to <- 4326
#' sp_transform(sp_transform(c(500, 1000), from, to), to, from)
sp_transform <- function(xy, from = NULL, to = 4326, cols = 1:2) {
  sp_from <- tryCatch(sp::proj4string(xy), error = function (e) NULL)
  is_sp <- !is.null(sp_from)
  if (is.null(from)) {
    from <- sp_from
  } else {
    if (is.numeric(from)) {
      from <- paste0("+init=epsg:", from)
    }
  }
  if (is.numeric(to)) {
    to <- paste0("+init=epsg:", to)
  }
  if (is_sp) {
    txy <- xy %>%
      sp::`proj4string<-`(from) %>%
      sp::spTransform(to)
    return(txy)
  }
  is_vector <- is.vector(xy)
  if (is_vector) {
    txy <- t(xy)
  } else {
    txy <- xy
  }
  # FIXME: Restore original class if data.table, tibble?
  txy %<>% as.data.frame()
  is_complete <- stats::complete.cases(txy[, cols])
  if (!any(is_complete)) {
    return(xy)
  }
  txy %<>%
    subset(is_complete) %>%
    sp::`coordinates<-`(cols) %>%
    sp::`proj4string<-`(from) %>%
    sp::spTransform(to)
  if (is_vector) {
    xy[cols] <- unlist(txy@coords)
  } else {
    xy[is_complete, cols] <- txy@coords
  }
  xy
}

#' Transform CH1903 to WGS84 Coordinates
#'
#' Converts Swiss Projection CH1903 projected coordinates (\href{http://spatialreference.org/ref/epsg/21781/}{EPSG:21781}) to WGS84 geographic coordinates (\href{http://spatialreference.org/ref/epsg/wgs-84/}{EPSG:4326}).
#'
#' @param xy Swiss Projection CH1903 coordinates (easting, northing).
#' @return WGS84 coordinates (longitude, latitude).
#' @family helper functions
#' @source \url{http://www.swisstopo.admin.ch/internet/swisstopo/de/home/topics/survey/sys/refsys/switzerland.parsysrelated1.24280.downloadList.87003.DownloadFile.tmp/ch1903wgs84de.pdf} (but switch x and y).
#' @export
#' @family helper functions
#' @examples
#' ch1903_to_wgs84(c(0, 1000))
ch1903_to_wgs84 <- function(xy) {
  if (is.vector(xy)) {
    xy <- t(xy[1:2])
  }
  x <- (xy[, 1] - 6e5) / 1e6
  y <- (xy[, 2] - 2e5) / 1e6
  lng <- (2.6779094 + 4.728982 * x + 0.791484 * x * y + 0.1306 * x * y^2 - 0.0436 * x^3) * (100 / 36)
  lat <- (16.9023892 + 3.238272 * y - 0.270978 * x^2 - 0.002528 * y^2 - 0.0447 * x^2 * y - 0.014 * y^3) * (100 / 36)
  return(cbind(lng, lat))
}
