#' Load Locations Dataset
#'
#' TODO: Check that id field is unique, warn otherwise.
#'
#' @param file The path of the file to be read.
#' @param xy Names of the x and y coordinate fields (renamed to "lng", "lat" respectively).
#' @param id Name of the id field (renamed to "id").
#' @param CRSobj Coordinate reference system (\code{\link{sp::CRS}}).
#' @param ... Additional parameters passed to \code{\link{read.table}} or \code{\link{rgdal::readOGR}}.
#' @export
#' @family location import functions
read_locations <- function(file, xy = c("lng", "lat"), id = "id", CRSobj = CRS("+proj=longlat +ellps=WGS84"), ...) {

  # Read file
  if (grepl("\\.csv", file)) {
    df <- read.csv(file, stringsAsFactors = FALSE, na.strings = c("", "NA"), ...)
  } else if (grepl("\\.dbf", file)) {
    df <- read.dbf(file, as.is = TRUE)
  } else if (grepl("\\.shp", file)) {
    shp <- readOGR(file, layer = ogrListLayers(file)[1], ...)
    shp <- spTransform(shp, CRSobj)
    df <- shp@data
    ind <- sapply(df, is.factor)
    df[ind] <- lapply(df[ind], as.character)
    df$lng <- shp@coords[, 1]
    df$lat <- shp@coords[, 2]
  } else if (grepl("\\.kml", file)) {
    # get layer name from ogrinfo
    shp <- readOGR(file, "Features", ...)
    shp <- spTransform(shp, CRSobj)
    df <- shp@data
    ind <- sapply(df, is.factor)
    df[ind] <- lapply(df[ind], as.character)
    df$lng <- shp@coords[, 1]
    df$lat <- shp@coords[, 2]
  } else {
    df <- read.table(file, stringsAsFactors = FALSE, ...)
  }

  # Standardize name of coordinate fiels
  if (all(!is.empty(xy), xy %in% names(df))) {
    names(df)[names(df) == xy[1]] = "lng"
    names(df)[names(df) == xy[2]] = "lat"
  } else {
    warning('No coordinates available!')
  }

  # Standardize name of id field
  if (all(!is.empty(id), id %in% names(df))) {
    names(df)[names(df) == id] = "id"
  } else {
    df$id <- 1:nrow(df)
  }

  # return
  return(data.table(df, key = "id"))
}

#' Match Names Against Falling Fruit Types
#'
#' @param scientific_names Vector of scientific names.
#' @param common_names Vector of common names.
#' @param types Falling Fruit types, as returned by \code{\link{get_ff_types}}.
#' @param max_distance Maximum distance for fuzzy matching, as returned by \code{\link{stringdist::stringdistmatrix}}.
#' @param locale Locale of names to use for matching.
#' @param ... Additional parameters passed to \code{\link{stringdist::stringdistmatrix}}.
#' @export
#' @family location import functions
match_names_to_ff_types <- function(scientific_names = NULL, common_names = NULL, types = get_ff_types(pending = FALSE), max_distance = 3, n_nearest = Inf, locale = "en", ...) {

  # Prepare names
  use_scientific_names <- !is.null(scientific_names)
  if (use_scientific_names) {
    original_names <- scientific_names
  } else if (!is.null(common_names)) {
    original_names <- common_names
  } else {
    stop("No names provided.")
  }
  matched_names <- unique(original_names[!is.empty(original_names)])

  # Prepare type names
  if (use_scientific_names) {
    type_names <- types[, .(name = unlist(matched_scientific_names)), by = id]
  } else {
    if (locale == "en") {
      type_names <- types[, .(name = unlist(matched_common_names)), by = id]
    } else {
      locale_name <- paste0(locale, "_name")
      type_names <- types[, c("id", locale_name), with = FALSE]
      data.table::setnames(type_names, locale_name, "name")
    }
  }
  type_names <- type_names[!is.empty(name), ]

  # Calculate string distances
  distance_matrix <- stringdist::stringdistmatrix(matched_names, type_names$name, ...)

  # Build match results
  n_nearest <- min(nrow(type_names), n_nearest)
  matches <- lapply(seq_along(matched_names), function(i) {
    distances <- distance_matrix[i, ]
    is_real <- !is.na(distances)
    is_exact <- is_real & distances == 0
    is_fuzzy <- is_real & distances > 0 & distances < max_distance
    exact_types <- type_names[is_exact, id]
    fuzzy_types <- type_names[which(is_fuzzy)[order(distances[is_fuzzy])], id]
    list(
      rows = list(which(matched_names[i] == original_names)),
      name = matched_names[i],
      scientific = use_scientific_names,
      exact = list(exact_types),
      fuzzy = list(head(fuzzy_types, min(n_nearest, length(fuzzy_types))))
    )
  })

  # Return tabulated results
  return(data.table::rbindlist(matches))
}

#' Built Match Table from Name Matches
#'
#' TODO: Support ordered list of name fields.
#'
#' @param matches Result of \code{\link{match_names_to_ff_types}}.
#' @param dt Original data.
#' @param types Falling Fruit types, as returned by \code{\link{get_ff_types}}.
#' @param saved_table Previous result with saved edits.
#' @export
#' @family location import functions
build_match_table <- function(matches, dt, types = get_ff_types(pending = FALSE), saved_table = NULL) {

  # Prepare match strings
  exact_strings <- lapply(matches$exact, function(type_ids) {
    sapply(type_ids, function(type_id) {
      types[id == type_id, build_type_strings(id, name, scientific_name)]
    })
  })
  fuzzy_strings <- lapply(matches$fuzzy, function(type_ids) {
    sapply(type_ids, function(type_id) {
      types[id == type_id, build_type_strings(id, name, scientific_name)]
    })
  })

  # Build initial table
  # dt | types (exact_strings[1] if length = 1) | fuzzy_strings | exact_matches | rows)
  type_string <- sapply(exact_strings, function(strings) {
    ifelse(length(strings) == 1, strings, "")
  })
  fuzzy_string <- sapply(fuzzy_strings, paste, collapse = ", ")
  exact_string <- sapply(exact_strings, function(strings) {
    ifelse(length(strings) < 2, "", paste(strings, collapse = ", "))
  })
  temp <- data.table::data.table(name = matches$name, types = type_string, unverified = "", fuzzy_matches = fuzzy_string, exact_matches = exact_string)

  # Format final table
  # (merge with original data, summarize unique by count and rows)
  temp <- temp[rep(1:.N, sapply(matches$rows, length))][order(unlist(matches$rows))]
  names(dt) <- paste0("dt.", names(dt))
  merged <- cbind(dt, temp)
  match_table <- merged[, .(count = .N, rows = paste(.I, collapse = ",")), by = names(merged)][order(count, decreasing = TRUE)][]

  # Update from saved table
  if (!is.null(saved_table)) {
    if (!identical(names(match_table), names(saved_table)) || !identical(match_table$name, saved_table$name) || !identical(match_table$rows, saved_table$rows)) {
      stop("Saved table does not match structure of new results.")
    }
    saved_fields <- c("types", "unverified")
    for (field in saved_fields) {
      match_table[, field := ifelse(is.empty(saved_table[[field]]), match_table[[field]], saved_table[[field]]), with = FALSE]
    }
  }

  # Return final table
  return(match_table)
}

#' Apply Type Matches
#'
#' @export
#' @family location import functions
apply_ff_type_matches <- function(dt, types, match_table, drop = FALSE) {

  # Verify completeness
  is_empty <- sapply(match_table$types, is.empty)
  if (sum(is_empty) > 0) {
    cat("Empty match_table rows:", sep = "\n")
    cat(build_type_strings(common_names = match_table$common_name[is_empty], scientific_names = match_table$scientific_name[is_empty]), sep = "\n")
    if (!drop) {
      stop("Use drop = TRUE to ignore and drop corresponding dt rows.")
    }
  }

  # Standardize assigned types
  # TODO: Avoid unfortunate scoping?
  e <- environment()
  match_table[, types := normalize_type_strings(types, e$types)]

  # Prepare row assignments
  dt <- merge(dt, match_table[, intersect(c("scientific_name", "common_name", "types", "unverified"), names(match_table)), with = FALSE], by.x = intersect(c("printed_scientific_name", "printed_common_name"), names(dt)), by.y = intersect(c("scientific_name", "common_name"), names(match_table)))

  # Drop unassigned rows
  if (drop) {
    unassigned <- is.na(dt$types) | dt$types == "NA"
    if (sum(unassigned) > 0) {
      cat(paste("Dropping", sum(unassigned), "unassigned dt row(s)."), sep = "\n")
      dt <- dt[!unassigned]
    }
  }
  return(dt)
}

#' Aggregate Locations by Position
#' TODO: Make faster?
#' WARNING: build_location_description expects singular type strings.
#' @export
#' @family location import functions
aggregate_locations_by_position <- function(dt, sep = ". ") {
  # Select position fields
  if (all(c("lat", "lng") %in% names(dt))) {
    position_fields <- c("lat", "lng")
  } else if ("address" %in% names(dt)) {
    position_fields <- "address"
  } else {
    stop("No position fields found (lat,lng | address).")
  }

  # Apply default description field
  if (!("description" %in% names(dt))) {
    printed_common_names <- dt$printed_common_name
    printed_scientific_names <- dt$printed_scientific_name
    science_in_string <- "()"
    if (is.null(printed_common_names)) {
      science_in_string <- ""
    }
    dt[, description := build_type_strings(common_names = printed_common_names, scientific_names = printed_scientific_names, science_in = science_in_string)]
  }

  # Add missing fields
  if (!("notes" %in% names(dt))) {
    dt[, notes := Map(list, NA)]
  }
  if (!("author" %in% names(dt))) {
    dt[, author := NA]
  }
  if (!("access" %in% names(dt))) {
    dt[, access := NA]
  }

  # Convert id to character (for ifelse/paste in next step)
  if (!is.character(dt$id)) {
    dt[, id := as.character(id)]
  }

  # Aggregate by duplicated positions
  # (Multi-type locations should be split apart before being joined together here)
  fdt <- dt[, .(
    ids = ifelse(.N == 1, id, paste(unique(id), collapse = ", ")),
    types = ifelse(.N == 1, types, paste(unique(types), collapse = ", ")),
    description = build_location_description(description, notes, sep = sep),
    access = ifelse(.N == 1, access, unique_na(access)),
    author = ifelse(.N == 1, author, paste(unique(author), collapse = ", "))
  ), by = position_fields]
  return(fdt)
}

#' Write Locations to File for Import
#'
#' See http://fallingfruit.org/locations/import for format.
#' FIXME: Do not edit dt in place.
#'
#' @export
#' @family location import functions
write_locations_for_import <- function(dt, file, drop_extra_fields = TRUE) {

  # Initialize
  Location_import_fields <- c('Ids','Types','Description','Lat','Lng','Address','Season Start','Season Stop','No Season','Access','Unverified','Yield Rating','Quality Rating','Author','Photo URL')
  setnames(dt, capitalize_words(gsub("\\.|_", " ", names(dt))))
  extra_fields <- setdiff(names(dt), Location_import_fields)
  missing_fields <- setdiff(Location_import_fields, names(dt))

  # Format columns
  if (length(missing_fields) > 0) {
    dt[, (missing_fields) := NA]
  }
  setcolorder(dt, c(Location_import_fields, extra_fields))
  if (drop_extra_fields & length(extra_fields) > 0) {
    dt[, (extra_fields) := NULL]
  }

  # Write result to file
  write.csv(dt, file, na = "", row.names = FALSE)
}
