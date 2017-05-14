#' Load Locations Dataset
#'
#' @param file The path of the file to be read.
#' @param xy Names of the x and y coordinate fields (renamed to "lng", "lat" respectively).
#' @param id Name of the id field (renamed to "id").
#' @param CRSobj Coordinate reference system (\code{\link{sp::CRS}}).
#' @param ... Additional parameters passed to \code{\link{data.table::fread}} (delimited files), \code{\link{rgdal::readOGR}} (spatial data), or \code{\link{xml2::read_xml}} (certain kml files).
#' @export
#' @family location import functions
read_locations <- function(file, xy = c("lng", "lat"), id = "id", CRSobj = sp::CRS("+proj=longlat +ellps=WGS84"), stringsAsFactors = FALSE, na.strings = c("", "NA", "N/A", "na", "n/a"), ...) {

  # Read file
  file <- tools::file_path_as_absolute(file)
  read_kml <- function(file, CRSobj, stringsAsFactors = FALSE, ...) {
    xml <- xml2::read_xml(file, ...)
    placemarks <- sapply(xml2::xml_find_all(xml, "//*[local-name() = 'Placemark']"), xml2::as_list)
    name <- unlist(sapply(placemarks, function(p) if (is.null(p$name)) NA else p$name))
    description <- unlist(sapply(placemarks, function(p) if (is.null(p$description)) NA else p$description))
    coordinates <- unlist(sapply(placemarks, function(p) p$Point$coordinates))
    shp <- as.data.frame(t(sapply(stringr::str_extract_all(coordinates, "([-0-9\\.]+)"), as.numeric)))
    sp::coordinates(shp) <- names(shp)[1:2]
    sp::proj4string(shp) <- sp::CRS("+proj=longlat +ellps=WGS84")
    shp <- sp::spTransform(shp, CRSobj)
    return(data.frame(name, description, lng = shp@coords[, 1], lat = shp@coords[, 2], stringsAsFactors = stringsAsFactors))
  }
  read_ogr <- function(file, CRSobj, stringsAsFactors = FALSE, ...) {
    layers <- rgdal::ogrListLayers(file)
    read_layer <- function(layer, stringsAsFactors = FALSE, ...) {
      shp <- rgdal::readOGR(file, layer, stringsAsFactors = stringsAsFactors, ...)
      shp <- sp::spTransform(shp, CRSobj)
      df <- shp@data
      df$lng <- shp@coords[, 1]
      df$lat <- shp@coords[, 2]
      df = cbind(df, layer, stringsAsFactors = stringsAsFactors)
    }
    return(Reduce(rbind, lapply(layers, read_layer, stringsAsFactors = stringsAsFactors, ...)))
  }
  df <- switch(tools::file_ext(file),
    dbf = foreign::read.dbf(file, as.is = !stringsAsFactors, ...),
    kml = if (length(rgdal::ogrListLayers(file)) > 1) read_kml(file, CRSobj, stringsAsFactors = stringsAsFactors, ...) else read_ogr(file, CRSobj, ...),
    tryCatch(data.table::fread(file, stringsAsFactors = stringsAsFactors, na.strings = na.strings, ...), error = function(e) read_ogr(file, CRSobj, stringsAsFactors = stringsAsFactors, ...))
  )

  # Standardize coordinate fields
  if (all(!is.empty(xy), xy %in% names(df))) {
    names(df)[names(df) == xy[1]] = "lng"
    names(df)[names(df) == xy[2]] = "lat"
  } else {
    warning('No coordinates available!')
  }

  # Standardize id field
  if (all(!is.empty(id), id %in% names(df))) {
    names(df)[names(df) == id] = "id"
  } else {
    df$id <- 1:nrow(df)
  }
  df$id <- as.character(df$id)
  if (any(duplicated(df$id))) {
    warning("ID field contains duplicates.")
  }

  # return
  return(data.table::data.table(df, key = "id"))
}

#' Match Names Against Falling Fruit Types
#'
#' @param names Names as a list of named vectors (or table columns). Use "scientific" for scientific names, and locale for common names (e.g. \code{list(scientific = c("malus pumila"), en = c("apple"))}).
#' @param ids Unique identifier for each row in \code{names}. If \code{NULL}, row indices are used.
#' @param simplify Method used to merge matches for each row in \code{names}. Default is \code{first}. If \code{NULL} or \code{FALSE}, all results are returned.
#' @param types Falling Fruit types, as returned by \code{\link{get_ff_types}}.
#' @param max_distance Maximum distance for fuzzy matching, as calculated by \code{\link{stringdist::stringdistmatrix}}.
#' @param n_nearest Maximum number of nearest fuzzy matches to return.
#' @param ... Additional arguments passed to \code{\link{stringdist::stringdistmatrix}}.
#' @export
#' @family location import functions
match_names_to_ff_types <- function(names, ids = NULL, simplify = c("first", "last", "union", "intersection"), types = get_ff_types(pending = FALSE), max_distance = 3, n_nearest = Inf, ...) {

  # Check names
  names <- names[!is.empty(names)]
  supported_locales <- c("cultivar", "en", gsub("_name$", "", names(types)[grepl("^[a-z_]+_name$", names(types))]))
  names <- names[intersect(names(names), supported_locales)]
  n_rows <- unique(sapply(names, length))
  if (length(n_rows) != 1 | n_rows == 0) {
    stop(paste0("Names must be a list of non-empty vectors of equal length from supported locales (", paste(supported_locales, collapse = ", "), ")"))
  }

  # Check ids
  if (is.null(ids)) {
    ids <- seq_len(n_rows)
  }

  # For each locale...
  locales <- names(names)
  name_groups <- cbind(id = ids, data.table::as.data.table(names))[, .(id = list(id)), by = locales]
  match_tables <- lapply(locales, function(locale) {

    # Prepare given names
    given_names <- name_groups[!is.empty(name_groups[[locale]]), .(id = list(unlist(id))), by = locale]
    data.table::setnames(given_names, locale, "name")

    # Prepare corresponding type names
    types_name_field <- switch(locale,
      scientific = "matched_scientific_names",
      cultivar = "matched_cultivars",
      en = "common_names",
      paste(locale, "name", sep = "_")
    )
    type_names <- types[!is.empty(types[[types_name_field]])][, .(name = unlist(.SD)), by = id, .SDcols = types_name_field]

    # Compute distance matrix
    distance_matrix <- stringdist::stringdistmatrix(tolower(given_names$name), tolower(type_names$name))

    # Build match results
    matches <- lapply(seq_len(nrow(given_names)), function(i) {
      distances <- distance_matrix[i, ]
      is_real <- !is.na(distances)
      is_exact <- is_real & distances == 0
      is_fuzzy <- is_real & distances > 0 & distances < max_distance
      is_fuzzy_sorted <- which(is_fuzzy)[order(distances[is_fuzzy])]
      exact_types <- type_names$id[is_exact]
      fuzzy_types <- type_names$id[is_fuzzy_sorted]
      fuzzy_distances <- distances[is_fuzzy_sorted]
      n_fuzzy <- min(n_nearest, length(fuzzy_types))
      list(
        exact = list(exact_types),
        fuzzy = list(head(fuzzy_types, n_fuzzy)),
        distance = list(head(fuzzy_distances, n_fuzzy))
      )
    })

    # Compile match results
    return(cbind(given_names, data.table::rbindlist(matches)))
  })

  # Return all matches
  if (is.null(simplify) || (is.logical(simplify) && !simplify)) {
    match_tables <- lapply(seq_along(match_tables), function(i) {
      match_tables[[i]][, locale := locales[i]]
    })
    return(data.table::rbindlist(match_tables))
  # Return simplified matches
  } else {
    merged <- data.table::rbindlist(match_tables)
    expanded <- melt_by_listcol(merged, "id")
    # NOTE: Strange CHARSXP errors appearing on matches, using ifelse() to avoid
    matches <- switch(simplify[1],
      first = expanded[, .(
        exact = ifelse(all(is.empty(exact)), list(integer()), exact[!is.empty(exact)][1]),
        fuzzy = ifelse(all(is.empty(fuzzy)), list(integer()), fuzzy[!is.empty(fuzzy)][1])), by = id],
      last = expanded[, .(
        exact = ifelse(all(is.empty(exact)), list(integer()), tail(exact[!is.empty(exact)], 1)),
        fuzzy = ifelse(all(is.empty(fuzzy)), list(integer()), tail(fuzzy[!is.empty(fuzzy)], 1))), by = id],
      union = expanded[, .(exact = list(Reduce(union, exact)), fuzzy = list(Reduce(union, fuzzy))), by = id],
      intersection = expanded[, .(exact = list(Reduce(intersect, exact)), fuzzy = list(Reduce(intersect, fuzzy))), by = id],
      stop(paste("Unsupported simplify:", simplify[1]))
    )
    return(matches[, .(exact, fuzzy = ifelse(all(fuzzy %in% exact), list(integer()), fuzzy[!fuzzy %in% exact])), by = id])
  }
}

#' Built Match Table from Name Matches
#'
#' @param dt Locations data.
#' @param matches Result of \code{\link{match_names_to_ff_types}} with a simplify method selected.
#' @param join_by Name of column in \code{dt} to join to \code{matches}.
#' @param group_by Name of column(s) in \code{dt} to include and group matches by in output. If specified, rows are sorted by these columns, rows are returned sorted by descending count.
#' @param types Falling Fruit types, as returned by \code{\link{get_ff_types}}.
#' @param saved_table Previous result with saved edits.
#' @param locales Additional common names displayed in match results.
#' @export
#' @family location import functions
build_match_table <- function(dt, matches, join_by = "id", group_by = NULL, types = get_ff_types(pending = FALSE), saved_table = NULL, locales = NULL) {

  # Initial match table
  # id | types (exact_strings[1] if length = 1) | fuzzy_strings | exact_matches | ...
  type_ids <- unique(unlist(matches[, .(exact, fuzzy)]))
  if (is.empty(locales)) {
    type_strings <- sapply(type_ids, function(type_id) {
      types[id == type_id, build_type_strings(id, name, scientific_name)]
    })
  } else {
    name_fields <- ifelse(locales == "en", "name", paste(locales, "name", sep = "_"))
    type_strings <- sapply(type_ids, function(type_id) {
      types[id == type_id, build_type_strings(id, name, scientific_name, paste(mapply(paste, locales, .SD, sep = ": "), collapse = ", ")), .SDcols = name_fields]
    })
  }
  selected_strings <- sapply(matches$exact, function(ids) {
    if (length(ids) != 1) "" else type_strings[match(ids, type_ids)]
  })
  exact_strings <- sapply(matches$exact, function(ids) {
    if (length(ids) < 2) "" else paste(type_strings[match(ids, type_ids)], collapse = ", ")
  })
  fuzzy_strings <- sapply(matches$fuzzy, function(ids) {
    paste(type_strings[match(ids, type_ids)], collapse = ", ")
  })
  match_temp <- data.table::data.table(id = matches$id, types = selected_strings, fuzzy_matches = fuzzy_strings, exact_matches = exact_strings)

  # Join with location data
  dt.group_by <- paste("dt", group_by, sep = ".")
  dt_subset <- dt[, union(join_by, group_by), with = FALSE]
  data.table::setnames(dt_subset, group_by, dt.group_by)
  merged <- merge(dt_subset, match_temp, by.x = join_by, by.y = "id", all = FALSE)

  # Group by grouping columns
  match_table <- merged[, .(unverified = "", count = .N, id = paste(id, collapse = ",")), by = c(dt.group_by, "types", "fuzzy_matches", "exact_matches")][order(count, decreasing = TRUE)]
  if (!is.null(group_by)) {
    data.table::setorderv(match_table, paste("dt", group_by, sep = "."))
  }

  # Update from saved table
  if (!is.null(saved_table)) {
    if (!identical(names(match_table), names(saved_table)) || !identical(match_table$name, saved_table$name) || !identical(match_table$id, saved_table$id)) {
      stop("Saved table does not match structure of new results.")
    }
    saved_fields <- c("types", "unverified")
    for (field in saved_fields) {
      match_table[, field := ifelse(is.empty(saved_table[[field]]), match_table[[field]], saved_table[[field]]), with = FALSE]
    }
  }

  # Return final table
  return(match_table[])
}

#' Apply Match Table to Locations
#'
#' @param dt Locations data.
#' @param match_table Type assignments, as returned by \code{\link{get_ff_types}}.
#' @param drop Whether to drop unassigned rows in \code{dt}.
#' @param Falling Fruit types, as returned by \code{\link{get_ff_types}}.
#' @export
#' @family location import functions
apply_match_table <- function(dt, match_table, drop = FALSE, types = get_ff_types(pending = FALSE)) {

  # Normalize (and validate) types
  match_table$types <- normalize_type_strings(match_table$types, types)

  # Verify completeness
  if (any(is.empty(match_table$types)) && !drop) {
    stop("Unassigned match_table rows (empty types field). Use drop = TRUE to drop corresponding dt rows.")
  }

  # Assign types
  id_lists <- strsplit(match_table$id, split = "\\s*,\\s*")
  temp <- match_table[rep(1:.N, sapply(id_lists, length))][, id := unlist(id_lists)]
  merged <- merge(dt, temp[, .(id, types)], by = "id")

  # Drop unassigned rows
  if (drop) {
    unassigned <- is.empty(merged$types) | merged$types == "NA"
    if (sum(unassigned) > 0) {
      cat(paste("Dropping", sum(unassigned), "unassigned dt row(s)."), sep = "\n")
      merged <- merged[!unassigned]
    }
  }
  return(merged)
}

#' Aggregate Locations by Position
#'
#' Falling Fruit does not support overlapping locations. This function merges locations with the same \code{lat}, \code{lng} or \code{address} by the methods described in Details.
#'
#' The following aggregation is applied to all non-missing values for each field:
#'
#' \itemize{
#'   \item \code{ids, types, author} - Comma-delimited list of all unique values.
#'   \item \code{description} - Result of \code{\link{build_location_descriptions}(types = description, notes = notes)}.
#'   \item \code{season.start} - Minimum value.
#'   \item \code{season.stop} - Maximum value.
#'   \item \code{unverified} - True ('x') if any are true.
#'   \item \code{yield.rating, quality.rating} - Rounded mean of all values.
#'   \item \code{photo.url} - First value.
#' }
#'
#' @param dt Locations data.
#' @param ... Arguments passed to \code{\link{build_location_descriptions}}.
#' @export
#' @family location import functions
merge_overlapping_locations <- function(dt, ...) {

  # Select position fields
  if (all(c("lat", "lng") %in% names(dt))) {
    position_fields <- c("lat", "lng")
  } else if ("address" %in% names(dt)) {
    position_fields <- "address"
  } else {
    stop("No position fields found (lat,lng | address).")
  }

  # Add missing fields
  dt <- data.table::copy(dt)
  fields <- gsub(" ", ".", tolower(Location_import_fields))
  missing_fields <- setdiff(c(fields, "notes"), names(dt))
  if (length(missing_fields) > 0) {
    dt[, (missing_fields) := NA_character_]
  }

  # Cast field types
  if (!is.list(dt$notes)) {
    dt[, notes := as.list(notes)]
  }
  if (!is.character(dt$id)) {
    dt[, id := as.character(id)]
  }

  # Merge locations by position fields
  merged <- dt[, .(
    ids = paste(na.omit(unique(id)), collapse = ", "),
    types = paste(na.omit(unique(types)), collapse = ", "),
    description = build_location_descriptions(description, notes, ...),
    # FIXME: May not work for seasons spanning two calendar years.
    season.start = if (all(is.na(season.start))) NA_integer_ else as.integer(min(season.start, na.rm = TRUE)),
    season.stop = if (all(is.na(season.stop))) NA_integer_ else as.integer(max(season.stop, na.rm = TRUE)),
    no.season = if (any(grepl("^x$|^t$|^true$", no.season, ignore.case = TRUE))) "x" else NA_character_,
    access = unique_na(access),
    unverified = if (any(grepl("^x$|^t$|^true$", unverified, ignore.case = TRUE))) "x" else NA_character_,
    yield.rating = if (all(is.na(yield.rating))) NA_integer_ else as.integer(round(mean(yield.rating, na.rm = TRUE))),
    quality.rating = if (all(is.na(quality.rating))) NA_integer_ else as.integer(round(mean(quality.rating, na.rm = TRUE))),
    author = paste(na.omit(unique(author)), collapse = ", "),
    photo.url = photo.url[1]
  ), by = position_fields]

  # Return merged locations
  return(merged)
}

#' Write Locations to File for Import
#'
#' @param dt Locations data.
#' @param file Path to file.
#' @export
#' @family location import functions
write_locations_for_import <- function(dt, file) {

  dt <- data.table::copy(dt)

  # Keep only recognized fields
  fields <- gsub(" ", ".", tolower(Location_import_fields))
  if (any(duplicated(intersect(names(dt), fields)))) {
    stop("Duplicate import field names found.")
  }
  dt <- dt[, intersect(fields, names(dt)), with = FALSE]

  # Add missing fields
  missing_fields <- setdiff(fields, names(dt))
  if (length(missing_fields) > 0) {
    dt[, (missing_fields) := NA]
    data.table::setcolorder(dt, fields)
  }

  # Rename fields
  data.table::setnames(dt, Location_import_fields[match(names(dt), fields)])

  # Write result to file
  write.csv(dt, file, na = "", row.names = FALSE)
}
