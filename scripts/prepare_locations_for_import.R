# ---- Initialize ----

# Read locations from file
file <- "PATH/TO/FILE"
dt <- fruitr::read_locations(file, id = "ID", xy = c("LNG", "LAT"), CRSobj = CRS("+proj=longlat +ellps=WGS84"))

# ---- Find matches ----

# Initialize empty match table
match_table <- NULL

# Prepare names for matching
matched_scientific_names <- fruitr::format_strings(dt[["SCIENTIFIC_NAME"]], "matched_scientific_name") # or NULL
matched_common_names <- fruitr::format_strings(dt[["COMMON_NAME"]], "matched_common_name") # or NULL

# Match names against Falling Fruit types (repeat as needed)
ff_types <- fruitr::get_ff_types(pending = FALSE)
matches <- fruitr::match_names_to_ff_types(names = list("scientific" = matched_scientific_names, "en" = matched_common_names), ids = dt$id, simplify = "first", max_distance = 3, n_nearest = 2, types = ff_types)
match_table <- fruitr::build_match_table(dt, matches, join_by = "id", group_by = c("GROUP", "COLUMNS"), saved_table = match_table, types = ff_types, locales = NULL)

# ---- Write match table ----

match_file <- gsub("(\\..*)*$", "-match_table.csv", file)
write.csv(match_table, match_file, row.names = FALSE)

# ---- Edit match table ----

# Assign grouping to Falling Fruit type(s) by entering type string(s) into the "types" column, making note of new common names ("114", "114: Apple [Malus]", "Apple [Malus] {fr: Pommier}"). If the type does not already exist, a new type will be created on import. Tag a grouping as unverified by entering 'x' into the "unverified" column.
match_table <- data.table::as.data.table(fix(match_table))[, types := fruitr::normalize_type_strings(types, ff_types)]

# ---- Write / Read match table ----

match_table <- data.table::as.data.table(read.csv(match_file, stringsAsFactors = FALSE))[, types := fruitr::normalize_type_strings(types, ff_types)]

# ---- Apply matches ----

dt <- fruitr::apply_match_table(dt, match_table, drop = FALSE)

# ---- Additional fields ----

## Address (required if lng, lat are not defined)
# Full address (street address, city, state/province, zip code, country), geocoded to coordinates during import.
dt[, address := NA]

## Author (optional)
# Author name, as displayed on the map.
dt[, author := NA]

## Description (optional)
# Information displayed in the infobox. <br> (HTML line breaks) are converted to \n (newline) characters. No other markup is currently supported. The intended use for import is type strings of the original data used for matching to Falling Fruit types. Additional information is added to notes (see below).
common_names <- fruitr::clean_strings(dt[["COMMON_NAME"]]) # or NULL
scientific_names <- fruitr::clean_strings(dt[["SCIENTIFIC_NAME"]]) # or NULL
dt[, description := fruitr::build_type_strings(common_names, scientific_names, science_in = "[]")]

## Notes (optional)
# Vector of strings which are appended to the description by fruitr::build_location_description(). When overlapping locations are merged, the resulting description is: "[nx] type string, [yx] type string, ... + sep + notes (those unique and equal for all)"
dt[, notes := Map(list, NA)]

## Access (optional)
# Access status, on a scale from 1 to 5:
# 1: Source is on the author's property
# 2: Author had permission from the owner to add the source
# 3: Source is on public land
# 4: Source is on private property but overhangs public land
# 5: Source is on private property (ask before you pick)
dt[, access := NA]

## Season Start/Stop (optional)
# Months that harvesting begins and ends, respectively, from 1 (January) to 12 (December).
dt[, season.start := NA]
dt[, season.stop := NA]

## No Season (optional)
# Whether seasonality does not apply. Mark 'x', 't', or 'true' if true, otherwise leave blank.
dt[, no.season := NA]

## Yield/Quality Rating (optional)
# Productivity and tastiness, on a scale from 1 to 5:
# 1: Poor
# 2: Fair
# 3: Good
# 4: Very Good
# 5: Excellent
dt[, yield.rating := NA]
dt[, quality.rating := NA]

## Photo URL (optional)
# Full path to a photo. A review is created with the photo added during import.
dt[, photo.url := NA]

# ---- Aggregate overlapping locations ----

mdt <- fruitr::merge_overlapping_locations(dt, frequency = TRUE, note_sep = ". ")

# ---- Write file for import ----

out_file <- file.path(directory, gsub("(\\..*)*$", "-FINAL.csv", file))
fruitr::write_locations_for_import(mdt, out_file)
