## Prepare locations for import

### Read locations from file

Load the locations as a `data.table` with unique identifiers (`id`), WGS84 coordinates (`lng`,`lat`), and any other available fields:

```R
file <- "PATH/TO/FILE"
dt <- fruitr::read_locations(file, id = "ID", xy = c("LNG", "LAT"), proj4 = "+proj=longlat +ellps=WGS84")
```

`fruitr::read_locations()` supports most delimited text files and [OGR vector formats](http://www.gdal.org/ogr_formats.html).

To get oriented, inspect the structure of the data:

```R
str(dt)
```

And inspect the values of a particular field:

```R
sort(unique(dt[["FIELD"]]))
```

### Prepare type names

Prepare any available scientific and common names for matching to Falling Fruit types:

```R
matched_common_names <- fruitr::clean_strings(dt[["COMMON_NAME"]]) # or NULL
matched_scientific_names <- fruitr::format_scientific_names(dt[["SCIENTIFIC_NAME"]], connecting_terms = FALSE, cultivars = FALSE) # or NULL
```

For scientific names, Falling Fruit follows these conventions:

  * Genus (or higher rank): `Prunus`
  * Subgenus: `Prunus subg. Amygdalus`
  * Species: `Prunus domestica`
  * Subspecies: `Prunus domestica subsp. domestica`, `Prunus persica var. nucipersica`, `Brassica oleracea var. capitata f. rubra`
  * Hybrid: `Prunus x eminens`, `Prunus cerasus x Prunus fruticosa`
  * Cultivar: `Prunus persica 'George IV'`, `Prunus domestica subsp. domestica 'Italian'`, `Acer truncatum x platanoides 'Keithsform'`

For matching, connecting terms (` x `, ` subsp. `, ...) must be removed, while cultivars (`'.*'`) can be either removed or retained. Although it should work in most cases, `fruitr::format_scientific_names()` may fail for your particular data. You should inspect the results:

```R
unique(cbind(dt[["SCIENTIFIC_NAME"]], matched_scientific_names))
```

If you have unexpected results, report the issue, edit the function and make a pull request, or build a custom solution starting from `fruitr::clean_strings()`.

### Match names to Falling Fruit types

Match names against Falling Fruit types by providing a list of named vectors, one for each locale. For example, if the names prepared above were scientific names (with connecting terms and cultivars removed: `"scientific"`) and English common names (`"en"`):

```R
ff_types <- fruitr::get_ff_types(pending = FALSE)
locales <- list("scientific" = matched_scientific_names, "en" = matched_common_names)
matches <- fruitr::match_names_to_ff_types(locales, ids = dt$id, simplify = "first", max_distance = 3, n_nearest = 2, types = ff_types)
```

For scientific names with connecting terms removed but cultivars retained, use `"cultivar"` instead of `"scientific"`.

`fruitr::match_names_to_ff_types()` returns a table of exact and fuzzy matches (to all available canonical or synonym names) as Falling Fruit type ids. To convert these to a human-readable form, use `fruitr::build_match_table()`:

```R
match_table <- fruitr::build_match_table(dt, matches, join_by = "id", group_by = c("COLUMN1", "COLUMN2"), locales = NULL, types = ff_types)
```

The result is a table with all unique combinations of the columns listed in `group_by` alongside the matches computed earlier:

| dt.COLUMN1 | dt.COLUMN2 | types | fuzzy_matches | exact_matches | unverified | count | id |
| --- | --- | --- | --- | --- | --- | --- | --- |
| Crabapple | Malus sp. | | | 114: Apple [Malus], 2194: Crabapple [Malus] | | 1 | 1 |
| Apple | Malus sp. | | | 114: Apple [Malus], 2194: Crabapple [Malus] | | 1 | 2 |
| Apple | Malus pumila | 14: Apple [Malus pumila] | | | | 1 | 3 |
| Gingko | | | 146: Ginkgo [Ginkgo biloba] | | | 1 | 4 |
| New common | New scientific | | | | | 2 | 5, 6 |
| Stone fruit (species unknown) | | | | | | 1 | 7 |
| Apple or Pear (?) | | | | | | 1 | 8 |

Where the matched Falling Fruit types are displayed using the following convention:

`<id>: <Canonical english name> [<Canonical scientific name>] {locale: name}`

### Edit type assignments

Assign groupings to Falling Fruit type(s) by entering them into the `types` column, as a comma-delimited list, following the format above (e.g. `114`, `114: Apple [Malus]`). If the type does not already exist (e.g. `New common [New scientific]`), a new type will be created on import. Tag a grouping as unverified by entering `x` into the `unverified` column. You can record new names by appending them using the format `{locale: name}` (e.g. `14: Apple [Malus pumila] {scientific: Malus paradisiaca, en: Paradise apple, Common apple}`), although this is currently unused.

As needed, reference the Falling Fruit type taxonomy at [fallingfruit.org/types](https://fallingfruit.org/types) (admin-only).

For the example above, a completed table might look like this:

| dt.COLUMN1 | dt.COLUMN2 | types | fuzzy_matches | exact_matches | unverified | count | id |
| --- | --- | --- | --- | --- | --- | --- | --- |
| Crabapple | Malus sp. | 2194 | | 114: Apple [Malus], 2194: Crabapple [Malus] | | 1 | 1 |
| Apple | Malus sp. | 114 | | 114: Apple [Malus], 2194: Crabapple [Malus] | | 1 | 2 |
| Apple | Malus pumila | 14: Apple [Malus pumila] | | | | 1 | 3 |
| Gingko | | 146 | 146: Ginkgo [Ginkgo biloba] | | | 1 | 4 |
| New common | New scientific | New common [New scientific] | | | | 2 | 5, 6 |
| Stone fruit (species unknown) | | 285: Stone fruit [Prunus] | | | | 1 | 7 |
| Apple or Pear (?) | | 114, 15: Pear [Pyrus] | | | x | 1 | 8 |

You can edit the table within R (but please save the final table to file, see below):

```R
match_table <- data.table::as.data.table(fix(match_table))
```

Or write the table to a file, edit it outside R, then read the table back into R:

```R
match_file <- gsub("(\\..*)*$", "-match_table.csv", file)
write.csv(match_table, match_file, row.names = FALSE, na = "")
match_table <- data.table::fread(match_file, stringsAsFactors = FALSE, na.strings = "")
```

### Apply type assignments

Once the match table is complete, apply the type assignments to the original data:

```R
dt <- fruitr::apply_match_table(dt, match_table, drop = FALSE, types = ff_types)
```

`fruitr::apply_match_table()` will first check the type assignments against the Falling Fruit database, then apply them if no errors are found.

### Format optional fields

#### `address`

Full address (street address, city, state/province, zip code, country), geocoded to coordinates during import. *Required for locations without coordinates* (`lng`, `lat`).

```R
dt[, address := NA_character_]
```

#### `author`

Author name, as displayed on the map.

```R
dt[, author := NA_character_]
```

#### `description`

Information displayed in the location infobox. `<br>` (HTML line breaks) are converted to `\n` (newline) characters. No other markup is currently supported. For imported data, this should be used to contain the fields used for matching to Falling Fruit types (e.g. formatted with `fruitr::build_type_strings()`) as `description` is later passed as the `types` argument in `fruitr::build_location_description()`. Additional information is added to `notes` (see below).

```R
common_names <- fruitr::clean_strings(dt[["COMMON_NAME"]]) # or NULL
scientific_names <- fruitr::clean_strings(dt[["SCIENTIFIC_NAME"]]) # or NULL
dt[, description := fruitr::build_type_strings(ids = NULL, common_names, scientific_names, science_in = "()")]
```

#### `notes`

Vector of additional strings later appended to the description by `fruitr::build_location_description()`.

```R
dt[, notes := Map(c, NA_character_)]
```

#### `access`

Access status, on a scale from 1 to 5:

  1. Source is on the author's property
  2. Author had permission from the owner to add the source
  3. Source is on public land
  4. Source is on private property but overhangs public land
  5. Source is on private property (ask before you pick)

```R
dt[, access := NA_integer_]
```

#### `season.start`, `season.stop`

Month that harvesting begins and ends, respectively, from 1 (January) to 12 (December).

```R
dt[, season.start := NA_integer_]
dt[, season.stop := NA_integer_]
```

#### `no.season`

Whether seasonality does not apply. Enter `x` if true, otherwise leave blank.

```R
dt[, no.season := NA_character_]
```

#### `yield.rating`, `quality.rating`

Yield and quality, on a scale from 1 to 5:

  1. Poor
  2. Fair
  3. Good
  4. Very Good
  5. Excellent

```R
dt[, yield.rating := NA_integer_]
dt[, quality.rating := NA_integer_]
```

#### `photo.url`

Full online path to a photo. A review is created with the photo added during import.

```R
dt[, photo.url := NA_character_]
```

### Merge overlapping locations

Since Falling Fruit does not support overlapping locations, these need to be merged together before import with `fruitr::merge_overlapping_locations()`:

```R
mdt <- fruitr::merge_overlapping_locations(dt)
```

Additional arguments are passed to `fruitr::build_location_description()`, the function responsible for constructing the final description by appending notes from single or overlapping locations. In particular, `merge` (default: `FALSE`) determines whether only the `notes` unique for all locations are preserved.

### Write file for import

Finally, format the data for import and write it to a file with `fruitr::write_locations_for_import`:

```R
out_file <- gsub("(\\..*)*$", "-IMPORT.csv", file)
fruitr::write_locations_for_import(mdt, out_file)
```

Submit the file for import at [fallingfruit.org/locations/import](https://fallingfruit.org/locations/import) (admin-only), or email the file and ancillary information (original URL and description of the dataset) directly to [ethan@fallingfruit.org](mailto:ethan@fallingfruit.org).
