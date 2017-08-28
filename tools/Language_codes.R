Language_codes <- suppressWarnings(data.table::fread("tools/Language_codes.csv", encoding = "UTF-8", na.strings = ""))
save(Language_codes, file = "data/Language_codes.rda")
