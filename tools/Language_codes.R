# --- Language_codes ----

Language_codes <- suppressWarnings(data.table::fread("tools/Language_codes.csv", encoding = "UTF-8", na.strings = ""))
save(Language_codes, file = "data/Language_codes.rda")

# --- Unicode ----

Unicode <- list()
files <- list.files("tools", "Unicode_.*\\.csv", full.names = TRUE)
for (file in files) {
  locale <- regmatches(file, regexec("_(.*)\\.csv", file))[[1]][2]
  Unicode[[locale]] <- read.csv(file, encoding = "UTF-8", stringsAsFactors = FALSE)[[1]]
}
zh_ambiguous <- intersect(Unicode$zh_hant, Unicode$zh_hans)
Unicode$zh_hant <- setdiff(Unicode$zh_hant, zh_ambiguous)
Unicode$zh_hans <- setdiff(Unicode$zh_hans, zh_ambiguous)
save(Unicode, file = "data/Unicode.rda")
