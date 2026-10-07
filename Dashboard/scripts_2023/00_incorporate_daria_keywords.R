# Incorporate Daria's manually reviewed keyword additions into the USC PWG list.
#
# Run this script from Dashboard/ before 08_run_text2sdg.R.
# Required inputs:
#   data_raw/USC_PWG-E_Keywords_11_5_24.csv
#   data_manual/SDG_keyword_additions_Daria.xlsx
#
# Output:
#   data_raw/USC_PWG-E_Keywords_with_Daria_additions.csv

library(here)
library(readxl)

base_keyword_file <- here::here(
  "data_raw",
  "USC_PWG-E_Keywords_11_5_24.csv"
)

daria_additions_file <- here::here(
  "data_manual",
  "SDG_keyword_additions_Daria.xlsx"
)

output_file <- here::here(
  "data_raw",
  "USC_PWG-E_Keywords_with_Daria_additions.csv"
)

required_columns <- c("goal", "keyword", "color")

base_keywords <- read.csv(
  base_keyword_file,
  fileEncoding = "CP1252",
  stringsAsFactors = FALSE,
  check.names = FALSE
)

daria_additions <- as.data.frame(
  readxl::read_excel(daria_additions_file),
  stringsAsFactors = FALSE
)

validate_columns <- function(data, data_name) {
  missing_columns <- setdiff(required_columns, names(data))

  if (length(missing_columns) > 0) {
    stop(
      data_name,
      " is missing required column(s): ",
      paste(missing_columns, collapse = ", ")
    )
  }
}

validate_columns(base_keywords, "Base keyword file")
validate_columns(daria_additions, "Daria additions file")

# Keep only the three fields used by the keyword pipeline and standardize types.
base_keywords <- base_keywords[required_columns]
daria_additions <- daria_additions[required_columns]

base_keywords$goal <- as.integer(base_keywords$goal)
daria_additions$goal <- as.integer(daria_additions$goal)
base_keywords$keyword <- trimws(as.character(base_keywords$keyword))
daria_additions$keyword <- trimws(as.character(daria_additions$keyword))
base_keywords$color <- trimws(as.character(base_keywords$color))
daria_additions$color <- trimws(as.character(daria_additions$color))

if (anyNA(daria_additions$goal) ||
    any(daria_additions$goal < 1 | daria_additions$goal > 17)) {
  stop("Every Daria addition must have a numeric goal from 1 through 17.")
}

if (any(is.na(daria_additions$keyword) | daria_additions$keyword == "")) {
  stop("Every Daria addition must have a non-empty keyword.")
}

# Derive each SDG's color from the most frequent value in the base list. This
# intentionally replaces blank cells and reviewer notes in Daria's color column
# (for example, "Could relate to SDG 11") with the selected goal's color. Using
# the mode also handles the four pre-existing SDG 9 rows containing #FD6926;
# the other 210 SDG 9 rows use the official #FD6925 value.
get_mode <- function(values) {
  counts <- sort(table(values), decreasing = TRUE)
  names(counts)[1]
}

color_reference <- aggregate(
  color ~ goal,
  data = base_keywords,
  FUN = get_mode
)

color_variants <- aggregate(
  color ~ goal,
  data = unique(base_keywords[c("goal", "color")]),
  FUN = length
)

goals_with_color_variants <- color_variants$goal[color_variants$color > 1]
if (length(goals_with_color_variants) > 0) {
  warning(
    "Multiple colors exist in the base list for goal(s) ",
    paste(goals_with_color_variants, collapse = ", "),
    "; using each goal's most frequent color."
  )
}

daria_additions$color <- color_reference$color[
  match(daria_additions$goal, color_reference$goal)
]

if (anyNA(daria_additions$color)) {
  missing_color_goals <- unique(daria_additions$goal[is.na(daria_additions$color)])
  stop(
    "No base-list color was found for goal(s): ",
    paste(missing_color_goals, collapse = ", ")
  )
}

# Compare goal + keyword case-insensitively and with repeated whitespace removed.
# A keyword may intentionally occur under more than one SDG, so goal is part of
# the duplicate key.
make_keyword_key <- function(goal, keyword) {
  normalized_keyword <- tolower(gsub("[[:space:]]+", " ", trimws(keyword)))
  paste(goal, normalized_keyword, sep = "|")
}

base_keys <- make_keyword_key(base_keywords$goal, base_keywords$keyword)
daria_keys <- make_keyword_key(daria_additions$goal, daria_additions$keyword)

duplicate_daria_rows <- duplicated(daria_keys)
if (any(duplicate_daria_rows)) {
  warning(sum(duplicate_daria_rows), " duplicate row(s) within Daria's file removed.")
  daria_additions <- daria_additions[!duplicate_daria_rows, , drop = FALSE]
  daria_keys <- daria_keys[!duplicate_daria_rows]
}

already_present <- daria_keys %in% base_keys
new_keywords <- daria_additions[!already_present, , drop = FALSE]

combined_keywords <- rbind(base_keywords, new_keywords)

dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)
write.csv(combined_keywords, output_file, row.names = FALSE, fileEncoding = "UTF-8")

message("Base keyword rows: ", nrow(base_keywords))
message("Daria rows supplied: ", nrow(daria_additions))
message("Daria rows already present: ", sum(already_present))
message("Daria rows added: ", nrow(new_keywords))
message("Combined keyword rows: ", nrow(combined_keywords))
message("Saved combined keyword list to: ", output_file)
