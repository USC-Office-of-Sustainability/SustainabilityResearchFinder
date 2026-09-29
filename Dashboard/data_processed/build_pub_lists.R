# Adds a "publications" column containing a semicolon-separated list
# of paper titles to the Category 1 and Category 2 duplicate-author CSVs.
#
# USAGE:
# 1. Put this script in the same folder as:
#    - 01_all_usc_pubs.csv
#    - category1_same_division_duplicates.csv
#    - category2_other_remaining_duplicates.csv
# 2. Run the script in R.
# 3. Two new files will be created in the same folder:
#    - category1_same_division_duplicates_with_pubs.csv
#    - category2_other_remaining_duplicates_with_pubs.csv
#
# This version uses base R, so no additional packages are required.


# --- File paths ---

PUBS_FILE <- "01_all_usc_pubs.csv"
CAT1_FILE <- "category1_same_division_duplicates.csv"
CAT2_FILE <- "category2_other_remaining_duplicates.csv"

CAT1_OUT <- "category1_same_division_duplicates_with_pubs.csv"
CAT2_OUT <- "category2_other_remaining_duplicates_with_pubs.csv"


# --- Load publication-level data ---

pubs <- read.csv(
  PUBS_FILE,
  stringsAsFactors = FALSE,
  check.names = FALSE
)


# --- Build authorID -> list of titles mapping ---

author_titles <- list()

for (row_number in seq_len(nrow(pubs))) {
  
  ids_raw <- pubs[["Author.s..ID"]][row_number]
  title <- pubs[["Titles"]][row_number]
  
  if (is.na(ids_raw) || is.na(title)) {
    next
  }
  
  ids <- trimws(strsplit(as.character(ids_raw), ";", fixed = TRUE)[[1]])
  ids <- ids[ids != ""]
  
  for (author_id in ids) {
    author_titles[[author_id]] <- c(
      author_titles[[author_id]],
      trimws(as.character(title))
    )
  }
}

cat(
  "Total unique author IDs with publication titles:",
  length(author_titles),
  "\n"
)


# --- Look up publications for one author ID ---

get_pub_string <- function(author_id) {
  
  if (is.na(author_id)) {
    return("")
  }
  
  # Convert IDs such as 12345 or 12345.0 into "12345"
  normalized_id <- format(
    as.numeric(author_id),
    scientific = FALSE,
    trim = TRUE
  )
  
  titles <- author_titles[[normalized_id]]
  
  if (is.null(titles)) {
    return("")
  }
  
  paste(titles, collapse = "; ")
}


# --- Process one category file ---

process_category <- function(input_file, output_file, label) {
  
  duplicate_authors <- read.csv(
    input_file,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  
  duplicate_authors$publications <- vapply(
    duplicate_authors$authorID,
    get_pub_string,
    character(1)
  )
  
  missing <- duplicate_authors[
    duplicate_authors$publications == "",
    ,
    drop = FALSE
  ]
  
  cat(
    label, ": ",
    nrow(duplicate_authors), " rows, ",
    nrow(missing), " with no publications found\n",
    sep = ""
  )
  
  if (nrow(missing) > 0) {
    possible_columns <- c(
      "firstname",
      "lastname",
      "authorID",
      "num_pubs"
    )
    
    columns_to_show <- possible_columns[
      possible_columns %in% names(missing)
    ]
    
    print(
      missing[, columns_to_show, drop = FALSE],
      row.names = FALSE
    )
  }
  
  write.csv(
    duplicate_authors,
    output_file,
    row.names = FALSE,
    na = ""
  )
  
  cat("  -> saved to ", output_file, "\n\n", sep = "")
}


# --- Create the two output files ---

process_category(
  CAT1_FILE,
  CAT1_OUT,
  "Category 1"
)

process_category(
  CAT2_FILE,
  CAT2_OUT,
  "Category 2"
)