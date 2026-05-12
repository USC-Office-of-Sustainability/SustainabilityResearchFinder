library(dplyr)

# ==============================================================================
# 01_read_data.R
# Read Scopus exports from multiple years, unify column names, merge, deduplicate
# by Link (keeping earlier year), filter out non-research document types,
# assign unique pubIDs, drop unused columns, and save.
#
# MAINTENANCE: To add a new year, just append one line to NEW_YEAR_FILES below.
# ==============================================================================

# ┌─────────────────────────────────────────────────────────────────────────────┐
# │  CONFIG — edit this section only                                           │
# └─────────────────────────────────────────────────────────────────────────────┘

# Each entry: file path + which year to extract from that file.
# Order matters: earlier entries take priority when the same Link appears.
NEW_YEAR_FILES <- list(
  list(path = "data_raw/2023_scopus_downloaded_03_08_2024.csv", year = 2023),
  list(path = "data_raw/2024_scopus_downloaded_01_01_2025.csv", year = 2024),
  list(path = "data_raw/2025_scopus_downloaded_02_10_2026.csv", year = 2025)
)

EXCLUDED_DOC_TYPES <- c("Letter", "Retracted", "Note", "Erratum")

# Columns actually used by downstream scripts (02-13) + shiny app
KEEP_COLS <- c(
  "pubID", "Authors", "Author.full.names", "Author.s..ID", "Titles", "Year",
  "Source.title", "Volume", "Issue", "Art..No.", "Page.start", "Page.end",
  "Page.count", "DOI", "Cited.by", "Link", "Affiliations",
  "Authors.with.affiliations", "Abstract", "Indexed.Keywords",
  "Author.Keywords", "Publisher", "Document.Type", "Publication.Stage",
  "Open.Access", "Source", "EID"
)

# ┌─────────────────────────────────────────────────────────────────────────────┐
# │  LOGIC — no need to touch below                                            │
# └─────────────────────────────────────────────────────────────────────────────┘

VALID_YEARS <- 2020:max(sapply(NEW_YEAR_FILES, `[[`, "year"))

# Standardize column names across different Scopus export formats
rename_scopus <- function(df) {
  if ("Title" %in% names(df))          df <- rename(df, Titles = Title)
  if ("Index.Keywords" %in% names(df)) df <- rename(df, Indexed.Keywords = Index.Keywords)
  if ("X" %in% names(df))              df <- select(df, -X)
  df
}

# Merge new year into existing data; if same Link exists, keep the older row
add_year <- function(existing, new_df, target_year) {
  new_rows <- new_df %>% filter(Year == target_year)
  bind_rows(existing, new_rows) %>%
    group_by(Link) %>%
    filter(!(n() > 1 & Year == target_year)) %>%
    ungroup()
}

# --- Build base: historical 2020-2022 ----------------------------------------
previous_pubs <- read.csv("data_processed/all_usc_pubs_2020_2021_2022.csv")
missing_pubs  <- read.csv("data_raw/MissingScopus_pubs_from_2020_2021_2022.csv")

all_pubs <- bind_rows(previous_pubs, rename_scopus(missing_pubs)) %>%
  filter(Year %in% 2020:2022)

# --- Incrementally add each new year ------------------------------------------
for (entry in NEW_YEAR_FILES) {
  new_df <- rename_scopus(read.csv(entry$path))
  all_pubs <- add_year(all_pubs, new_df, entry$year)
}

# --- Filter document types and year range -------------------------------------
final_pubs <- all_pubs %>%
  filter(Year %in% VALID_YEARS,
         !Document.Type %in% EXCLUDED_DOC_TYPES) %>%
  distinct()

# --- Assign pubIDs ------------------------------------------------------------
# 1) Propagate existing pubIDs within same EID (from previous_pubs)
# 2) For duplicates within (pubID, EID), keep the last row (most recent data)
# 3) Assign new sequential pubIDs starting from max+1 for any remaining NAs
final_pubs <- final_pubs %>%
  group_by(EID) %>%
  tidyr::fill(pubID, .direction = "downup") %>%
  ungroup() %>%
  group_by(pubID, EID) %>%
  slice_tail(n = 1) %>%
  ungroup()

n_missing <- sum(is.na(final_pubs$pubID))
if (n_missing > 0) {
  start_id <- max(final_pubs$pubID, na.rm = TRUE) + 1
  final_pubs$pubID[is.na(final_pubs$pubID)] <- seq(start_id, length.out = n_missing)
}

# --- Validate -----------------------------------------------------------------
stopifnot("Duplicate EIDs found"   = !any(duplicated(final_pubs$EID)))
stopifnot("Duplicate pubIDs found" = !any(duplicated(final_pubs$pubID)))

# --- Keep only needed columns and save ----------------------------------------
for (col in KEEP_COLS) {
  if (!col %in% names(final_pubs)) final_pubs[[col]] <- NA
}
final_pubs <- final_pubs %>% select(all_of(KEEP_COLS))

write.csv(final_pubs,
          file = here::here("data_processed/01_all_usc_pubs.csv"),
          row.names = FALSE)

cat("=== Publications per year ===\n")
print(table(final_pubs$Year))
cat("Total:", nrow(final_pubs), "\n")
