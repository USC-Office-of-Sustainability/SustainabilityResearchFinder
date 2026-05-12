library(dplyr)

# --- Config -------------------------------------------------------------------
# To add a new year of law publications, append one entry here.
LAW_FILES <- c(
  "data_raw/USC_Law_2023.csv",
  "data_raw/USC_Law_2024.csv",
  "data_raw/USC_Law_2025.csv"
)
# ------------------------------------------------------------------------------

usc_authors_base <- read.csv("data_processed/04_authors_combined_historical.csv")
bridge_base      <- read.csv("data_processed/04_bridge_combined_historical.csv")
usc_pubs_base    <- read.csv("data_processed/01_all_usc_pubs.csv")

law_all_years <- lapply(LAW_FILES, read.csv)
cat("Law files loaded:", paste(basename(LAW_FILES), collapse = ", "), "\n")

law_all_combined <- do.call(rbind, law_all_years) %>%
  filter(Year >= 2020)

# --- Identify law pubs not already in the publications table -----------------
# Dedup by normalized Title + Year (case-insensitive, stripped)
existing_keys <- paste(tolower(trimws(usc_pubs_base$Titles)), usc_pubs_base$Year, sep = "__")

law_pubs_to_add <- law_all_combined %>%
  mutate(.dedup_key = paste(tolower(trimws(Title)), Year, sep = "__")) %>%
  filter(!(.dedup_key %in% existing_keys)) %>%
  select(-.dedup_key)

# Summary: which law pubs were added vs. skipped
law_added <- law_pubs_to_add

law_not_added <- law_all_combined %>%
  mutate(.dedup_key = paste(tolower(trimws(Title)), Year, sep = "__")) %>%
  filter(.dedup_key %in% existing_keys) %>%
  select(-.dedup_key)

cat("Law pubs added:", nrow(law_added), "\n")
cat("Law pubs skipped (already exist):", nrow(law_not_added), "\n")

# Assign new pubIDs continuing from the current max
starting_pubID <- max(usc_pubs_base$pubID) + 1
ending_pubID   <- starting_pubID + nrow(law_pubs_to_add) - 1
missing_pubIDs <- seq(from = starting_pubID, to = ending_pubID, by = 1)
law_pubs_to_add$LawPubID <- missing_pubIDs

# law info: used to assign authorIDs manually into the spreadsheet
law_authorID <- merge(law_all_combined, usc_authors_base, by.x = c("USC.Author_Last_First"), by.y = c("fullname"))

# --- Build law authors table -------------------------------------------------
law_authors <- law_pubs_to_add %>%
  rename(pubID = LawPubID,
         Document.Type = Publication.Type..Article.or.Book..,
         fullname = USC.Author_Last_First,
         Dept = Focal.USC.Author.Department,
         Div = Focal.USC.Author.Division.School,
         authorID = Author.s..ID,
         Titles = Title) %>%
  select(-Authors_dont_use, -Author.full.names) %>%
  mutate(name_id = paste0(fullname, " (", authorID, ")"))
law_authors$Div <- trimws(gsub("USC", "", law_authors$Div))
law_authors$firstname <- ""
law_authors$lastname  <- ""
for (i in 1:nrow(law_authors)) {
  law_authors$firstname[i] <- trimws(strsplit(law_authors$fullname[i], ",")[[1]][2])
  law_authors$lastname[i]  <- trimws(strsplit(law_authors$fullname[i], ",")[[1]][1])
}
law_authors$initials <- sapply(law_authors$firstname, function(x) {
  paste0(paste(substr(strsplit(x, "-| ")[[1]], 1, 1), collapse = "."), ".")
})
law_authors$name <- paste0(law_authors$lastname, " ", law_authors$initials)

# --- Build bridge, pubs, and authors tables ----------------------------------
law_bridge <- law_authors %>%
  select(pubID, Link, authorID)

empty_cols <- setdiff(names(usc_pubs_base), names(law_authors))
law_authors[, empty_cols] <- ""
law_pubs <- law_authors %>%
  select(names(usc_pubs_base)) %>%
  distinct()

empty_cols <- setdiff(names(usc_authors_base), names(law_authors))
law_authors[, empty_cols] <- ""
law_authors2 <- law_authors %>%
  select(names(usc_authors_base)) %>%
  distinct()

usc_pubs_law <- rbind(usc_pubs_base, law_pubs)
write.csv(usc_pubs_law,
          "data_processed/05_pubs_with_law.csv",
          row.names = FALSE)

usc_authors_law <- rbind(usc_authors_base, law_authors2)
# Normalize law department names: all "Law*" variants -> "Law"
usc_authors_law %>%
  mutate(Dept = ifelse(grepl("^Law", Dept) & Dept != "Law Immigration Clinic", "Law", Dept)) %>%
  mutate(Dept = ifelse(Div == "Gould School of Law" & Dept == "Other", "Law", Dept)) -> usc_authors_law
write.csv(usc_authors_law,
          "data_processed/05_authors_with_law.csv",
          row.names = FALSE)

usc_bridge_law <- rbind(bridge_base, law_bridge)
write.csv(usc_bridge_law,
          "data_processed/05_bridge_with_law.csv",
          row.names = FALSE)
