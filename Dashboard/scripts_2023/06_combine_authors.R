library(dplyr)

# --- Load data ----------------------------------------------------------------
usc_authors_2020_23_filtered <- read.csv("data_processed/05_authors_with_law.csv", na.strings = c("", "NA"))
bridge_table_2020_23 <- read.csv("data_processed/05_bridge_with_law.csv")

# Fix encoding artifact in name fields
usc_authors_2020_23_filtered$fullname <- gsub("\u2019", "'", usc_authors_2020_23_filtered$fullname)
usc_authors_2020_23_filtered$lastname <- gsub("\u2019", "'", usc_authors_2020_23_filtered$lastname)

# --- Pass 1: Merge by full first+last name, Dept, Div -------------------------
# Normalize fullname to ASCII lowercase for matching
usc_authors_2020_23_filtered$firstlast <- sapply(usc_authors_2020_23_filtered$fullname, function(x) {
  s <- strsplit(x, ",")[[1]]
  tolower(stringr::str_squish(gsub("[^A-Za-z]", " ", stringi::stri_trans_general(paste(s[2], s[1]), "Latin-ASCII"))))
})
usc_authors_n_pub <- merge(usc_authors_2020_23_filtered, bridge_table_2020_23)
# Count publications per author to keep the ID with more publications when merging
usc_authors_n_pub <- usc_authors_n_pub %>%
  group_by(authorID) %>%
  mutate(num_pubs = length(unique(pubID))) %>%
  select(-pubID, -Link) %>%
  distinct()
# Find duplicate authors (same normalized name, Dept, Div) excluding "Other" division
usc_authors_n_pub %>%
  filter(Div != "Other") %>%
  arrange(desc(num_pubs)) %>%
  group_by(firstlast, Dept, Div) %>%
  mutate(n = length(unique(authorID))) %>%
  filter(n > 1) %>%
  summarize(authorIDs = paste(unique(authorID), collapse = ";")) -> combine_these_firstlast

# Build authorID remapping: old IDs -> canonical (first/most-published) ID
new_map <- unique(combine_these_firstlast$authorIDs)
authorID_new <- sapply(new_map, function(x) strsplit(x, ";")[[1]][1])
authorID_old <- sapply(new_map, function(x) paste(strsplit(x, ";")[[1]][-1], collapse = ";"))
map_df <- data.frame(authorID_new, authorID_old) %>%
  tidyr::separate_rows(authorID_old, sep = ";") %>%
  distinct()
authorID_map <- setNames(map_df$authorID_new, map_df$authorID_old)

new_authorIDs <- sapply(usc_authors_2020_23_filtered$authorID, function(x) {
  if (as.character(x) %in% names(authorID_map)) authorID_map[as.character(x)] else x
})
usc_authors_2020_23_filtered$authorID <- unname(new_authorIDs)

# Check which rows have authorID matching their name_id (used for priority sorting)
usc_authors_2020_23_filtered$matched <- apply(usc_authors_2020_23_filtered, 1, function(x) {
  grepl(paste0("(", x[['authorID']], ")"), x[['name_id']])
})
# Consolidate rows: fill missing fields, combine affiliations, keep one row per authorID x Dept x Div
usc_authors_2020_23_filtered %>%
  arrange(desc(matched)) %>%
  group_by(authorID) %>%
  tidyr::fill(initials, FirstSearch, LastSearch, First, Last, Email, PositionTitle, Type, InUSCDirectory, .direction = "downup") %>%
  mutate(affls = paste(unique(affls), collapse = ";"),
         # might need to fill in empty columns first
         across(c(name, name_id, firstname, lastname, fullname, initials, FirstSearch, LastSearch, First, Last, Email, PositionTitle, Type, InUSCDirectory, firstlast), first)) %>%
  ungroup() %>%
  group_by(authorID, Dept, Div) %>%
  slice(1) -> usc_authors_2020_23_filtered2

# Update bridge table with remapped IDs
new_authorIDs <- sapply(bridge_table_2020_23$authorID, function(x) {
  if (as.character(x) %in% names(authorID_map)) authorID_map[as.character(x)] else x
})
bridge_table2 <- bridge_table_2020_23
bridge_table2$authorID <- unname(new_authorIDs)


# --- Pass 2: Merge by first-name-only (no middle name), last name, Dept, Div -
# Extract only the first token of the first name to ignore middle names/initials
usc_authors_2020_23_filtered2$onlyfirst <- sapply(usc_authors_2020_23_filtered2$firstname, function(x) {
  stringr::str_squish(gsub("[^A-Za-z]", " ", (strsplit(stringi::stri_trans_general(x, "Latin-ASCII"), " ")[[1]][1])))
})
usc_authors_2020_23_filtered2$loweronlyfirst <- tolower(usc_authors_2020_23_filtered2$onlyfirst)
usc_authors_2020_23_filtered2$lowerlast <- tolower(usc_authors_2020_23_filtered2$lastname)

usc_authors_n_pub <- merge(usc_authors_2020_23_filtered2, bridge_table2)
usc_authors_n_pub <- usc_authors_n_pub %>%
  group_by(authorID) %>%
  mutate(num_pubs = length(unique(pubID))) %>%
  select(-pubID, -Link) %>%
  distinct()
usc_authors_n_pub %>%
  filter(Div != "Other") %>%
  filter(onlyfirst != "") %>%
  arrange(desc(num_pubs)) %>%
  group_by(loweronlyfirst, lowerlast, Dept, Div) %>%
  mutate(n = length(unique(authorID))) %>%
  filter(n > 1) %>%
  summarize(authorIDs = paste(unique(authorID), collapse = ";")) -> combine_these_onlyfirst

new_map <- unique(combine_these_onlyfirst$authorIDs)
authorID_new <- sapply(new_map, function(x) strsplit(x, ";")[[1]][1])
authorID_old <- sapply(new_map, function(x) paste(strsplit(x, ";")[[1]][-1], collapse = ";"))
map_df <- data.frame(authorID_new, authorID_old) %>%
  tidyr::separate_rows(authorID_old, sep = ";")
authorID_map <- setNames(map_df$authorID_new, map_df$authorID_old)

new_authorIDs <- sapply(usc_authors_2020_23_filtered2$authorID, function(x) {
  if (as.character(x) %in% names(authorID_map)) authorID_map[as.character(x)] else x
})
usc_authors_2020_23_filtered2$authorID <- unname(new_authorIDs)
usc_authors_2020_23_filtered2$matched <- apply(usc_authors_2020_23_filtered2, 1, function(x) {
  grepl(paste0("(", x[['authorID']], ")"), x[['name_id']])
})
usc_authors_2020_23_filtered2 %>%
  arrange(desc(matched)) %>%
  group_by(authorID) %>%
  tidyr::fill(initials, FirstSearch, LastSearch, First, Last, Email, PositionTitle, Type, InUSCDirectory, .direction = "downup") %>%
  mutate(affls = paste(unique(affls), collapse = ";"),
         across(c(name, name_id, firstname, lastname, fullname, initials, FirstSearch, LastSearch, First, Last, Email, PositionTitle, Type, InUSCDirectory, firstlast), first)) %>%
  ungroup() %>%
  group_by(authorID, Dept, Div) %>%
  slice(1) -> usc_authors_2020_23_filtered3

new_authorIDs <- sapply(bridge_table2$authorID, function(x) {
  if (as.character(x) %in% names(authorID_map)) authorID_map[as.character(x)] else x
})
bridge_table3 <- bridge_table2
bridge_table3$authorID <- unname(new_authorIDs)


# --- Pass 3: Merge by first initial + last name, Dept, Div (manual review) ---
# Flag authors whose first name is initials only (e.g. "A" or "A.B.")
usc_authors_2020_23_filtered3$firstnameissues <- grepl("^[A-Z] ?[A-Z]? ?[A-Z]? ?$", usc_authors_2020_23_filtered3$onlyfirst)
usc_authors_2020_23_filtered3 %>%
  mutate(firstletter = substr(strsplit(onlyfirst, " ")[[1]][1], 1, 1)) -> usc_authors_2020_23_filtered3

# Extract initial-only authors and find potential full-name matches
usc_authors_2020_23_filtered3 %>%
  filter(firstnameissues == TRUE) %>%
  filter(Div != "Other") -> usc_authors_firstletter

firstletter_merge <- merge(usc_authors_firstletter, usc_authors_2020_23_filtered3 %>% filter(firstnameissues == FALSE), by = c("firstletter", "lastname", "Dept", "Div"))
firstletter_merge %>%
  group_by(authorID.x) %>%
  mutate(n_id = length(unique(authorID.y))) %>%
  filter(n_id == 1) %>%
  mutate(authorIDs = paste(authorID.y, authorID.x, sep = ";")) -> combine_these_firstletter2
# Write candidate pairs for manual review
write.csv(combine_these_firstletter2,
          row.names = FALSE,
          "data_manual/check_usc_authors_firstletter_lastname_combined.csv")

# Load manual review results and apply only confirmed merges
returned_list <- read.csv("data_manual/First_initial_Last_Name_Author_merge_corrected_JH.xlsx - check_usc_authors_firstletter_l.csv")
combine_these_firstletter <- merge(combine_these_firstletter2, returned_list,
                                    by.x = c("firstletter", "lastname", "authorID.x", "Dept", "Div"),
                                    by.y = c("firstletter", "lastname", "authorID.x", "Shared.Dept", "Shared.Div")) %>%
  filter(Combine..TRUE_FALSE.)

new_map <- unique(combine_these_firstletter$authorIDs.x)
authorID_new <- sapply(new_map, function(x) strsplit(x, ";")[[1]][1])
authorID_old <- sapply(new_map, function(x) paste(strsplit(x, ";")[[1]][-1], collapse = ";"))
map_df <- data.frame(authorID_new, authorID_old) %>%
  tidyr::separate_rows(authorID_old, sep = ";")
authorID_map <- setNames(map_df$authorID_new, map_df$authorID_old)

new_authorIDs <- sapply(usc_authors_2020_23_filtered3$authorID, function(x) {
  if (as.character(x) %in% names(authorID_map)) authorID_map[as.character(x)] else x
})
usc_authors_2020_23_filtered3$authorID <- unname(new_authorIDs)
usc_authors_2020_23_filtered3$matched <- apply(usc_authors_2020_23_filtered3, 1, function(x) {
  grepl(paste0("(", x[['authorID']], ")"), x[['name_id']])
})
usc_authors_2020_23_filtered3 %>%
  arrange(desc(matched)) %>%
  group_by(authorID) %>%
  tidyr::fill(initials, FirstSearch, LastSearch, First, Last, Email, PositionTitle, Type, InUSCDirectory, .direction = "downup") %>%
  mutate(affls = paste(unique(affls), collapse = ";"),
         across(c(name, name_id, firstname, lastname, fullname, initials, FirstSearch, LastSearch, First, Last, Email, PositionTitle, Type, InUSCDirectory, firstlast), first)) %>%
  ungroup() %>%
  group_by(authorID, Dept, Div) %>%
  slice(1) -> usc_authors_2020_23_filtered4

new_authorIDs <- sapply(bridge_table3$authorID, function(x) {
  if (as.character(x) %in% names(authorID_map)) authorID_map[as.character(x)] else x
})
bridge_table4 <- bridge_table3
bridge_table4$authorID <- unname(new_authorIDs)
bridge_table4 <- bridge_table4 %>% distinct()

write.csv(usc_authors_2020_23_filtered4,
          "data_processed/06_authors_name_merged.csv",
          row.names = FALSE)
write.csv(bridge_table4,
          "data_processed/06_bridge_name_merged.csv",
          row.names = FALSE)


# --- Pass 4: Find same-div authors where one has "Other" Dept (for inspection) -
usc_authors_n_pub <- merge(usc_authors_2020_23_filtered4, bridge_table4)
usc_authors_n_pub <- usc_authors_n_pub %>%
  group_by(authorID) %>%
  mutate(num_pubs = length(unique(pubID))) %>%
  select(-pubID, -Link) %>%
  distinct()

usc_authors_n_pub %>%
  filter(Div != "Other") %>%
  arrange(desc(num_pubs)) %>%
  group_by(firstlast, Div) %>%
  mutate(n = length(unique(authorID))) %>%
  filter(n > 1) %>%
  summarize(authorIDs = paste(unique(authorID), collapse = ";"),
            hasOther = ifelse(grepl("Other", paste(unique(Dept), collapse = ";")), TRUE, FALSE)) %>%
  filter(hasOther) -> combine_these_firstlast_other

