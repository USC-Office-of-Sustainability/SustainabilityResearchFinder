library(dplyr)

usc_authors <- read.csv("data_processed/06_authors_name_merged.csv")
usc_bridge <- read.csv("data_processed/06_bridge_name_merged.csv")

# --- One-off authorID correction for Wändi Bruine de Bruin ------------------
usc_authors <- usc_authors[-which(usc_authors$authorID == "58298252500"), ]
usc_bridge$authorID[which(usc_bridge$authorID == "58298252500")] <- 7004764268

# --- Merge incorrect "de Smith, Adam J." record into "Smith, Adam J." -------

adam_smith_old_id <- "16416776100"
adam_smith_new_id <- "58487361600"

# Get the correct name information before changing the old author ID
adam_smith_correct <- usc_authors %>%
  filter(as.character(authorID) == adam_smith_new_id) %>%
  slice(1)

adam_smith_old_rows <- which(
  as.character(usc_authors$authorID) == adam_smith_old_id
)

# Move the old author's affiliation rows to the correct author ID
usc_authors$authorID[adam_smith_old_rows] <- adam_smith_new_id

# Replace the incorrect name fields while retaining all affiliations
adam_smith_name_columns <- intersect(
  c(
    "name",
    "name_id",
    "firstname",
    "lastname",
    "fullname",
    "initials",
    "FirstSearch",
    "LastSearch",
    "First",
    "Last"
  ),
  names(usc_authors)
)

for (column_name in adam_smith_name_columns) {
  usc_authors[adam_smith_old_rows, column_name] <-
    adam_smith_correct[[column_name]][1]
}

# Move all publications from the old ID to the correct ID
usc_bridge$authorID[
  as.character(usc_bridge$authorID) == adam_smith_old_id
] <- adam_smith_new_id

# Remove duplicate rows created by the merge
usc_authors <- usc_authors %>% distinct()
usc_bridge <- usc_bridge %>% distinct()

# --- Merge duplicate Gustavo Adolpho Lucas de Carvalho author IDs -----------

gustavo_carvalho_old_id <- "59341297200"
gustavo_carvalho_new_id <- "59675102000"

# Remove the incorrectly parsed Other/Other author record
usc_authors <- usc_authors %>%
  filter(as.character(authorID) != gustavo_carvalho_old_id)

# Transfer its publication to the correct USC Computer Science author ID
usc_bridge$authorID[
  !is.na(usc_bridge$authorID) &
    as.character(usc_bridge$authorID) == gustavo_carvalho_old_id
] <- gustavo_carvalho_new_id

usc_bridge <- usc_bridge %>% distinct()

# --- Apply manual affiliation fixes from spreadsheet -------------------------
manual_fix_auth_affiliation <- read.csv("data_manual/manual_fix_auth_affiliation.csv")

for (i in 1:nrow(manual_fix_auth_affiliation)) {
  row <- manual_fix_auth_affiliation[i, ]

  author_id <- row$authorID
  involved_school <- row$Div
  involved_dept <- row$Dept
  mod_type <- row$mod_type

  if (mod_type == "replace_other") {
    # Replace "Other/Other" affiliation with the specified Dept and Div
    matching_rows <- usc_authors %>%
      filter(authorID == author_id & Dept == "Other" & Div == "Other")

    if (nrow(matching_rows) > 0) {
      row_indices <- which(usc_authors$authorID == author_id &
                             usc_authors$Dept == "Other" &
                             usc_authors$Div == "Other")
      usc_authors[row_indices, "Dept"] <- involved_dept
      usc_authors[row_indices, "Div"] <- involved_school
    } else {
      message("Error: No rows with Dept and Div as 'Other' for authorID ", author_id)
    }
  } else if (mod_type == "remove") {
    rows_to_remove <- usc_authors %>%
      filter(authorID == author_id & Div == involved_school & Dept == involved_dept)
    if (nrow(rows_to_remove) > 0) {
      usc_authors <- usc_authors %>%
        filter(!(authorID == author_id & Div == involved_school & Dept == involved_dept))
      message("Removed rows: authorID = ", author_id,
              ", School = ", involved_school,
              ", Dept = ", involved_dept)
    } else {
      message("No rows to remove for authorID ", author_id)
    }
  } else if (mod_type == "delete_author") {
    rows_to_remove <- usc_authors %>%
      filter(authorID == author_id)
    if (nrow(rows_to_remove) > 0) {
      usc_authors <- usc_authors %>%
        filter(!(authorID == author_id))
      message("Removed rows: authorID = ", author_id)
    } else {
      message("No rows to remove for authorID ", author_id)
    }
  }
}

# --- Programmatic merge of Other/Other authors with same first+last name ------
# Load publication keywords for overlap calculation
usc_pubs <- read.csv("data_processed/05_pubs_with_law.csv", na.strings = c("", "NA"))
usc_pubs$combined_keywords <- paste(
  ifelse(is.na(usc_pubs$Author.Keywords), "", tolower(usc_pubs$Author.Keywords)),
  ifelse(is.na(usc_pubs$Indexed.Keywords), "", tolower(usc_pubs$Indexed.Keywords)),
  sep = ";"
)

# Build per-author keyword set via bridge table
author_kw <- usc_bridge %>%
  left_join(usc_pubs[, c("pubID", "combined_keywords")], by = "pubID") %>%
  group_by(authorID) %>%
  summarize(all_keywords = paste(unique(na.omit(combined_keywords)), collapse = ";"),
            n_pubs = length(unique(pubID)),
            .groups = "drop")

# Normalize first token of first name + last name
usc_authors$loweronlyfirst <- tolower(sapply(usc_authors$firstname, function(x) {
  if (is.na(x)) return("")
  stringr::str_squish(gsub("[^A-Za-z]", " ",
                           strsplit(stringi::stri_trans_general(as.character(x), "Latin-ASCII"), " ")[[1]][1]))
}))
usc_authors$lowerlast <- tolower(usc_authors$lastname)

# Split into Other/Other vs real-dept authors
authors_distinct <- usc_authors %>%
  select(authorID, loweronlyfirst, lowerlast, Dept, Div) %>%
  distinct()

other_authors <- authors_distinct %>%
  filter(Dept == "Other" & Div == "Other") %>%
  select(authorID, loweronlyfirst, lowerlast) %>%
  distinct() %>%
  left_join(author_kw, by = "authorID")

real_authors <- authors_distinct %>%
  filter(!(Dept == "Other" & Div == "Other")) %>%
  select(authorID, loweronlyfirst, lowerlast) %>%
  distinct() %>%
  left_join(author_kw, by = "authorID")

# Match on same first token + last name
candidates <- merge(
  other_authors,
  real_authors,
  by = c("loweronlyfirst", "lowerlast"),
  suffixes = c("_other", "_real")
) %>%
  filter(authorID_other != authorID_real)

# Only merge if exactly one real-dept author matches (avoid common name ambiguity)
candidates <- candidates %>%
  group_by(authorID_other) %>%
  filter(length(unique(authorID_real)) == 1) %>%
  ungroup()

# For each Other/Other ID, keep the real-dept ID as canonical
auto_merge <- candidates %>%
  group_by(authorID_other) %>%
  slice(1) %>%
  ungroup() %>%
  select(authorID_old = authorID_other, authorID_new = authorID_real)

if (nrow(auto_merge) > 0) {
  authorID_map_p <- setNames(as.character(auto_merge$authorID_new),
                             as.character(auto_merge$authorID_old))
  
  # Remap in authors table
  usc_authors$authorID <- sapply(usc_authors$authorID, function(x) {
    mapped <- authorID_map_p[as.character(x)]
    if (!is.na(mapped)) unname(mapped) else as.character(x)
  })
  
  # Remap in bridge table
  usc_bridge$authorID <- sapply(usc_bridge$authorID, function(x) {
    mapped <- authorID_map_p[as.character(x)]
    if (!is.na(mapped)) unname(mapped) else as.character(x)
  })
  usc_bridge <- usc_bridge %>% distinct()
  
  # Drop Other/Other rows for any authorID that now has a real dept row
  usc_authors <- usc_authors %>%
    group_by(authorID) %>%
    mutate(has_real_dept = any(!(Dept == "Other" & Div == "Other"))) %>%
    filter(!(has_real_dept & Dept == "Other" & Div == "Other")) %>%
    select(-has_real_dept) %>%
    ungroup()
  
  message("Auto-merged ", nrow(auto_merge), " Other/Other author IDs into real-dept IDs")
} else {
  message("No Other/Other authors to merge.")
}

write.csv(usc_authors,
          "data_processed/07_authors_manual_edited.csv",
          row.names = FALSE)
write.csv(usc_bridge,
          "shiny_app/07_bridge_manual_edited.csv",
          row.names = FALSE)
