library(dplyr)

usc_authors <- read.csv("data_processed/06_authors_name_merged.csv")
usc_bridge <- read.csv("data_processed/06_bridge_name_merged.csv")

# --- One-off authorID correction for Wändi Bruine de Bruin ------------------
usc_authors <- usc_authors[-which(usc_authors$authorID == "58298252500"), ]
usc_bridge$authorID[which(usc_bridge$authorID == "58298252500")] <- 7004764268

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

write.csv(usc_authors,
          "data_processed/07_authors_manual_edited.csv",
          row.names = FALSE)
write.csv(usc_bridge,
          "shiny_app/07_bridge_manual_edited.csv",
          row.names = FALSE)
