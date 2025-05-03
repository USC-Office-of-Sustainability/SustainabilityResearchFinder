library(dplyr)
# manual edits 07_fix_authors.R
usc_authors <- read.csv("data_processed/usc_authors_2020_24_combined.csv")
usc_authors_original <- read.csv("data_processed/usc_authors_2020_24_combined.csv")
usc_bridge <- read.csv("data_processed/usc_bridge_2020_24_combined.csv")

usc_authors_n_pub <- merge(usc_authors,
                           usc_bridge) %>%
  group_by(authorID) %>%
  mutate(num_pubs = length(unique(pubID))) %>%
  select(-pubID, -Link) %>%
  distinct()

# 58298252500 -> 7004764268 for Wändi Bruine de Bruin
usc_authors <- usc_authors[-which(usc_authors$authorID == "58298252500"),]
usc_bridge$authorID[which(usc_bridge$authorID == "58298252500")] <- 7004764268


# Read the manual_fix_auth_affiliation file
manual_fix_auth_affiliation <- read.csv("data_manual/manual_fix_auth_affiliation.csv")



# Process the manual fixes
for (i in 1:nrow(manual_fix_auth_affiliation)) {
  # Extract the current row
  row <- manual_fix_auth_affiliation[i, ]

  # Get the parameters
  author_id <- row$authorID
  involved_school <- row$Div
  involved_dept <- row$Dept
  mod_type <- row$mod_type

  if (mod_type == "replace_other") {
    # Find rows with matching authorID and "Other" in Dept and Div
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
      # Remove the rows
      usc_authors <- usc_authors %>%
        filter(!(authorID == author_id & Div == involved_school & Dept == involved_dept))
      message("Removed rows: authorID = ", author_id,
              ", School = ", involved_school,
              ", Dept = ", involved_dept)
    } else {
      message("No rows to remove for authorID ", author_id)
    }
  } else if (mod_type == "delete_author"){
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









#
# USC_department_category <- read.csv("data_raw/usc_departments_with_academic_admin_cate.csv")
#
# colnames(USC_department_category)[colnames(USC_department_category) == "Department.Group"] <- "Dept"
#
# # Merge author departments with categories
# usc_authors_with_category <- usc_authors %>%
#   left_join(USC_department_category, by = "Dept")
#
# # Identify authors who have both "Administrative" and "Academic" categories
# authors_with_both <- usc_authors_with_category %>%
#   group_by(authorID) %>%
#   filter(
#     # Check if there are rows with separate "Administrative" and "Academic"
#     any(Category == "Administrative") & any(Category == "Academic") |
#       # Check if any single row contains both "Administrative" and "Academic"
#       any(str_detect(Category, "Administrative") & str_detect(Category, "Academic"))
#   ) %>%
#   ungroup()
#
# academic_rows <- authors_with_both %>%
#   filter(str_detect(Category, "Academic"))
#
# non_academic_rows <- authors_with_both %>%
#   filter(!str_detect(Category, "Academic"))
#
#
#
# usc_authors_with_category <- usc_authors_with_category %>%
#   anti_join(non_academic_rows, by = c("authorID", "Category", "Dept", "Div"))
#
#
#
#
#
#
#
#
# # test purposes
# author_id <- 53564097100
# filtered_result <- usc_authors_with_category %>%
#   filter(authorID == author_id)
#
# partial_name <- "Epstein"
# filtered_result <- usc_authors_with_category %>%
#   filter(grepl(partial_name, fullname, ignore.case = TRUE))
#
# # Compare the two data frames to find differences
# added_rows <- anti_join(usc_authors_with_category, usc_authors_original)
# removed_rows <- anti_join(usc_authors_original, usc_authors_with_category)
#
#
#
#
#
#
#
#
#

write.csv(usc_authors,
          "shiny_app/usc_authors_2020_24_combined_edit.csv",
          row.names = FALSE)
write.csv(usc_bridge,
          "shiny_app/usc_bridge_2020_24_combined_edit.csv",
          row.names = FALSE)
