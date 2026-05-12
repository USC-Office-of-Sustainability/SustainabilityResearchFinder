library(dplyr)
corrections <- read.csv("data_manual/Research Division and Department Corrections - 2020-2024-Original Dept_Division with corrections.csv")

author_data <- read.csv("data_processed/07_authors_manual_edited.csv")

# Perform the corrections
author_data_corrected <- author_data %>%
  left_join(corrections, by = c("Dept" = "Dept", "Div" = "Div")) %>%
  mutate(
    Dept = ifelse(!is.na(Department.Name.Correction) & Department.Name.Correction != "", Department.Name.Correction, Dept),
    Div = ifelse(!is.na(Division.Name.Correction) & Division.Name.Correction != "", Division.Name.Correction, Div)
  ) %>%
  select(names(author_data)) %>%  # Ensure only original author_data columns are retained
  distinct(authorID, Dept, Div, .keep_all = TRUE)  # Remove rows that became identical after renaming

write.csv(author_data_corrected, "data_processed/07_authors_manual_edited.csv", row.names = FALSE)