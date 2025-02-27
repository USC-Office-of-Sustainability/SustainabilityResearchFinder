library(dplyr)
corrections <- read.csv("data_manual/Research Division and Department Corrections - 2020-2024-Original Dept_Division with corrections.csv")

author_data <- read.csv("shiny_app/usc_authors_2020_24_combined_edit.csv")

# Perform the corrections
author_data_corrected <- author_data %>%
  left_join(corrections, by = c("Dept" = "Dept", "Div" = "Div")) %>%
  mutate(
    Dept = ifelse(!is.na(Department.Name.Correction) & Department.Name.Correction != "", Department.Name.Correction, Dept),
    Div = ifelse(!is.na(Division.Name.Correction) & Division.Name.Correction != "", Division.Name.Correction, Div)
  ) %>%
  select(names(author_data))  # Ensure only original author_data columns are retained

write.csv(author_data_corrected, "shiny_app/usc_authors_2020_24_combined_edit.csv", row.names = FALSE)