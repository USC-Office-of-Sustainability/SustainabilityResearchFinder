# stars summaries
library(reshape2)
library(dplyr)
dir.create("summary_output", showWarnings = FALSE)

# --- Config -------------------------------------------------------------------
# Automatically uses the 3 most recent years in the data.
# No manual edits needed when a new year is added to the pipeline.
all_years    <- sort(unique(read.csv("data_processed/09_pubs_sdg_manual_fixed.csv")$Year))
YEARS        <- tail(all_years, 3)           # e.g. 2023 2024 2025 → becomes 2024 2025 2026 automatically
RECENT_YEARS <- tail(all_years, 3)           # same window for "recent years" breakdowns

YEARS_LABEL  <- paste0(min(YEARS), "_", substr(max(YEARS), 3, 4))
RECENT_LABEL <- paste0(min(RECENT_YEARS), "_", substr(max(RECENT_YEARS), 3, 4))
# e.g. YEARS_LABEL = "2020_25", RECENT_LABEL = "2023_25"
# ------------------------------------------------------------------------------

# load data
usc_sdgs <- read.csv("data_processed/08_pubs_sdg_categorized.csv")
usc_authors <- read.csv("shiny_app/14_authors_dept_corrected.csv") %>%
  rename(Division = Div, Department = Dept)
usc_bridge <- read.csv("shiny_app/07_bridge_manual_edited.csv")

usc_pubs_sdgs <- read.csv("data_processed/09_pubs_sdg_manual_fixed.csv")
usc_pubs_sdgs <- usc_pubs_sdgs %>%
  filter(Year %in% YEARS) %>%
  filter(!Document.Type %in% c("Letter", "Retracted", "Note", "Erratum"))

tmp <- merge(usc_pubs_sdgs, usc_bridge,
             by = c("pubID", "Link"))
usc_joined <- merge(tmp, usc_authors,
              by = "authorID")



# scholar classifications
dept_researcher_classification <- usc_joined %>%
  group_by(authorID, fullname, Department, Division) %>%
  summarize(
    all_sustainability_categories = paste(
      unique(sustainability_category),
      collapse = ";"
    ),
    .groups = "drop"
  ) %>%
  mutate(
    researcher_classification = case_when(
      grepl("Focused", all_sustainability_categories) ~ "Sustainability-Focused",
      grepl("Inclusive", all_sustainability_categories) ~ "Sustainability-Inclusive",
      grepl("SDG", all_sustainability_categories) ~ "SDG-Related",
      grepl("Not-Related", all_sustainability_categories) ~ "Not Related",
      TRUE ~ "Not Classified"
    )
  ) %>%
  select(Division, Department, authorID, fullname, researcher_classification)

write.csv(
  dept_researcher_classification,
  "summary_output/usc_department_researcher_classification.csv",
  row.names = FALSE
)



dept_researcher_classification_recent <- usc_joined %>%
  filter(Year %in% RECENT_YEARS) %>%
  group_by(authorID, fullname, Department, Division) %>%
  summarize(
    all_sustainability_categories = paste(
      unique(sustainability_category),
      collapse = ";"
    ),
    .groups = "drop"
  ) %>%
  mutate(
    researcher_classification = case_when(
      grepl("Focused", all_sustainability_categories) ~ "Sustainability-Focused",
      grepl("Inclusive", all_sustainability_categories) ~ "Sustainability-Inclusive",
      grepl("SDG", all_sustainability_categories) ~ "SDG-Related",
      grepl("Not-Related", all_sustainability_categories) ~ "Not Related",
      TRUE ~ "Not Classified"
    )
  ) %>%
  select(Division, Department, authorID, fullname, researcher_classification)

write.csv(
  dept_researcher_classification_recent,
  paste0("summary_output/usc_department_researcher_classification_", RECENT_LABEL, ".csv"),
  row.names = FALSE
)



dept_recent <- usc_joined %>%
  filter(Year %in% RECENT_YEARS) %>%
  group_by(Division, Department) %>%
  summarize(
    all_sustainability_categories = paste(
      unique(sustainability_category),
      collapse = ";"
    ),
    .groups = "drop"
  ) %>%
  mutate(
    sustainability_classification = case_when(
      grepl("Focused", all_sustainability_categories) ~ "Sustainability-Focused",
      grepl("Inclusive", all_sustainability_categories) ~ "Sustainability-Inclusive",
      grepl("SDG", all_sustainability_categories) ~ "SDG-Related",
      grepl("Not-Related", all_sustainability_categories) ~ "Not Related",
      TRUE ~ "Not Classified"
    )
  ) %>%
  select(Division, Department, sustainability_classification)

write.csv(
  dept_recent,
  paste0("summary_output/usc_departments_sustainability_category_counts_", RECENT_LABEL, ".csv"),
  row.names = FALSE
)



# By year summary of scholars
usc_joined %>%
  group_by(authorID, Year) %>%
  summarize(all_sustainability_categories = paste(sustainability_category[!duplicated(sustainability_category)], collapse = ";")) %>%
  mutate(one_sustainability_category = case_when(grepl("Focused", all_sustainability_categories)~"Sustainability-Focused",
                                                 grepl("Inclusive", all_sustainability_categories)~"Sustainability-Inclusive",
                                                 grepl("SDG", all_sustainability_categories)~"SDG-Related",
                                                 grepl("Not-Related", all_sustainability_categories)~"Not Related")) %>%
  select(authorID, one_sustainability_category, Year) %>%
  group_by(one_sustainability_category, Year) %>%
  count() -> scholars_summary_by_year

write.csv(scholars_summary_by_year,
          paste0("summary_output/scholars_summary_by_year_", YEARS_LABEL, ".csv"),
          row.names = FALSE)

# overall summary of scholars
usc_joined %>%
  group_by(authorID) %>%
  summarize(all_sustainability_categories = paste(sustainability_category[!duplicated(sustainability_category)], collapse = ";")) %>%
  mutate(one_sustainability_category = case_when(grepl("Focused", all_sustainability_categories)~"Sustainability-Focused",
                                                 grepl("Inclusive", all_sustainability_categories)~"Sustainability-Inclusive",
                                                 grepl("SDG", all_sustainability_categories)~"SDG-Related",
                                                 grepl("Not-Related", all_sustainability_categories)~"Not Related")) %>%
  select(authorID, one_sustainability_category) %>%
  group_by(one_sustainability_category) %>%
  count() -> scholars_summary_overall

write.csv(scholars_summary_overall,
          paste0("summary_output/scholars_summary_overall_", YEARS_LABEL, ".csv"),
          row.names = FALSE)


# By year summary of Departments/Centers/Institutes
usc_joined %>%
  group_by(Department, Year) %>%
  summarize(all_sustainability_categories = paste(sustainability_category[!duplicated(sustainability_category)], collapse = ";")) %>%
  mutate(one_sustainability_category = case_when(grepl("Focused", all_sustainability_categories)~"Sustainability-Focused",
                                                 grepl("Inclusive", all_sustainability_categories)~"Sustainability-Inclusive",
                                                 grepl("SDG", all_sustainability_categories)~"SDG-Related",
                                                 grepl("Not-Related", all_sustainability_categories)~"Not Related")) %>%
  select(Department, one_sustainability_category, Year) %>%
  group_by(one_sustainability_category, Year) %>%
  count() -> dept_centers_summary_by_year

write.csv(dept_centers_summary_by_year,
          paste0("summary_output/dept_centers_summary_by_year_", YEARS_LABEL, ".csv"),
          row.names = FALSE)

# Overall summary of Departments/Centers/Institutes
usc_joined %>%
  group_by(Department) %>%
  summarize(all_sustainability_categories = paste(sustainability_category[!duplicated(sustainability_category)], collapse = ";")) %>%
  mutate(one_sustainability_category = case_when(grepl("Focused", all_sustainability_categories)~"Sustainability-Focused",
                                                 grepl("Inclusive", all_sustainability_categories)~"Sustainability-Inclusive",
                                                 grepl("SDG", all_sustainability_categories)~"SDG-Related",
                                                 grepl("Not-Related", all_sustainability_categories)~"Not Related")) %>%
  select(Department, one_sustainability_category) %>%
  group_by(one_sustainability_category) %>%
  count() -> dept_centers_summary_overall

write.csv(dept_centers_summary_overall,
          paste0("summary_output/dept_centers_summary_overall_", YEARS_LABEL, ".csv"),
          row.names = FALSE)

# By year summary of just Departments
usc_joined %>%
  filter(!grepl("Center", Department) & !grepl("Institute", Department)) %>% select(Department, Division) %>% distinct() -> just_departments
usc_joined %>%
  filter(grepl("Program", Department)) %>% select(Department) %>% distinct() %>% pull() %>% sort()

write.csv(just_departments, "summary_output/just_departments.csv", row.names = FALSE)

usc_joined %>%
  filter(!grepl("Center", Department) & !grepl("Institute", Department)) %>%
  group_by(Department, Year) %>%
  summarize(all_sustainability_categories = paste(sustainability_category[!duplicated(sustainability_category)], collapse = ";")) %>%
  mutate(one_sustainability_category = case_when(grepl("Focused", all_sustainability_categories)~"Sustainability-Focused",
                                                 grepl("Inclusive", all_sustainability_categories)~"Sustainability-Inclusive",
                                                 grepl("SDG", all_sustainability_categories)~"SDG-Related",
                                                 grepl("Not-Related", all_sustainability_categories)~"Not Related")) %>%
  select(Department, one_sustainability_category, Year) %>%
  group_by(one_sustainability_category, Year) %>%
  count() -> dept_summary_by_year

write.csv(dept_summary_by_year,
          paste0("summary_output/dept_summary_by_year_", YEARS_LABEL, ".csv"),
          row.names = FALSE)

# Overall summary of just Departments
usc_joined %>%
  filter(!grepl("Center", Department) & !grepl("Institute", Department)) %>%
  group_by(Department) %>%
  summarize(all_sustainability_categories = paste(sustainability_category[!duplicated(sustainability_category)], collapse = ";")) %>%
  mutate(one_sustainability_category = case_when(grepl("Focused", all_sustainability_categories)~"Sustainability-Focused",
                                                 grepl("Inclusive", all_sustainability_categories)~"Sustainability-Inclusive",
                                                 grepl("SDG", all_sustainability_categories)~"SDG-Related",
                                                 grepl("Not-Related", all_sustainability_categories)~"Not Related")) %>%
  select(Department, one_sustainability_category) %>%
  group_by(one_sustainability_category) %>%
  count() -> dept_summary_overall

write.csv(dept_summary_overall,
          paste0("summary_output/dept_summary_overall_", YEARS_LABEL, ".csv"),
          row.names = FALSE)

# pubs per year per sustainability classification
usc_pubs_sdgs %>%
  group_by(Year, sustainability_category) %>%
  summarize(num_pubs = n()) -> pubs_per_year_per_classification

write.csv(pubs_per_year_per_classification,
          paste0("summary_output/pubs_per_year_per_classification_", YEARS_LABEL, ".csv"),
          row.names = FALSE)


# larger summary file with every row having the USC scholar name,
# string of affiliations, string of research paper titles, string of
# SDG keywords, string of SDG #s, and focused, inclusive or not-related
# classification

melt(data.table::as.data.table(usc_sdgs), id.vars = c("document", "pubID", "Link", "sustainability_category"), variable.factor = FALSE) %>%
  filter(value > 0) -> only_sdg
only_sdg$variable <- as.character(only_sdg$variable)
only_sdg$goal <- sapply(only_sdg$variable, function(x) {
  as.numeric(strsplit(x, "\\.")[[1]][2])
})
only_sdg %>%
  group_by(document, pubID, Link, sustainability_category) %>%
  mutate(sdgs = paste(unique(goal), collapse = ",")) %>%
  select(-variable, -goal, -value) %>%
  distinct() -> only_sdg

features <- read.csv("data_processed/08_text2sdg_features.csv")
features %>%
  group_by(document, pubID, Link) %>%
  mutate(features = paste(unique(features), collapse = ",")) %>%
  select(document, pubID, Link, features) %>%
  distinct() -> features_all

usc_joined_only_sdg <- merge(merge(usc_joined, only_sdg, all.x = TRUE), features_all, all.x = TRUE)

usc_joined_only_sdg %>%
  group_by(authorID, sustainability_category) %>%
  summarize(Titles = paste(unique(Titles), collapse = ";")) -> by_category

dcast(by_category, authorID ~ sustainability_category, value.var = "Titles") -> titles

usc_joined_only_sdg %>%
  group_by(authorID) %>%
  summarize(firstname = first(firstname),
            lastname = first(lastname),
            fullname = first(fullname),
            Departments = paste(unique(Department), collapse = ";"),
            Divisions = paste(unique(Division), collapse = ";"),
            Affiliations = paste(Department, Division, sep=",", collapse = ";"),
            num_pubs = length(unique(Titles)),
            Years = paste(unique(Year), collapse = ";"),
            sdgs_all = paste(sdgs, collapse = ","),
            social_economic_SDGs = paste(unique(social_economic_SDGs), collapse = ","),
            environmental_SDGs = paste(unique(environmental_SDGs), collapse = ","),
            keywords_all = paste(features[!is.na(features)], collapse = ","),
            sustainability_category =
              ifelse("Sustainability-Focused" %in% sustainability_category,
                     "Sustainability-Focused",
                     ifelse("Sustainability-Inclusive" %in% sustainability_category,
                            "Sustainability-Inclusive",
                            ifelse("SDG-Related" %in% sustainability_category,
                                   "SDG-Related", "Not Related")))) -> author_summary

author_summary$sdgs <- sapply(author_summary$sdgs_all, function(x) {
  paste(sort(as.numeric(unique(strsplit(x, ",")[[1]]))), collapse = ",")
})
author_summary$keywords <- sapply(author_summary$keywords_all, function(x) {
  paste(unique(strsplit(x, ",")[[1]]), collapse = ",")
})
author_summary$Affiliations <- sapply(author_summary$Affiliations, function(x) {
  paste(unique(strsplit(x, ";")[[1]]), collapse = ";")
})
author_summary %>%
  select(-sdgs_all, -keywords_all) -> author_summary

# add titles columns
final_author_summary <- merge(author_summary, titles) %>%
  relocate("Sustainability-Inclusive", .after = last_col()) %>%
  relocate("SDG-Related", .after = last_col()) %>%
  relocate("Not-Related", .after = last_col()) %>%
  rename("Sustainability-Focused Titles" = "Sustainability-Focused",
         "Sustainability-Inclusive Titles" = "Sustainability-Inclusive",
         "SDG-Related Titles" = "SDG-Related",
         "Not-Related Titles" = "Not-Related")

write.csv(final_author_summary,
          paste0("summary_output/usc_authors_", YEARS_LABEL, "_combined_dept_data.csv"),
          row.names = FALSE)
