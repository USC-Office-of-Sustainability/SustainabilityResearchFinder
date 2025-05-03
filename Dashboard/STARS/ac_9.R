# stars summaries
library(reshape2)
library(dplyr)
# load data
# usc_pubs <- read.csv("data_processed/usc_pubs_law_2020_24.csv")
# usc_sdgs <- read.csv("data_processed/usc_sdgs_with_categories_2020_23_manual_fix.csv")
usc_authors <- read.csv("shiny_app/usc_authors_2020_24_combined_edit.csv") %>%
  rename(Division = Div, Department = Dept)
usc_bridge <- read.csv("shiny_app/usc_bridge_2020_24_combined_edit.csv")
# 2020-2022
# usc_pubs <- usc_pubs %>% 
#   filter(Year %in% c(2020, 2021, 2022, 2023)) %>% 
#   filter(!Document.Type %in% c("Letter", "Retracted", "Note", "Erratum"))
# merge
usc_pubs_sdgs <- read.csv("data_processed/usc_pubs_with_sdgs_2020_24_manual_fix.csv")
usc_pubs_sdgs <- usc_pubs_sdgs %>% 
  filter(Year %in% c(2020, 2021, 2022, 2023, 2024)) %>%
  filter(!Document.Type %in% c("Letter", "Retracted", "Note", "Erratum"))

tmp <- merge(usc_pubs_sdgs, usc_bridge,
             by = c("pubID", "Link"))
usc_joined <- merge(tmp, usc_authors,
              by = "authorID")

# By year summary of scholars 
# (year, # of focused, inclusive or not -related scholars)

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
          "new_scholars_summary_by_year_2020_24.csv",
          row.names = FALSE)

# overall summary of scholars
# (#of focused, inclusive or not-related scholars)

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
          "new_scholars_summary_overall_2020_24.csv",
          row.names = FALSE)



# By year summary of Departments/Centers/Institutes
# (year, # of focused ,inclusive or not-related)

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
          "new_dept_centers_summary_by_year_2020_24.csv",
          row.names = FALSE)

# Overall summary of Departments/Centers/ Institutes
# (# of focused, inclusive or not -related)

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
          "new_dept_centers_summary_overall_2020_24.csv",
          row.names = FALSE)

# By year summary of just Departments

# some are still not exactly departments

usc_joined %>%
  filter(!grepl("Center", Department) & !grepl("Institute", Department)) %>% select(Department, Division) %>% distinct() -> just_departments
usc_joined %>%
  filter(grepl("Program", Department)) %>% select(Department) %>% distinct() %>% pull() %>% sort()

write.csv(just_departments, "new_just_departments.csv", row.names = FALSE)

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
          "new_dept_summary_by_year_2020_24.csv",
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
          "new_dept_summary_overall_2020_24.csv",
          row.names = FALSE)

# pubs per year per sustainability classification

usc_pubs_sdgs %>%
  group_by(Year, sustainability_category) %>%
  summarize(num_pubs = n()) -> pubs_per_year_per_classification
write.csv(pubs_per_year_per_classification,
          "new_pubs_per_year_per_classification_2020_24.csv",
          row.names = FALSE)

# larger summary file with every row having the Usc scholar name,
# string of affiliations, string of research paper titles, string of
# SDG keywords, string of SDG #s, and focused, inclusive or not-related
# classification (if they have just 1 paper across the three years that
# is focused then they are focused, if they have just 1 paper that is
# related but not any that are focused then they are 'SDG-Related' or
# 'Inclusive', then otherwise they are not-related



# dcast(melt(data.table::as.data.table(usc_sdgs), id.vars = c("document", "pubID", "Link", "sustainability_category")), document + pubID + Link + sustainability_category ~ variable) %>% View
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

# features <- read.csv("data_processed/usc_text2sdg_features.csv")
features <- read.csv("data_processed/usc_text2sdg_features_2020_24.csv")
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
            # Titles = paste(unique(Titles), collapse = ";"),
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
# NA coercion warning
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
# final_author_summary$firstname[which(final_author_summary$authorID == "57204376126")] <- "Cameron J."
# final_author_summary$lastname[which(final_author_summary$authorID == "57204376126")] <- "Thrash"


#
# social_economic_sdgs <- c(1, 2, 3, 4, 5, 8, 9, 10, 11, 16, 17)
# environmental_sdgs <- c(6, 7, 12, 13, 14, 15)
#
# # Function to separate SDGs into categories
# categorize_sdgs <- function(sdg_string, category) {
#   # Split the string into individual SDGs
#   sdg_list <- unlist(strsplit(sdg_string, ",\\s*"))
#   # Keep only those that match the category
#   selected_sdgs <- sdg_list[sdg_list %in% as.character(category)]
#   # Collapse back into a comma-separated string
#   return(paste(selected_sdgs, collapse = ", "))
# }
#
# # Apply the function to create new columns
# final_author_summary <- final_author_summary %>%
#   mutate(
#     social_economic_SDGs = sapply(sdgs, categorize_sdgs, category = social_economic_sdgs),
#     environmental_SDGs = sapply(sdgs, categorize_sdgs, category = environmental_sdgs)
#   ) %>%
#   relocate(social_economic_SDGs, environmental_SDGs, .after = sdgs)
#
#


write.csv(final_author_summary,
          "new_usc_authors_2020_24_combined_dept_data.csv",
          row.names = FALSE)



## numbers generated below are different since they are based off of author summary file
library(readxl)
read_excel("STARS_AC-9_USC_2020_2021_2022_Revised_02_07_24.xlsx", col_types = "text") -> new_data
new_data %>%
  tidyr::separate_rows(Departments, sep=";") %>%
  group_by(Departments) %>%
  summarize(all_sustainability_categories = paste(Corrected_Sustainability_Category[!duplicated(Corrected_Sustainability_Category)], collapse = ";")) %>%
  mutate(one_sustainability_category = case_when(grepl("Focused", all_sustainability_categories)~"Sustainability-Focused",
                                                 grepl("Inclusive", all_sustainability_categories)~"Sustainability-Inclusive",
                                                 grepl("SDG", all_sustainability_categories)~"SDG-Related",
                                                 grepl("Not", all_sustainability_categories)~"Not Related")) %>%
  select(Departments, one_sustainability_category) %>%
  group_by(one_sustainability_category) %>%
  count() -> dept_centers_summary_overall
dept_centers_summary_overall

new_data$`SDG Keywords`[is.na(new_data$`SDG Keywords`)] <- ""
new_data$`SDG Goals`[is.na(new_data$`SDG Goals`)] <- ""
new_data$lastname[is.na(new_data$lastname)] <- ""
final_author_summary$lastname <- trimws(final_author_summary$lastname)
# new_data$fullname[which(new_data$fullname == "Young , Lindsay")] <- "Young, Lindsay"
final_author_summary$fullname <- trimws(final_author_summary$fullname)
comp <- merge(new_data, final_author_summary, all.x = FALSE, by = "authorID", suffixes = c("", "y")) %>%
  select(names(new_data), sdgs)
comp$sdgs <- paste0("SDG", comp$sdgs)
all.equal(comp$`SDG Keywords`, comp$keywords)
all.equal(comp$`SDG Goals`, comp$sdgs)
write.csv(comp, row.names = FALSE,
          "ac_9_2020_22_sdg_column.csv")

comparing <- merge(new_data, final_author_summary, all.x = FALSE, by = "authorID")
all.equal(comparing$firstname.x, comparing$firstname.y)
all.equal(comparing$lastname.x, comparing$lastname.y)
all.equal(comparing$fullname.x, comparing$fullname.y)
comparing[which(comparing$Sustainability.Focused.Titles != comparing$`Sustainability-Focused Titles`),] %>% View


new_data %>%
  tidyr::separate_rows(Years, sep = ";") %>%
  group_by(Years, sustainability_category) %>%
  summarize(n = length(unique(authorID))) -> authors_year_summary
write.csv(authors_year_summary,
          row.names = FALSE,
          "author_summary_by_year_2020_22.csv")

new_data %>%
  tidyr::separate_rows(Departments, sep = ";") %>%
  tidyr::separate_rows(Years, sep = ";") %>%
  group_by(Years, sustainability_category) %>%
  summarize(n = length(unique(Departments))) -> dept_year_summary
write.csv(dept_year_summary,
          row.names = FALSE,
          "dept_summary_by_year_2020_22.csv")
