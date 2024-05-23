# stacked bar chart
library(dplyr)
# data
# usc_pubs <- read.csv("data_processed/usc_pubs_law_2020_23.csv")
# usc_sdgs <- read.csv("data_processed/usc_sdgs_with_categories_2020_23_manual_fix.csv")
# usc_authors <- read.csv("data_processed/authors_only_revalued.csv")
usc_authors <- read.csv("shiny_app/usc_authors_2020_23_combined_edit.csv") %>%
  rename(Division = Div, Department = Dept)
usc_bridge <- read.csv("shiny_app/usc_bridge_2020_23_combined_edit.csv")
# dei_data <- read.csv("data_processed/DEI_pubs.csv")
# dei_joined <- read.csv("data_processed/DEI_pubs_ordered.csv")

# 2020-2022
# usc_pubs <- usc_pubs %>% 
#   filter(Year %in% c(2020, 2021, 2022, 2023)) %>% 
#   filter(!Document.Type %in% c("Letter", "Retracted", "Note", "Erratum"))

# url
# usc_pubs$url <- paste0("<a href='", usc_pubs$Link, "' target='_blank'>", usc_pubs$Link, "</a>")

# merge
usc_pubs_sdgs <- read.csv("data_processed/usc_pubs_with_sdgs_2020_23_manual_fix.csv") %>% 
  filter(Year %in% c(2020, 2021, 2022, 2023)) %>% 
  filter(!Document.Type %in% c("Letter", "Retracted", "Note", "Erratum"))

tmp <- merge(usc_pubs_sdgs, usc_bridge,
             by = c("pubID", "Link"))
usc_joined <- merge(tmp, usc_authors,
              by = "authorID")
# usc_joined$sustainability_category[is.na(usc_joined$sustainability_category)] = "Not-Related"

# summarize data for the charts
usc_by_product_sust_cat <- usc_pubs_sdgs %>%
  group_by(pubID, Year) %>%
  summarize(all_sustainability_categories = paste(sustainability_category[!duplicated(sustainability_category)], collapse = ";")) %>%
  mutate(one_sustainability_category = case_when(grepl("Focused", all_sustainability_categories)~"Sustainability-Focused",
                                                 grepl("Inclusive", all_sustainability_categories)~"Sustainability-Inclusive",
                                                 grepl("SDG-Related", all_sustainability_categories)~"SDG-Related",
                                                 grepl("Not-Related", all_sustainability_categories)~"Not Related")) %>%
  select(pubID, one_sustainability_category, Year) %>% 
  group_by(one_sustainability_category, Year) %>%
  count()
write.csv(usc_by_product_sust_cat,
          "shiny_app/usc_by_product_sust_cat_2020_23.csv",
          row.names = FALSE)

usc_by_author_sust_cat <- usc_joined %>%
  group_by(authorID, Year) %>%
  summarize(all_sustainability_categories = paste(sustainability_category[!duplicated(sustainability_category)], collapse = ";")) %>%
  mutate(one_sustainability_category = case_when(grepl("Focused", all_sustainability_categories)~"Sustainability-Focused",
                                                 grepl("Inclusive", all_sustainability_categories)~"Sustainability-Inclusive",
                                                 grepl("SDG-Related", all_sustainability_categories)~"SDG-Related",
                                                 grepl("Not-Related", all_sustainability_categories)~"Not Related")) %>%
  select(authorID, one_sustainability_category, Year) %>% 
  group_by(one_sustainability_category, Year) %>%
  count()
write.csv(usc_by_author_sust_cat,
          "shiny_app/usc_by_author_sust_cat_2020_23.csv",
          row.names = FALSE)

usc_by_dept_sust_cat <- usc_joined %>%
  group_by(Department, Year) %>%
  summarize(all_sustainability_categories = paste(sustainability_category[!duplicated(sustainability_category)], collapse = ";")) %>%
  mutate(one_sustainability_category = case_when(grepl("Focused", all_sustainability_categories)~"Sustainability-Focused",
                                                 grepl("Inclusive", all_sustainability_categories)~"Sustainability-Inclusive",
                                                 grepl("SDG-Related", all_sustainability_categories)~"SDG-Related",
                                                 grepl("Not-Related", all_sustainability_categories)~"Not Related")) %>%
  select(Department, one_sustainability_category, Year) %>%
  group_by(one_sustainability_category, Year) %>%
  count()
write.csv(usc_by_dept_sust_cat,
          "shiny_app/usc_by_dept_sust_cat_2020_23.csv",
          row.names = FALSE)

