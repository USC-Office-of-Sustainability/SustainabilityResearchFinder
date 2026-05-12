# Summarize publication counts by sustainability category for stacked bar charts
library(dplyr)

usc_authors <- read.csv("data_processed/07_authors_manual_edited.csv") %>%
  rename(Division = Div, Department = Dept)
usc_bridge <- read.csv("shiny_app/07_bridge_manual_edited.csv")

usc_pubs_sdgs <- read.csv("data_processed/09_pubs_sdg_manual_fixed.csv") %>%
  filter(!Document.Type %in% c("Letter", "Retracted", "Note", "Erratum"))

tmp <- merge(usc_pubs_sdgs, usc_bridge,
             by = c("pubID", "Link"))
usc_joined <- merge(tmp, usc_authors,
              by = "authorID")

# --- Summarize by publication, author, and department -----------------------
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
          "shiny_app/12_pubs_by_product_sust_category.csv",
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
          "shiny_app/12_pubs_by_author_sust_category.csv",
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
          "shiny_app/12_pubs_by_dept_sust_category.csv",
          row.names = FALSE)

