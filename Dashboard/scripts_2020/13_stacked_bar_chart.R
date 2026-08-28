# stacked bar chart
library(dplyr)
# data
usc_pubs <- read.csv("data_processed/usc_pubs_law.csv")
usc_sdgs <- read.csv("data_processed/usc_sdgs_with_categories.csv")
# usc_authors <- read.csv("data_processed/authors_only_revalued.csv")
usc_authors <- read.csv("data_processed/usc_authors_law_fixed_dept.csv") %>%
  rename(Division = Div, Department = Dept)
usc_bridge <- read.csv("data_processed/bridge_law_fixed2.csv")
# dei_data <- read.csv("data_processed/DEI_pubs.csv")
# dei_joined <- read.csv("data_processed/DEI_pubs_ordered.csv")

# 2020-2022
usc_pubs <- usc_pubs %>% filter(Year %in% c(2020, 2021, 2022))

# url
# usc_pubs$url <- paste0("<a href='", usc_pubs$Link, "' target='_blank'>", usc_pubs$Link, "</a>")

# merge
usc_pubs_sdgs <- merge(usc_pubs, usc_sdgs, 
                       by = c("pubID", "Link"),
                       all.x = TRUE)
usc_pubs_sdgs$sustainability_category[is.na(usc_pubs_sdgs$sustainability_category)] = "Not-Related"

tmp <- merge(usc_pubs, usc_bridge,
             by = c("pubID", "Link"))
tmp2 <- merge(tmp, usc_authors,
              by = "authorID")
usc_joined <- merge(tmp2, usc_sdgs,
                    by = c("pubID", "Link"),
                    all.x = TRUE)
usc_joined$sustainability_category[is.na(usc_joined$sustainability_category)] = "Not-Related"

# summarize data for the charts
usc_by_product_sust_cat <- usc_pubs_sdgs %>%
  group_by(pubID, Year) %>%
  summarize(all_sustainability_categories = paste(sustainability_category[!duplicated(sustainability_category)], collapse = ";")) %>%
  mutate(one_sustainability_category = case_when(grepl("Focused", all_sustainability_categories)~"Sustainability-Focused",
                                                 grepl("Inclusive", all_sustainability_categories)~"Sustainability-Inclusive",
                                                 grepl("Not-Related", all_sustainability_categories)~"Not Related")) %>%
  select(pubID, one_sustainability_category, Year) %>% 
  group_by(one_sustainability_category, Year) %>%
  count()
write.csv(usc_by_product_sust_cat,
          "usc_by_product_sust_cat.csv",
          row.names = FALSE)

usc_by_author_sust_cat <- usc_joined %>%
  group_by(authorID, Year) %>%
  summarize(all_sustainability_categories = paste(sustainability_category[!duplicated(sustainability_category)], collapse = ";")) %>%
  mutate(one_sustainability_category = case_when(grepl("Focused", all_sustainability_categories)~"Sustainability-Focused",
                                                 grepl("Inclusive", all_sustainability_categories)~"Sustainability-Inclusive",
                                                 grepl("Not-Related", all_sustainability_categories)~"Not Related")) %>%
  select(authorID, one_sustainability_category, Year) %>% 
  group_by(one_sustainability_category, Year) %>%
  count()
write.csv(usc_by_author_sust_cat,
          "usc_by_author_sust_cat.csv",
          row.names = FALSE)

usc_by_dept_sust_cat <- usc_joined %>%
  group_by(Department, Year) %>%
  summarize(all_sustainability_categories = paste(sustainability_category[!duplicated(sustainability_category)], collapse = ";")) %>%
  mutate(one_sustainability_category = case_when(grepl("Focused", all_sustainability_categories)~"Sustainability-Focused",
                                                 grepl("Inclusive", all_sustainability_categories)~"Sustainability-Inclusive",
                                                 grepl("Not-Related", all_sustainability_categories)~"Not Related")) %>%
  select(Department, one_sustainability_category, Year) %>%
  group_by(one_sustainability_category, Year) %>%
  count()
write.csv(usc_by_dept_sust_cat,
          "usc_by_dept_sust_cat.csv",
          row.names = FALSE)
