# stacked bar chart

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
