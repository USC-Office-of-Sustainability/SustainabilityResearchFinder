library(dplyr)

# --- Merge new authors with 2020-22 historical baseline ----------------------
usc_authors_2023 <- read.csv("data_processed/03_authors_with_dept_div.csv")
bridge_table_2023 <- read.csv("data_processed/03_bridge_pubid_authorid.csv")

usc_authors_2020 <- read.csv("data_processed/usc_authors_law_fixed2.csv")
bridge_table_2020 <- read.csv("data_processed/bridge_law_fixed2.csv")

usc_authors_2023$authorID <- as.character(usc_authors_2023$authorID)
bridge_table_2023$authorID <- as.character(bridge_table_2023$authorID)

usc_authors_2020_23 <- bind_rows(usc_authors_2020, usc_authors_2023)
bridge_table_2020_23 <- bind_rows(bridge_table_2020, bridge_table_2023)

# Merge duplicate authorID rows: pick historical values for name fields,
# combine all affiliation strings, deduplicate by authorID x Div x Dept
usc_authors_2020_23 %>%
  group_by(authorID) %>%
  mutate(affls = paste(affls, collapse = ";"),
         name          = first(name),
         name_id       = first(name_id),
         firstname     = first(firstname),
         lastname      = first(lastname),
         fullname      = first(fullname),
         initials      = first(initials),
         FirstSearch   = first(FirstSearch),
         LastSearch    = first(LastSearch),
         First         = first(First),
         Last          = first(Last),
         Email         = first(Email),
         PositionTitle = first(PositionTitle),
         Type          = first(Type),
         InUSCDirectory = first(InUSCDirectory)) %>%
  mutate(affls = paste(unique(strsplit(affls, ";")[[1]]), collapse = ";")) %>%
  distinct(authorID, Div, Dept, .keep_all = TRUE) -> usc_authors_2020_23_filtered

# Remove Other dept when a real dept exists for the same author x Div
usc_authors_2020_23_filtered %>%
  group_by(authorID, Div) %>%
  add_count() %>%
  mutate(hasOther = grepl("Other", Dept)) %>%
  filter(!(n > 1 & hasOther == TRUE)) %>%
  select(-n, -hasOther) -> usc_authors_2020_23_filtered

# Remove Other div when a real div exists for the same author
usc_authors_2020_23_filtered %>%
  group_by(authorID) %>%
  mutate(n = length(unique(Div)), hasOther = grepl("Other", Div)) %>%
  filter(!(n > 1 & hasOther == TRUE)) %>%
  select(-n, -hasOther) -> usc_authors_2020_23_filtered

write.csv(usc_authors_2020_23_filtered,
          "data_processed/04_authors_combined_historical.csv",
          row.names = FALSE)

bridge_table_2020_23 <- bridge_table_2020_23 %>% distinct()
write.csv(bridge_table_2020_23,
          "data_processed/04_bridge_combined_historical.csv",
          row.names = FALSE)