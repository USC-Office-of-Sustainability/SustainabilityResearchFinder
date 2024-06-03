# merge 2023 with 2020-22
library(dplyr)
usc_authors_2023 <- read.csv("data_processed/focused_usc_authors_dept_div_separate_combine_past.csv")
bridge_table_2023 <- read.csv("data_processed/bridge_table_2023_combine_past.csv")

usc_authors_2020 <- read.csv("data_processed/usc_authors_law_fixed2.csv") # some are all uppercase
bridge_table_2020 <- read.csv("data_processed/bridge_law_fixed2.csv")

usc_authors_2023$authorID <- as.character(usc_authors_2023$authorID)
bridge_table_2023$authorID <- as.character(bridge_table_2023$authorID)

usc_authors_2020_23 <- bind_rows(usc_authors_2020, usc_authors_2023)
bridge_table_2020_23 <- bind_rows(bridge_table_2020, bridge_table_2023)

# remove duplicates - keep the first one
# remove the ones where the authorID is not in name_id - no
# combine the info first
usc_authors_2020_23 %>%
  group_by(authorID) %>%
  mutate(affls = paste(affls, collapse = ";"), # too many
            name = first(name),
            name_id = first(name_id),
            firstname = first(firstname),
            lastname = first(lastname),
            fullname = first(fullname),
            initials = first(initials),
            FirstSearch = first(FirstSearch),
            LastSearch = first(LastSearch),
            First = first(First),
            Last = first(Last),
            Email = first(Email),
            PositionTitle = first(PositionTitle),
            Type = first(Type),
            InUSCDirectory = first(InUSCDirectory)) %>%
  mutate(affls = paste(unique(strsplit(affls, ";")[[1]]), collapse = ";")) %>%
  distinct(authorID, Div, Dept, .keep_all = TRUE) -> usc_authors_2020_23_filtered
# correct_name_id <- apply(usc_authors_2020_23_filtered[1:nrow(usc_authors_2020_23_filtered),],1, function(x) {
#   !grepl(paste0("(",x['authorID'],")"), x['name_id'])
# })
# sum(correct_name_id) # incorrect name ids -> were combined

# there's extra other, other
# school has dept + other -> remove other
usc_authors_2020_23_filtered %>%
  group_by(authorID, Div) %>%
  add_count() %>%
  mutate(hasOther = grepl("Other", Dept)) %>% 
  filter(!(n > 1 & hasOther == TRUE)) %>% 
  select(-n, -hasOther) -> usc_authors_2020_23_filtered

# div other should not have other divs
usc_authors_2020_23_filtered %>%
  group_by(authorID) %>%
  mutate(n = length(unique(Div))) %>%
  mutate(hasOther = grepl("Other", Div)) %>%
  filter(!(n>1 & hasOther == TRUE)) %>%
  select(-n, -hasOther) -> usc_authors_2020_23_filtered

write.csv(usc_authors_2020_23_filtered,
          "data_processed/usc_authors_2020_23.csv",
          row.names = FALSE)

which(duplicated(bridge_table_2020_23))
bridge_table_2020_23 <- bridge_table_2020_23 %>%
  distinct()
write.csv(bridge_table_2020_23,
          "data_processed/bridge_table_2020_23.csv",
          row.names = FALSE)

# after combining these there are some authorIDs with different capitalization of names
