# add 2023 data
library(dplyr)

# all the data
previous_pubs <- read.csv("data_processed/usc_pubs_all.csv")
missing_pubs <- read.csv("data_raw/MissingScopus_pubs_from_2020_2021_2022.csv")
new_pubs <- read.csv("data_raw/updated_scopus_2023_03_08_24.csv")
new_pubs_2024 <- read.csv("data_raw/partial_scopus_2025_01_01.csv")
# make all the colnames/headers the same
setdiff(names(missing_pubs), names(previous_pubs))
missing_pubs <- missing_pubs %>%
  rename(Titles = Title,
         "Indexed.Keywords" = Index.Keywords)


pubs_with_2022 <- bind_rows(previous_pubs, missing_pubs) %>%
   filter(Year %in% c(2020, 2021, 2022))



setdiff(names(new_pubs), names(previous_pubs))
pubs_2023_only <- new_pubs %>%
  rename(Titles = Title,
         "Indexed.Keywords" = Index.Keywords) %>%
  filter(Year == 2023)

pubs_with_2023 <- bind_rows(pubs_with_2022, pubs_2023_only) %>%
  group_by(Link) %>%
  mutate(is_duplicated = n() > 1) %>%  # Check if Link is duplicated
  filter(!(is_duplicated & Year == 2023)) %>%  # Drop rows where Link is duplicated AND Year == 2023
  select(-is_duplicated) %>%  # Clean up the temporary column
  ungroup()


pubs_2023_prev <- pubs_with_2023 %>%
  filter(Year != 2023)

# antijoin pubs_2023_prev pubs_with_2022
diff <- anti_join(pubs_2023_prev, pubs_with_2022)






pubs_2024_only <- new_pubs_2024 %>%
  rename(Titles = Title,
         "Indexed.Keywords" = Index.Keywords) %>%
  filter(Year == 2024)


pubs_with_2024 <- bind_rows(pubs_with_2023, pubs_2024_only) %>%
  group_by(Link) %>%
  mutate(is_duplicated = n() > 1) %>%  # Check if Link is duplicated
  filter(!(is_duplicated & Year == 2024)) %>%  # Drop rows where Link is duplicated AND Year == 2023
  select(-is_duplicated) %>%  # Clean up the temporary column
  ungroup()

pubs_2024_prev <- pubs_with_2024 %>%
  filter(Year != 2024)






all_pubs <- pubs_with_2024



# remove document types - Letter, Retracted, Note, Erratum
# select only 2020-23
final_pubs <- all_pubs %>%
  filter(!Document.Type %in% c("Letter", "Retracted", "Note", "Erratum")) %>%
  filter(Year %in% c(2020, 2021, 2022, 2023, 2024)) %>%
  distinct()

final_pubs

# assign pubID according to EID
final_pubs %>%
  group_by(EID) %>%
  tidyr::fill(pubID, .direction = "downup") -> final_pubs
# distinct makes no difference since some of the cited by #s change

# assume that the newer publications appear lower in the df
# keep the last row
final_pubs %>%
  group_by(pubID, EID) %>%
  slice(n()) -> updated_pubs

# uniquecourses=coursesAC1 [!duplicated(coursesAC1 [,'courseID']),]

# assign NA pubID to a continuing pubID #
starting_pubID <- max(updated_pubs$pubID, na.rm = TRUE) + 1
ending_pubID <- starting_pubID + sum(is.na(updated_pubs$pubID)) - 1
missing_pubIDs <- seq(from = starting_pubID, to = ending_pubID, by = 1)
updated_pubs$pubID[is.na(updated_pubs$pubID)] <- missing_pubIDs

# no more duplicated EID, pubIDs
which(duplicated(updated_pubs$EID))
which(duplicated(updated_pubs$pubID))

# save as csv
write.csv(updated_pubs,
          file = here::here("data_processed/usc_pubs_2020_23.csv"),
          row.names = FALSE)

# counts rows group by Year in updated_pubs
updated_pubs %>%
  group_by(Year) %>%
  summarise(n = n())

updated_pubs