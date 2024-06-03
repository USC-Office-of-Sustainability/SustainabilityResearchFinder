# add 2023 data
library(dplyr)

# all the data
missing_pubs <- read.csv("data_raw/MissingScopus_pubs_from_2020_2021_2022.csv")
new_pubs <- read.csv("data_raw/updated_scopus_2023_03_08_24.csv")
previous_pubs <- read.csv("data_processed/usc_pubs_all.csv")
# make all the colnames/headers the same
setdiff(names(missing_pubs), names(previous_pubs))
missing_pubs <- missing_pubs %>%
  rename(Titles = Title,
         "Indexed.Keywords" = Index.Keywords)
setdiff(names(new_pubs), names(previous_pubs))
new_pubs <- new_pubs %>%
  rename(Titles = Title,
         "Indexed.Keywords" = Index.Keywords)
# combine + remove duplicates
all_pubs <- bind_rows(previous_pubs, missing_pubs, new_pubs) %>%
  select(pubID, names(new_pubs)) %>%
  distinct()

# remove document types - Letter, Retracted, Note, Erratum
# select only 2020-23
final_pubs <- all_pubs %>%
  filter(!Document.Type %in% c("Letter", "Retracted", "Note", "Erratum")) %>%
  filter(Year %in% c(2020, 2021, 2022, 2023)) %>%
  distinct()

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
