# Read all the raw data files and combine them into 1 file

library(here)
library(dplyr)

# path of csv files
ff <- list.files(here::here("data_raw"), pattern = "USC202[012]\\.csv", full.names = TRUE)

# read data
tmp <- lapply(ff, read.csv)

# combine
usc_data <- do.call(rbind, tmp)

# save as csv
write.csv(usc_data, file = here::here("data_processed/usc_pubs_all.csv"), row.names = FALSE)

# edit columns
usc_pubs <- read.csv(here::here("data_processed/usc_pubs_all.csv"))
names(usc_pubs)[names(usc_pubs) == 'X'] <- 'pubID'

# remove document types - Letter, Retracted, Note, Erratum
usc_pubs <- usc_pubs %>%
  filter(!Document.Type %in% c("Letter", "Retracted", "Note", "Erratum"))

# save as csv
write.csv(usc_pubs, 
          file = here::here("data_processed/usc_pubs.csv"),
          row.names = FALSE)

