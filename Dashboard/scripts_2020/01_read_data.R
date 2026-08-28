# Read all the raw data files and combine them into 1 file

library(here)
library(dplyr)

# path of csv files
ff <- list.files(here::here("data_raw"), pattern = "USC202[012]\\.csv", full.names = TRUE)

# read data
tmp <- lapply(ff, read.csv)

# combine
usc_data <- do.call(rbind, tmp)

# add pubID as first column
usc_pubs <- cbind(pubID = 1:nrow(usc_data), usc_data)

# save as csv
write.csv(usc_pubs, file = here::here("data_processed/usc_pubs_all.csv"), row.names = FALSE)

# remove document types - Letter, Retracted, Note, Erratum
# 2020-22
usc_pubs <- usc_pubs %>%
  filter(!Document.Type %in% c("Letter", "Retracted", "Note", "Erratum")) %>%
  filter(Year %in% c(2020, 2021, 2022))

# save as csv
write.csv(usc_pubs, 
          file = here::here("data_processed/usc_pubs.csv"),
          row.names = FALSE)

