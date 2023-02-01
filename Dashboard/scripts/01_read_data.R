# Read all the raw data files and combine them into 1 file

library(here)

# path of csv files
ff <- list.files(here::here("data_raw"), pattern = "csv", full.names = TRUE)

# read data
tmp <- lapply(ff, read.csv)

# combine
usc_data <- do.call(rbind, tmp)

# save as csv
write.csv(usc_data, file = here::here("data_processed/usc_pubs.csv"))
