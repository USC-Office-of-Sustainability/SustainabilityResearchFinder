# creating a bandaid..
# create a column with all associated authorIDs
library(dplyr)

# from 02_identify_usc_authors.R
first_map <- read.csv("data_processed/authorID_map2.csv")
second_map <- read.csv("data_processed/six_or_more_map_df.csv")
third_map <- read.csv("data_processed/less_six_map_df.csv")
# manual fixes
manual_fixes <- bind_rows(data.frame(authorID_new = 57211586105, authorID_old = 57668533700),
data.frame(authorID_new = 56740038200, authorID_old = 57218170772),
data.frame(authorID_new = 6602691708, authorID_old = 57731298700),
data.frame(authorID_new = 6505568173, authorID_old = 57857181600),
data.frame(authorID_new = 57226278102, authorID_old = 38961461600),
data.frame(authorID_new = 57226278102, authorID_old = 26538380300))
first_half_map <- bind_rows(first_map, second_map, third_map, manual_fixes) %>%
  mutate(across(everything(), as.character))
# from 07_fix_authors.R
manual_fixes2 <- bind_rows(data.frame(authorID_new = 57214836310, authorID_old = 57224679598),
# data.frame(authorID_new = 57657883900, authorID_old = "Gould1"),
data.frame(authorID_new = 57692525700, authorID_old = 57216528127),
data.frame(authorID_new = 57073146400, authorID_old = 57215497016),
data.frame(authorID_new = 57204376126, authorID_old = 57189185230),
data.frame(authorID_new = 57204376126, authorID_old = 57219003786)) %>%
  mutate(across(everything(), as.character)) %>%
  rbind(data.frame(authorID_new = 57657883900, authorID_old = "Gould1"))
# from 08_fix_authors.R
final_map <- read.csv("data_processed/julie_map_df.csv") %>%
  mutate(across(everything(), as.character))

# need to convert them to string
combined_map <- bind_rows(first_half_map, manual_fixes2, final_map)

# get all overlaping authorIDs into one row

# get forward - backward (both pairs)
reflected_map <- data.frame(authorID_old = combined_map$authorID_new,
                            authorID_new = combined_map$authorID_old)

entire_map <- rbind(combined_map, reflected_map)

combined_map %>%
  group_by(authorID_new) %>%
  mutate(authorIDs = paste(sort(unique(authorID_old)), collapse = ";")) %>%
  mutate(all_authorIDs = paste(authorID_new, authorIDs, sep=";")) %>%
  mutate(all_authorIDs = paste(sort(strsplit(all_authorIDs,";")[[1]]), collapse = ";")) %>%
  # ungroup() %>%
  select(all_authorIDs) %>% 
  distinct() -> past_authorIDs
# two columns
# from 1000 to 830
# all the authorID_new is the assumed new authorID - the final one being used

write.csv(past_authorIDs,
          "data_processed/past_authorIDs.csv",
          row.names = FALSE)

# is there any authorID in both columns? no
res <- c()
for (i in 1:nrow(past_authorIDs)) {
  current <- past_authorIDs$authorID_new[i]
  res <- c(res,current %in% past_authorIDs$all_authorIDs[-i])
}
sum(res) # no

res <- c()
for (i in 1:nrow(combined_map)) {
  res <- c(res, combined_map$authorID_new[i] %in% combined_map$authorID_old)
}
sum(res)
which(res)
# problems - transitive property - some authorIDs in both columns
res <- c()
for (i in 1:nrow(temp1)) {
  if (sum(grepl(temp1$all_authorIDs[i],temp1$all_authorIDs[-i]))) {
    res <- c(res, i)
  }
}


entire_map %>%
  group_by(authorID_new) %>%
  mutate(authorIDs = paste(sort(unique(authorID_old)), collapse = ";")) %>%
  mutate(all_authorIDs = paste(authorID_new, authorIDs, sep=";")) %>%
  mutate(all_authorIDs = paste(sort(strsplit(all_authorIDs,";")[[1]]), collapse = ";")) %>%
  ungroup() %>% View
  distinct(all_authorIDs) -> temp1
