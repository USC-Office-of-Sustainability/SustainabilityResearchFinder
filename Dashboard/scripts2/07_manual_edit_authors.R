# manual edits 07_fix_authors.R
usc_authors <- read.csv("data_processed/usc_authors_2020_23_combined.csv")
usc_bridge <- read.csv("data_processed/usc_bridge_2020_23_combined.csv")

usc_authors_n_pub <- merge(usc_authors, 
                           usc_bridge) %>%
  group_by(authorID) %>%
  mutate(num_pubs = length(unique(pubID))) %>%
  select(-pubID, -Link) %>%
  distinct()

# 58298252500 -> 7004764268 for Wändi Bruine de Bruin
usc_authors <- usc_authors[-which(usc_authors$authorID == "58298252500"),]
usc_bridge$authorID[which(usc_bridge$authorID == "58298252500")] <- 7004764268

write.csv(usc_authors,
          "shiny_app/usc_authors_2020_23_combined_edit.csv",
          row.names = FALSE)
write.csv(usc_bridge,
          "shiny_app/usc_bridge_2020_23_combined_edit.csv",
          row.names = FALSE)
