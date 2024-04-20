# fix based on spreadsheet
library(readxl)
library(dplyr)
dup_authors <- read_excel("duplicate_research_authors_syntax_corrected_use.xlsx")
dup_authors %>%
  filter(Combine == TRUE) %>%
  group_by(firstname, lastname) %>%
  mutate(authorIDs = paste(authorID, collapse = ",")) %>% 
  select(authorIDs) %>% pull() -> authorIDs_to_fix
separate_authorIDs_to_fix <- as.data.frame(t(sapply(authorIDs_to_fix, function(x) {
  c(strsplit(x, ",")[[1]])
}))) %>% unique

# create map
auth_map <- separate_authorIDs_to_fix$V1
names(auth_map) <- separate_authorIDs_to_fix$V2
julie_map_df <- data.frame(authorID_new = unname(auth_map), authorID_old = names(auth_map))
write.csv(julie_map_df,
          "data_processed/julie_map_df.csv",
          row.names = FALSE)

usc_bridge <- read.csv("data_processed/bridge_law_fixed.csv")
usc_authors <- read.csv("data_processed/usc_authors_law_fixed.csv")
# some error -> too many duplicate dept div caused by pattern
usc_authors %>%
  select(-Pattern, -id) %>% distinct() -> usc_authors
# update authors or just remove the rows
new_authorIDs <- sapply(usc_authors$authorID, function(x) {
  if (x %in% names(auth_map)) {
    auth_map[x]
  } else {
    x
  }
})
sum(is.na(new_authorIDs))
new_authorIDs <- unname(new_authorIDs)
usc_authors$authorID <- new_authorIDs

# fill in columns
usc_authors %>%
  mutate_if(is.character, list(~na_if(.,""))) %>%
  group_by(authorID) %>%
  tidyr::fill(initials:InUSCDirectory, .direction = "updown") %>%
  ungroup() %>%
  mutate_if(is.character, ~tidyr::replace_na(.,"")) -> usc_authors_temp

# remove duplicates
usc_authors_temp %>%
  group_by(authorID, Dept, Div) %>%
  distinct(authorID, Dept, Div, .keep_all = TRUE) -> usc_authors_temp

# update bridge
new_authorIDs <- sapply(usc_bridge$authorID, function(x) {
  if (x %in% names(auth_map)) {
    auth_map[x]
  } else {
    x
  }
})
new_authorIDs <- unname(new_authorIDs)
usc_bridge$authorID <- new_authorIDs

write.csv(usc_authors_temp,
          "data_processed/usc_authors_law_fixed2.csv",
          row.names = FALSE)
write.csv(usc_bridge,
          "data_processed/bridge_law_fixed2.csv",
          row.names = FALSE)
