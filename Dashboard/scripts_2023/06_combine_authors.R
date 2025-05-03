# combine authors
# 02_identify_usc_authors.R
# combine authors with same first last dept div
# ignore other dept -> maybe ignore other div instead?
library(dplyr)
# usc_authors_2020_23_filtered <- read.csv("data_processed/usc_authors_2020_24.csv")
usc_authors_2020_23_filtered <- read.csv("data_processed/usc_authors_law_2020_24.csv", na.strings = c("", "NA"))
# need to fix some weird symbols 
usc_authors_2020_23_filtered$fullname <- gsub("’", "'", usc_authors_2020_23_filtered$fullname)
usc_authors_2020_23_filtered$lastname <- gsub("’", "'", usc_authors_2020_23_filtered$lastname)

# some rows are uppercase
# which(usc_authors_2020_23_filtered$fullname == toupper(usc_authors_2020_23_filtered$fullname))
# usc_authors_2020_23_filtered[which(usc_authors_2020_23_filtered$fullname == toupper(usc_authors_2020_23_filtered$fullname)),]$fullname <- stringr::str_to_title(usc_authors_2020_23_filtered[which(usc_authors_2020_23_filtered$fullname == toupper(usc_authors_2020_23_filtered$fullname)),]$fullname)
# get number of publications for each ID
# want to keep the ID w more publications
# bridge_table_2020_23 <- read.csv("data_processed/bridge_table_2020_24.csv")
bridge_table_2020_23 <- read.csv("data_processed/usc_bridge_law_2020_24.csv")

# usc_authors_n_pub <- merge(usc_authors_2020_23_filtered, 
#                            bridge_table_2020_23)
# # count the number of pubs per author
# usc_authors_n_pub <- usc_authors_n_pub %>%
#   group_by(authorID) %>%
#   mutate(num_pubs = length(unique(pubID))) %>%
#   select(-pubID, -Link) %>%
#   distinct()
# # combine authors with same firstname, lastname, dept, div
# # keep the authorID w more publications
# usc_authors_n_pub %>%
#   filter(Dept != "Other") %>%
#   arrange(desc(num_pubs)) %>%
#   group_by(firstname, lastname, Dept, Div) %>%
#   mutate(n = length(unique(authorID))) %>%
#   filter(n>1) %>%
#   summarize(authorIDs = paste(authorID, collapse = ";")
#             # ,affls = paste(affls, collapse = ";"),
#             # name = first(name),
#             # name_id = paste(name_id, collapse = ";"), # remake this one
#             # fullname = first(fullname), # assume all the same
#             # initials = paste(initials, collapse = ";"),
#             # FirstSearch = paste(FirstSearch, collapse = ";"),
#             # LastSearch = paste(LastSearch, collapse = ";"),
#             # First = paste(First, collapse = ";"),
#             # Last = paste(Last, collapse = ";"),
#             # Email = paste(Email, collapse = ";"),
#             # PositionTitle = paste(PositionTitle, collapse = ";"),
#             # Type = paste(Type, collapse = ";"),
#             # InUSCDirectory = any(InUSCDirectory)
#             ) -> combine_these
# 
# # combine these, then check it
# new_map <- unique(combine_these$authorIDs)
# authorID_new <- sapply(new_map, function(x) {
#   strsplit(x, ";")[[1]][1]
# })
# authorID_old <- sapply(new_map, function(x) {
#   paste(strsplit(x, ";")[[1]][-1], collapse = ";")
# })
# map_df <- data.frame(authorID_new, authorID_old) %>%
#   tidyr::separate_rows(authorID_old, sep = ";")
# 
# authorID_map <- setNames(map_df$authorID_new, map_df$authorID_old)
# 
# new_authorIDs <- sapply(usc_authors_2020_23_filtered$authorID, function(x) {
#   if (as.character(x) %in% authorID_old) {
#     authorID_map[as.character(x)]
#   } else {
#     x
#   }
# })
# 
# new_authorIDs <- unname(new_authorIDs)
# 
# usc_authors_2020_23_filtered$authorID <- new_authorIDs
# 
# usc_authors_2020_23_filtered %>%
#   group_by(authorID, Div, Dept) %>%
#   mutate(affls = paste(affls, collapse = ";")) %>%
#   slice(1) -> usc_authors_2020_23_filtered2
# 
# # update bridge
# new_authorIDs <- sapply(bridge_table_2020_23$authorID, function(x) {
#   if (as.character(x) %in% authorID_old) {
#     authorID_map[as.character(x)]
#   } else {
#     x
#   }
# })
# new_authorIDs <- unname(new_authorIDs)
# bridge_table2 <- bridge_table_2020_23
# bridge_table2$authorID <- new_authorIDs
# 
# # combine same firstlast, dept, div
# usc_authors_2020_23_filtered2$firstlast <- sapply(usc_authors_2020_23_filtered2$fullname, function(x) {
#   s <- strsplit(x, ",")[[1]]
#   stringr::str_squish(paste(s[2], s[1]))
# })
# 
# usc_authors_n_pub <- merge(usc_authors_2020_23_filtered2, 
#                            bridge_table2)
# # count the number of pubs per author
# usc_authors_n_pub <- usc_authors_n_pub %>%
#   group_by(authorID) %>%
#   mutate(num_pubs = length(unique(pubID))) %>%
#   select(-pubID, -Link) %>%
#   distinct()
# # combine authors with same firstname, lastname, dept, div
# # keep the authorID w more publications
# usc_authors_n_pub %>%
#   filter(Dept != "Other") %>%
#   arrange(desc(num_pubs)) %>%
#   group_by(firstlast, Dept, Div) %>%
#   mutate(n = length(unique(authorID))) %>%
#   filter(n>1) %>%
#   summarize(authorIDs = paste(authorID, collapse = ";")) -> combine_these
# 
# # combine these, then check it
# new_map <- unique(combine_these$authorIDs)
# authorID_new <- sapply(new_map, function(x) {
#   strsplit(x, ";")[[1]][1]
# })
# authorID_old <- sapply(new_map, function(x) {
#   paste(strsplit(x, ";")[[1]][-1], collapse = ";")
# })
# map_df <- data.frame(authorID_new, authorID_old) %>%
#   tidyr::separate_rows(authorID_old, sep = ";")
# 
# authorID_map <- setNames(map_df$authorID_new, map_df$authorID_old)
# 
# new_authorIDs <- sapply(usc_authors_2020_23_filtered2$authorID, function(x) {
#   if (as.character(x) %in% authorID_old) {
#     authorID_map[as.character(x)]
#   } else {
#     x
#   }
# })
# 
# new_authorIDs <- unname(new_authorIDs)
# 
# usc_authors_2020_23_filtered2$authorID <- new_authorIDs
# 
# usc_authors_2020_23_filtered2 %>%
#   group_by(authorID, Div, Dept) %>%
#   mutate(affls = paste(affls, collapse = ";")) %>%
#   slice(1) -> usc_authors_2020_23_filtered3
# 
# # update bridge
# new_authorIDs <- sapply(bridge_table2$authorID, function(x) {
#   if (as.character(x) %in% authorID_old) {
#     authorID_map[as.character(x)]
#   } else {
#     x
#   }
# })
# new_authorIDs <- unname(new_authorIDs)
# bridge_table3 <- bridge_table2
# bridge_table3$authorID <- new_authorIDs

# what if skipped last, first? and went straight to first last
# combine same first last, dept, div
usc_authors_2020_23_filtered$firstlast <- sapply(usc_authors_2020_23_filtered$fullname, function(x) {
  s <- strsplit(x, ",")[[1]]
  tolower(stringr::str_squish(gsub("[^A-Za-z]", " ", stringi::stri_trans_general(paste(s[2], s[1]), "Latin-ASCII"))))
})
usc_authors_n_pub <- merge(usc_authors_2020_23_filtered, 
                           bridge_table_2020_23)
# count the number of pubs per author
usc_authors_n_pub <- usc_authors_n_pub %>%
  group_by(authorID) %>%
  mutate(num_pubs = length(unique(pubID))) %>%
  select(-pubID, -Link) %>%
  distinct()
# combine authors with same firstname, lastname, dept, div
# keep the authorID w more publications
usc_authors_n_pub %>%
  filter(Div != "Other") %>%
  # filter(Dept != "Other") %>%
  arrange(desc(num_pubs)) %>%
  group_by(firstlast, Dept, Div) %>%
  mutate(n = length(unique(authorID))) %>%
  filter(n>1) %>%
  summarize(authorIDs = paste(unique(authorID), collapse = ";")
  ) -> combine_these_firstlast
# 21 are dept other

# combine these, then check it
new_map <- unique(combine_these_firstlast$authorIDs)
authorID_new <- sapply(new_map, function(x) {
  strsplit(x, ";")[[1]][1]
})
authorID_old <- sapply(new_map, function(x) {
  paste(strsplit(x, ";")[[1]][-1], collapse = ";")
})
map_df <- data.frame(authorID_new, authorID_old) %>%
  tidyr::separate_rows(authorID_old, sep = ";") %>%
  distinct()

authorID_map <- setNames(map_df$authorID_new, map_df$authorID_old)

new_authorIDs <- sapply(usc_authors_2020_23_filtered$authorID, function(x) {
  if (as.character(x) %in% names(authorID_map)) {
    authorID_map[as.character(x)]
  } else {
    x
  }
})

new_authorIDs <- unname(new_authorIDs)

usc_authors_2020_23_filtered$authorID <- new_authorIDs
# there's an issue where I am taking the first id but does not correspond to taking that row's info using slice(1) without sorting
usc_authors_2020_23_filtered$matched <- apply(usc_authors_2020_23_filtered, 1, function(x) {
  grepl(paste0("(",x[['authorID']],")"), x[['name_id']])
})
usc_authors_2020_23_filtered %>%
  arrange(desc(matched)) %>%
  group_by(authorID) %>%
  tidyr::fill(initials, FirstSearch, LastSearch, First, Last, Email, PositionTitle, Type, InUSCDirectory, .direction = "downup") %>%
  mutate(affls = paste(unique(affls), collapse = ";"),
         # might need to fill in empty columns first
         across(c(name, name_id, firstname, lastname, fullname, initials, FirstSearch, LastSearch, First, Last, Email, PositionTitle, Type, InUSCDirectory, firstlast), first)) %>%
  ungroup() %>% 
  group_by(authorID, Dept, Div) %>%
  slice(1) -> usc_authors_2020_23_filtered2


# update bridge
new_authorIDs <- sapply(bridge_table_2020_23$authorID, function(x) {
  if (as.character(x) %in% names(authorID_map)) {
    authorID_map[as.character(x)]
  } else {
    x
  }
})
new_authorIDs <- unname(new_authorIDs)
bridge_table2 <- bridge_table_2020_23
bridge_table2$authorID <- new_authorIDs



# exclude middle name and combine first last dept div

# usc_authors_2020_23_filtered2$onlyfirst <- stringr::str_squish(gsub("([A-Z]\\.?-? ?)+$", " ", usc_authors_2020_23_filtered2$firstname))
usc_authors_2020_23_filtered2$onlyfirst <- sapply(usc_authors_2020_23_filtered2$firstname, function(x) {
  stringr::str_squish(gsub("[^A-Za-z]", " ", (strsplit(stringi::stri_trans_general(x, "Latin-ASCII"), " ")[[1]][1])))
})
usc_authors_2020_23_filtered2$loweronlyfirst <- tolower(usc_authors_2020_23_filtered2$onlyfirst)
usc_authors_2020_23_filtered2$lowerlast <- tolower(usc_authors_2020_23_filtered2$lastname)

# usc_authors_2020_23_filtered2[which(usc_authors_2020_23_filtered2$onlyfirst != usc_authors_2020_23_filtered2$firstname),] %>% View

usc_authors_n_pub <- merge(usc_authors_2020_23_filtered2, 
                           bridge_table2)
# count the number of pubs per author
usc_authors_n_pub <- usc_authors_n_pub %>%
  group_by(authorID) %>%
  mutate(num_pubs = length(unique(pubID))) %>%
  select(-pubID, -Link) %>%
  distinct()
# combine authors with same firstname, lastname, dept, div
# keep the authorID w more publications
usc_authors_n_pub %>%
  filter(Div != "Other") %>%
  # filter(Dept != "Other") %>%
  filter(onlyfirst != "") %>%
  arrange(desc(num_pubs)) %>%
  group_by(loweronlyfirst, lowerlast, Dept, Div) %>%
  mutate(n = length(unique(authorID))) %>%
  filter(n>1) %>%
  summarize(authorIDs = paste(unique(authorID), collapse = ";")
  ) -> combine_these_onlyfirst
# 7 in other dept

# combine these, then check it
new_map <- unique(combine_these_onlyfirst$authorIDs)
authorID_new <- sapply(new_map, function(x) {
  strsplit(x, ";")[[1]][1]
})
authorID_old <- sapply(new_map, function(x) {
  paste(strsplit(x, ";")[[1]][-1], collapse = ";")
})
map_df <- data.frame(authorID_new, authorID_old) %>%
  tidyr::separate_rows(authorID_old, sep = ";")

authorID_map <- setNames(map_df$authorID_new, map_df$authorID_old)

new_authorIDs <- sapply(usc_authors_2020_23_filtered2$authorID, function(x) {
  if (as.character(x) %in% names(authorID_map)) {
    authorID_map[as.character(x)]
  } else {
    x
  }
})

new_authorIDs <- unname(new_authorIDs)

usc_authors_2020_23_filtered2$authorID <- new_authorIDs
usc_authors_2020_23_filtered2$matched <- apply(usc_authors_2020_23_filtered2, 1, function(x) {
  grepl(paste0("(",x[['authorID']],")"), x[['name_id']])
})
usc_authors_2020_23_filtered2 %>%
  arrange(desc(matched)) %>%
  group_by(authorID) %>%
  tidyr::fill(initials, FirstSearch, LastSearch, First, Last, Email, PositionTitle, Type, InUSCDirectory, .direction = "downup") %>%
  mutate(affls = paste(unique(affls), collapse = ";"),
         across(c(name, name_id, firstname, lastname, fullname, initials, FirstSearch, LastSearch, First, Last, Email, PositionTitle, Type, InUSCDirectory, firstlast), first)) %>%
  ungroup() %>%
  group_by(authorID, Dept, Div) %>%
  slice(1) -> usc_authors_2020_23_filtered3

# update bridge
new_authorIDs <- sapply(bridge_table2$authorID, function(x) {
  if (as.character(x) %in% names(authorID_map)) {
    authorID_map[as.character(x)]
  } else {
    x
  }
})
new_authorIDs <- unname(new_authorIDs)
bridge_table3 <- bridge_table2
bridge_table3$authorID <- new_authorIDs


# write.csv(usc_authors_2020_23_filtered3, 
#           "data_processed/usc_authors_2020_24_combined.csv",
#           row.names = FALSE)
# write.csv(bridge_table3,
#           "data_processed/usc_bridge_2020_24_combined.csv",
#           row.names = FALSE)

# combine A with Aaron if same dept and div -> track this!
# usc_authors_2020_23_filtered3 %>%
#   mutate(firstletter = substr(firstname, 1, 1)) %>%
#   group_by(firstletter, lastname, Dept, Div) %>%
#   mutate(n_id = length(unique(authorID))) %>%
#   filter(n_id > 1) %>% View

usc_authors_2020_23_filtered3$firstnameissues <- grepl("^[A-Z] ?[A-Z]? ?[A-Z]? ?$", usc_authors_2020_23_filtered3$onlyfirst)
usc_authors_2020_23_filtered3 %>%
  mutate(firstletter = substr(strsplit(onlyfirst, " ")[[1]][1], 1, 1)) -> usc_authors_2020_23_filtered3


# usc_authors_n_pub <- merge(usc_authors_2020_23_filtered3, 
#                            bridge_table3)
# # count the number of pubs per author
# usc_authors_n_pub <- usc_authors_n_pub %>%
#   group_by(authorID) %>%
#   mutate(num_pubs = length(unique(pubID))) %>%
#   select(-pubID, -Link) %>%
#   distinct()

usc_authors_2020_23_filtered3 %>%
  filter(firstnameissues == TRUE) %>%
  filter(Div != "Other") -> usc_authors_firstletter

firstletter_merge <- merge(usc_authors_firstletter, usc_authors_2020_23_filtered3 %>% filter(firstnameissues == FALSE), by = c("firstletter", "lastname", "Dept", "Div"))
firstletter_merge %>%
  group_by(authorID.x) %>%
  mutate(n_id = length(unique(authorID.y))) %>% 
  filter(n_id == 1) %>%
  mutate(authorIDs = paste(authorID.y, authorID.x, sep = ";")) -> combine_these_firstletter2
# picking the second author since it has the complete name
# not super confident about a few of them
write.csv(combine_these_firstletter2,
          row.names = FALSE,
          "check_usc_authors_firstletter_lastname_combined.csv")

returned_list <- read.csv("data_manual/First_initial_Last_Name_Author_merge_corrected_JH.xlsx - check_usc_authors_firstletter_l.csv")
combine_these_firstletter <- merge(combine_these_firstletter2, returned_list, 
                                    by.x = c("firstletter", "lastname", "authorID.x", "Dept", "Div"),
                                    by.y = c("firstletter", "lastname", "authorID.x", "Shared.Dept", "Shared.Div")) %>%
  filter(Combine..TRUE_FALSE.)

# combine these, then check it
new_map <- unique(combine_these_firstletter$authorIDs.x)
authorID_new <- sapply(new_map, function(x) {
  strsplit(x, ";")[[1]][1]
})
authorID_old <- sapply(new_map, function(x) {
  paste(strsplit(x, ";")[[1]][-1], collapse = ";")
})
map_df <- data.frame(authorID_new, authorID_old) %>%
  tidyr::separate_rows(authorID_old, sep = ";")

authorID_map <- setNames(map_df$authorID_new, map_df$authorID_old)

new_authorIDs <- sapply(usc_authors_2020_23_filtered3$authorID, function(x) {
  if (as.character(x) %in% names(authorID_map)) {
    authorID_map[as.character(x)]
  } else {
    x
  }
})

new_authorIDs <- unname(new_authorIDs)

usc_authors_2020_23_filtered3$authorID <- new_authorIDs
usc_authors_2020_23_filtered3$matched <- apply(usc_authors_2020_23_filtered3, 1, function(x) {
  grepl(paste0("(",x[['authorID']],")"), x[['name_id']])
})
usc_authors_2020_23_filtered3 %>%
  arrange(desc(matched)) %>%
  group_by(authorID) %>%
  tidyr::fill(initials, FirstSearch, LastSearch, First, Last, Email, PositionTitle, Type, InUSCDirectory, .direction = "downup") %>%
  mutate(affls = paste(unique(affls), collapse = ";"),
         across(c(name, name_id, firstname, lastname, fullname, initials, FirstSearch, LastSearch, First, Last, Email, PositionTitle, Type, InUSCDirectory, firstlast), first)) %>%
  ungroup() %>%
  group_by(authorID, Dept, Div) %>%
  slice(1) -> usc_authors_2020_23_filtered4

# update bridge
new_authorIDs <- sapply(bridge_table3$authorID, function(x) {
  if (as.character(x) %in% names(authorID_map)) {
    authorID_map[as.character(x)]
  } else {
    x
  }
})
new_authorIDs <- unname(new_authorIDs)
bridge_table4 <- bridge_table3
bridge_table4$authorID <- new_authorIDs
bridge_table4 <- bridge_table4 %>%
  distinct()

write.csv(usc_authors_2020_23_filtered4, 
          "data_processed/usc_authors_2020_24_combined.csv",
          row.names = FALSE)
write.csv(bridge_table4,
          "data_processed/usc_bridge_2020_24_combined.csv",
          row.names = FALSE)

# combine same div but other dept?

usc_authors_n_pub <- merge(usc_authors_2020_23_filtered4, 
                           bridge_table4)
# count the number of pubs per author
usc_authors_n_pub <- usc_authors_n_pub %>%
  group_by(authorID) %>%
  mutate(num_pubs = length(unique(pubID))) %>%
  select(-pubID, -Link) %>%
  distinct()

usc_authors_n_pub %>%
  filter(Div != "Other") %>%
  # filter(Dept != "Other") %>%
  arrange(desc(num_pubs)) %>%
  group_by(firstlast, Div) %>%
  mutate(n = length(unique(authorID))) %>%
  filter(n>1) %>%
  summarize(authorIDs = paste(unique(authorID), collapse = ";"),
            hasOther = ifelse(grepl("Other", paste(unique(Dept), collapse = ";")), TRUE, FALSE)
  ) %>%
  filter(hasOther) -> combine_these_firstlast_other


# # combine xx
# 
# usc_authors_2020_23_filtered2$cleanFirst <- gsub("[^a-zA-Z]", " ", usc_authors_2020_23_filtered2$firstname)
# usc_authors_2020_23_filtered2$cleanLast <- gsub("[^a-zA-Z]", " ", usc_authors_2020_23_filtered2$lastname)
# 
# 
# # these are the ids to map
# strsplit(paste(combine_these$authorIDs, collapse = ";"), ";")[[1]] %>% unique-> unique_ids
# 
# # current map
# past_authorIDs <- read.csv("data_processed/past_authorIDs.csv")
# past_authorIDs %>%
#   tidyr::separate_rows(all_authorIDs, sep=";") %>%
#   filter(all_authorIDs != authorID_new) -> separate_past_authorIDs
# past_authorIDs_map <- setNames(separate_past_authorIDs$authorID_new, 
#                                separate_past_authorIDs$all_authorIDs)
# 
# # first check if the past_map is correct
# present <- sapply(usc_authors_2020_23_filtered$authorID, function(x) {
#   any(grepl(x, past_authorIDs$all_authorIDs))
# })
# present2 <- sapply(usc_authors_2020_23_filtered$authorID, function(x) {
#   any(grepl(x, past_authorIDs$authorID_new))
# })
# which(present & !present2) 
# 
# 
# # do these two maps overlap?
# present <- sapply(unique_ids, function(x) {
#   any(grepl(x, past_authorIDs$all_authorIDs))
# })
# present2 <- sapply(unique_ids, function(x) {
#   any(grepl(x, past_authorIDs$authorID_new))
# })
# which(present & !present2)


# pick the authorID with most publications


# a wang + aaron wang -> aaron wang if same dept + div


# same last name + first name but different dept
# same first name + last name but one has a dot instead of full name

# same first and last excluding symbols