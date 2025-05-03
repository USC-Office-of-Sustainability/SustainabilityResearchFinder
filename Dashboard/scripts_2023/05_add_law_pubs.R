# add law pubs before combining authors?
usc_authors_2020_23 <- read.csv("data_processed/usc_authors_2020_24.csv")
bridge_2020_23 <- read.csv("data_processed/bridge_table_2020_24.csv")
usc_pubs_2020_23 <- read.csv("data_processed/usc_pubs_2020_24.csv")

law_2023 <- read.csv("data_raw/USC_Law_2023.csv")
law_2024 <- read.csv("data_raw/USC_Law_2024.csv")
sum(duplicated(law_2023 %>% select(Title, Year, USC.Author_Last_First))) # no duplicates in spreadsheet

binded_2023_2024 <- rbind(law_2023, law_2024)
# remove the law pubs that are already in the list of publications
law_pubs_to_add <- merge(binded_2023_2024, usc_pubs_2020_23,
                  by.x = c("Title", "Year"), 
                  by.y = c("Titles", "Year"),
                  suffixes = c("", ".y"),
                  all.x = TRUE) %>%
  filter(is.na(pubID)) %>%
  select(names(law_2023))
# assign pubID
starting_pubID <- max(usc_pubs_2020_23$pubID) + 1
ending_pubID <- starting_pubID + nrow(law_pubs_to_add) - 1
missing_pubIDs <- seq(from = starting_pubID, to = ending_pubID, by = 1)
law_pubs_to_add$LawPubID <- missing_pubIDs

# law info
law_authorID <- merge(law_2023, usc_authors_2020_23, by.x = c("USC.Author_Last_First"), by.y = c("fullname"))
# use law_authorID$authorID to assign authorIDs manually into the spreadsheet
law_authors <- law_pubs_to_add %>%
  rename(pubID = LawPubID,
         Document.Type = Publication.Type..Article.or.Book..,
         fullname = USC.Author_Last_First,
         Dept = Focal.USC.Author.Department,
         Div = Focal.USC.Author.Division.School,
         authorID = Author.s..ID,
         Titles = Title) %>%
  select(-Authors_dont_use, -Author.full.names) %>%
  mutate(name_id = paste0(fullname, " (", authorID, ")"))
law_authors$Div <- trimws(gsub("USC", "", law_authors$Div))
law_authors$firstname <- ""
law_authors$lastname <- ""
for (i in 1:nrow(law_authors)) {
  law_authors$firstname[i] <- trimws(strsplit(law_authors$fullname[i], ",")[[1]][2])
  law_authors$lastname[i] <- trimws(strsplit(law_authors$fullname[i], ",")[[1]][1])
}
law_authors$initials <- sapply(law_authors$firstname, function(x) {
  paste0(paste(substr(strsplit(x, "-| ")[[1]], 1, 1), collapse ="."), ".")
})
law_authors$name <- paste0(law_authors$lastname, " ", law_authors$initials)

# create law bridge, authors, pubs

law_bridge <- law_authors %>%
  select(pubID, Link, authorID)
empty_cols <- setdiff(names(usc_pubs_2020_23), names(law_authors))
law_authors[,empty_cols] <- ""
law_pubs <- law_authors %>%
  select(names(usc_pubs_2020_23)) %>%
  distinct
empty_cols <- setdiff(names(usc_authors_2020_23), names(law_authors))
law_authors[,empty_cols] <- ""
law_authors2 <- law_authors %>%
  select(names(usc_authors_2020_23)) %>%
  distinct
  # filter(!authorID %in% usc_authors_2020_23$authorID)

usc_pubs_law <- rbind(usc_pubs_2020_23, law_pubs)
write.csv(usc_pubs_law,
          "data_processed/usc_pubs_law_2020_24.csv",
          row.names = FALSE)
usc_authors_law <- rbind(usc_authors_2020_23, law_authors2)
# fix law departments
# all law departments are Law
usc_authors_law %>%
  mutate(Dept = ifelse(grepl("^Law", Dept) & Dept != "Law Immigration Clinic", "Law", Dept)) %>%
  mutate(Dept = ifelse(Div == "Gould School of Law" & Dept == "Other", "Law", Dept)) -> usc_authors_law
# need to fix law authors - merge multiple entries based on ID, fix law + law JD departments
# usc_authors_law %>%
#   mutate(Dept = ifelse("Law Budget Office" == Dept, "Law", Dept)) %>%
#   distinct() %>%
#   arrange(Dept) %>%
#   group_by(authorID, Div) %>%
#   mutate(Depts = paste(unique(Dept), collapse = ";")) %>%
#   mutate(doublelaw = ifelse(Depts == "Law;Law JD Operations", TRUE, FALSE)) %>%
#   mutate(keep = ifelse(Dept == "Law" & doublelaw, FALSE, TRUE)) %>%
#   ungroup() %>%
#   group_by(authorID) %>%
#   tidyr::fill(Email, .direction = "updown") %>%
#   ungroup() %>%
#   filter(keep == TRUE) %>% 
#   group_by(authorID, Dept, Div) %>%
#   slice(1) %>% View
# sum(duplicated(usc_authors_law))
write.csv(usc_authors_law,
          "data_processed/usc_authors_law_2020_24.csv",
          row.names = FALSE)
usc_bridge_law <- rbind(bridge_2020_23, law_bridge)
write.csv(usc_bridge_law,
          "data_processed/usc_bridge_law_2020_24.csv",
          row.names = FALSE)
