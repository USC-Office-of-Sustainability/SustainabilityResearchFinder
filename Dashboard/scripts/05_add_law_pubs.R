# 05 add law pubs
library(here)
library(dplyr)

# 57657883900 is Amour J
usc_law_pubs <- read.csv(here::here("data_raw/USC_Law.csv"))
usc_data <- read.csv(here::here("data_processed/usc_pubs.csv")) # usc_pubs_all.csv max pubID is 25915 which is the same as usc_pubs.csv

# check duplicate entries -> manually removed
usc_law_pubs$LawPubID[duplicated(usc_law_pubs %>% select(Title, Year, USC.Author_Last_First))]

together <- merge(usc_law_pubs, usc_data, 
                  by.x = c("Title", "Year"), 
                  by.y = c("Titles", "Year"), 
                  all.x = TRUE)
law_pubs_ids <- together[is.na(together$pubID),]$LawPubID
law_pubs_to_add <- usc_law_pubs %>%
  filter(LawPubID %in% law_pubs_ids) %>%
  filter(Include.in.Dashboard == "Yes")


usc_authors <- read.csv(here::here("data_processed/author_dept_11_6_23.csv"))
unique(law_pubs_to_add$Author.s..ID)[!unique(law_pubs_to_add$Author.s..ID) %in% usc_authors$authorID]

# author info
law_pubs_to_add$authorID <- law_pubs_to_add$Author.s..ID
law_pubs_to_add$fullname <- law_pubs_to_add$USC.Author_Last_First
for (i in 1:nrow(law_pubs_to_add)) {
  law_pubs_to_add$firstname[i] <- trimws(strsplit(law_pubs_to_add$USC.Author_Last_First[i], ",")[[1]][2])
  law_pubs_to_add$lastname[i] <- trimws(strsplit(law_pubs_to_add$USC.Author_Last_First[i], ",")[[1]][1])
}
law_pubs_to_add$Dept <- law_pubs_to_add$Focal.USC.Author.Department
law_pubs_to_add$Div <- law_pubs_to_add$Focal.USC.Author.Division.School
law_pubs_to_add$InUSCDirectory <- FALSE
law_pubs_to_add$Pattern <- NA
law_pubs_to_add$id <- NA
law_pubs_to_add$name_id <- paste0(law_pubs_to_add$fullname, " (", law_pubs_to_add$authorID ,")")
law_pubs_to_add$initials <- sapply(law_pubs_to_add$firstname, function(x) {
  paste0(paste(substr(strsplit(x, "-| ")[[1]], 1, 1), collapse ="."), ".")
})
law_pubs_to_add$name <- paste0(law_pubs_to_add$lastname, " ", law_pubs_to_add$initials)
empty_cols <- setdiff(names(usc_authors), names(law_pubs_to_add))
law_pubs_to_add[,empty_cols] <- ""

# pub info
law_pubs_to_add$pubID <- seq(max(usc_data$pubID)+1, 
                             max(usc_data$pubID)+nrow(law_pubs_to_add))
names(law_pubs_to_add)[names(law_pubs_to_add) == 'Authors_dont_use'] <- 'Authors'
# law_pubs_to_add$Author.full.names
# law_pubs_to_add$Author.s..ID
names(law_pubs_to_add)[names(law_pubs_to_add) == 'Title'] <- 'Titles'
names(law_pubs_to_add)[names(law_pubs_to_add) == 'Publication.Type..Article.or.Book..'] <- 'Document.Type'
law_pubs_to_add$Source <- law_pubs_to_add$Source.title
empty_cols <- setdiff(names(usc_data), names(law_pubs_to_add))
law_pubs_to_add[,empty_cols] <- ""

law_authors <- law_pubs_to_add %>%
  select(names(usc_authors)) %>%
  distinct %>%
  filter(!authorID %in% usc_authors$authorID)
law_bridge <- law_pubs_to_add %>%
  select(pubID, Link, authorID)
law_pubs <- law_pubs_to_add %>%
  select(names(usc_data)) %>%
  distinct

usc_bridge <- read.csv(here::here("data_processed/bridge_after_fix_11_6_23.csv"))
usc_bridge_law <- rbind(usc_bridge, law_bridge)
write.csv(usc_bridge_law,
          file = here::here("data_processed/bridge_law.csv"),
          row.names = FALSE)
usc_data_law <- rbind(usc_data, law_pubs)
write.csv(usc_data_law,
          file = here::here("data_processed/usc_pubs_law.csv"),
          row.names = FALSE)
law_authors$Div <- trimws(gsub("USC", "", law_authors$Div))
usc_authors_law <- rbind(usc_authors, law_authors)
write.csv(usc_authors_law,
          file = here::here("data_processed/usc_authors_law.csv"),
          row.names = FALSE)

