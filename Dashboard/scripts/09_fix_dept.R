# fix departments
library(dplyr)
library(readxl)
dept_edits <- read.csv("just_departments_edits2.csv") %>%
  filter(Change == TRUE)
usc_authors <- read.csv("data_processed/usc_authors_law_fixed2.csv")

for (i in 1:nrow(dept_edits)) {
  idx <- which(usc_authors$Dept == dept_edits$Department[i] & 
          usc_authors$Div == dept_edits$Division[i])
  usc_authors$Dept[idx] <- dept_edits$Change.to[i]
}

# school has dept + other -> remove other
usc_authors %>%
  group_by(authorID, Div) %>%
  add_count() %>%
  mutate(hasOther = grepl("Other", Dept)) %>% 
  filter(!(n > 1 & hasOther == TRUE)) %>% 
  select(-n, -hasOther) -> usc_authors

# div other should not have other divs
usc_authors %>%
  group_by(authorID) %>%
  mutate(n = length(unique(Div))) %>%
  mutate(hasOther = grepl("Other", Div)) %>%
  filter(!(n>1 & hasOther == TRUE)) %>%
  select(-n, -hasOther) -> usc_authors

# metrans
metrans <- read_excel("data_raw/USC Researchers_METRANS.xlsx")
metrans$scopus_id <- ""
for (i in 1:nrow(metrans)) {
  matched_lastname <- which(metrans$lastname[i]==usc_authors$lastname)
  if (length(matched_lastname) == 0) next
  matched_lastname_firstname <- which(startsWith(usc_authors[matched_lastname,]$firstname, metrans$firstname[i]))
  if (length(matched_lastname_firstname) == 0) next
  matched_indices <- matched_lastname[matched_lastname_firstname]
  print(length(matched_indices))
  print(usc_authors[matched_indices,])
  stopifnot(length(unique(usc_authors[matched_indices,]$authorID))==1) # should be 1
  metrans$scopus_id[i] <- unique(usc_authors[matched_indices,]$authorID)
  # new row for metrans dept + other div
  new_row <- usc_authors[matched_indices[1],]
  new_row$Dept <- metrans$DEPARTMENT_AffiliationAdd[i]
  new_row$Div <- "Other"
  usc_authors <- rbind(usc_authors, new_row)
  # email update
  if (is.na(usc_authors[matched_indices[1],]$Email)) {
    usc_authors[matched_indices,]$Email <- metrans$Email[i]
  }
}
# rename + move to other (div)
usc_authors$Div[which(usc_authors$Dept == "METRANS Transportation Center")] <- "Other"
usc_authors$Dept[which(usc_authors$Dept == "METRANS Transportation Center")] <- metrans$DEPARTMENT_AffiliationAdd[1]
usc_authors_final <- usc_authors %>% distinct()

write.csv(usc_authors_final,
          "data_processed/usc_authors_law_fixed_dept.csv",
          row.names = FALSE)

write.csv(metrans,
          "USC_Researchers_METRANS.csv",
          row.names = FALSE)
