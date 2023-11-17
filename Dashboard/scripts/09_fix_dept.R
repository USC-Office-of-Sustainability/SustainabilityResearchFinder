# fix departments

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

write.csv(usc_authors,
          "data_processed/usc_authors_law_fixed_dept.csv",
          row.names = FALSE)
