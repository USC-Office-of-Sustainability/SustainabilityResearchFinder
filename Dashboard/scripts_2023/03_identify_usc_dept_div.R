# identify department and division from the affiliations
# based on 03_get_usc_author_info.R
# focused_usc_authors <- read.csv("data_processed/all_usc_authors_2023.csv")
focused_usc_authors_dept_div <- focused_usc_authors %>%
  group_by(authorID) %>%
  summarize(name = first(auth_name),
            name_id = first(auth_full_names),
            affls = paste(unique(auth_affl_vec), collapse = ";"),
            fullname = first(full_name),
            lastname = first(last_name),
            firstname = first(first_name))

# actual number of new ppl
# sum(!unique(focused_usc_authors$authorID) %in% existing_authorID)


focused_usc_authors_dept_div$Dept <- ""
focused_usc_authors_dept_div$Div <- ""


usc_schools <- read.csv(here::here("data_raw/usc_schools.csv"))
usc_departments <- read.csv(here::here("data_raw/usc_departments.csv"))

# takes 5 min
for (i in 1:nrow(focused_usc_authors_dept_div)) {
  trimmed_affl_seg <- strsplit(focused_usc_authors_dept_div[i,]$affls, ";")[[1]]
  affl_seg <- sapply(trimmed_affl_seg, function(x) {
    trimws(strsplit(x, ",")[[1]][-1])
  })
  affl_segs <- unlist(affl_seg)
  affl_segs <- unname(affl_segs)
  # affl_segs <- unique(affl_segs) # breaks up names w ,
  affiliations <- paste(affl_segs, collapse = " ")
  matched_dept <- apply(usc_departments, 1, function(x) {
    if (grepl(x['Pattern'], affiliations, ignore.case = TRUE)) { # want to ignore case
      x
    }
  })
  matched_dept <- matched_dept[lengths(matched_dept) != 0]
  departments <- c()
  divisions <- c()
  for (j in 1:length(matched_dept)) {
    departments <- append(departments, matched_dept[[j]]["Department.Group"])
    divisions <- append(divisions, matched_dept[[j]]["School.Institute.Center"])
  }
  departments <- unname(departments)
  divisions <- unname(divisions)
  focused_usc_authors_dept_div[i,]$Dept <- paste(departments[!duplicated(departments)], collapse = ";")
  
  matched_schools <- apply(usc_schools, 1, function(x) {
    if (grepl(x['pattern'], affiliations, ignore.case = TRUE)) {
      x['name']
    }
  })
  
  all_schools <- c(divisions, unname(unlist(matched_schools)))
  all_schools <- all_schools[!duplicated(all_schools)]
  
  focused_usc_authors_dept_div[i,]$Div <- paste(all_schools, collapse = ";")
}

write.csv(focused_usc_authors_dept_div,
          here::here("data_processed/focused_usc_authors_dept_div.csv"),
          row.names = FALSE)

# some of the div and dept have multiple -> should have 1
# 04_create_dept_div.R
focused_usc_authors_dept_div_separate <- focused_usc_authors_dept_div %>%
  tidyr::separate_rows(Dept, sep=";") %>%
  mutate(Dept = ifelse(Dept == "", "Other", Dept)) %>%
  tidyr::separate_rows(Div, sep=";") %>%
  mutate(Div = ifelse(Div == "", "Other", Div))

usc_departments <- read.csv("data_raw/usc_departments.csv") %>%
  rename(Division = School.Institute.Center,
         Department = Department.Group)
df <- data.frame(Division = c(unique(usc_departments$Division), "Other"),
                 Department = "Other", Pattern = "")
usc_departments_other <- rbind(usc_departments, df)
usc_departments_other$id <- 1:nrow(usc_departments_other)


focused_usc_authors_dept_div_separate_fixed <- merge(focused_usc_authors_dept_div_separate, usc_departments_other,
                     by.x = c("Dept", "Div"),
                     by.y = c("Department", "Division"),
                     all.x = TRUE)

focused_usc_authors_dept_div_separate_fixed %>%
  mutate(DeptDivExists = ifelse(is.na(id), FALSE, TRUE)) %>%
  mutate(IncorrectDiv = ifelse(DeptDivExists == FALSE & Dept %in% usc_departments_other$Department, TRUE, FALSE)) %>%
  filter(IncorrectDiv != TRUE) %>%
  select(-DeptDivExists, -IncorrectDiv) -> focused_usc_authors_dept_div_separate_fixed

# lab -> other
focused_usc_authors_dept_div_separate_fixed[grep("Lab", focused_usc_authors_dept_div_separate_fixed$Dept),]$Dept <- "Other"

# school has dept + other -> remove other
focused_usc_authors_dept_div_separate_fixed %>%
  group_by(authorID, Div) %>%
  add_count() %>%
  mutate(hasOther = grepl("Other", Dept)) %>% 
  filter(!(n > 1 & hasOther == TRUE)) %>% 
  select(-n, -hasOther) -> focused_usc_authors_dept_div_separate_fixed


# additional dept fix 09_fix_dept.R
dept_edits <- read.csv("data_manual/just_departments_edits2.csv") %>%
  filter(Change == TRUE)

for (i in 1:nrow(dept_edits)) {
  idx <- which(focused_usc_authors_dept_div_separate_fixed$Dept == dept_edits$Department[i] & 
                 focused_usc_authors_dept_div_separate_fixed$Div == dept_edits$Division[i])
  focused_usc_authors_dept_div_separate_fixed$Dept[idx] <- dept_edits$Change.to[i]
}

# school has dept + other -> remove other
focused_usc_authors_dept_div_separate_fixed %>%
  group_by(authorID, Div) %>%
  add_count() %>%
  mutate(hasOther = grepl("Other", Dept)) %>% 
  filter(!(n > 1 & hasOther == TRUE)) %>% 
  select(-n, -hasOther) -> focused_usc_authors_dept_div_separate_fixed

# div other should not have other divs
focused_usc_authors_dept_div_separate_fixed %>%
  group_by(authorID) %>%
  mutate(n = length(unique(Div))) %>%
  mutate(hasOther = grepl("Other", Div)) %>%
  filter(!(n>1 & hasOther == TRUE)) %>%
  select(-n, -hasOther) -> focused_usc_authors_dept_div_separate_fixed

# metrans
library(readxl)
metrans <- read_excel("data_raw/USC Researchers_METRANS.xlsx")
focused_usc_authors_dept_div_separate_fixed$Email <- ""
# metrans$scopus_id <- ""
for (i in 1:nrow(metrans)) {
  matched_lastname <- which(metrans$lastname[i]==focused_usc_authors_dept_div_separate_fixed$lastname)
  if (length(matched_lastname) == 0) next
  matched_lastname_firstname <- which(startsWith(focused_usc_authors_dept_div_separate_fixed[matched_lastname,]$firstname, metrans$firstname[i]))
  if (length(matched_lastname_firstname) == 0) next
  matched_indices <- matched_lastname[matched_lastname_firstname]
  print(length(matched_indices))
  print(focused_usc_authors_dept_div_separate_fixed[matched_indices,])
  stopifnot(length(unique(focused_usc_authors_dept_div_separate_fixed[matched_indices,]$authorID))==1) # should be 1
  # metrans$scopus_id[i] <- unique(focused_usc_authors_dept_div[matched_indices,]$authorID)
  # new row for metrans dept + other div
  new_row <- focused_usc_authors_dept_div_separate_fixed[matched_indices[1],]
  new_row$Dept <- metrans$DEPARTMENT_AffiliationAdd[i]
  new_row$Div <- "Other"
  focused_usc_authors_dept_div_separate_fixed <- rbind(focused_usc_authors_dept_div_separate_fixed, new_row)
  # email update
  if (is.na(focused_usc_authors_dept_div_separate_fixed[matched_indices[1],]$Email)) {
    focused_usc_authors_dept_div_separate_fixed[matched_indices,]$Email <- metrans$Email[i]
  }
}
# rename + move to other (div)
focused_usc_authors_dept_div_separate_fixed$Div[which(focused_usc_authors_dept_div_separate_fixed$Dept == "METRANS Transportation Center")] <- "Other"
focused_usc_authors_dept_div_separate_fixed$Dept[which(focused_usc_authors_dept_div_separate_fixed$Dept == "METRANS Transportation Center")] <- metrans$DEPARTMENT_AffiliationAdd[1]
focused_usc_authors_dept_div_separate_fixed <- focused_usc_authors_dept_div_separate_fixed %>% distinct()

write.csv(focused_usc_authors_dept_div_separate_fixed,
          here::here("data_processed/focused_usc_authors_dept_div_separate.csv"),
          row.names = FALSE)

# what to do with the authors that already exist? 
# add the info

# combine the IDs that were previously (2020-22) combined

focused_usc_authors_dept_div_separate_fixed <- read.csv("data_processed/focused_usc_authors_dept_div_separate.csv")
past_authorIDs <- read.csv("data_processed/past_authorIDs.csv")
past_authorIDs %>%
  tidyr::separate_rows(all_authorIDs, sep=";") %>%
  filter(all_authorIDs != authorID_new) -> separate_past_authorIDs
past_authorIDs_map <- setNames(separate_past_authorIDs$authorID_new, 
                               separate_past_authorIDs$all_authorIDs)

new_authorIDs <- sapply(focused_usc_authors_dept_div_separate_fixed$authorID, function(x) {
  if (as.character(x) %in% separate_past_authorIDs$all_authorIDs) {
    past_authorIDs_map[as.character(x)]
  } else {
    x
  }
})

new_authorIDs <- unname(new_authorIDs)
# only 155 changed
sum(focused_usc_authors_dept_div_separate_fixed$authorID != new_authorIDs)
focused_usc_authors_dept_div_separate_fixed$authorID <- new_authorIDs

write.csv(focused_usc_authors_dept_div_separate_fixed,
          "data_processed/focused_usc_authors_dept_div_separate_combine_past.csv",
          row.names = FALSE)

# update bridge too
bridge_table <- read.csv("data_processed/bridge_table_2023.csv")
new_authorIDs <- sapply(bridge_table$authorID, function(x) {
  if (as.character(x) %in% separate_past_authorIDs$all_authorIDs) {
    past_authorIDs_map[as.character(x)]
  } else {
    x
  }
})
new_authorIDs <- unname(new_authorIDs)
bridge_table$authorID <- new_authorIDs
write.csv(bridge_table,
          "data_processed/bridge_table_2023_combine_past.csv",
          row.names = FALSE)

