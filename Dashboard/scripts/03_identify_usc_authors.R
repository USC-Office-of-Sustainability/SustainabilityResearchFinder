# Identify USC authors in the list of publications by
# 1. parsing authors with affiliations text
# 2. searching first 10 author's current affiliation on scopus via API

library(here)
library(rscopus)

usc_regex <- paste0("(uni[versity\\.]{0,} (of )?sou?th[ernm]{0,}[,-]? ?",
                   "california)|(USC)|(keck school of medicine)|(keck medical",
                   " center)|(keck medical school)")
usc_affiliation_ids <- c(60029311, 60015183, 60022143, 60009207, 60015400,
                         60013994, 60099658, 60019009, 60086699, 60005801,
                         60026672, 60006209, 60268548)

elsevier_api_key <- Sys.getenv("Elsevier_API")

publication_author <- data.frame()
author <- data.frame(matrix(ncol = 2, nrow = 0, dimnames = list(NULL, c("ID", "Name"))))

usc_data <- read.csv(here::here("data_processed/usc_pubs.csv"))
usc_data$ScopusAPI <- FALSE

tmp <- usc_data[sample(1:nrow(usc_data), 10), ]

for (i in 1:nrow(usc_data)) {
  # split
}

# check if

# first round - split by ; then only look for USC if Author full names and Authors with affiliations can be split the same
authors_working = apply(usc_data, 1, function(x) {
  res = data.frame()
  auth_full_name = trimws(strsplit(x['Author.full.names'], ";")[[1]])
  auth_affl_vec = strsplit(x['Authors.with.affiliations'], ";")[[1]]
  if (length(auth_full_name) == length(auth_affl_vec)) {
    matched = grep(usc_regex, auth_affl_vec, ignore.case = TRUE)
    for (i in matched) {
      name = trimws(strsplit(auth_affl_vec[i], ",")[[1]][1])
      lastname = strsplit(name, " ")[[1]][1]
      firstname = strsplit(trimws(strsplit(auth_full_name[i], ",")[[1]][2]), " ")[[1]][1]
      authorID = trimws(strsplit(x['Author.s..ID'], ";")[[1]][i])
      res <- rbind(res, data.frame(pubID = x['X'], link = x['Link'], authorID = authorID, "Name" = name, 
                                   "FName" = firstname, "LName" = lastname))
    }
  }
  res
})

authors_working <- do.call(rbind, authors_working)

write.csv(authors_working,
          here::here("data_processed/authors_working.csv"),
          row.names = FALSE)

# testing --------------------------------------
missing_pubs <- setdiff(1:nrow(usc_data),as.numeric(authors_working$pubID))

current_pubs <- usc_data[missing_pubs,]

tmp <- head(current_pubs)

y = apply(current_pubs, 1, function(x) {
  length(strsplit(x['Author.s..ID'], ";")[[1]])
})
