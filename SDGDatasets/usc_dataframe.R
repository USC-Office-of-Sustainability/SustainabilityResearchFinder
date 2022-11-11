# combine 2 csv files
a = read.csv("./USCData/USC20_21.csv")
b = read.csv("./USCData/USC22_23.csv")
all = rbind(a, b)
# remove duplicate rows
all = all[!duplicated(all),]
write.csv(all, file = "./USCData/USC_all.csv", row.names = FALSE)

# combine all files in folder
ff = list.files("./USCData/bySDG", pattern = "csv", full.names = TRUE)

tmp = lapply(ff, function(filename) {
  d = read.csv(filename)
  d$Primary.SDG = gsub("[^[:digit:]]*([[:digit:]]*)","\\1", filename) # USC_SDG#.csv
  d
})
tmp = do.call(rbind, tmp)


# many to many relationship btwn publication and authors
# publication table, publicationauthor bridge table, author table
publication_author = data.frame() # DOI and AuthorID
author = data.frame(matrix(ncol=2,nrow=0, dimnames=list(NULL, c("ID", "Name"))))

for (j in 1:10) {
  auth_affl_vec = strsplit(tmp$Authors.with.affiliations[j], ";")[[1]]
  matched = grep("(uni[versity\\.]{0,} (of )?sou?th[ernm]{0,}[,-]? ?california)|(USC)|(keck school of medicine)|(keck medical center)|(keck medical school)", auth_affl_vec, ignore.case = TRUE)
  # for each match split by , 
  # name is first entry
  for (i in matched) {
    name = trimws(strsplit(auth_affl_vec[i], ",")[[1]][1])
    authorID = trimws(strsplit(tmp$Author.s..ID[j], ";")[[1]][i])
    publication_author = rbind(publication_author, data.frame(DOI = tmp$DOI[j], AuthorId = authorID))
    if (!(authorID %in% author$ID)) {
      author = rbind(author, data.frame("ID" = authorID, "Name" = name))
    }
  }
}

# check which(!(tmp$DOI %in% publication_author$DOI))
# tmp$Authors.with.affiliations[9250]

api_key = Sys.getenv('Elsevier_API')

#install.packages("rscopus")
library(rscopus)

usc_affiliation_ids = c(60029311, 60015183, 60022143, 60009207, 60015400, 60013994, 60099658, 60019009, 60086699, 60005801, 60026672, 60006209, 60268548)

author$affiliationid = ""
author$affiliationname = ""
author$lastname = ""
author$firstname = ""
for (i in 1:nrow(author)) {
  x = get_complete_author_info(api_key = api_key, au_id = author$ID[i])
  author[i,]$affiliationid = x$content$`search-results`$entry[[1]]$`affiliation-current`$`affiliation-id`
  author[i,]$affiliationname = x$content$`search-results`$entry[[1]]$`affiliation-current`$`affiliation-name`
  author[i,]$lastname = x$content$`search-results`$entry[[1]]$`preferred-name`$surname
  author[i,]$firstname = strsplit(x$content$`search-results`$entry[[1]]$`preferred-name`$`given-name`, " ")[[1]][1]
}

author[author$affiliationid %in% usc_affiliation_ids,]
