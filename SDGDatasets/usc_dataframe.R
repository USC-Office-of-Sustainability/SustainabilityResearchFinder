# combine 2 csv files
a = read.csv("./USC20000.csv")
b = read.csv("./USC4067.csv")
all = rbind(a, b)
# remove duplicate row
all = all[!duplicated(all),]
write.csv(all, file = "./USC_all.csv", row.names = FALSE)


# many to many relationship btwn publication and authors
# publication table, publicationauthor bridge table, author table
publication_author = data.frame()
author = data.frame(matrix(ncol=2,nrow=0, dimnames=list(NULL, c("ID", "Name"))))

auth_affl_vec = strsplit(b$Authors.with.affiliations[1], ";")[[1]]
matched = grep("university of southern california", auth_affl_vec, ignore.case = TRUE)
# for each match split by , 
# name is first entry
for (i in matched) {
  name = trimws(strsplit(auth_affl_vec[i], ",")[[1]][1])
  authID = trimws(strsplit(b$Author.s..ID[1], ";")[[1]][i])
  publication_author = rbind(publication_author, data.frame(DOI = b$DOI[1], AuthorId = authID))
  if (!(authID %in% author$ID)) {
    author = rbind(author, data.frame("ID" = authID, "Name" = name))
  }
  
}

