# combine 2 csv files
a = read.csv("./USCpart1.csv")
b = read.csv("./USCpart2.csv")
all = rbind(a, b)
# remove duplicate rows
all = all[!duplicated(all),]
write.csv(all, file = "./USC_all.csv", row.names = FALSE)


# many to many relationship btwn publication and authors
# publication table, publicationauthor bridge table, author table
publication_author = data.frame() # DOI and AuthorID
author = data.frame(matrix(ncol=2,nrow=0, dimnames=list(NULL, c("ID", "Name"))))

for (j in 1:nrow(b)) {
  auth_affl_vec = strsplit(b$Authors.with.affiliations[j], ";")[[1]]
  matched = grep("university of southern california", auth_affl_vec, ignore.case = TRUE)
  # misses Keck School of Medicine, University of South California
  # Univ. of Southern California
  # University of Southerm California
  # University of Southern, California
  # USC Spine Center
  # USC/LAC+USC Medical Center
  # Keck School of Medicine USC
  # Keck University School of Medicine at USC
  # Keck School of Medicine at USC
  # Keck School of Medicine, USC
  # Keck School of Medicine, University of South California
  # Univeristy of Southern California
  # for each match split by , 
  # name is first entry
  for (i in matched) {
    name = trimws(strsplit(auth_affl_vec[i], ",")[[1]][1])
    authorID = trimws(strsplit(b$Author.s..ID[j], ";")[[1]][i])
    publication_author = rbind(publication_author, data.frame(DOI = b$DOI[j], AuthorId = authorID))
    if (!(authorID %in% author$ID)) {
      author = rbind(author, data.frame("ID" = authorID, "Name" = name))
    }
  }
}

# check which!(b$DOI %in% publication_author$DOI)