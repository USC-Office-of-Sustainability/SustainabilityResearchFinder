# fix authors + bridge manually

usc_bridge <- read.csv("data_processed/bridge_law.csv")
usc_authors <- read.csv("data_processed/usc_authors_law.csv")

# 57214836310 <- 57224679598 for Prieto Rudolphy M.
usc_authors <- usc_authors[-which(usc_authors$authorID == 57224679598),]
usc_bridge$authorID[which(usc_bridge$authorID == 57224679598)] <- 57214836310

# Gould1 -> 57657883900 for Jody Armour
usc_authors <- usc_authors[-which(usc_authors$authorID == "Gould1"),]
usc_bridge$authorID[which(usc_bridge$authorID == "Gould1")] <- 57657883900

#  57216528127 -> 57692525700 for Adam Leventhal
usc_authors <- usc_authors[-which(usc_authors$authorID == 57216528127),]
usc_bridge$authorID[which(usc_bridge$authorID == 57216528127)] <- 57692525700

# 57215497016 -> 57073146400 for Ibrahim Burak
usc_authors <- usc_authors[-which(usc_authors$authorID == 57215497016),]
usc_bridge$authorID[which(usc_bridge$authorID == 57215497016)] <- 57073146400

# 57189185230 -> 57204376126
# 57219003786 -> 57204376126 for Cameron J. Thrash
usc_authors <- usc_authors[-which(usc_authors$authorID == 57189185230),]
usc_bridge$authorID[which(usc_bridge$authorID == 57189185230)] <- 57204376126
usc_authors <- usc_authors[-which(usc_authors$authorID == 57219003786),]
usc_bridge$authorID[which(usc_bridge$authorID == 57219003786)] <- 57204376126
usc_authors$firstname[which(usc_authors$authorID == 57204376126)] <- "Cameron J."
usc_authors$lastname[which(usc_authors$authorID == 57204376126)] <- "Thrash"
usc_authors$fullname[which(usc_authors$authorID == 57204376126)] <- "Thrash, Cameron J."

# 56828355600 Annenberg
usc_authors[which(usc_authors$authorID == 56828355600),]$Dept <- "Annenberg School for Communication and Journalism"
usc_authors[which(usc_authors$authorID == 56828355600),]$Email <- "chi.zhang.7@usc.edu"
usc_authors[which(usc_authors$authorID == 56828355600),]$Type <- ""
usc_authors[which(usc_authors$authorID == 56828355600),]$InUSCDirectory <- FALSE
usc_authors[which(usc_authors$authorID == 56828355600),]$id <- ""

write.csv(usc_authors,
          "data_processed/usc_authors_law_fixed.csv",
          row.names = FALSE)
write.csv(usc_bridge,
          "data_processed/bridge_law_fixed.csv",
          row.names = FALSE)
