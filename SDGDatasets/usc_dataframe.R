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

for (j in 1:nrow(tmp)) {
  auth_affl_vec = strsplit(tmp$Authors.with.affiliations[j], ";")[[1]]
  matched = grep("(uni[versity\\.]{0,} (of )?sou?th[ernm]{0,}[,-]? ?california)|(USC)|(keck school of medicine)|(keck medical center)|(keck medical school)", auth_affl_vec, ignore.case = TRUE)
  auth_full_name = strsplit(tmp$Author.full.names[j], ";")[[1]]
  # for each match split by , 
  # name is first entry
  for (i in matched) {
    name = trimws(strsplit(auth_affl_vec[i], ",")[[1]][1])
    lastname = strsplit(name, " ")[[1]][1]
    firstname = strsplit(trimws(strsplit(auth_full_name[i], ",")[[1]][2]), " ")[[1]][1]
    authorID = trimws(strsplit(tmp$Author.s..ID[j], ";")[[1]][i])
    publication_author = rbind(publication_author, data.frame(DOI = tmp$DOI[j], AuthorId = authorID))
    if (!(authorID %in% author$ID)) {
      author = rbind(author, data.frame("ID" = authorID, "Name" = name, "FName" = firstname, "LName" = lastname))
    }
  }
}

# check which(!(tmp$DOI %in% publication_author$DOI))
# tmp$Authors.with.affiliations[9250]

# USC directory
library(RCurl)
library(RJSONIO)
USCDirectoryCookie = readLines("uscdirectory.cookie", warn = FALSE)[1]
con = getCurlHandle(followlocation = TRUE, 
                    ssl.verifypeer = FALSE, 
                    #verbose = TRUE,
                    cookie = USCDirectoryCookie)

author$Department = ""
author$Division = ""
for (i in 1:nrow(author)) {
  tt = getForm("https://uscdirectory.usc.edu/web/directory/faculty-staff/proxy.php", 
             first = author$FName[i],
             last = author$LName[i], 
             curl = con,
             .opts = list(httpheader = c("Accept" = "text/plain, */*; q=0.01",
                                         "Accept-Encoding" = "gzip, deflate, br",
                                         "Accept-Language" = "en-US,en;q=0.9",
                                         "Cache-Control" = "no-cache",
                                         "Connection" = "keep-alive",
                                         "Host" = "uscdirectory.usc.edu",
                                         "Pragma" = "no-cache",
                                         "Referer" = "https://uscdirectory.usc.edu/web/directory/faculty-staff/",
                                           "sec-ch-ua" = '"Google Chrome";v="107", "Chromium";v="107", "Not=A?Brand";v="24"',
                                         "sec-ch-ua-mobile" = "?1",
                                         "sec-ch-ua-platform" = "Android",
                                         "Sec-Fetch-Dest" ="empty",
                                         "Sec-Fetch-Mode" = "cors",
                                         "Sec-Fetch-Site" = "same-origin",
                                         "User-Agent" = "Mozilla/5.0 (Linux; Android 6.0; Nexus 5 Build/MRA58N) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/107.0.0.0 Mobile Safari/537.36",
                                         "X-Requested-With" = "XMLHttpRequest")))
  if (tt != "") {
    vals = fromJSON(tt)
    if (is.null(names(vals))) { # multiple results
      for (i in 1:length(vals)) {
        v = vals[[i]]
        if (author$FName[i] == v$uscdisplaygivenname & author$LName[i] == v$uscdisplaysn) {
          if ("departmentnumber" %in% names(v)) {
            author[i,]$Department = v$departmentnumber
          }
          if ("uscemployeedivision" %in% names(v)) {
            author[i,]$Division = v$uscemployeedivision 
          }
          break
        }
      }
    } else {
      if (author$FName[i] == vals$uscdisplaygivenname & author$LName[i] == vals$uscdisplaysn) {
        if ("departmentnumber" %in% names(vals)) {
          author[i,]$Department = vals$departmentnumber
        }
        if ("uscemployeedivision" %in% names(vals)) {
          author[i,]$Division = vals$uscemployeedivision 
        }
      }
    }
    
  } # else no results
  
}

write.csv(author, file = "./USCauthorsSDG1to11.csv", row.names = FALSE)
write.csv(publication_author, file = "./USCpubidauthidSDG1to11.csv", row.names = FALSE)

auth_info = merge(publication_author, author, by.x = "AuthorId", by.y = "ID")
pub_auth_full = merge(auth_info, tmp, by.x = "DOI", by.y = "DOI")

write.csv(pub_auth_full, file = "./USCpubauthfullSDG1to11.csv", row.names = FALSE)

# SCOPUS

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
