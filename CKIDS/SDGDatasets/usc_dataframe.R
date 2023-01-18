# combine 2 csv files
a = read.csv("./USCData/USC20_21.csv")
b = read.csv("./USCData/USC22_23.csv")
ab = rbind(a, b)
# remove duplicate rows
ab = ab[!duplicated(ab),]
write.csv(ab, file = "./USCData/USC_all.csv", row.names = FALSE)

# combine all files in folder
ff = list.files("./USCData/bySDG", pattern = "csv", full.names = TRUE)

tmp = lapply(ff, function(filename) {
  d = read.csv(filename)
  d$Primary.SDG = gsub("[^[:digit:]]*([[:digit:]]*)","\\1", filename) # USC_SDG#.csv
  d
})
tmp = do.call(rbind, tmp)
write.csv(tmp, file = "USC_SDG1to16.csv", row.names = FALSE)

allUSC = read.csv("./USCData/USC_all.csv")

df = merge(allUSC, tmp, all.x = TRUE)

no_sdg = df[which(is.na(df$Primary.SDG)),]
write.csv(no_sdg, file = "USC_SDG0.csv", row.names = FALSE)
# some pub are duplicated since they have different primary SDG
# some pub's DOI == "" so can't use DOI as unique primary key
tmp$X = seq(1,nrow(tmp)) # primary key (PK)


# many to many relationship btwn publication and authors]
# publication table, publicationauthor bridge table, author table
publication_author = data.frame() # pub PK and AuthorID
author = data.frame(matrix(ncol=2,nrow=0, dimnames=list(NULL, c("ID", "Name"))))

tmp = read.csv("./USCData/USC_all.csv")
tmp$X = seq(1, nrow(tmp))
tmp$ScopusAPI = FALSE

for (j in 1:nrow(tmp)) {
  auth_full_name = trimws(strsplit(tmp$Author.full.names[j], ";")[[1]])
  n = length(auth_full_name)
  auth_affl_vec = strsplit(tmp$Authors.with.affiliations[j], ";")[[1]]
  if (length(auth_affl_vec) != n) { # some extra ;
    auth_affl_vec = strsplit(tmp$Authors.with.affiliations[j], ";\\s(?=([^\\(\\); ]* ){1,3}[^a-z]\\.)", perl = TRUE)[[1]]
  }
  matched = grep("(uni[versity\\.]{0,} (of )?sou?th[ernm]{0,}[,-]? ?california)|(USC)|(keck school of medicine)|(keck medical center)|(keck medical school)", auth_affl_vec, ignore.case = TRUE)
  
  if (all(grepl(tmp$Affiliations[j], auth_affl_vec, fixed = TRUE)) &
      grepl(" with ", tmp$Affiliations[j])) {
    print(j)
    print("all affiliations were the same")
    auth_affl_vec = strsplit(tmp$Affiliations[j], "(?<=[^A-Z]\\.)\\s(?=[A-Z])", perl=T)[[1]]
    
    auth_affl_vec = sapply(auth_affl_vec, function(x) {
      m = regexpr("(with|was)", x)
      substring(x, m)
    })
    
    if (length(auth_affl_vec) == length(auth_full_name)) {
      matched = grep("(uni[versity\\.]{0,} (of )?sou?th[ernm]{0,}[,-]? ?california)|(USC)|(keck school of medicine)|(keck medical center)|(keck medical school)", auth_affl_vec, ignore.case = TRUE)
      
      for (i in matched) {
        name = trimws(strsplit(tmp$Authors[j], ";")[[1]][i])
        lastname = strsplit(name, " ")[[1]][1]
        fullnameid = trimws(strsplit(tmp$Author.full.names[j], ";")[[1]][i])
        firstname = strsplit(fullnameid, " ")[[1]][2]
        authorID = trimws(strsplit(tmp$Author.s..ID[j], ";")[[1]][i])
        publication_author = rbind(publication_author, data.frame(pubID = tmp$X[j], AuthorId = authorID))
        if (!(authorID %in% author$ID)) {
          author = rbind(author, data.frame("ID" = authorID, "Name" = name, "FName" = firstname, "LName" = lastname))
        }
      }
    #} else if (length(auth_affl_vec) > length(auth_full_name)){
    #  auth_affl_vec = strsplit(tmp$Affiliations[j], "(?<=[^A-Z]\\.)\\s(?=[A-Z])", perl=T)[[1]]
    #  
    #  for (i in 1:length(auth_full_name)) {
    #    lastname = strsplit(auth_full_name[i], ",")[[1]][1]
    #    matched = grep(lastname, auth_affl_vec, fixed = TRUE)
    #    curr = auth_affl_vec[matched]
    #    m = regexpr(lastname, curr, fixed = TRUE)
    #    if (m) {
    #      curr = substring(curr, m)
    #    }
    #    if(grepl("(uni[versity\\.]{0,} (of )?sou?th[ernm]{0,}[,-]? ?california)|(USC)|(keck school of medicine)|(keck medical center)|(keck medical school)", curr, ignore.case = TRUE)) {
    #      name = trimws(strsplit(tmp$Authors[j], ";")[[1]][i])
    #      firstname = strsplit(auth_full_name, " ")[[1]][2]
    #      authorID = trimws(strsplit(tmp$Author.s..ID[j], ";")[[1]][i])
    #      publication_author = rbind(publication_author, data.frame(pubID = tmp$X[j], AuthorId = authorID))
    #      if (!(authorID %in% author$ID)) {
    #        author = rbind(author, data.frame("ID" = authorID, "Name" = name, "FName" = firstname, "LName" = lastname))
    #      }
    #    }
    #  }
    } else {
      # search up all current affiliation on scopus
      tmp[j,]$ScopusAPI = TRUE
      
      usc_affiliation_ids = c(60029311, 60015183, 60022143, 60009207, 60015400, 60013994, 60099658, 60019009, 60086699, 60005801, 60026672, 60006209, 60268548)
      api_key = Sys.getenv('Elsevier_API')
      ten_authors = trimws(strsplit(tmp$Author.s..ID[j], ";")[[1]])
      if (length(ten_authors) > 10) {
        ten_authors =  c(trimws(strsplit(tmp$Author.s..ID[j], ";")[[1]])[1:9], trimws(strsplit(tmp$Author.s..ID[j], ";")[[1]])[n])
      }
      for (i in ten_authors) {
        x = get_complete_author_info(api_key = api_key, au_id = i, count = 1)
        if (x$content$`search-results`$`opensearch:totalResults` == "1") {
          x_affilid = x$content$`search-results`$entry[[1]]$`affiliation-current`$`affiliation-id`
          if (x_affilid %in% usc_affiliation_ids) {
            lastname = x$content$`search-results`$entry[[1]]$`preferred-name`$surname
            firstname = strsplit(x$content$`search-results`$entry[[1]]$`preferred-name`$`given-name`, " ")[[1]][1]
            name = paste(lastname, " ", substr(firstname, 1, 1), ".", sep = "")
            publication_author = rbind(publication_author, data.frame(pubID = tmp$X[j], AuthorId = i))
            if (!(authorID %in% author$ID)) {
              author = rbind(author, data.frame("ID" = i, "Name" = name, "FName" = firstname, "LName" = lastname))
            }
          }
        }
        Sys.sleep(10) # rate limit exceeded
      }
    }
    
    
  } else if (length(auth_affl_vec) != length(auth_full_name)) {
    print(j)
    print("could not split auth with affil correctly")
    # search up all current affiliation on scopus
    tmp[j,]$ScopusAPI = TRUE
    usc_affiliation_ids = c(60029311, 60015183, 60022143, 60009207, 60015400, 60013994, 60099658, 60019009, 60086699, 60005801, 60026672, 60006209, 60268548)
    api_key = Sys.getenv('Elsevier_API')
    ten_authors = trimws(strsplit(tmp$Author.s..ID[j], ";")[[1]])
    if (length(ten_authors) > 10) {
      ten_authors =  c(trimws(strsplit(tmp$Author.s..ID[j], ";")[[1]])[1:9], trimws(strsplit(tmp$Author.s..ID[j], ";")[[1]])[n])
    }
    for (i in ten_authors) {
      x = get_complete_author_info(api_key = api_key, au_id = i, count = 1)
      #print(x)
      if (x$content$`search-results`$`opensearch:totalResults` == "1") {
        x_affilid = x$content$`search-results`$entry[[1]]$`affiliation-current`$`affiliation-id`
        if (x_affilid %in% usc_affiliation_ids) {
          lastname = x$content$`search-results`$entry[[1]]$`preferred-name`$surname
          firstname = strsplit(x$content$`search-results`$entry[[1]]$`preferred-name`$`given-name`, " ")[[1]][1]
          name = paste(lastname, " ", substr(firstname, 1, 1), ".", sep = "")
          publication_author = rbind(publication_author, data.frame(pubID = tmp$X[j], AuthorId = i))
          if (!(authorID %in% author$ID)) {
            author = rbind(author, data.frame("ID" = i, "Name" = name, "FName" = firstname, "LName" = lastname))
          }
        }
      }
      Sys.sleep(10) # rate limit exceeded
    }
    #for (i in 1:length(auth_full_name)) {
    #  lastname = strsplit(auth_full_name[i], ",")[[1]][1]
    #  matched = grep(lastname, auth_affl_vec, fixed = TRUE)
    #  if(grepl("(uni[versity\\.]{0,} (of )?sou?th[ernm]{0,}[,-]? ?california)|(USC)|(keck school of medicine)|(keck medical center)|(keck medical school)", auth_affl_vec[matched], ignore.case = TRUE)) {
    #    name = trimws(strsplit(tmp$Authors[j], ";")[[1]][i])
    #    firstname = strsplit(auth_full_name, " ")[[1]][2]
    #    authorID = trimws(strsplit(tmp$Author.s..ID[j], ";")[[1]][i])
    #    publication_author = rbind(publication_author, data.frame(pubID = tmp$X[j], AuthorId = authorID))
    #    if (!(authorID %in% author$ID)) {
    #      author = rbind(author, data.frame("ID" = authorID, "Name" = name, "FName" = firstname, "LName" = lastname))
    #    }
    #  }
    #}
  } else {
    # for each match split by , 
    # name is first entry
    for (i in matched) {
      name = trimws(strsplit(auth_affl_vec[i], ",")[[1]][1])
      lastname = strsplit(name, " ")[[1]][1]
      firstname = strsplit(trimws(strsplit(auth_full_name[i], ",")[[1]][2]), " ")[[1]][1]
      authorID = trimws(strsplit(tmp$Author.s..ID[j], ";")[[1]][i])
      publication_author = rbind(publication_author, data.frame(pubID = tmp$X[j], AuthorId = authorID))
      if (!(authorID %in% author$ID)) {
        author = rbind(author, data.frame("ID" = authorID, "Name" = name, "FName" = firstname, "LName" = lastname))
      }
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
author$Email = ""
author$PositionTitle = ""
author$InUSCDirectory = FALSE
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
          author[i,]$InUSCDirectory = TRUE
          if ("departmentnumber" %in% names(v)) {
            author[i,]$Department = v$departmentnumber # assume same as usccostcentername
          }
          if ("uscemployeedivision" %in% names(v)) {
            author[i,]$Division = v$uscemployeedivision  # assume same as usccostcenternamelevel5
          }
          if ("mail" %in% names(v)) {
            author[i,]$Email = v$mail
          }
          if ("title" %in% names(v)) {
            author[i,]$PositionTitle = v$title
          }
          break
        }
      }
    } else {
      if (author$FName[i] == vals$uscdisplaygivenname & author$LName[i] == vals$uscdisplaysn) {
        author[i,]$InUSCDirectory = TRUE
        if ("departmentnumber" %in% names(vals)) {
          author[i,]$Department = vals$departmentnumber
        }
        if ("uscemployeedivision" %in% names(vals)) {
          author[i,]$Division = vals$uscemployeedivision 
        }
        if ("mail" %in% names(vals)) {
          author[i,]$Email = vals$mail
        }
        if ("title" %in% names(vals)) {
          author[i,]$PositionTitle = vals$title
        }
      }
    }
    
  } # else no results
  
}

write.csv(author, file = "./USCauthorsSDG1to16.csv", row.names = FALSE)
write.csv(publication_author, file = "./USCpubidauthidSDG1to16.csv", row.names = FALSE)

auth_info = merge(publication_author, author, by.x = "AuthorId", by.y = "ID")
pub_auth_full = merge(auth_info, tmp, by.x = "pubID", by.y = "X")

write.csv(pub_auth_full, file = "./USCpubauthfullinfoSDG1to16.csv", row.names = FALSE)

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


sdg0 = read.csv("USC_SDG0.csv")
col2keep = intersect(names(sdg0), names(sdg1to16))
sdg0subset = sdg0[col2keep]
sdg1to16 = read.csv("USC_SDG1to16.csv")
sdg0to16 = rbind(sdg0subset, sdg1to16)
write.csv(sdg0to16, file = "./USC_SDG0to16.csv", row.names = FALSE)
