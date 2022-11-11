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
con = getCurlHandle(followlocation = TRUE, verbose = TRUE,ssl.verifypeer = FALSE, cookie = "mbox=session#cf1541a039224330a7690b36e8632037#1663617437|PC#cf1541a039224330a7690b36e8632037.35_0#1726860377; AMCV_4D6368F454EC41940A4C98A6@AdobeOrg=1176715910|MCIDTS|19255|MCMID|81731551737931773397180553428496293094|MCAID|NONE|MCOPTOUT-1663622785s|NONE|vVersion|5.4.0; AMCV_A450776A5245ACC00A490D44@AdobeOrg=-1124106680|MCIDTS|19269|MCMID|36216961312940415734330619965701752051|MCAID|NONE|MCOPTOUT-1664835445s|NONE|vVersion|5.2.0; WWTRBQJP=0297a45327-1bed-4feP8LPIjg7QJrxyxTcPqkTtMYHx66rtBXzG3rtRfi8z0ixdXzFE1sGBteOer3oArIFIY")
tt = getForm("https://uscdirectory.usc.edu/web/directory/faculty-staff/proxy.php", 
             first = "lynne",
             last = "casper", 
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
vals = fromJSON(tt)

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
