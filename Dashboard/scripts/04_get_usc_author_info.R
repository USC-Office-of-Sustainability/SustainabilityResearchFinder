# Search up each author in USC directory to get author info

library(here)
library(RCurl)
library(RJSONIO)
library(dplyr)

USCDirectoryCookie <- readLines("uscdirectory.cookie", warn = FALSE)[1]
con <- getCurlHandle(followlocation = TRUE, 
                     ssl.verifypeer = FALSE, 
                     #verbose = TRUE,
                     cookie = USCDirectoryCookie)
authors_working <- read.csv(here::here("data_processed/authors_working.csv"))
bridge_table <- authors_working %>% select(pubID, link, authorID)
authors_only <- authors_working %>% select(authorID, Name, FName, LName)
authors_only <- authors_only[!duplicated(authors_only),]
# remove dup ID
authors_only <- authors_only[!duplicated(authors_only$authorID),]
authors_only$Department = ""
authors_only$Division = ""
authors_only$Email = ""
authors_only$PositionTitle = ""
authors_only$InUSCDirectory = FALSE

for (i in 8929:nrow(authors_only)) {
  tt = getForm("https://uscdirectory.usc.edu/web/directory/faculty-staff/proxy.php", 
               first = authors_only$FName[i],
               last = authors_only$LName[i], 
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
        if (authors_only$FName[i] == v$uscdisplaygivenname & authors_only$LName[i] == v$uscdisplaysn) {
          authors_only[i,]$InUSCDirectory = TRUE
          if ("departmentnumber" %in% names(v)) {
            authors_only[i,]$Department = v$departmentnumber # assume same as usccostcentername
          }
          if ("uscemployeedivision" %in% names(v)) {
            authors_only[i,]$Division = v$uscemployeedivision  # assume same as usccostcenternamelevel5
          }
          if ("mail" %in% names(v)) {
            authors_only[i,]$Email = v$mail
          }
          if ("title" %in% names(v)) {
            authors_only[i,]$PositionTitle = v$title
          }
          break
        }
      }
    } else {
      if (authors_only$FName[i] == vals$uscdisplaygivenname & authors_only$LName[i] == vals$uscdisplaysn) {
        authors_only[i,]$InUSCDirectory = TRUE
        if ("departmentnumber" %in% names(vals)) {
          authors_only[i,]$Department = vals$departmentnumber
        }
        if ("uscemployeedivision" %in% names(vals)) {
          authors_only[i,]$Division = vals$uscemployeedivision 
        }
        if ("mail" %in% names(vals)) {
          authors_only[i,]$Email = vals$mail
        }
        if ("title" %in% names(vals)) {
          authors_only[i,]$PositionTitle = vals$title
        }
      }
    }
    
  } # else no results
  
}

write.csv(authors_only,
          here::here("data_processed/authors_only.csv"),
          row.names = FALSE)
write.csv(bridge_table,
          here::here("data_processed/bridge.csv"),
          row.names = FALSE)  
