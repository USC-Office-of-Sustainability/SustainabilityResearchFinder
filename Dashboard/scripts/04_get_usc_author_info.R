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

for (i in 1:nrow(authors_only)) {
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


authors_only <- read.csv(here::here("data_processed/authors_only.csv"))
# fix sunny's firstname
authors_only[which(authors_only$authorID == "57218170772"),]$FName = "Sachin (Sunny)"
authors_only[which(authors_only$authorID == "57218170772"),]$LName = "Jha"
authors_only[which(authors_only$authorID == "57668533700"),]$FName = "Eunjin (Anna)"
authors_only[which(authors_only$authorID == "57668533700"),]$LName = "Kim"
authors_only[which(authors_only$authorID == "57668533700"),]$Department = "Annenberg"
authors_only[which(authors_only$authorID == "57668533700"),]$Division = "Annenberg"

authors_only[which(authors_only$authorID == "6603569444"),]$LName = "Arvai"
authors_only[which(authors_only$authorID == "6603569444"),]$Department = "Dornsife Psychology"
authors_only[which(authors_only$authorID == "6603569444"),]$Division = "Dornsife College of Letters, Arts and Sciences"
"55858656400" # joseph kitchen
"39761538500" # julie
"57073146400" "57215497016"
"57212903285"
"57218394162"

write.csv(authors_only,
          here::here("data_processed/authors_only.csv"),
          row.names = FALSE)



# combine / reclassify departments
authors_only <- read.csv(here::here("data_processed/authors_only.csv"))
# manual fixes
authors_only[which(authors_only$Department == "Viterbi - IMSC"),]$Department = "Viterbi Computer Science"

dept <- authors_only$Department
# remove everything after -
dept_short <- trimws(gsub("-[^-]*", "", dept))
# revalue
dept_reval <- plyr::revalue(
  dept_short,
  c("1710" = "Health Systems Operations",
    "1726" = "Keck Hospital of USC",
    "1733" = "USC Norris Comprehensive Cancer Center and Hospital",
    "1750" = "USC Verdugo Hills Hospital",
    "1776" = "USC Care Clinical - Ambulatory",
    "1790" = "Student Health"
    )
  )
dept_reval <- gsub(" and ", " & ", dept_reval)
authors_only$Department <- dept_reval

# move dept/divi to different division
authors_only[which(authors_only$Department == "Pediatrics/Childrens Hospital"),]$Division = "Keck Medicine of USC"
authors_only[which(authors_only$Department == "Keck Hospital of USC"),]$Division = "Keck Medicine of USC"
authors_only[grep("^KSOM", authors_only$Department),]$Division = "Keck School of Medicine"
authors_only[which(authors_only$Department == "Student Health"),]$Division = "Keck School of Medicine"
authors_only[grep("^OT", authors_only$Department),]$Division = "Chan Division of Occupational Science and Occupational Therapy"
authors_only[grep("^PT", authors_only$Department),]$Division = "Division of Biokinesiology and Physical Therapy"
authors_only[grep("^Dornsife", authors_only$Department),]$Division = "Dornsife College of Letters, Arts and Sciences"
authors_only[grep("^Annenberg", authors_only$Department),]$Division = "Annenberg School for Communication and Journalism"
authors_only[grep("^ITS", authors_only$Department),]$Division = "Information Technology Services"
authors_only[grep("^Viterbi", authors_only$Department),]$Division = "Viterbi School of Engineering"
authors_only[grep("^CCT", authors_only$Department),]$Division = "Institute for Creative Technologies"
authors_only[grep("^Bovard", authors_only$Department),]$Division = "Bovard College"
authors_only[grep("^Cinematic", authors_only$Department),]$Division = "School of Cinematic Arts"
authors_only[grep("^Dramatic", authors_only$Department),]$Division = "School of Dramatic Arts"
authors_only[grep("^Marshall", authors_only$Department),]$Division = "Marshall School of Business"
authors_only[grep("^Sol Price", authors_only$Department),]$Division = "Sol Price School of Public Policy"
authors_only[grep("^Law", authors_only$Department),]$Division = "Gould School of Law"
authors_only[grep("^Pharmacy", authors_only$Department),]$Division = "Alred E. Mann School of Pharmacy"
authors_only[grep("^Rossier", authors_only$Department),]$Division = "Rossier School of Education"
authors_only[grep("^Gerontology", authors_only$Department),]$Division = "Leonard Davis School of Gerontology"
authors_only[grep("^Dworak", authors_only$Department),]$Division = "Dworak-Peck School of Social Work"
authors_only[grep("^AMI", authors_only$Department),]$Division = "AMI-USC"
authors_only[grep("^DEN", authors_only$Department),]$Division = "Herman Ostrow School of Dentistry of USC"
authors_only[grep("^Thornton", authors_only$Department),]$Division = "Thornton School of Music"
authors_only[which(authors_only$Department == ""),]$Division = ""
authors_only[which(authors_only$Department == "USC School of Architecture All Departments"),]$Division = ""


divi <- authors_only$Division
divi_reval <- plyr::revalue(
  divi,
  c("Annenberg School for Communication" = "Annenberg School for Communication and Journalism",
    "AMI-USC" = "Alfred E. Mann Institute for Biomedical Engineering",
    "USC University Hospital" = "Keck Medicine of USC",
    "Music" = "Thornton School of Music"
    )
  )
authors_only$Division <- divi_reval

# aux services, financial + business services double check the ppl
# office of research

write.csv(authors_only,
          here::here("data_processed/authors_only_revalued.csv"),
          row.names = FALSE)

not_sure <- authors_only[which(authors_only$Department %in% c("Academic Affairs", 
                                                  "Academic Leadership", 
                                                  "Department of Public Safety", 
                                                  "Educational Partnerships",
                                                  "Enrollment Services Academic Records & Registrar",
                                                  "Enrollment Services Undergraduate Admission",
                                                  "FBS Procurement Services",
                                                  "Fire Safety & Emergency Planning",
                                                  "FMS Facilities Financial Services",
                                                  "FMS Plumbing Shop",
                                                  "Fringe Pool Center WorkWell Center Fringe Benefits",
                                                  "Office Equity Equal Opportunity & Title IX",
                                                  "Office of Community Expectations",
                                                  "Office of Research",
                                                  "Office of Senior VP Administration",
                                                  "President Staff",
                                                  "Provost Staff",
                                                  "Student Affairs Campus Activities",
                                                  "Student Affairs Recreation Sports",
                                                  "University Advancement Central Development",
                                                  "USC Hospitality Administration & General",
                                                  "USC Hospitality Town & Gown Catering",
                                                  "USC Housing Building Services Parkside",
                                                  "USC Housing Central Maintenance Office")),]

fname_problems <- authors_only[union(grep("\\.",authors_only$FName),which(apply(authors_only, 1, function(x) nchar(x['FName'])) == 1)),]
for (i in 1:nrow(fname_problems)) {
  api_res = get_complete_author_info(au_id = fname_problems[i,]$authorID, count = 1,
                                     headers = c("X-ELS-Insttoken" = institution_token, 
                                                 "X-ELS-APIKey" = elsevier_api_key))
  if (api_res$content$`search-results`$`opensearch:totalResults` == "1") {
    x_affilid = api_res$content$`search-results`$entry[[1]]$`affiliation-current`$`affiliation-id`
    if (!is.null(x_affilid) && x_affilid %in% usc_affiliation_ids) {
      lastname = api_res$content$`search-results`$entry[[1]]$`preferred-name`$surname
      if ("given-name" %in% names(api_res$content$`search-results`$entry[[1]]$`preferred-name`)) {
        firstname = strsplit(api_res$content$`search-results`$entry[[1]]$`preferred-name`$`given-name`, " ")[[1]][1]
        name = paste(lastname, " ", substr(firstname, 1, 1), ".", sep = "")
      } else {
        firstname = NA
        name = lastname
      }
      fname_problems[i,]$FName = firstname
      fname_problems[i,]$LName = lastname
      fname_problems[i,]$Name = name
    }
    
  }
  Sys.sleep(.4) # rate limit exceeded
}

authors_only[union(grep("\\.",authors_only$FName),which(apply(authors_only, 1, function(x) nchar(x['FName'])) == 1)),]$Name = fname_problems$Name
authors_only[union(grep("\\.",authors_only$FName),which(apply(authors_only, 1, function(x) nchar(x['FName'])) == 1)),]$LName = fname_problems$LName
authors_only[union(grep("\\.",authors_only$FName),which(apply(authors_only, 1, function(x) nchar(x['FName'])) == 1)),]$FName = fname_problems$FName
write.csv(authors_only,
          here::here("data_processed/authors_only_fname_fix.csv"),
          row.names = FALSE)

usc_pubs <- read.csv(here::here("data_processed/usc_pubs.csv"))
usc_bridge <- read.csv(here::here("data_processed/bridge.csv"))

# merge
tmp <- merge(usc_pubs, usc_bridge,
             by.x = c("X", "Link"), by.y = c("pubID", "link"))
tmp2 <- merge(tmp, fname_problems,
              by.x = "authorID", by.y = "authorID")

tmp2 %>% select(X, authorID, Name, FName, LName, Department, Division, InUSCDirectory, Link, Author.full.names) -> tmp3
write.csv(tmp3,
          here::here("data_processed/authors_fname_problems.csv"),
          row.names = FALSE)
