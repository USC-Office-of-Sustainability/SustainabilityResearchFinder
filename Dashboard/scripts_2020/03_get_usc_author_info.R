# Search up each author in USC directory to get author info

library(here)
library(RCurl)
library(RJSONIO)
library(dplyr)
library(stringi)

USCDirectoryCookie <- readLines("uscdirectory.cookie", warn = FALSE)[1]
con <- getCurlHandle(followlocation = TRUE, 
                     ssl.verifypeer = FALSE, 
                     verbose = TRUE,
                     cookie = USCDirectoryCookie)
authors_after_fix <- read.csv("data_processed/2020 Data/authors_after_fix_4_16_24.csv")
authors_only <- authors_after_fix %>%
  filter(Dept == "")
authors_only[,c("FirstSearch", "LastSearch","First", "Last", "Department", "Division", "Email", "PositionTitle", "Type")] = ""
authors_only$InUSCDirectory <- FALSE

authors_only_lastname_length <- sapply(authors_only$lastname, function(x) {
  length(strsplit(x, " ")[[1]])
})

authors_only_lastname_period <- sapply(authors_only$lastname, function(x) {
  grepl("\\.", x)
})

# remove .
authors_only$lastname <- gsub("\\.", "", authors_only$lastname)
# remove single letter in last names that start with single letter
# A. Kitchen -> Kitchen
authors_only$lastname <- sapply(authors_only$lastname, function(x) {
  s <- strsplit(x, " ")[[1]]
  if (length(s) > 1) {
    if (nchar(s[1]) == 1) {
      return(s[2])
    }
  }
  return(x)
})

authors_only_firstname_length <- sapply(authors_only$firstname, function(x) {
  length(strsplit(x, " ")[[1]])
})

authors_only_firstname_search <- sapply(authors_only$firstname, function(x) {
  firstnames = strsplit(x, " ")[[1]]
  usename = firstnames[1]
  j = 2
  while(j <= length(firstnames) & (grepl("\\.",usename) | nchar(gsub("\\.", "", usename)) == 1)) {
    usename = firstnames[j]
    j = j + 1
  }
  return(stri_trans_general(usename, "latin-ascii"))
})

# authors_working <- read.csv(here::here("data_processed/authors_working.csv"))
# # bridge_table <- authors_working %>% select(pubID, link, authorID)
# authors_only <- authors_working %>% select(authorID, Name, FName, LName)
# authors_only <- authors_only[!duplicated(authors_only),]
# # remove dup ID
# authors_only <- authors_only[!duplicated(authors_only$authorID),]
# authors_only$Department = ""
# authors_only$Division = ""
# authors_only$Email = ""
# authors_only$PositionTitle = ""
# authors_only$InUSCDirectory = FALSE

for (i in 1:nrow(authors_only)) {
  first_search = stri_trans_general(authors_only$firstname[i], "latin-ascii")
  last_search = stri_trans_general(authors_only$lastname[i], "latin-ascii")
  authors_only[i,]$FirstSearch <- first_search
  authors_only[i,]$LastSearch <- last_search
  # search div too?
  tt = getForm("https://uscdirectory.usc.edu/web/directory/faculty-staff/proxy.php", 
               first = first_search,
               last = last_search, 
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
  # search with first name without initials added
  
  if (tt == "") {
    firstnames = strsplit(authors_only$firstname[i], " ")[[1]]
    if (length(firstnames) > 1) {
      usename = firstnames[1]
      j = 2
      while(j <= length(firstnames) & (grepl("\\.",usename) | nchar(gsub("\\.", "", usename)) == 1)) {
        usename = firstnames[j]
        j = j + 1
      }
      first_search = stri_trans_general(usename, "latin-ascii")
      authors_only[i,]$FirstSearch <- first_search
      tt = getForm("https://uscdirectory.usc.edu/web/directory/faculty-staff/proxy.php", 
                   first = first_search,
                   last = last_search, 
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
    }
    
  }
  if (tt != "") {
    if (is.null(names(vals))) { # multiple results
      # search faculty first then student
      vals_students <- list()
      vals_faculty <- list()
      for (j in 1:length(vals)) {
        v = vals[[j]]
        if ("uscstudent" %in% names(v)) {
          vals_students[[length(vals_students)+1]] <- v
        } 
        if ("uscfaculty" %in% names(v)) {
          vals_faculty[[length(vals_faculty)+1]] <- v
        }
      }
      print(vals_faculty)
      if (length(vals_faculty) >= 1) {
        for (j in 1:length(vals_faculty)) {
          
          v = vals_faculty[[j]]
          if (first_search %in% v$givenname & authors_only$lastname[i] == v$uscdisplaysn) {
            authors_only[i,]$InUSCDirectory = TRUE
            authors_only[i,]$First <- v$uscdisplaygivenname
            authors_only[i,]$Last <- v$uscdisplaysn
            if ("usccostcentername" %in% names(v)) {
              authors_only[i,]$Department = v$usccostcentername # assume same as usccostcentername
            } else if ("uscmajordescription" %in% names(v)) {
              authors_only[i,]$Department = paste(v$uscmajordescription, collapse = ";")
            } 
            if ("usccostcenternamelevel5" %in% names(v)) {
              authors_only[i,]$Division = v$usccostcenternamelevel5  # assume same as usccostcenternamelevel5
            } else if ("uscmajorowningschooldescription" %in% names(v)) {
              authors_only[i,]$Division = paste(v$uscmajorowningschooldescription, collapse = ";")
            }  
            if ("mail" %in% names(v)) {
              authors_only[i,]$Email = v$mail
            }
            if ("title" %in% names(v)) {
              authors_only[i,]$PositionTitle = v$title
            }
            # if ("uscstudent" %in% names(v)) {
            #   authors_only[i,]$Type = "student"
            # }
            if("employeetype" %in% names(v)) {
              authors_only[i,]$Type = v$employeetype
            } else if ("uscprimaryaffiliation" %in% names(v)) {
              authors_only[i,]$Type = v$uscprimaryaffiliation
            }
            break
          }
        }
      }
      
      if (authors_only[i,]$InUSCDirectory == FALSE & length(vals_students) >= 1) {
        for (j in 1:length(vals_students)) {
          v = vals_students[[j]]
          if (first_search %in% v$givenname & authors_only$lastname[i] == v$uscdisplaysn) {
            authors_only[i,]$InUSCDirectory = TRUE
            authors_only[i,]$First <- v$uscdisplaygivenname
            authors_only[i,]$Last <- v$uscdisplaysn
            if ("uscmajordescription" %in% names(v)) {
              authors_only[i,]$Department = paste(v$uscmajordescription, collapse = ";")
            } else if ("uscemployeedepartment" %in% names(v)) {
              authors_only[i,]$Department = v$uscemployeedepartment # assume same as usccostcentername - it is not
            } 
            if ("uscmajorowningschooldescription" %in% names(v)) {
              authors_only[i,]$Division = paste(v$uscmajorowningschooldescription, collapse = ";")
            } else if ("uscemployeedivision" %in% names(v)) {
              authors_only[i,]$Division = v$uscemployeedivision  # assume same as usccostcenternamelevel5 - it is not
            } 
            if ("mail" %in% names(v)) {
              authors_only[i,]$Email = v$mail
            }
            if ("title" %in% names(v)) {
              authors_only[i,]$PositionTitle = v$title
            }
            if ("uscstudent" %in% names(v)) {
              authors_only[i,]$Type = "student"
            }
            if("employeetype" %in% names(v)) {
              authors_only[i,]$Type = v$employeetype
            } else if ("uscprimaryaffiliation" %in% names(v)) {
              authors_only[i,]$Type = v$uscprimaryaffiliation
            }
            break
          }
        }
      }
    } else { # one result
      if (first_search %in% vals$givenname & grepl(authors_only$lastname[i], vals$uscdisplaysn)) {
        authors_only[i,]$First <- vals$uscdisplaygivenname
        authors_only[i,]$Last <- vals$uscdisplaysn
        # if (first_search %in% vals$givenname & authors_only$lastname[i] == vals$uscdisplaysn) {
        authors_only[i,]$InUSCDirectory = TRUE
        if ("uscstudent" %in% names(vals)) {
          authors_only[i,]$Type = "student"
        }
        if("employeetype" %in% names(vals)) {
          authors_only[i,]$Type = vals$employeetype
        } else if ("uscprimaryaffiliation" %in% names(vals)) {
          authors_only[i,]$Type = vals$uscprimaryaffiliation
        }
        if (authors_only[i,]$Type != "student") {
          if ("usccostcentername" %in% names(vals)) {
            authors_only[i,]$Department = vals$usccostcentername
          } else if ("uscmajordescription" %in% names(vals)) {
            authors_only[i,]$Department = paste(vals$uscmajordescription, collapse = ";")
          }
          if ("usccostcenternamelevel5" %in% names(vals)) {
            authors_only[i,]$Division = vals$usccostcenternamelevel5 
          } else if ("uscmajorowningschooldescription" %in% names(vals)) {
            authors_only[i,]$Division = paste(vals$uscmajorowningschooldescription, collapse = ";")
          }
        } else {
          if ("uscmajordescription" %in% names(vals)) {
            authors_only[i,]$Department = paste(vals$uscmajordescription, collapse = ";")
          } else if ("uscemployeedepartment" %in% names(vals)) {
            authors_only[i,]$Department = vals$uscemployeedepartment
          } 
          if ("uscmajorowningschooldescription" %in% names(vals)) {
            authors_only[i,]$Division = paste(vals$uscmajorowningschooldescription, collapse = ";")
          } else if ("uscemployeedivision" %in% names(vals)) {
            authors_only[i,]$Division = vals$uscemployeedivision 
          } 
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


# authors_only_temp <- authors_only
# authors_only_temp$same_first <- mapply(grepl, authors_only_temp$First, authors_only_temp$firstname)
# authors_only_temp %>%
#   filter(InUSCDirectory == TRUE) %>% 
#   filter(same_first == FALSE) %>% View
#   filter(First != firstname) %>%
#   mutate(same_first = grepl(First, firstname)) %>%
#   filter(same_first == FALSE) %>% View

write.csv(authors_only,
          here::here("data_processed/authors_no_dept_major_first_4_16_24.csv"),
          row.names = FALSE)

# combine authors together
authors_original <- authors_after_fix %>%
  filter(Dept != "")
unique(authors_only$Dept)
# authors_only$Dept <- authors_only$Department
authors_only <- read.csv("data_processed/authors_no_dept_major_first_4_16_24.csv")
authors_only$Div <- gsub("^;", "", authors_only$Div)
authors_only$Div <- gsub(";$", "", authors_only$Div)
authors_only$Division <- gsub("USC ", "", authors_only$Division)

# manual fix
i <- which(authors_only$fullname == "Quinn, David M.")
authors_only[i,]$Department <- "Rossier Faculty Affairs Salaries"
authors_only[i,]$Division <- "Rossier School of Education"
authors_only[i,]$Email <- "quinnd@rossier.usc.edu"
authors_only[i,]$PositionTitle <- "Associate Professor of Education"
authors_only[i,]$Type <- "faculty"
# Chen, Xuanjin -> false
# Lu, Lei -> false
# Robinson, Kevin -> student at Keck


authors_only %>%
  mutate_at(c('Div', 'Division'), ~na_if(., '')) %>%
  tidyr::separate_rows(Department, sep = ";") %>%
  tidyr::separate_rows(Division, sep = ";") %>%
  mutate(Division = plyr::revalue(Division, c("Andrew and Erna Viterbi School of Engineering" = "Viterbi School of Engineering",
                                                "Dornsife College of Letters, Arts and Sciences" = "Dornsife College of Letters Arts and Sciences",
                                                "Dworak-Peck School of Social Work" = "Suzanne Dworak-Peck School of Social Work",
                                                "Keck School of Medicine" = "Keck Medicine of USC",
                                                "Sol Price School of Public Policy" = "Price School of Public Policy",
                                                "Annenberg School for Communication" = "Annenberg School for Communication and Journalism",
                                                "School of Pharmacy" = "Alfred E. Mann School of Pharmacy and Pharmaceutical Sciences",
                                                "School of Law" = "Gould School of Law",
                                                "Music" = "Thornton School of Music",
                                                "LAS- Humanities" = "Dornsife College of Letters Arts and Sciences",
                                                "LAS - Natural Sciences & Mathematics" = "Dornsife College of Letters Arts and Sciences",
                                                "LAS- Social Science & Communications" = "Dornsife College of Letters Arts and Sciences",
                                              "Dornsife College of Letters, Arts, and Sciences" = "Dornsife College of Letters Arts and Sciences",
                                                "LAS - Other Programs" = "Dornsife College of Letters Arts and Sciences",
                                                "Barbara J. and Roger W. Rossier School of Education" = "Rossier School of Education",
                                                "Ostrow School of Dentistry" = "Herman Ostrow School of Dentistry of USC",
                                              "Herman Ostrow School of Dentistry" = "Herman Ostrow School of Dentistry of USC",
                                                "Gordon S. Marshall School of Business" = "Marshall School of Business",
                                                "Jimmy Iovine and Andre Young Academy" = "Iovine and Young Academy",
                                                "USC Roski School of Art and Design" = "Roski School of Art and Design",
                                              "Registrar's Office" = "",
                                              "USC University Hospital" = "Keck Medicine of USC",
                                              "Keck School of Medicine of USC" = "Keck Medicine of USC",
                                              "Keck Hospital of USC" = "Keck Medicine of USC",
                                              "Davis School of Gerontology" = "Leonard Davis School of Gerontology",
                                              "University Libraries" = "Libraries"
  ))) %>% mutate(Division = ifelse(Division == "", 
                               case_when(grepl("KSOM", Department) ~ "Keck Medicine of USC",
                                         grepl("Dornsife", Department) ~ "Dornsife College of Letters Arts and Sciences",
                                         grepl("Marshall", Department) ~ "Marshall School of Business",
                                         grepl("Viterbi", Department) ~ "Viterbi School of Engineering",
                                         grepl("Rossier", Department) ~ "Rossier School of Education",
                                         grepl("Dworak", Department) ~ "Suzanne Dworak-Peck School of Social Work",
                                         grepl("Pharmacy", Department) ~ "Alfred E. Mann School of Pharmacy and Pharmaceutical Sciences",
                                         grepl("Law", Department) ~ "Gould School of Law",
                                         grepl("Gerontology", Department) ~ "Leonard Davis School of Gerontology",
                                         grepl("Dramatic Arts", Department) ~ "School of Dramatic Arts",
                                         grepl("DEN", Department) ~ "Herman Ostrow School of Dentistry of USC",
                                         grepl("Sol Price", Department) ~ "Sol Price School of Public Policy",
                                         grepl("Annenberg", Department) ~ "Annenberg School for Communication and Journalism"), Division)) %>%
  group_by(authorID) %>%
  mutate(Division = paste(unique(Division), collapse=";")) %>%
  mutate(Dept = ifelse(grepl("Leventhal School of Accounting", Div), "Leventhal School of Accounting", Dept)) %>%
  mutate(Div = ifelse(grepl("Leventhal School of Accounting", Dept), "Marshall School of Business", Div)) -> authors_separate_division
authors_separate_division %>%
  mutate(Division = ifelse(Division == "NA", "", Division),
         Div = ifelse(is.na(Div), "", Div)) %>%
  mutate(Div_count = length(strsplit(Div, ";")[[1]]),
         Division_count = length(strsplit(Division, ";")[[1]])) %>%
  mutate(Div_Division_count_sum = sum(Div_count, Division_count)) %>%
  mutate(Division = ifelse(Division == "", NA, Division),
         Div = ifelse(Div == "", NA, Div)) %>%
  mutate(Div_copy = Div, Division_copy = Division) %>%
  tidyr::unite(Divisions, c(Div_copy,Division_copy), sep=";", na.rm = TRUE) %>%
  mutate(Divisions = paste(unique(strsplit(Divisions, ";")[[1]]), collapse = ";")) %>%
  mutate(Divisions_count = length(strsplit(Divisions, ";")[[1]])) %>%
  distinct() -> author_division_count
# authors_separate_division %>% 
#   filter(InUSCDirectory == TRUE & Div != Division & Div != "" & Division != "NA") %>% View
# idxs <- which(authors_separate_division$InUSCDirectory == TRUE & 
#                 authors_separate_division$Div != authors_separate_division$Division & 
#                 authors_separate_division$Div != "" & 
#                 authors_separate_division$Division != "NA")
# assume incorrect info
idxs <- which(author_division_count$InUSCDirectory == TRUE &
                author_division_count$Divisions_count >= author_division_count$Div_Division_count_sum &
                author_division_count$Div_Division_count_sum > 1)
# authors_separate_division[idxs,]$InUSCDirectory <- FALSE
# authors_separate_division[idxs,]$Type <- ""
# authors_separate_division[idxs,]$PositionTitle <- ""
# authors_separate_division[idxs,]$Email <- ""
# authors_separate_division[idxs,]$Division <- ""
# authors_separate_division[idxs,]$Department <- ""
# authors_separate_division[idxs,]$Last <- ""
# authors_separate_division[idxs,]$First <- ""

author_division_count[idxs,]$InUSCDirectory <- FALSE
author_division_count[idxs,]$Type <- ""
author_division_count[idxs,]$PositionTitle <- ""
author_division_count[idxs,]$Email <- ""
author_division_count[idxs,]$Division <- ""
author_division_count[idxs,]$Department <- ""
author_division_count[idxs,]$Last <- ""
author_division_count[idxs,]$First <- ""
author_division_count[idxs,]$Divisions <- author_division_count[idxs,]$Div

author_division_count %>%
  mutate(Dept = Department, Div = Divisions) %>%
  select(-Div_count, -Division_count, -Div_Division_count_sum, -Divisions, -Divisions_count, -Department, -Division) %>%
  tidyr::separate_rows(Div, sep=";") -> authors_only_separate2




####
# authors_only %>%
#   mutate_at(c('Div', 'Division'), ~na_if(., '')) %>%
#   tidyr::separate_rows(Department, sep=";") %>%
#   tidyr::unite(Divisions, c(Div,Division), sep=";", na.rm = TRUE) %>% 
#   tidyr::separate_rows(Divisions, sep=";") %>%
#   mutate(Divisions = plyr::revalue(Divisions, c("Andrew and Erna Viterbi School of Engineering" = "Viterbi School of Engineering",
#                                           "Dornsife College of Letters, Arts and Sciences" = "Dornsife College of Letters Arts and Sciences",
#                                           "Dworak-Peck School of Social Work" = "Suzanne Dworak-Peck School of Social Work",
#                                           "Keck School of Medicine" = "Keck Medicine of USC",
#                                           "Price School of Public Policy" = "Sol Price School of Public Policy",
#                                           "Annenberg School for Communication" = "Annenberg School for Communication and Journalism",
#                                           "School of Pharmacy" = "Alfred E. Mann School of Pharmacy and Pharmaceutical Sciences",
#                                           "School of Law" = "Gould School of Law",
#                                           "Music" = "Thornton School of Music",
#                                           "LAS- Humanities" = "Dornsife College of Letters Arts and Sciences",
#                                           "LAS - Natural Sciences & Mathematics" = "Dornsife College of Letters Arts and Sciences",
#                                           "LAS- Social Science & Communications" = "Dornsife College of Letters Arts and Sciences",
#                                           "LAS - Other Programs" = "Dornsife College of Letters Arts and Sciences",
#                                           "Barbara J. and Roger W. Rossier School of Education" = "Rossier School of Education",
#                                           "Ostrow School of Dentistry" = "Herman Ostrow School of Dentistry of USC",
#                                           "Gordon S. Marshall School of Business" = "Marshall School of Business",
#                                           "Jimmy Iovine and Andre Young Academy" = "Iovine and Young Academy",
#                                           "USC Roski School of Art and Design" = "Roski School of Art and Design"
#   ))) -> authors_only_separate
# authors_only_separate2 <- authors_only_separate %>%
#   mutate(Division = ifelse(Divisions == "", 
#                            case_when(grepl("KSOM", Department) ~ "Keck Medicine of USC",
#                                      grepl("Dornsife", Department) ~ "Dornsife College of Letters Arts and Sciences",
#                                      grepl("Marshall", Department) ~ "Marshall School of Business",
#                                      grepl("Viterbi", Department) ~ "Viterbi School of Engineering",
#                                      grepl("Rossier", Department) ~ "Rossier School of Education",
#                                      grepl("Dworak", Department) ~ "Suzanne Dworak-Peck School of Social Work",
#                                      grepl("Pharmacy", Department) ~ "Alfred E. Mann School of Pharmacy and Pharmaceutical Sciences",
#                                      grepl("Law", Department) ~ "Gould School of Law",
#                                      grepl("Gerontology", Department) ~ "Leonard Davis School of Gerontology",
#                                      grepl("Dramatic Arts", Department) ~ "School of Dramatic Arts",
#                                      grepl("DEN", Department) ~ "Herman Ostrow School of Dentistry of USC",
#                                      grepl("Sol Price", Department) ~ "Sol Price School of Public Policy",
#                                      grepl("Annenberg", Department) ~ "Annenberg School for Communication and Journalism"), Divisions)) %>%
#   # distinct() %>%
#   mutate(Div = Division, Dept = Department) %>%
#   select(-Division, -Department, -Divisions) %>%
#   distinct()
# duplicated(authors_only_separate2) %>% sum
###
authors_only_separate2$Dept <- trimws(gsub("^(Dornsife|Viterbi|Marshall|KSOM|Annenberg|Pharmacy|Rossier|USC Libraries|DEN) ", "", authors_only_separate2$Dept))

usc_departments <- read.csv("data_raw/usc_departments.csv")
authors_only_separate2$Dept2 <- ""
authors_only_separate2$Div2 <- ""
for (i in 1:nrow(authors_only_separate2)) {
  
  matched_dept <- apply(usc_departments, 1, function(x) {
    if (grepl(x['Pattern'], authors_only_separate2$Dept[i], ignore.case = TRUE)) {
      x
    }
  })
  matched_dept <- matched_dept[lengths(matched_dept) != 0]
  departments <- c()
  divisions <- c()
  for (j in 1:length(matched_dept)) {
    departments <- append(departments, matched_dept[[j]]["Department.Group"])
    divisions <- append(divisions, matched_dept[[j]]["School.Institute.Center"])
  }
  departments <- unname(departments)
  divisions <- unname(divisions)
  authors_only_separate2[i,]$Dept2 <- paste(departments[!duplicated(departments)], collapse = ";")
  authors_only_separate2[i,]$Div2 <- paste(divisions[!duplicated(divisions)], collapse = ";")

}

authors_only_separate2 %>%
  mutate(Dept3 = ifelse(Dept2 == "", Dept, Dept2)) -> authors_only_separate3

authors_only_separate3 %>%
  select(-Dept, -Dept2, -Div2) %>%
  rename(Dept = Dept3) %>%
  distinct() -> authors_only_separate4

duplicated(authors_only_separate4) %>% sum

authors_only_separate4$Dept[which(authors_only_separate4$Div == "Student Affairs")] <- "Other"
authors_only_separate4$Div[which(authors_only_separate4$Div == "Student Affairs")] <- "Other"
i <- which(authors_only_separate4$fullname == "Wang, Jonathan")
authors_only_separate4$InUSCDirectory[i] <- FALSE
authors_only_separate4$Dept[i] <- ""
authors_only_separate4$Div[i] <- ""
authors_only_separate4$Type[i] <- ""
authors_only_separate4$PositionTitle[i] <- ""
authors_only_separate4$Email[i] <- ""
authors_only_separate4$Last[i] <- ""
authors_only_separate4$First[i] <- ""




# authors_only$Div <- paste(authors_only$Div, authors_only$Division, collaspe = ";")
# authors_only %>%
#   select(-Department, -Division) -> authors_only
names(authors_original)
names(authors_only_separate4)

empty_cols <- setdiff(names(authors_only_separate4), names(authors_original))
authors_original[, empty_cols] <- ""

authors_all <- rbind(authors_original, authors_only_separate4)

write.csv(authors_all,
          "data_processed/authors_all_4_16_24.csv",
          row.names = FALSE)
# authors_only <- read.csv(here::here("data_processed/authors_only.csv"))
# fix sunny's firstname
# authors_only[which(authors_only$authorID == "57218170772"),]$FName = "Sachin (Sunny)"
# authors_only[which(authors_only$authorID == "57218170772"),]$LName = "Jha"
# authors_only[which(authors_only$authorID == "57668533700"),]$FName = "Eunjin (Anna)"
# authors_only[which(authors_only$authorID == "57668533700"),]$LName = "Kim"
# authors_only[which(authors_only$authorID == "57668533700"),]$Department = "Annenberg"
# authors_only[which(authors_only$authorID == "57668533700"),]$Division = "Annenberg"
# 
# authors_only[which(authors_only$authorID == "6603569444"),]$LName = "Arvai"
# authors_only[which(authors_only$authorID == "6603569444"),]$Department = "Dornsife Psychology"
# authors_only[which(authors_only$authorID == "6603569444"),]$Division = "Dornsife College of Letters, Arts and Sciences"
# "55858656400" # joseph kitchen
# "39761538500" # julie
# "57073146400" "57215497016"
# "57212903285"
# "57218394162"

# write.csv(authors_only,
#           here::here("data_processed/authors_only.csv"),
#           row.names = FALSE)



# # combine / reclassify departments
# authors_only <- read.csv(here::here("data_processed/authors_only.csv"))
# # manual fixes
# authors_only[which(authors_only$Department == "Viterbi - IMSC"),]$Department = "Viterbi Computer Science"
# 
# dept <- authors_only$Department
# # remove everything after -
# dept_short <- trimws(gsub("-[^-]*", "", dept))
# # revalue
# dept_reval <- plyr::revalue(
#   dept_short,
#   c("1710" = "Health Systems Operations",
#     "1726" = "Keck Hospital of USC",
#     "1733" = "USC Norris Comprehensive Cancer Center and Hospital",
#     "1750" = "USC Verdugo Hills Hospital",
#     "1776" = "USC Care Clinical - Ambulatory",
#     "1790" = "Student Health"
#     )
#   )
# dept_reval <- gsub(" and ", " & ", dept_reval)
# authors_only$Department <- dept_reval
# 
# # move dept/divi to different division
# authors_only[which(authors_only$Department == "Pediatrics/Childrens Hospital"),]$Division = "Keck Medicine of USC"
# authors_only[which(authors_only$Department == "Keck Hospital of USC"),]$Division = "Keck Medicine of USC"
# authors_only[grep("^KSOM", authors_only$Department),]$Division = "Keck School of Medicine"
# authors_only[which(authors_only$Department == "Student Health"),]$Division = "Keck School of Medicine"
# authors_only[grep("^OT", authors_only$Department),]$Division = "Chan Division of Occupational Science and Occupational Therapy"
# authors_only[grep("^PT", authors_only$Department),]$Division = "Division of Biokinesiology and Physical Therapy"
# authors_only[grep("^Dornsife", authors_only$Department),]$Division = "Dornsife College of Letters, Arts and Sciences"
# authors_only[grep("^Annenberg", authors_only$Department),]$Division = "Annenberg School for Communication and Journalism"
# authors_only[grep("^ITS", authors_only$Department),]$Division = "Information Technology Services"
# authors_only[grep("^Viterbi", authors_only$Department),]$Division = "Viterbi School of Engineering"
# authors_only[grep("^CCT", authors_only$Department),]$Division = "Institute for Creative Technologies"
# authors_only[grep("^Bovard", authors_only$Department),]$Division = "Bovard College"
# authors_only[grep("^Cinematic", authors_only$Department),]$Division = "School of Cinematic Arts"
# authors_only[grep("^Dramatic", authors_only$Department),]$Division = "School of Dramatic Arts"
# authors_only[grep("^Marshall", authors_only$Department),]$Division = "Marshall School of Business"
# authors_only[grep("^Sol Price", authors_only$Department),]$Division = "Sol Price School of Public Policy"
# authors_only[grep("^Law", authors_only$Department),]$Division = "Gould School of Law"
# authors_only[grep("^Pharmacy", authors_only$Department),]$Division = "Alred E. Mann School of Pharmacy"
# authors_only[grep("^Rossier", authors_only$Department),]$Division = "Rossier School of Education"
# authors_only[grep("^Gerontology", authors_only$Department),]$Division = "Leonard Davis School of Gerontology"
# authors_only[grep("^Dworak", authors_only$Department),]$Division = "Dworak-Peck School of Social Work"
# authors_only[grep("^AMI", authors_only$Department),]$Division = "AMI-USC"
# authors_only[grep("^DEN", authors_only$Department),]$Division = "Herman Ostrow School of Dentistry of USC"
# authors_only[grep("^Thornton", authors_only$Department),]$Division = "Thornton School of Music"
# authors_only[which(authors_only$Department == ""),]$Division = ""
# authors_only[which(authors_only$Department == "USC School of Architecture All Departments"),]$Division = ""
# 
# 
# divi <- authors_only$Division
# divi_reval <- plyr::revalue(
#   divi,
#   c("Annenberg School for Communication" = "Annenberg School for Communication and Journalism",
#     "AMI-USC" = "Alfred E. Mann Institute for Biomedical Engineering",
#     "USC University Hospital" = "Keck Medicine of USC",
#     "Music" = "Thornton School of Music"
#     )
#   )
# authors_only$Division <- divi_reval
# 
# # aux services, financial + business services double check the ppl
# # office of research
# 
# write.csv(authors_only,
#           here::here("data_processed/authors_only_revalued.csv"),
#           row.names = FALSE)
# 
# not_sure <- authors_only[which(authors_only$Department %in% c("Academic Affairs", 
#                                                   "Academic Leadership", 
#                                                   "Department of Public Safety", 
#                                                   "Educational Partnerships",
#                                                   "Enrollment Services Academic Records & Registrar",
#                                                   "Enrollment Services Undergraduate Admission",
#                                                   "FBS Procurement Services",
#                                                   "Fire Safety & Emergency Planning",
#                                                   "FMS Facilities Financial Services",
#                                                   "FMS Plumbing Shop",
#                                                   "Fringe Pool Center WorkWell Center Fringe Benefits",
#                                                   "Office Equity Equal Opportunity & Title IX",
#                                                   "Office of Community Expectations",
#                                                   "Office of Research",
#                                                   "Office of Senior VP Administration",
#                                                   "President Staff",
#                                                   "Provost Staff",
#                                                   "Student Affairs Campus Activities",
#                                                   "Student Affairs Recreation Sports",
#                                                   "University Advancement Central Development",
#                                                   "USC Hospitality Administration & General",
#                                                   "USC Hospitality Town & Gown Catering",
#                                                   "USC Housing Building Services Parkside",
#                                                   "USC Housing Central Maintenance Office")),]
# 
# fname_problems <- authors_only[union(grep("\\.",authors_only$FName),which(apply(authors_only, 1, function(x) nchar(x['FName'])) == 1)),]
# for (i in 1:nrow(fname_problems)) {
#   api_res = get_complete_author_info(au_id = fname_problems[i,]$authorID, count = 1,
#                                      headers = c("X-ELS-Insttoken" = institution_token, 
#                                                  "X-ELS-APIKey" = elsevier_api_key))
#   if (api_res$content$`search-results`$`opensearch:totalResults` == "1") {
#     x_affilid = api_res$content$`search-results`$entry[[1]]$`affiliation-current`$`affiliation-id`
#     if (!is.null(x_affilid) && x_affilid %in% usc_affiliation_ids) {
#       lastname = api_res$content$`search-results`$entry[[1]]$`preferred-name`$surname
#       if ("given-name" %in% names(api_res$content$`search-results`$entry[[1]]$`preferred-name`)) {
#         firstname = strsplit(api_res$content$`search-results`$entry[[1]]$`preferred-name`$`given-name`, " ")[[1]][1]
#         name = paste(lastname, " ", substr(firstname, 1, 1), ".", sep = "")
#       } else {
#         firstname = NA
#         name = lastname
#       }
#       fname_problems[i,]$FName = firstname
#       fname_problems[i,]$LName = lastname
#       fname_problems[i,]$Name = name
#     }
#     
#   }
#   Sys.sleep(.4) # rate limit exceeded
# }
# 
# authors_only[union(grep("\\.",authors_only$FName),which(apply(authors_only, 1, function(x) nchar(x['FName'])) == 1)),]$Name = fname_problems$Name
# authors_only[union(grep("\\.",authors_only$FName),which(apply(authors_only, 1, function(x) nchar(x['FName'])) == 1)),]$LName = fname_problems$LName
# authors_only[union(grep("\\.",authors_only$FName),which(apply(authors_only, 1, function(x) nchar(x['FName'])) == 1)),]$FName = fname_problems$FName
# write.csv(authors_only,
#           here::here("data_processed/authors_only_fname_fix.csv"),
#           row.names = FALSE)
# 
# usc_pubs <- read.csv(here::here("data_processed/usc_pubs.csv"))
# usc_bridge <- read.csv(here::here("data_processed/bridge.csv"))
# 
# # merge
# tmp <- merge(usc_pubs, usc_bridge,
#              by.x = c("X", "Link"), by.y = c("pubID", "link"))
# tmp2 <- merge(tmp, fname_problems,
#               by.x = "authorID", by.y = "authorID")
# 
# tmp2 %>% select(X, authorID, Name, FName, LName, Department, Division, InUSCDirectory, Link, Author.full.names) -> tmp3
# write.csv(tmp3,
#           here::here("data_processed/authors_fname_problems.csv"),
#           row.names = FALSE)
