# Identify USC authors in the list of publications by
# 1. parsing authors with affiliations text
# 2. searching first 9 + last 1 author's current affiliation on scopus via API

library(here)
library(rscopus)
library(dplyr)
library(stringr)

usc_regex <- paste0("([uU]ni[versity\\.]{0,} ([oO]f )?[sS]ou?th[ernm]{0,}[,-]? ?",
                   "[Cc]alifornia)|([kK]eck [Ss]chool [Oo]f [Mm]edicine)|",
                   "([Kk]eck [Mm]edical [Cc]enter)|([Kk]eck [Mm]edical [Ss]chool)")
usc_affiliation_ids <- c(60029311, 60015183, 60022143, 60009207, 60015400,
                         60013994, 60099658, 60019009, 60086699, 60005801,
                         60026672, 60006209, 60268548)

# saved in .Renviron
elsevier_api_key <- Sys.getenv("Elsevier_API")
institution_token <- Sys.getenv("Institution_Token")

usc_data <- read.csv(here::here("data_processed/usc_pubs.csv"))
#usc_data <- usc_data[-c(20634,24986,4340),] # removing pubs that aren't USC
# usc_data$ScopusAPI <- FALSE

# tmp <- usc_data[sample(1:nrow(usc_data), 10), ]

# check authors, authors full name, authors ID columns match
same_num_auth <- apply(usc_data, 1, function(x) {
  n_authors <- length(strsplit(x['Authors'], ";")[[1]])
  n_full_name <- length(strsplit(x['Author.full.names'], ";")[[1]])
  n_id <- length(strsplit(x['Author.s..ID'], ";")[[1]])
  all(sapply(list(n_authors, n_full_name, n_id), function(y) y == n_authors))
})
sum(same_num_auth) == nrow(usc_data) # something is wrong
# author name has ;
which(same_num_auth == FALSE)
# manually look to find the problem + fix
grep("íte;", usc_data[1396,])
usc_data[1396,] <- gsub("íte;", "í", usc_data[1396,])
grep("&ntilde;", usc_data[1471,])
usc_data[1471,] <- gsub("&ntilde;", "ñ", usc_data[1471,])
# check again
same_num_auth <- apply(usc_data, 1, function(x) {
  n_authors <- length(strsplit(x['Authors'], ";")[[1]])
  n_full_name <- length(strsplit(x['Author.full.names'], ";")[[1]])
  n_id <- length(strsplit(x['Author.s..ID'], ";")[[1]])
  all(sapply(list(n_authors, n_full_name, n_id), function(y) y == n_authors))
})
sum(same_num_auth) == nrow(usc_data) # fixed

# number of authors per publication
num_authors_per <- apply(usc_data, 1, function(x) {
  length(strsplit(x['Author.s..ID'], ";")[[1]])
})
hist(num_authors_per)
boxplot(num_authors_per)
summary(num_authors_per)

# affiliations written like sentences
haswith <- grep(" with ", usc_data$Authors.with.affiliations)
hasis <- grep(" is ", usc_data$Authors.with.affiliations)

# authors with affiliations split by ; probably matches author names
same_len_name_aff <- apply(usc_data, 1, function(x) {
  length(strsplit(x['Author.full.names'], ";")[[1]]) == length(strsplit(x['Authors.with.affiliations'], ";")[[1]])
})
sum(same_len_name_aff)

# where same affiliation is used for all authors
# exclude single author and affiliations that are short
same_aff_for_all <- apply(usc_data, 1, function(x) {
  auth_affl_vec <- strsplit(x['Authors.with.affiliations'], ";")[[1]]
  if (length(strsplit(x['Affiliations'], ";")[[1]]) == 1 & length(strsplit(x['Affiliations'], ",")[[1]]) <= 12) {
    FALSE
  } else {
    all(grepl(x['Affiliations'], auth_affl_vec, fixed = TRUE))
  }
})
sum(same_aff_for_all)
tmp1 <- usc_data[same_aff_for_all,] %>% select(pubID, Authors, Author.full.names, Author.s..ID, Link, Affiliations, Authors.with.affiliations)
# # new
# multiple_univ <- apply(usc_data, 1, function(x) {
#   if (grepl(";", x['Affiliations'])) { # multiple authors
#     return(FALSE)
#   }
#   if (str_count(x['Affiliations'], "[uU]niv") > 1) { # more than 1 univ
#     if (grepl("University Park", x['Affiliations']) | grepl("USC\\)|California Medical Center", x['Affiliations'])) {
#       return(FALSE)
#     }
#     return(TRUE) 
#   }
#   return(FALSE)
# })
# sum(multiple_univ)
# tmp3 <- usc_data[multiple_univ,] %>% select(pubID, Authors, Author.full.names, Author.s..ID, Link, Affiliations, Authors.with.affiliations)
# same_aff_for_all2 <- apply(usc_data, 1, function(x) {
#   if (length(strsplit(x['Authors'], ";")[[1]]) == 1) {  # only 1 author
#     return(FALSE)
#   }
#   if (length(strsplit(x['Affiliations'], ",")[[1]]) > 13) {
#     auth_affl_vec <- strsplit(x['Authors.with.affiliations'], ";")[[1]]
#     return(all(grepl(x['Affiliations'], auth_affl_vec, fixed = TRUE)))
#   }
#   return(FALSE)
# })
# # exploring manually
# tmp <- usc_data[same_aff_for_all2,] %>% select(pubID, Authors, Author.full.names, Author.s..ID, Link, Affiliations, Authors.with.affiliations)
# tmp$aff_length <- sapply(tmp$Affiliations, function(x) {length(strsplit(x, " ")[[1]])})
# tmp$aff_length <- sapply(tmp$Affiliations, function(x) {length(strsplit(x, ",")[[1]])})
# sum(same_aff_for_all2)
# 
# grep("Yale University / University of Southern California", usc_data$Affiliations)
# usc_data$Affiliations[23692] <- "Yale University; University of Southern California"
# grep("University of Southern California / Pasadena City College", usc_data$Affiliations)
# usc_data$Affiliations[23611] <- "University of Southern California; Pasadena City College"
# grep("University of Southern California / University of Hong Kong", usc_data$Affiliations)
# usc_data$Affiliations[23700] <- "University of Southern California; University of Hong Kong"
# grep("USC Information Sciences Institute, University of Washington, Seattle, United States", usc_data$Affiliations)
# grep("University of Southern California, University of California, Irvine, United States", usc_data$Affiliations)

not_covered <- unique(c(haswith, hasis, 
                        which(same_len_name_aff == FALSE), 
                        which(same_aff_for_all == TRUE)))
not_covered_pubs <- usc_data[not_covered,]

current_pubs <- usc_data[-not_covered,]
# parse, split by ;
all_current_authors <- apply(current_pubs, 1, function(x) {
  auth_name <- trimws(strsplit(x['Authors'], ";")[[1]])
  auth_full_names <- trimws(strsplit(x['Author.full.names'], ";")[[1]])
  auth_affl_vec <- trimws(strsplit(x['Authors.with.affiliations'], ";")[[1]])
  auth_ids <- trimws(strsplit(x['Author.s..ID'], ";")[[1]])
  res <- data.frame(auth_name, auth_full_names, auth_affl_vec, auth_ids)
  res$pubID <- x['pubID']
  res$Link <- x['Link']
  res
})
all_current_authors_df <- do.call(rbind, all_current_authors)

# create a column to determine whether author is USC affiliated or not
all_current_authors_df$USC <- grepl(usc_regex, all_current_authors_df$auth_affl_vec)

current_usc_authors <- all_current_authors_df %>%
  filter(USC == TRUE) %>%
  filter(!grepl("Baidu Research", auth_affl_vec))  # remove "Baidu Research (USA) University of Southern California Department of Computer Vision Technology (VIS), Baidu Incorporation, United States"

# create columns for authorID, fullname, firstname, lastname
# all have ( and )
all(grepl("\\(",current_usc_authors$auth_full_names))
all(grepl("\\)",current_usc_authors$auth_full_names))
current_usc_authors$authorID <- regmatches(current_usc_authors$auth_full_names, regexpr("[0-9]+", current_usc_authors$auth_full_names))
all.equal(current_usc_authors$auth_ids, current_usc_authors$authorID) # author IDs are equal
current_usc_authors$full_name <- sapply(current_usc_authors$auth_full_names, function(x) {
  trimws(strsplit(x, "\\(")[[1]][1])
})
# some names without ,
which(grepl(",", current_usc_authors$auth_full_names) == FALSE)
current_usc_authors$last_name <- sapply(current_usc_authors$full_name, function(x) {
  if (grepl(",", x)) {
    strsplit(x, ",")[[1]][1]
  } else {
    s <- strsplit(x, " ")[[1]]
    if (length(s) == 1) {
      ""
    } else {
      s[1]
    }
  }
})
current_usc_authors$first_name <- sapply(current_usc_authors$full_name, function(x) {
  if (grepl(",", x)) {
    strsplit(x, ",")[[1]][2]
  } else {
    s <- strsplit(x, " ")[[1]]
    if (length(s) == 1) {
      s[1]
    } else {
      s[2]
    }
  }
})


# authors_working <- apply(current_pubs, 1, function(x) {
#   res = data.frame()
#   auth_name = trimws(strsplit(x['Authors'], ";")[[1]])
#   auth_full_name = trimws(strsplit(x['Author.full.names'], ";")[[1]])
#   auth_affl_vec = strsplit(x['Authors.with.affiliations'], ";")[[1]]
#   # if (length(auth_full_name) == length(auth_affl_vec)) 
#   # already removed those that don't satisfy ^
#   matched = grep(usc_regex, auth_affl_vec, ignore.case = FALSE)
#   for (i in matched) {
#     name = trimws(strsplit(auth_full_name[i], "\\(")[[1]][1])
#     if (grepl(",", name)) {
#       lastname = strsplit(name, ",")[[1]][1]
#       firstname = strsplit(name, ",")[[1]][2]
#     } else {
#       lastname = strsplit(name, " ")[[1]][2]
#       firstname = strsplit(name, " ")[[1]][1]
#     }
#     authorID = trimws(strsplit(x['Author.s..ID'], ";")[[1]][i])
#     res <- rbind(res, data.frame(pubID = x['pubID'], Link = x['Link'], 
#                                  authorID = authorID, "Name" = auth_name[i], 
#                                  "FName" = firstname, "LName" = lastname,
#                                  fullName = auth_full_name[i],
#                                  affl = auth_affl_vec[i]))
#   }
#   res
# })
# 
# authors_working <- do.call(rbind, authors_working)
# 
# write.csv(authors_working,
          # here::here("data_processed/authors_working.csv"),
          # row.names = FALSE)


# use scopus API for rest of the pubs
# takes a while... .4 sec * 10 authors * # pubs
# saving output for all entries
# no_usc <- apply(not_covered_pubs, 1, function(x) {
#   grepl(usc_regex, x['Affiliations'], ignore.case = TRUE)
# })
other_authors = data.frame()
for (j in 1:nrow(not_covered_pubs)) {
  ten_authors = trimws(strsplit(not_covered_pubs[j,]$Author.s..ID, ";")[[1]])
  n = length(ten_authors)
  if (n > 10) {
    ten_authors =  ten_authors[c(1:9,n)]
  }
  for (i in ten_authors) {
    api_res = get_complete_author_info(au_id = i, count = 1,
                                       headers = c("X-ELS-Insttoken" = institution_token, 
                                                   "X-ELS-APIKey" = elsevier_api_key))
    if (api_res$content$`search-results`$`opensearch:totalResults` == "1") {
      x_affilid = api_res$content$`search-results`$entry[[1]]$`affiliation-current`$`affiliation-id`
      x_affilname = api_res$content$`search-results`$entry[[1]]$`affiliation-current`$`affiliation-name`
      # if (!is.null(x_affilid) && x_affilid %in% usc_affiliation_ids) {
        # dv <- ifelse(x_affilid == "60015183",
        #              "Keck School of Medicine",
        #              ifelse(x_affilid == "60022143",
        #                     "Norris Comprehensive Cancer Center",
        #                     ifelse(x_affilid == "60009207",
        #                            "LAC+USC Medical Center",
        #                            ifelse(x_affilid == "60015400",
        #                                   "Information Sciences Institute",
        #                                   ifelse(x_affilid == "60013994",
        #                                          "School of Pharmacy",
        #                                          ifelse(x_affilid == "60099658",
        #                                                 "Marshall School of Business",
        #                                                 ifelse(x_affilid == "60019009",
        #                                                        "Herman Ostrow School of Dentistry",
        #                                                        ifelse(x_affilid == "60086699",
        #                                                               "Andrus Gerontology",
        #                                                               ifelse(x_affilid == "60005801",
        #                                                                      "Women's Hospital",
        #                                                                      ifelse(x_affilid == "60026672",
        #                                                                             "Gould School of Law",
        #                                                                             ifelse(x_affilid == "60006209",
        #                                                                                    "Keck Hospital",
        #                                                                                    ifelse(x_affilid == "60268548",
        #                                                                                           "Keck Medicine",
        #                                                                                           ""))))))))))
        #              ))
      lastname = ""
      if ("surname" %in% names(api_res$content$`search-results`$entry[[1]]$`preferred-name`)) {
        lastname = api_res$content$`search-results`$entry[[1]]$`preferred-name`$surname
      }
      firstname = ""
      if ("given-name" %in% names(api_res$content$`search-results`$entry[[1]]$`preferred-name`)) {
        firstname = api_res$content$`search-results`$entry[[1]]$`preferred-name`$`given-name`
        # firstname = strsplit(api_res$content$`search-results`$entry[[1]]$`preferred-name`$`given-name`, " ")[[1]][1]
        # name = paste(lastname, " ", substr(firstname, 1, 1), ".", sep = "")
        # fullname = paste(lastname, api_res$content$`search-results`$entry[[1]]$`preferred-name`$`given-name`)
      } 
      # else {
      #   firstname = NA
      #   name = lastname
      #   fullname = lastname
      # }
      initials = ""
      if ("initials" %in% names(api_res$content$`search-results`$entry[[1]]$`preferred-name`)) {
        initials = api_res$content$`search-results`$entry[[1]]$`preferred-name`$initials
      }
      other_authors <- rbind(other_authors, 
                             data.frame(pubID = not_covered_pubs[j,]$pubID, 
                                        Link = not_covered_pubs[j,]$Link,
                                        authorID = i,
                                        firstname = firstname, 
                                        lastname = lastname,
                                        initials = initials,
                                        affilid = ifelse(is.null(x_affilid), "", x_affilid),
                                        affilname = ifelse(is.null(x_affilname), "", x_affilname)))
      # }
    }
    Sys.sleep(.4) # rate limit exceeded
  }
}

write.csv(other_authors,
          here::here("data_processed/other_authors.csv"),
          row.names = FALSE)

# 2 results 57557858100

# grab authors, authors full name, authorID columns
not_covered_pubs_authors <- apply(not_covered_pubs, 1, function(x) {
  auth_name <- trimws(strsplit(x['Authors'], ";")[[1]])
  auth_full_names <- trimws(strsplit(x['Author.full.names'], ";")[[1]])
  auth_ids <- trimws(strsplit(x['Author.s..ID'], ";")[[1]])
  res <- data.frame(auth_name, auth_full_names, auth_ids)
  res$pubID <- x['pubID']
  res$Link <- x['Link']
  res
})
not_covered_authors_df <- do.call(rbind, not_covered_pubs_authors)

# join with Scopus API output
not_covered_authors <- merge(not_covered_authors_df, other_authors,
                             by.x = c("pubID", "Link", "auth_ids"),
                             by.y = c("pubID", "Link", "authorID"))

not_covered_usc_authors <- not_covered_authors %>%
  filter(affilid %in% as.character(usc_affiliation_ids) |
           grepl(usc_regex, affilname))

# might have to remake the first and last name columns
# since they don't match
tmp <- paste(not_covered_usc_authors$lastname, not_covered_usc_authors$initials)
all.equal(tmp, not_covered_usc_authors$auth_name)

# combine current_usc_authors and not_covered_usc_authors df
names(current_usc_authors)
names(not_covered_usc_authors)
not_covered_usc_authors$auth_affl_vec <- not_covered_usc_authors$affilname
not_covered_usc_authors$USC <- TRUE
not_covered_usc_authors$first_name <- not_covered_usc_authors$firstname
not_covered_usc_authors$last_name <- not_covered_usc_authors$lastname
not_covered_usc_authors$full_name <- sapply(not_covered_usc_authors$auth_full_names, function(x) {
  trimws(strsplit(x, "\\(")[[1]][1])
})
current_usc_authors$initials <- ""

all_usc_authors <- rbind(select(current_usc_authors, intersect(names(current_usc_authors), names(not_covered_usc_authors))),
      select(not_covered_usc_authors, intersect(names(current_usc_authors), names(not_covered_usc_authors))))

write.csv(all_usc_authors,
          here::here("data_processed/all_usc_authors.csv"),
          row.names = FALSE)

bridge_table <- all_usc_authors %>%
  rename(authorID = auth_ids) %>%
  select(pubID, Link, authorID)
write.csv(bridge_table,
          here::here("data_processed/bridge_table.csv"),
          row.names = FALSE)

# bridge_table <- authors_working %>% select(pubID, Link, authorID)
# bridge_table <- rbind(bridge_table, authors_other %>% select(pubID, Link, authorID))
# write.csv(bridge_table,
#           here::here("data_processed/bridge_new.csv"),
#           row.names = FALSE)

all_usc_authors_dept_div <- all_usc_authors %>%
  group_by(auth_ids) %>%
  summarize(name = first(auth_name),
            name_id = first(auth_full_names),
            affls = paste(auth_affl_vec, collapse = ";"),
            fullname = first(full_name),
            lastname = first(last_name),
            firstname = first(first_name),
            initials = first(initials)) %>%
  rename(authorID = auth_ids)

# authors_parsed <- authors_working %>%
#   group_by(authorID) %>%
#   summarize(affls = paste(trimws(affl)[!duplicated(trimws(affl))], collapse = ";"),
#             name = first(Name), firstname = first(FName), lastname = first(LName),
#             fullname = first(fullName)
#             # names = paste(Name[!duplicated(Name)], collapse = ";"),
#             # firstnames = paste(FName[!duplicated(FName)], collapse = ";"),
#             # lastnames = paste(LName[!duplicated(LName)], collapse = ";"),
#             # fullnames = paste(fullName[!duplicated(fullName)], collapse = ";")
#   )

all_usc_authors_dept_div$Dept <- ""
all_usc_authors_dept_div$Div <- ""


usc_schools <- read.csv(here::here("data_processed/usc_schools.csv"))
usc_departments <- read.csv(here::here("data_processed/usc_departments.csv"))

# takes 5 min
for (i in 1:nrow(all_usc_authors_dept_div)) {
  trimmed_affl_seg <- strsplit(all_usc_authors_dept_div[i,]$affls, ";")[[1]]
  affl_seg <- sapply(trimmed_affl_seg, function(x) {
    trimws(strsplit(x, ",")[[1]][-1])
  })
  affl_segs <- unlist(affl_seg)
  affl_segs <- unname(affl_segs)
  # affl_segs <- unique(affl_segs) # breaks up names w ,
  affiliations <- paste(affl_segs, collapse = " ")
  matched_dept <- apply(usc_departments, 1, function(x) {
    if (grepl(x['Pattern'], affiliations)) {
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
  all_usc_authors_dept_div[i,]$Dept <- paste(departments[!duplicated(departments)], collapse = ";")
  
  matched_schools <- apply(usc_schools, 1, function(x) {
    if (grepl(x['pattern'], affiliations)) {
      x['name']
    }
  })
  
  all_schools <- c(divisions, unname(unlist(matched_schools)))
  all_schools <- all_schools[!duplicated(all_schools)]
  
  all_usc_authors_dept_div[i,]$Div <- paste(all_schools, collapse = ";")
}

write.csv(all_usc_authors_dept_div,
          here::here("data_processed/all_usc_authors_dept_div_11_6_23.csv"),
          row.names = FALSE)

# authors_other <- read.csv(here::here("data_processed/authors_other.csv"))
# 
# authors_other$Dept = ifelse(authors_other$Div %in% 
#                               c("LAC+USC Medical Center",
#                                 "Norris Comprehensive Cancer Center",
#                                 "Information Sciences Institute"), 
#                             authors_other$Div, 
#                             ifelse(authors_other$Div == "Women's Hospital",
#                             "LAC+USC Medical Center", ""))
# authors_other$Div = ifelse(authors_other$Div %in% 
#                              c("LAC+USC Medical Center", 
#                                "Norris Comprehensive Cancer Center",
#                                "Keck Medicine",
#                                "Keck School of Medicine",
#                                "Keck Hospital",
#                                "Women's Hospital"),
#                            "Keck Medicine of USC",
#                            ifelse(authors_other$Div == "Information Sciences Institute",
#                                   "Viterbi School of Engineering",
#                                   ifelse(authors_other$Div == "School of Pharmacy",
#                                          "Alfred E. Mann School of Pharmacy and Pharmaceutical Sciences",
#                                          ifelse(authors_other$Div == "Herman Ostrow School of Dentistry",
#                                                 "Herman Ostrow School of Dentistry of USC", 
#                                                 ifelse(authors_other$Div == "Andrus Gerontology",
#                                                        "Leonard Davis School of Gerontology",
#                                                        "")))))
# 
# authors_more <- authors_other[!duplicated(authors_other$authorID),] %>% 
#   mutate(affls = "", name = Name, firstname = FName, lastname = LName, fullname = fullName) %>%
#   select(authorID, affls, name, firstname, lastname, fullname, Dept, Div)
# 
# authors_combined_temp <- rbind(authors_parsed, authors_more)
# authors_combined <- authors_combined_temp %>%
#   group_by(authorID) %>%
#   summarize(
#     affls = first(affls),
#     name = last(name),
#     firstname = last(firstname),
#     lastname = last(lastname),
#     fullname = last(fullname),
#     Dept = paste(Dept[nzchar(Dept)], collapse = ";"),
#     Div = paste(Div[nzchar(Div)], collapse = ";")
#     #Dept = paste(Dept[!duplicated(Dept)][nzchar(Dept[!duplicated(Dept)])], collapse = ";"),
#     #Div = paste(Div[!duplicated(Div)][nzchar(Div[!duplicated(Div)])], collapse = ";")
#     )
remove_duplicates <- function(x) {
  list_values <- strsplit(x, ";")[[1]]
  unique_values <- list_values[!duplicated(list_values)]
  paste(unique_values, collapse = ";")
}
# authors_combined$Div <- sapply(authors_combined$Div, remove_duplicates)
# authors_combined$Dept <- sapply(authors_combined$Dept, remove_duplicates)

# # remove herman ostrow school of dentistry when chan division or division of biokinesiology are present
# authors_combined$Div <- unlist(
#   lapply(authors_combined$Div,
#          function(x) {
#            if (grepl("Herman Ostrow", x) & grepl("(Chan Division|Division of Biokinesiology)", x)) {
#              paste(setdiff(strsplit(x, ";")[[1]],
#                            c("Herman Ostrow School of Dentistry of USC")),
#                    collapse = ";")
#            } else {
#              x
#            }
#            }))

# number of entries empty dept AND div
# length(which(authors_combined$Dept == "" & authors_combined$Div == ""))
# 
# write.csv(authors_combined,
#           here::here("data_processed/authors_combined.csv"),
#           row.names = FALSE,
#           fileEncoding = 'UTF-8')

# authors_combined <- read.csv(here::here("data_processed/authors_combined.csv"))

authors_combined_name <- all_usc_authors_dept_div %>%
  group_by(firstname, lastname) %>% 
  summarize(authorIDs = paste(authorID, collapse = "!"),
            affiliations = paste(affls, collapse = "!"),
            names = paste(name, collapse = "!"),
            name_ids = paste(name_id, collapse = "!"),
            fullnames = paste(fullname, collapse = "!"),
            initials = paste(initials, collapse = "!"),
            depts = paste(Dept, collapse = "!"),
            divs = paste(Div, collapse = "!"))

authors_multiple_entries <- authors_combined_name %>%
  filter(grepl("!", authorIDs))

# maximum # of authors per same f & l name
max(sapply(authors_multiple_entries$authorIDs, function(x) {
  length(strsplit(x, "!")[[1]])
}))

# # for those with exactly the same dept and div
# authors_combined %>% filter(Dept != "") %>% filter(Div != "") %>%
#   group_by(firstname, lastname, Dept, Div) %>%
#   summarize(authorIDs = paste(authorID, collapse = ";")) %>%
#   filter(length(strsplit(authorIDs, ";")[[1]])>1) -> same_name_dept_div

# map old authorID (key) to new authorID (value)
# keys are names
# c(old = new)
authorID_old = c()
authorID_new = c()
# same author if firstname, lastname, dept, div are all the same
# same author if firstname, lastname are the same and >=1 dept overlap
# ignore those with no dept or no div
for (i in 1:nrow(authors_multiple_entries)) {
  current_row <- authors_multiple_entries[i,]
  num_id <- length(strsplit(current_row$authorIDs, "!")[[1]])
  queue <- 1:num_id
  while (length(queue) > 0) {
    j = queue[1]
    queue <- queue[-c(1)]
    j_dept <- strsplit(current_row$depts, "!")[[1]][j]
    j_div <- strsplit(current_row$divs, "!")[[1]][j]
    
    if (identical(j_dept, character(0)) | identical(j_div, character(0))) next
    if (is.na(j_dept) | is.na(j_div)) next
    if (j_dept == "" | j_div == "") next
    
    remove_from_queue <- c()
    for (k in queue) {
      k_dept <- strsplit(current_row$depts, "!")[[1]][k]
      k_div <- strsplit(current_row$divs, "!")[[1]][k]
      
      if (identical(k_dept, character(0)) | identical(k_div, character(0))) next
      if (is.na(k_dept) | is.na(k_div)) next
      if (k_dept == "" | k_div == "") next
      
      if (j_dept == k_dept & j_div == k_div) {
        remove_from_queue <- append(remove_from_queue, k)
        authorID_old <- append(authorID_old, strsplit(current_row$authorIDs, "!")[[1]][k])
        authorID_new <- append(authorID_new, strsplit(current_row$authorIDs, "!")[[1]][j])
      } else if (length(intersect(strsplit(j_dept, ";")[[1]], 
                                  strsplit(k_dept, ";")[[1]])) > 0) {
        remove_from_queue <- append(remove_from_queue, k)
        authorID_old <- append(authorID_old, strsplit(current_row$authorIDs, "!")[[1]][k])
        authorID_new <- append(authorID_new, strsplit(current_row$authorIDs, "!")[[1]][j])
      }
    }
    queue <- queue[!queue %in% remove_from_queue]
  }
}

authorID_map <- setNames(authorID_new, authorID_old)
authorID_map_df <- data.frame(authorID_new, authorID_old)
write.csv(authorID_map_df,
          "data_processed/authorID_map2.csv",
          row.names = FALSE)

all_usc_authors_dept_div %>% filter(authorID %in% authorID_old) -> entries_to_be_deleted

new_authorIDs <- sapply(all_usc_authors_dept_div$authorID, function(x) {
  if (as.character(x) %in% authorID_old) {
    authorID_map[as.character(x)]
  } else {
    x
  }
})

new_authorIDs <- unname(new_authorIDs)

authors_combined_new_ids <- all_usc_authors_dept_div
authors_combined_new_ids$authorID <- new_authorIDs
authors_combined_new_ids %>%
  group_by(authorID) %>%
  summarize(affls = paste(affls, collapse = ";"),
            name = first(name),
            name_id = first(name_id),
            firstname = first(firstname),
            lastname = first(lastname),
            fullname = first(fullname),
            initials = first(initials),
            Dept = paste(Dept, collapse = ";"),
            Div = paste(Div, collapse = ";")) -> authors_temp
authors_temp$Dept <- sapply(authors_temp$Dept, remove_duplicates)
authors_temp$Div <- sapply(authors_temp$Div, remove_duplicates)

authors_without_some_same_names <- authors_temp
write.csv(authors_without_some_same_names,
          "data_processed/authors_without_some_same_names2.csv",
          row.names = FALSE)

# update bridge table
new_authorIDs <- sapply(bridge_table$authorID, function(x) {
  if (as.character(x) %in% authorID_old) {
    authorID_map[as.character(x)]
  } else {
    x
  }
})
new_authorIDs <- unname(new_authorIDs)
bridge_table2 <- bridge_table
bridge_table2$authorID <- new_authorIDs
write.csv(bridge_table2,
          "data_processed/bridge_table_after_1_combine.csv",
          row.names = FALSE)

authors_without_some_same_names %>%
  group_by(firstname, lastname) %>%
  summarize(authorIDs = paste(authorID, collapse = "###"),
            affiliations = paste(affls, collapse = "###"),
            names = paste(name, collapse = "###"),
            name_ids = paste(name_id, collapse = "###"),
            fullnames = paste(fullname, collapse = "###"),
            initials = paste(initials, collapse = "###"),
            depts = paste(Dept, collapse = "###"),
            divs = paste(Div, collapse = "###")) %>%
  filter(grepl("###", authorIDs)) -> authors_still_with_same_name

exclude_rows = c()
for (i in 1:nrow(authors_still_with_same_name)) {
  current_row <- authors_still_with_same_name[i,]
  current_divisions <- strsplit(current_row$divs, "###")[[1]]
  if (any(current_divisions == "")) next
  if (length(unique(current_divisions)) == 1) next
  lapply(current_divisions, function(x) {
    strsplit(x, ";")[[1]]
  }) -> separate_divisions
  if (length(Reduce(union, separate_divisions)) != length(unlist(separate_divisions))) {
    next
  }
  if (identical(Reduce(intersect, separate_divisions), character(0))) {
    exclude_rows <- append(exclude_rows, i)
  }
}

authors_prob_diff <- authors_still_with_same_name[exclude_rows,]

authors_with_same_name <- authors_still_with_same_name[-exclude_rows,]
num_ids <- sapply(authors_with_same_name$authorIDs, function(x) {
  length(strsplit(x, "###")[[1]])
})
authors_with_same_name$Number_of_IDs <- num_ids
# 
# write.csv(authors_with_same_name,
#           "data_processed/authors_with_same_name.csv",
#           row.names = FALSE)

authors_without_some_same_names %>%
  group_by(name) %>%
  summarize(authorIDs = paste(authorID, collapse = "###"),
            name_ids = paste(name_id, collapse = "###"),
            affiliations = paste(affls, collapse = "###"),
            firstnames = paste(firstname, collapse = "###"),
            lastnames = paste(lastname, collapse = "###"),
            fullnames = paste(fullname, collapse = "###"),
            initials = paste(initials, collapse = "###"),
            depts = paste(Dept, collapse = "###"),
            divs = paste(Div, collapse = "###")) %>%
  filter(grepl("###", authorIDs)) -> authors_still_with_same_name2
num_ids <- sapply(authors_still_with_same_name2$authorIDs, function(x) {
  length(strsplit(x, "###")[[1]])
})
authors_still_with_same_name2$Number_of_IDs <- num_ids


dontkeep <- is.element(authors_with_same_name$authorIDs, authors_still_with_same_name2$authorIDs)
names(authors_with_same_name)[1] <- "firstnames"
names(authors_with_same_name)[2] <- "lastnames"
names(authors_still_with_same_name2)[1] <- "names"
authors_to_check <- rbind(authors_still_with_same_name2, authors_with_same_name[!dontkeep,])
write.csv(authors_to_check,
          "data_processed/authors_to_check.csv",
          row.names = FALSE)

# authors_to_check %>%
#   filter(Number_of_IDs>5) -> too_many

# 

six_or_more <- read.csv("data_processed/authors_to_check - six or more.csv")
# check
which(six_or_more$COMBINE_TF == TRUE)
which(six_or_more$combine..duplicate.the.row.if.there.are.multiple.combines. != "")
six_or_more_i <- six_or_more %>%
  filter(COMBINE_TF == TRUE)
six_or_more_authorID <- sapply(six_or_more_i$combine..duplicate.the.row.if.there.are.multiple.combines., function(x) {
  strsplit(x, "###")[[1]]
})
six_or_more_authorIDs <- unlist(six_or_more_authorID)
which(!six_or_more_authorIDs %in% all_usc_authors_dept_div$authorID) # some authorIDs don't exist
which(!six_or_more_authorIDs %in% authors_without_some_same_names$authorID) # additional authorIDs have already been combined

six_or_more_c <- lapply(six_or_more_authorID, function(x) {
  old_id = x[2:length(x)]
  new_id = rep(x[1], length(old_id))
  setNames(new_id, old_id)
})

six_or_more_map <- unlist(six_or_more_c)
names(six_or_more_map) <- sapply(names(six_or_more_map), function(x) {
  s <- strsplit(x,"\\.")[[1]]
  s[length(s)]
})

six_or_more_map_df <- data.frame(authorID_new = unname(six_or_more_map), authorID_old = names(six_or_more_map))
write.csv(six_or_more_map_df,
          "data_processed/six_or_more_map_df.csv",
          row.names = FALSE)
new_authorIDs <- sapply(authors_without_some_same_names$authorID, function(x) {
  if (x %in% names(six_or_more_map)) {
    six_or_more_map[x]
  } else {
    x
  }
})
new_authorIDs <- unname(new_authorIDs)
authors_six <- authors_without_some_same_names
authors_six$authorID <- new_authorIDs
authors_six %>%
  group_by(authorID) %>%
  summarize(affls = paste(affls, collapse = ";"),
            name = first(name),
            name_id = first(name_id),
            firstname = first(firstname),
            lastname = first(lastname),
            fullname = first(fullname),
            initials = first(initials),
            Dept = paste(Dept, collapse = ";"),
            Div = paste(Div, collapse = ";")) -> authors_temp
authors_temp$Dept <- sapply(authors_temp$Dept, remove_duplicates)
authors_temp$Div <- sapply(authors_temp$Div, remove_duplicates)
# replace firstname lastname fullname columns
for (i in which(authors_temp$authorID %in% six_or_more_map)) {
  cur_row <- authors_temp[i,]
  cur_authorID <- cur_row$authorID
  idx <- grep(cur_authorID, six_or_more_i$combine..duplicate.the.row.if.there.are.multiple.combines.)
  authors_temp[i,]$firstname <- six_or_more_i$First.Name[idx]
  authors_temp[i,]$lastname <- six_or_more_i$Last.Name[idx]
  authors_temp[i,]$fullname <- six_or_more_i$Full.Name..Last..First.Middle.[idx]
}
# View(authors_temp[which(authors_temp$authorID %in% six_or_more_map),])
authors_after_six <- authors_temp
write.csv(authors_after_six,
          "data_processed/authors_after_six.csv",
          row.names = FALSE)

# update bridge
new_authorIDs <- sapply(bridge_table2$authorID, function(x) {
  if (x %in% names(six_or_more_map)) {
    six_or_more_map[x]
  } else {
    x
  }
})
new_authorIDs <- unname(new_authorIDs)
bridge_table3 <- bridge_table2
bridge_table3$authorID <- new_authorIDs
write.csv(bridge_table3,
          "data_processed/bridge_table_after_six_combine.csv",
          row.names = FALSE)

# 
less_six <- read.csv("data_processed/authors_to_check - less than six.csv")
less_six_i <- less_six %>%
  filter(combine..duplicate.the.row.if.there.are.multiple.combines. != "") %>%
  select(combine..duplicate.the.row.if.there.are.multiple.combines.,
         First.Name, Last.Name, Full.Name.Last..First.Middle.) %>%
  distinct()
less_six_authorID <- sapply(less_six_i$combine..duplicate.the.row.if.there.are.multiple.combines., function(x) {
  strsplit(x, "###")[[1]]
})
less_six_authorIDs <- unlist(less_six_authorID)
which(!less_six_authorIDs %in% all_usc_authors_dept_div$authorID)
which(!less_six_authorIDs %in% authors_after_six$authorID)
less_six_c <- lapply(less_six_authorID, function(x) {
  old_id = x[2:length(x)]
  new_id = rep(x[1], length(old_id))
  setNames(new_id, old_id)
})
less_six_map <- unlist(less_six_c)
names(less_six_map) <- sapply(names(less_six_map), function(x) {
  s <- strsplit(x,"\\.")[[1]]
  s[length(s)]
})
less_six_map_df <- data.frame(authorID_new = unname(less_six_map), authorID_old = names(less_six_map))
write.csv(less_six_map_df,
          "data_processed/less_six_map_df.csv",
          row.names = FALSE)

new_authorIDs <- sapply(authors_after_six$authorID, function(x) {
  if (x %in% names(less_six_map)) {
    less_six_map[x]
  } else {
    x
  }
})
new_authorIDs <- unname(new_authorIDs)
authors_one <- authors_after_six
authors_one$authorID <- new_authorIDs
authors_one %>%
  group_by(authorID) %>%
  summarize(affls = paste(affls, collapse = ";"),
            name = first(name),
            name_id = first(name_id),
            firstname = first(firstname),
            lastname = first(lastname),
            fullname = first(fullname),
            initials = first(initials),
            Dept = paste(Dept, collapse = ";"),
            Div = paste(Div, collapse = ";")) -> authors_temp
authors_temp$Dept <- sapply(authors_temp$Dept, remove_duplicates)
authors_temp$Div <- sapply(authors_temp$Div, remove_duplicates)
# replace firstname lastname fullname columns
for (i in which(authors_temp$authorID %in% less_six_map)) {
  cur_row <- authors_temp[i,]
  cur_authorID <- cur_row$authorID
  idx <- grep(cur_authorID, less_six_i$combine..duplicate.the.row.if.there.are.multiple.combines.)
  authors_temp[i,]$firstname <- less_six_i$First.Name[idx]
  authors_temp[i,]$lastname <- less_six_i$Last.Name[idx]
  authors_temp[i,]$fullname <- less_six_i$Full.Name.Last..First.Middle.[idx]
}
# View(authors_temp[which(authors_temp$authorID %in% less_six_map),])
authors_after_second <- authors_temp
write.csv(authors_after_second,
          "data_processed/authors_after_second.csv",
          row.names = FALSE)

# update bridge
# bridge_table3$authorID <- as.character(bridge_table3$authorID)
new_authorIDs <- sapply(bridge_table3$authorID, function(x) {
  if (x %in% names(less_six_map)) {
    less_six_map[x]
  } else {
    x
  }
})
new_authorIDs <- unname(new_authorIDs)
bridge_table4 <- bridge_table3
bridge_table4$authorID <- new_authorIDs
write.csv(bridge_table4,
          "data_processed/bridge_table_after_second_csv.csv",
          row.names = FALSE)

# manual fixes
bridge_table_after_fix <- bridge_table4
authors_after_fix <- authors_after_second
authors_after_fix$firstname <- trimws(authors_after_fix$firstname)
# change 57668533700 to 57211586105 for kim, eunjin
authors_after_fix <- authors_after_fix[-which(authors_after_fix$authorID == "57668533700"),]
bridge_table_after_fix$authorID[which(bridge_table_after_fix$authorID == "57668533700")] <- 57211586105
# change 57218170772 to 56740038200 for jha, sachin
authors_after_fix <- authors_after_fix[-which(authors_after_fix$authorID == "57218170772"),]
bridge_table_after_fix$authorID[which(bridge_table_after_fix$authorID == "57218170772")] <- 56740038200
# fix 53983918800 name  to Dan Wadhwani
authors_after_fix[which(authors_after_fix$authorID == "53983918800"),]$name <- "Wadhwani D."
authors_after_fix[which(authors_after_fix$authorID == "53983918800"),]$firstname <- "Dan"
authors_after_fix[which(authors_after_fix$authorID == "53983918800"),]$lastname <- "Wadhwani"
authors_after_fix[which(authors_after_fix$authorID == "53983918800"),]$fullname <- "Wadhwani, Dan"
# change 57731298700 to 6602691708 for Andrea Belz
authors_after_fix <- authors_after_fix[-which(authors_after_fix$authorID == "57731298700"),]
bridge_table_after_fix$authorID[which(bridge_table_after_fix$authorID == "57731298700")] <- 6602691708
# change 57857181600 to 6505568173 for Zoe Corwin
authors_after_fix <- authors_after_fix[-which(authors_after_fix$authorID == "57857181600"),]
bridge_table_after_fix$authorID[which(bridge_table_after_fix$authorID == "57857181600")] <- 6505568173
# 2 david j cote
# change 38961461600 and 26538380300 to 57226278102 for Josh West
authors_after_fix <- authors_after_fix[-which(authors_after_fix$authorID == "38961461600"),]
bridge_table_after_fix$authorID[which(bridge_table_after_fix$authorID == "38961461600")] <- 57226278102
authors_after_fix <- authors_after_fix[-which(authors_after_fix$authorID == "26538380300"),]
bridge_table_after_fix$authorID[which(bridge_table_after_fix$authorID == "26538380300")] <- 57226278102
authors_after_fix[which(authors_after_fix$authorID == "57226278102"),]$fullname <- "West, Josh"
authors_after_fix[which(authors_after_fix$authorID == "57226278102"),]$firstname <- "Josh"





write.csv(authors_after_fix,
          "data_processed/authors_after_fix_11_6_23.csv",
          row.names = FALSE)
write.csv(bridge_table_after_fix,
          "data_processed/bridge_after_fix_11_6_23.csv",
          row.names = FALSE)
###############################################################################
library(stringi)
authors_after_fix2 <- authors_after_fix
authors_after_fix2$name <- stri_trans_general(authors_after_fix$name, "latin-ascii")

# fix any missing/messed up names
authors_only <- authors_working %>% select(authorID, Name, FName, LName)
authors_only <- authors_only[!duplicated(authors_only),]
authors_only <- authors_only[!duplicated(authors_only$authorID),]

grep("[\\.]", authors_only$FName) # a lot are abbrev

# manually fix NA
missing_fnames <- which(is.na(authors_only$FName))
authors_only[missing_fnames[1],]$Name = "Matei N."
authors_only[missing_fnames[1],]$FName = "Nathanael"
authors_only[missing_fnames[1],]$LName = "Matei"
# NA ppl are hard to figure out who -> replace with ""
missing_fnames <- which(is.na(authors_only$FName))
authors_only[missing_fnames,]$FName = ""

write.csv(authors_only,
          here::here("data_processed/authors_working.csv"),
          row.names = FALSE)



# testing --------------------------------------


authors_working <- read.csv("data_processed/authors_working.csv")
authors_other <- read.csv("data_processed/authors_other.csv")
authors_working$Div = ""
authors_other$affl = ""
authors_with_pubs_all <- rbind(authors_working, authors_other)

# clean symbols first

distinct_authorid_only <- authors_with_pubs_all %>% 
  distinct(authorID, .keep_all = TRUE) 

distinct_authorid_only_with_names <- authors_with_pubs_all %>% 
  distinct(authorID, FName, LName, .keep_all = TRUE) 

difference <- distinct_authorid_only_with_names %>% anti_join(distinct_authorid_only)

# unique_authorid_only <- unique(authors_with_pubs_all[c("FName", "LName", "authorID")]) 


duplicate_first_last_group_by <- distinct_authorid_only %>%
  group_by(FName, LName) %>%
  mutate(n = n()) %>%
  filter(n > 1)

# duplicate_first_last_group_by_all <- authors_with_pubs_all %>%
#   group_by(authorID, FName, LName) %>%
#   mutate(n = n()) %>%
#   filter(n > 1)

no_duplicate_authors <- authors_with_pubs_all %>%
  anti_join(duplicate_first_last_group_by)


# duplicate_first_last_names <- subset(unique_authorid_only, 
#                                      duplicated(unique_authorid_only[c("FName", "LName")]))
write.csv(duplicate_first_last_names,
          here::here("data_processed/duplicate_first_last_names.csv"),
          row.names = FALSE)
write.csv(duplicate_first_last_group_by,
          here::here("data_processed/duplicate_first_last_group_by2.csv"),
          row.names = FALSE)
