# Identify USC authors in the list of publications by
# 1. parsing authors with affiliations text
# 2. searching first 9 + last 1 author's current affiliation on scopus via API

library(here)
library(rscopus)
library(dplyr)

usc_regex <- paste0("([uU]ni[versity\\.]{0,} ([oO]f )?[sS]ou?th[ernm]{0,}[,-]? ?",
                   "[Cc]alifornia)|([kK]eck [Ss]chool [Oo]f [Mm]edicine)|",
                   "([Kk]eck [Mm]edical [Cc]enter)|([Kk]eck [Mm]edical [Ss]chool)")
usc_affiliation_ids <- c(60029311, 60015183, 60022143, 60009207, 60015400,
                         60013994, 60099658, 60019009, 60086699, 60005801,
                         60026672, 60006209, 60268548)

elsevier_api_key <- Sys.getenv("Elsevier_API")
institution_token <- Sys.getenv("Institution_Token")

usc_data <- read.csv(here::here("data_processed/usc_pubs_all.csv"))
#usc_data <- usc_data[-c(20634,24986,4340),] # removing pubs that aren't USC
# usc_data$ScopusAPI <- FALSE

# tmp <- usc_data[sample(1:nrow(usc_data), 10), ]


num_authors_per <- apply(usc_data, 1, function(x) {
  length(strsplit(x['Author.s..ID'], ";")[[1]])
})

haswith <- grep("with", usc_data$Authors.with.affiliations)
hasis <- grep(" is ", usc_data$Authors.with.affiliations)

same_len_name_aff <- apply(usc_data, 1, function(x) {
  length(strsplit(x['Author.full.names'], ";")[[1]]) == length(strsplit(x['Authors.with.affiliations'], ";")[[1]])
})

same_aff_for_all <- apply(usc_data, 1, function(x) {
  auth_affl_vec <- strsplit(x['Authors.with.affiliations'], ";")[[1]]
  if (length(strsplit(x['Affiliations'], ";")[[1]]) == 1 & length(strsplit(x['Affiliations'], ",")[[1]]) <= 12) {
    FALSE
  } else {
    all(grepl(x['Affiliations'], auth_affl_vec, fixed = TRUE))
  }
})

not_covered <- 
  union(
    union(
      union(haswith, hasis), 
      which(same_aff_for_all == TRUE)),
    which(same_len_name_aff == FALSE))
not_covered_pubs <- usc_data[not_covered,]



current_pubs <- usc_data[-not_covered,]
# parse, split by ;
authors_working <- apply(current_pubs, 1, function(x) {
  res = data.frame()
  auth_full_name = trimws(strsplit(x['Author.full.names'], ";")[[1]])
  auth_affl_vec = strsplit(x['Authors.with.affiliations'], ";")[[1]]
  # if (length(auth_full_name) == length(auth_affl_vec)) 
  # already removed those that don't satisfy ^
  matched = grep(usc_regex, auth_affl_vec, ignore.case = FALSE)
  for (i in matched) {
    name = trimws(strsplit(auth_affl_vec[i], ",")[[1]][1])
    lastname = strsplit(name, " ")[[1]][1]
    firstname = strsplit(trimws(strsplit(auth_full_name[i], ",")[[1]][2]), " ")[[1]][1]
    authorID = trimws(strsplit(x['Author.s..ID'], ";")[[1]][i])
    res <- rbind(res, data.frame(pubID = x['pubID'], Link = x['Link'], 
                                 authorID = authorID, "Name" = name, 
                                 "FName" = firstname, "LName" = lastname,
                                 fullName = auth_full_name[i],
                                 affl = auth_affl_vec[i]))
  }
  res
})

authors_working <- do.call(rbind, authors_working)



authors_parsed <- authors_working %>%
  group_by(authorID) %>%
  summarize(affls = paste(trimws(affl)[!duplicated(trimws(affl))], collapse = ";"),
            names = paste(Name[!duplicated(Name)], collapse = ";"),
            firstnames = paste(FName[!duplicated(FName)], collapse = ";"),
            lastnames = paste(LName[!duplicated(LName)], collapse = ";"),
            fullnames = paste(fullName[!duplicated(fullName)], collapse = ";"))

authors_parsed$Dept <- ""
authors_parsed$Div <- ""


usc_schools <- read.csv(here::here("data_processed/usc_schools.csv"))
usc_departments <- read.csv(here::here("data_processed/usc_departments.csv"))

# takes a while
#for (i in 1:nrow(authors_parsed)) {
for (i in 1:10) {
  # trimmed_affl_seg <- sapply(strsplit(authors_parsed[i,]$affls, ";")[[1]], function(x) {
  #   # sub("[^(Hospital)]{8}(CA|Los Angeles)[^(CA|Los Angeles)]*", "", x)
  #   m = gregexec("United States|Los Angeles|CA", x)[[1]]
  #   if (!is.null(dim(m))) {
  #     m = m[1,]
  #     substring(x, 1, m[length(m)]-1)
  #   } else {
  #     x
  #   }
  # })
  # trimmed_affl_seg <- unname(trimmed_affl_seg)
  trimmed_affl_seg <- strsplit(authors_parsed[i,]$affls, ";")[[1]]
  affl_seg <- sapply(trimmed_affl_seg, function(x) {
    trimws(strsplit(x, ",")[[1]][-1])
  })
  affl_segs <- unlist(affl_seg)
  affl_segs <- unname(affl_segs)
  affl_segs <- unique(affl_segs)
  affiliations <- paste(affl_segs, collapse = " ")
  # affl_seg <- trimws(strsplit(trimmed_affl_seg, ",")[[1]][-1]) # ignore name
  dpt <- regmatches(affiliations,
                    gregexec(paste(usc_departments$Pattern, collapse = "|"),
                             affiliations))[[1]]
  if (identical(dpt, character(0))) {
    dpt <- ""
  } else {
    dpt <- dpt[1,]
    dpt <- dpt[!duplicated(dpt)]
    dpt <- paste(dpt, collapse = ";")
  }
  authors_parsed[i,]$Dept <- ifelse(identical(dpt, character(0)), "", dpt)
  # identical(dpt, character(0))
  dv <- regmatches(affiliations, 
                   gregexec(paste(usc_schools$pattern, collapse = "|"), 
                            affiliations))[[1]]
  if (identical(dv, character(0))) {
    dv <- ""
  } else {
    dv <- dv[1,]
    dv <- dv[!duplicated(dv)]
    dv <- paste(dv, collapse = ";")
  }
  
  authors_parsed[i,]$Div <- dv
}

# how to assign div according to dept
# ifelse
# check if some schools are not right 
# 
write.csv(authors_working,
          here::here("data_processed/authors_working.csv"),
          row.names = FALSE) 
write.csv(authors_parsed,
          here::here("data_processed/authors_parsed.csv"),
          row.names = FALSE)  

# use scopus API for rest of the pubs
# takes a while... .4 sec * 10 authors * # pubs
no_usc <- apply(not_covered_pubs, 1, function(x) {
  grepl(usc_regex, x['Affiliations'], ignore.case = TRUE)
})
authors_other = data.frame()
all_api_results <- list()
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
    all_api_results <- append(all_api_results, api_res$content)
    if (api_res$content$`search-results`$`opensearch:totalResults` == "1") {
      x_affilid = api_res$content$`search-results`$entry[[1]]$`affiliation-current`$`affiliation-id`
      if (!is.null(x_affilid) && x_affilid %in% usc_affiliation_ids) {
        dv <- ifelse(x_affilid == "60015183",
                     "Keck School of Medicine",
                     ifelse(x_affilid == "60022143",
                            "Norris Comprehensive Cancer Center",
                            ifelse(x_affilid == "60009207",
                                   "LAC+USC Medical Center",
                                   ifelse(x_affilid == "60015400",
                                          "Information Sciences Institute",
                                          ifelse(x_affilid == "60013994",
                                                 "School of Pharmacy",
                                                 ifelse(x_affilid == "60099658",
                                                        "Marshall School of Business",
                                                        ifelse(x_affilid == "60019009",
                                                               "Herman Ostrow School of Dentistry",
                                                               ifelse(x_affilid == "60086699",
                                                                      "Andrus Gerontology",
                                                                      ifelse(x_affilid == "60005801",
                                                                             "Women's Hospital",
                                                                             ifelse(x_affilid == "60026672",
                                                                                    "Gould School of Law",
                                                                                    ifelse(x_affilid == "60006209",
                                                                                           "Keck Hospital",
                                                                                           ifelse(x_affilid == "60268548",
                                                                                                  "Keck Medicine",
                                                                                                  ""))))))))))
                     ))
        lastname = api_res$content$`search-results`$entry[[1]]$`preferred-name`$surname
        if ("given-name" %in% names(api_res$content$`search-results`$entry[[1]]$`preferred-name`)) {
          firstname = strsplit(api_res$content$`search-results`$entry[[1]]$`preferred-name`$`given-name`, " ")[[1]][1]
          name = paste(lastname, " ", substr(firstname, 1, 1), ".", sep = "")
          fullname = paste(lastname, api_res$content$`search-results`$entry[[1]]$`preferred-name`$`given-name`)
        } else {
          firstname = NA
          name = lastname
          fullname = lastname
        }
        authors_other <- rbind(authors_other, 
                               data.frame(pubID = not_covered_pubs[j,]$X, 
                                          Link = not_covered_pubs[j,]$Link,
                                          authorID = i, "Name" = name,
                                          "FName" = firstname, "LName" = lastname,
                                          fullName = fullname, Div = dv))
      }
    }
    Sys.sleep(.4) # rate limit exceeded
  }
}

write.csv(authors_other,
          here::here("data_processed/authors_other.csv"),
          row.names = FALSE)
saveRDS(all_api_results, file="all_api_results.RData")
# readRDS("all_api_results.RData)

bridge_table <- authors_working %>% select(pubID, Link, authorID)
bridge_table <- rbind(bridge_table, authors_other %>% select(pubID, Link, authorID))
write.csv(bridge_table,
          here::here("data_processed/bridge_new.csv"),
          row.names = FALSE)  

authors_more <- authors_other[!duplicated(authors_other$authorID),] %>% 
  mutate(Dept = "", affls = "", names = Name, firstnames = FName, lastnames = LName, fullnames = fullName) %>%
  select(authorID, affls, names, firstnames, lastnames, fullnames, Dept, Div)

authors_combined <- rbind(authors_parsed, authors_more)
authors_combined <- authors_combined %>%
  group_by(authorID) %>%
  summarize(
    affls = paste(trimws(affls)[!duplicated(trimws(affls))], collapse = ";"),
    names = paste(names[!duplicated(names)], collapse = ";"),
    firstnames = paste(firstnames[!duplicated(firstnames)], collapse = ";"),
    lastnames = paste(lastnames[!duplicated(lastnames)], collapse = ";"),
    fullnames = paste(fullnames[!duplicated(fullnames)], collapse = ";"),
    Dept = paste(Dept[!duplicated(Dept)], collapse = ";"),
    Div = paste(Div[!duplicated(Div)], collapse = ";"))

write.csv(authors_combined,
          here::here("data_processed/authors_combined.csv"),
          row.names = FALSE)

authors_combined <- read.csv(here::here("data_processed/authors_combined.csv"))

# rename divisions
authors_combined$Div <- gsub("School of Cinema-Television", "School of Cinematic Arts", authors_combined$Div)
authors_combined$Div <- gsub("(Mann )?School of Pharmacy", "School of Pharmacy", authors_combined$Div)
authors_combined$Div <- gsub("(Dworak-?Peck School|Suzanne-?Dworak|School of Social Work)", "Suzanne Dworak Peck School of Social Work", authors_combined$Div)
authors_combined$Div <- gsub("(Herman )?Ostrow School", "Herman Ostrow School of Dentistry", authors_combined$Div)
authors_combined$Div <- gsub("(Viterbi School|School of Engineering)", "Viterbi School of Engineering", authors_combined$Div)
authors_combined$Div <- gsub("(Leonard Davis School|(Davis )?School of Gerontology)", "Leonard Davis School of Gerontology", authors_combined$Div)
authors_combined$Div <- gsub("Annenberg School", "Annenberg School of Communication and Journalism", authors_combined$Div)
authors_combined$Div <- gsub("Gould School", "Gould School of Law", authors_combined$Div)
authors_combined$Div <- gsub("(Sol )?Price School", "Sol Price School of Public Policy", authors_combined$Div)
authors_combined$Div <- gsub("Chan Division", "Chan Division of Occupational Science and Occupational Therapy", authors_combined$Div)
authors_combined$Div <- gsub("Division of Biokinesiology", "Division of Biokinesiology and Physical Therapy", authors_combined$Div)
authors_combined$Div <- gsub("(Dornsife|College of Letters Arts and Sciences)", "Dornsife College of Letters Arts and Sciences", authors_combined$Div)
authors_combined$Div <- gsub("Leventhal School", "Leventhal School of Accounting", authors_combined$Div)
authors_combined$Div <- gsub("Roski School", "Roski School of Art & Design", authors_combined$Div)
authors_combined$Div <- gsub("Rossier School", "Rossier School of Education", authors_combined$Div)
authors_combined$Div <- gsub("Thorton School", "Thornton School of Music", authors_combined$Div)
authors_combined$Div <- gsub("(Norris (Jr\\. )?(Comprehensive )?Cancer Center|Norris Center for Cancer Drug Development)", "Norris Comprehensive Cancer Center", authors_combined$Div)
authors_combined$Div <- gsub("Mann Institute", "Alfred E. Mann Institute of Biomedical Engineering", authors_combined$Div)
authors_combined$Div <- gsub("(USC|University of Southern California) Libraries", "Libraries", authors_combined$Div)

authors_combined$Div <- apply(authors_combined, 1, function(x) {
  s <- strsplit(x['Div'], ";")[[1]]
  paste(s[!duplicated(s)], collapse = ";")
})

# rename departments
authors_combined$Dept <- gsub("Division of Medical Onocology", "Libraries", authors_combined$Dept)


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
