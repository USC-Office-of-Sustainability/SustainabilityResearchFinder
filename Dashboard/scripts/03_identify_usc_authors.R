# Identify USC authors in the list of publications by
# 1. parsing authors with affiliations text
# 2. searching first 10 author's current affiliation on scopus via API

library(here)
library(rscopus)

usc_regex <- paste0("(uni[versity\\.]{0,} (of )?sou?th[ernm]{0,}[,-]? ?",
                   "california)|(USC)|(keck school of medicine)|(keck medical",
                   " center)|(keck medical school)")
usc_affiliation_ids <- c(60029311, 60015183, 60022143, 60009207, 60015400,
                         60013994, 60099658, 60019009, 60086699, 60005801,
                         60026672, 60006209, 60268548)

elsevier_api_key <- Sys.getenv("Elsevier_API")

publication_author <- data.frame()
author <- data.frame(matrix(ncol = 2, nrow = 0, dimnames = list(NULL, c("ID", "Name"))))

usc_data <- read.csv(here::here("data_processed/usc_pubs.csv"))
usc_data$ScopusAPI <- FALSE

tmp <- usc_data[sample(1:nrow(usc_data), 10), ]

for (i in 1:nrow(usc_data)) {
  # split
}

# first round - split by ; then only look for USC if Author full names and Authors with affiliations can be split the same
authors_working = apply(usc_data, 1, function(x) {
  res = data.frame()
  auth_full_name = trimws(strsplit(x['Author.full.names'], ";")[[1]])
  auth_affl_vec = strsplit(x['Authors.with.affiliations'], ";")[[1]]
  if (length(auth_full_name) == length(auth_affl_vec)) {
    matched = grep(usc_regex, auth_affl_vec, ignore.case = TRUE)
    for (i in matched) {
      name = trimws(strsplit(auth_affl_vec[i], ",")[[1]][1])
      lastname = strsplit(name, " ")[[1]][1]
      firstname = strsplit(trimws(strsplit(auth_full_name[i], ",")[[1]][2]), " ")[[1]][1]
      authorID = trimws(strsplit(x['Author.s..ID'], ";")[[1]][i])
      res <- rbind(res, data.frame(pubID = x['X'], authorID = authorID, "Name" = name, 
                                   "FName" = firstname, "LName" = lastname))
    }
  }
  res
})

authors_working <- do.call(rbind, authors_working)

missing_pubs <- setdiff(1:nrow(usc_data),as.numeric(authors_working$pubID))

current_pubs <- usc_data[missing_pubs,]

tmp <- head(current_pubs)

y = apply(current_pubs, 1, function(x) {
  length(strsplit(x['Author.s..ID'], ";")[[1]])
})

for (j in 1:nrow(tmp)) {
  auth_full_name = trimws(strsplit(tmp$Author.full.names[j], ";")[[1]])
  n = length(auth_full_name)
  auth_affl_vec = strsplit(tmp$Authors.with.affiliations[j], ";")[[1]]
  if (length(auth_affl_vec) != n) { # some extra ;
    auth_affl_vec = strsplit(tmp$Authors.with.affiliations[j], 
                             ";\\s(?=([^\\(\\); ]* ){1,3}[^a-z]\\.)", 
                             perl = TRUE)[[1]]
  }
  matched = grep(usc_regex, auth_affl_vec, ignore.case = TRUE)
  
  if (all(grepl(tmp$Affiliations[j], auth_affl_vec, fixed = TRUE)) &
      grepl(" with ", tmp$Affiliations[j])) {
    print(j)
    print("all affiliations were the same")
    auth_affl_vec = strsplit(tmp$Affiliations[j], 
                             "(?<=[^A-Z]\\.)\\s(?=[A-Z])", perl=T)[[1]]
    
    auth_affl_vec = sapply(auth_affl_vec, function(x) {
      m = regexpr("(with|was)", x)
      substring(x, m)
    })
    
    if (length(auth_affl_vec) == length(auth_full_name)) {
      matched = grep(usc_regex, auth_affl_vec, ignore.case = TRUE)
      
      for (i in matched) {
        name = trimws(strsplit(tmp$Authors[j], ";")[[1]][i])
        lastname = strsplit(name, " ")[[1]][1]
        fullnameid = trimws(strsplit(tmp$Author.full.names[j], ";")[[1]][i])
        firstname = strsplit(fullnameid, " ")[[1]][2]
        authorID = trimws(strsplit(tmp$Author.s..ID[j], ";")[[1]][i])
        publication_author = rbind(publication_author, 
                                   data.frame(pubID = tmp$X[j], AuthorId = authorID))
        if (!(authorID %in% author$ID)) {
          author = rbind(author, data.frame("ID" = authorID, "Name" = name, 
                                            "FName" = firstname, "LName" = lastname))
        }
      }
    } else {
      # search up all current affiliation on scopus
      tmp[j,]$ScopusAPI = TRUE
      
      api_key = Sys.getenv('Elsevier_API')
      ten_authors = trimws(strsplit(tmp$Author.s..ID[j], ";")[[1]])
      if (length(ten_authors) > 10) {
        ten_authors =  c(trimws(strsplit(tmp$Author.s..ID[j], ";")[[1]])[1:9], 
                         trimws(strsplit(tmp$Author.s..ID[j], ";")[[1]])[n])
      }
      for (i in ten_authors) {
        x = get_complete_author_info(api_key = api_key, au_id = i, count = 1)
        if (x$content$`search-results`$`opensearch:totalResults` == "1") {
          x_affilid <- 
            x$content$`search-results`$entry[[1]]$`affiliation-current`$`affiliation-id`
          if (x_affilid %in% usc_affiliation_ids) {
            lastname = x$content$`search-results`$entry[[1]]$`preferred-name`$surname
            firstname = strsplit(
              x$content$`search-results`$entry[[1]]$`preferred-name`$`given-name`, 
              " "
              )[[1]][1]
            name = paste(lastname, " ", substr(firstname, 1, 1), ".", sep = "")
            publication_author = rbind(publication_author, 
                                       data.frame(pubID = tmp$X[j], AuthorId = i))
            if (!(authorID %in% author$ID)) {
              author = rbind(author, data.frame("ID" = i, "Name" = name, 
                                                "FName" = firstname, "LName" = lastname))
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
    api_key = Sys.getenv('Elsevier_API')
    ten_authors = trimws(strsplit(tmp$Author.s..ID[j], ";")[[1]])
    if (length(ten_authors) > 10) {
      ten_authors =  c(trimws(strsplit(tmp$Author.s..ID[j], ";")[[1]])[1:9], 
                       trimws(strsplit(tmp$Author.s..ID[j], ";")[[1]])[n])
    }
    for (i in ten_authors) {
      x = get_complete_author_info(api_key = api_key, au_id = i, count = 1)
      #print(x)
      if (x$content$`search-results`$`opensearch:totalResults` == "1") {
        x_affilid = 
          x$content$`search-results`$entry[[1]]$`affiliation-current`$`affiliation-id`
        if (x_affilid %in% usc_affiliation_ids) {
          lastname = x$content$`search-results`$entry[[1]]$`preferred-name`$surname
          firstname = strsplit(
            x$content$`search-results`$entry[[1]]$`preferred-name`$`given-name`, 
            " "
            )[[1]][1]
          name = paste(lastname, " ", substr(firstname, 1, 1), ".", sep = "")
          publication_author = rbind(publication_author, 
                                     data.frame(pubID = tmp$X[j], AuthorId = i))
          if (!(authorID %in% author$ID)) {
            author = rbind(author, data.frame("ID" = i, "Name" = name, 
                                              "FName" = firstname, "LName" = lastname))
          }
        }
      }
      Sys.sleep(10) # rate limit exceeded
    }
  } else {
    # for each match split by , 
    # name is first entry
    for (i in matched) {
      name = trimws(strsplit(auth_affl_vec[i], ",")[[1]][1])
      lastname = strsplit(name, " ")[[1]][1]
      firstname = strsplit(trimws(strsplit(auth_full_name[i], ",")[[1]][2]), " ")[[1]][1]
      authorID = trimws(strsplit(tmp$Author.s..ID[j], ";")[[1]][i])
      publication_author = rbind(publication_author, 
                                 data.frame(pubID = tmp$X[j], AuthorId = authorID))
      if (!(authorID %in% author$ID)) {
        author = rbind(author, data.frame("ID" = authorID, "Name" = name, 
                                          "FName" = firstname, "LName" = lastname))
      }
    }
  }
}
