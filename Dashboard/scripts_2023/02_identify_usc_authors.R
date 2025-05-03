# identify new usc authors from affiliations
library(dplyr)
pubs <- read.csv("data_processed/usc_pubs_2020_24.csv")
# first pubID 25916
focused_pubs <- pubs %>%
  filter(pubID > 25915) # number of pubs in data_processed/usc_pubs_all.csv

# identify USC authors in the new pubs

# all usc authors 2020-22
# all_usc_authors <- read.csv("data_processed/all_usc_authors.csv")
# existing_authorID <- unique(all_usc_authors$auth_ids)

# based off of 02_identify_usc_authors.R
usc_regex <- paste0("([uU]ni[versity\\.]{0,} ([oO]f )?[sS]ou?th[ernm]{0,}[,-]? ?",
                    "[Cc]alifornia)|([kK]eck [Ss]chool [Oo]f [Mm]edicine)|",
                    "([Kk]eck [Mm]edical [Cc]enter)|([Kk]eck [Mm]edical [Ss]chool)")
usc_affiliation_ids <- c(60029311, 60015183, 60022143, 60009207, 60015400,
                         60013994, 60099658, 60019009, 60086699, 60005801,
                         60026672, 60006209, 60268548)

# check authors, authors full name, authors ID columns match
same_num_auth <- apply(focused_pubs, 1, function(x) {
  n_authors <- length(strsplit(x['Authors'], ";")[[1]])
  n_full_name <- length(strsplit(x['Author.full.names'], ";")[[1]])
  n_id <- length(strsplit(x['Author.s..ID'], ";")[[1]])
  all(sapply(list(n_authors, n_full_name, n_id), function(y) y == n_authors))
})
sum(same_num_auth) == nrow(focused_pubs) # nothing wrong

# n_authors <- sapply(focused_pubs$Authors, function(x) length(strsplit(x, ";")[[1]]))
# n_fullname <- sapply(focused_pubs$Author.full.names, function(x) length(strsplit(x, ";")[[1]]))
# n_id <- sapply(focused_pubs$Author.s..ID, function(x) length(strsplit(x, ";")[[1]]))
# unname(which(n_authors != n_id)) # no problem
# unname(which(n_fullname != n_id)) # problem w fullname -> missing fullname!


# authors with affiliations split by ; probably matches author names
grep("&amp;", focused_pubs$Authors.with.affiliations)
focused_pubs$Authors.with.affiliations <- gsub("&amp;", "&", focused_pubs$Authors.with.affiliations)
grep(";,", focused_pubs$Authors.with.affiliations)
focused_pubs$Authors.with.affiliations <- gsub(";,", ",", focused_pubs$Authors.with.affiliations)

same_len_name_aff <- apply(focused_pubs, 1, function(x) {
  length(strsplit(x['Author.full.names'], ";")[[1]]) == length(strsplit(x['Authors.with.affiliations'], ";")[[1]])
})
sum(same_len_name_aff)

# where same affiliation is used for all authors
# exclude single author and affiliations that are short
same_aff_for_all <- apply(focused_pubs, 1, function(x) {
  auth_affl_vec <- strsplit(x['Authors.with.affiliations'], ";")[[1]]
  if (length(strsplit(x['Author.s..ID'], ";")[[1]]) == 1) { # single author
    FALSE
  } else if (length(strsplit(x['Affiliations'], ";")[[1]]) == 1) { # one affiliation
    FALSE
  # } else if (length(strsplit(x['Affiliations'], ";")[[1]]) == 1 & length(strsplit(x['Affiliations'], ",")[[1]]) <= 12) {
  #   FALSE
  } else {
    # some ; get replaced with , in Authors with affiliations
    all(grepl(gsub("[[:punct:]]", " ", x['Affiliations']), gsub("[[:punct:]]", " ", auth_affl_vec), fixed = TRUE))
    # all(grepl(x['Affiliations'], auth_affl_vec, fixed = TRUE))
  }
})
sum(same_aff_for_all) # author with affiliations column is empty, they have 300+ authors
ignore_pubIDs <- focused_pubs$pubID[same_aff_for_all]

not_covered <- which(same_len_name_aff == FALSE)
not_covered_pubs <- focused_pubs[not_covered,]
# number of authors per publication
num_authors_per <- apply(focused_pubs, 1, function(x) {
  length(strsplit(x['Author.s..ID'], ";")[[1]])
})

# send questionable num_authors_per > 500
focused_pubs[which(num_authors_per>500),] -> over_500_authors

# number of authors with affiliations per publication
num_authors_per <- apply(not_covered_pubs, 1, function(x) {
  length(strsplit(x['Authors.with.affiliations'], ";")[[1]])
})

# some issues if a column is empty
sum(is.na(focused_pubs$Authors.with.affiliations))
sum(focused_pubs$Authors.with.affiliations == "") # issue
# focused_pubs <- focused_pubs %>%
#   mutate(Authors.with.affiliations = ifelse(Authors.with.affiliations == "", paste0(rep(Affiliations, length(strsplit(Author.s..ID, ";")[[1]])), collapse = ";"), Authors.with.affiliations))
sum(is.na(focused_pubs$Author.full.names))
sum(focused_pubs$Author.full.names == "")
sum(is.na(focused_pubs$Authors))
sum(focused_pubs$Authors == "")
sum(is.na(focused_pubs$Author.s..ID))
sum(focused_pubs$Author.s..ID == "")

# current_pubs <- focused_pubs[-not_covered,]
# parse, split by ;
# how to deal with the auth_affl_vec == each entry
all_focused_authors <- apply(focused_pubs, 1, function(x) {
  auth_name <- trimws(strsplit(x['Authors'], ";")[[1]])
  auth_full_names <- trimws(strsplit(x['Author.full.names'], ";")[[1]])
  auth_affl_vec <- trimws(strsplit(x['Authors.with.affiliations'], ";")[[1]])
  auth_ids <- trimws(strsplit(x['Author.s..ID'], ";")[[1]])
  if (x['pubID'] %in% ignore_pubIDs) {
    res <- data.frame(auth_name, auth_full_names, "auth_affl_vec" = auth_name, auth_ids)
    res$pubID <- x['pubID']
    res$Link <- x['Link']
    return(res)
  } else if (length(auth_name) == length(auth_affl_vec)) {
    res <- data.frame(auth_name, auth_full_names, auth_affl_vec, auth_ids)
    res$pubID <- x['pubID']
    res$Link <- x['Link']
    return(res)
  } else {
    # need to match author name with correct author affiliation
    res <- data.frame(auth_name = character(0), auth_full_names = character(0), auth_affl_vec = character(0), auth_ids = character(0), pubID = numeric(0), Link = character(0))
    for (i in 1:length(auth_name)) { # takes a while since some have over 100 authors
      fullname = auth_full_names[i]
      lastname = strsplit(fullname, ",")[[1]][1]
      idx <- grep(paste0("^", lastname), auth_affl_vec)[1]
      if (length(idx) == 0) {
        new_entry <- data.frame(auth_name[i], fullname, "", auth_ids[i], x['pubID'], x['Link'])
      } else {
        new_entry <- data.frame(auth_name[i], fullname, auth_affl_vec[idx], auth_ids[i], x['pubID'], x['Link'])
        auth_affl_vec <- auth_affl_vec[-idx]
      }
      
      res[nrow(res)+1,] <- new_entry
    }
    return(res)
  }
  
})
all_focused_authors_df <- do.call(rbind, all_focused_authors)

# create a column to determine whether author is USC affiliated or not
# based on affiliations
all_focused_authors_df$USC <- grepl(usc_regex, all_focused_authors_df$auth_affl_vec)
not_covered_pubs$pubID %in% all_focused_authors_df$pubID # all not covered pubs are included
# based on past authorIDs - gets 2000 more
# don't do this since some authors are no longer affiliated w usc
# all_focused_authors_df$USC <- ifelse(all_focused_authors_df$USC | all_focused_authors_df$auth_ids %in% existing_authorID, TRUE, FALSE)

focused_usc_authors <- all_focused_authors_df %>%
  group_by(auth_ids) %>%
  mutate(USCfinal = any(USC)) %>% # get all the rows with usc
  filter(USCfinal == TRUE) %>% # filter(USC) will lose some pubs
  ungroup()

# create columns for authorID, fullname, firstname, lastname
# all have ( and )
all(grepl("\\(",focused_usc_authors$auth_full_names))
all(grepl("\\)",focused_usc_authors$auth_full_names))
focused_usc_authors$authorID <- regmatches(focused_usc_authors$auth_full_names, regexpr("[0-9]+", focused_usc_authors$auth_full_names))
all.equal(focused_usc_authors$auth_ids, focused_usc_authors$authorID) # author IDs are equal
focused_usc_authors$full_name <- sapply(focused_usc_authors$auth_full_names, function(x) {
  trimws(strsplit(x, "\\(")[[1]][1])
})
# no full names without ,
which(grepl(",", focused_usc_authors$auth_full_names) == FALSE)
# no full names with more than 1 ,
which(stringr::str_count(focused_usc_authors$auth_full_names, ",")>1)
focused_usc_authors$last_name <- sapply(focused_usc_authors$full_name, function(x) {
  if (grepl(",", x)) {
    strsplit(x, ",")[[1]][1]
  } else { # should not be reached
    s <- strsplit(x, " ")[[1]]
    if (length(s) == 1) {
      ""
    } else {
      s[1]
    }
  }
})
focused_usc_authors$first_name <- sapply(focused_usc_authors$full_name, function(x) {
  if (grepl(",", x)) {
    trimws(strsplit(x, ",")[[1]][2])
  } else { # should not be reached
    s <- strsplit(x, " ")[[1]]
    if (length(s) == 1) {
      s[1]
    } else {
      s[2]
    }
  }
})

# these focused ones are mainly 2023
table(focused_pubs$Year)

write.csv(focused_usc_authors,
          here::here("data_processed/all_usc_authors_2024.csv"),
          row.names = FALSE)

bridge_table <- focused_usc_authors %>%
  select(pubID, Link, authorID)
write.csv(bridge_table,
          here::here("data_processed/bridge_table_2024.csv"),
          row.names = FALSE)
