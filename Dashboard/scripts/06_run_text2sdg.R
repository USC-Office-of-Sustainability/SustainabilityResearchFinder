# Use text2sdg package to classify publications
# Use USC PWG curated keywords

library(text2sdg)
library(here)
library(dplyr)
library(reshape2)
library(stringr)
library(stringi)
library(pluralize)

# clean keywords
usc_pwg_keywords <- read.csv(here::here("data_raw/USC_PWG-E_Keywords_1_24_24.csv"))
# # causes errors
usc_pwg_keywords <- usc_pwg_keywords[-grep("#", usc_pwg_keywords$keyword),]
# remove punctuation
usc_pwg_keywords$keyword <- gsub("[^[:alnum:][:space:]]", " ", usc_pwg_keywords$keyword)
# lowercase
usc_pwg_keywords$keyword <- tolower(usc_pwg_keywords$keyword)
# remove duplicates bc otherwise text2sdg will count the word twice
usc_pwg_keywords <- usc_pwg_keywords[!duplicated(usc_pwg_keywords),]
# remove the word race and related
usc_pwg_keywords <- usc_pwg_keywords[-which(usc_pwg_keywords$keyword %in% c("race gender", "gender and sex", "sex and gender", "race and gender", "gender and race", "class and gender", "gender and class", "race")),]
# create system for text2sdg
usc_pwg_system <- usc_pwg_keywords %>%
  mutate(system = "usc_pwg",
         query = paste0('"', keyword, '"')) %>%
  rename(sdg = goal) %>%
  select(system, sdg, query)


usc_data <- read.csv(here::here("data_processed/usc_pubs_law.csv"))

# context dependency
apply_context_dependency <- function(tt) {
  tt <- tolower(tt)
  corrections <- read.csv("data_raw/context_dependencies_01_19_24.csv")
  corrections$before <- tolower(corrections$before)
  corrections$after <- tolower(corrections$after)
  tt <- stri_replace_all_regex(tt,
                               pattern = corrections$before,
                               replacement = corrections$after,
                               vectorize = FALSE)
  tt
}

remove_punctuation <- function(tt) {
  gsub("[^[:alnum:][:space:]']", " ", tt)
}
keep_semicolon <- function(tt) {
  gsub("[^[:alnum:][:space:]';]", " ", tt)
}

remove_copyright <- function(tt) {
  tt <- gsub("([^.]*Copyright.*)", " ", tt)
  tt <- gsub("([^.]*©.*)", " ", tt)
}

usc_data$newtitle <- apply_context_dependency(remove_punctuation(usc_data$Titles))
usc_data$newabstract <- apply_context_dependency(remove_punctuation(remove_copyright(usc_data$Abstract)))
usc_data$newauthorkeywords <- apply_context_dependency(keep_semicolon(usc_data$Author.Keywords))
usc_data$newindexedkeywords <- apply_context_dependency(keep_semicolon(usc_data$Indexed.Keywords))


# scopus uses elsevier search queries on title, abstract, keywords
usc_data$alltext <- paste(usc_data$newtitle,
                          usc_data$newabstract,
                          usc_data$newauthorkeywords,
                          usc_data$newindexedkeywords, sep = ";")

# lowercase 
# usc_data$alltext <- tolower(usc_data$alltext)

# remove '
usc_data$alltext <- gsub("'", " ", usc_data$alltext)
  
# run text2sdg
hits <- detect_any(usc_data$alltext, usc_pwg_system, output = "features")
hits$features <- gsub(",", "", hits$features)
usc_data$document <- 1:nrow(usc_data)
hits_link_count <- merge(hits, usc_data[c("document", "pubID", "Link")], by = "document")

# hits_link_count$count <- str_count(hits_link_count$alltext, hits_link_count$features)

write.csv(hits_link_count, 
          here::here("data_processed/usc_text2sdg_features.csv"), 
          row.names = FALSE)


# animal and animals is 1 keyword
# singularize is not 100% accurate
# takes 2 min
hits_sum <- hits %>%
  group_by(document) %>%
  # distinct(sdg) %>%
  count(sdg) %>%
  filter(n >= 2) %>%
  left_join(hits) %>%
  group_by(document, sdg) %>%
  summarize(nkeywords = length(unique(singularize(features)))) %>%
  filter(nkeywords >= 2) %>%
  dcast(document ~ sdg, fill = 0)


# each keyword appearance as separate keyword
# hits_sum <- dcast(hits_link_count %>%
#                     group_by(document, sdg) %>% 
#                     summarize(n = sum(count)) %>% 
#                     filter(n >= 2),
#                     document ~ sdg, 
#                     fill = 0)

# original min 2 keywords
# hits_sum <- hits %>%
#   group_by(document) %>%
#   # distinct(sdg) %>%
#   count(sdg) %>%
#   filter(n >= 2) %>% 
#   dcast(document ~ sdg, fill = 0)

# keywords + text for specific sdg
# hits %>%
#   filter(sdg == "SDG-07") %>%
#   group_by(document) %>%
#   summarize(keywords = paste(unique(singularize(features)), collapse = ","),
#             nkeywords = length(unique(singularize(features)))) %>%
#   filter(nkeywords >= 2) -> sdg_7_rows
# sdg_7_rows$document <- as.numeric(sdg_7_rows$document)
# sdg_7_rows %>%
#   left_join(usc_data[c("pubID", "alltext", "Link")], by = c("document" = "pubID")) -> sdg_7_output
# 
# write.csv(sdg_7_output,
#           "data_processed/sdg_7_keywords_text.csv",
#           row.names = FALSE)


hits_sum_link <- merge(hits_sum, usc_data[c("document", "pubID", "Link")], by = "document")

write.csv(hits_sum_link, 
          here::here("data_processed/usc_sdgs.csv"), 
          row.names = FALSE)
# 
# # manual fixes?
# idx = which(usc_pubs$Titles %in% 
#               "Mechanical properties of laminated bamboo designed for curvature")
# hits_sum_link[which(hits_sum_link$document == idx),]$SDG.11 = 1
# hits_sum_link[which(hits_sum_link$document == idx),]$SDG.12 = 1
# 
# write.csv(hits_sum_link,
#           here::here("data_processed/usc_sdgs_edited.csv"),
#           row.names = FALSE)

# new column for 3 sustainability categories
# env 6 7 12-15
# socio economic 1-5 8-11 16-17
usc_sdgs <- read.csv("data_processed/usc_sdgs.csv")
usc_sdgs$num_sdgs <- rowSums(select(usc_sdgs,starts_with("SDG")) != 0)
sustainabilityresearch <- usc_sdgs %>%
  mutate(
    sustainability_category = 
      case_when(
        (SDG.06 > 0 | SDG.07 > 0 | SDG.12 > 0 | SDG.13 > 0 | SDG.14 > 0 | SDG.15 > 0) & 
          (SDG.01 > 0 | SDG.02 > 0 | SDG.03 > 0 | SDG.04 > 0 |
             SDG.05 > 0 | SDG.08 > 0 |
             SDG.09 > 0 | SDG.10 > 0 | SDG.11 > 0 |
             SDG.16 > 0 | SDG.17 > 0) ~ "Sustainability-Focused",  
        (num_sdgs >= 2) ~ "Sustainability-Inclusive",
        (num_sdgs == 1) ~ "SDG-Related"))
# everything in here is sustainability inclusive so next line does nothing
sustainabilityresearch$sustainability_category[is.na(sustainabilityresearch$sustainability_category)] = "Not-Related"
write.csv(sustainabilityresearch,
          here::here("data_processed/usc_sdgs_with_categories.csv"),
          row.names = FALSE)

