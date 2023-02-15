# Use text2sdg package to classify publications
# Use USC PWG curated keywords

library(text2sdg)
library(here)
library(dplyr)
library(reshape2)

usc_pwg_keywords <- read.csv(here::here("data_raw/USC_PWG-E_2023Keywords_2_14_23.csv"))
usc_pwg_keywords <- usc_pwg_keywords %>%
  mutate(system = "usc_pwg") %>%
  mutate(sdg = goal) %>%
  mutate(query = pattern) %>%
  select(system, sdg, query)
# # causes errors
usc_pwg_keywords <- usc_pwg_keywords[-grep("#", usc_pwg_keywords$query),]

usc_pwg_keywords$query <- gsub("\\\\b\\(\\\\d\\*\\)", '\\\"', usc_pwg_keywords$query)
usc_pwg_keywords$query <- gsub("\\(\\\\d\\*\\)\\\\b", '\\\"', usc_pwg_keywords$query)

usc_data <- read.csv(here::here("data_processed/usc_pubs.csv"))

# scopus uses elsevier search queries on title, abstract, keywords
usc_data$alltext <- paste(usc_data$Titles,
                          usc_data$Abstract,
                          usc_data$Author.Keywords,
                          usc_data$Indexed.Keywords)

# lowercase all
usc_pwg_keywords$query <- tolower(usc_pwg_keywords$query)
usc_data$alltext <- tolower(usc_data$alltext)


hits <- detect_any(usc_data$alltext, usc_pwg_keywords, output = "features")

hits_link <- merge(hits, usc_data[c("X", "Link")], by.x = "document", by.y = "X")

write.csv(hits_link, 
          here::here("data_processed/usc_text2sdg_features.csv"), 
          row.names = FALSE)

hits_sum <- dcast(hits %>% 
                    group_by(document) %>% 
                    # distinct(sdg) %>% 
                    count(sdg) %>%
                    filter(n >= 3), 
                  document ~ sdg, 
                  fill = 0)


hits_sum_link <- merge(hits_sum, usc_data[c("X", "Link")], by.x = "document", by.y = "X")

write.csv(hits_sum_link, 
          here::here("data_processed/usc_sdgs.csv"), 
          row.names = FALSE)

# manual fixes?
idx = which(usc_pubs$Titles %in% 
              "Mechanical properties of laminated bamboo designed for curvature")
hits_sum_link[which(hits_sum_link$document == idx),]$SDG.11 = 1
hits_sum_link[which(hits_sum_link$document == idx),]$SDG.12 = 1

write.csv(hits_sum_link,
          here::here("data_processed/usc_sdgs_edited.csv"),
          row.names = FALSE)
