# Use text2sdg package to classify publications
# Use the union of Aurora and Elsevier system outputs

library(text2sdg)
library(here)
library(dplyr)
library(reshape2)

usc_data <- read.csv(here::here("data_processed/usc_pubs.csv"))

# scopus uses elsevier search queries on title, abstract, keywords
usc_data$alltext <- paste(usc_data$Titles,
                          usc_data$Abstract,
                          usc_data$Author.Keywords,
                          usc_data$Indexed.Keywords)

hits <- detect_sdg(usc_data$alltext, 
                   systems = c("Aurora", "Elsevier"),
                   output = "features")

hits_link <- merge(hits, usc_data[c("X", "Link")], by.x = "document", by.y = "X")

write.csv(hits_link, 
          here::here("data_processed/usc_text2sdg_output.csv"), 
          row.names = FALSE)

hits_sum <- dcast(hits %>% 
                    group_by(document) %>% 
                    distinct(sdg) %>% 
                    count(sdg), 
                  document ~ sdg, 
                  fill = 0)


hits_sum_link <- merge(hits_sum, usc_data[c("X", "Link")], by.x = "document", by.y = "X")

write.csv(hits_sum_link, 
          here::here("data_processed/usc_sdgs.csv"), 
          row.names = FALSE)
