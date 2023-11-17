# for finding AE related pubs
library(stringr)
library(dplyr)
usc_pubs <- read.csv("data_processed/usc_pubs_law.csv")
usc_sdgs <- read.csv("data_processed/usc_sdgs_with_categories.csv")
usc_pubs <- usc_pubs %>% filter(Year %in% c(2020, 2021, 2022)) %>% filter(!Document.Type %in% c("Letter", "Retracted", "Note", "Erratum"))

usc_pubs_sdgs <- merge(usc_pubs, usc_sdgs, 
                       by = c("pubID", "Link"), 
                       all.x = TRUE)
usc_pubs_sdgs$sustainability_category[is.na(usc_pubs_sdgs$sustainability_category)] = "Not-Related"
sustainabilityrelated <- usc_pubs_sdgs %>% 
  filter(sustainability_category %in% c("Sustainability-Focused", "Sustainability-Inclusive"))
sustainabilityrelated$alltext <- paste(sustainabilityrelated$Titles,
                                       sustainabilityrelated$Abstract,
                                       sustainabilityrelated$Author.Keywords,
                                       sustainabilityrelated$Indexed.Keywords)
sustainabilityrelated$alltext <- gsub("-", " ", tolower(sustainabilityrelated$alltext))

ae_keywords <- read.csv(here::here("data_raw/AE_Research_2.3_keywords.csv"))
ae_pattern <- paste(gsub("-", " ", tolower(trimws(ae_keywords$AE_2.3_Keywords))), collapse = "|")
ae_pubs <- sustainabilityrelated[grep(ae_pattern, sustainabilityrelated$alltext),]
ae_pubs$AE_2.3_keywords <- ""
for (i in 1:nrow(ae_pubs)){
  ae_pubs$AE_2.3_keywords[i] <- paste(unique(str_match_all(ae_pubs$alltext[i],ae_pattern)[[1]]), collapse = ", ")
}

# ae_final <- _pubs %>%
#   select(pubID, Titles, Year, Source.title, DOI, Cited.by, Link, Abstract, Indexed.Keywords, Author.Keywords, Publisher, PubMed.ID, Open.Access, Source, EID, SDG.01,SDG.02,SDG.03,SDG.04,SDG.05,SDG.06,SDG.07,SDG.08,SDG.09,SDG.10,SDG.11,SDG.12,SDG.13,SDG.14,SDG.15,SDG.16,SDG.17,sustainability_category,DEI_3.3_keywords)
write.csv(ae_pubs,
          here::here("data_processed/AE_pubs.csv"),
          row.names = FALSE)

