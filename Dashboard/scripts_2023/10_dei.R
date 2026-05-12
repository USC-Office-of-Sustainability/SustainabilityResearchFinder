# Identify DEI-related publications among sustainability-focused/inclusive pubs
library(stringr)
library(dplyr)

usc_pubs_sdgs <- read.csv("data_processed/09_pubs_sdg_manual_fixed.csv")
usc_pubs_sdgs <- usc_pubs_sdgs %>%
  filter(!Document.Type %in% c("Letter", "Retracted", "Note", "Erratum"))

# --- Filter to sustainability-related pubs and run DEI keyword match ---------
sustainabilityrelated <- usc_pubs_sdgs %>%
  filter(sustainability_category %in% c("Sustainability-Focused", "Sustainability-Inclusive"))
sustainabilityrelated$alltext <- paste(sustainabilityrelated$Titles,
                                       sustainabilityrelated$Abstract,
                                       sustainabilityrelated$Author.Keywords,
                                       sustainabilityrelated$Indexed.Keywords)
sustainabilityrelated$alltext <- tolower(sustainabilityrelated$alltext)

dei_keywords <- read.csv(here::here("data_raw/AsgmtEarth_DEI_3.3_Catalog_Keywords.csv"))
dei_pattern <- paste(tolower(trimws(dei_keywords$Keywords)), collapse = "|")
dei_pubs <- sustainabilityrelated[grep(dei_pattern, sustainabilityrelated$alltext), ]
dei_pubs$DEI_3.3_keywords <- ""
for (i in 1:nrow(dei_pubs)) {
  dei_pubs$DEI_3.3_keywords[i] <- paste(unique(str_match_all(dei_pubs$alltext[i], dei_pattern)[[1]]), collapse = ", ")
}

dei_final <- dei_pubs %>%
  select(pubID, Titles, Year, Source.title, DOI, Cited.by, Link, Abstract, Indexed.Keywords, Author.Keywords, Publisher, Open.Access, Source, EID, SDG.01, SDG.02, SDG.03, SDG.04, SDG.05, SDG.06, SDG.07, SDG.08, SDG.09, SDG.10, SDG.11, SDG.12, SDG.13, SDG.14, SDG.15, SDG.16, SDG.17, all_SDGs, sustainability_category, DEI_3.3_keywords)
write.csv(dei_final,
          here::here("data_processed/10_dei_pubs.csv"),
          row.names = FALSE)


# --- Join with author/bridge tables and sort by LA keywords + sust category --
dei_data <- read.csv("data_processed/10_dei_pubs.csv")
usc_bridge <- read.csv("shiny_app/07_bridge_manual_edited.csv")
usc_authors <- read.csv("data_processed/07_authors_manual_edited.csv")

tmp <- merge(dei_data, usc_bridge,
             by.x = c("pubID", "Link"), by.y = c("pubID", "Link"))
dei_joined <- merge(tmp, usc_authors,
                    by.x = "authorID", by.y = "authorID")

specific_keywords <- c("los angeles", "central la", "east la", "south la", "boyle heights")
specific_pattern <- paste(specific_keywords, collapse = "|")
dei_joined <- dei_joined %>% mutate(important_keywords = grepl(specific_pattern, DEI_3.3_keywords))
# Sort: LA-specific keywords first, then Sustainability-Focused before Sustainability-Inclusive
dei_joined <- dei_joined[order(match(dei_joined$important_keywords, c(TRUE, FALSE)),
                               match(dei_joined$sustainability_category,
                                     c("Sustainability-Focused", "Sustainability-Inclusive"))), ]
write.csv(dei_joined,
          here::here("shiny_app/10_dei_pubs_ordered.csv"),
          row.names = FALSE)
