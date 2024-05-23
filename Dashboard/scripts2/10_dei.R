# for finding DEI related pubs
library(stringr)
library(dplyr)
usc_pubs_sdgs <- read.csv("data_processed/usc_pubs_with_sdgs_2020_23_manual_fix.csv")
usc_pubs_sdgs <- usc_pubs_sdgs %>% 
  filter(Year %in% c(2020, 2021, 2022, 2023)) %>% 
  filter(!Document.Type %in% c("Letter", "Retracted", "Note", "Erratum"))

sustainabilityrelated <- usc_pubs_sdgs %>% 
  filter(sustainability_category %in% c("Sustainability-Focused", "Sustainability-Inclusive"))
sustainabilityrelated$alltext <- paste(sustainabilityrelated$Titles,
                                       sustainabilityrelated$Abstract,
                                       sustainabilityrelated$Author.Keywords,
                                       sustainabilityrelated$Indexed.Keywords)
sustainabilityrelated$alltext <- tolower(sustainabilityrelated$alltext)

dei_keywords <- read.csv(here::here("data_raw/AsgmtEarth_DEI_3.3_Catalog_Keywords - Keywords for crossmapping.csv"))
# dei_pattern <- "disproportionate impact|environmental disruptions?|environmental justice|local communities|local community|neighborhoods?|local people|Los Angeles|risk exposures?|sustainability|non-white"
dei_pattern <- paste(tolower(trimws(dei_keywords$Keywords)), collapse = "|")
dei_pubs <- sustainabilityrelated[grep(dei_pattern, sustainabilityrelated$alltext),]
dei_pubs$DEI_3.3_keywords <- ""
for (i in 1:nrow(dei_pubs)){
  dei_pubs$DEI_3.3_keywords[i] <- paste(unique(str_match_all(dei_pubs$alltext[i],dei_pattern)[[1]]), collapse = ", ")
}

dei_final <- dei_pubs %>%
  select(pubID, Titles, Year, Source.title, DOI, Cited.by, Link, Abstract, Indexed.Keywords, Author.Keywords, Publisher, Open.Access, Source, EID, SDG.01,SDG.02,SDG.03,SDG.04,SDG.05,SDG.06,SDG.07,SDG.08,SDG.09,SDG.10,SDG.11,SDG.12,SDG.13,SDG.14,SDG.15,SDG.16,SDG.17,all_SDGs,sustainability_category,DEI_3.3_keywords)
write.csv(dei_final,
          here::here("data_processed/DEI_pubs_2020_23.csv"),
          row.names = FALSE)


# sorted by specific keywords then sustainability category
dei_data <- read.csv("data_processed/DEI_pubs_2020_23.csv")
usc_bridge <- read.csv("shiny_app/usc_bridge_2020_23_combined_edit.csv")
usc_authors <- read.csv("shiny_app/usc_authors_2020_23_combined_edit.csv")

tmp <- merge(dei_data, usc_bridge,
             by.x = c("pubID", "Link"), by.y = c("pubID", "Link"))
dei_joined <- merge(tmp, usc_authors,
                    by.x = "authorID", by.y = "authorID")

# dei_joined$sustainability_category <- factor(dei_joined$sustainability_category, levels = c("Sustainability-Focused", "Sustainability-Inclusive"))
specific_keywords <- c("los angeles", "central la", "east la", "south la", "boyle heights")
specific_pattern <- paste(specific_keywords, collapse = "|")
dei_joined <- dei_joined %>% mutate(important_keywords = grepl(specific_pattern, DEI_3.3_keywords))
# dei_joined$important_keywords <- factor(dei_joined$important_keywords, levels = c(TRUE, FALSE))
# dei_joined[order(dei_joined$important_keywords, dei_joined$sustainability_category),]
dei_joined <- dei_joined[order(match(dei_joined$important_keywords, c(TRUE,FALSE)), 
                               match(dei_joined$sustainability_category, 
                                     c("Sustainability-Focused", "Sustainability-Inclusive"))),]
write.csv(dei_joined,
          here::here("shiny_app/DEI_pubs_ordered_2020_23.csv"),
          row.names = FALSE)

