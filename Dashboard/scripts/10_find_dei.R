# for finding DEI related pubs
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
  select(pubID, Titles, Year, Source.title, DOI, Cited.by, Link, Abstract, Indexed.Keywords, Author.Keywords, Publisher, PubMed.ID, Open.Access, Source, EID, SDG.01,SDG.02,SDG.03,SDG.04,SDG.05,SDG.06,SDG.07,SDG.08,SDG.09,SDG.10,SDG.11,SDG.12,SDG.13,SDG.14,SDG.15,SDG.16,SDG.17,sustainability_category,DEI_3.3_keywords)
write.csv(dei_final,
          here::here("data_processed/DEI_pubs.csv"),
          row.names = FALSE)


# sorted by specific keywords then sustainability category
dei_data <- read.csv("data_processed/DEI_pubs.csv")
usc_bridge <- read.csv("data_processed/bridge_law_fixed2.csv")
usc_authors <- read.csv("data_processed/usc_authors_law_fixed_dept.csv")

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
sdgs_only <- dei_joined %>%
  select(starts_with("SDG")) %>%
  replace(is.na(.), 0)
w <- which(sdgs_only != 0, arr.ind = TRUE)
sdgs_only[w] <- as.numeric(substr(names(sdgs_only)[w[, "col"]], start = 5, stop = 6))
sdgs_collapsed <- apply(sdgs_only, 1, function(x) {
  res = ""
  for (i in 1:length(x)) {
    if (x[i] != 0) {
      if (res == "") {
        res = x[i]
      } else {
        res = paste(res, x[i], sep = ", ")
      }
    }
  }
  res
})
dei_joined$SDGs <- sdgs_collapsed
write.csv(dei_joined,
          here::here("data_processed/DEI_pubs_ordered.csv"),
          row.names = FALSE)

