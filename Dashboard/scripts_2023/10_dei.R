# for finding DEI related pubs
library(stringr)
library(dplyr)
usc_pubs_sdgs <- read.csv("data_processed/usc_pubs_with_sdgs_2020_24_manual_fix.csv")
usc_pubs_sdgs <- usc_pubs_sdgs %>% 
  filter(Year %in% c(2020, 2021, 2022, 2023, 2024)) %>% 
  filter(!Document.Type %in% c("Letter", "Retracted", "Note", "Erratum"))

sustainabilityrelated <- usc_pubs_sdgs %>% 
  filter(sustainability_category %in% c("Sustainability-Focused", "Sustainability-Inclusive"))
sustainabilityrelated$alltext <- paste(sustainabilityrelated$Titles,
                                       sustainabilityrelated$Abstract,
                                       sustainabilityrelated$Author.Keywords,
                                       sustainabilityrelated$Indexed.Keywords)
sustainabilityrelated$alltext <- tolower(sustainabilityrelated$alltext)

dei_keywords <- read.csv(here::here("data_raw/AsgmtEarth_DEI_3.3_Catalog_Keywords.csv"))
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
          here::here("data_processed/DEI_pubs_2020_24.csv"),
          row.names = FALSE)


# sorted by specific keywords then sustainability category
dei_data <- read.csv("data_processed/DEI_pubs_2020_24.csv")
usc_bridge <- read.csv("shiny_app/usc_bridge_2020_24_combined_edit.csv")
usc_authors <- read.csv("shiny_app/usc_authors_2020_24_combined_edit.csv")

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
          here::here("shiny_app/DEI_pubs_ordered_2020_24.csv"),
          row.names = FALSE)










# select from dei_joined only sustainability_category == Sustainability-Focused
# sort based on name_id and year
sus_focused_dei_joined <- dei_joined %>%
  filter(sustainability_category == "Sustainability-Focused")


sus_focused_dei_joined_20_23 <- sus_focused_dei_joined %>%
  filter(Year %in% c(2020, 2021, 2022, 2023))

sus_focused_dei_joined_21_24 <- sus_focused_dei_joined %>%
  filter(Year %in% c(2021, 2022, 2023, 2024))


# combine info for the same author_id, remove duplicates and use ; to connect other rows info
# headers are authorID	pubID	Link	Titles	Year	Source.title	DOI	Cited.by	Abstract	Indexed.Keywords	Author.Keywords	Publisher	Open.Access	Source	EID	SDG 0.01	SDG 0.02	SDG 0.03	SDG 0.04	SDG 0.05	SDG 0.06	SDG 0.07	SDG 0.08	SDG 0.09	SDG 0.10	SDG 0.11	SDG 0.12	SDG 0.13	SDG 0.14	SDG 0.15	SDG 0.16	SDG 0.17	all_SDGs	sustainability_category	DEI_3.3_keywords	Dept	Div	affls	name	name_id	firstname	lastname	fullname	initials	FirstSearch	LastSearch	First	Last	Email	PositionTitle	Type	InUSCDirectory	Pattern	id	firstlast	matched	onlyfirst	loweronlyfirst	lowerlast	firstnameissues	firstletter	important_keywords

library(stringr)

sus_focused_combined_pub_dei_joined_20_23 <- sus_focused_dei_joined_20_23 %>%
  group_by(authorID) %>%
  summarise(
    across(c(pubID, Link, Titles, Year, Source.title, DOI, Cited.by, Abstract,
             Indexed.Keywords, Author.Keywords, Publisher, Open.Access, Source,
             EID, sustainability_category, Dept, Div, affls,
             name, name_id, firstname, lastname, fullname, initials,
             FirstSearch, LastSearch, First, Last, Email, PositionTitle, Type,
             InUSCDirectory, Pattern, id, firstlast, matched, onlyfirst,
             loweronlyfirst, lowerlast, firstnameissues, firstletter,
             important_keywords),
           ~ paste(unique(.), collapse = "; ")),
    
    # Ensure unique DEI_3.3_keywords while keeping commas
    DEI_3.3_keywords = paste(unique(unlist(str_split(DEI_3.3_keywords, ",\\s*"))), collapse = ", "),
    
    # Ensure unique all_SDGs while keeping commas
    all_SDGs = paste(unique(unlist(str_split(all_SDGs, ",\\s*"))), collapse = ", "),
    
    across(starts_with("SDG"), sum, na.rm = TRUE)
  ) %>% 
  arrange(name_id, Year)  %>%
  ungroup()



