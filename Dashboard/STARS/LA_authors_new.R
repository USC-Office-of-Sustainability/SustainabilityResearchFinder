library(stringr)
library(dplyr)


data <- read.csv("shiny_app/09_pubs_sdg_manual_fixed.csv")
usc_bridge <- read.csv("shiny_app/07_bridge_manual_edited.csv")
usc_authors <- read.csv("shiny_app/14_authors_dept_corrected.csv")

tmp <- merge(data, usc_bridge,
             by.x = c("pubID", "Link"), by.y = c("pubID", "Link"))
data_joined <- merge(tmp, usc_authors,
                    by.x = "authorID", by.y = "authorID")



sustainabilityrelated <- data_joined %>% 
  #filter(sustainability_category %in% c("Sustainability-Inclusive", "Sustainability-Focused"))
  filter(sustainability_category %in% c("Sustainability-Focused"))
sustainabilityrelated$alltext <- paste(sustainabilityrelated$Titles,
                                       sustainabilityrelated$Abstract,
                                       sustainabilityrelated$Author.Keywords,
                                       sustainabilityrelated$Indexed.Keywords)
sustainabilityrelated$alltext <- tolower(sustainabilityrelated$alltext)

specific_keywords <- c("los angeles", "central la", "east la", "south la", "boyle heights")
specific_pattern <- paste(specific_keywords, collapse = "|")



LA_pubs <- sustainabilityrelated[grep(specific_pattern, sustainabilityrelated$alltext),]



LA_authors <- LA_pubs %>%
  group_by(authorID) %>%
  summarise(across(everything(), ~ paste(unique(.x), collapse = ";"))) %>%
  select(
    name,
    name_id,
    fullname,
    Dept,
    Div,
    sustainability_category,
    social_economic_SDGs,
    environmental_SDGs,
    all_SDGs,
    pubID,
    Link,
    Titles,
    Indexed.Keywords,
    Author.Keywords,
    alltext
  )


write.csv(LA_authors, "shiny_app/LA_authors.csv", row.names = FALSE)





