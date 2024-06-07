library(readxl)
manual <- readxl::read_excel("data_manual/USC_Pubs_by_SDGs_Classification_for_manual_review.xlsx")
usc_pubs <- read.csv("data_processed/usc_pubs_law_2020_23.csv")
usc_sdgs <- read.csv("data_processed/usc_sdgs_with_categories_2020_23.csv")
usc_pubs_sdgs <- merge(usc_pubs, usc_sdgs, 
                       by = c("pubID", "Link"),
                       all.x = TRUE)
usc_pubs_sdgs$sustainability_category[is.na(usc_pubs_sdgs$sustainability_category)] = "Not-Related"

compare <- merge(usc_pubs_sdgs, manual, by = "pubID", suffixes = c("", ".y")) %>%
  mutate(sustainability_category.y = 
           ifelse(sustainability_category.y == "NA", "Not-Related", sustainability_category.y)) %>%
  select(names(usc_pubs_sdgs), sustainability_category.y, Manual_category)

all.equal(compare$sustainability_category, compare$sustainability_category.y)
# besides some NA

compare <- compare %>%
  mutate(final_category = ifelse(is.na(Manual_category), sustainability_category, Manual_category)) %>%
  select(-sustainability_category, -sustainability_category.y, -Manual_category) %>%
  rename(sustainability_category = final_category)

focused_titles = c("Geological Aspects of Using Saline Aquifers in the San Joaquin Basin for Energy Storage and Carbon Dioxide Sequestration",
                   "Integrated Carbon Emission Estimation Method and Energy Conservation Analysis: The Port of Los Angles Case Study",
                   "China’s Port Carbon Emission Reduction: A Study of Emission-Driven Factors",
                   "Remembering the ocean in water law")
which(compare$Titles %in% focused_titles)
compare[which(compare$Titles %in% focused_titles),]$sustainability_category <- "Sustainability-Focused"

write.csv(compare, 
          "data_processed/usc_pubs_with_sdgs_2020_23_manual_fix.csv", 
          row.names = FALSE)
write.csv(compare, 
          "shiny_app/usc_pubs_with_sdgs_2020_23_manual_fix.csv", 
          row.names = FALSE)
