library(readxl)
library(dplyr)

manual <- readxl::read_excel("data_manual/USC_Pubs_by_SDGs_Classification_for_manual_review.xlsx")
usc_pubs <- read.csv("data_processed/usc_pubs_law_2020_23.csv")
usc_sdgs <- read.csv("data_processed/usc_sdgs_with_categories_2020_23.csv")
usc_pubs_sdgs <- merge(usc_pubs, usc_sdgs, 
                       by = c("pubID", "Link"),
                       all.x = TRUE)
usc_pubs_sdgs$sustainability_category[is.na(usc_pubs_sdgs$sustainability_category)] = "Not-Related"


# compare <- merge(usc_pubs_sdgs, manual, by = "pubID", suffixes = c("", ".y")) %>%
#   mutate(sustainability_category.y =
#            ifelse(sustainability_category.y == "NA", "Not-Related", sustainability_category.y)) %>%
#   select(names(usc_pubs_sdgs), sustainability_category.y, Manual_category)


compare <- merge(usc_pubs_sdgs, manual, by = "Link", suffixes = c("", ".y")) %>%
  mutate(sustainability_category.y = 
           ifelse(sustainability_category.y == "NA", "Not-Related", sustainability_category.y)) %>%
  select(names(usc_pubs_sdgs), sustainability_category.y, Manual_category) %>%
  unique()


filtered_2024 <- usc_pubs_sdgs %>%
  filter(Year == 2024)


filtered_2024 <- filtered_2024 %>%
  mutate(
    sustainability_category.y = sustainability_category, 
    Manual_category = sustainability_category, 
  )




# bind filtered_2024 with compare
compare <- rbind(compare, filtered_2024)

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




manual_classification_first_pass_2025 <- read.csv("data_manual/pub_manual_reclassification_first_pass_02-20-25.csv")

# go through every row in compare, and if the Titles col in compare matched pub col in manual_classification_first_pass_2025,
# change the compare's sustainability_category to the one in manual_classification_first_pass_2025's new_classification col
for (i in 1:nrow(compare)) {
  # print(compare$Titles[i])
  if (compare$Titles[i] %in% manual_classification_first_pass_2025$pub) {
    print(paste("change row", i, "from", compare$sustainability_category[i],
                "to", manual_classification_first_pass_2025$new_classification[which(manual_classification_first_pass_2025$pub == compare$Titles[i])]))
    compare$sustainability_category[i] <- manual_classification_first_pass_2025$new_classification[which(manual_classification_first_pass_2025$pub == compare$Titles[i])]
  }
}




manual_classification_second_pass_2025 <- read.csv("data_manual/pub_manual_reclassification_second_pass_02-26-25.csv")
# go through every row in compare, and if the Titles col in compare matched pub col in manual_classification_second_pass_2025,
# change the compare's sustainability_category to the one in manual_classification_second_pass_2025's new_classification col
for (i in 1:nrow(compare)) {
  # print(compare$Titles[i])
  if (compare$Titles[i] %in% manual_classification_second_pass_2025$pub) {
    print(paste("change row", i, "from", compare$sustainability_category[i],
                "to", manual_classification_second_pass_2025$new_classification[which(manual_classification_second_pass_2025$pub == compare$Titles[i])]))
    compare$sustainability_category[i] <- manual_classification_second_pass_2025$new_classification[which(manual_classification_second_pass_2025$pub == compare$Titles[i])]
  }
}

not_found_pubs_first_pass <- setdiff(manual_classification_first_pass_2025$pub, compare$Titles)
print(not_found_pubs_first_pass)

not_found_pubs_second_pass <- setdiff(manual_classification_second_pass_2025$pub, compare$Titles)
print(not_found_pubs_second_pass)




write.csv(compare, 
          "data_processed/usc_pubs_with_sdgs_2020_24_manual_fix.csv",
          row.names = FALSE)
write.csv(compare, 
          "shiny_app/usc_pubs_with_sdgs_2020_24_manual_fix.csv",
          row.names = FALSE)

