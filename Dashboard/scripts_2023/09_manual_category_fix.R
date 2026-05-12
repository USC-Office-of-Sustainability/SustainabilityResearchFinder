library(readxl)
library(dplyr)

manual <- readxl::read_excel("data_manual/USC_Pubs_by_SDGs_Classification_for_manual_review.xlsx")
usc_pubs <- read.csv("data_processed/05_pubs_with_law.csv")
usc_sdgs <- read.csv("data_processed/08_pubs_sdg_categorized.csv")
usc_pubs_sdgs <- merge(usc_pubs, usc_sdgs, 
                       by = c("pubID", "Link"),
                       all.x = TRUE)
usc_pubs_sdgs$sustainability_category[is.na(usc_pubs_sdgs$sustainability_category)] = "Not-Related"


compare <- merge(usc_pubs_sdgs, manual, by = "Link", suffixes = c("", ".y")) %>%
  mutate(sustainability_category.y = 
           ifelse(sustainability_category.y == "NA", "Not-Related", sustainability_category.y)) %>%
  select(names(usc_pubs_sdgs), sustainability_category.y, Manual_category) %>%
  unique()


# Add all publications not covered by the manual review spreadsheet
# (previously only Year == 2024 was added; this handles 2025 and any future years automatically)
not_reviewed <- usc_pubs_sdgs %>%
  filter(!Link %in% compare$Link) %>%
  mutate(
    sustainability_category.y = sustainability_category,
    Manual_category = sustainability_category
  )

compare <- rbind(compare, not_reviewed)
cat("Publications added without manual review:", nrow(not_reviewed),
    "| Years:", paste(sort(unique(not_reviewed$Year)), collapse = ", "), "\n")

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
          "data_processed/09_pubs_sdg_manual_fixed.csv",
          row.names = FALSE)
write.csv(compare,
          "shiny_app/09_pubs_sdg_manual_fixed.csv",
          row.names = FALSE)

