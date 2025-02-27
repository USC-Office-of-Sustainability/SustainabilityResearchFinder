duplicated_rows <- all_pubs %>%
  group_by(Link) %>%
  filter(n() > 1) %>%
  ungroup() %>%
  arrange(Link, Year)

write.csv(duplicated_rows, "duplicated_rows.csv")


# read data_processed/usc_pubs_with_sdgs_2020_24_manual_fix.csv
final_data <- read.csv("data_processed/usc_pubs_with_sdgs_2020_24_manual_fix.csv")
merged_data <- merge(duplicated_rows, final_data[, c("Link", "Year")], by = "Link", all.x = TRUE)
cat("c(", paste0('"', colnames(merged_data), '"', collapse = ", "), ")\n")
new_header <- c("Year.x", "Year.y", "Document.Type","Link", "Publication.Stage", "Titles",  "Authors", "Author.full.names", "Source.title", "Volume", "DOI", "Issue", "Art..No.", "Page.start", "Page.end", "Page.count","Author.s..ID", "pubID",  "Cited.by", "Affiliations", "Authors.with.affiliations", "Abstract", "Author.Keywords", "Indexed.Keywords", "Publisher",  "Open.Access", "Source", "EID")
merged_data <- merged_data[, new_header]
# sort first by Link then Year.x
merged_data <- merged_data[order(merged_data$Link, merged_data$Year.x), ]
# write merged_data to csv file called comparison_original_final.csv
write.csv(merged_data, "comparison_original_final.csv", row.names = FALSE)




final_data <- read.csv("data_processed/usc_pubs_with_sdgs_2020_24_manual_fix.csv")
# summarize row counts by Year column 
final_data %>%
  group_by(Year) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  arrange(Year)

