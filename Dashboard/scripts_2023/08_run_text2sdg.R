# Use text2sdg package to classify publications against USC PWG curated keywords

library(text2sdg)
library(here)
library(dplyr)
library(reshape2)
library(stringr)
library(stringi)
library(pluralize)

# --- Prepare USC PWG keyword system ------------------------------------------
usc_pwg_keywords <- read.csv("data_raw/USC_PWG-E_Keywords_11_5_24.csv", fileEncoding = "CP1252")
# Remove problematic character Ê
usc_pwg_keywords$keyword <- gsub('\u00CA', "", usc_pwg_keywords$keyword)
# Remove entries containing "#" (causes regex errors)
usc_pwg_keywords <- usc_pwg_keywords[-grep("#", usc_pwg_keywords$keyword), ]
# Normalize: remove punctuation, lowercase, deduplicate
usc_pwg_keywords$keyword <- gsub("[^[:alnum:][:space:]]", " ", usc_pwg_keywords$keyword)
usc_pwg_keywords$keyword <- tolower(usc_pwg_keywords$keyword)
usc_pwg_keywords <- usc_pwg_keywords[!duplicated(usc_pwg_keywords), ]
# Remove "race" and ambiguous gender/race compound terms
usc_pwg_keywords <- usc_pwg_keywords[-which(usc_pwg_keywords$keyword %in% c("race gender", "gender and sex", "sex and gender", "race and gender", "gender and race", "class and gender", "gender and class", "race")), ]
# Format as text2sdg query system
usc_pwg_system <- usc_pwg_keywords %>%
  mutate(system = "usc_pwg",
         query = paste0('"', keyword, '"')) %>%
  rename(sdg = goal) %>%
  select(system, sdg, query)


usc_data <- read.csv(here::here("data_processed/05_pubs_with_law.csv"))

# --- Text preprocessing functions --------------------------------------------
apply_context_dependency <- function(tt) {
  tt <- tolower(tt)
  corrections <- read.csv("data_raw/context_dependencies_01_21_25.csv")
  corrections$before <- tolower(corrections$before)
  corrections$after <- tolower(corrections$after)
  corrections$before <- gsub("([^0-9]),([^0-9])", "\\1 \\2", corrections$before)
  corrections$after <- gsub("([^0-9]),([^0-9])", "\\1 \\2", corrections$after)
  corrections$before <- gsub("/", " ", corrections$before)
  tt <- stri_replace_all_regex(tt,
                               pattern = corrections$before,
                               replacement = corrections$after,
                               vectorize = FALSE)
  tt
}

fix_apostrophe <- function(tt) {
  gsub("'", "'", tt)
}
remove_punctuation <- function(tt) {
  gsub("[^[:alnum:][:space:]']", " ", fix_apostrophe(tt))
}
keep_semicolon <- function(tt) {
  gsub("[^[:alnum:][:space:]';]", " ", fix_apostrophe(tt))
}
remove_copyright <- function(tt) {
  tt <- gsub("([^.]*Copyright.*)", " ", tt)
  tt <- gsub("([^.]*\u00a9.*)", " ", tt)
}

# --- Build combined text field for each publication --------------------------
usc_data$newtitle          <- apply_context_dependency(remove_punctuation(usc_data$Titles))
usc_data$newabstract       <- apply_context_dependency(remove_punctuation(remove_copyright(usc_data$Abstract)))
usc_data$newauthorkeywords <- apply_context_dependency(keep_semicolon(usc_data$Author.Keywords))
usc_data$newindexedkeywords <- apply_context_dependency(keep_semicolon(usc_data$Indexed.Keywords))

# Scopus uses Elsevier search queries on title, abstract, keywords
usc_data$alltext <- paste(usc_data$newtitle,
                          usc_data$newabstract,
                          usc_data$newauthorkeywords,
                          usc_data$newindexedkeywords, sep = ";")
usc_data$alltext <- gsub("'", " ", usc_data$alltext)

# --- Run text2sdg keyword detection ------------------------------------------
hits <- detect_any(usc_data$alltext, usc_pwg_system, output = "features")
hits$features <- gsub(",", "", hits$features)
usc_data$document <- 1:nrow(usc_data)
hits_link_count <- merge(hits, usc_data[c("document", "pubID", "Link")], by = "document")

write.csv(hits_link_count,
          here::here("data_processed/08_text2sdg_features.csv"),
          row.names = FALSE)


# --- Aggregate: count unique keywords per document x SDG (min 2 to flag) ----
social_economic_SDGs <- c("SDG-01", "SDG-02", "SDG-03", "SDG-04", "SDG-05", "SDG-08",
                           "SDG-09", "SDG-10", "SDG-11", "SDG-16", "SDG-17")
environmental_SDGs   <- c("SDG-06", "SDG-07", "SDG-12", "SDG-13", "SDG-14", "SDG-15")

# animal and animals count as 1 keyword (singularize); takes ~2 min
hits_sum <- hits %>%
  group_by(document, sdg) %>%
  summarize(nkeywords = length(unique(singularize(features)))) %>%
  filter(nkeywords >= 2) %>%
  dcast(document ~ sdg, fill = 0) %>%
  left_join(
    hits %>%
      filter(sdg %in% social_economic_SDGs) %>%
      group_by(document) %>%
      summarize(social_economic_SDGs = paste(unique(features), collapse = ", ")),
    by = "document"
  ) %>%
  left_join(
    hits %>%
      filter(sdg %in% environmental_SDGs) %>%
      group_by(document) %>%
      summarize(environmental_SDGs = paste(unique(features), collapse = ", ")),
    by = "document"
  )

hits_sum_link <- merge(hits_sum, usc_data[c("document", "pubID", "Link")], by = "document")

# Collapse per-SDG flag columns into a single comma-separated SDG list
sdgs_only <- hits_sum_link %>%
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
hits_sum_link$all_SDGs <- sdgs_collapsed

write.csv(hits_sum_link,
          here::here("data_processed/08_pubs_sdg_flags.csv"),
          row.names = FALSE)


# --- Assign sustainability category based on SDG coverage --------------------
# Sustainability-Focused:  has both environmental SDGs (6,7,12-15) AND social/economic SDGs
# Sustainability-Inclusive: 2+ SDGs total
# SDG-Related:              exactly 1 SDG
usc_sdgs <- read.csv("data_processed/08_pubs_sdg_flags.csv")
usc_sdgs$num_sdgs <- rowSums(select(usc_sdgs, starts_with("SDG")) != 0)
sustainabilityresearch <- usc_sdgs %>%
  mutate(
    sustainability_category =
      case_when(
        (SDG.06 > 0 | SDG.07 > 0 | SDG.12 > 0 | SDG.13 > 0 | SDG.14 > 0 | SDG.15 > 0) &
          (SDG.01 > 0 | SDG.02 > 0 | SDG.03 > 0 | SDG.04 > 0 |
             SDG.05 > 0 | SDG.08 > 0 |
             SDG.09 > 0 | SDG.10 > 0 | SDG.11 > 0 |
             SDG.16 > 0 | SDG.17 > 0) ~ "Sustainability-Focused",
        (num_sdgs >= 2) ~ "Sustainability-Inclusive",
        (num_sdgs == 1) ~ "SDG-Related"))
# everything in here is sustainability inclusive so next line does nothing
sustainabilityresearch$sustainability_category[is.na(sustainabilityresearch$sustainability_category)] = "Not-Related"
write.csv(sustainabilityresearch,
          here::here("data_processed/08_pubs_sdg_categorized.csv"),
          row.names = FALSE)
