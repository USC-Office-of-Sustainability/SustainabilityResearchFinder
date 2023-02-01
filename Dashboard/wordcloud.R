# Create word clouds for each SDG using Title, Abstract, Keywords

# Load the required packages --------------------------------------------------
# install.packages("name") to install any missing packages
list_of_packages <- c("tm", "tidyverse", "dplyr", "here", "wordcloud",
                      "RColorBrewer", "wordcloud2")
lapply(list_of_packages, library, character.only = TRUE)

set.seed(1234)

usc_pubs <- read.csv(here::here("data_processed/usc_pubs.csv"))
usc_sdgs <- read.csv(here::here("data_processed/usc_sdgs.csv"))
usc_pubs <- usc_pubs %>% filter(Year %in% c(2020, 2021, 2022))
usc_pubs_sdgs <- merge(usc_pubs, usc_sdgs, 
                       by.x = c("X", "Link"), by.y = c("document", "Link"))

sdg_col_names <- usc_sdgs %>% select(starts_with("SDG")) %>% names()
sdg_colors <- c("1" = "#E5243B", "2" = "#DDA63A", "3" = "#4C9F38", 
                "4" = "#C5192D", "5" = "#FF3A21", "6" = "#26BDE2",
                "7" = "#FCC30B", "8" = "#A21942", "9" = "#FD6925",
                "10" = "#DD1367", "11" = "#FD9D24", "12" = "#BF8B2E",
                "13" = "#3F7E44", "14" = "#0A97D9", "15" = "#56C02B",
                "16" = "#00689D", "17" = "#19486A")

morestopwords <- c("used","also","using","ghg","found","model","models-","soc",
                   "across","including","will","well","csr","use","authors",
                   "high","whether","findings","higher","ses","per","can",
                   "however","time","two","show","may","-well","data","years",
                   "adds","days","among","total","results","text","risk","low",
                   "one","work")

create_sdg_wordcloud <- function(i, sdg_col, sdg_color) {
  target_text <- usc_pubs_sdgs %>%
    filter(!!sdg_col != 0) %>%
    select(Titles, Abstract, Author.Keywords, Indexed.Keywords) %>%
    head(1000)
  target_text$all <- paste(target_text$Titles, 
                           target_text$Abstract, 
                           target_text$Author.Keywords, 
                           target_text$Indexed.Keywords)
  
  # remove numbers and punctuation
  target_text$all <- gsub("[^[:alpha:][:blank:]]", "", target_text$all)
  
  docs <- Corpus(VectorSource(target_text$all))
  m <- docs %>% 
    tm_map(stripWhitespace) %>%
    tm_map(content_transformer(tolower)) %>%
    tm_map(removeWords, stopwords("english")) %>%
    tm_map(removeWords, morestopwords) %>%
    TermDocumentMatrix %>%
    as.matrix
  words <- sort(rowSums(m), decreasing = TRUE) 
  df <- data.frame(word = names(words), freq = words)
  png(here::here(paste0("www/sdg", i, ".png")))
  wordcloud(words = df$word, freq = df$freq, min.freq = 1,
            max.words = 50, random.order = FALSE, rot.per = 0,
            colors = sdg_color, scale = c(8,1))
  dev.off()
}

for (i in 1:17) {
  create_sdg_wordcloud(i, sym(sdg_col_names[i]), sdg_colors[i])
}
