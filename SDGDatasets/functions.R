# downloaded csv goes into DownloadedData folder
# newly formatted csv will be found in FormattedData folder

# paths of csv files
ff = list.files("./DownloadedData", pattern = "csv", full.names = TRUE)

removePunctuation = function(x) {
  gsub("[^[:alnum:][:space:]-]", "", x)
}

reformat = function(filename) {
  # read csv
  d = read.csv(filename)
  
  # clean abstract
  # remove copyright
  d$Abstract = gsub("([^.]*©.*)", "", d$Abstract)
  # remove punctuation
  d$Titles = removePunctuation(d$Titles)
  d$Abstract = removePunctuation(d$Abstract)

  # combine into a new dataframe with one column for SDG and one column for text
  # assuming that the file name is in the format with SDG#
  relativefilename = gsub(".*\\/", "", filename)
  sdgnum = gsub("[0-9]*SDG([0-9]*)\\.csv", "\\1", relativefilename)
  
  combinedtext = ""
  if ("Author.Keywords" %in% colnames(d)) {
    combinedtext = paste("[TITLE]", d$Titles, "[KEYWORDS]", d$Author.Keywords, "[ABSTRACT]", d$Abstract)
  } else {
    combinedtext = paste("[TITLE]", d$Titles, "[ABSTRACT]", d$Abstract)
  }
  
  df = data.frame("SDG" = sdgnum, "Text" = combinedtext)
  
  # output csv
  newfilename = paste("./FormattedData/re", relativefilename, sep = "")
  write.csv(df, file = newfilename, row.names = FALSE)
  
  df
}

tmp = lapply(ff, reformat)

# clean up
rm(tmp)
