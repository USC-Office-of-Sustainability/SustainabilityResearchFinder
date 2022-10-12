# for training data
# downloaded csv goes into DownloadedData folder
# newly formatted csv will be found in FormattedData folder

# paths of csv files
ff = list.files("./DownloadedData", pattern = "csv", full.names = TRUE)

removePunctuation = function(x) {
  gsub("[^[:alnum:][:space:]-]", "", x)
}

# input: 1 data frame
# output: 1 data frame with cleaned text
cleanText = function(d) {
  # remove copyright
  d$Abstract = gsub("([^.]*©.*)", "", d$Abstract)
  
  # remove punctuation
  if ("Titles" %in% colnames(d)) {
    d$Titles = removePunctuation(d$Titles)
  } else {
    d$Title = removePunctuation(d$Title)
  }
  d$Abstract = removePunctuation(d$Abstract)
  if ("Author.Keywords" %in% colnames(d)) {
    d$Author.Keywords = removePunctuation(d$Author.Keywords)
  }
  d
}

# input: 1 data frame
# output: string in the format "[TITLE]...[KEYWORDS]...[ABSTRACT]..."
oneString = function(d) {
  s = paste("[TITLE]", d$Title) # includes d$Titles
  if ("Author.Keywords" %in% colnames(d)) {
    s = paste(s, "[KEYWORDS]", d$Author.Keywords)
  }
  s = paste(s, "[ABSTRACT]", d$Abstract)
  s
}

# input: file name
# output: 1 data frame: file name and "Done"
# writes a CSV file with one column for SDG and one column for text
# formatted CSV files found in ./FormattedData/
reformat = function(filename) {
  # read csv
  d = read.csv(filename)
  
  # remove SDG 0 and 17 for testing data
  if ("Primary.SDG" %in% colnames(d)) {
    d = d[d$Primary.SDG %in% seq(1,16),]
  }
  
  # clean text
  d = cleanText(d)
  
  # make new data frame for formatted csv
  combinedtext = oneString(d)
  df = data.frame("Text" = combinedtext)
  
  # get the SDG
  if ("Primary.SDG" %in% colnames(d)) {
    df$SDG = d$Primary.SDG
  } else {
    # assuming that the file name is in the format with SDG#
    relativefilename = gsub(".*\\/", "", filename)
    df$SDG = gsub("[0-9]*SDG([0-9]*)\\.csv", "\\1", relativefilename)
  }
  
  # output csv
  newfilename = gsub("DownloadedData", "FormattedData", filename)
  write.csv(df, file = newfilename, row.names = FALSE)
  
  data.frame("File name" = filename, "Text" = "done")
}

tmp = lapply(ff, reformat)

# clean up
rm(tmp)
