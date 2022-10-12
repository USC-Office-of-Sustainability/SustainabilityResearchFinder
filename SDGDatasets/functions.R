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
  if ("Author.Keywords" %in% colnames(d)) {
    return (paste("[TITLE]", d$Titles, "[KEYWORDS]", d$Author.Keywords, "[ABSTRACT]", d$Abstract))
  } else {
    return (paste("[TITLE]", d$Titles, "[ABSTRACT]", d$Abstract))
  }
}

# input: file name
# output: 1 data frame: file name and "Done"
# writes a CSV file with one column for SDG and one column for text
# CSV files found in ./FormattedData/
reformat = function(filename, SDGinName = FALSE) {
  # read csv
  d = read.csv(filename)
  
  # clean text
  d = cleanText(d)

  # assuming that the file name is in the format with SDG#
  relativefilename = gsub(".*\\/", "", filename)
  sdgnum = gsub("[0-9]*SDG([0-9]*)\\.csv", "\\1", relativefilename)
  
  combinedtext = oneString(d)
  
  df = data.frame("SDG" = sdgnum, "Text" = combinedtext)
  
  # output csv
  newfilename = paste("./FormattedData/re", relativefilename, sep = "")
  write.csv(df, file = newfilename, row.names = FALSE)
  
  data.frame("File name" = filename, "Text" = "done")
}

tmp = lapply(ff, reformat)

# clean up
rm(tmp)

# for testing data
d = read.csv("./DownloadedTestingData/SDG_PUB_testing_ckidsdataset.csv")
d = cleanText(d)

# remove SDG 0 and 17
d = d[d$Primary.SDG %in% seq(1,16),]

combinedtext = oneString(d)

df = data.frame("SDG" = d$Primary.SDG, "Text" = combinedtext)

newfilename = "./FormattedData/SDG_PUB_Testing.csv"
write.csv(df, file = newfilename, row.names = FALSE)
