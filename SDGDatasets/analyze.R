# comparing the accuracy of predictions by ML model

# paths of csv files
ff = list.files("../Predictions", pattern = "csv", full.names = TRUE)

# get primary and secondary SDGs
orig = read.csv("./DownloadedData/SDG_PUB_testing_ckidsdataset.csv")
orig = orig[, c("Primary.SDG", "SDGs_All")]
orig = orig[orig$Primary.SDG %in% seq(1,16),]
orig$X = seq(0, length(orig$Primary.SDG)-1)

calculateAccuracy = function(filename) {
  pred = read.csv(filename)
  # rename prediction column
  names(pred)[2] <- 'Predicted.SDG'
  d = merge(orig, pred, by = intersect(names(orig), names(pred)))
  
  # for primary SDG
  d$MatchedPrimary = d$Primary.SDG == d$Predicted.SDG
  
  # for secondary SDGs
  d$MatchedSecondary = FALSE
  separatedSDGs = lapply(strsplit(d$SDGs_All, ","), trimws)
  for (i in 1:length(separatedSDGs)) {
    d[i,]$MatchedSecondary = d[i,]$Predicted.SDG %in% separatedSDGs[[i]]
  }
  
  #df = d[!names(d) %in% c("X")]
  #write.csv(df, file = "./testing_results.csv", row.names = FALSE)
  
  res = data.frame("name" = filename, 
                   "Primary SDG Accuracy" = sum(d$MatchedPrimary)/length(d$MatchedPrimary),
                   "Secondary SDG Accuracy" = sum(d$MatchedSecondary)/length(d$MatchedSecondary))
  return (res)
}

res = lapply(ff, calculateAccuracy)
