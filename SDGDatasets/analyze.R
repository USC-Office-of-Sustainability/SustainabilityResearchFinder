# comparing the accuracy of predictions by ML model

# paths of csv files
ff = list.files("../Predictions", pattern = "csv", full.names = TRUE)

# get primary and secondary SDGs
orig = read.csv("./DownloadedData/SDG_PUB_testing_ckidsdataset.csv")
orig = orig[, c("Primary.SDG", "SDGs_All", "DOI")]
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

pred = read.csv("predictions_aurora.csv")
names(pred)[3] <- 'Primary.SDG'
d = merge(orig, pred, by = intersect(names(orig), names(pred)))
d$Primary.SDG = paste("'",d$Primary.SDG,"'", sep="")

# remove []
d$SDG.Predictions = sapply(d$SDG.Predictions, function(x) {x = gsub("\\[([^]]*)\\]", "\\1",x)})
d$Probabilities = sapply(d$Probabilities, function(x) {x = gsub("\\[([^]]*)\\]", "\\1", x)})

d$PredictedPrimary = 0
for (i in 1:length(d$X)) {
  idx = which.max(lapply(strsplit(d$Probabilities[i], ","), function(x) {x = as.numeric(trimws(x))})[[1]])
  d[i,]$PredictedPrimary = lapply(strsplit(d$SDG.Predictions[i], ","), trimws)[[1]][idx]
}

d$MatchedPrimary = d$PredictedPrimary == d$Primary.SDG
d$MatchedSecondary = FALSE
d$MatchedAny = FALSE

separatedSDGs = lapply(strsplit(d$SDGs_All, ","), function(x) {
  x=trimws(x)
  paste("'",x,"'", sep="")})
for (i in 1:length(separatedSDGs)) {
  d[i,]$MatchedSecondary = any(grepl(d[i,]$PredictedPrimary, separatedSDGs[[i]]))
  for (j in separatedSDGs[[i]]) {
    d[i,]$MatchedAny = any(grepl(j, d$SDG.Predictions[i]), d[i,]$MatchedAny)
  }
}

# calculate
res = data.frame("name" = "predictions_aurora.csv", 
                 "Primary SDG Accuracy" = sum(d$MatchedPrimary)/length(d$MatchedPrimary),
                 "Secondary SDG Accuracy" = sum(d$MatchedSecondary)/length(d$MatchedSecondary),
                 "Any SDG Accuracy" = sum(d$MatchedAny)/length(d$MatchedAny))

write.csv(d, file = "./predictions_aurora_primarysecondaryall.csv", row.names = FALSE)

