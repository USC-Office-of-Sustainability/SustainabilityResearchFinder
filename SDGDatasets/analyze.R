# comparing the accuracy of predictions by ML model

pred = read.csv("./predictions.csv")
# get primary and secondary SDGs
orig = read.csv("./DownloadedData/SDG_PUB_testing_ckidsdataset.csv")
orig = orig[, c("Primary.SDG", "SDGs_All")]
orig = orig[orig$Primary.SDG %in% seq(1,16),]
orig$X = seq(0, length(pred$X)-1)
d = merge(orig, pred, by = intersect(names(orig), names(pred)))

# for primary SDG
d$MatchedPrimary = d$Primary.SDG == d$Predicted.SDG
sum(d$MatchedPrimary)/length(d$MatchedPrimary)

# for secondary SDGs
d$MatchedSecondary = FALSE
separatedSDGs = lapply(strsplit(d$SDGs_All, ","), trimws)
for (i in 1:length(separatedSDGs)) {
  d[i,]$MatchedSecondary = d[i,]$Predicted.SDG %in% separatedSDGs[[i]]
}
sum(d$MatchedSecondary)/length(d$MatchedSecondary)

df = d[!names(d) %in% c("X")]
write.csv(df, file = "./testing_results.csv", row.names = FALSE)
