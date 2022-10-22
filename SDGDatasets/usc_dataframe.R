# combine 2 csv files
a = read.csv("./USC20000.csv")
b = read.csv("./USC4067.csv")
all = rbind(a, b)
# remove duplicate row
all = all[!duplicated(all),]
write.csv(all, file = "./USC_all.csv", row.names = FALSE)
