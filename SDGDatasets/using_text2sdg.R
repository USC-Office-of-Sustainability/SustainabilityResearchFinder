library(text2sdg)
d = read.csv("./USCData/USC_all.csv")
h = detect_sdg(d$Abstract,
               system = c("Aurora", "Elsevier", "SIRIS", "SDSN", "OSDG")) # output = features
plot_sdg(h)
crosstab_sdg(h)

plot_sdg(h) +
  ggplot2::facet_wrap(~system, ncol = 1, scales = "free_y")

plot_sdg(h[which(h$system %in% c("SDSN", "OSDG")),])

plot_sdg(h[which(h$system %in% c("Aurora", "Elsevier", "SIRIS")),])

plot_sdg(h[which(h$system %in% c("Aurora", "SDSN", "OSDG")),])


h_doc = detect_sdg(d$Abstract,
                   system = c("Aurora", "Elsevier", "SIRIS", "SDSN", "OSDG"),
                   output = "documents")

# only aurora, osdg, sdsn have sdg17

# check bhavna sharma
idx = which(d$Titles %in% "Mechanical properties of laminated bamboo designed for curvature")
h[which(h$document == idx),]
h_doc[which(h_doc$document == idx),]

# use title instead of abstract
h_title = detect_sdg(d$Titles,
                     system = c("Aurora", "Elsevier", "SIRIS", "SDSN", "OSDG"))
## check bhavna sharma
h_title[which(h_title$document == idx),]
## does not exist

# compare osdg and scopus search queries
scopus_search = read.csv("../RShinyDashboard/USC_SDG0to16.csv")
d$i = 1:nrow(d)
c = merge(d, scopus_search)
c$sdg = ifelse(c$Primary.SDG<10, paste("SDG-0", c$Primary.SDG, sep=""), paste("SDG-", c$Primary.SDG, sep=""))
h_osdg = h[which(h$system == "OSDG"),]

res = data.frame(matrix(ncol = 2, nrow = 0))
colnames(res) = c("i", "sdg_found")
for (j in 1:nrow(c)) {
  h_sub = h_osdg[which(h_osdg$document == c[j,]$i),]
  if (c[j,]$i %in% res$i) {
    res[which(res$i == c[j,]$i),]$sdg_found = res[which(res$i == c[j,]$i),]$sdg_found | (c[j,]$sdg %in% h_sub$sdg)
  } else {
    res = rbind(res, data.frame(i = c[j,]$i, sdg_found = c[j,]$sdg %in% h_sub$sdg))
  }
}

sum(res$sdg_found)/nrow(res)
# for each pub, checks if primary sdg from scopus search queries matches 1 of the sdg assigned by osdg
# res has 24541 instead of 25143 (some pub got assigned multiple by scopus)

# text2sdg on USC_SDG0to16
h_scopus = detect_sdg(scopus_search$Abstract,
                      system = c("OSDG"))
scopus_search$sdg = ifelse(scopus_search$Primary.SDG<10, paste("SDG-0", scopus_search$Primary.SDG, sep=""), paste("SDG-", scopus_search$Primary.SDG, sep=""))
t = 0
for (i in 1:nrow(scopus_search)) {
  h_sub = h_scopus[which(h_scopus$document == i),]
  if (scopus_search[i,]$sdg %in% h_sub$sdg) {
    t = t + 1
  }
}
t/nrow(scopus_search)
# did not remove pubs that appear >1 time

# compare osdg and elsevier
crosstab_sdg(h, compare = "systems")

h_elsevier = h[which(h$system == "Elsevier"),]
library(dplyr)
osdg_tf = h_osdg %>% group_by(document) %>% distinct(sdg) %>%
  summarise("SDG1" = sum(sdg == "SDG-01"),
            "SDG2" = sum(sdg == "SDG-02"),
            "SDG3" = sum(sdg == "SDG-03"),
            "SDG4" = sum(sdg == "SDG-04"),
            "SDG5" = sum(sdg == "SDG-05"),
            "SDG6" = sum(sdg == "SDG-06"),
            "SDG7" = sum(sdg == "SDG-07"),
            "SDG8" = sum(sdg == "SDG-08"),
            "SDG9" = sum(sdg == "SDG-09"),
            "SDG10" = sum(sdg == "SDG-10"),
            "SDG11" = sum(sdg == "SDG-11"),
            "SDG12" = sum(sdg == "SDG-12"),
            "SDG13" = sum(sdg == "SDG-13"),
            "SDG14" = sum(sdg == "SDG-14"),
            "SDG15" = sum(sdg == "SDG-15"),
            "SDG16" = sum(sdg == "SDG-16"),
            "SDG17" = sum(sdg == "SDG-17")
            )
nrow(osdg_tf) # 20581 rows
elsevier_tf = h_elsevier %>% group_by(document) %>% distinct(sdg) %>%
  summarise("SDG1" = sum(sdg == "SDG-01"),
            "SDG2" = sum(sdg == "SDG-02"),
            "SDG3" = sum(sdg == "SDG-03"),
            "SDG4" = sum(sdg == "SDG-04"),
            "SDG5" = sum(sdg == "SDG-05"),
            "SDG6" = sum(sdg == "SDG-06"),
            "SDG7" = sum(sdg == "SDG-07"),
            "SDG8" = sum(sdg == "SDG-08"),
            "SDG9" = sum(sdg == "SDG-09"),
            "SDG10" = sum(sdg == "SDG-10"),
            "SDG11" = sum(sdg == "SDG-11"),
            "SDG12" = sum(sdg == "SDG-12"),
            "SDG13" = sum(sdg == "SDG-13"),
            "SDG14" = sum(sdg == "SDG-14"),
            "SDG15" = sum(sdg == "SDG-15"),
            "SDG16" = sum(sdg == "SDG-16"),
            #"SDG17" = sum(sdg == "SDG-17")
            )
nrow(elsevier_tf) # 6170 rows
length(intersect(elsevier_tf$document, osdg_tf$document))
osdg_elsevier = merge(osdg_tf, elsevier_tf, by = "document", suffixes = c(".osdg", ".elsevier"))
osdg_elsevier[1,]
# did not make comparison

# compare scopus with Elsevier, Aurora, SDSN, SIRIS, separately
h_scopus = detect_sdg(scopus_search$Abstract,
                      system = c("Elsevier"))
scopus_search$sdg = ifelse(scopus_search$Primary.SDG<10, paste("SDG-0", scopus_search$Primary.SDG, sep=""), paste("SDG-", scopus_search$Primary.SDG, sep=""))
t = 0
for (i in 1:nrow(scopus_search)) {
  h_sub = h_scopus[which(h_scopus$document == i),]
  if (scopus_search[i,]$sdg %in% h_sub$sdg) {
    t = t + 1
  }
}
t/nrow(scopus_search)

# compare scopus with all
h_scopus = detect_sdg(scopus_search$Abstract,
                      system = c("Aurora", "Elsevier", "SIRIS", "SDSN", "OSDG"))
scopus_search$sdg = ifelse(scopus_search$Primary.SDG<10, paste("SDG-0", scopus_search$Primary.SDG, sep=""), paste("SDG-", scopus_search$Primary.SDG, sep=""))
t = 0
for (i in 1:nrow(scopus_search)) {
  h_sub = h_scopus[which(h_scopus$document == i),]
  if (scopus_search[i,]$sdg %in% h_sub$sdg) {
    t = t + 1
  }
}
t/nrow(scopus_search)
