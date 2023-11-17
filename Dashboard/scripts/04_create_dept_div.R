# create department + division table
library(dplyr)
authors_all <- read.csv("data_processed/authors_all_11_6_23.csv")

usc_departments <- read.csv("data_processed/usc_departments.csv") %>%
  rename(Division = School.Institute.Center,
         Department = Department.Group)
df <- data.frame(Division = c(unique(usc_departments$Division), "Other"),
           Department = "Other", Pattern = "")
usc_departments_other <- rbind(usc_departments, df)
usc_departments_other$id <- 1:nrow(usc_departments_other)

# remove ; in ;Keck or Keck;
authors_all$Dept <- sapply(authors_all$Dept, function(x) {
  d <- strsplit(x, ';')[[1]]
  d <- d[d != ""]
  paste(d, collapse = ";")
})
authors_all$Div[is.na(authors_all$Div)] <- ""
authors_all$Div <- sapply(authors_all$Div, function(x) {
  d <- strsplit(x, ';')[[1]]
  d <- d[d != ""]
  paste(d, collapse = ";")
})

authors_all %>%
  tidyr::separate_rows(Dept, sep=";") %>%
  mutate(Dept = ifelse(Dept == "", "Other", Dept)) -> authors_split
authors_split %>%
  tidyr::separate_rows(Div, sep=";") %>%
  mutate(Div = ifelse(Div == "", "Other", Div)) -> authors_split2

authors_split2$Dept <- gsub("&", "and", authors_split2$Dept)

# remove office of provost - faculty affairs
# authors_split2[-which(authors_split2$Div == "Office of the Provost" & authors_split2$Dept == "Academic and Faculty Affairs"),] -> authors_split2
# remove sarah nguyen in viterbi
authors_split2[-which(authors_split2$fullname == "Nguyen, Sarah Hoan" & authors_split2$Div == "Viterbi School of Engineering"),] -> authors_split2

# fix 17##- departments
authors_split2$Dept[grep("1710", authors_split2$Dept)] <- "Health Systems Operations"
authors_split2$Dept[grep("1726", authors_split2$Dept)] <- "Keck Hospital of USC"
authors_split2$Dept[grep("1733", authors_split2$Dept)] <- "Norris Comprehensive Cancer Center"
authors_split2$Dept[grep("1750", authors_split2$Dept)] <- "USC Verdugo Hills Hospital"
authors_split2$Dept[grep("1776", authors_split2$Dept)] <- "USC Care Clinical - Ambulatory"
authors_split2$Dept[grep("1790", authors_split2$Dept)] <- "Student Health"
authors_split2$Dept[grep("1740", authors_split2$Dept)] <- "USC Care MSO"

# all law departments to "Law"

# other dept
authors_split2$Dept[grep("Compensation", authors_split2$Dept)] <- "Other"
authors_split2$Dept[grep("Dean", authors_split2$Dept)] <- "Other"
authors_split2$Dept[grep("Salaries", authors_split2$Dept)] <- "Other"
authors_split2$Dept[grep("Human Resources", authors_split2$Dept)] <- "Other"
authors_split2$Dept[grep("Faculty Affairs", authors_split2$Dept)] <- "Other"
authors_split2$Dept[grep("Business Office", authors_split2$Dept)] <- "Other"
authors_split2$Dept[grep("Administrative and Business Affairs", authors_split2$Dept)] <- "Other"
authors_split2$Dept[grep("Distance Learning Program", authors_split2$Dept)] <- "Other"
authors_split2$Dept[grep("Postdoctoral Fellows", authors_split2$Dept)] <- "Other"
authors_split2$Dept[grep("On-line Dental Program", authors_split2$Dept)] <- "Other"
authors_split2$Dept[grep("Graduate Assistantships", authors_split2$Dept)] <- "Other"
authors_split2$Dept[grep("Admission and Student Engagement", authors_split2$Dept)] <- "Other"
authors_split2$Dept[setdiff(grep("Administration", authors_split2$Dept), grep("Business|Health Administration", authors_split2$Dept))] <- "Other"

authors_split2$Dept[grep("Shoah Foundation", authors_split2$Dept)] <- "Shoah Foundation"


# fix majors that exist in spreadsheet - maybe edit spreadsheet
authors_split2$Dept[which(authors_split2$Dept == "Aerospace Engineering")] <- "Aerospace and Mechanical Engineering"
authors_split2$Dept[which(authors_split2$Dept == "Advanced Orthodontics")] <- "Orthodontics"
authors_split2$Dept[which(authors_split2$Dept == "Advanced Prosthodontics")] <- "Prosthodontics"
authors_split2$Dept[which(authors_split2$Dept == "Advanced Oral and Maxillofacial Surgery")] <- "Oral and Maxillofacial Surgery"
authors_split2$Dept[which(authors_split2$Dept == "Applied and Computational Mathematics")] <- "Mathematics"
authors_split2$Dept[which(authors_split2$Dept == "Applied Mathematics")] <- "Mathematics"
authors_split2$Dept[which(authors_split2$Dept == "Applied Biomedical and Clinical Sciences")] <- "Programs in Biomedical and Biological Sciences"
authors_split2$Dept[which(authors_split2$Dept == "Development, Stem Cells and Regenerative Medicine")] <- "Programs in Biomedical and Biological Sciences"
authors_split2$Dept[which(authors_split2$Dept == "Infectious Diseases, Immunology and Pathogenesis")] <- "Programs in Biomedical and Biological Sciences"
authors_split2$Dept[which(authors_split2$Dept == "Applied Economics and Econometrics")] <- "Economics"
authors_split2$Dept[which(authors_split2$Dept == "Biochemistry")] <- "Biochemistry and Molecular Medicine"
authors_split2$Dept[which(authors_split2$Dept == "Biokinesiology")] <- "Division of Biokinesiology and Physical Therapy"
authors_split2$Dept[which(authors_split2$Div == "Division of Biokinesiology and Physical Therapy")] <- "Division of Biokinesiology and Physical Therapy"
authors_split2$Dept[which(authors_split2$Dept == "Biological Sciences (Molecular, Cellular and Developmental Biology)")] <- "Biological Sciences"
authors_split2$Dept[which(authors_split2$Dept == "Human Biology")] <- "Biological Sciences"
authors_split2$Dept[which(authors_split2$Dept == "Biology (Marine Biology and Biological Oceanography)")] <- "Biological Sciences"
authors_split2$Dept[which(authors_split2$Dept == "Center For Effective Organization (CEO)")] <- "Center for Effective Organizations"
authors_split2$Dept[which(authors_split2$Dept == "Civil Engineering")] <- "Civil and Environmental Engineering"
authors_split2$Dept[which(authors_split2$Dept == "Civil/Environmental Engineering")] <- "Civil and Environmental Engineering"
authors_split2$Dept[which(authors_split2$Dept == "Civil Engineering (Transportation Engineering)")] <- "Civil and Environmental Engineering"
authors_split2$Dept[which(authors_split2$Dept == "Communication")] <- "School of Communication"
authors_split2$Dept[which(authors_split2$Dept == "All Other Grants - School of Communication")] <- "School of Communication"
authors_split2$Dept[which(authors_split2$Dept == "Journalism")] <- "School of Journalism"
authors_split2$Dept[which(authors_split2$Dept == "Computer Engineering")] <- "Electrical and Computer Engineering"
authors_split2$Dept[which(authors_split2$Dept == "East Asian Language and Cultures")] <- "East Asian Languages and Cultures"
authors_split2$Dept[which(authors_split2$Dept == "East Asian Area Studies")] <- "East Asian Studies Center"
authors_split2$Dept[which(authors_split2$Dept == "Epidemiology")] <- "Population and Public Health Sciences"
authors_split2$Dept[which(authors_split2$Dept == "Public Health")] <- "Population and Public Health Sciences"
authors_split2$Dept[which(authors_split2$Dept == "Preventive Medicine (Health Behavior Research)")] <- "Population and Public Health Sciences"
authors_split2$Dept[which(authors_split2$Dept == "Gender Studies")] <- "Gender and Sexuality Studies"
authors_split2$Dept[which(authors_split2$Dept == "Geological Sciences")] <- "Earth Sciences"
authors_split2$Dept[which(authors_split2$Dept == "Global Medicine")] <- "Master of Science in Global Medicine"
authors_split2$Dept[which(authors_split2$Dept == "Religious Studies")] <- "Religion"
authors_split2$Dept[which(authors_split2$Dept == "Psychiatry")] <- "Psychiatry and Behavioral Sciences"
authors_split2$Dept[which(authors_split2$Dept == "Physiology")] <- "Physiology and Neuroscience"
authors_split2$Dept[which(authors_split2$Dept == "Physics")] <- "Physics and Astronomy"
authors_split2$Dept[which(authors_split2$Dept == "Pharmacy/Healthcare Decision Analysis")] <- "Healthcare Decision Analysis" # pharmacy
authors_split2$Dept[which(authors_split2$Div == "Chan Division of Occupational Science and Occupational Therapy")] <- "Chan Division of Occupational Science and Occupational Therapy"
authors_split2$Dept[which(authors_split2$Dept == "Occupational Science")] <- "Chan Division of Occupational Science and Occupational Therapy"
authors_split2$Dept[which(authors_split2$Dept == "Materials Engineering")] <- "Chemical Engineering and Materials Science"
authors_split2$Dept[which(authors_split2$Dept == "Latinx and Latin American Studies")] <- "Latinx and Latin American Studies Center"
authors_split2$Dept[which(authors_split2$Dept == "Health and the Human Sciences")] <- "Sociology"
authors_split2$Dept[which(authors_split2$Dept == "Social Work - On Line")] <- "Social Work"
authors_split2$Dept[which(authors_split2$Dept == "Dworak-Peck Social Work On-campus Program")] <- "Social Work"
authors_split2$Dept[which(authors_split2$Dept == "Aging Services Management - On Line")] <- "Agign Services Management"
authors_split2$Dept[grep("^Medicine ", authors_split2$Dept)] <- "Medicine"
authors_split2$Dept[grep("^Radiology -", authors_split2$Dept)] <- "Radiology"
authors_split2$Dept[grep("^Neurosurgery -", authors_split2$Dept)] <- "Neurosurgery"
authors_split2$Dept[grep("^Business Administration", authors_split2$Dept)] <- "Business Administration"
authors_split2$Dept[grep("WorkWell Center", authors_split2$Dept)] <- "WorkWell Center"

# new dept not in spreadsheet
authors_split2$Dept[which(authors_split2$Dept == "Graduate Program in the Biology of Aging")] <- "Biology of Aging" # gerontology
authors_split2$Dept[which(authors_split2$Dept == "Specialty")] <- "Specialty Pharmacy" # gerontology


# change div
authors_split2$Div[which(authors_split2$Div == "Verdugo Hills Hospital")] <- "Keck Medicine of USC"
authors_split2$Div[which(authors_split2$Div == "Shoah Foundation")] <- "Other"
authors_split2$Div[which(authors_split2$Div == "Division of Biokinesiology and Physical Therapy")] <- "Herman Ostrow School of Dentistry of USC"
authors_split2$Div[which(authors_split2$Div == "Chan Division of Occupational Science and Occupational Therapy")] <- "Herman Ostrow School of Dentistry of USC"
authors_split2$Div[which(authors_split2$Div == "Health Systems Operations (Level 5)")] <- "Keck Medicine of USC"
authors_split2$Div[which(authors_split2$Div == "WorkWell Center")] <- "Human Resources, Equity and Compliance"

# assign div according to dept (not in spreadsheet)
# finance -> marshall

authors_split2$Div[which(authors_split2$Dept == "Technology Innovation and Entrepreneurship")] <- "Viterbi School of Engineering"
authors_split2$Div[which(authors_split2$Dept == "Biology of Aging")] <- "Leonard Davis School of Gerontology"
authors_split2$Div[which(authors_split2$Dept == "Sustainable Policy and Planning")] <- "Price School of Public Policy"
authors_split2$Div[which(authors_split2$Dept == "School of Communication")] <- "Annenberg School for Communication and Journalism"
authors_split2$Div[which(authors_split2$Dept == "School of Journalism")] <- "Annenberg School for Communication and Journalism"
authors_split2$Div[which(authors_split2$Dept == "Molecular Biology")] <- "Dornsife College of Letters Arts and Sciences"
authors_split2$Div[grep("^Law", authors_split2$Dept)] <- "Gould School of Law"
authors_split2$Div[which(authors_split2$Dept == "Healthcare Decision Analysis")] <- "Alfred E. Mann School of Pharmacy and Pharmaceutical Sciences"
authors_split2$Div[which(authors_split2$Dept == "Health Promotion and Disease Prevention Studies")] <- "Keck Medicine of USC"
authors_split2$Div[which(authors_split2$Dept == "Arts, Technology and the Business of Innovation")] <- "Iovine and Young Academy"
authors_split2$Div[which(authors_split2$Dept == "Finance")] <- "Marshall School of Business"
authors_split2$Div[which(authors_split2$Dept == "Business Administration")] <- "Marshall School of Business"
authors_split2$Div[which(authors_split2$Dept == "Business Analytics")] <- "Marshall School of Business"

authors_split2 %>%
  distinct() -> authors_split2




author_dept <- merge(authors_split2, usc_departments_other,
                     by.x = c("Dept", "Div"),
                     by.y = c("Department", "Division"),
                     all.x = TRUE)
author_dept %>%
  mutate(DeptDivExists = ifelse(is.na(id), FALSE, TRUE)) %>%
  mutate(IncorrectDiv = ifelse(DeptDivExists == FALSE & Dept %in% usc_departments_other$Department, TRUE, FALSE)) %>%
  filter(IncorrectDiv != TRUE) %>%
  select(-DeptDivExists, -IncorrectDiv) -> author_dept2

author_dept2 %>%
  filter(is.na(id)) %>%
  select(Dept, Div) %>%
  distinct() -> dept_to_fix

unique(author_dept2$Dept[grep("Lab",author_dept2$Dept)])
# authors_lab <- unique(author_dept2$authorID[grep("Lab",author_dept2$Dept)])
# authors_lab_idx <- which(author_dept2$authorID %in% authors_lab)
# author_dept2[authors_lab_idx,] %>% View
# lab -> other
author_dept2[grep("Lab", author_dept2$Dept),]$Dept <- "Other"
# school has dept + other -> remove other
author_dept2 %>%
  group_by(authorID, Div) %>%
  add_count() %>%
  mutate(hasOther = grepl("Other", Dept)) %>% 
  filter(!(n > 1 & hasOther == TRUE)) %>% 
  select(-n, -hasOther) -> author_dept3

write.csv(author_dept3,
          "data_processed/author_dept_11_6_23.csv",
          row.names = FALSE)
