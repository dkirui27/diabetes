rm(list = ls(all = TRUE))
install.packages("bestglm")
install.packages("pROC")
install.packages("leaps")
install.packages("car")
library(xtable)
library(bestglm)
library(pROC)
library(plyr)
library(dplyr)
library(ggplot2)
library(leaps)
library(glmnet)
library(car)
library(stargazer)


readmission_data <- read.csv("readmission.csv") #clean data
str(readmission_data) #all categorical variables are correctly identified as factors
summary(readmission_data)
table(readmission_data$diag1_mod)
sum(is.na(readmission_data)) #no NAs
names(readmission_data)
table(readmission_data$diag1_mod)
table(readmission_data$readmitted)
table(as.numeric(readmission_data$readmitted)) #getting real values for categories
readmission_data$recode_readmitted <- c()
readmission_data$recode_readmitted <- readmission_data$readmitted #creating duplicate readmitted variable
table(readmission_data$recode_readmitted)
levels(readmission_data$recode_readmitted) <- c(1,0,0) #recoding readmitted (1 = readmitted < 30 days; 0 = not readmitted < 30 days)
table(readmission_data$recode_readmitted)

summary(readmission_data$recode_readmitted)
names(readmission_data)

unique(readmission_data$patient_nbr)

ifelse(duplicated(readmission_data$patient_nbr, incomparables = FALSE),1,0)

duplicated(readmission_data$patient_nbr, incomparables = FALSE) #identifying the duplicate values
sum(duplicated(readmission_data$patient_nbr, incomparables = FALSE)) #30248 observations that are duplicated

readmission_data$duplicate <- c()
readmission_data$duplicate <-ifelse(duplicated(readmission_data$patient_nbr,
                                               incomparables = FALSE),1,0) #creating a dummy for patients with more than one hospital visit - 1 = >1 observation
names(readmission_data)

sum(readmission_data$duplicate)


distinct <- readmission_data %>%
  filter(duplicate == 1) %>%
  select(patient_nbr,duplicate) %>%
  arrange(desc(patient_nbr)) %>%
  distinct(patient_nbr)

nrow(distinct) #number of patients who visited the hospital more than once


names(readmission_data)

unique(readmission_data$diag1_mod) #unique primary diagnoses
unique(readmission_data$diag2_mod) #unique secondary diagnoses
unique(readmission_data$diag3_mod) #unique tertiary diagnoses

summary(readmission_data)
summary(readmission_data$diag1_mod)
class(readmission_data$diag1_mod)


class(readmission_data$diag1_mod)
#readmission_data$diag2_mod<-as.character(readmission_data$diag2_mod) 
readmission_data$diag1_name <- c()
readmission_data$diag1_name <- ifelse(as.character(readmission_data$diag1_mod) >=0 & as.character(readmission_data$diag1_mod) <= 139, "infect/para", 
                                      ifelse(as.character(readmission_data$diag1_mod) >= 140 & as.character(readmission_data$diag1_mod) <= 239, "neoplasms", 
                                             ifelse(as.character(readmission_data$diag1_mod) >= 240 & as.character(readmission_data$diag1_mod) <= 279, "endocrine, nut & meta, immunity",
                                                    ifelse(as.character(readmission_data$diag1_mod) >= 280 & as.character(readmission_data$diag1_mod) <= 289, "blood & blood-forming organs",
                                                           ifelse(as.character(readmission_data$diag1_mod) >= 290 & as.character(readmission_data$diag1_mod) <= 319, "mental disorders", 
                                                                  ifelse(as.character(readmission_data$diag1_mod) >= 320 & as.character(readmission_data$diag1_mod) <= 359, "nervous system", 
                                                                         ifelse(as.character(readmission_data$diag1_mod) >= 360 & as.character(readmission_data$diag1_mod) <= 389, "sense organs", 
                                                                                ifelse(as.character(readmission_data$diag1_mod) >= 390 & as.character(readmission_data$diag1_mod) <= 459, "circulatory system", 
                                                                                       ifelse(as.character(readmission_data$diag1_mod) >= 460 & as.character(readmission_data$diag1_mod) <= 519, "respiratory system", 
                                                                                              ifelse(as.character(readmission_data$diag1_mod) >= 520 & as.character(readmission_data$diag1_mod) <= 579, "digestive system", 
                                                                                                     ifelse(as.character(readmission_data$diag1_mod) >= 580 & as.character(readmission_data$diag1_mod) <= 629, "genitourinary system", 
                                                                                                            ifelse(as.character(readmission_data$diag1_mod) >= 630 & as.character(readmission_data$diag1_mod) <= 679, "pregnancy, childbirth, & puerperium",
                                                                                                                   ifelse(as.character(readmission_data$diag1_mod) >= 680 & as.character(readmission_data$diag1_mod) <= 709, "skin & subcutaneous tissue",
                                                                                                                          ifelse(as.character(readmission_data$diag1_mod) >= 710 & as.character(readmission_data$diag1_mod) <= 739, "musculoskeletal system & connective tissue",
                                                                                                                                 ifelse(as.character(readmission_data$diag1_mod) >= 740 & as.character(readmission_data$diag1_mod) <= 759, "congenital anomalies",
                                                                                                                                        ifelse(as.character(readmission_data$diag1_mod) >= 760 & as.character(readmission_data$diag1_mod) <= 779, "perinatal period",
                                                                                                                                               ifelse(as.character(readmission_data$diag1_mod) >= 780 & as.character(readmission_data$diag1_mod) <= 799, "symptoms, signs, & ill-defined conditions",
                                                                                                                                                      ifelse(as.character(readmission_data$diag1_mod) >= 800 & as.character(readmission_data$diag1_mod) <= 999, "injury and poisoning",
                                                                                                                                                             ifelse(as.character(readmission_data$diag1_mod) == "V45", "external causes & supplemental",  "Other")))))))))))))))))))

readmission_data$diag1_name <-as.factor(readmission_data$diag1_name)

summary(readmission_data$diag1_name)
summary(readmission_data$diag1_mod)
table(readmission_data$diag1_name)

print(levels(readmission_data$diag1_name))
summary(readmission_data$diag1_name)
summary(readmission_data)

summary(readmission_data)

                                                      


#readmission_data$diag2_mod<-as.character(readmission_data$diag2_mod) 
readmission_data$diag2_name <- c()
readmission_data$diag2_name <- ifelse(as.character(readmission_data$diag2_mod) >=0 & as.character(readmission_data$diag2_mod) <= 139, "infect/para", 
                               ifelse(as.character(readmission_data$diag2_mod) >= 140 & as.character(readmission_data$diag2_mod) <= 239, "neoplasms", 
                               ifelse(as.character(readmission_data$diag2_mod) >= 240 & as.character(readmission_data$diag2_mod) <= 279, "endocrine, nut & meta, immunity",
                               ifelse(as.character(readmission_data$diag2_mod) >= 280 & as.character(readmission_data$diag2_mod) <= 289, "blood & blood-forming organs",
                               ifelse(as.character(readmission_data$diag2_mod) >= 290 & as.character(readmission_data$diag2_mod) <= 319, "mental disorders", 
                               ifelse(as.character(readmission_data$diag2_mod) >= 320 & as.character(readmission_data$diag2_mod) <= 359, "nervous system", 
                               ifelse(as.character(readmission_data$diag2_mod) >= 360 & as.character(readmission_data$diag2_mod) <= 389, "sense organs", 
                               ifelse(as.character(readmission_data$diag2_mod) >= 390 & as.character(readmission_data$diag2_mod) <= 459, "circulatory system", 
                               ifelse(as.character(readmission_data$diag2_mod) >= 460 & as.character(readmission_data$diag2_mod) <= 519, "respiratory system", 
                               ifelse(as.character(readmission_data$diag2_mod) >= 520 & as.character(readmission_data$diag2_mod) <= 579, "digestive system", 
                               ifelse(as.character(readmission_data$diag2_mod) >= 580 & as.character(readmission_data$diag2_mod) <= 629, "genitourinary system", 
                               ifelse(as.character(readmission_data$diag2_mod) >= 630 & as.character(readmission_data$diag2_mod) <= 679, "pregnancy, childbirth, & puerperium",
                               ifelse(as.character(readmission_data$diag2_mod) >= 680 & as.character(readmission_data$diag2_mod) <= 709, "skin & subcutaneous tissue",
                               ifelse(as.character(readmission_data$diag2_mod) >= 710 & as.character(readmission_data$diag2_mod) <= 739, "musculoskeletal system & connective tissue",
                               ifelse(as.character(readmission_data$diag2_mod) >= 740 & as.character(readmission_data$diag2_mod) <= 759, "congenital anomalies",
                               ifelse(as.character(readmission_data$diag2_mod) >= 760 & as.character(readmission_data$diag2_mod) <= 779, "perinatal period",
                               ifelse(as.character(readmission_data$diag2_mod) >= 780 & as.character(readmission_data$diag2_mod) <= 799, "symptoms, signs, & ill-defined conditions",
                               ifelse(as.character(readmission_data$diag2_mod) >= 800 & as.character(readmission_data$diag2_mod) <= 999, "injury and poisoning",
                               ifelse(as.character(readmission_data$diag2_mod) == "V45", "external causes & supplemental", "Other")))))))))))))))))))
readmission_data$diag2_name <-as.factor(readmission_data$diag2_name) 
summary(readmission_data$diag2_name)
summary(readmission_data$diag2_mod)


readmission_data$diag3_name <- c()
readmission_data$diag3_mod <- revalue(readmission_data$diag3_mod, c("?"="Other"))
readmission_data$diag3_name <- ifelse(as.character(readmission_data$diag3_mod) >=0 & as.character(readmission_data$diag3_mod) <= 139, "infect/para", 
                                      ifelse(as.character(readmission_data$diag3_mod) >= 140 & as.character(readmission_data$diag3_mod) <= 239, "neoplasms", 
                                             ifelse(as.character(readmission_data$diag3_mod) >= 240 & as.character(readmission_data$diag3_mod) <= 279, "endocrine, nut & meta, immunity",
                                                    ifelse(as.character(readmission_data$diag3_mod) >= 280 & as.character(readmission_data$diag3_mod) <= 289, "blood & blood-forming organs",
                                                           ifelse(as.character(readmission_data$diag3_mod) >= 290 & as.character(readmission_data$diag3_mod) <= 319, "mental disorders", 
                                                                  ifelse(as.character(readmission_data$diag3_mod) >= 320 & as.character(readmission_data$diag3_mod) <= 359, "nervous system", 
                                                                         ifelse(as.character(readmission_data$diag3_mod) >= 360 & as.character(readmission_data$diag3_mod) <= 389, "sense organs", 
                                                                                ifelse(as.character(readmission_data$diag3_mod) >= 390 & as.character(readmission_data$diag3_mod) <= 459, "circulatory system", 
                                                                                       ifelse(as.character(readmission_data$diag3_mod) >= 460 & as.character(readmission_data$diag3_mod) <= 519, "respiratory system", 
                                                                                              ifelse(as.character(readmission_data$diag3_mod) >= 520 & as.character(readmission_data$diag3_mod) <= 579, "digestive system", 
                                                                                                     ifelse(as.character(readmission_data$diag3_mod) >= 580 & as.character(readmission_data$diag3_mod) <= 629, "genitourinary system", 
                                                                                                            ifelse(as.character(readmission_data$diag3_mod) >= 630 & as.character(readmission_data$diag3_mod) <= 679, "pregnancy, childbirth, & puerperium",
                                                                                                                   ifelse(as.character(readmission_data$diag3_mod) >= 680 & as.character(readmission_data$diag3_mod) <= 709, "skin & subcutaneous tissue",
                                                                                                                          ifelse(as.character(readmission_data$diag3_mod) >= 710 & as.character(readmission_data$diag3_mod) <= 739, "musculoskeletal system & connective tissue",
                                                                                                                                 ifelse(as.character(readmission_data$diag3_mod) >= 740 & as.character(readmission_data$diag3_mod) <= 759, "congenital anomalies",
                                                                                                                                        ifelse(as.character(readmission_data$diag3_mod) >= 760 & as.character(readmission_data$diag3_mod) <= 779, "perinatal period",
                                                                                                                                               ifelse(as.character(readmission_data$diag3_mod) >= 780 & as.character(readmission_data$diag3_mod) <= 799, "symptoms, signs, & ill-defined conditions",
                                                                                                                                                      ifelse(as.character(readmission_data$diag3_mod) >= 800 & as.character(readmission_data$diag3_mod) <= 999, "injury and poisoning",
                                                                                                                                                             ifelse(as.character(readmission_data$diag3_mod) == "V45", "external causes & supplemental", "Other" )))))))))))))))))))
readmission_data$diag3_name <-as.factor(readmission_data$diag3_name)
summary(readmission_data$diag3_name)
summary(readmission_data$diag3_mod)
summary(readmission_data)


###cleaning RACE, reference category is OTHER
levels(readmission_data$race)
NA_race <- which(readmission_data$race == "?") #identifying the sissings
readmission_data <- readmission_data[-NA_race,] #removing the missings
table(readmission_data$race)
readmission_data$race <- droplevels(readmission_data$race)
table(readmission_data$race)
readmission_data$race <- revalue(readmission_data$race, c("AfricanAmerican"="African-American", #changing level names
                                 "Asian"="Asian", "Caucasian"="Caucasian", "Hispanic"="Hispanic", 
                                 "Other"="Other"))
readmission_data$race <- relevel(readmission_data$race, "Other")  #changing reference category to Other

summary(readmission_data)
table(readmission_data$race)

### cleaning GENDER, reference category is MALE
levels(readmission_data$gender)
summary(readmission_data$gender)
NA_gender <- which(readmission_data$gender == "Unknown/Invalid")
readmission_data <- readmission_data[-NA_gender,]
summary(readmission_data$gender)
readmission_data$gender <- droplevels(readmission_data$gender)
## releveling gender
readmission_data$gender <- relevel(readmission_data$gender, "Male","Female")  #reference category is MALE
table(readmission_data$gender)

#releveling max_glu_serum and creating recode_max_glu_serum #reference category for max_glu_serum is NONE, ref for recode is Not Tested
table(readmission_data$max_glu_serum)
print(levels(readmission_data$max_glu_serum))
readmission_data$max_glu_serum <- factor(readmission_data$max_glu_serum,levels(readmission_data$max_glu_serum)[c(3,4,1,2)]) #releveling 
table(readmission_data$max_glu_serum)
readmission_data$recode_max_glu_serum <- c()
readmission_data$recode_max_glu_serum <- revalue(readmission_data$max_glu_serum, c(">200"="Elevated",">300"="Elevated","None"="Not Tested", "Norm"="Normal")) #collapsed >7 and >8 into "Elevated" category
table(readmission_data$recode_max_glu_serum)

### Cleaning and releveling A1Cresult and created a recoded A1Cresult and releveling recode_A1Cresult
class(readmission_data$A1Cresult)
table(readmission_data$A1Cresult)
readmission_data$recode_A1Cresult <- c()
readmission_data$recode_A1Cresult <- revalue(readmission_data$A1Cresult, c(">7"="Elevated",">8"="Elevated","None"="Not Tested", "Norm"="Normal")) #collapsed >7 and >8 into "Elevated" category
table(readmission_data$recode_A1Cresult)
print(levels(readmission_data$A1Cresult))
readmission_data$A1Cresult <- factor(readmission_data$A1Cresult,levels(readmission_data$A1Cresult)[c(3,4,1,2)]) #releveling 
print(levels(readmission_data$recode_A1Cresult))
readmission_data$recode_A1Cresult <- factor(readmission_data$recode_A1Cresult,levels(readmission_data$recode_A1Cresult)[c(2,3,1)]) #releveling
table(readmission_data$recode_A1Cresult)

### creating recode_metformin
names(readmission_data)
table(readmission_data$metformin)
levels(readmission_data$metformin) 
readmission_data$recode_metformin <- c()
readmission_data$recode_metformin <- revalue(readmission_data$metformin, c("Down"="Yes", "No"="No",
                                                          "Steady"="Yes", "Up"="Yes"))
readmission_data$recode_metformin <- relevel(readmission_data$recode_metformin, "No")

###creating recode_glimepiride
readmission_data$recode_glimepiride <- c()
readmission_data$recode_glimepiride <- revalue(readmission_data$glimepiride, c("Down"="Yes", "No"="No",
                                                                           "Steady"="Yes", "Up"="Yes"))
readmission_data$recode_glimepiride <- relevel(readmission_data$recode_glimepiride, "No")
table(readmission_data$recode_glimepiride)

###creating recode_glipizide
readmission_data$recode_glipizide <- c()
readmission_data$recode_glipizide <- revalue(readmission_data$glipizide, c("Down"="Yes", "No"="No",
                                                                               "Steady"="Yes", "Up"="Yes"))
readmission_data$recode_glipizide <- relevel(readmission_data$recode_glipizide, "No")
table(readmission_data$recode_glipizide)

###creating recode_glyburide
readmission_data$recode_glyburide <- c()
readmission_data$recode_glyburide <- revalue(readmission_data$glyburide, c("Down"="Yes", "No"="No",
                                                                           "Steady"="Yes", "Up"="Yes"))
readmission_data$recode_glyburide <- relevel(readmission_data$recode_glyburide, "No")
table(readmission_data$recode_glyburide)

###creating recode_pioglitazone
readmission_data$recode_pioglitazone <- c()
readmission_data$recode_pioglitazone <- revalue(readmission_data$pioglitazone, c("Down"="Yes", "No"="No",
                                                                           "Steady"="Yes", "Up"="Yes"))
readmission_data$recode_pioglitazone <- relevel(readmission_data$recode_pioglitazone, "No")
table(readmission_data$recode_pioglitazone)

###creating recode_rosiglitazone
readmission_data$recode_rosiglitazone <- c()
readmission_data$recode_rosiglitazone <- revalue(readmission_data$rosiglitazone, c("Down"="Yes", "No"="No",
                                                                                 "Steady"="Yes", "Up"="Yes"))
readmission_data$recode_rosiglitazone <- relevel(readmission_data$recode_rosiglitazone, "No")
table(readmission_data$recode_rosiglitazone)

###relevling insulin
readmission_data$insulin <- relevel(readmission_data$insulin, "No","Down","Steady","Up")
table(readmission_data$insulin)

###releveling change
table(readmission_data$change)
readmission_data$change <- revalue(readmission_data$change, c("Ch"="Yes", "No"="No"))
readmission_data$change <- relevel(readmission_data$change, "No")

#releveling disch_disp_modified
table(readmission_data$disch_disp_modified)
readmission_data$disch_disp_modified <- relevel(readmission_data$disch_disp_modified, "Other")

#releveling adm_src_mod
table(readmission_data$adm_src_mod)
readmission_data$adm_src_mod <- relevel(readmission_data$adm_src_mod, "Other")

#releveling adm_typ_mod
table(readmission_data$adm_typ_mod)
readmission_data$adm_typ_mod <- relevel(readmission_data$adm_typ_mod, "Other")

#releveling recode_readmitted No = not readmitted w/in 30 days, Yes = readmitted w/in 30 days
table(readmission_data$recode_readmitted)
readmission_data$recode_readmitted <- revalue(readmission_data$recode_readmitted, c("1"="Yes", "0"="No"))
readmission_data$recode_readmitted <- relevel(readmission_data$recode_readmitted, "No")

names(readmission_data)

summary(readmission_data$diag1_mod)

table(readmission_data$diag1_mod)

summary(readmission_data$recode_readmitted)

readmission_data2 <- readmission_data[,-c(1,2,15:20,28:31,33,37:38)] #taking out encounter_id, patient_nbr, "metformin","glimepiride",
                                                                     #"glipizide", "glyburide","pioglitazone","rosiglitazone""diag1_mod"
                                                                     #"diag2_mod","diag3_mod","readmitted","duplicate",recode_A1Cresult, and recode_max_glu_serum 
                                                                     #(WITH ORIGINAL A1C and GLU variables) 

readmission_data3 <- readmission_data[,-c(1,2,13:20,28:31,33)]       #taking out encounter_id, patient_nbr, "metformin","glimepiride", A1Cresult, max_glu_serum
                                                                     #"glipizide", "glyburide","pioglitazone","rosiglitazone""diag1_mod"
                                                                     #"diag2_mod","diag3_mod","readmitted","duplicate", (WITH RECODED A1C and GLU VARIABLES)
names(readmission_data)
names(readmission_data2)
names(readmission_data3)
#### DEMOGRAPHIC DESCRIPTIVES ####

#Race:
ggplot(readmission_data, aes(x = race)) + geom_bar() #mostly white

#Gender
ggplot(readmission_data, aes(x = gender)) + geom_bar() #slightly more women than men
table(readmission_data$gender)

#Length of Hospital Stay (in days)
ggplot(readmission_data, aes(x = "", y = time_in_hospital)) + geom_boxplot()
ggplot(readmission_data, aes(x = time_in_hospital)) + geom_bar() #right skewed (median < mean)
summary(readmission_data$time_in_hospital)

#Number of Lab Procedures
ggplot(readmission_data, aes(x = num_lab_procedures)) + geom_histogram(bins = 50)
summary(readmission_data$num_lab_procedures) #left skewed (median > mean)

#Number of Procedures
ggplot(readmission_data, aes(x = num_procedures)) + geom_histogram(bins = 7) 
summary(readmission_data$num_procedures) #right skewed (median < mean)

#Number of Medications
ggplot(readmission_data, aes(x = num_medications)) + geom_bar()
summary(readmission_data$num_medications) #right skewed (median < mean)

#Number of outpatient procedures
ggplot(readmission_data, aes(x =number_outpatient)) + geom_bar()
summary(readmission_data$number_outpatient)

#Number of Emergency Room Visits
ggplot(readmission_data, aes(x =number_emergency)) + geom_bar() #right skewed (median < mean)
summary(readmission_data$number_emergency)

#Number of Inpatient Visits
ggplot(readmission_data, aes(x = number_inpatient)) + geom_histogram(bins = 10)
summary(readmission_data$number_emergency) #right skewed (median < mean)

#Number of Diagnoses
ggplot(readmission_data, aes(x = number_diagnoses)) + geom_histogram(bins = 15)
summary(readmission_data$number_diagnoses) #left skewed (median > mean)

#Max Glu Serum #base category: not tested
ggplot(readmission_data, aes(x = max_glu_serum)) + geom_bar ()

#recode_max_glu_serum
ggplot(readmission_data, aes(x = recode_max_glu_serum)) + geom_bar()

#A1Cresult
ggplot(readmission_data, aes(x = A1Cresult)) + geom_bar()

#recode_A1Cresult
ggplot(readmission_data, aes(x = recode_A1Cresult)) + geom_bar()

#metformin 
ggplot(readmission_data, aes(x = recode_metformin)) + geom_bar()

names(readmission_data)

#glimepiride
ggplot(readmission_data, aes(x = recode_glimepiride)) + geom_bar ()

#glipizide 
ggplot(readmission_data, aes(x = recode_glipizide)) + geom_bar()

#pioglitazone
ggplot(readmission_data, aes(x = recode_pioglitazone)) + geom_bar()

#rosiglitazone
ggplot(readmission_data, aes(x = recode_rosiglitazone)) + geom_bar()

#insulin (a lot of people have insulin - duh)
ggplot(readmission_data, aes(x = insulin)) + geom_bar()

#change
ggplot(readmission_data, aes(x = change)) + geom_bar()

#diabetesMed
ggplot(readmission_data, aes(x = diabetesMed)) + geom_bar()


#disch_disp_modified
ggplot(readmission_data, aes(x = disch_disp_modified)) + geom_bar()


#adm_src_mod
ggplot(readmission_data, aes(x = adm_src_mod)) + geom_bar()

#adm_typ_mod
ggplot(readmission_data, aes(x = adm_typ_mod)) + geom_bar()

#age_mod 
ggplot(readmission_data, aes(x = age_mod)) + geom_bar()


#readmitted
ggplot(readmission_data, aes(x = readmitted)) + geom_bar()
ggplot(readmission_data, aes(x = recode_readmitted)) + geom_bar() #1 = readmitted w/in 30 days
table(readmission_data$recode_readmitted)



names(readmission_data)
summary(readmission_data$max_glu_serum)

##bivariate relationships

## number of procedures and readmission
ggplot(readmission_data) +
  geom_boxplot(aes(x = recode_readmitted, y = num_procedures, fill = recode_readmitted), alpha = .3) +
  theme_bw()

## number of emergency visits and readmission
ggplot(readmission_data) +
  geom_boxplot(aes(x = recode_readmitted, y = number_emergency, fill = recode_readmitted), alpha = .3) +
  theme_bw()

##number of inpatient visits and readmission
ggplot(readmission_data) +
  geom_boxplot(aes(x = recode_readmitted, y = number_inpatient, fill = recode_readmitted), alpha = .3) +
  theme_bw()

#number of diagnoses and readmission
ggplot(readmission_data) +
  geom_boxplot(aes(x = recode_readmitted, y = number_diagnoses, fill = recode_readmitted), alpha = .3) +
  theme_bw()

##number of medications and readmission
ggplot(readmission_data) +
  geom_boxplot(aes(x = recode_readmitted, y = num_medications, fill = recode_readmitted), alpha = .3) +
  theme_bw()


###diabetes med and readmission
test2 <- as.data.frame(table(readmission_data$diabetesMed, readmission_data$recode_readmitted))
ggplot(test2, aes(fill= Var1, y = Freq, x=Var2)) + 
  geom_bar(position="fill", stat="identity")

##insuln and readmission
test <- as.data.frame(table(readmission_data$insulin, readmission_data$recode_readmitted))
ggplot(test, aes(fill= Var1, y = Freq , x= Var2)) + 
  geom_bar(position="fill", stat="identity")

#Dispatched Type and readmission (something here)
test3 <- as.data.frame(table(readmission_data$disch_disp_modified, readmission_data$recode_readmitted))
ggplot(test3, aes(fill= Var1, y = Freq , x= Var2)) + 
  geom_bar(position="fill", stat="identity") +
  theme_bw()

#Primary Diagnosis and readmission
test4 <- as.data.frame(table(readmission_data$diag1_name, readmission_data$recode_readmitted))
ggplot(test4, aes(fill= Var1, y = Freq , x= Var2)) + 
  geom_bar(position="fill", stat="identity") 
  
#scatterplot matrix of the continuous variables
colnames(readmission_data)
pairs(readmission_data[c(7,8,10,11, 12)], panel = panel.smooth)
cor(readmission_data[c(7,8,10,11, 12)])

### inpatient x emergency
ggplot(readmission_data2, aes(x = number_emergency, y = number_inpatient, color = diag1_name)) +
  geom_point()

colnames(readmission_data)

#use this fig for proposal time in hospital and readmission
ggplot(readmission_data, aes(x = time_in_hospital, fill = recode_readmitted)) +
  geom_density(position = "stack") +
  xlim(0, 16) 


age <- as.data.frame(table(readmission_data$recode_readmitted, readmission_data$age_mod))
names(age) <- c("Readmitted","Age_Group", "Frequency")
##age and readmission
ggplot(age, aes(fill = Readmitted, y = Frequency, x = Age_Group)) + 
  geom_bar(aes(fill = Readmitted), position="dodge", stat="identity")

#### MODEL SELECTION ####

###DO DATA SPLITS FIRST
N <- length(readmission_data2$recode_readmitted)
N
set.seed(10) 
index.train <- sample(N, (N/2))
data.train <- readmission_data2[index.train,] # Set the N/2 randomly chosen subjects as a training data
data.test <- readmission_data2[-index.train,] # The remaining subjects will be reserved for testing purposes.

#### LASSO ####

X <- model.matrix(recode_readmitted~., data.train)[,-1]
dim(X)
colnames(X)
names(data.train)
Y <- data.train[,20]  #col 20 in data2, 18 in data3

set.seed(10)  
##### with type.measure = deviance ####  seems like deviance is the best measure of the three
fit1.cv <- cv.glmnet(X,Y, alpha=1, family="binomial", nfolds = 10, type.measure = "deviance")
plot(fit1.cv)

names(fit1.cv)
fit1.cv$name

#using lambda.1se:
coef.1se <- coef(fit1.cv, s= "lambda.1se")
coef.1se <- coef.1se[which(coef.1se !=0),]
coef.1se
rownames(as.matrix(coef.1se))

#using lambda.min:
coef.min <-coef(fit1.cv, s="lambda.min") 
coef.min <- coef.min[which(coef.min !=0), ]

as.matrix(coef.min)
rownames(as.matrix(coef.min))

#### with type.measure = AUC ####
fit2.cv <- cv.glmnet(X,Y, alpha=1, family="binomial", nfolds = 10, type.measure = "auc")
plot(fit2.cv)

names(fit2.cv)
fit2.cv$name

#using lambda.1se:
coef.1se <- coef(fit2.cv, s= "lambda.1se")
coef.1se <- coef.1se[which(coef.1se !=0),]
coef.1se
rownames(as.matrix(coef.1se))

#using lambda.min:
coef.min <-coef(fit2.cv, s="lambda.min") 
coef.min <- coef.min[which(coef.min !=0), ]

as.matrix(coef.min)
rownames(as.matrix(coef.min))

##### with type.measure = class ####
fit3.cv <- cv.glmnet(X,Y, alpha=1, family="binomial", nfolds = 10, type.measure = "class") #misclassification error
plot(fit3.cv)

names(fit3.cv)
fit3.cv$name

#using lambda.1se:
coef.1se <- coef(fit3.cv, s= "lambda.1se")
coef.1se <- coef.1se[which(coef.1se !=0),]
coef.1se
rownames(as.matrix(coef.1se))

#using lambda.min:
coef.min <-coef(fit3.cv, s="lambda.min") 
coef.min <- coef.min[which(coef.min !=0), ]

as.matrix(coef.min)
rownames(as.matrix(coef.min))

#Final LASSO Model (Use Deviance + lambda.min)
names(readmission_data2)
beta.min <- rownames(as.matrix(coef.min)) 

fit1.train <- glm(recode_readmitted ~ race + time_in_hospital + num_procedures + num_medications + number_emergency + number_inpatient + number_diagnoses + 
              max_glu_serum + A1Cresult + insulin + diabetesMed + disch_disp_modified + adm_src_mod + adm_typ_mod + age_mod + diag1_name + diag2_name +
              diag3_name + recode_metformin + recode_glimepiride + recode_glipizide + recode_glyburide + recode_rosiglitazone, data.train, family=binomial(logit)) 
summary(fit1.train)
Anova(fit1.train)
fit1.roc <-roc(data.train$recode_readmitted, fit1.train$fitted.values, plot=T, col="blue")
fit1.roc$auc #Area under the curve: 0.6528

#Manual Backward Selection

#1) dropping race
fit2.train <- glm(recode_readmitted ~ time_in_hospital + num_procedures + num_medications + number_emergency + number_inpatient + number_diagnoses + 
              max_glu_serum + A1Cresult + insulin + diabetesMed + disch_disp_modified + adm_src_mod + adm_typ_mod + age_mod + diag1_name + diag2_name +
              diag3_name + recode_metformin + recode_glimepiride + recode_glipizide + recode_glyburide + recode_rosiglitazone, data.train, family=binomial(logit)) 
summary(fit2.train)
Anova(fit2.train)
fit2.roc <-roc(data.train$recode_readmitted, fit2.train$fitted.values, plot=T, col="blue")
fit2.roc$auc #Area under the curve: 0.6528

#2) dropping diag2_name
fit3.train <- glm(recode_readmitted ~ time_in_hospital + num_procedures + num_medications + number_emergency + number_inpatient + number_diagnoses + 
              max_glu_serum + A1Cresult + insulin + diabetesMed + disch_disp_modified + adm_src_mod + adm_typ_mod + age_mod + diag1_name + diag2_name +
              recode_metformin + recode_glimepiride + recode_glipizide + recode_glyburide + recode_rosiglitazone, data.train, family=binomial(logit)) 
summary(fit3.train)
Anova(fit3.train)
fit3.roc <-roc(data.train$recode_readmitted, fit3.train$fitted.values, plot=T, col="blue")
fit3.roc$auc #Area under the curve: 0.6521

#3) dropping recode_glipizide
fit4.train <- glm(recode_readmitted ~ time_in_hospital + num_procedures + num_medications + number_emergency + number_inpatient + number_diagnoses + 
              max_glu_serum + A1Cresult + insulin + diabetesMed + disch_disp_modified + adm_src_mod + adm_typ_mod + age_mod + diag1_name + diag2_name +
              recode_metformin + recode_glimepiride + recode_glyburide + recode_rosiglitazone, data.train, family=binomial(logit)) 
summary(fit4.train)
Anova(fit4.train)
fit4.roc <-roc(data.train$recode_readmitted, fit4.train$fitted.values, plot=T, col="blue")
fit4.roc$auc #Area under the curve:  0.652

#4) dropping A1C_result
fit5.train <- glm(recode_readmitted ~ time_in_hospital + num_procedures + num_medications + number_emergency + number_inpatient + number_diagnoses + 
              max_glu_serum + insulin + diabetesMed + disch_disp_modified + adm_src_mod + adm_typ_mod + age_mod + diag1_name + diag2_name +
              recode_metformin + recode_glimepiride + recode_glyburide + recode_rosiglitazone, data.train, family=binomial(logit)) 
summary(fit5.train)
Anova(fit5.train)
fit5.roc <-roc(data.train$recode_readmitted, fit5.train$fitted.values, plot=T, col="blue")
fit5.roc$auc #Area under the curve:  0.652

#5) dropping time_in_hospital
fit6.train <- glm(recode_readmitted ~ num_procedures + num_medications + number_emergency + number_inpatient + number_diagnoses + 
              max_glu_serum + insulin + diabetesMed + disch_disp_modified + adm_src_mod + adm_typ_mod + age_mod + diag1_name + diag2_name +
              recode_metformin + recode_glimepiride + recode_glyburide + recode_rosiglitazone, data.train, family=binomial(logit)) 
summary(fit6.train)
Anova(fit6.train)
fit6.roc <-roc(data.train$recode_readmitted, fit6.train$fitted.values, plot=T, col="blue")
fit6.roc$auc #Area under the curve: 0.6517


#6) dropping recode_glyburide
fit7.train <- glm(recode_readmitted ~ num_procedures + num_medications + number_emergency + number_inpatient + number_diagnoses + 
              max_glu_serum + insulin + diabetesMed + disch_disp_modified + adm_src_mod + adm_typ_mod + age_mod + diag1_name + diag2_name +
              recode_metformin + recode_glimepiride + recode_rosiglitazone, data.train, family=binomial(logit)) 
summary(fit7.train)
Anova(fit7.train)
fit7.roc <-roc(data.train$recode_readmitted, fit7.train$fitted.values, plot=T, col="blue")
fit7.roc$auc #Area under the curve: 0.6515

#7) dropping recode_rosiglitazone
fit8.train <- glm(recode_readmitted ~ num_procedures + num_medications + number_emergency + number_inpatient + number_diagnoses + 
              max_glu_serum + insulin + diabetesMed + disch_disp_modified + adm_src_mod + adm_typ_mod + age_mod + diag1_name + diag2_name +
              recode_metformin + recode_glimepiride, data.train, family=binomial(logit)) 
summary(fit8.train)
Anova(fit8.train)
fit8.roc <-roc(data.train$recode_readmitted, fit8.train$fitted.values, plot=T, col="blue")
fit8.roc$auc #Area under the curve: 0.6516

#8) dropping adm_typ_mod
fit9.train <- glm(recode_readmitted ~ num_procedures + num_medications + number_emergency + number_inpatient + number_diagnoses + 
              max_glu_serum + insulin + diabetesMed + disch_disp_modified + adm_src_mod + age_mod + diag1_name + diag2_name +
              recode_metformin + recode_glimepiride, data.train, family=binomial(logit)) 
summary(fit9.train)
Anova(fit9.train)
fit9.roc <-roc(data.train$recode_readmitted, fit9.train$fitted.values, plot=T, col="blue")
fit9.roc$auc #Area under the curve: 0.6512

#9) dropping recode_glimepiride
fit10.train <- glm(recode_readmitted ~ num_procedures + num_medications + number_emergency + number_inpatient + number_diagnoses + 
              max_glu_serum + insulin + diabetesMed + disch_disp_modified + adm_src_mod + age_mod + diag1_name + diag2_name +
              recode_metformin, data.train, family=binomial(logit)) 
summary(fit10.train)
Anova(fit10.train)
fit10.roc <-roc(data.train$recode_readmitted, fit10.train$fitted.values, plot=T, col="blue")
fit10.roc$auc #Area under the curve: 0.6509

#10) dropping max_glu_serum (**********FINAL MODEL*********)
fit11.train <- glm(recode_readmitted ~ num_procedures + num_medications + number_emergency + number_inpatient + number_diagnoses + 
               insulin + diabetesMed + disch_disp_modified + adm_src_mod + age_mod + diag1_name + diag2_name +
               recode_metformin, data.train, family=binomial(logit)) 
summary(fit11.train)
Anova(fit11.train)
fit11.roc <-roc(data.train$recode_readmitted, fit11.train$fitted.values, plot=T, col="blue")
fit11.roc$auc #Area under the curve: 0.6508

#11) dropping adm_src_mod (ALTERNATIVE FINAL MODEL 1)
fit12.train <- glm(recode_readmitted ~ num_procedures + num_medications + number_emergency + number_inpatient + number_diagnoses + 
               insulin + diabetesMed + disch_disp_modified + age_mod + diag1_name + diag2_name +
               recode_metformin, data.train, family=binomial(logit)) 
summary(fit12.train)
Anova(fit12.train)
fit12.roc <-roc(data.train$recode_readmitted, fit12.train$fitted.values, plot=T, col="blue")
fit12.roc$auc #Area under the curve: 0.6502

#12) dropping diag2_name (ALTERNATIVE FINAL MODEL 2)
fit13.train <- glm(recode_readmitted ~ num_procedures + num_medications + number_emergency + number_inpatient + number_diagnoses + 
               insulin + diabetesMed + disch_disp_modified + age_mod + diag1_name +
               recode_metformin, data.train, family=binomial(logit)) 
summary(fit13.train)
Anova(fit13.train)
fit13.roc <-roc(data.train$recode_readmitted, fit13.train$fitted.values, plot=T, col="blue")
fit13.roc$auc #Area under the curve: 0.6496

##FINAL MODEL AFTER BACKWARDS SELECTION IS FIT-11 WITH ALL VARIABLES SIGNIFICANT AT THE .05 LEVEL (USING ANOVA)
fit11.fitted.test <- predict(fit11.train, data.test) # fit11 prob
fit11.roc <- roc(data.test$recode_readmitted, fit11.fitted.test, plot=T, col="blue")  #first col is the truth, second are the fitted values
names(fit11.roc)
fit11.roc$auc #AUC: 0.6467 # area under the curve, the larger the better. an ROC curve whose area is 1 would be perfect

#### CLASSIFICATION ####  
 
#DETERMING THE THRESHOLD:
### An example: Suppose (a_{0,1}/a_{1,0})=1/2=.5, then 
#The threshold over the prob(Y=1|x) > .5/(1+.5)=.333 
threshold <- .5/(1+.5)
logit <- log(threshold/(1-threshold))
fit11.train.pred.33 <- rep("0", length(data.train$recode_readmitted)) # prediction step 1, 101762 people in the dataset, threshold is 2/3
fit11.train.pred.33[fit11.train$fitted > threshold] <- "1" # prediction step 2 to get a classifier, fitted value is computing a probability for each person
fit11.train.pred.33 <- as.factor(fit11.train.pred.33) #creating y hat
length(fit11.train.pred.33)
# c) Misclassification error= Mean(miss-classification)
length(fit11.train$fitted)
# We can get all three quantities through confusion matrix or directly find the 
# mis-classification errors

set.seed(10) # be able to reproduce the following result.

cm.33 <- table(fit11.train.pred.33,data.train$recode_readmitted) # confusion matrix: 
cm.33

sensitivity <- cm.33[2,2]/sum(data.train$recode_readmitted == "Yes")
sensitivity

specificity <- cm.33[1,1]/ sum(data.train$recode_readmitted == "No")
false.positive <- cm.33[2,1]/sum(data.train$recode_readmitted == "No")  # 5/1095
false.positive

### Mis-classification error (MCE): she labels it as training error because that's what we're doing
error.training <- (cm.33[1,2]+cm.33[2,1])/length(fit11.train.pred.33) # training error
error.training #0.1147228


plot(1-fit11.roc$specificities, fit11.roc$thresholds, col="green", pch=16,  
     xlab="False Positive",
     ylab="Threshold on prob")


#   e) Positive Prediction
#   f) Negative Prediction 

# Positive Prediction = P( Positive | Classified as Positive)
# Negative Prediction = P( Negative | Classified as Negative)

positive.pred <- cm.33[2, 2] / (cm.33[2, 1] + cm.33[2, 2])
positive.pred

negative.pred <- cm.33[1, 1] / (cm.33[1, 1] + cm.33[1, 2])
negative.pred

### Finally we get the weighted mis-classification error
# MCE=(a10 sum(y != hat y|y=1) + a01 sum(y != hat y|y=0))/n

# Get the classes first
fit11.train.pred.bayes=rep("0", length(data.train$recode_readmitted))
fit11.train.pred.bayes[fit11.train$fitted > threshold]="1" #fitted value is a probability
MCE.bayes=(sum(2*(fit11.train.pred.bayes[data.train$recode_readmitted == "Yes"] != "1")) 
           + sum(fit11.train.pred.bayes[data.train$recode_readmitted == "No"] != "0"))/length(data.train$recode_readmitted)
MCE.bayes #0.2220882

fit11.test <- glm(recode_readmitted ~ num_procedures + num_medications + number_emergency + number_inpatient + number_diagnoses + 
                    insulin + diabetesMed + disch_disp_modified + age_mod + diag1_name +
                    recode_metformin, data.test, family=binomial(logit)) 

summary(fit11.test) #FINAL MODEL
exp(coefficients(fit11.test))
fit11.roc <- roc(data.test$recode_readmitted, fit11.test$fitted.values, plot=T, col="blue") 
fit11.roc$auc

stargazer(fit11.test, type = "text")
stargazer(fit11.test, type = "text",
          apply.coef = exp,
          apply.se = exp)

