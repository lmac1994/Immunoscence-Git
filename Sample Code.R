ddfdfd
###Set up working directory, load packages
setwd("C:/Users/lmacconn/OneDrive - Umich/Desktop")
install.packages("haven")
library(haven)

##Reading VBS, LHS, and RAND data from SAS format (No LBS)
vbs_data <- read_sav("hrs2016vbs.sav")
vbsfcyto_data <- read_sas("flocyt2016.sas7bdat")
vbsimmune <- read_sas("vbs16aa.sas7bdat")
lhs_data <- read_sas("lhms1517a_r.sas7bdat")
hrs_data <- read_sas("randhrs1992_2018v1.sas7bdat")

##Converting to dataframes
vbs = data.frame(vbs_data)
vbsfcyto = data.frame(vbsfcyto_data)
lhs = data.frame(lhs_data)
hrs = data.frame(hrs_data)
vbs_sup= data.frame(vbsimmune)

###Extracting HRS LBS data (2012-2014 only)
install.packages("SAScii")
library(SAScii)

fn12 <-  "C:/Users/lmacconn/OneDrive - Umich/Desktop/H12LB_R.da"
sas.input12 <-  "C:/Users/lmacconn/OneDrive - Umich/Desktop/H12LB_R.sas"
hrs12 <- read.SAScii(fn12, sas.input12)


fn14 <-  "C:/Users/lmacconn/OneDrive - Umich/Desktop/H14LB_R.da"
sas.input14 <-  "C:/Users/lmacconn/OneDrive - Umich/Desktop/H14LB_R.sas"
hrs14 <- read.SAScii(fn14, sas.input14)

###Read in tracker file
tracker <- read_sas("trk2020tr_r.sas7bdat")

###Select all participants who completed a HRS Core from 2006-2016
tracker06_16 <- tracker %>% filter(KIWWAVE == 1 | LIWWAVE == 1 | MIWWAVE == 1 | NIWWAVE == 1 | OIWWAVE == 1 | PIWWAVE == 1)
nrow(tracker06_16)

###From core sample, select participants who were completed 2015 core & eligible for 2015 LHMS
trackerLHS2015elig <- tracker06_16[which(tracker06_16$OIWWAVE == 1 & tracker06_16$LHMS15 != 99 ),]
nrow(trackerLHS2015elig)
###From above, select all those who completed LHMS 2015
trackerLHS2015 <- trackerLHS2015elig[which(trackerLHS2015elig$LHMS15 == 1),]


###From core sample, select participants who completed a HRS Core 2016 & eligble for LHMS Fall or Spring 2017
trackerLHS2017elig <- tracker06_16[which(tracker06_16$PIWWAVE == 1 & tracker06_16$LHMS17FALL != 99 | tracker06_16$LHMS17SPR != 99),]
nrow(trackerLHS2017elig)
###From above, select all those who completed 2017
trackerLHS2017 <- trackerLHS2017elig[which(trackerLHS2017elig$LHMS17FALL == 1 | trackerLHS2017elig$LHMS17SPR == 1),]
nrow(trackerLHS2017)

###Merge sample of participants who completed LHMS 2015 or 2017 

trackerLHS15_17 <- merge(trackerLHS2015, trackerLHS2017, by=c("HHID", "PN"), all.x = TRUE, all.y = TRUE)
nrow(trackerLHS15_17)

###From hrs core, select those who were eligible for LBS in 2012 or 2014
### to make merging easier, created new df for HRS core with only variables of interest 
hrs12_lbs = hrs12[,c("HHID", "PN", "NLBELIG", "NLBCOMP", "NLB035_A", "NLB035_B", "NLB035_C","NLB037L", "NLB036F", "NLB036AY")] ##For ease, select variables needed from LBS 2012
lbs2012_elig <- merge( (hrs12[which(hrs12_lbs$NLBELIG == 1),]), (tracker06_16[which(tracker06_16$NIWWAVE== 1),]), by=c("HHID", "PN"))
nrow(lbs2012_elig)

hrs14_lbs = hrs14[,c("HHID", "PN", "OLBELIG", "OLBCOMP", "OLB033_A", "OLB033_B", "OLB033_C")]
lbs2014_elig <- merge((hrs14_lbs[which(hrs14_lbs$OLBELIG ==1),]), (tracker06_16[which(tracker06_16$OIWWAVE ==1),]), by=c("HHID", "PN"))
nrow(lbs2014_elig)

###From above, select those who complete 2012 or 2014
lbs_12_comp <- lbs2012_elig[which(lbs2012_elig$NLBCOMP != 5),]
lbs_14_comp <- lbs2014_elig[which(lbs2014_elig$OLBCOMP != 5),]

### Merge sample of particpants who completed either 2012 or 2014 LBS
lbs12_14 <- merge(lbs_12_comp, lbs_14_comp, by=c("HHID", "PN"), all.x = TRUE, all.y = TRUE)
nrow(lbs12_14)

###Create dataset of participants who completed 2015LHMS, 2017 LHMS, 2012 lbs, or 2014 lbs 
trackerlbs_lhs <- merge(lbs12_14, trackerLHS15_17, by=c("HHID","PN"), all.x=TRUE, all.y=TRUE)
nrow(trackerlbs_lhs)

##Now select from above for VBS eligible -> consented -> complete and valid
##Must create variable as merging create four
trackerlbs_lhs$VBS16ELIG = ifelse(trackerlbs_lhs$VBS16ELIG.x.x == 1 | trackerlbs_lhs$VBS16ELIG.x.y == 1 | trackerlbs_lhs$VBS16ELIG.y.x == 1 | trackerlbs_lhs$VBS16ELIG.y.y == 1, 1, 0)
trackerlbs_lhs$VBS16CONSENT = ifelse(trackerlbs_lhs$VBS16CONSENT.x.x == 1 | trackerlbs_lhs$VBS16CONSENT.y.x == 1 | trackerlbs_lhs$VBS16CONSENT.x.y == 1 | trackerlbs_lhs$VBS16CONSENT.y.y == 1, 1, 0)
trackerlbs_lhs$VBS16COMPLETE = ifelse(trackerlbs_lhs$VBS16COMPLETE.x.x == 1 | trackerlbs_lhs$VBS16COMPLETE.y.x == 1 | trackerlbs_lhs$VBS16COMPLETE.x.y==1 | trackerlbs_lhs$VBS16COMPLETE.y.y== 1, 1, 0)
trackerlbs_lhs$VBS16VALID = ifelse(trackerlbs_lhs$VBS16VALID.x.x == 1 | trackerlbs_lhs$VBS16VALID.y.x == 1 | trackerlbs_lhs$VBS16VALID.x.y == 1 | trackerlbs_lhs$VBS16VALID.y.y == 1, 1, 0)
trackerlbs_lhs$ZEROWGT = ifelse(is.na(trackerlbs_lhs$VBS16WGTR.x.x) & is.na(trackerlbs_lhs$VBS16WGTR.y.x) & is.na(trackerlbs_lhs$VBS16WGTR.x.y) & is.na(trackerlbs_lhs$VBS16WGTR.y.y), 0, 1)

##Select for those who are eligible first
trackerlbs_lhs_vbselig <- trackerlbs_lhs[which(trackerlbs_lhs$VBS16ELIG== 1),]
nrow(trackerlbs_lhs_vbselig)

###From all eligible for VBS, select all that consented 
trackerlbs_lhs_vbsconsent <- trackerlbs_lhs_vbselig[which(trackerlbs_lhs_vbselig$VBS16CONSENT ==1),]
nrow(trackerlbs_lhs_vbsconsent)
##From all eligible and consented, select those with complete and valid test results
trackerlbs_lhs_vbs <- trackerlbs_lhs_vbsconsent[which(trackerlbs_lhs_vbsconsent$VBS16COMPLETE == 1 & trackerlbs_lhs_vbsconsent$VBS16VALID),]
nrow(trackerlbs_lhs_vbs)

##From all eligible, consented, complete, valid, select those with non-zero survey weights
tracker_final <- trackerlbs_lhs_vbs[which(trackerlbs_lhs_vbs$ZEROWGT == 1),]
nrow(tracker_final)

###Now create data set with full VBS data + tracker, then select for cases with complete outcome data
d1= merge(vbs, vbsfcyto, by=c("HHID", "PN"))
d2 = merge(d1, vbs_sup, by=c("HHID", "PN"))
vbs_tracker = merge(d2, tracker_final, by=c("HHID", "PN"))
vbs_tracker$OUTCOME = ifelse(is.na(vbs_tracker$PIL6) | is.na(vbs_tracker$PCRP) | is.na(vbs_tracker$PTNFR1) |is.na(vbs_tracker$PCMVGE) | is.na(vbs_tracker$PCMVGINT) | is.na(vbs_tracker$PCD4N_PCT) | is.na(vbs_tracker$PCD4TEMRA_PCT) | is.na(vbs_tracker$PCD4T_PCT) | is.na(vbs_tracker$PCD8N_PCT) | is.na(vbs_tracker$PCD8TEMRA_PCT) | is.na(vbs_tracker$PCD8T_PCT), 0, 1)
vbs_complete = vbs_tracker %>% filter(OUTCOME == 1)

##Now merge in the LHS and RAND results
lhs$HHID = lhs$hhid ##recode ID variables for merging
lhs$PN = lhs$pn

vbs_lhs = merge(vbs_complete, lhs, by=c("HHID", "PN"), all.x = TRUE, all.y = FALSE)

vbs_lhs$ID = paste0(vbs_lhs$HHID, vbs_lhs$PN) ##Create new ID to merge with RAND
vbs_lhs$HHIDPN = str_remove(vbs_lhs$ID, "^0+")

data = merge(vbs_lhs, hrs, by = "HHIDPN", all.x=TRUE) ## This is the final data set
nrow(data)



###Recode LHS POIs into categorical variables 
data$orphanage <- factor(data$LH2A, c(1, 5), labels=c("Yes", "No"))
data$foster <- factor(data$LH2B, c(1, 5), labels=c("Yes", "No"))
data$prison <- factor(data$LH4A, c(1, 5), labels=c("Yes", "No"))
data$long_term_hospital <- factor(data$LH4B, c(1,5), labels=c("Yes", "No"))
data$combatzone <- factor(data$LH4C, c(1,5), labels=c("Yes", "No"))
data$homeless <- factor(data$LH4E, c(1,5), labels=c("Yes", "No"))
data$long_term_pysch<- factor(data$LH4F, c(1,5), labels=c("Yes", "No"))
data$naturaldisater<- factor(data$LH4G, c(1,5), labels=c("Yes", "No"))

###Install packages for data cleaning and manipulation 
install.packages("dplyr")
install.packages("tidyr")
install.packages("janitor")
install.packages("gtsummary")
library(dplyr)
library(tidyr)
library(janitor)
library(gtsummary)

###Table Summary of just LHS Data 
data %>% select(foster, orphanage) %>%
  tbl_summary(type = everything() ~ "categorical") %>%
  modify_caption("Table 1. LHS Childhood Traumatic Enviromental Changes") 

data %>% select(prison, homeless, combatzone) %>%
  tbl_summary(type = everything() ~ "categorical") %>%
  modify_caption("Table 2. LHS Mid Age Traumatic Enviromental Changes")

data %>% select(long_term_hospital, long_term_pysch,naturaldisater) %>%
  tbl_summary(type = everything() ~ "categorical") %>%
  modify_caption("Table 3. LHS Mid Age Traumatic Enviromental Changes")




###Merge LBS data (keeping all unique participants)
lbs_d1 = merge(hrs12, hrs14, by="ID", all.x=TRUE, all.y=TRUE)
lbs_d2 = merge(lbs_d1, vbs, by="ID")
lbs_d3 = merge(lbs_d2, vbs_sup, by="ID")
lbs_d4 = merge(lbs_d3, vbsfcyto, by="ID")

###Recode LBS prison and homeless into categorical 
lbs_d4$prison12= factor(lbs_d4$NLB035_B, c(1,5), labels=c("Yes", "No"))
lbs_d4$homeless12= factor(lbs_d4$NLB035_C, c(1,5), labels=c("Yes", "No"))
lbs_d4$homeless14= factor(lbs_d4$OLB033_A, c(1,5), labels=c("Yes", "No"))
lbs_d4$prison14= factor(lbs_d4$OLB033_B, c(1,5), labels=c("Yes", "No"))
lbs_d4$prisontime14 = factor(lbs_d4$OLB033_C, c(1,2,3,4,5), labels=c("lessthan_1month", "1month-1year", "1-5 years", ">5years", "Don't Know"))
lbs_d4$prisontime12 = factor(lbs_d4$NLB035_C, c(1,2,3,4,5), labels=c("lessthan_1month", "1month-1year", "1-5 years", ">5years", "Don't Know"))
###Summary table of LBS POI (uncombined)
lbs_d4 %>% select(prison12, prisontime12, prison14, prisontime14, homeless12, homeless14) %>%
  tbl_summary()

###Clean LBS data and create a new variable for prison/homeless based on both 2012 and 2014 answers
lbs_d5 = clean_names(lbs_d4)

lbs_d6 <- lbs_d5 %>% mutate(prison12_14 = case_when(
  prison12 == prison14 ~ "match",
  prison12 != prison14 ~ "not match"
))### 3 matches, no unmatch

lbs_d7 <- lbs_d6 %>% mutate(homeless12_14 = case_when(
  homeless12 == homeless14 ~ "match",
  homeless12 != homeless14 ~ "not match"
))### no "no match"

lbs_d7$prison_lbs = ifelse(is.na(lbs_d7$prison12), lbs_d7$prison14, lbs_d7$prison12)
lbs_d7$prisonlbs = factor(lbs_d7$prison_lbs, c(1,2), labels=c("Yes", "No"))
lbs_d7$home_lbs = ifelse(is.na(lbs_d7$homeless12), lbs_d7$homeless14, lbs_d7$homeless12)
lbs_d7$homelesslbs = factor(lbs_d7$home_lbs, c(1,2), labels=c("Yes", "No"))

lbs_d7$prisontime = ifelse(is.na(lbs_d7$prisontime12), lbs_d7$prisontime14, lbs_d7$prisontime12)
lbs_d7$prison_time = factor(lbs_d7$prisontime, c(1,2,3,4,5), labels=c("lessthan_1month", "1month-1year", "1-5 years", ">5years", "Don't Know"))

###Summary table of cleaned & combined data 
lbs_data_sum <- lbs_d7[,c("homelesslbs", "prisonlbs", "prison_time")]
lbs_data_sum %>% 
  tbl_summary(type=everything()~ "categorical") %>%
  modify_caption("Table 3. LBS Traumatic Enviromental Change")

###Merge LBS with LHS+VBS+RAND
lbs_d7$HHIDPN = str_remove(lbs_d7$id, "^0+")
lbs_lhs_data = merge(data, lbs_d7, by="HHIDPN", all.x=TRUE, all.y=TRUE) ### keeps unique people

###Create variable to compare LHS and LBS answers
lbs_lhs_data1 = clean_names(lbs_lhs_data)
lbs_lhs_data2 <- lbs_lhs_data1 %>% mutate(prison_cum = case_when(
  prisonlbs == prison ~ "match",
  prisonlbs != prison ~ "no match"
)) ###335 variables don't not match

lbs_lhs_data3 <- lbs_lhs_data2 %>% mutate(homeless_cum = case_when(
  homelesslbs == homeless ~ "match",
  homelesslbs != homeless ~ "no match"
)) ### 293 participant answers don't match 

### Create Variable that merges LHS/LBS data for prison and homeless
lbs_lhs_data3$home1 = ifelse(lbs_lhs_data3$homeless_cum == "match", lbs_lhs_data3$homeless, lbs_lhs_data3$homeless_cum )
lbs_lhs_data3$home2 = ifelse(is.na(lbs_lhs_data3$home1), lbs_lhs_data3$homeless, lbs_lhs_data3$home1)
lbs_lhs_data3$home3 = ifelse(is.na(lbs_lhs_data3$home2), lbs_lhs_data3$homelesslbs, lbs_lhs_data3$home2)
lbs_lhs_data3$HOMELESS = factor(lbs_lhs_data3$home3, c(1,2, "no match"), labels=c("Yes", "No", "Incongruent"))

lbs_lhs_data3$p1 = ifelse(lbs_lhs_data3$prison_cum == "match", lbs_lhs_data3$prison, lbs_lhs_data3$prison_cum)
lbs_lhs_data3$p2 = ifelse(is.na(lbs_lhs_data3$p1), lbs_lhs_data3$prison, lbs_lhs_data3$p1)
lbs_lhs_data3$p3 = ifelse(is.na(lbs_lhs_data3$p2), lbs_lhs_data3$prisonlbs, lbs_lhs_data3$p2)
lbs_lhs_data3$PRISON = factor(lbs_lhs_data3$p3, c(1,2, "no match"), labels=c("Yes", "No", "Incongruent"))


### Recode race/ethincity and gender into categorical 
dfv1 <- lbs_lhs_data3 %>% mutate(gender = case_when(
  ragender == 1 ~ "Male",
  ragender == 2 ~ "Female"
)) 
dfv2 <- dfv1 %>% mutate(race = case_when(
  raracem == 1 ~ "White",
  raracem == 2 ~ "Black",
  raracem == 3 ~ "Other"
))
dfv3 <- dfv2 %>% mutate(ethinicity = case_when(
  rahispan == 0 ~ "Not hisp",
  rahispan == 1 ~ "hisp"
))
dfv3$race_ethinicity = paste(dfv3$ethinicity, dfv3$race)
dfv3$gender_race = paste(dfv3$gender, dfv3$race)

###Create cumulative trauma score
dfv3$o = ifelse(dfv3$orphanage == "Yes", 1, 0)
dfv3$f = ifelse(dfv3$foster == "Yes", 1, 0)
dfv3$p = ifelse(dfv3$PRISON =="Yes", 1, 0)
dfv3$lth = ifelse(dfv3$long_term_hospital == "Yes", 1, 0)
dfv3$c = ifelse(dfv3$combatzone == "Yes", 1, 0)
dfv3$h = ifelse(dfv3$HOMELESS == "Yes", 1, 0)
dfv3$ltp = ifelse(dfv3$long_term_pysch == "Yes", 1, 0)
dfv3$nd = ifelse(dfv3$naturaldisater == "Yes", 1, 0)
dfv3$cum_trauma_score = rowSums(dfv3[,c("o","f","p","lth","c","h","ltp","nd")], na.rm=TRUE)

### Remove in congruent data (not run)
df_nomatch_removed = dfv3[!(dfv3$prison_cum == "no match" | dfv3$homeless_cum == "no match")]


dfv3 %>% select(orphanage, foster) %>%
  tbl_summary(type= everything()~"categorical")
dfv3 %>% select(PRISON, HOMELESS, combatzone) %>%
  tbl_summary(type= everything()~"categorical")
dfv3 %>% select(long_term_hospital, long_term_pysch, naturaldisater) %>%
  tbl_summary(type= everything()~"categorical")
dfv3 %>% select(cum_trauma_score) %>%
  tbl_summary(type= everything()~"categorical")
hist(dfv3$cum_trauma_score)


###Make tables by gender & race
dfv3 %>% select(orphanage, foster, gender)%>%
  tbl_summary(by="gender", type = everything() ~ "categorical")
dfv3 %>% select(PRISON, HOMELESS, combatzone, gender)%>%
  tbl_summary(by="gender", type = everything() ~ "categorical")
dfv3 %>% select(long_term_hospital, long_term_pysch, naturaldisater,gender)%>%
  tbl_summary(by="gender", type = everything() ~ "categorical")
dfv3 %>% select(cum_trauma_score,gender)%>%
  tbl_summary(by="gender", type = everything() ~ "categorical")

dfv3 %>% select(orphanage, foster, race)%>%
  tbl_summary(by="race", type = everything() ~ "categorical")
dfv3 %>% select(PRISON, HOMELESS, combatzone, race)%>%
  tbl_summary(by="race", type = everything() ~ "categorical")
dfv3 %>% select(long_term_hospital, long_term_pysch, naturaldisater,race)%>%
  tbl_summary(by="race", type = everything() ~ "categorical")
dfv3 %>% select(cum_trauma_score,race)%>%
  tbl_summary(by="race", type = everything() ~ "categorical")

dfv3 %>% select(orphanage, foster, ethinicity)%>%
  tbl_summary(by="ethinicity", type = everything() ~ "categorical")
dfv3 %>% select(PRISON, HOMELESS, combatzone, ethinicity)%>%
  tbl_summary(by="ethinicity", type = everything() ~ "categorical")
dfv3 %>% select(long_term_hospital, long_term_pysch, naturaldisater,ethinicity)%>%
  tbl_summary(by="ethinicity", type = everything() ~ "categorical")
dfv3 %>% select(cum_trauma_score,ethinicity)%>%
  tbl_summary(by="ethinicity", type = everything() ~ "categorical")

dfv3 %>% select(orphanage, foster, PRISON, HOMELESS, combatzone, long_term_hospital, long_term_pysch, naturaldisater, cum_trauma_score, race_ethinicity)%>%
  tbl_summary(by="race_ethinicity", type = everything() ~ "categorical")
dfv3 %>% select(PRISON, HOMELESS, combatzone, race_ethinicity)%>%
  tbl_summary(by="race_ethinicity", type = everything() ~ "categorical")
dfv3 %>% select(long_term_hospital, long_term_pysch, naturaldisater,race_ethinicity)%>%
  tbl_summary(by="race_ethinicity", type = everything() ~ "categorical")
dfv3 %>% select(cum_trauma_score,race_ethinicity)%>%
  tbl_summary(by="race_ethinicity", type = everything() ~ "categorical")




