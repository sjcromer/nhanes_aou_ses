#####AoU_RExSES_analysis.R#####
#####AoU INDIVIDUAL SES ANALYSIS - NHANES race*SES replication#####

#####packages#####
#rm(list=ls())
#update.packages()
library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggridges)
library(RColorBrewer)
library(progress)
install.packages('forestplot')
library(forestplot)
install.packages('ggpubr')
library(ggpubr)
install.packages('survey')
library(survey)
install.packages('metafor')
library(metafor)
install.packages('tableone')
library(tableone)
install.packages('jtools')
library(jtools)
install.packages('remotes')
library(remotes)
install.packages('haven')
library(haven)
install.packages('fmsb')
library(fmsb)
install.packages('sandwich')
library(sandwich)
install.packages('lmtest')
library(lmtest)
#install.packages('sociome')
#library(sociome)
#install.packages('rqlm')
#library(rqlm)


#####Import data sets#####
# Read person from Google bucket
person_filepath <- "gs://fc-secure-f542e522-bad2-4a28-b38f-d5e3621fe4cd/AoU_RExSES/AoU_RExSES_person_final.csv"
system(paste0("gsutil cp -r ", person_filepath, " ."),intern=TRUE)
list.files(basename(person_filepath))
p <- read.csv(file='AoU_RExSES_person_final.csv', header=TRUE, sep=',')
dim(p)

# Read survey from Google bucket
survey_filepath <- "gs://fc-secure-f542e522-bad2-4a28-b38f-d5e3621fe4cd/AoU_RExSES/AoU_RExSES_survey_final.csv"
system(paste0("gsutil cp -r ", survey_filepath, " ."),intern=TRUE)
list.files(basename(survey_filepath))
s <- read.csv(file='AoU_RExSES_survey_final.csv', header=TRUE, sep=',')
dim(s)

# Read condition from Google bucket
cond_filepath <- "gs://fc-secure-f542e522-bad2-4a28-b38f-d5e3621fe4cd/AoU_RExSES/AoU_RExSES_cond_final.csv"
system(paste0("gsutil cp -r ", cond_filepath, " ."),intern=TRUE)
list.files(basename(cond_filepath))
c <- read.csv(file='AoU_RExSES_cond_final.csv', header=TRUE, sep=',')
dim(c)

# Read measures from Google bucket
meas_filepath <- "gs://fc-secure-f542e522-bad2-4a28-b38f-d5e3621fe4cd/AoU_RExSES/AoU_RExSES_meas_final.csv"
system(paste0("gsutil cp -r ", meas_filepath, " ."),intern=TRUE)
list.files(basename(meas_filepath))
m <- read.csv(file='AoU_RExSES_meas_final.csv', header=TRUE, sep=',')
dim(m)

# Read person from Google bucket
cond2_filepath <- "gs://fc-secure-f542e522-bad2-4a28-b38f-d5e3621fe4cd/AoU_RExSES/AoU_RExSES_cond2_final.csv"
system(paste0("gsutil cp -r ", cond2_filepath, " ."),intern=TRUE)
list.files(basename(cond2_filepath))
c2 <- read.csv(file='AoU_RExSES_cond2_final.csv', header=TRUE, sep=',')
dim(c2)
c2$in_cond_df <- 1

# Read person from Google bucket
surv2_filepath <- "gs://fc-secure-f542e522-bad2-4a28-b38f-d5e3621fe4cd/AoU_RExSES/AoU_RExSES_surv2_final.csv"
system(paste0("gsutil cp -r ", surv2_filepath, " ."),intern=TRUE)
list.files(basename(surv2_filepath))
s2 <- read.csv(file='AoU_RExSES_surv2_final.csv', header=TRUE, sep=',')
dim(s2)

# Read person from Google bucket
med_filepath <- "gs://fc-secure-f542e522-bad2-4a28-b38f-d5e3621fe4cd/AoU_RExSES/AoU_RExSES_med_final.csv"
system(paste0("gsutil cp -r ", med_filepath, " ."),intern=TRUE)
list.files(basename(med_filepath))
med <- read.csv(file='AoU_RExSES_med_final.csv', header=TRUE, sep=',')
dim(med)

ps <- merge(p,s, by='person_id', all.x=T, all.y=T, sort=T)
psc <- merge (ps,c, by='person_id', all.x=T, all.y=T, sort=T)
pscm <- merge (psc,m, by='person_id', all.x=T, all.y=T, sort=T)
pscmc2 <- merge (pscm,c2, by='person_id', all.x=T, all.y=T, sort=T)
pscmc2s2 <- merge (pscmc2,s2, by='person_id', all.x=T, all.y=T, sort=T)
aou_raw <- merge (pscmc2s2,med, by='person_id', all.x=T, all.y=T, sort=T)
dim(aou_raw)
colnames(aou_raw)




#####Variable cleaning#####
aou_raw$t1d <- aou_raw$t1d.y
aou_raw$t2d <- aou_raw$t2d.y
aou_raw$ob <- aou_raw$ob.y
aou_raw$predm <- aou_raw$predm.y
aou_raw[,c('in_surv_df','in_cond_df','in_med_df','dm_a1c','dm_rbg','dm_fbg','t1d','t2d','ob','predm','t1d_selfrep','ob_selfrep','otherdm_selfrep','t2d_selfrep','predm_selfrep')][is.na(aou_raw[,c('in_surv_df','in_cond_df','in_med_df','dm_a1c','dm_rbg','dm_fbg','t1d','t2d','ob','predm','t1d_selfrep','ob_selfrep','otherdm_selfrep','t2d_selfrep','predm_selfrep')])] <- 0

aou_raw$survey_date <- as.Date(aou_raw$survey_date, format='%Y-%m-%d')
class(aou_raw$survey_date)
aou_raw$survey_date[1:10]
aou_raw$date_of_birth <- as.Date(aou_raw$date_of_birth, format='%Y-%m-%d %H:%M:%S')
class(aou_raw$date_of_birth)
aou_raw$date_of_birth[1:10]
aou_raw$age <- as.numeric((aou_raw$survey_date - aou_raw$date_of_birth) / 365)
summary(aou_raw$age)
aou_raw$age[1:20]
aou_raw$agecat <- ifelse(aou_raw$age<=29,'2', 
                         ifelse(aou_raw$age<=39,'3', 
                                ifelse(aou_raw$age<=49, '4',
                                       ifelse(aou_raw$age<=59,'5',
                                              ifelse(aou_raw$age<=69,'6',
                                                     ifelse(aou_raw$age<=79, '7', '8+'))))))
aou_raw$age_gt18 <- ifelse(aou_raw$age >=18,1,0)
table(aou_raw$age_gt18, exclude=NULL)
table(aou_raw$agecat, aou_raw$race, exclude=NULL)
table(aou_raw$sex)
aou_raw$female <- ifelse(aou_raw$sex=='Female',1, ifelse(aou_raw$sex=='Male',0,NA))
table(aou_raw$female, exclude=NULL)
aou_raw$racecat <- aou_raw$race
aou_raw$racecat <- factor(aou_raw$racecat, levels=c("NHW","NHB","Hispanic","NHAsian","Multiracial","Other","None of these"))

aou_raw$diab_any_selfrep <- ifelse(aou_raw$t2d_selfrep==1 | aou_raw$t1d_selfrep==1 | aou_raw$otherdm_selfrep==1, 1, 0)
aou_raw$diab_any_ehr <- ifelse(aou_raw$t2d==1 | aou_raw$t1d==1 | aou_raw$dm_a1c==1 | aou_raw$dm_fbg==1 | aou_raw$dm_rbg==1, 1, 0)
aou_raw$diab_any_med <- ifelse(aou_raw$sum_insulin >=1 | aou_raw$sum_met >=1 | aou_raw$sum_su >=1 | aou_raw$sum_dpp4 >=1 | aou_raw$sum_glp >=1 | aou_raw$sum_sglt2 >=1 | aou_raw$sum_tzd >=1 | aou_raw$sum_meglit >=1,1,0)
aou_raw$diab_any_diabspecmed <- ifelse(aou_raw$sum_insulin >=1 | aou_raw$sum_su >=1 | aou_raw$sum_dpp4 >=1 | aou_raw$sum_tzd >=1 | aou_raw$sum_meglit >=1,1,0)
aou_raw$diab_any <- ifelse(aou_raw$diab_any_ehr==1 | aou_raw$diab_any_selfrep==1 | aou_raw$diab_any_diabspecmed==1, 1, 0)
table(aou_raw$diab_any, aou_raw$dm_rbg, exclude=NULL)
aou_raw[,c('diab_any')][is.na(aou_raw[,c('diab_any')])] <- 0
table(aou_raw$diab_any, aou_raw$diab_any_diabspecmed, exclude=NULL)
#aou_raw$all_diab_var_na <- ifelse(is.na(aou_raw$t2d) & is.na(aou_raw$t1d) & is.na(aou_raw$dm_a1c) & is.na(aou_raw$dm_fbg) & is.na(aou_raw$dm_rbg), 1, 0)#, 1, 0)
#table(aou_raw$all_diab_var_na, exclude=NULL)
summary(m$rbg_max)
table(aou_raw$dm_rbg, exclude=NULL)

aou_raw$t2d_onset_date <- ifelse(aou_raw$t2d_onset_date=="Inf",NA,aou_raw$t2d_onset_date)
aou_raw$t2d_onset_date[1:10]
aou_raw$t2d_onset_date <- as.Date(aou_raw$t2d_onset_date, format='%Y-%m-%d')
class(aou_raw$t2d_onset_date)
aou_raw$t2d_onset_date[1:10]
aou_raw$t2d_age_ehr <- as.numeric((aou_raw$t2d_onset_date - aou_raw$date_of_birth) / 365)
summary(aou_raw$t2d_age_ehr, na.rm=T)
aou_raw$t2d_age_selfrep_child <- ifelse(aou_raw$t2d_age_selfrep=='About how old were you when you were first told you had type 2 diabetes? - Adolescent (12-17)' |
                                          aou_raw$t2d_age_selfrep=='About how old were you when you were first told you had type 2 diabetes? - Child (0-11)',1,0)
aou_raw$otherdm_age_selfrep_child <- ifelse(aou_raw$otherdm_age_selfrep=='About how old were you when you were first told you had other/unknown diabetes? - Adolescent (12-17)' |
                                              aou_raw$otherdm_age_selfrep=='About how old were you when you were first told you had other/unknown diabetes? - Child (0-11)',1,0)
table(aou_raw$otherdm_age_selfrep_child)

#aou_raw[1:100,c('t2d_age_ehr','t2d_age_selfrep','otherdm_age_selfrep')]
aou_raw$t1d_ageconcern <- ifelse(is.na(aou_raw$t2d_age_ehr) & is.na(aou_raw$t2d_age_selfrep) & is.na(aou_raw$otherdm_age_selfrep),NA,
                                 ifelse(aou_raw$diab_any==1 & aou_raw$t2d_age_ehr <30, 1, 
                                        ifelse(aou_raw$diab_any==1 & aou_raw$t2d_age_selfrep_child==1,1,
                                               ifelse(aou_raw$diab_any==1 & aou_raw$otherdm_age_selfrep<30,1,0))))
aou_raw$t1d_medconcern <- ifelse(aou_raw$diab_any==1 & aou_raw$insulin_ever==1 & aou_raw$oad_ever==0,1,0)
table(aou_raw$t1d_ageconcern, aou_raw$t1d_medconcern, exclude=NULL)
aou_raw$any_t1d_chance <- ifelse(aou_raw$t1d==1 | aou_raw$t1d_selfrep==1 | aou_raw$t1d_ageconcern==1 | aou_raw$t1d_medconcern==1,1,0)
table(aou_raw$t1d, aou_raw$t1d_selfrep, exclude=NULL)
aou_raw$any_t1d_chance <- ifelse(is.na(aou_raw$any_t1d_chance),0,aou_raw$any_t1d_chance)
table(aou_raw$any_t1d_chance, exclude=NULL)

aou_raw$diab <- ifelse(aou_raw$diab_any==0,0,
                       ifelse(aou_raw$diab_any==1 & aou_raw$any_t1d_chance==0,1,0))
addmargins(table(aou_raw$diab_any, aou_raw$diab, exclude=NULL)) #7k have ehr t1d, 2.5k self-report t1d (1k overlap so 8.5k total); 3k report onset <30, 14k only ever insulin
addmargins(table(aou_raw$diab_any, aou_raw$insulin_ever, exclude=NULL)) 
table(aou_raw$t2d_selfrep, aou_raw$t2d, exclude=NULL)
table(aou_raw$t1d_selfrep, aou_raw$t2d, exclude=NULL)
table(aou_raw$otherdm_selfrep, aou_raw$t2d, exclude=NULL)

#change NAs to 0s for a select group of variables
colnames(aou_raw)
table(aou_raw$t1d, aou_raw$diab_any_selfrep, exclude=NULL)
aou_raw[,c('in_surv_df','in_cond_df','in_med_df','dm_a1c','dm_rbg','dm_fbg','t1d','t2d','ob','predm','t1d_selfrep','ob_selfrep','otherdm_selfrep','t2d_selfrep','predm_selfrep','diab_any_selfrep','diab_any_ehr','diab_any_med','diab_any_diabspecmed','diab_any','t1d_ageconcern','t1d_medconcern','any_t1d_chance','diab')][is.na(aou_raw[,c('in_surv_df','in_cond_df','in_med_df','dm_a1c','dm_rbg','dm_fbg','t1d','t2d','ob','predm','t1d_selfrep','ob_selfrep','otherdm_selfrep','t2d_selfrep','predm_selfrep','diab_any_selfrep','diab_any_ehr','diab_any_med','diab_any_diabspecmed','diab_any','t1d_ageconcern','t1d_medconcern','any_t1d_chance','diab')])] <- 0
table(aou_raw$t1d, aou_raw$diab_any_selfrep, exclude=NULL)

aou_raw$diab_known <- ifelse(aou_raw$t2d_selfrep==1 | aou_raw$t1d_selfrep==1 | aou_raw$otherdm_selfrep==1, 1, 0)
aou_raw$diab_known[is.na(aou_raw$diab_known)] <- 0
table(aou_raw$diab_known, aou_raw$diab, exclude=NULL) #15k of 58k know of dm (but only 1/3 completed survey and had chance to self-report)


#aou_raw$diab_controlled <- ifelse(aou_raw$a1c_max <=7, 1, 0)
#aou_raw$diab_controlled <- ifelse(is.na(aou_raw$diab_controlled),NA,aou_raw$diab_controlled)
#table(aou_raw$diab_controlled, exclude=NULL)
#aou_raw$diab_controlled8 <- ifelse(aou_raw$a1c_max <=8, 1, 0)
#aou_raw$diab_controlled8 <- ifelse(is.na(aou_raw$diab_controlled),NA,aou_raw$diab_controlled8)

summary(aou_raw$bmi_max)
aou_raw$obese_bmi <- ifelse(aou_raw$bmi_max >= 30, 1, 0)
aou_raw$obese_bmi2 <- ifelse(aou_raw$racecat=='NHAsian' & aou_raw$bmi_max >= 27.5, 1, aou_raw$obese_bmi)
table(aou_raw$obese_bmi, aou_raw$obese_bmi2, exclude=NULL)

aou_raw$obese <- ifelse(aou_raw$ob==1 | aou_raw$obese_bmi2==1, 1, 0)
aou_raw$obese <- ifelse(is.na(aou_raw$obese), 0, ifelse(aou_raw$obese==1,1,0))
aou_raw$all_ob_var_na <- ifelse(is.na(aou_raw$ob) & is.na(aou_raw$obese_bmi2),1,0)
table(aou_raw$all_ob_var_na, exclude=NULL)

aou_raw$obese_known <- ifelse(aou_raw$ob==1, 1, 0)
aou_raw$obese_known <- ifelse(is.na(aou_raw$obese_known), 0, ifelse(aou_raw$obese_known==1,1,0))
table(aou_raw$obese, aou_raw$obese_known, exclude=NULL)


table(aou_raw$educ_num, aou_raw$educ_simple, exclude=NULL)
aou_raw$educ_simple <- factor(aou_raw$educ_simple, levels=c("College Degree","Some College","HS Degree","Less than HS"))
table(aou_raw$hreduc,aou_raw$educ_num, exclude=NULL)
boxplot_indiveduc <- ggplot(aou_raw, aes(factor(x=racecat), y=educ_num, fill=factor(diab)), exclude=TRUE) + geom_boxplot(notch=F)

table(aou_raw$hhinc)
aou_raw$inc_simple <- ifelse(aou_raw$hhinc=="Annual Income: less 10k" | aou_raw$hhinc=="Annual Income: 10k 25k",  'Under $25,000',
                             ifelse(aou_raw$hhinc=="Annual Income: 25k 35k" | aou_raw$hhinc=="Annual Income: 35k 50k", '$25,000-49,999', 
                                    ifelse(aou_raw$hhinc=="Annual Income: 50k 75k", '$50,000-74,999',
                                           ifelse(aou_raw$hhinc=="Annual Income: 75k 100k" | aou_raw$hhinc=="Annual Income: 100k 150k", '$75,000-149,999',
                                                  ifelse(aou_raw$hhinc=="Annual Income: 150k 200k"|aou_raw$hhinc=="Annual Income: more 200k", "More than $150,000", NA)))))
table(aou_raw$hhinc, aou_raw$inc_simple, exclude=NULL)
aou_raw$inc_num <- ifelse(aou_raw$hhinc=="Annual Income: less 10k" | aou_raw$hhinc=="Annual Income: 10k 25k",  1,
                          ifelse(aou_raw$hhinc=="Annual Income: 25k 35k" | aou_raw$hhinc=="Annual Income: 35k 50k", 2, 
                                 ifelse(aou_raw$hhinc=="Annual Income: 50k 75k", 3,
                                        ifelse(aou_raw$hhinc=="Annual Income: 75k 100k",4,
                                               ifelse(aou_raw$hhinc=="Annual Income: 100k 150k", 5.5,
                                                      ifelse(aou_raw$hhinc=="Annual Income: 150k 200k", 7.5,
                                                             ifelse(aou_raw$hhinc=="Annual Income: more 200k", 9, NA)))))))
table(aou_raw$hhinc,aou_raw$inc_num, exclude=NULL)


aou_raw$racecat <- factor(aou_raw$racecat, levels=c("NHW","NHB","Hispanic","NHAsian","Multiracial","Other","None of these"))
aou_raw$educ_simple <- factor(aou_raw$educ_simple, levels=c("College Degree","Some College","HS Degree","Less than HS"))
aou_raw$hhinc <- factor(aou_raw$hhinc, levels=c("Annual Income: more 200k","Annual Income: 150k 200k","Annual Income: 100k 150k","Annual Income: 75k 100k","Annual Income: 50k 75k","Annual Income: 35k 50k","Annual Income: 25k 35k","Annual Income: 10k 25k","Annual Income: less 10k","PMI: Prefer Not To Answer","PMI: Skip"))
aou_raw$inc_simple <- factor(aou_raw$inc_simple, levels=c("More than $150,000","$75,000-149,999","$50,000-74,999","$25,000-49,999","Under $25,000"))
aou_raw$insur_stability <- factor(aou_raw$insur_stability, levels=c("Insured-Private","Insured-Other","Uninsured"))
aou_raw$smoke_status <- factor(aou_raw$smoke_status, levels=c("Never Smoker","Former Smoker","Current or Former Smoker NOS","Current Smoker"))
table(aou_raw$educ_simple, aou_raw$inc_simple, exclude=NULL)
table(aou_raw$insur_stability, aou_raw$smoke_status, exclude=NULL)

#aou_raw$work_yn <- ifelse(aou_raw$working=='Working',1, ifelse(aou_raw$working=='Looking for work' | aou_raw$working=='Not Working',0, NA))
addmargins(table(aou_raw$work_yn, aou_raw$hhinc, exclude=NULL))
ggplot(aou_raw, aes(factor(x=work_yn), y=age, fill=factor(diab)), exclude=TRUE) + geom_boxplot(notch=F)

dim(aou_raw)
aou_raw$all_ses_na <- ifelse(is.na(aou_raw$educ_num) & is.na(aou_raw$inc_simple),1,0)
addmargins(table(aou_raw$all_ses_na, exclude=NULL))


#flow diagram
dim(aou_raw)
aou_personid <- aou_raw[!is.na(aou_raw$person_id),]
dim(aou_personid) #393840
aou_nokids <- aou_personid[aou_raw$age>=18,]
dim(aou_nokids) #393840
aou_nokids_fulldemog <- aou_nokids[!is.na(aou_nokids$female) & !is.na(aou_nokids$racecat) & !is.na(aou_nokids$age),]#no missingness age or racecat ("none of these" but prefer not to answer previously excluded)
dim(aou_nokids_fulldemog) #390346
aou_nokids_fulldemog_fullses <- aou_nokids_fulldemog[aou_nokids_fulldemog$all_ses_na==0,]
dim(aou_nokids_fulldemog_fullses) #390346
aou_nokids_fulldemog_fullses_dmob <- aou_nokids_fulldemog_fullses[aou_nokids_fulldemog_fullses$in_surv_df==1 | aou_nokids_fulldemog_fullses$in_med_df==1 | aou_nokids_fulldemog_fullses$in_cond_df==1,]
dim(aou_nokids_fulldemog_fullses_dmob) #304179
table(aou_nokids_fulldemog_fullses_dmob$smoke_status, aou_nokids_fulldemog_fullses_dmob$insur_stability, exclude=NULL)
aou_nokids_fulldemog_fullses_dmob_fullcov <- aou_nokids_fulldemog_fullses_dmob[!aou_nokids_fulldemog_fullses_dmob$smoke_status=='Missing' & !is.na(aou_nokids_fulldemog_fullses_dmob$smoke_status) & !aou_nokids_fulldemog_fullses_dmob$insur_stability=='Missing',]
dim(aou_nokids_fulldemog_fullses_dmob_fullcov) #297069
table(aou_nokids_fulldemog_fullses_dmob_fullcov$diab_any, aou_nokids_fulldemog_fullses_dmob_fullcov$diab, exclude=NULL)
aou_nokids_fulldemog_fullses_dmob_fullcov_noequivocaldm <- aou_nokids_fulldemog_fullses_dmob_fullcov[aou_nokids_fulldemog_fullses_dmob_fullcov$diab_any==0 | aou_nokids_fulldemog_fullses_dmob_fullcov$diab==1,] #excludes diab_any=1 but diab=0
dim(aou_nokids_fulldemog_fullses_dmob_fullcov_noequivocaldm) #275292


aou <- aou_nokids_fulldemog_fullses_dmob_fullcov_noequivocaldm
table(aou_raw$diab_any, aou_raw$diab, exclude=NULL)
table(aou$diab_any, aou$diab, exclude=NULL)
rm(aou_nokids, aou_nokids_fulldemog,aou_nokids_fulldemog_fullses,aou_nokids_fulldemog_fullses_dmob,aou_nokids_fulldemog_fullses_dmob_fullcov)



####correlation between exposures#####
cor.test(aou$educ_num, aou$inc_num) #pearson's 0.4734495
ggplot(aou[!is.na(aou$educ_num) & !is.na(aou$inc_num),], aes(x=racecat, y=inc_num, fill=as.factor(educ_num))) + geom_violin() + 
  labs(x='', y='Income (Multiples of $25k)', title='Education-Income Relationship', fill="Educational \n Attainment")

ggplot(aou[!is.na(aou$educ_num) & !is.na(aou$inc_num),], aes(x=racecat, y=inc_num, fill=as.factor(educ_num))) + geom_boxplot() + 
  labs(x='Race/Ethnicity', y='Income (Multiples of $25k)', title='Education-Income Relationship', fill="Educational Attainment") + 
  scale_fill_discrete(labels=c("Less than HS","HS Degree","Some College","College Degree"))
ggsave('aou_boxplot_educxinc_racestrat.png')


#####table 1#####
dim(aou)
tab1_all <- CreateTableOne(data=aou, #strata='hba1c.F12_missing', 
                           vars=c('age','female','racecat','hreduc','educ_num','hhinc','inc_num','work_yn','insur_stability','smoke_status','diab','ob'), test=T,
                           factorVars=c('female','racecat','hreduc','hhinc','work_yn','insur_stability','diab','ob'))
tab1_diab_strat <- CreateTableOne(data=aou, strata='diab', 
                                  vars=c('age','female','racecat','hreduc','educ_num','hhinc','inc_num','work_yn','insur_stability','smoke_status'), test=T,
                                  factorVars=c('female','racecat','hreduc','hhinc','work_yn','hoursworked_cat','insur_stability','smoke_status'))
print(tab1_all, quote=T, noSpaces=T)
print(tab1_diab_strat, quote=T, noSpaces=T)


#####visuals: violins, ridge plots = distribution of SES variables by race ethnicity#####
table(aou$hreduc, aou$racecat)
aou_racexeduc <- as.data.frame(100*prop.table(table(aou$hreduc, aou$racecat),2))

ggplot(aou_racexeduc, aes(x=Var2, y=Freq, fill=Var1)) + 
  geom_bar(position='fill', stat='identity') + ggtitle('Distribution of Individual-Level Educational Attainment \n by Race/Ethnicity of Participant') + 
  xlab('') + ylab('Proportion') + labs(fill='Educational \n Attainment') + 
  theme(legend.position='bottom', legend.text=element_text(size=4), plot.title=element_text(hjust=0.5))
ggsave('aou_stackbar_indiveduc_racestrat.png')

aou_racexinc <- as.data.frame(100*prop.table(table(aou$hhinc, aou$racecat),2))
ggplot(aou_racexinc, aes(x=Var2, y=Freq, fill=Var1)) + 
  geom_bar(position='fill', stat='identity') + ggtitle('Distribution of Individual-Level Income \n by Race/Ethnicity of Participant') + 
  xlab('') + ylab('Proportion') + labs(fill='Income') + 
  theme(legend.position='bottom', legend.text=element_text(size=5), plot.title=element_text(hjust=0.5))
ggsave('aou_stackbar_indivinc_racestrat.png')

ggplot(aou, aes(factor(x=racecat), y=inc_num, color=racecat), exclude=TRUE) + geom_violin(trim=TRUE) + geom_boxplot(width=0.1)
ggplot(aou, aes(inc_num, fill=racecat)) + geom_density(alpha=0.2) + labs(x='Income (Multiples of $25k)', fill='Race') + scale_fill_manual(values=c('lightblue2','dodgerblue1','seagreen2','green4','goldenrod','salmon','mediumpurple'))#,'firebrick3'))

violin<-ggplot(aou, aes(factor(x=racecat), y=inc_num), exclude=TRUE) + geom_violin(trim=TRUE)
violin + geom_boxplot(width=0.1)

violin<-ggplot(aou, aes(factor(x=racecat), y=inc_num, fill=factor(diab)), exclude=TRUE) + geom_violin(position=position_dodge(1))
violin + geom_boxplot(position=position_dodge(1), width=0.1)

violin<-ggplot(aou[!is.na(aou$educ_simple),], aes(factor(x=racecat), y=inc_num, fill=factor(educ_simple)), exclude=TRUE) + geom_violin(position=position_dodge(1))
violin + geom_boxplot(position=position_dodge(1), width=0.1)
table(aou$inc_num, exclude=NULL)

aou2 <- aou[!is.na(aou$educ_num) & !is.na(aou$diab),]
aou3 <- aou[!is.na(aou$inc_num) & !is.na(aou$diab),]
dim(aou2) #185k
dim(aou3) #151k
aou3[1:40,c('racecat','inc_num','diab')]
table(aou2$racecat, aou2$diab, exclude=NULL)
summary(aou2$inc_num)
aou2$diab <- ifelse(aou2$diab==0, 'No Diabetes','Diabetes')
aou2$diab <- factor(aou2$diab, levels=c("Diabetes","No Diabetes"))
aou3$diab <- ifelse(aou3$diab==0, 'No Diabetes','Diabetes')
aou3$diab <- factor(aou3$diab, levels=c("Diabetes","No Diabetes"))

dev.off()
png('SuppFig1A_aou_ridgeline_indiveduc_racestrat.png', width=8, height=5, units='in', res=300)
aou2 %>%
  ggplot(aes(y = fct_rev(racecat))) +
  geom_density_ridges(
    aes(x = educ_num, fill = fct_rev(diab)), 
    alpha = .7
  ) +
  labs(
    x = "Educational Attainment",
    y = "",
    title = 'Distribution of Educational Attainment \n by Race/Ethnicity and Diabetes Status'
  ) +
  scale_y_discrete(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  theme_ridges(grid = FALSE, center=T) +
  scale_fill_manual(name='',values=c('#74add1','#d73027'))
dev.off()
#ggsave('SuppFig1A_aou_ridgeline_indiveduc_racestrat.png')

png('SuppFig1B_aou_ridgeline_indivinc_racestrat.png', width=8, height=5, units='in', res=300)
aou3 %>%
  ggplot(aes(y = fct_rev(racecat))) +
  geom_density_ridges(
    aes(x = inc_num, fill = fct_rev(diab)), 
    alpha = .7
  ) +
  labs(
    x = "Income (Multiples of $25k)",
    y = "",
    title = 'Distribution of Income (Multiples of $25k) \n by Race/Ethnicity and Diabetes Status'
  ) +
  scale_y_discrete(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  theme_ridges(grid = FALSE, center=T) + 
  scale_fill_manual(name='',values=c('#74add1','#d73027'))
dev.off()
#ggsave('SuppFig1B_aou_ridgeline_indivinc_racestrat.png')




#####analyses: diabetes ~ categorical and continuous educ and income#####
#educ cat
mod_diab_educ_all <- glm(data=aou, diab ~ age + female + smoke_status + insur_stability + educ_simple, family=binomial(link=logit))
mod_diab_educ_raceadj <- glm(data=aou, diab ~ age + female + smoke_status + insur_stability + educ_simple + racecat, family=binomial(link=logit))
mod_diab_educ_nhw <- glm(data=aou[aou$racecat=='NHW',], diab ~ age + female + smoke_status + insur_stability + educ_simple, family=binomial(link=logit))
mod_diab_educ_nhb <- glm(data=aou[aou$racecat=='NHB',], diab ~ age + female + smoke_status + insur_stability + educ_simple, family=binomial(link=logit))
mod_diab_educ_nha <- glm(data=aou[aou$racecat=='NHAsian',], diab ~ age + female + smoke_status + insur_stability + educ_simple, family=binomial(link=logit))
mod_diab_educ_his <- glm(data=aou[aou$racecat=='Hispanic',], diab ~ age + female + smoke_status + insur_stability + educ_simple, family=binomial(link=logit))
mod_diab_educ_multi <- glm(data=aou[aou$racecat=='Multiracial',], diab ~ age + female + smoke_status + insur_stability + educ_simple, family=binomial(link=logit))
mod_diab_educ_oth <- glm(data=aou[aou$racecat=='Other',], diab ~ age + female + smoke_status + insur_stability + educ_simple, family=binomial(link=logit))
mod_diab_educ_none <- glm(data=aou[aou$racecat=='None of these',], diab ~ age + female + smoke_status + insur_stability + educ_simple, family=binomial(link=logit))
aou_educ_diab_result <- data.frame(exp(mod_diab_educ_all$coefficients),exp(confint(mod_diab_educ_all)),coef(summary(mod_diab_educ_all))[,"Pr(>|z|)"],
                                   exp(mod_diab_educ_raceadj$coefficients)[1:11],exp(confint(mod_diab_educ_raceadj))[1:11,1:2],coef(summary(mod_diab_educ_raceadj))[,"Pr(>|z|)"][1:11],
                                   exp(mod_diab_educ_nhw$coefficients),exp(confint(mod_diab_educ_nhw)),coef(summary(mod_diab_educ_nhw))[,"Pr(>|z|)"],
                                   exp(mod_diab_educ_nhb$coefficients),exp(confint(mod_diab_educ_nhb)),coef(summary(mod_diab_educ_nhb))[,"Pr(>|z|)"],
                                   exp(mod_diab_educ_his$coefficients),exp(confint(mod_diab_educ_his)),coef(summary(mod_diab_educ_his))[,"Pr(>|z|)"],
                                   exp(mod_diab_educ_nha$coefficients),exp(confint(mod_diab_educ_nha)),coef(summary(mod_diab_educ_nha))[,"Pr(>|z|)"],
                                   exp(mod_diab_educ_multi$coefficients),exp(confint(mod_diab_educ_multi)),coef(summary(mod_diab_educ_multi))[,"Pr(>|z|)"],
                                   exp(mod_diab_educ_oth$coefficients),exp(confint(mod_diab_educ_oth)),coef(summary(mod_diab_educ_oth))[,"Pr(>|z|)"],
                                   exp(mod_diab_educ_none$coefficients),exp(confint(mod_diab_educ_none)),coef(summary(mod_diab_educ_none))[,"Pr(>|z|)"],stringsAsFactors = FALSE)
aou_educ_diab_result

#educ cont
mod_diab_educcont_all <- glm(data=aou, diab ~ age + female + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_diab_educcont_raceadj <- glm(data=aou, diab ~ age + female + smoke_status + insur_stability + educ_num + racecat, family=binomial(link=logit))
mod_diab_educcont_nhw <- glm(data=aou[aou$racecat=='NHW',], diab ~ age + female + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_diab_educcont_nhb <- glm(data=aou[aou$racecat=='NHB',], diab ~ age + female + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_diab_educcont_nha <- glm(data=aou[aou$racecat=='NHAsian',], diab ~ age + female + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_diab_educcont_his <- glm(data=aou[aou$racecat=='Hispanic',], diab ~ age + female + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_diab_educcont_multi <- glm(data=aou[aou$racecat=='Multiracial',], diab ~ age + female + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_diab_educcont_oth <- glm(data=aou[aou$racecat=='Other',], diab ~ age + female + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_diab_educcont_none <- glm(data=aou[aou$racecat=='None of these',], diab ~ age + female + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
aou_educcont_diab_result <- data.frame(exp(mod_diab_educcont_all$coefficients),exp(confint(mod_diab_educcont_all)),coef(summary(mod_diab_educcont_all))[,"Pr(>|z|)"],
                                       exp(mod_diab_educcont_raceadj$coefficients)[1:9],exp(confint(mod_diab_educcont_raceadj))[1:9,1:2],coef(summary(mod_diab_educcont_raceadj))[,"Pr(>|z|)"][1:9],
                                       exp(mod_diab_educcont_nhw$coefficients),exp(confint(mod_diab_educcont_nhw)),coef(summary(mod_diab_educcont_nhw))[,"Pr(>|z|)"],
                                       exp(mod_diab_educcont_nhb$coefficients),exp(confint(mod_diab_educcont_nhb)),coef(summary(mod_diab_educcont_nhb))[,"Pr(>|z|)"],
                                       exp(mod_diab_educcont_his$coefficients),exp(confint(mod_diab_educcont_his)),coef(summary(mod_diab_educcont_his))[,"Pr(>|z|)"],
                                       exp(mod_diab_educcont_nha$coefficients),exp(confint(mod_diab_educcont_nha)),coef(summary(mod_diab_educcont_nha))[,"Pr(>|z|)"],
                                       exp(mod_diab_educcont_multi$coefficients),exp(confint(mod_diab_educcont_multi)),coef(summary(mod_diab_educcont_multi))[,"Pr(>|z|)"],
                                       exp(mod_diab_educcont_oth$coefficients),exp(confint(mod_diab_educcont_oth)),coef(summary(mod_diab_educcont_oth))[,"Pr(>|z|)"],
                                       exp(mod_diab_educcont_none$coefficients),exp(confint(mod_diab_educcont_none)),coef(summary(mod_diab_educcont_none))[,"Pr(>|z|)"],stringsAsFactors = FALSE)
aou_educcont_diab_result

#income ordinal
mod_diab_inc_all <- glm(data=aou, diab ~ age + female + smoke_status + insur_stability + inc_simple, family=binomial(link=logit))
mod_diab_inc_raceadj <- glm(data=aou, diab ~ age + female + smoke_status + insur_stability + inc_simple + racecat, family=binomial(link=logit))
mod_diab_inc_nhw <- glm(data=aou[aou$racecat=='NHW',], diab ~ age + female + smoke_status + insur_stability + inc_simple, family=binomial(link=logit))
mod_diab_inc_nhb <- glm(data=aou[aou$racecat=='NHB',], diab ~ age + female + smoke_status + insur_stability + inc_simple, family=binomial(link=logit))
mod_diab_inc_nha <- glm(data=aou[aou$racecat=='NHAsian',], diab ~ age + female + smoke_status + insur_stability + inc_simple, family=binomial(link=logit))
mod_diab_inc_his <- glm(data=aou[aou$racecat=='Hispanic',], diab ~ age + female + smoke_status + insur_stability + inc_simple, family=binomial(link=logit))
mod_diab_inc_multi <- glm(data=aou[aou$racecat=='Multiracial',], diab ~ age + female + smoke_status + insur_stability + inc_simple, family=binomial(link=logit))
mod_diab_inc_oth <- glm(data=aou[aou$racecat=='Other',], diab ~ age + female + smoke_status + insur_stability + inc_simple, family=binomial(link=logit))
mod_diab_inc_none <- glm(data=aou[aou$racecat=='None of these',], diab ~ age + female + smoke_status + insur_stability + inc_simple, family=binomial(link=logit))
#x<-9999
#xx<-c(9999,9999)
aou_inc_diab_result <- data.frame(exp(mod_diab_inc_all$coefficients),exp(confint(mod_diab_inc_all)),coef(summary(mod_diab_inc_all))[,"Pr(>|z|)"],
                                  exp(mod_diab_inc_raceadj$coefficients)[1:12],exp(confint(mod_diab_inc_raceadj))[1:12,1:2],coef(summary(mod_diab_inc_raceadj))[,"Pr(>|z|)"][1:12],
                                  exp(mod_diab_inc_nhw$coefficients),exp(confint(mod_diab_inc_nhw)),coef(summary(mod_diab_inc_nhw))[,"Pr(>|z|)"],
                                  exp(mod_diab_inc_nhb$coefficients),exp(confint(mod_diab_inc_nhb)),coef(summary(mod_diab_inc_nhb))[,"Pr(>|z|)"],
                                  exp(mod_diab_inc_his$coefficients),exp(confint(mod_diab_inc_his)),coef(summary(mod_diab_inc_his))[,"Pr(>|z|)"],
                                  exp(mod_diab_inc_nha$coefficients),exp(confint(mod_diab_inc_nha)),coef(summary(mod_diab_inc_nha))[,"Pr(>|z|)"],
                                  #append(exp(mod_diab_inc_nha$coefficients),x),rbind(exp(confint(mod_diab_inc_nha)),xx),append(coef(summary(mod_diab_inc_nha))[,"Pr(>|t|)"],x),
                                  exp(mod_diab_inc_multi$coefficients),exp(confint(mod_diab_inc_multi)),coef(summary(mod_diab_inc_multi))[,"Pr(>|z|)"],
                                  exp(mod_diab_inc_oth$coefficients),exp(confint(mod_diab_inc_oth)),coef(summary(mod_diab_inc_oth))[,"Pr(>|z|)"],
                                  exp(mod_diab_inc_none$coefficients),exp(confint(mod_diab_inc_none)),coef(summary(mod_diab_inc_none))[,"Pr(>|z|)"],stringsAsFactors = FALSE)
aou_inc_diab_result

#inc continuous
mod_diab_incnum_all <- glm(data=aou, diab ~ age + female + smoke_status + insur_stability + inc_num, family=binomial(link=logit))
mod_diab_incnum_raceadj <- glm(data=aou, diab ~ age + female + smoke_status + insur_stability + inc_num + racecat, family=binomial(link=logit))
mod_diab_incnum_nhw <- glm(data=aou[aou$racecat=='NHW',], diab ~ age + female + smoke_status + insur_stability + inc_num, family=binomial(link=logit))
mod_diab_incnum_nhb <- glm(data=aou[aou$racecat=='NHB',], diab ~ age + female + smoke_status + insur_stability + inc_num, family=binomial(link=logit))
mod_diab_incnum_nha <- glm(data=aou[aou$racecat=='NHAsian',], diab ~ age + female + smoke_status + insur_stability + inc_num, family=binomial(link=logit))
mod_diab_incnum_his <- glm(data=aou[aou$racecat=='Hispanic',], diab ~ age + female + smoke_status + insur_stability + inc_num, family=binomial(link=logit))
mod_diab_incnum_multi <- glm(data=aou[aou$racecat=='Multiracial',], diab ~ age + female + smoke_status + insur_stability + inc_num, family=binomial(link=logit))
mod_diab_incnum_oth <- glm(data=aou[aou$racecat=='Other',], diab ~ age + female + smoke_status + insur_stability + inc_num, family=binomial(link=logit))
mod_diab_incnum_none <- glm(data=aou[aou$racecat=='None of these',], diab ~ age + female + smoke_status + insur_stability + inc_num, family=binomial(link=logit))
aou_incnum_diab_result <- data.frame(exp(mod_diab_incnum_all$coefficients),exp(confint(mod_diab_incnum_all)),coef(summary(mod_diab_incnum_all))[,"Pr(>|z|)"],
                                     exp(mod_diab_incnum_raceadj$coefficients)[1:9],exp(confint(mod_diab_incnum_raceadj))[1:9,1:2],coef(summary(mod_diab_incnum_raceadj))[,"Pr(>|z|)"][1:9],
                                     exp(mod_diab_incnum_nhw$coefficients),exp(confint(mod_diab_incnum_nhw)),coef(summary(mod_diab_incnum_nhw))[,"Pr(>|z|)"],
                                     exp(mod_diab_incnum_nhb$coefficients),exp(confint(mod_diab_incnum_nhb)),coef(summary(mod_diab_incnum_nhb))[,"Pr(>|z|)"],
                                     exp(mod_diab_incnum_his$coefficients),exp(confint(mod_diab_incnum_his)),coef(summary(mod_diab_incnum_his))[,"Pr(>|z|)"],
                                     exp(mod_diab_incnum_nha$coefficients),exp(confint(mod_diab_incnum_nha)),coef(summary(mod_diab_incnum_nha))[,"Pr(>|z|)"],
                                     exp(mod_diab_incnum_multi$coefficients),exp(confint(mod_diab_incnum_multi)),coef(summary(mod_diab_incnum_multi))[,"Pr(>|z|)"],
                                     exp(mod_diab_incnum_oth$coefficients),exp(confint(mod_diab_incnum_oth)),coef(summary(mod_diab_incnum_oth))[,"Pr(>|z|)"],
                                     exp(mod_diab_incnum_none$coefficients),exp(confint(mod_diab_incnum_none)),coef(summary(mod_diab_incnum_none))[,"Pr(>|z|)"],stringsAsFactors = FALSE)
aou_incnum_diab_result

colnames(aou_educ_diab_result) <- c('coeff','lower CI','upper CI','p','coeff','lower CI','upper CI','p','coeff','lower CI','upper CI','p','coeff','lower CI','upper CI','p','coeff','lower CI','upper CI','p','coeff','lower CI','upper CI','p','coeff','lower CI','upper CI','p','coeff','lower CI','upper CI','p','coeff','lower CI','upper CI','p')
colnames(aou_educcont_diab_result) <- c('coeff','lower CI','upper CI','p','coeff','lower CI','upper CI','p','coeff','lower CI','upper CI','p','coeff','lower CI','upper CI','p','coeff','lower CI','upper CI','p','coeff','lower CI','upper CI','p','coeff','lower CI','upper CI','p','coeff','lower CI','upper CI','p','coeff','lower CI','upper CI','p')
colnames(aou_inc_diab_result) <- c('coeff','lower CI','upper CI','p','coeff','lower CI','upper CI','p','coeff','lower CI','upper CI','p','coeff','lower CI','upper CI','p','coeff','lower CI','upper CI','p','coeff','lower CI','upper CI','p','coeff','lower CI','upper CI','p','coeff','lower CI','upper CI','p','coeff','lower CI','upper CI','p')
colnames(aou_incnum_diab_result) <- c('coeff','lower CI','upper CI','p','coeff','lower CI','upper CI','p','coeff','lower CI','upper CI','p','coeff','lower CI','upper CI','p','coeff','lower CI','upper CI','p','coeff','lower CI','upper CI','p','coeff','lower CI','upper CI','p','coeff','lower CI','upper CI','p','coeff','lower CI','upper CI','p')
aou_diab_models <- rbind(aou_educ_diab_result, aou_educcont_diab_result, aou_inc_diab_result, aou_incnum_diab_result)
dim(aou_diab_models)
write_excel_csv(aou_diab_models, "aou_diab_models.csv") 
#aou_diab_models <- read.csv(file='aou_diab_models.csv', header=TRUE, sep=',')
#aou_educ_diab_result <- aou_diab_models[1:11,]
#aou_inc_diab_result <- aou_diab_models[21:32,]

#####analyses: obesity ~ categorical and continuous educ and income#####
#educ cat
mod_ob_educ_all <- glm(data=aou, obese ~ age + female + smoke_status + insur_stability + educ_simple, family=binomial(link=logit))
mod_ob_educ_raceadj <- glm(data=aou, obese ~ age + female + smoke_status + insur_stability + educ_simple + racecat, family=binomial(link=logit))
mod_ob_educ_nhw <- glm(data=aou[aou$racecat=='NHW',], obese ~ age + female + smoke_status + insur_stability + educ_simple, family=binomial(link=logit))
mod_ob_educ_nhb <- glm(data=aou[aou$racecat=='NHB',], obese ~ age + female + smoke_status + insur_stability + educ_simple, family=binomial(link=logit))
mod_ob_educ_nha <- glm(data=aou[aou$racecat=='NHAsian',], obese ~ age + female + smoke_status + insur_stability + educ_simple, family=binomial(link=logit))
mod_ob_educ_his <- glm(data=aou[aou$racecat=='Hispanic',], obese ~ age + female + smoke_status + insur_stability + educ_simple, family=binomial(link=logit))
mod_ob_educ_multi <- glm(data=aou[aou$racecat=='Multiracial',], obese ~ age + female + smoke_status + insur_stability + educ_simple, family=binomial(link=logit))
mod_ob_educ_oth <- glm(data=aou[aou$racecat=='Other',], obese ~ age + female + smoke_status + insur_stability + educ_simple, family=binomial(link=logit))
mod_ob_educ_none <- glm(data=aou[aou$racecat=='None of these',], obese ~ age + female + smoke_status + insur_stability + educ_simple, family=binomial(link=logit))
aou_educ_ob_result <- data.frame(exp(mod_ob_educ_all$coefficients),exp(confint(mod_ob_educ_all)),coef(summary(mod_ob_educ_all))[,"Pr(>|z|)"],
                                 exp(mod_ob_educ_raceadj$coefficients)[1:11],exp(confint(mod_ob_educ_raceadj))[1:11,1:2],coef(summary(mod_ob_educ_raceadj))[,"Pr(>|z|)"][1:11],
                                 exp(mod_ob_educ_nhw$coefficients),exp(confint(mod_ob_educ_nhw)),coef(summary(mod_ob_educ_nhw))[,"Pr(>|z|)"],
                                 exp(mod_ob_educ_nhb$coefficients),exp(confint(mod_ob_educ_nhb)),coef(summary(mod_ob_educ_nhb))[,"Pr(>|z|)"],
                                 exp(mod_ob_educ_his$coefficients),exp(confint(mod_ob_educ_his)),coef(summary(mod_ob_educ_his))[,"Pr(>|z|)"],
                                 exp(mod_ob_educ_nha$coefficients),exp(confint(mod_ob_educ_nha)),coef(summary(mod_ob_educ_nha))[,"Pr(>|z|)"],
                                 exp(mod_ob_educ_multi$coefficients),exp(confint(mod_ob_educ_multi)),coef(summary(mod_ob_educ_multi))[,"Pr(>|z|)"],
                                 exp(mod_ob_educ_oth$coefficients),exp(confint(mod_ob_educ_oth)),coef(summary(mod_ob_educ_oth))[,"Pr(>|z|)"],
                                 exp(mod_ob_educ_none$coefficients),exp(confint(mod_ob_educ_none)),coef(summary(mod_ob_educ_none))[,"Pr(>|z|)"],stringsAsFactors = FALSE)
aou_educ_ob_result

#educ cont
mod_ob_educcont_all <- glm(data=aou, obese ~ age + female + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_ob_educcont_raceadj <- glm(data=aou, obese ~ age + female + smoke_status + insur_stability + educ_num + racecat, family=binomial(link=logit))
mod_ob_educcont_nhw <- glm(data=aou[aou$racecat=='NHW',], obese ~ age + female + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_ob_educcont_nhb <- glm(data=aou[aou$racecat=='NHB',], obese ~ age + female + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_ob_educcont_nha <- glm(data=aou[aou$racecat=='NHAsian',], obese ~ age + female + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_ob_educcont_his <- glm(data=aou[aou$racecat=='Hispanic',], obese ~ age + female + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_ob_educcont_multi <- glm(data=aou[aou$racecat=='Multiracial',], obese ~ age + female + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_ob_educcont_oth <- glm(data=aou[aou$racecat=='Other',], obese ~ age + female + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_ob_educcont_none <- glm(data=aou[aou$racecat=='None of these',], obese ~ age + female + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
aou_educcont_ob_result <- data.frame(exp(mod_ob_educcont_all$coefficients),exp(confint(mod_ob_educcont_all)),coef(summary(mod_ob_educcont_all))[,"Pr(>|z|)"],
                                     exp(mod_ob_educcont_raceadj$coefficients)[1:9],exp(confint(mod_ob_educcont_raceadj))[1:9,1:2],coef(summary(mod_ob_educcont_raceadj))[,"Pr(>|z|)"][1:9],
                                     exp(mod_ob_educcont_nhw$coefficients),exp(confint(mod_ob_educcont_nhw)),coef(summary(mod_ob_educcont_nhw))[,"Pr(>|z|)"],
                                     exp(mod_ob_educcont_nhb$coefficients),exp(confint(mod_ob_educcont_nhb)),coef(summary(mod_ob_educcont_nhb))[,"Pr(>|z|)"],
                                     exp(mod_ob_educcont_his$coefficients),exp(confint(mod_ob_educcont_his)),coef(summary(mod_ob_educcont_his))[,"Pr(>|z|)"],
                                     exp(mod_ob_educcont_nha$coefficients),exp(confint(mod_ob_educcont_nha)),coef(summary(mod_ob_educcont_nha))[,"Pr(>|z|)"],
                                     exp(mod_ob_educcont_multi$coefficients),exp(confint(mod_ob_educcont_multi)),coef(summary(mod_ob_educcont_multi))[,"Pr(>|z|)"],
                                     exp(mod_ob_educcont_oth$coefficients),exp(confint(mod_ob_educcont_oth)),coef(summary(mod_ob_educcont_oth))[,"Pr(>|z|)"],
                                     exp(mod_ob_educcont_none$coefficients),exp(confint(mod_ob_educcont_none)),coef(summary(mod_ob_educcont_none))[,"Pr(>|z|)"],stringsAsFactors = FALSE)
aou_educcont_ob_result

#income ordinal
mod_ob_inc_all <- glm(data=aou, obese ~ age + female + smoke_status + insur_stability + inc_simple, family=binomial(link=logit))
mod_ob_inc_raceadj <- glm(data=aou, obese ~ age + female + smoke_status + insur_stability + inc_simple + racecat, family=binomial(link=logit))
mod_ob_inc_nhw <- glm(data=aou[aou$racecat=='NHW',], obese ~ age + female + smoke_status + insur_stability + inc_simple, family=binomial(link=logit))
mod_ob_inc_nhb <- glm(data=aou[aou$racecat=='NHB',], obese ~ age + female + smoke_status + insur_stability + inc_simple, family=binomial(link=logit))
mod_ob_inc_nha <- glm(data=aou[aou$racecat=='NHAsian',], obese ~ age + female + smoke_status + insur_stability + inc_simple, family=binomial(link=logit))
mod_ob_inc_his <- glm(data=aou[aou$racecat=='Hispanic',], obese ~ age + female + smoke_status + insur_stability + inc_simple, family=binomial(link=logit))
mod_ob_inc_multi <- glm(data=aou[aou$racecat=='Multiracial',], obese ~ age + female + smoke_status + insur_stability + inc_simple, family=binomial(link=logit))
mod_ob_inc_oth <- glm(data=aou[aou$racecat=='Other',], obese ~ age + female + smoke_status + insur_stability + inc_simple, family=binomial(link=logit))
mod_ob_inc_none <- glm(data=aou[aou$racecat=='None of these',], obese ~ age + female + smoke_status + insur_stability + inc_simple, family=binomial(link=logit))
#x<-9999
#xx<-c(9999,9999)
aou_inc_ob_result <- data.frame(exp(mod_ob_inc_all$coefficients),exp(confint(mod_ob_inc_all)),coef(summary(mod_ob_inc_all))[,"Pr(>|z|)"],
                                exp(mod_ob_inc_raceadj$coefficients)[1:12],exp(confint(mod_ob_inc_raceadj))[1:12,1:2],coef(summary(mod_ob_inc_raceadj))[,"Pr(>|z|)"][1:12],
                                exp(mod_ob_inc_nhw$coefficients),exp(confint(mod_ob_inc_nhw)),coef(summary(mod_ob_inc_nhw))[,"Pr(>|z|)"],
                                exp(mod_ob_inc_nhb$coefficients),exp(confint(mod_ob_inc_nhb)),coef(summary(mod_ob_inc_nhb))[,"Pr(>|z|)"],
                                exp(mod_ob_inc_his$coefficients),exp(confint(mod_ob_inc_his)),coef(summary(mod_ob_inc_his))[,"Pr(>|z|)"],
                                exp(mod_ob_inc_nha$coefficients),exp(confint(mod_ob_inc_nha)),coef(summary(mod_ob_inc_nha))[,"Pr(>|z|)"],
                                #append(exp(mod_ob_inc_nha$coefficients),x),rbind(exp(confint(mod_ob_inc_nha)),xx),append(coef(summary(mod_ob_inc_nha))[,"Pr(>|t|)"],x),
                                exp(mod_ob_inc_multi$coefficients),exp(confint(mod_ob_inc_multi)),coef(summary(mod_ob_inc_multi))[,"Pr(>|z|)"],
                                exp(mod_ob_inc_oth$coefficients),exp(confint(mod_ob_inc_oth)),coef(summary(mod_ob_inc_oth))[,"Pr(>|z|)"],
                                exp(mod_ob_inc_none$coefficients),exp(confint(mod_ob_inc_none)),coef(summary(mod_ob_inc_none))[,"Pr(>|z|)"],stringsAsFactors = FALSE)
aou_inc_ob_result

#inc continuous
mod_ob_incnum_all <- glm(data=aou, obese ~ age + female + smoke_status + insur_stability + inc_num, family=binomial(link=logit))
mod_ob_incnum_raceadj <- glm(data=aou, obese ~ age + female + smoke_status + insur_stability + inc_num + racecat, family=binomial(link=logit))
mod_ob_incnum_nhw <- glm(data=aou[aou$racecat=='NHW',], obese ~ age + female + smoke_status + insur_stability + inc_num, family=binomial(link=logit))
mod_ob_incnum_nhb <- glm(data=aou[aou$racecat=='NHB',], obese ~ age + female + smoke_status + insur_stability + inc_num, family=binomial(link=logit))
mod_ob_incnum_nha <- glm(data=aou[aou$racecat=='NHAsian',], obese ~ age + female + smoke_status + insur_stability + inc_num, family=binomial(link=logit))
mod_ob_incnum_his <- glm(data=aou[aou$racecat=='Hispanic',], obese ~ age + female + smoke_status + insur_stability + inc_num, family=binomial(link=logit))
mod_ob_incnum_multi <- glm(data=aou[aou$racecat=='Multiracial',], obese ~ age + female + smoke_status + insur_stability + inc_num, family=binomial(link=logit))
mod_ob_incnum_oth <- glm(data=aou[aou$racecat=='Other',], obese ~ age + female + smoke_status + insur_stability + inc_num, family=binomial(link=logit))
mod_ob_incnum_none <- glm(data=aou[aou$racecat=='None of these',], obese ~ age + female + smoke_status + insur_stability + inc_num, family=binomial(link=logit))
aou_incnum_ob_result <- data.frame(exp(mod_ob_incnum_all$coefficients),exp(confint(mod_ob_incnum_all)),coef(summary(mod_ob_incnum_all))[,"Pr(>|z|)"],
                                   exp(mod_ob_incnum_raceadj$coefficients)[1:9],exp(confint(mod_ob_incnum_raceadj))[1:9,1:2],coef(summary(mod_ob_incnum_raceadj))[,"Pr(>|z|)"][1:9],
                                   exp(mod_ob_incnum_nhw$coefficients),exp(confint(mod_ob_incnum_nhw)),coef(summary(mod_ob_incnum_nhw))[,"Pr(>|z|)"],
                                   exp(mod_ob_incnum_nhb$coefficients),exp(confint(mod_ob_incnum_nhb)),coef(summary(mod_ob_incnum_nhb))[,"Pr(>|z|)"],
                                   exp(mod_ob_incnum_his$coefficients),exp(confint(mod_ob_incnum_his)),coef(summary(mod_ob_incnum_his))[,"Pr(>|z|)"],
                                   exp(mod_ob_incnum_nha$coefficients),exp(confint(mod_ob_incnum_nha)),coef(summary(mod_ob_incnum_nha))[,"Pr(>|z|)"],
                                   exp(mod_ob_incnum_multi$coefficients),exp(confint(mod_ob_incnum_multi)),coef(summary(mod_ob_incnum_multi))[,"Pr(>|z|)"],
                                   exp(mod_ob_incnum_oth$coefficients),exp(confint(mod_ob_incnum_oth)),coef(summary(mod_ob_incnum_oth))[,"Pr(>|z|)"],
                                   exp(mod_ob_incnum_none$coefficients),exp(confint(mod_ob_incnum_none)),coef(summary(mod_ob_incnum_none))[,"Pr(>|z|)"],stringsAsFactors = FALSE)
aou_incnum_ob_result

colnames(aou_educ_ob_result) <- c('coeff','lower CI','upper CI','p','coeff','lower CI','upper CI','p','coeff','lower CI','upper CI','p','coeff','lower CI','upper CI','p','coeff','lower CI','upper CI','p','coeff','lower CI','upper CI','p','coeff','lower CI','upper CI','p','coeff','lower CI','upper CI','p','coeff','lower CI','upper CI','p')
colnames(aou_educcont_ob_result) <- c('coeff','lower CI','upper CI','p','coeff','lower CI','upper CI','p','coeff','lower CI','upper CI','p','coeff','lower CI','upper CI','p','coeff','lower CI','upper CI','p','coeff','lower CI','upper CI','p','coeff','lower CI','upper CI','p','coeff','lower CI','upper CI','p','coeff','lower CI','upper CI','p')
colnames(aou_inc_ob_result) <- c('coeff','lower CI','upper CI','p','coeff','lower CI','upper CI','p','coeff','lower CI','upper CI','p','coeff','lower CI','upper CI','p','coeff','lower CI','upper CI','p','coeff','lower CI','upper CI','p','coeff','lower CI','upper CI','p','coeff','lower CI','upper CI','p','coeff','lower CI','upper CI','p')
colnames(aou_incnum_ob_result) <- c('coeff','lower CI','upper CI','p','coeff','lower CI','upper CI','p','coeff','lower CI','upper CI','p','coeff','lower CI','upper CI','p','coeff','lower CI','upper CI','p','coeff','lower CI','upper CI','p','coeff','lower CI','upper CI','p','coeff','lower CI','upper CI','p','coeff','lower CI','upper CI','p')
aou_ob_models <- rbind(aou_educ_ob_result, aou_educcont_ob_result, aou_inc_ob_result, aou_incnum_ob_result)
dim(aou_ob_models)
write_excel_csv(aou_ob_models, "aou_ob_models.csv") 



#####sensitivity analyses - quasibinomial and modified poisson and poisson#####
#educ cont
mod_diab_educcont_all_qb <- glm(data=aou, diab ~ age + female + smoke_status + insur_stability + educ_num, family=quasibinomial)
mod_diab_educcont_raceadj_qb <- glm(data=aou, diab ~ age + female + smoke_status + insur_stability + educ_num + racecat, family=quasibinomial)
mod_diab_educcont_nhw_qb <- glm(data=aou[aou$racecat=='NHW',], diab ~ age + female + smoke_status + insur_stability + educ_num, family=quasibinomial)
mod_diab_educcont_nhb_qb <- glm(data=aou[aou$racecat=='NHB',], diab ~ age + female + smoke_status + insur_stability + educ_num, family=quasibinomial)
mod_diab_educcont_his_qb <- glm(data=aou[aou$racecat=='Hispanic',], diab ~ age + female + smoke_status + insur_stability + educ_num, family=quasibinomial)
mod_diab_educcont_nha_qb <- glm(data=aou[aou$racecat=='NHAsian',], diab ~ age + female + smoke_status + insur_stability + educ_num, family=quasibinomial)
mod_diab_educcont_multi_qb <- glm(data=aou[aou$racecat=='Multiracial',], diab ~ age + female + smoke_status + insur_stability + educ_num, family=quasibinomial)
mod_diab_educcont_oth_qb <- glm(data=aou[aou$racecat=='Other',], diab ~ age + female + smoke_status + insur_stability + educ_num, family=quasibinomial)
mod_diab_educcont_none_qb <- glm(data=aou[aou$racecat=='None of these',], diab ~ age + female + smoke_status + insur_stability + educ_num, family=quasibinomial)
aou_educcont_diab_qb_result <- data.frame(exp(mod_diab_educcont_all_qb$coefficients),exp(confint(mod_diab_educcont_all_qb)),coef(summary(mod_diab_educcont_all_qb))[,"Pr(>|t|)"],
                                          exp(mod_diab_educcont_raceadj_qb$coefficients)[1:9],exp(confint(mod_diab_educcont_raceadj_qb))[1:9,1:2],coef(summary(mod_diab_educcont_raceadj_qb))[,"Pr(>|t|)"][1:9],
                                          exp(mod_diab_educcont_nhw_qb$coefficients),exp(confint(mod_diab_educcont_nhw_qb)),coef(summary(mod_diab_educcont_nhw_qb))[,"Pr(>|t|)"],
                                          exp(mod_diab_educcont_nhb_qb$coefficients),exp(confint(mod_diab_educcont_nhb_qb)),coef(summary(mod_diab_educcont_nhb_qb))[,"Pr(>|t|)"],
                                          exp(mod_diab_educcont_his_qb$coefficients),exp(confint(mod_diab_educcont_his_qb)),coef(summary(mod_diab_educcont_his_qb))[,"Pr(>|t|)"],
                                          exp(mod_diab_educcont_nha_qb$coefficients),exp(confint(mod_diab_educcont_nha_qb)),coef(summary(mod_diab_educcont_nha_qb))[,"Pr(>|t|)"],
                                          exp(mod_diab_educcont_multi_qb$coefficients),exp(confint(mod_diab_educcont_multi_qb)),coef(summary(mod_diab_educcont_multi_qb))[,"Pr(>|t|)"],
                                          exp(mod_diab_educcont_oth_qb$coefficients),exp(confint(mod_diab_educcont_oth_qb)),coef(summary(mod_diab_educcont_oth_qb))[,"Pr(>|t|)"],
                                          exp(mod_diab_educcont_none_qb$coefficients),exp(confint(mod_diab_educcont_none_qb)),coef(summary(mod_diab_educcont_none_qb))[,"Pr(>|t|)"],stringsAsFactors = FALSE)
colnames(aou_educcont_diab_qb_result) <- c('coef','lower CI','upper CI','p','coef','lower CI','upper CI','p','coef','lower CI','upper CI','p','coef','lower CI','upper CI','p','coef','lower CI','upper CI','p','coef','lower CI','upper CI','p','coef','lower CI','upper CI','p','coef','lower CI','upper CI','p','coef','lower CI','upper CI','p')
aou_educcont_diab_qb_result
summary(mod_diab_educcont_all_qb)

mod_ob_educcont_all_qb <- glm(data=aou, obese ~ age + female + smoke_status + insur_stability + educ_num, family=quasibinomial)
mod_ob_educcont_raceadj_qb <- glm(data=aou, obese ~ age + female + smoke_status + insur_stability + educ_num + racecat, family=quasibinomial)
mod_ob_educcont_nhw_qb <- glm(data=aou[aou$racecat=='NHW',], obese ~ age + female + smoke_status + insur_stability + educ_num, family=quasibinomial)
mod_ob_educcont_nhb_qb <- glm(data=aou[aou$racecat=='NHB',], obese ~ age + female + smoke_status + insur_stability + educ_num, family=quasibinomial)
mod_ob_educcont_his_qb <- glm(data=aou[aou$racecat=='Hispanic',], obese ~ age + female + smoke_status + insur_stability + educ_num, family=quasibinomial)
mod_ob_educcont_nha_qb <- glm(data=aou[aou$racecat=='NHAsian',], obese ~ age + female + smoke_status + insur_stability + educ_num, family=quasibinomial)
mod_ob_educcont_multi_qb <- glm(data=aou[aou$racecat=='Multiracial',], obese ~ age + female + smoke_status + insur_stability + educ_num, family=quasibinomial)
mod_ob_educcont_oth_qb <- glm(data=aou[aou$racecat=='Other',], obese ~ age + female + smoke_status + insur_stability + educ_num, family=quasibinomial)
mod_ob_educcont_none_qb <- glm(data=aou[aou$racecat=='None of these',], obese ~ age + female + smoke_status + insur_stability + educ_num, family=quasibinomial)
aou_educcont_ob_qb_result <- data.frame(exp(mod_ob_educcont_all_qb$coefficients),exp(confint(mod_ob_educcont_all_qb)),coef(summary(mod_ob_educcont_all_qb))[,"Pr(>|t|)"],
                                        exp(mod_ob_educcont_raceadj_qb$coefficients)[1:9],exp(confint(mod_ob_educcont_raceadj_qb))[1:9,1:2],coef(summary(mod_ob_educcont_raceadj_qb))[,"Pr(>|t|)"][1:9],
                                        exp(mod_ob_educcont_nhw_qb$coefficients),exp(confint(mod_ob_educcont_nhw_qb)),coef(summary(mod_ob_educcont_nhw_qb))[,"Pr(>|t|)"],
                                        exp(mod_ob_educcont_nhb_qb$coefficients),exp(confint(mod_ob_educcont_nhb_qb)),coef(summary(mod_ob_educcont_nhb_qb))[,"Pr(>|t|)"],
                                        exp(mod_ob_educcont_his_qb$coefficients),exp(confint(mod_ob_educcont_his_qb)),coef(summary(mod_ob_educcont_his_qb))[,"Pr(>|t|)"],
                                        exp(mod_ob_educcont_nha_qb$coefficients),exp(confint(mod_ob_educcont_nha_qb)),coef(summary(mod_ob_educcont_nha_qb))[,"Pr(>|t|)"],
                                        exp(mod_ob_educcont_multi_qb$coefficients),exp(confint(mod_ob_educcont_multi_qb)),coef(summary(mod_ob_educcont_multi_qb))[,"Pr(>|t|)"],
                                        exp(mod_ob_educcont_oth_qb$coefficients),exp(confint(mod_ob_educcont_oth_qb)),coef(summary(mod_ob_educcont_oth_qb))[,"Pr(>|t|)"],
                                        exp(mod_ob_educcont_none_qb$coefficients),exp(confint(mod_ob_educcont_none_qb)),coef(summary(mod_ob_educcont_none_qb))[,"Pr(>|t|)"],stringsAsFactors = FALSE)
colnames(aou_educcont_ob_qb_result) <- c('coef','lower CI','upper CI','p','coef','lower CI','upper CI','p','coef','lower CI','upper CI','p','coef','lower CI','upper CI','p','coef','lower CI','upper CI','p','coef','lower CI','upper CI','p','coef','lower CI','upper CI','p','coef','lower CI','upper CI','p','coef','lower CI','upper CI','p')
aou_educcont_ob_qb_result

aou_educcont_diabob_qb_result <- cbind(aou_educcont_diab_qb_result,aou_educcont_ob_qb_result)


mod_diab_educcont_all_mp <- glm(data=aou, diab ~ age + female + smoke_status + insur_stability + educ_num, family=poisson(link=log))
mod_diab_educcont_all_mp_robust <- as.data.frame(coeftest(mod_diab_educcont_all_mp, vcov=sandwich)[,])
mod_diab_educcont_all_mp_robust_values <- data.frame(exp(mod_diab_educcont_all_mp_robust$Estimate), exp(mod_diab_educcont_all_mp_robust$Estimate - qnorm(1-0.05)/2*mod_diab_educcont_all_mp_robust$'Std. Error'), exp(mod_diab_educcont_all_mp_robust$Estimate + qnorm(1-0.05)/2*mod_diab_educcont_all_mp_robust$'Std. Error'), mod_diab_educcont_all_mp_robust$`Pr(>|z|)`)
colnames(mod_diab_educcont_all_mp_robust_values) <- c('coef','lower CI','upper CI','p')

mod_diab_educcont_raceadj_mp <- glm(data=aou, diab ~ age + female + smoke_status + insur_stability + educ_num + racecat, family=poisson(link=log))
mod_diab_educcont_raceadj_mp_robust <- as.data.frame(coeftest(mod_diab_educcont_raceadj_mp, vcov=sandwich)[,])
mod_diab_educcont_nhw_mp <- glm(data=aou[aou$racecat=='NHW',], diab ~ age + female + smoke_status + insur_stability + educ_num, family=poisson(link=log))
mod_diab_educcont_nhw_mp_robust <- as.data.frame(coeftest(mod_diab_educcont_nhw_mp, vcov=sandwich)[,])
mod_diab_educcont_nhb_mp <- glm(data=aou[aou$racecat=='NHB',], diab ~ age + female + smoke_status + insur_stability + educ_num, family=poisson(link=log))
mod_diab_educcont_nhb_mp_robust <- as.data.frame(coeftest(mod_diab_educcont_nhb_mp, vcov=sandwich)[,])
mod_diab_educcont_his_mp <- glm(data=aou[aou$racecat=='Hispanic',], diab ~ age + female + smoke_status + insur_stability + educ_num, family=poisson(link=log))
mod_diab_educcont_his_mp_robust <- as.data.frame(coeftest(mod_diab_educcont_his_mp, vcov=sandwich)[,])
mod_diab_educcont_nha_mp <- glm(data=aou[aou$racecat=='NHAsian',], diab ~ age + female + smoke_status + insur_stability + educ_num, family=poisson(link=log))
mod_diab_educcont_nha_mp_robust <- as.data.frame(coeftest(mod_diab_educcont_nha_mp, vcov=sandwich)[,])
mod_diab_educcont_multi_mp <- glm(data=aou[aou$racecat=='Multiracial',], diab ~ age + female + smoke_status + insur_stability + educ_num, family=poisson(link=log))
mod_diab_educcont_multi_mp_robust <- as.data.frame(coeftest(mod_diab_educcont_multi_mp, vcov=sandwich)[,])
mod_diab_educcont_oth_mp <- glm(data=aou[aou$racecat=='Other',], diab ~ age + female + smoke_status + insur_stability + educ_num, family=poisson(link=log))
mod_diab_educcont_oth_mp_robust <- as.data.frame(coeftest(mod_diab_educcont_oth_mp, vcov=sandwich)[,])
mod_diab_educcont_none_mp <- glm(data=aou[aou$racecat=='None of these',], diab ~ age + female + smoke_status + insur_stability + educ_num, family=poisson(link=log))
mod_diab_educcont_none_mp_robust <- as.data.frame(coeftest(mod_diab_educcont_none_mp, vcov=sandwich)[,])

aou_educcont_diab_mp_result <- data.frame(exp(mod_diab_educcont_all_mp_robust$Estimate), exp(mod_diab_educcont_all_mp_robust$Estimate - qnorm(1-0.05)/2*mod_diab_educcont_all_mp_robust$'Std. Error'), exp(mod_diab_educcont_all_mp_robust$Estimate + qnorm(1-0.05)/2*mod_diab_educcont_all_mp_robust$'Std. Error'), mod_diab_educcont_all_mp_robust$`Pr(>|z|)`,
                                          exp(mod_diab_educcont_raceadj_mp_robust$Estimate)[1:9], exp(mod_diab_educcont_raceadj_mp_robust$Estimate - qnorm(1-0.05)/2*mod_diab_educcont_raceadj_mp_robust$'Std. Error')[1:9], exp(mod_diab_educcont_raceadj_mp_robust$Estimate + qnorm(1-0.05)/2*mod_diab_educcont_raceadj_mp_robust$'Std. Error')[1:9], mod_diab_educcont_raceadj_mp_robust$`Pr(>|z|)`[1:9],
                                          exp(mod_diab_educcont_nhw_mp_robust$Estimate), exp(mod_diab_educcont_nhw_mp_robust$Estimate - qnorm(1-0.05)/2*mod_diab_educcont_nhw_mp_robust$'Std. Error'), exp(mod_diab_educcont_nhw_mp_robust$Estimate + qnorm(1-0.05)/2*mod_diab_educcont_nhw_mp_robust$'Std. Error'), mod_diab_educcont_nhw_mp_robust$`Pr(>|z|)`,
                                          exp(mod_diab_educcont_nhb_mp_robust$Estimate), exp(mod_diab_educcont_nhb_mp_robust$Estimate - qnorm(1-0.05)/2*mod_diab_educcont_nhb_mp_robust$'Std. Error'), exp(mod_diab_educcont_nhb_mp_robust$Estimate + qnorm(1-0.05)/2*mod_diab_educcont_nhb_mp_robust$'Std. Error'), mod_diab_educcont_nhb_mp_robust$`Pr(>|z|)`,
                                          exp(mod_diab_educcont_his_mp_robust$Estimate), exp(mod_diab_educcont_his_mp_robust$Estimate - qnorm(1-0.05)/2*mod_diab_educcont_his_mp_robust$'Std. Error'), exp(mod_diab_educcont_his_mp_robust$Estimate + qnorm(1-0.05)/2*mod_diab_educcont_his_mp_robust$'Std. Error'), mod_diab_educcont_his_mp_robust$`Pr(>|z|)`,
                                          exp(mod_diab_educcont_nha_mp_robust$Estimate), exp(mod_diab_educcont_nha_mp_robust$Estimate - qnorm(1-0.05)/2*mod_diab_educcont_nha_mp_robust$'Std. Error'), exp(mod_diab_educcont_nha_mp_robust$Estimate + qnorm(1-0.05)/2*mod_diab_educcont_nha_mp_robust$'Std. Error'), mod_diab_educcont_nha_mp_robust$`Pr(>|z|)`,
                                          exp(mod_diab_educcont_multi_mp_robust$Estimate), exp(mod_diab_educcont_multi_mp_robust$Estimate - qnorm(1-0.05)/2*mod_diab_educcont_multi_mp_robust$'Std. Error'), exp(mod_diab_educcont_multi_mp_robust$Estimate + qnorm(1-0.05)/2*mod_diab_educcont_multi_mp_robust$'Std. Error'), mod_diab_educcont_multi_mp_robust$`Pr(>|z|)`,
                                          exp(mod_diab_educcont_oth_mp_robust$Estimate), exp(mod_diab_educcont_oth_mp_robust$Estimate - qnorm(1-0.05)/2*mod_diab_educcont_oth_mp_robust$'Std. Error'), exp(mod_diab_educcont_oth_mp_robust$Estimate + qnorm(1-0.05)/2*mod_diab_educcont_oth_mp_robust$'Std. Error'), mod_diab_educcont_oth_mp_robust$`Pr(>|z|)`,
                                          exp(mod_diab_educcont_none_mp_robust$Estimate), exp(mod_diab_educcont_none_mp_robust$Estimate - qnorm(1-0.05)/2*mod_diab_educcont_none_mp_robust$'Std. Error'), exp(mod_diab_educcont_none_mp_robust$Estimate + qnorm(1-0.05)/2*mod_diab_educcont_none_mp_robust$'Std. Error'), mod_diab_educcont_none_mp_robust$`Pr(>|z|)`, stringsAsFactors=F)
colnames(aou_educcont_diab_mp_result) <- c('coef','lower CI','upper CI','p','coef','lower CI','upper CI','p','coef','lower CI','upper CI','p','coef','lower CI','upper CI','p','coef','lower CI','upper CI','p','coef','lower CI','upper CI','p','coef','lower CI','upper CI','p','coef','lower CI','upper CI','p','coef','lower CI','upper CI','p')
aou_educcont_diab_mp_result


mod_ob_educcont_all_mp <- glm(data=aou, obese ~ age + female + smoke_status + insur_stability + educ_num, family=poisson(link=log))
mod_ob_educcont_all_mp_robust <- as.data.frame(coeftest(mod_ob_educcont_all_mp, vcov=sandwich)[,])
mod_ob_educcont_raceadj_mp <- glm(data=aou, obese ~ age + female + smoke_status + insur_stability + educ_num + racecat, family=poisson(link=log))
mod_ob_educcont_raceadj_mp_robust <- as.data.frame(coeftest(mod_ob_educcont_raceadj_mp, vcov=sandwich)[,])
mod_ob_educcont_nhw_mp <- glm(data=aou[aou$racecat=='NHW',], obese ~ age + female + smoke_status + insur_stability + educ_num, family=poisson(link=log))
mod_ob_educcont_nhw_mp_robust <- as.data.frame(coeftest(mod_ob_educcont_nhw_mp, vcov=sandwich)[,])
mod_ob_educcont_nhb_mp <- glm(data=aou[aou$racecat=='NHB',], obese ~ age + female + smoke_status + insur_stability + educ_num, family=poisson(link=log))
mod_ob_educcont_nhb_mp_robust <- as.data.frame(coeftest(mod_ob_educcont_nhb_mp, vcov=sandwich)[,])
mod_ob_educcont_his_mp <- glm(data=aou[aou$racecat=='Hispanic',], obese ~ age + female + smoke_status + insur_stability + educ_num, family=poisson(link=log))
mod_ob_educcont_his_mp_robust <- as.data.frame(coeftest(mod_ob_educcont_his_mp, vcov=sandwich)[,])
mod_ob_educcont_nha_mp <- glm(data=aou[aou$racecat=='NHAsian',], obese ~ age + female + smoke_status + insur_stability + educ_num, family=poisson(link=log))
mod_ob_educcont_nha_mp_robust <- as.data.frame(coeftest(mod_ob_educcont_nha_mp, vcov=sandwich)[,])
mod_ob_educcont_multi_mp <- glm(data=aou[aou$racecat=='Multiracial',], obese ~ age + female + smoke_status + insur_stability + educ_num, family=poisson(link=log))
mod_ob_educcont_multi_mp_robust <- as.data.frame(coeftest(mod_ob_educcont_multi_mp, vcov=sandwich)[,])
mod_ob_educcont_oth_mp <- glm(data=aou[aou$racecat=='Other',], obese ~ age + female + smoke_status + insur_stability + educ_num, family=poisson(link=log))
mod_ob_educcont_oth_mp_robust <- as.data.frame(coeftest(mod_ob_educcont_oth_mp, vcov=sandwich)[,])
mod_ob_educcont_none_mp <- glm(data=aou[aou$racecat=='None of these',], obese ~ age + female + smoke_status + insur_stability + educ_num, family=poisson(link=log))
mod_ob_educcont_none_mp_robust <- as.data.frame(coeftest(mod_ob_educcont_none_mp, vcov=sandwich)[,])

aou_educcont_ob_mp_result <- data.frame(exp(mod_ob_educcont_all_mp_robust$Estimate), exp(mod_ob_educcont_all_mp_robust$Estimate - qnorm(1-0.05)/2*mod_ob_educcont_all_mp_robust$'Std. Error'), exp(mod_ob_educcont_all_mp_robust$Estimate + qnorm(1-0.05)/2*mod_ob_educcont_all_mp_robust$'Std. Error'), mod_ob_educcont_all_mp_robust$`Pr(>|z|)`,
                                        exp(mod_ob_educcont_raceadj_mp_robust$Estimate)[1:9], exp(mod_ob_educcont_raceadj_mp_robust$Estimate - qnorm(1-0.05)/2*mod_ob_educcont_raceadj_mp_robust$'Std. Error')[1:9], exp(mod_ob_educcont_raceadj_mp_robust$Estimate + qnorm(1-0.05)/2*mod_ob_educcont_raceadj_mp_robust$'Std. Error')[1:9], mod_ob_educcont_raceadj_mp_robust$`Pr(>|z|)`[1:9],
                                        exp(mod_ob_educcont_nhw_mp_robust$Estimate), exp(mod_ob_educcont_nhw_mp_robust$Estimate - qnorm(1-0.05)/2*mod_ob_educcont_nhw_mp_robust$'Std. Error'), exp(mod_ob_educcont_nhw_mp_robust$Estimate + qnorm(1-0.05)/2*mod_ob_educcont_nhw_mp_robust$'Std. Error'), mod_ob_educcont_nhw_mp_robust$`Pr(>|z|)`,
                                        exp(mod_ob_educcont_nhb_mp_robust$Estimate), exp(mod_ob_educcont_nhb_mp_robust$Estimate - qnorm(1-0.05)/2*mod_ob_educcont_nhb_mp_robust$'Std. Error'), exp(mod_ob_educcont_nhb_mp_robust$Estimate + qnorm(1-0.05)/2*mod_ob_educcont_nhb_mp_robust$'Std. Error'), mod_ob_educcont_nhb_mp_robust$`Pr(>|z|)`,
                                        exp(mod_ob_educcont_his_mp_robust$Estimate), exp(mod_ob_educcont_his_mp_robust$Estimate - qnorm(1-0.05)/2*mod_ob_educcont_his_mp_robust$'Std. Error'), exp(mod_ob_educcont_his_mp_robust$Estimate + qnorm(1-0.05)/2*mod_ob_educcont_his_mp_robust$'Std. Error'), mod_ob_educcont_his_mp_robust$`Pr(>|z|)`,
                                        exp(mod_ob_educcont_nha_mp_robust$Estimate), exp(mod_ob_educcont_nha_mp_robust$Estimate - qnorm(1-0.05)/2*mod_ob_educcont_nha_mp_robust$'Std. Error'), exp(mod_ob_educcont_nha_mp_robust$Estimate + qnorm(1-0.05)/2*mod_ob_educcont_nha_mp_robust$'Std. Error'), mod_ob_educcont_nha_mp_robust$`Pr(>|z|)`,
                                        exp(mod_ob_educcont_multi_mp_robust$Estimate), exp(mod_ob_educcont_multi_mp_robust$Estimate - qnorm(1-0.05)/2*mod_ob_educcont_multi_mp_robust$'Std. Error'), exp(mod_ob_educcont_multi_mp_robust$Estimate + qnorm(1-0.05)/2*mod_ob_educcont_multi_mp_robust$'Std. Error'), mod_ob_educcont_multi_mp_robust$`Pr(>|z|)`,
                                        exp(mod_ob_educcont_oth_mp_robust$Estimate), exp(mod_ob_educcont_oth_mp_robust$Estimate - qnorm(1-0.05)/2*mod_ob_educcont_oth_mp_robust$'Std. Error'), exp(mod_ob_educcont_oth_mp_robust$Estimate + qnorm(1-0.05)/2*mod_ob_educcont_oth_mp_robust$'Std. Error'), mod_ob_educcont_oth_mp_robust$`Pr(>|z|)`,
                                        exp(mod_ob_educcont_none_mp_robust$Estimate), exp(mod_ob_educcont_none_mp_robust$Estimate - qnorm(1-0.05)/2*mod_ob_educcont_none_mp_robust$'Std. Error'), exp(mod_ob_educcont_none_mp_robust$Estimate + qnorm(1-0.05)/2*mod_ob_educcont_none_mp_robust$'Std. Error'), mod_ob_educcont_none_mp_robust$`Pr(>|z|)`, stringsAsFactors=F)
colnames(aou_educcont_ob_mp_result) <- c('coef','lower CI','upper CI','p','coef','lower CI','upper CI','p','coef','lower CI','upper CI','p','coef','lower CI','upper CI','p','coef','lower CI','upper CI','p','coef','lower CI','upper CI','p','coef','lower CI','upper CI','p','coef','lower CI','upper CI','p','coef','lower CI','upper CI','p')
aou_educcont_ob_mp_result

aou_educcont_diabob_mp_result <- cbind(aou_educcont_diab_mp_result,aou_educcont_ob_mp_result)



mod_diab_educcont_all_pois <- glm(data=aou, diab ~ age + female + smoke_status + insur_stability + educ_num, family=poisson(link=log))
mod_diab_educcont_raceadj_pois <- glm(data=aou, diab ~ age + female + smoke_status + insur_stability + educ_num + racecat, family=poisson(link=log))
mod_diab_educcont_nhw_pois <- glm(data=aou[aou$racecat=='NHW',], diab ~ age + female + smoke_status + insur_stability + educ_num, family=poisson(link=log))
mod_diab_educcont_nhb_pois <- glm(data=aou[aou$racecat=='NHB',], diab ~ age + female + smoke_status + insur_stability + educ_num, family=poisson(link=log))
mod_diab_educcont_his_pois <- glm(data=aou[aou$racecat=='Hispanic',], diab ~ age + female + smoke_status + insur_stability + educ_num, family=poisson(link=log))
mod_diab_educcont_nha_pois <- glm(data=aou[aou$racecat=='NHAsian',], diab ~ age + female + smoke_status + insur_stability + educ_num, family=poisson(link=log))
mod_diab_educcont_multi_pois <- glm(data=aou[aou$racecat=='Multiracial',], diab ~ age + female + smoke_status + insur_stability + educ_num, family=poisson(link=log))
mod_diab_educcont_oth_pois <- glm(data=aou[aou$racecat=='Other',], diab ~ age + female + smoke_status + insur_stability + educ_num, family=poisson(link=log))
mod_diab_educcont_none_pois <- glm(data=aou[aou$racecat=='None of these',], diab ~ age + female + smoke_status + insur_stability + educ_num, family=poisson(link=log))

aou_educcont_diab_pois_result <- data.frame(exp(mod_diab_educcont_all_pois$coefficients),exp(confint(mod_diab_educcont_all_pois)),coef(summary(mod_diab_educcont_all_pois))[,"Pr(>|z|)"],
                                            exp(mod_diab_educcont_raceadj_pois$coefficients)[1:9],exp(confint(mod_diab_educcont_raceadj_pois))[1:9,1:2],coef(summary(mod_diab_educcont_raceadj_pois))[,"Pr(>|z|)"][1:9],
                                            exp(mod_diab_educcont_nhw_pois$coefficients),exp(confint(mod_diab_educcont_nhw_pois)),coef(summary(mod_diab_educcont_nhw_pois))[,"Pr(>|z|)"],
                                            exp(mod_diab_educcont_nhb_pois$coefficients),exp(confint(mod_diab_educcont_nhb_pois)),coef(summary(mod_diab_educcont_nhb_pois))[,"Pr(>|z|)"],
                                            exp(mod_diab_educcont_his_pois$coefficients),exp(confint(mod_diab_educcont_his_pois)),coef(summary(mod_diab_educcont_his_pois))[,"Pr(>|z|)"],
                                            exp(mod_diab_educcont_nha_pois$coefficients),exp(confint(mod_diab_educcont_nha_pois)),coef(summary(mod_diab_educcont_nha_pois))[,"Pr(>|z|)"],
                                            exp(mod_diab_educcont_multi_pois$coefficients),exp(confint(mod_diab_educcont_multi_pois)),coef(summary(mod_diab_educcont_multi_pois))[,"Pr(>|z|)"],
                                            exp(mod_diab_educcont_oth_pois$coefficients),exp(confint(mod_diab_educcont_oth_pois)),coef(summary(mod_diab_educcont_oth_pois))[,"Pr(>|z|)"],
                                            exp(mod_diab_educcont_none_pois$coefficients),exp(confint(mod_diab_educcont_none_pois)),coef(summary(mod_diab_educcont_none_pois))[,"Pr(>|z|)"],stringsAsFactors = FALSE)
colnames(aou_educcont_diab_pois_result) <- c('coef','lower CI','upper CI','p','coef','lower CI','upper CI','p','coef','lower CI','upper CI','p','coef','lower CI','upper CI','p','coef','lower CI','upper CI','p','coef','lower CI','upper CI','p','coef','lower CI','upper CI','p','coef','lower CI','upper CI','p','coef','lower CI','upper CI','p')
aou_educcont_diab_pois_result


mod_ob_educcont_all_pois <- glm(data=aou, obese ~ age + female + smoke_status + insur_stability + educ_num, family=poisson(link=log))
mod_ob_educcont_raceadj_pois <- glm(data=aou, obese ~ age + female + smoke_status + insur_stability + educ_num + racecat, family=poisson(link=log))
mod_ob_educcont_nhw_pois <- glm(data=aou[aou$racecat=='NHW',], obese ~ age + female + smoke_status + insur_stability + educ_num, family=poisson(link=log))
mod_ob_educcont_nhb_pois <- glm(data=aou[aou$racecat=='NHB',], obese ~ age + female + smoke_status + insur_stability + educ_num, family=poisson(link=log))
mod_ob_educcont_his_pois <- glm(data=aou[aou$racecat=='Hispanic',], obese ~ age + female + smoke_status + insur_stability + educ_num, family=poisson(link=log))
mod_ob_educcont_nha_pois <- glm(data=aou[aou$racecat=='NHAsian',], obese ~ age + female + smoke_status + insur_stability + educ_num, family=poisson(link=log))
mod_ob_educcont_multi_pois <- glm(data=aou[aou$racecat=='Multiracial',], obese ~ age + female + smoke_status + insur_stability + educ_num, family=poisson(link=log))
mod_ob_educcont_oth_pois <- glm(data=aou[aou$racecat=='Other',], obese ~ age + female + smoke_status + insur_stability + educ_num, family=poisson(link=log))
mod_ob_educcont_none_pois <- glm(data=aou[aou$racecat=='None of these',], obese ~ age + female + smoke_status + insur_stability + educ_num, family=poisson(link=log))

aou_educcont_ob_pois_result <- data.frame(exp(mod_ob_educcont_all_pois$coefficients),exp(confint(mod_ob_educcont_all_pois)),coef(summary(mod_ob_educcont_all_pois))[,"Pr(>|z|)"],
                                          exp(mod_ob_educcont_raceadj_pois$coefficients)[1:9],exp(confint(mod_ob_educcont_raceadj_pois))[1:9,1:2],coef(summary(mod_ob_educcont_raceadj_pois))[,"Pr(>|z|)"][1:9],
                                          exp(mod_ob_educcont_nhw_pois$coefficients),exp(confint(mod_ob_educcont_nhw_pois)),coef(summary(mod_ob_educcont_nhw_pois))[,"Pr(>|z|)"],
                                          exp(mod_ob_educcont_nhb_pois$coefficients),exp(confint(mod_ob_educcont_nhb_pois)),coef(summary(mod_ob_educcont_nhb_pois))[,"Pr(>|z|)"],
                                          exp(mod_ob_educcont_his_pois$coefficients),exp(confint(mod_ob_educcont_his_pois)),coef(summary(mod_ob_educcont_his_pois))[,"Pr(>|z|)"],
                                          exp(mod_ob_educcont_nha_pois$coefficients),exp(confint(mod_ob_educcont_nha_pois)),coef(summary(mod_ob_educcont_nha_pois))[,"Pr(>|z|)"],
                                          exp(mod_ob_educcont_multi_pois$coefficients),exp(confint(mod_ob_educcont_multi_pois)),coef(summary(mod_ob_educcont_multi_pois))[,"Pr(>|z|)"],
                                          exp(mod_ob_educcont_oth_pois$coefficients),exp(confint(mod_ob_educcont_oth_pois)),coef(summary(mod_ob_educcont_oth_pois))[,"Pr(>|z|)"],
                                          exp(mod_ob_educcont_none_pois$coefficients),exp(confint(mod_ob_educcont_none_pois)),coef(summary(mod_ob_educcont_none_pois))[,"Pr(>|z|)"],stringsAsFactors = FALSE)
colnames(aou_educcont_ob_pois_result) <- c('coef','lower CI','upper CI','p','coef','lower CI','upper CI','p','coef','lower CI','upper CI','p','coef','lower CI','upper CI','p','coef','lower CI','upper CI','p','coef','lower CI','upper CI','p','coef','lower CI','upper CI','p','coef','lower CI','upper CI','p','coef','lower CI','upper CI','p')
aou_educcont_ob_pois_result

aou_educcont_diabob_pois_result <- cbind(aou_educcont_diab_pois_result,aou_educcont_ob_pois_result)



#inc cont, inc_num
mod_diab_incnum_all_qb <- glm(data=aou, diab ~ age + female + smoke_status + insur_stability + inc_num, family=quasibinomial)
mod_diab_incnum_raceadj_qb <- glm(data=aou, diab ~ age + female + smoke_status + insur_stability + inc_num + racecat, family=quasibinomial)
mod_diab_incnum_nhw_qb <- glm(data=aou[aou$racecat=='NHW',], diab ~ age + female + smoke_status + insur_stability + inc_num, family=quasibinomial)
mod_diab_incnum_nhb_qb <- glm(data=aou[aou$racecat=='NHB',], diab ~ age + female + smoke_status + insur_stability + inc_num, family=quasibinomial)
mod_diab_incnum_his_qb <- glm(data=aou[aou$racecat=='Hispanic',], diab ~ age + female + smoke_status + insur_stability + inc_num, family=quasibinomial)
mod_diab_incnum_nha_qb <- glm(data=aou[aou$racecat=='NHAsian',], diab ~ age + female + smoke_status + insur_stability + inc_num, family=quasibinomial)
mod_diab_incnum_multi_qb <- glm(data=aou[aou$racecat=='Multiracial',], diab ~ age + female + smoke_status + insur_stability + inc_num, family=quasibinomial)
mod_diab_incnum_oth_qb <- glm(data=aou[aou$racecat=='Other',], diab ~ age + female + smoke_status + insur_stability + inc_num, family=quasibinomial)
mod_diab_incnum_none_qb <- glm(data=aou[aou$racecat=='None of these',], diab ~ age + female + smoke_status + insur_stability + inc_num, family=quasibinomial)
aou_incnum_diab_qb_result <- data.frame(exp(mod_diab_incnum_all_qb$coefficients),exp(confint(mod_diab_incnum_all_qb)),coef(summary(mod_diab_incnum_all_qb))[,"Pr(>|t|)"],
                                        exp(mod_diab_incnum_raceadj_qb$coefficients)[1:9],exp(confint(mod_diab_incnum_raceadj_qb))[1:9,1:2],coef(summary(mod_diab_incnum_raceadj_qb))[,"Pr(>|t|)"][1:9],
                                        exp(mod_diab_incnum_nhw_qb$coefficients),exp(confint(mod_diab_incnum_nhw_qb)),coef(summary(mod_diab_incnum_nhw_qb))[,"Pr(>|t|)"],
                                        exp(mod_diab_incnum_nhb_qb$coefficients),exp(confint(mod_diab_incnum_nhb_qb)),coef(summary(mod_diab_incnum_nhb_qb))[,"Pr(>|t|)"],
                                        exp(mod_diab_incnum_his_qb$coefficients),exp(confint(mod_diab_incnum_his_qb)),coef(summary(mod_diab_incnum_his_qb))[,"Pr(>|t|)"],
                                        exp(mod_diab_incnum_nha_qb$coefficients),exp(confint(mod_diab_incnum_nha_qb)),coef(summary(mod_diab_incnum_nha_qb))[,"Pr(>|t|)"],
                                        exp(mod_diab_incnum_multi_qb$coefficients),exp(confint(mod_diab_incnum_multi_qb)),coef(summary(mod_diab_incnum_multi_qb))[,"Pr(>|t|)"],
                                        exp(mod_diab_incnum_oth_qb$coefficients),exp(confint(mod_diab_incnum_oth_qb)),coef(summary(mod_diab_incnum_oth_qb))[,"Pr(>|t|)"],
                                        exp(mod_diab_incnum_none_qb$coefficients),exp(confint(mod_diab_incnum_none_qb)),coef(summary(mod_diab_incnum_none_qb))[,"Pr(>|t|)"],stringsAsFactors = FALSE)
colnames(aou_incnum_diab_qb_result) <- c('coef','lower CI','upper CI','p','coef','lower CI','upper CI','p','coef','lower CI','upper CI','p','coef','lower CI','upper CI','p','coef','lower CI','upper CI','p','coef','lower CI','upper CI','p','coef','lower CI','upper CI','p','coef','lower CI','upper CI','p','coef','lower CI','upper CI','p')
aou_incnum_diab_qb_result
summary(mod_diab_incnum_all_qb)

mod_ob_incnum_all_qb <- glm(data=aou, obese ~ age + female + smoke_status + insur_stability + inc_num, family=quasibinomial)
mod_ob_incnum_raceadj_qb <- glm(data=aou, obese ~ age + female + smoke_status + insur_stability + inc_num + racecat, family=quasibinomial)
mod_ob_incnum_nhw_qb <- glm(data=aou[aou$racecat=='NHW',], obese ~ age + female + smoke_status + insur_stability + inc_num, family=quasibinomial)
mod_ob_incnum_nhb_qb <- glm(data=aou[aou$racecat=='NHB',], obese ~ age + female + smoke_status + insur_stability + inc_num, family=quasibinomial)
mod_ob_incnum_his_qb <- glm(data=aou[aou$racecat=='Hispanic',], obese ~ age + female + smoke_status + insur_stability + inc_num, family=quasibinomial)
mod_ob_incnum_nha_qb <- glm(data=aou[aou$racecat=='NHAsian',], obese ~ age + female + smoke_status + insur_stability + inc_num, family=quasibinomial)
mod_ob_incnum_multi_qb <- glm(data=aou[aou$racecat=='Multiracial',], obese ~ age + female + smoke_status + insur_stability + inc_num, family=quasibinomial)
mod_ob_incnum_oth_qb <- glm(data=aou[aou$racecat=='Other',], obese ~ age + female + smoke_status + insur_stability + inc_num, family=quasibinomial)
mod_ob_incnum_none_qb <- glm(data=aou[aou$racecat=='None of these',], obese ~ age + female + smoke_status + insur_stability + inc_num, family=quasibinomial)
aou_incnum_ob_qb_result <- data.frame(exp(mod_ob_incnum_all_qb$coefficients),exp(confint(mod_ob_incnum_all_qb)),coef(summary(mod_ob_incnum_all_qb))[,"Pr(>|t|)"],
                                      exp(mod_ob_incnum_raceadj_qb$coefficients)[1:9],exp(confint(mod_ob_incnum_raceadj_qb))[1:9,1:2],coef(summary(mod_ob_incnum_raceadj_qb))[,"Pr(>|t|)"][1:9],
                                      exp(mod_ob_incnum_nhw_qb$coefficients),exp(confint(mod_ob_incnum_nhw_qb)),coef(summary(mod_ob_incnum_nhw_qb))[,"Pr(>|t|)"],
                                      exp(mod_ob_incnum_nhb_qb$coefficients),exp(confint(mod_ob_incnum_nhb_qb)),coef(summary(mod_ob_incnum_nhb_qb))[,"Pr(>|t|)"],
                                      exp(mod_ob_incnum_his_qb$coefficients),exp(confint(mod_ob_incnum_his_qb)),coef(summary(mod_ob_incnum_his_qb))[,"Pr(>|t|)"],
                                      exp(mod_ob_incnum_nha_qb$coefficients),exp(confint(mod_ob_incnum_nha_qb)),coef(summary(mod_ob_incnum_nha_qb))[,"Pr(>|t|)"],
                                      exp(mod_ob_incnum_multi_qb$coefficients),exp(confint(mod_ob_incnum_multi_qb)),coef(summary(mod_ob_incnum_multi_qb))[,"Pr(>|t|)"],
                                      exp(mod_ob_incnum_oth_qb$coefficients),exp(confint(mod_ob_incnum_oth_qb)),coef(summary(mod_ob_incnum_oth_qb))[,"Pr(>|t|)"],
                                      exp(mod_ob_incnum_none_qb$coefficients),exp(confint(mod_ob_incnum_none_qb)),coef(summary(mod_ob_incnum_none_qb))[,"Pr(>|t|)"],stringsAsFactors = FALSE)
colnames(aou_incnum_ob_qb_result) <- c('coef','lower CI','upper CI','p','coef','lower CI','upper CI','p','coef','lower CI','upper CI','p','coef','lower CI','upper CI','p','coef','lower CI','upper CI','p','coef','lower CI','upper CI','p','coef','lower CI','upper CI','p','coef','lower CI','upper CI','p','coef','lower CI','upper CI','p')
aou_incnum_ob_qb_result

aou_incnum_diabob_qb_result <- cbind(aou_incnum_diab_qb_result,aou_incnum_ob_qb_result)


mod_diab_incnum_all_mp <- glm(data=aou, diab ~ age + female + smoke_status + insur_stability + inc_num, family=poisson(link=log))
mod_diab_incnum_all_mp_robust <- as.data.frame(coeftest(mod_diab_incnum_all_mp, vcov=sandwich)[,])
mod_diab_incnum_all_mp_robust_values <- data.frame(exp(mod_diab_incnum_all_mp_robust$Estimate), exp(mod_diab_incnum_all_mp_robust$Estimate - qnorm(1-0.05)/2*mod_diab_incnum_all_mp_robust$'Std. Error'), exp(mod_diab_incnum_all_mp_robust$Estimate + qnorm(1-0.05)/2*mod_diab_incnum_all_mp_robust$'Std. Error'), mod_diab_incnum_all_mp_robust$`Pr(>|z|)`)
colnames(mod_diab_incnum_all_mp_robust_values) <- c('coef','lower CI','upper CI','p')

mod_diab_incnum_raceadj_mp <- glm(data=aou, diab ~ age + female + smoke_status + insur_stability + inc_num + racecat, family=poisson(link=log))
mod_diab_incnum_raceadj_mp_robust <- as.data.frame(coeftest(mod_diab_incnum_raceadj_mp, vcov=sandwich)[,])
mod_diab_incnum_nhw_mp <- glm(data=aou[aou$racecat=='NHW',], diab ~ age + female + smoke_status + insur_stability + inc_num, family=poisson(link=log))
mod_diab_incnum_nhw_mp_robust <- as.data.frame(coeftest(mod_diab_incnum_nhw_mp, vcov=sandwich)[,])
mod_diab_incnum_nhb_mp <- glm(data=aou[aou$racecat=='NHB',], diab ~ age + female + smoke_status + insur_stability + inc_num, family=poisson(link=log))
mod_diab_incnum_nhb_mp_robust <- as.data.frame(coeftest(mod_diab_incnum_nhb_mp, vcov=sandwich)[,])
mod_diab_incnum_his_mp <- glm(data=aou[aou$racecat=='Hispanic',], diab ~ age + female + smoke_status + insur_stability + inc_num, family=poisson(link=log))
mod_diab_incnum_his_mp_robust <- as.data.frame(coeftest(mod_diab_incnum_his_mp, vcov=sandwich)[,])
mod_diab_incnum_nha_mp <- glm(data=aou[aou$racecat=='NHAsian',], diab ~ age + female + smoke_status + insur_stability + inc_num, family=poisson(link=log))
mod_diab_incnum_nha_mp_robust <- as.data.frame(coeftest(mod_diab_incnum_nha_mp, vcov=sandwich)[,])
mod_diab_incnum_multi_mp <- glm(data=aou[aou$racecat=='Multiracial',], diab ~ age + female + smoke_status + insur_stability + inc_num, family=poisson(link=log))
mod_diab_incnum_multi_mp_robust <- as.data.frame(coeftest(mod_diab_incnum_multi_mp, vcov=sandwich)[,])
mod_diab_incnum_oth_mp <- glm(data=aou[aou$racecat=='Other',], diab ~ age + female + smoke_status + insur_stability + inc_num, family=poisson(link=log))
mod_diab_incnum_oth_mp_robust <- as.data.frame(coeftest(mod_diab_incnum_oth_mp, vcov=sandwich)[,])
mod_diab_incnum_none_mp <- glm(data=aou[aou$racecat=='None of these',], diab ~ age + female + smoke_status + insur_stability + inc_num, family=poisson(link=log))
mod_diab_incnum_none_mp_robust <- as.data.frame(coeftest(mod_diab_incnum_none_mp, vcov=sandwich)[,])

aou_incnum_diab_mp_result <- data.frame(exp(mod_diab_incnum_all_mp_robust$Estimate), exp(mod_diab_incnum_all_mp_robust$Estimate - qnorm(1-0.05)/2*mod_diab_incnum_all_mp_robust$'Std. Error'), exp(mod_diab_incnum_all_mp_robust$Estimate + qnorm(1-0.05)/2*mod_diab_incnum_all_mp_robust$'Std. Error'), mod_diab_incnum_all_mp_robust$`Pr(>|z|)`,
                                        exp(mod_diab_incnum_raceadj_mp_robust$Estimate)[1:9], exp(mod_diab_incnum_raceadj_mp_robust$Estimate - qnorm(1-0.05)/2*mod_diab_incnum_raceadj_mp_robust$'Std. Error')[1:9], exp(mod_diab_incnum_raceadj_mp_robust$Estimate + qnorm(1-0.05)/2*mod_diab_incnum_raceadj_mp_robust$'Std. Error')[1:9], mod_diab_incnum_raceadj_mp_robust$`Pr(>|z|)`[1:9],
                                        exp(mod_diab_incnum_nhw_mp_robust$Estimate), exp(mod_diab_incnum_nhw_mp_robust$Estimate - qnorm(1-0.05)/2*mod_diab_incnum_nhw_mp_robust$'Std. Error'), exp(mod_diab_incnum_nhw_mp_robust$Estimate + qnorm(1-0.05)/2*mod_diab_incnum_nhw_mp_robust$'Std. Error'), mod_diab_incnum_nhw_mp_robust$`Pr(>|z|)`,
                                        exp(mod_diab_incnum_nhb_mp_robust$Estimate), exp(mod_diab_incnum_nhb_mp_robust$Estimate - qnorm(1-0.05)/2*mod_diab_incnum_nhb_mp_robust$'Std. Error'), exp(mod_diab_incnum_nhb_mp_robust$Estimate + qnorm(1-0.05)/2*mod_diab_incnum_nhb_mp_robust$'Std. Error'), mod_diab_incnum_nhb_mp_robust$`Pr(>|z|)`,
                                        exp(mod_diab_incnum_his_mp_robust$Estimate), exp(mod_diab_incnum_his_mp_robust$Estimate - qnorm(1-0.05)/2*mod_diab_incnum_his_mp_robust$'Std. Error'), exp(mod_diab_incnum_his_mp_robust$Estimate + qnorm(1-0.05)/2*mod_diab_incnum_his_mp_robust$'Std. Error'), mod_diab_incnum_his_mp_robust$`Pr(>|z|)`,
                                        exp(mod_diab_incnum_nha_mp_robust$Estimate), exp(mod_diab_incnum_nha_mp_robust$Estimate - qnorm(1-0.05)/2*mod_diab_incnum_nha_mp_robust$'Std. Error'), exp(mod_diab_incnum_nha_mp_robust$Estimate + qnorm(1-0.05)/2*mod_diab_incnum_nha_mp_robust$'Std. Error'), mod_diab_incnum_nha_mp_robust$`Pr(>|z|)`,
                                        exp(mod_diab_incnum_multi_mp_robust$Estimate), exp(mod_diab_incnum_multi_mp_robust$Estimate - qnorm(1-0.05)/2*mod_diab_incnum_multi_mp_robust$'Std. Error'), exp(mod_diab_incnum_multi_mp_robust$Estimate + qnorm(1-0.05)/2*mod_diab_incnum_multi_mp_robust$'Std. Error'), mod_diab_incnum_multi_mp_robust$`Pr(>|z|)`,
                                        exp(mod_diab_incnum_oth_mp_robust$Estimate), exp(mod_diab_incnum_oth_mp_robust$Estimate - qnorm(1-0.05)/2*mod_diab_incnum_oth_mp_robust$'Std. Error'), exp(mod_diab_incnum_oth_mp_robust$Estimate + qnorm(1-0.05)/2*mod_diab_incnum_oth_mp_robust$'Std. Error'), mod_diab_incnum_oth_mp_robust$`Pr(>|z|)`,
                                        exp(mod_diab_incnum_none_mp_robust$Estimate), exp(mod_diab_incnum_none_mp_robust$Estimate - qnorm(1-0.05)/2*mod_diab_incnum_none_mp_robust$'Std. Error'), exp(mod_diab_incnum_none_mp_robust$Estimate + qnorm(1-0.05)/2*mod_diab_incnum_none_mp_robust$'Std. Error'), mod_diab_incnum_none_mp_robust$`Pr(>|z|)`, stringsAsFactors=F)
colnames(aou_incnum_diab_mp_result) <- c('coef','lower CI','upper CI','p','coef','lower CI','upper CI','p','coef','lower CI','upper CI','p','coef','lower CI','upper CI','p','coef','lower CI','upper CI','p','coef','lower CI','upper CI','p','coef','lower CI','upper CI','p','coef','lower CI','upper CI','p','coef','lower CI','upper CI','p')
aou_incnum_diab_mp_result


mod_ob_incnum_all_mp <- glm(data=aou, obese ~ age + female + smoke_status + insur_stability + inc_num, family=poisson(link=log))
mod_ob_incnum_all_mp_robust <- as.data.frame(coeftest(mod_ob_incnum_all_mp, vcov=sandwich)[,])
mod_ob_incnum_raceadj_mp <- glm(data=aou, obese ~ age + female + smoke_status + insur_stability + inc_num + racecat, family=poisson(link=log))
mod_ob_incnum_raceadj_mp_robust <- as.data.frame(coeftest(mod_ob_incnum_raceadj_mp, vcov=sandwich)[,])
mod_ob_incnum_nhw_mp <- glm(data=aou[aou$racecat=='NHW',], obese ~ age + female + smoke_status + insur_stability + inc_num, family=poisson(link=log))
mod_ob_incnum_nhw_mp_robust <- as.data.frame(coeftest(mod_ob_incnum_nhw_mp, vcov=sandwich)[,])
mod_ob_incnum_nhb_mp <- glm(data=aou[aou$racecat=='NHB',], obese ~ age + female + smoke_status + insur_stability + inc_num, family=poisson(link=log))
mod_ob_incnum_nhb_mp_robust <- as.data.frame(coeftest(mod_ob_incnum_nhb_mp, vcov=sandwich)[,])
mod_ob_incnum_his_mp <- glm(data=aou[aou$racecat=='Hispanic',], obese ~ age + female + smoke_status + insur_stability + inc_num, family=poisson(link=log))
mod_ob_incnum_his_mp_robust <- as.data.frame(coeftest(mod_ob_incnum_his_mp, vcov=sandwich)[,])
mod_ob_incnum_nha_mp <- glm(data=aou[aou$racecat=='NHAsian',], obese ~ age + female + smoke_status + insur_stability + inc_num, family=poisson(link=log))
mod_ob_incnum_nha_mp_robust <- as.data.frame(coeftest(mod_ob_incnum_nha_mp, vcov=sandwich)[,])
mod_ob_incnum_multi_mp <- glm(data=aou[aou$racecat=='Multiracial',], obese ~ age + female + smoke_status + insur_stability + inc_num, family=poisson(link=log))
mod_ob_incnum_multi_mp_robust <- as.data.frame(coeftest(mod_ob_incnum_multi_mp, vcov=sandwich)[,])
mod_ob_incnum_oth_mp <- glm(data=aou[aou$racecat=='Other',], obese ~ age + female + smoke_status + insur_stability + inc_num, family=poisson(link=log))
mod_ob_incnum_oth_mp_robust <- as.data.frame(coeftest(mod_ob_incnum_oth_mp, vcov=sandwich)[,])
mod_ob_incnum_none_mp <- glm(data=aou[aou$racecat=='None of these',], obese ~ age + female + smoke_status + insur_stability + inc_num, family=poisson(link=log))
mod_ob_incnum_none_mp_robust <- as.data.frame(coeftest(mod_ob_incnum_none_mp, vcov=sandwich)[,])

aou_incnum_ob_mp_result <- data.frame(exp(mod_ob_incnum_all_mp_robust$Estimate), exp(mod_ob_incnum_all_mp_robust$Estimate - qnorm(1-0.05)/2*mod_ob_incnum_all_mp_robust$'Std. Error'), exp(mod_ob_incnum_all_mp_robust$Estimate + qnorm(1-0.05)/2*mod_ob_incnum_all_mp_robust$'Std. Error'), mod_ob_incnum_all_mp_robust$`Pr(>|z|)`,
                                      exp(mod_ob_incnum_raceadj_mp_robust$Estimate)[1:9], exp(mod_ob_incnum_raceadj_mp_robust$Estimate - qnorm(1-0.05)/2*mod_ob_incnum_raceadj_mp_robust$'Std. Error')[1:9], exp(mod_ob_incnum_raceadj_mp_robust$Estimate + qnorm(1-0.05)/2*mod_ob_incnum_raceadj_mp_robust$'Std. Error')[1:9], mod_ob_incnum_raceadj_mp_robust$`Pr(>|z|)`[1:9],
                                      exp(mod_ob_incnum_nhw_mp_robust$Estimate), exp(mod_ob_incnum_nhw_mp_robust$Estimate - qnorm(1-0.05)/2*mod_ob_incnum_nhw_mp_robust$'Std. Error'), exp(mod_ob_incnum_nhw_mp_robust$Estimate + qnorm(1-0.05)/2*mod_ob_incnum_nhw_mp_robust$'Std. Error'), mod_ob_incnum_nhw_mp_robust$`Pr(>|z|)`,
                                      exp(mod_ob_incnum_nhb_mp_robust$Estimate), exp(mod_ob_incnum_nhb_mp_robust$Estimate - qnorm(1-0.05)/2*mod_ob_incnum_nhb_mp_robust$'Std. Error'), exp(mod_ob_incnum_nhb_mp_robust$Estimate + qnorm(1-0.05)/2*mod_ob_incnum_nhb_mp_robust$'Std. Error'), mod_ob_incnum_nhb_mp_robust$`Pr(>|z|)`,
                                      exp(mod_ob_incnum_his_mp_robust$Estimate), exp(mod_ob_incnum_his_mp_robust$Estimate - qnorm(1-0.05)/2*mod_ob_incnum_his_mp_robust$'Std. Error'), exp(mod_ob_incnum_his_mp_robust$Estimate + qnorm(1-0.05)/2*mod_ob_incnum_his_mp_robust$'Std. Error'), mod_ob_incnum_his_mp_robust$`Pr(>|z|)`,
                                      exp(mod_ob_incnum_nha_mp_robust$Estimate), exp(mod_ob_incnum_nha_mp_robust$Estimate - qnorm(1-0.05)/2*mod_ob_incnum_nha_mp_robust$'Std. Error'), exp(mod_ob_incnum_nha_mp_robust$Estimate + qnorm(1-0.05)/2*mod_ob_incnum_nha_mp_robust$'Std. Error'), mod_ob_incnum_nha_mp_robust$`Pr(>|z|)`,
                                      exp(mod_ob_incnum_multi_mp_robust$Estimate), exp(mod_ob_incnum_multi_mp_robust$Estimate - qnorm(1-0.05)/2*mod_ob_incnum_multi_mp_robust$'Std. Error'), exp(mod_ob_incnum_multi_mp_robust$Estimate + qnorm(1-0.05)/2*mod_ob_incnum_multi_mp_robust$'Std. Error'), mod_ob_incnum_multi_mp_robust$`Pr(>|z|)`,
                                      exp(mod_ob_incnum_oth_mp_robust$Estimate), exp(mod_ob_incnum_oth_mp_robust$Estimate - qnorm(1-0.05)/2*mod_ob_incnum_oth_mp_robust$'Std. Error'), exp(mod_ob_incnum_oth_mp_robust$Estimate + qnorm(1-0.05)/2*mod_ob_incnum_oth_mp_robust$'Std. Error'), mod_ob_incnum_oth_mp_robust$`Pr(>|z|)`,
                                      exp(mod_ob_incnum_none_mp_robust$Estimate), exp(mod_ob_incnum_none_mp_robust$Estimate - qnorm(1-0.05)/2*mod_ob_incnum_none_mp_robust$'Std. Error'), exp(mod_ob_incnum_none_mp_robust$Estimate + qnorm(1-0.05)/2*mod_ob_incnum_none_mp_robust$'Std. Error'), mod_ob_incnum_none_mp_robust$`Pr(>|z|)`, stringsAsFactors=F)
colnames(aou_incnum_ob_mp_result) <- c('coef','lower CI','upper CI','p','coef','lower CI','upper CI','p','coef','lower CI','upper CI','p','coef','lower CI','upper CI','p','coef','lower CI','upper CI','p','coef','lower CI','upper CI','p','coef','lower CI','upper CI','p','coef','lower CI','upper CI','p','coef','lower CI','upper CI','p')
aou_incnum_ob_mp_result

aou_incnum_diabob_mp_result <- cbind(aou_incnum_diab_mp_result,aou_incnum_ob_mp_result)


mod_diab_incnum_all_pois <- glm(data=aou, diab ~ age + female + smoke_status + insur_stability + inc_num, family=poisson(link=log))
mod_diab_incnum_raceadj_pois <- glm(data=aou, diab ~ age + female + smoke_status + insur_stability + inc_num + racecat, family=poisson(link=log))
mod_diab_incnum_nhw_pois <- glm(data=aou[aou$racecat=='NHW',], diab ~ age + female + smoke_status + insur_stability + inc_num, family=poisson(link=log))
mod_diab_incnum_nhb_pois <- glm(data=aou[aou$racecat=='NHB',], diab ~ age + female + smoke_status + insur_stability + inc_num, family=poisson(link=log))
mod_diab_incnum_his_pois <- glm(data=aou[aou$racecat=='Hispanic',], diab ~ age + female + smoke_status + insur_stability + inc_num, family=poisson(link=log))
mod_diab_incnum_nha_pois <- glm(data=aou[aou$racecat=='NHAsian',], diab ~ age + female + smoke_status + insur_stability + inc_num, family=poisson(link=log))
mod_diab_incnum_multi_pois <- glm(data=aou[aou$racecat=='Multiracial',], diab ~ age + female + smoke_status + insur_stability + inc_num, family=poisson(link=log))
mod_diab_incnum_oth_pois <- glm(data=aou[aou$racecat=='Other',], diab ~ age + female + smoke_status + insur_stability + inc_num, family=poisson(link=log))
mod_diab_incnum_none_pois <- glm(data=aou[aou$racecat=='None of these',], diab ~ age + female + smoke_status + insur_stability + inc_num, family=poisson(link=log))

aou_incnum_diab_pois_result <- data.frame(exp(mod_diab_incnum_all_pois$coefficients),exp(confint(mod_diab_incnum_all_pois)),coef(summary(mod_diab_incnum_all_pois))[,"Pr(>|z|)"],
                                          exp(mod_diab_incnum_raceadj_pois$coefficients)[1:9],exp(confint(mod_diab_incnum_raceadj_pois))[1:9,1:2],coef(summary(mod_diab_incnum_raceadj_pois))[,"Pr(>|z|)"][1:9],
                                          exp(mod_diab_incnum_nhw_pois$coefficients),exp(confint(mod_diab_incnum_nhw_pois)),coef(summary(mod_diab_incnum_nhw_pois))[,"Pr(>|z|)"],
                                          exp(mod_diab_incnum_nhb_pois$coefficients),exp(confint(mod_diab_incnum_nhb_pois)),coef(summary(mod_diab_incnum_nhb_pois))[,"Pr(>|z|)"],
                                          exp(mod_diab_incnum_his_pois$coefficients),exp(confint(mod_diab_incnum_his_pois)),coef(summary(mod_diab_incnum_his_pois))[,"Pr(>|z|)"],
                                          exp(mod_diab_incnum_nha_pois$coefficients),exp(confint(mod_diab_incnum_nha_pois)),coef(summary(mod_diab_incnum_nha_pois))[,"Pr(>|z|)"],
                                          exp(mod_diab_incnum_multi_pois$coefficients),exp(confint(mod_diab_incnum_multi_pois)),coef(summary(mod_diab_incnum_multi_pois))[,"Pr(>|z|)"],
                                          exp(mod_diab_incnum_oth_pois$coefficients),exp(confint(mod_diab_incnum_oth_pois)),coef(summary(mod_diab_incnum_oth_pois))[,"Pr(>|z|)"],
                                          exp(mod_diab_incnum_none_pois$coefficients),exp(confint(mod_diab_incnum_none_pois)),coef(summary(mod_diab_incnum_none_pois))[,"Pr(>|z|)"],stringsAsFactors = FALSE)
colnames(aou_incnum_diab_pois_result) <- c('coef','lower CI','upper CI','p','coef','lower CI','upper CI','p','coef','lower CI','upper CI','p','coef','lower CI','upper CI','p','coef','lower CI','upper CI','p','coef','lower CI','upper CI','p','coef','lower CI','upper CI','p','coef','lower CI','upper CI','p','coef','lower CI','upper CI','p')
aou_incnum_diab_pois_result


mod_ob_incnum_all_pois <- glm(data=aou, obese ~ age + female + smoke_status + insur_stability + inc_num, family=poisson(link=log))
mod_ob_incnum_raceadj_pois <- glm(data=aou, obese ~ age + female + smoke_status + insur_stability + inc_num + racecat, family=poisson(link=log))
mod_ob_incnum_nhw_pois <- glm(data=aou[aou$racecat=='NHW',], obese ~ age + female + smoke_status + insur_stability + inc_num, family=poisson(link=log))
mod_ob_incnum_nhb_pois <- glm(data=aou[aou$racecat=='NHB',], obese ~ age + female + smoke_status + insur_stability + inc_num, family=poisson(link=log))
mod_ob_incnum_his_pois <- glm(data=aou[aou$racecat=='Hispanic',], obese ~ age + female + smoke_status + insur_stability + inc_num, family=poisson(link=log))
mod_ob_incnum_nha_pois <- glm(data=aou[aou$racecat=='NHAsian',], obese ~ age + female + smoke_status + insur_stability + inc_num, family=poisson(link=log))
mod_ob_incnum_multi_pois <- glm(data=aou[aou$racecat=='Multiracial',], obese ~ age + female + smoke_status + insur_stability + inc_num, family=poisson(link=log))
mod_ob_incnum_oth_pois <- glm(data=aou[aou$racecat=='Other',], obese ~ age + female + smoke_status + insur_stability + inc_num, family=poisson(link=log))
mod_ob_incnum_none_pois <- glm(data=aou[aou$racecat=='None of these',], obese ~ age + female + smoke_status + insur_stability + inc_num, family=poisson(link=log))

aou_incnum_ob_pois_result <- data.frame(exp(mod_ob_incnum_all_pois$coefficients),exp(confint(mod_ob_incnum_all_pois)),coef(summary(mod_ob_incnum_all_pois))[,"Pr(>|z|)"],
                                        exp(mod_ob_incnum_raceadj_pois$coefficients)[1:9],exp(confint(mod_ob_incnum_raceadj_pois))[1:9,1:2],coef(summary(mod_ob_incnum_raceadj_pois))[,"Pr(>|z|)"][1:9],
                                        exp(mod_ob_incnum_nhw_pois$coefficients),exp(confint(mod_ob_incnum_nhw_pois)),coef(summary(mod_ob_incnum_nhw_pois))[,"Pr(>|z|)"],
                                        exp(mod_ob_incnum_nhb_pois$coefficients),exp(confint(mod_ob_incnum_nhb_pois)),coef(summary(mod_ob_incnum_nhb_pois))[,"Pr(>|z|)"],
                                        exp(mod_ob_incnum_his_pois$coefficients),exp(confint(mod_ob_incnum_his_pois)),coef(summary(mod_ob_incnum_his_pois))[,"Pr(>|z|)"],
                                        exp(mod_ob_incnum_nha_pois$coefficients),exp(confint(mod_ob_incnum_nha_pois)),coef(summary(mod_ob_incnum_nha_pois))[,"Pr(>|z|)"],
                                        exp(mod_ob_incnum_multi_pois$coefficients),exp(confint(mod_ob_incnum_multi_pois)),coef(summary(mod_ob_incnum_multi_pois))[,"Pr(>|z|)"],
                                        exp(mod_ob_incnum_oth_pois$coefficients),exp(confint(mod_ob_incnum_oth_pois)),coef(summary(mod_ob_incnum_oth_pois))[,"Pr(>|z|)"],
                                        exp(mod_ob_incnum_none_pois$coefficients),exp(confint(mod_ob_incnum_none_pois)),coef(summary(mod_ob_incnum_none_pois))[,"Pr(>|z|)"],stringsAsFactors = FALSE)
colnames(aou_incnum_ob_pois_result) <- c('coef','lower CI','upper CI','p','coef','lower CI','upper CI','p','coef','lower CI','upper CI','p','coef','lower CI','upper CI','p','coef','lower CI','upper CI','p','coef','lower CI','upper CI','p','coef','lower CI','upper CI','p','coef','lower CI','upper CI','p','coef','lower CI','upper CI','p')
aou_incnum_ob_pois_result

aou_incnum_diabob_pois_result <- cbind(aou_incnum_diab_pois_result,aou_incnum_ob_pois_result)



dim(aou_educcont_diabob_qb_result)
dim(aou_educcont_diabob_mp_result)
dim(aou_educcont_diabob_pois_result)
dim(aou_incnum_diabob_qb_result)
dim(aou_incnum_diabob_mp_result)
dim(aou_incnum_diabob_pois_result)
aou_diabob_qb_mp_pois_result <- rbind(aou_educcont_diabob_qb_result,aou_educcont_diabob_mp_result,aou_educcont_diabob_pois_result,aou_incnum_diabob_qb_result,aou_incnum_diabob_mp_result,aou_incnum_diabob_pois_result)
dim(aou_diabob_qb_mp_pois_result) #54r x 72c
rownames_qb_mp_pois_result <- c(paste('educqb_',rownames(aou_educcont_diab_qb_result)),paste('educmp_',rownames(aou_educcont_diab_qb_result)),paste('educpois_',rownames(aou_educcont_diab_qb_result)),paste('incqb_',rownames(aou_incnum_ob_qb_result)),paste('incmp_',rownames(aou_incnum_ob_qb_result)),paste('incpois_',rownames(aou_educcont_diab_qb_result)))
rownames(aou_diabob_qb_mp_pois_result) <- rownames_qb_mp_pois_result
write_excel_csv(aou_diabob_qb_mp_pois_result, "aou_qb_mp_pois_models.csv") 

#####sensitivity analyses - forest plots of cont educ and inc with QB and MP and POIS models)
#tiff(file='C:/Users/sarac/OneDrive/Documents/Research/Area v Individual SES/aou_SES and AoU_SES/SuppFig_aou_forest_indiv_educcont_stratandnonstrat_qb.tiff', width=8, height=5, units='in', res=300)
png('SuppFig_aou_forest_indiv_educcont_stratandnonstrat_qb.png', width=8, height=5, units='in', res=300)
forestplot(c('Type 2 Diabetes','Obesity'), 
           mean = cbind(t(aou_educcont_diabob_qb_result[9,c(1,37)]),t(aou_educcont_diabob_qb_result[9,c(5,41)]),
                        t(aou_educcont_diabob_qb_result[9,c(9,45)]),t(aou_educcont_diabob_qb_result[9,c(13,49)]),
                        t(aou_educcont_diabob_qb_result[9,c(17,53)]),t(aou_educcont_diabob_qb_result[9,c(21,57)]),
                        t(aou_educcont_diabob_qb_result[9,c(25,61)]),t(aou_educcont_diabob_qb_result[9,c(29,65)]),
                        t(aou_educcont_diabob_qb_result[9,c(33,69)])),
           lower = cbind(t(aou_educcont_diabob_qb_result[9,c(2,38)]),t(aou_educcont_diabob_qb_result[9,c(6,42)]),
                         t(aou_educcont_diabob_qb_result[9,c(9,46)]),t(aou_educcont_diabob_qb_result[9,c(14,50)]),
                         t(aou_educcont_diabob_qb_result[9,c(18,54)]),t(aou_educcont_diabob_qb_result[9,c(22,58)]),
                         t(aou_educcont_diabob_qb_result[9,c(26,62)]),t(aou_educcont_diabob_qb_result[9,c(30,66)]),
                         t(aou_educcont_diabob_qb_result[9,c(34,68)])),
           upper = cbind(t(aou_educcont_diabob_qb_result[9,c(3,39)]),t(aou_educcont_diabob_qb_result[9,c(7,43)]),
                         t(aou_educcont_diabob_qb_result[9,c(11,47)]),t(aou_educcont_diabob_qb_result[9,c(15,51)]),
                         t(aou_educcont_diabob_qb_result[9,c(19,55)]),t(aou_educcont_diabob_qb_result[9,c(23,59)]),
                         t(aou_educcont_diabob_qb_result[9,c(27,63)]),t(aou_educcont_diabob_qb_result[9,c(31,67)]),
                         t(aou_educcont_diabob_qb_result[9,c(35,71)])),
           col = fpColors(box = c('gray23','gray57','dodgerblue4','darkslategray3','palegreen3','lightgoldenrod','salmon1','indianred3','mediumpurple4')),
           legend=c("All","All, Race-adjusted","NHW","NHB","Mexican-American","Other Hispanic","NHA","Other/Multi-Racial \n including NHA (pre-2011)","Other/Multi-Racial \n (post-2011)"),
           legend_args = fpLegend(pos = "right"),#list(x = 0.5, y = -0.2), gp = gpar(cex=0.9,col = "#CCCCCC", fill = "#F9F9F9")),
           xlab = "OR (95% CI)",
           title = 'Adjusted OR for Continuous Education on T2D and Obesity Prevalence, \n Overall and Stratified by Race/Ethnicity \n (Quasi-Binomial Regression)',
           txt_gp = fpTxtGp(title=gpar(cex=1),xlab=gpar(cex=1), ticks=gpar(cex=1), label=gpar(cex=1)),
           vertices=TRUE,
           line.margin=0.25,
           graph.pos=2,
           clip=c(0.7,1.1), xlog=T, grid=structure(c(0.7,0.8,0.9,1.0,1.1)),
           xticks=c(-0.356674943938738,-0.223143551314213,-0.105360515657828,0,0.0953101798043265))#, xticks=c(0.7,0.8,0.9,1.0,1.1))#grid=structure(c(0.75,0.8,0.9,1.0,1.1)))
dev.off()


#tiff(file='C:/Users/sarac/OneDrive/Documents/Research/Area v Individual SES/aou_SES and AoU_SES/SuppFig_aou_forest_indiv_inccont_stratandnonstrat_qb.tiff', width=8, height=5, units='in', res=300)
png('SuppFig_aou_forest_indiv_inccont_stratandnonstrat_qb.png', width=8, height=5, units='in', res=300)
forestplot(c('Type 2 Diabetes','Obesity'), 
           mean = cbind(t(aou_incnum_diabob_qb_result[10,c(1,37)]),t(aou_incnum_diabob_qb_result[10,c(5,41)]),
                        t(aou_incnum_diabob_qb_result[10,c(9,45)]),t(aou_incnum_diabob_qb_result[10,c(13,49)]),
                        t(aou_incnum_diabob_qb_result[10,c(17,53)]),t(aou_incnum_diabob_qb_result[10,c(21,57)]),
                        t(aou_incnum_diabob_qb_result[10,c(25,61)]),t(aou_incnum_diabob_qb_result[10,c(29,65)]),
                        t(aou_incnum_diabob_qb_result[10,c(33,69)])),
           lower = cbind(t(aou_incnum_diabob_qb_result[10,c(2,38)]),t(aou_incnum_diabob_qb_result[10,c(6,42)]),
                         t(aou_incnum_diabob_qb_result[10,c(10,46)]),t(aou_incnum_diabob_qb_result[10,c(14,50)]),
                         t(aou_incnum_diabob_qb_result[10,c(18,54)]),t(aou_incnum_diabob_qb_result[10,c(22,58)]),
                         t(aou_incnum_diabob_qb_result[10,c(26,62)]),t(aou_incnum_diabob_qb_result[10,c(30,66)]),
                         t(aou_incnum_diabob_qb_result[10,c(34,68)])),
           upper = cbind(t(aou_incnum_diabob_qb_result[10,c(3,39)]),t(aou_incnum_diabob_qb_result[10,c(7,43)]),
                         t(aou_incnum_diabob_qb_result[10,c(11,47)]),t(aou_incnum_diabob_qb_result[10,c(15,51)]),
                         t(aou_incnum_diabob_qb_result[10,c(19,55)]),t(aou_incnum_diabob_qb_result[10,c(23,59)]),
                         t(aou_incnum_diabob_qb_result[10,c(27,63)]),t(aou_incnum_diabob_qb_result[10,c(31,67)]),
                         t(aou_incnum_diabob_qb_result[10,c(35,71)])),
           col = fpColors(box = c('gray23','gray57','dodgerblue4','darkslategray3','palegreen3','lightgoldenrod','salmon1','indianred3','mediumpurple4')),
           legend=c("All","All, Race-adjusted","NHW","NHB","Mexican-American","Other Hispanic","NHA","Other/Multi-Racial \n including NHA (pre-2011)","Other/Multi-Racial \n (post-2011)"),
           legend_args = fpLegend(pos = "right"),#list(x = 0.5, y = -0.2), gp = gpar(cex=0.9,col = "#CCCCCC", fill = "#F9F9F9")),
           xlab = "OR (95% CI)",
           title = 'Adjusted OR for Continuous Income on T2D and Obesity Prevalence, \n Overall and Stratified by Race/Ethnicity \n (Quasi-Binomial Regression)',
           txt_gp = fpTxtGp(title=gpar(cex=1),xlab=gpar(cex=1), ticks=gpar(cex=1), label=gpar(cex=1)),
           vertices=TRUE,
           line.margin=0.25,
           graph.pos=2,
           clip=c(0.7,1.1), xlog=T,grid=structure(c(0.7,0.8,0.9,1.0,1.1)),
           xticks=c(-0.356674943938738,-0.223143551314213,-0.105360515657828,0,0.0953101798043265))#xlim=c(0.7,1.1),xticks=c(0.75,0.8,0.9,1.0,1.1), , xticks=c(0.75,0.8,0.9,1.0,1.1), xlog=TRUE, grid=structure(c(0.75,0.8,0.9,1.0,1.1)))
dev.off()

#tiff(file='C:/Users/sarac/OneDrive/Documents/Research/Area v Individual SES/aou_SES and AoU_SES/SuppFig_aou_forest_indiv_educcont_stratandnonstrat_mp.tiff', width=8, height=5, units='in', res=300)
png('SuppFig_aou_forest_indiv_educcont_stratandnonstrat_mp.png', width=8, height=5, units='in', res=300)
forestplot(c('Type 2 Diabetes','Obesity'), 
           mean = cbind(t(aou_educcont_diabob_mp_result[9,c(1,37)]),t(aou_educcont_diabob_mp_result[9,c(5,41)]),
                        t(aou_educcont_diabob_mp_result[9,c(9,45)]),t(aou_educcont_diabob_mp_result[9,c(13,49)]),
                        t(aou_educcont_diabob_mp_result[9,c(17,53)]),t(aou_educcont_diabob_mp_result[9,c(21,57)]),
                        t(aou_educcont_diabob_mp_result[9,c(25,61)]),t(aou_educcont_diabob_mp_result[9,c(29,65)]),
                        t(aou_educcont_diabob_mp_result[9,c(33,69)])),
           lower = cbind(t(aou_educcont_diabob_mp_result[9,c(2,38)]),t(aou_educcont_diabob_mp_result[9,c(6,42)]),
                         t(aou_educcont_diabob_mp_result[9,c(9,46)]),t(aou_educcont_diabob_mp_result[9,c(14,50)]),
                         t(aou_educcont_diabob_mp_result[9,c(18,54)]),t(aou_educcont_diabob_mp_result[9,c(22,58)]),
                         t(aou_educcont_diabob_mp_result[9,c(26,62)]),t(aou_educcont_diabob_mp_result[9,c(30,66)]),
                         t(aou_educcont_diabob_mp_result[9,c(34,68)])),
           upper = cbind(t(aou_educcont_diabob_mp_result[9,c(3,39)]),t(aou_educcont_diabob_mp_result[9,c(7,43)]),
                         t(aou_educcont_diabob_mp_result[9,c(11,47)]),t(aou_educcont_diabob_mp_result[9,c(15,51)]),
                         t(aou_educcont_diabob_mp_result[9,c(19,55)]),t(aou_educcont_diabob_mp_result[9,c(23,59)]),
                         t(aou_educcont_diabob_mp_result[9,c(27,63)]),t(aou_educcont_diabob_mp_result[9,c(31,67)]),
                         t(aou_educcont_diabob_mp_result[9,c(35,71)])),
           col = fpColors(box = c('gray23','gray57','dodgerblue4','darkslategray3','palegreen3','lightgoldenrod','salmon1','indianred3','mediumpurple4')),
           legend=c("All","All, Race-adjusted","NHW","NHB","Mexican-American","Other Hispanic","NHA","Other/Multi-Racial \n including NHA (pre-2011)","Other/Multi-Racial \n (post-2011)"),
           legend_args = fpLegend(pos = "right"),#list(x = 0.5, y = -0.2), gp = gpar(cex=0.9,col = "#CCCCCC", fill = "#F9F9F9")),
           xlab = "Estimated RR (95% CI)",
           title = 'Estimated RR for Continuous Education on T2D and Obesity Prevalence, \n Overall and Stratified by Race/Ethnicity \n (Modified Poisson Regression)',
           txt_gp = fpTxtGp(title=gpar(cex=1),xlab=gpar(cex=1), ticks=gpar(cex=1), label=gpar(cex=1)),
           vertices=TRUE,
           line.margin=0.25,
           graph.pos=2,
           clip=c(0.7,1.1), xlog=T, grid=structure(c(0.7,0.8,0.9,1.0,1.1)),
           xticks=c(-0.356674943938738,-0.223143551314213,-0.105360515657828,0,0.0953101798043265))#, xticks=c(0.7,0.8,0.9,1.0,1.1))#grid=structure(c(0.75,0.8,0.9,1.0,1.1)))
dev.off()


#tiff(file='C:/Users/sarac/OneDrive/Documents/Research/Area v Individual SES/aou_SES and AoU_SES/SuppFig_aou_forest_indiv_inccont_stratandnonstrat_mp.tiff', width=8, height=5, units='in', res=300)
png('SuppFig_aou_forest_indiv_inccont_stratandnonstrat_mp.png', width=8, height=5, units='in', res=300)
forestplot(c('Type 2 Diabetes','Obesity'), 
           mean = cbind(t(aou_incnum_diabob_mp_result[9,c(1,37)]),t(aou_incnum_diabob_mp_result[9,c(5,41)]),
                        t(aou_incnum_diabob_mp_result[9,c(9,45)]),t(aou_incnum_diabob_mp_result[9,c(13,49)]),
                        t(aou_incnum_diabob_mp_result[9,c(17,53)]),t(aou_incnum_diabob_mp_result[9,c(21,57)]),
                        t(aou_incnum_diabob_mp_result[9,c(25,61)]),t(aou_incnum_diabob_mp_result[9,c(29,65)]),
                        t(aou_incnum_diabob_mp_result[9,c(33,69)])),
           lower = cbind(t(aou_incnum_diabob_mp_result[9,c(2,38)]),t(aou_incnum_diabob_mp_result[9,c(6,42)]),
                         t(aou_incnum_diabob_mp_result[9,c(9,46)]),t(aou_incnum_diabob_mp_result[9,c(14,50)]),
                         t(aou_incnum_diabob_mp_result[9,c(18,54)]),t(aou_incnum_diabob_mp_result[9,c(22,58)]),
                         t(aou_incnum_diabob_mp_result[9,c(26,62)]),t(aou_incnum_diabob_mp_result[9,c(30,66)]),
                         t(aou_incnum_diabob_mp_result[9,c(34,68)])),
           upper = cbind(t(aou_incnum_diabob_mp_result[9,c(3,39)]),t(aou_incnum_diabob_mp_result[9,c(7,43)]),
                         t(aou_incnum_diabob_mp_result[9,c(11,47)]),t(aou_incnum_diabob_mp_result[9,c(15,51)]),
                         t(aou_incnum_diabob_mp_result[9,c(19,55)]),t(aou_incnum_diabob_mp_result[9,c(23,59)]),
                         t(aou_incnum_diabob_mp_result[9,c(27,63)]),t(aou_incnum_diabob_mp_result[9,c(31,67)]),
                         t(aou_incnum_diabob_mp_result[9,c(35,71)])),
           col = fpColors(box = c('gray23','gray57','dodgerblue4','darkslategray3','palegreen3','lightgoldenrod','salmon1','indianred3','mediumpurple4')),
           legend=c("All","All, Race-adjusted","NHW","NHB","Mexican-American","Other Hispanic","NHA","Other/Multi-Racial \n including NHA (pre-2011)","Other/Multi-Racial \n (post-2011)"),
           legend_args = fpLegend(pos = "right"),#list(x = 0.5, y = -0.2), gp = gpar(cex=0.9,col = "#CCCCCC", fill = "#F9F9F9")),
           xlab = "Estimated RR (95% CI)",
           title = 'Estimated RR for Continuous Income on T2D and Obesity Prevalence, \n Overall and Stratified by Race/Ethnicity \n (Modified Poisson Regression)',
           txt_gp = fpTxtGp(title=gpar(cex=1),xlab=gpar(cex=1), ticks=gpar(cex=1), label=gpar(cex=1)),
           vertices=TRUE,
           line.margin=0.25,
           graph.pos=2,
           clip=c(0.7,1.1), xlog=T,grid=structure(c(0.7,0.8,0.9,1.0,1.1)),
           xticks=c(-0.356674943938738,-0.223143551314213,-0.105360515657828,0,0.0953101798043265))#xlim=c(0.7,1.1),xticks=c(0.75,0.8,0.9,1.0,1.1), , xticks=c(0.75,0.8,0.9,1.0,1.1), xlog=TRUE, grid=structure(c(0.75,0.8,0.9,1.0,1.1)))
dev.off()


#tiff(file='C:/Users/sarac/OneDrive/Documents/Research/Area v Individual SES/aou_SES and AoU_SES/SuppFig_aou_forest_indiv_educcont_stratandnonstrat_pois.tiff', width=8, height=5, units='in', res=300)
png('SuppFig_aou_forest_indiv_educcont_stratandnonstrat_pois.png', width=8, height=5, units='in', res=300)
forestplot(c('Type 2 Diabetes','Obesity'), 
           mean = cbind(t(aou_educcont_diabob_pois_result[9,c(1,37)]),t(aou_educcont_diabob_pois_result[9,c(5,41)]),
                        t(aou_educcont_diabob_pois_result[9,c(9,45)]),t(aou_educcont_diabob_pois_result[9,c(13,49)]),
                        t(aou_educcont_diabob_pois_result[9,c(17,53)]),t(aou_educcont_diabob_pois_result[9,c(21,57)]),
                        t(aou_educcont_diabob_pois_result[9,c(25,61)]),t(aou_educcont_diabob_pois_result[9,c(29,65)]),
                        t(aou_educcont_diabob_pois_result[9,c(33,69)])),
           lower = cbind(t(aou_educcont_diabob_pois_result[9,c(2,38)]),t(aou_educcont_diabob_pois_result[9,c(6,42)]),
                         t(aou_educcont_diabob_pois_result[9,c(9,46)]),t(aou_educcont_diabob_pois_result[9,c(14,50)]),
                         t(aou_educcont_diabob_pois_result[9,c(18,54)]),t(aou_educcont_diabob_pois_result[9,c(22,58)]),
                         t(aou_educcont_diabob_pois_result[9,c(26,62)]),t(aou_educcont_diabob_pois_result[9,c(30,66)]),
                         t(aou_educcont_diabob_pois_result[9,c(34,68)])),
           upper = cbind(t(aou_educcont_diabob_pois_result[9,c(3,39)]),t(aou_educcont_diabob_pois_result[9,c(7,43)]),
                         t(aou_educcont_diabob_pois_result[9,c(11,47)]),t(aou_educcont_diabob_pois_result[9,c(15,51)]),
                         t(aou_educcont_diabob_pois_result[9,c(19,55)]),t(aou_educcont_diabob_pois_result[9,c(23,59)]),
                         t(aou_educcont_diabob_pois_result[9,c(27,63)]),t(aou_educcont_diabob_pois_result[9,c(31,67)]),
                         t(aou_educcont_diabob_pois_result[9,c(35,71)])),
           col = fpColors(box = c('gray23','gray57','dodgerblue4','darkslategray3','palegreen3','lightgoldenrod','salmon1','indianred3','mediumpurple4')),
           legend=c("All","All, Race-adjusted","NHW","NHB","Mexican-American","Other Hispanic","NHA","Other/Multi-Racial \n including NHA (pre-2011)","Other/Multi-Racial \n (post-2011)"),
           legend_args = fpLegend(pos = "right"),#list(x = 0.5, y = -0.2), gp = gpar(cex=0.9,col = "#CCCCCC", fill = "#F9F9F9")),
           xlab = "Estimated RR (95% CI)",
           title = 'Estimated RR for Continuous Education on T2D and Obesity Prevalence, \n Overall and Stratified by Race/Ethnicity \n (Poisson Regression)',
           txt_gp = fpTxtGp(title=gpar(cex=1),xlab=gpar(cex=1), ticks=gpar(cex=1), label=gpar(cex=1)),
           vertices=TRUE,
           line.margin=0.25,
           graph.pos=2,
           clip=c(0.7,1.1), xlog=T, grid=structure(c(0.7,0.8,0.9,1.0,1.1)),
           xticks=c(-0.356674943938738,-0.223143551314213,-0.105360515657828,0,0.0953101798043265))#, xticks=c(0.7,0.8,0.9,1.0,1.1))#grid=structure(c(0.75,0.8,0.9,1.0,1.1)))
dev.off()


#tiff(file='C:/Users/sarac/OneDrive/Documents/Research/Area v Individual SES/aou_SES and AoU_SES/SuppFig_aou_forest_indiv_inccont_stratandnonstrat_pois.tiff', width=8, height=5, units='in', res=300)
png('SuppFig_aou_forest_indiv_inccont_stratandnonstrat_pois.png', width=8, height=5, units='in', res=300)
forestplot(c('Type 2 Diabetes','Obesity'), 
           mean = cbind(t(aou_incnum_diabob_pois_result[9,c(1,37)]),t(aou_incnum_diabob_pois_result[9,c(5,41)]),
                        t(aou_incnum_diabob_pois_result[9,c(9,45)]),t(aou_incnum_diabob_pois_result[9,c(13,49)]),
                        t(aou_incnum_diabob_pois_result[9,c(17,53)]),t(aou_incnum_diabob_pois_result[9,c(21,57)]),
                        t(aou_incnum_diabob_pois_result[9,c(25,61)]),t(aou_incnum_diabob_pois_result[9,c(29,65)]),
                        t(aou_incnum_diabob_pois_result[9,c(33,69)])),
           lower = cbind(t(aou_incnum_diabob_pois_result[9,c(2,38)]),t(aou_incnum_diabob_pois_result[9,c(6,42)]),
                         t(aou_incnum_diabob_pois_result[9,c(9,46)]),t(aou_incnum_diabob_pois_result[9,c(14,50)]),
                         t(aou_incnum_diabob_pois_result[9,c(18,54)]),t(aou_incnum_diabob_pois_result[9,c(22,58)]),
                         t(aou_incnum_diabob_pois_result[9,c(26,62)]),t(aou_incnum_diabob_pois_result[9,c(30,66)]),
                         t(aou_incnum_diabob_pois_result[9,c(34,68)])),
           upper = cbind(t(aou_incnum_diabob_pois_result[9,c(3,39)]),t(aou_incnum_diabob_pois_result[9,c(7,43)]),
                         t(aou_incnum_diabob_pois_result[9,c(11,47)]),t(aou_incnum_diabob_pois_result[9,c(15,51)]),
                         t(aou_incnum_diabob_pois_result[9,c(19,55)]),t(aou_incnum_diabob_pois_result[9,c(23,59)]),
                         t(aou_incnum_diabob_pois_result[9,c(27,63)]),t(aou_incnum_diabob_pois_result[9,c(31,67)]),
                         t(aou_incnum_diabob_pois_result[9,c(35,71)])),
           col = fpColors(box = c('gray23','gray57','dodgerblue4','darkslategray3','palegreen3','lightgoldenrod','salmon1','indianred3','mediumpurple4')),
           legend=c("All","All, Race-adjusted","NHW","NHB","Mexican-American","Other Hispanic","NHA","Other/Multi-Racial \n including NHA (pre-2011)","Other/Multi-Racial \n (post-2011)"),
           legend_args = fpLegend(pos = "right"),#list(x = 0.5, y = -0.2), gp = gpar(cex=0.9,col = "#CCCCCC", fill = "#F9F9F9")),
           xlab = "Estimated RR (95% CI)",
           title = 'Estimated RR for Continuous Income on T2D and Obesity Prevalence, \n Overall and Stratified by Race/Ethnicity \n (Poisson Regression)',
           txt_gp = fpTxtGp(title=gpar(cex=1),xlab=gpar(cex=1), ticks=gpar(cex=1), label=gpar(cex=1)),
           vertices=TRUE,
           line.margin=0.25,
           graph.pos=2,
           clip=c(0.7,1.1), xlog=T,grid=structure(c(0.7,0.8,0.9,1.0,1.1)),
           xticks=c(-0.356674943938738,-0.223143551314213,-0.105360515657828,0,0.0953101798043265))#xlim=c(0.7,1.1),xticks=c(0.75,0.8,0.9,1.0,1.1), , xticks=c(0.75,0.8,0.9,1.0,1.1), xlog=TRUE, grid=structure(c(0.75,0.8,0.9,1.0,1.1)))
dev.off()





#####interaction terms#####
#categorical SES
mod_diab_educcat_interact <- glm(data=aou, diab ~ age + female + smoke_status + insur_stability + educ_simple*racecat, family=binomial(link=logit))
summary(mod_diab_educcat_interact)
mod_diab_inccat_interact <- glm(data=aou, diab ~ age + female + smoke_status + insur_stability + inc_simple*racecat, family=binomial(link=logit))
summary(mod_diab_inccat_interact)
mod_ob_educcat_interact <- glm(data=aou, obese ~ age + female + smoke_status + insur_stability + educ_simple*racecat, family=binomial(link=logit))
summary(mod_ob_educcat_interact)
mod_ob_inccat_interact <- glm(data=aou, obese ~ age + female + smoke_status + insur_stability + inc_simple*racecat, family=binomial(link=logit))
summary(mod_ob_inccat_interact)
aou_educcat_interact_result <- data.frame(exp(mod_diab_educcat_interact$coefficients),exp(confint(mod_diab_educcat_interact)),coef(summary(mod_diab_educcat_interact))[,"Pr(>|z|)"],
                                          exp(mod_ob_educcat_interact$coefficients),exp(confint(mod_ob_educcat_interact)),coef(summary(mod_ob_educcat_interact))[,"Pr(>|z|)"],stringsAsFactors = FALSE)
aou_educcat_interact_result
aou_inccat_interact_result <- data.frame(exp(mod_diab_inccat_interact$coefficients),exp(confint(mod_diab_inccat_interact)),coef(summary(mod_diab_inccat_interact))[,"Pr(>|z|)"],
                                         exp(mod_ob_inccat_interact$coefficients),exp(confint(mod_ob_inccat_interact)),coef(summary(mod_ob_inccat_interact))[,"Pr(>|z|)"],stringsAsFactors = FALSE)
aou_inccat_interact_result
table(aou$smoke_status)

#continuous SES
mod_diab_educcont_interact <- glm(data=aou, diab ~ age + female + smoke_status + insur_stability + educ_num*racecat, family=binomial(link=logit))
summary(mod_diab_educcont_interact)
mod_diab_incnum_interact <- glm(data=aou, diab ~ age + female + smoke_status + insur_stability + inc_num*racecat, family=binomial(link=logit))
summary(mod_diab_incnum_interact)
mod_ob_educcont_interact <- glm(data=aou, obese ~ age + female + smoke_status + insur_stability + educ_num*racecat, family=binomial(link=logit))
summary(mod_ob_educcont_interact)
mod_ob_incnum_interact <- glm(data=aou, obese ~ age + female + smoke_status + insur_stability + inc_num*racecat, family=binomial(link=logit))
summary(mod_ob_incnum_interact)
aou_educinccont_interact_result <- data.frame(exp(mod_diab_educcont_interact$coefficients),exp(confint(mod_diab_educcont_interact)),coef(summary(mod_diab_educcont_interact))[,"Pr(>|z|)"],
                                              exp(mod_ob_educcont_interact$coefficients),exp(confint(mod_ob_educcont_interact)),coef(summary(mod_ob_educcont_interact))[,"Pr(>|z|)"],
                                              exp(mod_diab_incnum_interact$coefficients),exp(confint(mod_diab_incnum_interact)),coef(summary(mod_diab_incnum_interact))[,"Pr(>|z|)"],
                                              exp(mod_ob_incnum_interact$coefficients),exp(confint(mod_ob_incnum_interact)),coef(summary(mod_ob_incnum_interact))[,"Pr(>|z|)"],stringsAsFactors = FALSE)
aou_educinccont_interact_result


colnames(aou_educcat_interact_result) <- c('coeff','lower CI','upper CI','p','coeff','lower CI','upper CI','p')
colnames(aou_inccat_interact_result) <- c('coeff','lower CI','upper CI','p','coeff','lower CI','upper CI','p')
colnames(aou_educinccont_interact_result) <- c('coeff','lower CI','upper CI','p','coeff','lower CI','upper CI','p','coeff','lower CI','upper CI','p','coeff','lower CI','upper CI','p')
aou_educinccont_interact_result1 <- aou_educinccont_interact_result[,1:8]
aou_educinccont_interact_result2 <- aou_educinccont_interact_result[,9:16]
colnames(aou_educinccont_interact_result1) <- c('coeff','lower CI','upper CI','p','coeff','lower CI','upper CI','p')
colnames(aou_educinccont_interact_result2) <- c('coeff','lower CI','upper CI','p','coeff','lower CI','upper CI','p')
aou_interaction_results <- rbind(aou_educcat_interact_result, aou_inccat_interact_result,aou_educinccont_interact_result1,aou_educinccont_interact_result2)
dim(aou_interaction_results)
write_excel_csv(aou_interaction_results, "aou_interact_models.csv") 


#####likelihood ratio tests#####
#need package lmtest?
teststat_diabeduc <- -2 * (as.numeric(logLik(mod_diab_educcont_raceadj)) - as.numeric(logLik(mod_diab_educcont_interact)))
pchisq(teststat_diabeduc,df=1,lower.tail=F)
teststat_diabeduc <- -2 * (as.numeric(logLik(mod_diab_incnum_raceadj)) - as.numeric(logLik(mod_diab_incnum_interact)))
pchisq(teststat_diabeduc,df=1,lower.tail=F)
teststat_diabeduc <- -2 * (as.numeric(logLik(mod_ob_educcont_raceadj)) - as.numeric(logLik(mod_ob_educcont_interact)))
pchisq(teststat_diabeduc,df=1,lower.tail=F)
teststat_diabeduc <- -2 * (as.numeric(logLik(mod_ob_incnum_raceadj)) - as.numeric(logLik(mod_ob_incnum_interact)))
pchisq(teststat_diabeduc,df=1,lower.tail=F)
#need package lmtest
library(lmtest)
lrtest(mod_diab_educcont_raceadj,mod_diab_educcont_interact)
lrtest(mod_diab_incnum_raceadj,mod_diab_incnum_interact)
lrtest(mod_ob_educcont_raceadj,mod_ob_educcont_interact)
lrtest(mod_ob_incnum_raceadj,mod_ob_incnum_interact)


#####I2 and influence#####
#I2 from raw numbers: educ->diab prevalence
n <- as.vector(t(table(aou$racecat)))
racecatname <-rownames(table(aou$racecat))
ESTIM_educ_diab <- c(coef(summary(mod_diab_educcont_nhw))[9,"Estimate"],coef(summary(mod_diab_educcont_nhb))[9,"Estimate"],
                     coef(summary(mod_diab_educcont_his))[9,"Estimate"],coef(summary(mod_diab_educcont_nha))[9,"Estimate"],coef(summary(mod_diab_educcont_multi))[9,"Estimate"],
                     coef(summary(mod_diab_educcont_oth))[9,"Estimate"],coef(summary(mod_diab_educcont_none))[9,"Estimate"])
SE_educ_diab <- c(coef(summary(mod_diab_educcont_nhw))[9,"Std. Error"],coef(summary(mod_diab_educcont_nhb))[9,"Std. Error"],
                  coef(summary(mod_diab_educcont_his))[9,"Std. Error"],coef(summary(mod_diab_educcont_nha))[9,"Std. Error"],coef(summary(mod_diab_educcont_multi))[9,"Std. Error"],
                  coef(summary(mod_diab_educcont_oth))[9,"Std. Error"],coef(summary(mod_diab_educcont_none))[9,"Std. Error"])
vi <- as.vector(t((SE_educ_diab*sqrt(n))^2)) #variance is SD^2, SD is SE*sqrt(n)
aou_dat_educ_diab <- data.frame(racecatname,n,ESTIM_educ_diab,vi,SE_educ_diab)
res_educ_diab <- rma(yi=ESTIM_educ_diab,sei=SE_educ_diab, data=aou_dat_educ_diab)
res_nonhw_educ_diab <- rma(yi=ESTIM_educ_diab,sei=SE_educ_diab, data=aou_dat_educ_diab[!racecatname=='NHW',])
res_nonhb_educ_diab <- rma(yi=ESTIM_educ_diab,sei=SE_educ_diab, data=aou_dat_educ_diab[!racecatname=='NHB',])
res_nohis_educ_diab <- rma(yi=ESTIM_educ_diab,sei=SE_educ_diab, data=aou_dat_educ_diab[!racecatname=='Hispanic',])
res_nonha_educ_diab <- rma(yi=ESTIM_educ_diab,sei=SE_educ_diab, data=aou_dat_educ_diab[!racecatname=='NHAsian',])
res_nomul_educ_diab <- rma(yi=ESTIM_educ_diab,sei=SE_educ_diab, data=aou_dat_educ_diab[!racecatname=='Multiracial',])
res_nooth_educ_diab <- rma(yi=ESTIM_educ_diab,sei=SE_educ_diab, data=aou_dat_educ_diab[!racecatname=='Other',])
res_nonone_educ_diab <- rma(yi=ESTIM_educ_diab,sei=SE_educ_diab, data=aou_dat_educ_diab[!racecatname=='None of these',])
res_nonhwnha_educ_diab <- rma(yi=ESTIM_educ_diab,sei=SE_educ_diab, data=aou_dat_educ_diab[!racecatname=='NHW' & !racecatname=='NHAsian',])
res_nhwnha_educ_diab <- rma(yi=ESTIM_educ_diab,sei=SE_educ_diab, data=aou_dat_educ_diab[racecatname=='NHW' | racecatname=='NHAsian',])
i2_educ_diab <- rbind(res_educ_diab$I2, res_nonhw_educ_diab$I2, res_nonhb_educ_diab$I2, res_nohis_educ_diab$I2, res_nonha_educ_diab$I2, res_nomul_educ_diab$I2, res_nooth_educ_diab$I2, res_nonone_educ_diab$I2)
i2_educ_diab
influence(res_educ_diab, progbar=F)

#I2 from raw numbers: inc->diab prevalence
n <- as.vector(t(table(aou$racecat)))
racecatname <-rownames(table(aou$racecat))
ESTIM_inc_diab <- c(coef(summary(mod_diab_incnum_nhw))[9,"Estimate"],coef(summary(mod_diab_incnum_nhb))[9,"Estimate"],
                    coef(summary(mod_diab_incnum_his))[9,"Estimate"],coef(summary(mod_diab_incnum_nha))[9,"Estimate"],coef(summary(mod_diab_incnum_multi))[9,"Estimate"],
                    coef(summary(mod_diab_incnum_oth))[9,"Estimate"],coef(summary(mod_diab_incnum_none))[9,"Estimate"])
SE_inc_diab <- c(coef(summary(mod_diab_incnum_nhw))[9,"Std. Error"],coef(summary(mod_diab_incnum_nhb))[9,"Std. Error"],
                 coef(summary(mod_diab_incnum_his))[9,"Std. Error"],coef(summary(mod_diab_incnum_nha))[9,"Std. Error"],coef(summary(mod_diab_incnum_multi))[9,"Std. Error"],
                 coef(summary(mod_diab_incnum_oth))[9,"Std. Error"],coef(summary(mod_diab_incnum_none))[9,"Std. Error"])
vi <- as.vector(t((SE_inc_diab*sqrt(n))^2)) #variance is SD^2, SD is SE*sqrt(n)
aou_dat_inc_diab <- data.frame(racecatname,n,ESTIM_inc_diab,vi,SE_inc_diab)
res_inc_diab <- rma(yi=ESTIM_inc_diab,sei=SE_inc_diab, data=aou_dat_inc_diab)
res_nonhw_inc_diab <- rma(yi=ESTIM_inc_diab,sei=SE_inc_diab, data=aou_dat_inc_diab[!racecatname=='NHW',])
res_nonhb_inc_diab <- rma(yi=ESTIM_inc_diab,sei=SE_inc_diab, data=aou_dat_inc_diab[!racecatname=='NHB',])
res_nohis_inc_diab <- rma(yi=ESTIM_inc_diab,sei=SE_inc_diab, data=aou_dat_inc_diab[!racecatname=='Hispanic',])
res_nonha_inc_diab <- rma(yi=ESTIM_inc_diab,sei=SE_inc_diab, data=aou_dat_inc_diab[!racecatname=='NHAsian',])
res_nomul_inc_diab <- rma(yi=ESTIM_inc_diab,sei=SE_inc_diab, data=aou_dat_inc_diab[!racecatname=='Multiracial',])
res_nooth_inc_diab <- rma(yi=ESTIM_inc_diab,sei=SE_inc_diab, data=aou_dat_inc_diab[!racecatname=='Other',])
res_nonone_inc_diab <- rma(yi=ESTIM_inc_diab,sei=SE_inc_diab, data=aou_dat_inc_diab[!racecatname=='None of these',])
res_nonhwnha_inc_diab <- rma(yi=ESTIM_inc_diab,sei=SE_inc_diab, data=aou_dat_inc_diab[!racecatname=='NHW' & !racecatname=='NHAsian',])
res_nhwnha_inc_diab <- rma(yi=ESTIM_inc_diab,sei=SE_inc_diab, data=aou_dat_inc_diab[racecatname=='NHW' | racecatname=='NHAsian',])
i2_inc_diab <- rbind(res_inc_diab$I2, res_nonhw_inc_diab$I2, res_nonhb_inc_diab$I2, res_nohis_inc_diab$I2, res_nonha_inc_diab$I2, res_nomul_inc_diab$I2, res_nooth_inc_diab$I2, res_nonone_inc_diab$I2)
i2_inc_diab
influence(res_inc_diab, progbar=F)

#I2 from raw numbers: educ->obese prevalence
n <- as.vector(t(table(aou$racecat)))
racecatname <-rownames(table(aou$racecat))
ESTIM_educ_ob <- c(coef(summary(mod_ob_educcont_nhw))[9,"Estimate"],coef(summary(mod_ob_educcont_nhb))[9,"Estimate"],
                   coef(summary(mod_ob_educcont_his))[9,"Estimate"],coef(summary(mod_ob_educcont_nha))[9,"Estimate"],coef(summary(mod_ob_educcont_multi))[9,"Estimate"],
                   coef(summary(mod_ob_educcont_oth))[9,"Estimate"],coef(summary(mod_ob_educcont_none))[9,"Estimate"])
SE_educ_ob <- c(coef(summary(mod_ob_educcont_nhw))[9,"Std. Error"],coef(summary(mod_ob_educcont_nhb))[9,"Std. Error"],
                coef(summary(mod_ob_educcont_his))[9,"Std. Error"],coef(summary(mod_ob_educcont_nha))[9,"Std. Error"],coef(summary(mod_ob_educcont_multi))[9,"Std. Error"],
                coef(summary(mod_ob_educcont_oth))[9,"Std. Error"],coef(summary(mod_ob_educcont_none))[9,"Std. Error"])
vi <- as.vector(t((SE_educ_ob*sqrt(n))^2)) #variance is SD^2, SD is SE*sqrt(n)
aou_dat_educ_ob <- data.frame(racecatname,n,ESTIM_educ_ob,vi,SE_educ_ob)
res_educ_ob <- rma(yi=ESTIM_educ_ob,sei=SE_educ_ob, data=aou_dat_educ_ob)
res_nonhw_educ_ob <- rma(yi=ESTIM_educ_ob,sei=SE_educ_ob, data=aou_dat_educ_ob[!racecatname=='NHW',])
res_nonhb_educ_ob <- rma(yi=ESTIM_educ_ob,sei=SE_educ_ob, data=aou_dat_educ_ob[!racecatname=='NHB',])
res_nohis_educ_ob <- rma(yi=ESTIM_educ_ob,sei=SE_educ_ob, data=aou_dat_educ_ob[!racecatname=='Hispanic',])
res_nonha_educ_ob <- rma(yi=ESTIM_educ_ob,sei=SE_educ_ob, data=aou_dat_educ_ob[!racecatname=='NHAsian',])
res_nomul_educ_ob <- rma(yi=ESTIM_educ_ob,sei=SE_educ_ob, data=aou_dat_educ_ob[!racecatname=='Multiracial',])
res_nooth_educ_ob <- rma(yi=ESTIM_educ_ob,sei=SE_educ_ob, data=aou_dat_educ_ob[!racecatname=='Other',])
res_nonone_educ_ob <- rma(yi=ESTIM_educ_ob,sei=SE_educ_ob, data=aou_dat_educ_ob[!racecatname=='None of these',])
res_nonhwnha_educ_ob <- rma(yi=ESTIM_educ_ob,sei=SE_educ_ob, data=aou_dat_educ_ob[!racecatname=='NHW' & !racecatname=='NHAsian',])
res_nhwnha_educ_ob <- rma(yi=ESTIM_educ_ob,sei=SE_educ_ob, data=aou_dat_educ_ob[racecatname=='NHW' | racecatname=='NHAsian',])
i2_educ_ob <- rbind(res_educ_ob$I2, res_nonhw_educ_ob$I2, res_nonhb_educ_ob$I2, res_nohis_educ_ob$I2, res_nonha_educ_ob$I2, res_nomul_educ_ob$I2, res_nooth_educ_ob$I2, res_nonone_educ_ob$I2)
i2_educ_ob
influence(res_educ_ob, progbar=F)

#I2 from raw numbers: inc->obese prevalence
n <- as.vector(t(table(aou$racecat)))
racecatname <-rownames(table(aou$racecat))
ESTIM_inc_ob <- c(coef(summary(mod_ob_incnum_nhw))[9,"Estimate"],coef(summary(mod_ob_incnum_nhb))[9,"Estimate"],
                  coef(summary(mod_ob_incnum_his))[9,"Estimate"],coef(summary(mod_ob_incnum_nha))[9,"Estimate"],coef(summary(mod_ob_incnum_multi))[9,"Estimate"],
                  coef(summary(mod_ob_incnum_oth))[9,"Estimate"],coef(summary(mod_ob_incnum_none))[9,"Estimate"])
SE_inc_ob <- c(coef(summary(mod_ob_incnum_nhw))[9,"Std. Error"],coef(summary(mod_ob_incnum_nhb))[9,"Std. Error"],
               coef(summary(mod_ob_incnum_his))[9,"Std. Error"],coef(summary(mod_ob_incnum_nha))[9,"Std. Error"],coef(summary(mod_ob_incnum_multi))[9,"Std. Error"],
               coef(summary(mod_ob_incnum_oth))[9,"Std. Error"],coef(summary(mod_ob_incnum_none))[9,"Std. Error"])
vi <- as.vector(t((SE_inc_ob*sqrt(n))^2)) #variance is SD^2, SD is SE*sqrt(n)
aou_dat_inc_ob <- data.frame(racecatname,n,ESTIM_inc_ob,vi,SE_inc_ob)
res_inc_ob <- rma(yi=ESTIM_inc_ob,sei=SE_inc_ob, data=aou_dat_inc_ob)
res_nonhw_inc_ob <- rma(yi=ESTIM_inc_ob,sei=SE_inc_ob, data=aou_dat_inc_ob[!racecatname=='NHW',])
res_nonhb_inc_ob <- rma(yi=ESTIM_inc_ob,sei=SE_inc_ob, data=aou_dat_inc_ob[!racecatname=='NHB',])
res_nohis_inc_ob <- rma(yi=ESTIM_inc_ob,sei=SE_inc_ob, data=aou_dat_inc_ob[!racecatname=='Hispanic',])
res_nonha_inc_ob <- rma(yi=ESTIM_inc_ob,sei=SE_inc_ob, data=aou_dat_inc_ob[!racecatname=='NHAsian',])
res_nomul_inc_ob <- rma(yi=ESTIM_inc_ob,sei=SE_inc_ob, data=aou_dat_inc_ob[!racecatname=='Multiracial',])
res_nooth_inc_ob <- rma(yi=ESTIM_inc_ob,sei=SE_inc_ob, data=aou_dat_inc_ob[!racecatname=='Other',])
res_nonone_inc_ob <- rma(yi=ESTIM_inc_ob,sei=SE_inc_ob, data=aou_dat_inc_ob[!racecatname=='None of these',])
res_nonhwnha_inc_ob <- rma(yi=ESTIM_inc_ob,sei=SE_inc_ob, data=aou_dat_inc_ob[!racecatname=='NHW' & !racecatname=='NHAsian',])
res_nhwnha_inc_ob <- rma(yi=ESTIM_inc_ob,sei=SE_inc_ob, data=aou_dat_inc_ob[racecatname=='NHW' | racecatname=='NHAsian',])
i2_inc_ob <- rbind(res_inc_ob$I2, res_nonhw_inc_ob$I2, res_nonhb_inc_ob$I2, res_nohis_inc_ob$I2, res_nonha_inc_ob$I2, res_nomul_inc_ob$I2, res_nooth_inc_ob$I2, res_nonone_inc_ob$I2)
i2_inc_ob
influence(res_inc_ob, progbar=F)


#####COME BACK HERE - NOT YET WORKING
#####mediation analysis#####
#t2d - bmi mediation - education
mod_diab_educcont_all_bmi <- glm(data=aou, diab ~ age + female + smoke_status + insur_stability + educ_num + bmi_max, family=binomial(link=logit))
mod_diab_educcont_all_raceadj_bmi <- glm(data=aou, diab ~ age + female + smoke_status + insur_stability + educ_num + bmi_max + racecat, family=binomial(link=logit))
mod_diab_educcont_nhw_bmi <- glm(data=aou[aou$racecat=='NHW',], diab ~ age + female + smoke_status + insur_stability + educ_num + bmi_max, family=binomial(link=logit))
mod_diab_educcont_nhb_bmi <- glm(data=aou[aou$racecat=='NHB',], diab ~ age + female + smoke_status + insur_stability + educ_num + bmi_max, family=binomial(link=logit))
mod_diab_educcont_his_bmi <- glm(data=aou[aou$racecat=='Hispanic',], diab ~ age + female + smoke_status + insur_stability + educ_num + bmi_max, family=binomial(link=logit))
mod_diab_educcont_nha_bmi <- glm(data=aou[aou$racecat=='NHAsian',], diab ~ age + female + smoke_status + insur_stability + educ_num + bmi_max, family=binomial(link=logit))
mod_diab_educcont_mul_bmi <- glm(data=aou[aou$racecat=='Multiracial',], diab ~ age + female + smoke_status + insur_stability + educ_num + bmi_max + bmi_max, family=binomial(link=logit))
mod_diab_educcont_oth_bmi <- glm(data=aou[aou$racecat=='Other',], diab ~ age + female + smoke_status + insur_stability + educ_num + bmi_max, family=binomial(link=logit))
mod_diab_educcont_none_bmi <- glm(data=aou[aou$racecat=='None of these',], diab ~ age + female + smoke_status + insur_stability + educ_num + bmi_max, family=binomial(link=logit))

bmi_med_educall <- glm(data=aou, bmi_max ~ age + female + smoke_status + insur_stability + educ_num)
bmi_med_educall_raceadj <- glm(data=aou, bmi_max ~ age + female + smoke_status + insur_stability + educ_num + racecat)
bmi_med_educnhw <- glm(data=aou[aou$racecat=='NHW',], bmi_max ~ age + female + smoke_status + insur_stability + educ_num)
bmi_med_educnhb <- glm(data=aou[aou$racecat=='NHB',], bmi_max ~ age + female + smoke_status + insur_stability + educ_num)
bmi_med_educhis <- glm(data=aou[aou$racecat=='Hispanic',], bmi_max ~ age + female + smoke_status + insur_stability + educ_num)
bmi_med_educnha <- glm(data=aou[aou$racecat=='NHAsian',], bmi_max ~ age + female + smoke_status + insur_stability + educ_num)
bmi_med_educmul <- glm(data=aou[aou$racecat=='Multiracial',], bmi_max ~ age + female + smoke_status + insur_stability + educ_num)
bmi_med_educoth <- glm(data=aou[aou$racecat=='Other',], bmi_max ~ age + female + smoke_status + insur_stability + educ_num)
bmi_med_educnone <- glm(data=aou[aou$racecat=='None of these',], bmi_max ~ age + female + smoke_status + insur_stability + educ_num)

t2d_propmediated_bmi_educ <- rbind(cbind('All',mod_diab_educcont_all_bmi$coefficients[9] + bmi_med_educall$coefficients[9]*mod_diab_educcont_all_bmi$coefficients[10], mod_diab_educcont_all_bmi$coefficients[9], bmi_med_educall$coefficients[9]*mod_diab_educcont_all_bmi$coefficients[10], bmi_med_educall$coefficients[9]*mod_diab_educcont_all_bmi$coefficients[10]/(mod_diab_educcont_all_bmi$coefficients[9] + bmi_med_educall$coefficients[9]*mod_diab_educcont_all_bmi$coefficients[10])),
                                   cbind('All-Race-adjusted',mod_diab_educcont_all_raceadj_bmi$coefficients[9] + bmi_med_educall_raceadj$coefficients[9]*mod_diab_educcont_all_raceadj_bmi$coefficients[10], mod_diab_educcont_all_raceadj_bmi$coefficients[9], bmi_med_educall_raceadj$coefficients[9]*mod_diab_educcont_all_bmi$coefficients[10], bmi_med_educall$coefficients[9]*mod_diab_educcont_all_raceadj_bmi$coefficients[10]/(mod_diab_educcont_all_raceadj_bmi$coefficients[9])),
                                   cbind('NHW',mod_diab_educcont_nhw_bmi$coefficients[9] + bmi_med_educnhw$coefficients[9]*mod_diab_educcont_nhw_bmi$coefficients[10], mod_diab_educcont_nhw_bmi$coefficients[9], bmi_med_educnhw$coefficients[9]*mod_diab_educcont_nhw_bmi$coefficients[10], bmi_med_educnhw$coefficients[9]*mod_diab_educcont_nhw_bmi$coefficients[10]/(mod_diab_educcont_nhw_bmi$coefficients[9] + bmi_med_educnhw$coefficients[9]*mod_diab_educcont_nhw_bmi$coefficients[10])),
                                   cbind('NHB',mod_diab_educcont_nhb_bmi$coefficients[9] + bmi_med_educnhb$coefficients[9]*mod_diab_educcont_nhb_bmi$coefficients[10], mod_diab_educcont_nhb_bmi$coefficients[9], bmi_med_educnhb$coefficients[9]*mod_diab_educcont_nhb_bmi$coefficients[10], bmi_med_educnhb$coefficients[9]*mod_diab_educcont_nhb_bmi$coefficients[10]/(mod_diab_educcont_nhb_bmi$coefficients[9] + bmi_med_educnhb$coefficients[9]*mod_diab_educcont_nhb_bmi$coefficients[10])),
                                   cbind('HIS',mod_diab_educcont_his_bmi$coefficients[9] + bmi_med_educhis$coefficients[9]*mod_diab_educcont_his_bmi$coefficients[10], mod_diab_educcont_his_bmi$coefficients[9], bmi_med_educhis$coefficients[9]*mod_diab_educcont_his_bmi$coefficients[10], bmi_med_educhis$coefficients[9]*mod_diab_educcont_his_bmi$coefficients[10]/(mod_diab_educcont_his_bmi$coefficients[9] + bmi_med_educhis$coefficients[9]*mod_diab_educcont_his_bmi$coefficients[10])),
                                   cbind('NHA',mod_diab_educcont_nha_bmi$coefficients[9] + bmi_med_educnha$coefficients[9]*mod_diab_educcont_nha_bmi$coefficients[10], mod_diab_educcont_nha_bmi$coefficients[9], bmi_med_educnha$coefficients[9]*mod_diab_educcont_nha_bmi$coefficients[10], bmi_med_educnha$coefficients[9]*mod_diab_educcont_nha_bmi$coefficients[10]/(mod_diab_educcont_nha_bmi$coefficients[9] + bmi_med_educnha$coefficients[9]*mod_diab_educcont_nha_bmi$coefficients[10])),
                                   cbind('MUL',mod_diab_educcont_mul_bmi$coefficients[9] + bmi_med_educmul$coefficients[9]*mod_diab_educcont_mul_bmi$coefficients[10], mod_diab_educcont_mul_bmi$coefficients[9], bmi_med_educmul$coefficients[9]*mod_diab_educcont_mul_bmi$coefficients[10], bmi_med_educmul$coefficients[9]*mod_diab_educcont_mul_bmi$coefficients[10]/(mod_diab_educcont_mul_bmi$coefficients[9] + bmi_med_educmul$coefficients[9]*mod_diab_educcont_mul_bmi$coefficients[10])),
                                   cbind('OTH',mod_diab_educcont_oth_bmi$coefficients[9] + bmi_med_educoth$coefficients[9]*mod_diab_educcont_oth_bmi$coefficients[10], mod_diab_educcont_oth_bmi$coefficients[9], bmi_med_educoth$coefficients[9]*mod_diab_educcont_oth_bmi$coefficients[10], bmi_med_educoth$coefficients[9]*mod_diab_educcont_oth_bmi$coefficients[10]/(mod_diab_educcont_oth_bmi$coefficients[9] + bmi_med_educoth$coefficients[9]*mod_diab_educcont_oth_bmi$coefficients[10])),
                                   cbind('NONE',mod_diab_educcont_none_bmi$coefficients[9] + bmi_med_educnone$coefficients[9]*mod_diab_educcont_none_bmi$coefficients[10], mod_diab_educcont_none_bmi$coefficients[9], bmi_med_educnone$coefficients[9]*mod_diab_educcont_none_bmi$coefficients[10], bmi_med_educnone$coefficients[9]*mod_diab_educcont_none_bmi$coefficients[10]/(mod_diab_educcont_none_bmi$coefficients[9] + bmi_med_educnone$coefficients[9]*mod_diab_educcont_none_bmi$coefficients[10])))
colnames(t2d_propmediated_bmi_educ) <- c('Race/Ethnicity Group','Total Effect','Direct Effect','Indirect Effect','Proportion Mediated')
t2d_propmediated_bmi_educ

#CIs are tiny (<0.01 for group as a whole); takes A LOT of ram to run so only ran for nha to confirm small
mod_educ_bmi_all <- glm(data=aou, bmi ~ age + female + survyr + smoke_status + insur_stability + educ_num)
mod_educ_diab_bmiadj_all <- glm(data=aou, diab ~ age + female + survyr + smoke_status + insur_stability + educ_num + bmi, family=binomial(link=logit))
set.seed(123)
med.out_all <- mediate(mod_educ_bmi_all, mod_educ_diab_bmiadj_all, treat='educ_num', mediator='bmi', robustSE=T, sims=1000)
summary(med.out_all)
mod_educ_bmi_raceadj <- glm(data=aou, bmi ~ age + female + survyr + smoke_status + insur_stability + educ_num + racecat)
mod_educ_diab_bmiadj_raceadj <- glm(data=aou, diab ~ age + female + survyr + smoke_status + insur_stability + educ_num + bmi + racecat, family=binomial(link=logit))
med.out_raceadj <- mediate(mod_educ_bmi_raceadj, mod_educ_diab_bmiadj_raceadj, treat='educ_num', mediator='bmi', robustSE=T, sims=1000)
summary(med.out_raceadj)
mod_educ_bmi_nhw <- glm(data=aou[aou$racecat=='NHW',], bmi ~ age + female + survyr + smoke_status + insur_stability + educ_num)
mod_educ_diab_bmiadj_nhw <- glm(data=aou[aou$racecat=='NHW',], diab ~ age + female + survyr + smoke_status + insur_stability + educ_num + bmi, family=binomial(link=logit))
med.out_nhw <- mediate(mod_educ_bmi_nhw, mod_educ_diab_bmiadj_nhw, treat='educ_num', mediator='bmi', robustSE=T, sims=1000)
summary(med.out_nhw)
mod_educ_bmi_nhb <- glm(data=aou[aou$racecat=='NHB',], bmi ~ age + female + survyr + smoke_status + insur_stability + educ_num)
mod_educ_diab_bmiadj_nhb <- glm(data=aou[aou$racecat=='NHB',], diab ~ age + female + survyr + smoke_status + insur_stability + educ_num + bmi, family=binomial(link=logit))
med.out_nhb <- mediate(mod_educ_bmi_nhb, mod_educ_diab_bmiadj_nhb, treat='educ_num', mediator='bmi', robustSE=T, sims=1000)
summary(med.out_nhb)
mod_educ_bmi_his <- glm(data=aou[aou$racecat=='Hispanic',], bmi ~ age + female + survyr + smoke_status + insur_stability + educ_num)
mod_educ_diab_bmiadj_his <- glm(data=aou[aou$racecat=='Hispanic',], diab ~ age + female + survyr + smoke_status + insur_stability + educ_num + bmi, family=binomial(link=logit))
med.out_his <- mediate(mod_educ_bmi_his, mod_educ_diab_bmiadj_his, treat='educ_num', mediator='bmi', robustSE=T, sims=1000)
summary(med.out_his)
mod_educ_bmi_nha <- glm(data=aou[aou$racecat=='NHAsian',], bmi ~ age + female + survyr + smoke_status + insur_stability + educ_num)
mod_educ_diab_bmiadj_nha <- glm(data=aou[aou$racecat=='NHAsian',], diab ~ age + female + survyr + smoke_status + insur_stability + educ_num + bmi, family=binomial(link=logit))
med.out_nha <- mediate(mod_educ_bmi_nha, mod_educ_diab_bmiadj_nha, treat='educ_num', mediator='bmi', robustSE=T, sims=1000)
summary(med.out_nha)
mod_educ_bmi_mul <- glm(data=aou[aou$racecat=='Multiracial',], bmi ~ age + female + survyr + smoke_status + insur_stability + educ_num)
mod_educ_diab_bmiadj_mul <- glm(data=aou[aou$racecat=='Multiracial',], diab ~ age + female + survyr + smoke_status + insur_stability + educ_num + bmi, family=binomial(link=logit))
med.out_mul <- mediate(mod_educ_bmi_mul, mod_educ_diab_bmiadj_mul, treat='educ_num', mediator='bmi', robustSE=T, sims=1000)
summary(med.out_mul)
mod_educ_bmi_oth <- glm(data=aou[aou$racecat=='Other',], bmi ~ age + female + survyr + smoke_status + insur_stability + educ_num)
mod_educ_diab_bmiadj_oth <- glm(data=aou[aou$racecat=='Other',], diab ~ age + female + survyr + smoke_status + insur_stability + educ_num + bmi, family=binomial(link=logit))
med.out_oth <- mediate(mod_educ_bmi_oth, mod_educ_diab_bmiadj_oth, treat='educ_num', mediator='bmi', robustSE=T, sims=1000)
summary(med.out_oth)
mod_educ_bmi_none <- glm(data=aou[aou$racecat=='None of these',], bmi ~ age + female + survyr + smoke_status + insur_stability + educ_num)
mod_educ_diab_bmiadj_none <- glm(data=aou[aou$racecat=='None of these',], diab ~ age + female + survyr + smoke_status + insur_stability + educ_num + bmi, family=binomial(link=logit))
med.out_none <- mediate(mod_educ_bmi_none, mod_educ_diab_bmiadj_none, treat='educ_num', mediator='bmi', robustSE=T, sims=1000)
summary(med.out_none)




#t2d - bmi mediation - income
mod_diab_inccont_all_bmi <- glm(data=aou, diab ~ age + female + smoke_status + insur_stability + inc_num + bmi_max, family=binomial(link=logit))
mod_diab_inccont_all_raceadj_bmi <- glm(data=aou, diab ~ age + female + smoke_status + insur_stability + inc_num + bmi_max + racecat, family=binomial(link=logit))
mod_diab_inccont_nhw_bmi <- glm(data=aou[aou$racecat=='NHW',], diab ~ age + female + smoke_status + insur_stability + inc_num + bmi_max, family=binomial(link=logit))
mod_diab_inccont_nhb_bmi <- glm(data=aou[aou$racecat=='NHB',], diab ~ age + female + smoke_status + insur_stability + inc_num + bmi_max, family=binomial(link=logit))
mod_diab_inccont_his_bmi <- glm(data=aou[aou$racecat=='Hispanic',], diab ~ age + female + smoke_status + insur_stability + inc_num + bmi_max, family=binomial(link=logit))
mod_diab_inccont_nha_bmi <- glm(data=aou[aou$racecat=='NHAsian',], diab ~ age + female + smoke_status + insur_stability + inc_num + bmi_max, family=binomial(link=logit))
mod_diab_inccont_mul_bmi <- glm(data=aou[aou$racecat=='Multiracial',], diab ~ age + female + smoke_status + insur_stability + inc_num + bmi_max + bmi_max, family=binomial(link=logit))
mod_diab_inccont_oth_bmi <- glm(data=aou[aou$racecat=='Other',], diab ~ age + female + smoke_status + insur_stability + inc_num + bmi_max, family=binomial(link=logit))
mod_diab_inccont_none_bmi <- glm(data=aou[aou$racecat=='None of these',], diab ~ age + female + smoke_status + insur_stability + inc_num + bmi_max, family=binomial(link=logit))

bmi_med_incall <- glm(data=aou, bmi_max ~ age + female + smoke_status + insur_stability + inc_num)
bmi_med_incall_raceadj <- glm(data=aou, bmi_max ~ age + female + smoke_status + insur_stability + inc_num + racecat)
bmi_med_incnhw <- glm(data=aou[aou$racecat=='NHW',], bmi_max ~ age + female + smoke_status + insur_stability + inc_num)
bmi_med_incnhb <- glm(data=aou[aou$racecat=='NHB',], bmi_max ~ age + female + smoke_status + insur_stability + inc_num)
bmi_med_inchis <- glm(data=aou[aou$racecat=='Hispanic',], bmi_max ~ age + female + smoke_status + insur_stability + inc_num)
bmi_med_incnha <- glm(data=aou[aou$racecat=='NHAsian',], bmi_max ~ age + female + smoke_status + insur_stability + inc_num)
bmi_med_incmul <- glm(data=aou[aou$racecat=='Multiracial',], bmi_max ~ age + female + smoke_status + insur_stability + inc_num)
bmi_med_incoth <- glm(data=aou[aou$racecat=='Other',], bmi_max ~ age + female + smoke_status + insur_stability + inc_num)
bmi_med_incnone <- glm(data=aou[aou$racecat=='None of these',], bmi_max ~ age + female + smoke_status + insur_stability + inc_num)

t2d_propmediated_bmi_inc <- rbind(cbind('All',mod_diab_inccont_all_bmi$coefficients[9] + bmi_med_incall$coefficients[9]*mod_diab_inccont_all_bmi$coefficients[10], mod_diab_inccont_all_bmi$coefficients[9], bmi_med_incall$coefficients[9]*mod_diab_inccont_all_bmi$coefficients[10], bmi_med_incall$coefficients[9]*mod_diab_inccont_all_bmi$coefficients[10]/(mod_diab_inccont_all_bmi$coefficients[9] + bmi_med_incall$coefficients[9]*mod_diab_inccont_all_bmi$coefficients[10])),
                                  cbind('All-Race-adjusted',mod_diab_inccont_all_raceadj_bmi$coefficients[9] + bmi_med_incall_raceadj$coefficients[9]*mod_diab_inccont_all_raceadj_bmi$coefficients[10], mod_diab_inccont_all_raceadj_bmi$coefficients[9], bmi_med_incall_raceadj$coefficients[9]*mod_diab_inccont_all_bmi$coefficients[10], bmi_med_incall$coefficients[9]*mod_diab_inccont_all_raceadj_bmi$coefficients[10]/(mod_diab_inccont_all_raceadj_bmi$coefficients[9])),
                                  cbind('NHW',mod_diab_inccont_nhw_bmi$coefficients[9] + bmi_med_incnhw$coefficients[9]*mod_diab_inccont_nhw_bmi$coefficients[10], mod_diab_inccont_nhw_bmi$coefficients[9], bmi_med_incnhw$coefficients[9]*mod_diab_inccont_nhw_bmi$coefficients[10], bmi_med_incnhw$coefficients[9]*mod_diab_inccont_nhw_bmi$coefficients[10]/(mod_diab_inccont_nhw_bmi$coefficients[9] + bmi_med_incnhw$coefficients[9]*mod_diab_inccont_nhw_bmi$coefficients[10])),
                                  cbind('NHB',mod_diab_inccont_nhb_bmi$coefficients[9] + bmi_med_incnhb$coefficients[9]*mod_diab_inccont_nhb_bmi$coefficients[10], mod_diab_inccont_nhb_bmi$coefficients[9], bmi_med_incnhb$coefficients[9]*mod_diab_inccont_nhb_bmi$coefficients[10], bmi_med_incnhb$coefficients[9]*mod_diab_inccont_nhb_bmi$coefficients[10]/(mod_diab_inccont_nhb_bmi$coefficients[9] + bmi_med_incnhb$coefficients[9]*mod_diab_inccont_nhb_bmi$coefficients[10])),
                                  cbind('HIS',mod_diab_inccont_his_bmi$coefficients[9] + bmi_med_inchis$coefficients[9]*mod_diab_inccont_his_bmi$coefficients[10], mod_diab_inccont_his_bmi$coefficients[9], bmi_med_inchis$coefficients[9]*mod_diab_inccont_his_bmi$coefficients[10], bmi_med_inchis$coefficients[9]*mod_diab_inccont_his_bmi$coefficients[10]/(mod_diab_inccont_his_bmi$coefficients[9] + bmi_med_inchis$coefficients[9]*mod_diab_inccont_his_bmi$coefficients[10])),
                                  cbind('NHA',mod_diab_inccont_nha_bmi$coefficients[9] + bmi_med_incnha$coefficients[9]*mod_diab_inccont_nha_bmi$coefficients[10], mod_diab_inccont_nha_bmi$coefficients[9], bmi_med_incnha$coefficients[9]*mod_diab_inccont_nha_bmi$coefficients[10], bmi_med_incnha$coefficients[9]*mod_diab_inccont_nha_bmi$coefficients[10]/(mod_diab_inccont_nha_bmi$coefficients[9] + bmi_med_incnha$coefficients[9]*mod_diab_inccont_nha_bmi$coefficients[10])),
                                  cbind('MUL',mod_diab_inccont_mul_bmi$coefficients[9] + bmi_med_incmul$coefficients[9]*mod_diab_inccont_mul_bmi$coefficients[10], mod_diab_inccont_mul_bmi$coefficients[9], bmi_med_incmul$coefficients[9]*mod_diab_inccont_mul_bmi$coefficients[10], bmi_med_incmul$coefficients[9]*mod_diab_inccont_mul_bmi$coefficients[10]/(mod_diab_inccont_mul_bmi$coefficients[9] + bmi_med_incmul$coefficients[9]*mod_diab_inccont_mul_bmi$coefficients[10])),
                                  cbind('OTH',mod_diab_inccont_oth_bmi$coefficients[9] + bmi_med_incoth$coefficients[9]*mod_diab_inccont_oth_bmi$coefficients[10], mod_diab_inccont_oth_bmi$coefficients[9], bmi_med_incoth$coefficients[9]*mod_diab_inccont_oth_bmi$coefficients[10], bmi_med_incoth$coefficients[9]*mod_diab_inccont_oth_bmi$coefficients[10]/(mod_diab_inccont_oth_bmi$coefficients[9] + bmi_med_incoth$coefficients[9]*mod_diab_inccont_oth_bmi$coefficients[10])),
                                  cbind('NONE',mod_diab_inccont_none_bmi$coefficients[9] + bmi_med_incnone$coefficients[9]*mod_diab_inccont_none_bmi$coefficients[10], mod_diab_inccont_none_bmi$coefficients[9], bmi_med_incnone$coefficients[9]*mod_diab_inccont_none_bmi$coefficients[10], bmi_med_incnone$coefficients[9]*mod_diab_inccont_none_bmi$coefficients[10]/(mod_diab_inccont_none_bmi$coefficients[9] + bmi_med_incnone$coefficients[9]*mod_diab_inccont_none_bmi$coefficients[10])))
colnames(t2d_propmediated_bmi_inc) <- c('Race/Ethnicity Group','Total Effect','Direct Effect','Indirect Effect','Proportion Mediated')
t2d_propmediated_bmi_inc

aou_mediation_results <- rbind(t2d_propmediated_bmi_educ, t2d_propmediated_bmi_inc)
dim(aou_mediation_results)
aou_mediation_results <- as.data.frame(aou_mediation_results)
write_excel_csv(aou_mediation_results, "aou_mediation_models.csv") 



#####visuals: forest plots, categorical income and educ - diabetes#####
#tiff(file='C:/Users/sarac/OneDrive/Documents/Research/Area v Individual SES/aou_forest_indiv_educ_racestrat.tiff', width=7, height=5, units='in', res=300)
forestplot(c("HS Degree","Some HS","No HS"), 
           mean = cbind(aou_educ_diab_result[9:11,1],aou_educ_diab_result[9:11,5],aou_educ_diab_result[9:11,9],
                        aou_educ_diab_result[9:11,13],aou_educ_diab_result[9:11,17],aou_educ_diab_result[9:11,21],
                        aou_educ_diab_result[9:11,25],aou_educ_diab_result[9:11,29],aou_educ_diab_result[9:11,33]),
           lower = cbind(aou_educ_diab_result[9:11,2],aou_educ_diab_result[9:11,6],aou_educ_diab_result[9:11,10],
                         aou_educ_diab_result[9:11,14],aou_educ_diab_result[9:11,18],aou_educ_diab_result[9:11,22],
                         aou_educ_diab_result[9:11,26],aou_educ_diab_result[9:11,30],aou_educ_diab_result[9:11,34]),
           upper = cbind(aou_educ_diab_result[9:11,3],aou_educ_diab_result[9:11,7],aou_educ_diab_result[9:11,11],
                         aou_educ_diab_result[9:11,15],aou_educ_diab_result[9:11,19],aou_educ_diab_result[9:11,23],
                         aou_educ_diab_result[9:11,27],aou_educ_diab_result[9:11,31],aou_educ_diab_result[9:11,35]),
           col = fpColors(box = c('gray23','gray57','dodgerblue4','darkslategray3','palegreen3','salmon1','indianred3','mediumpurple4','maroon')),
           legend=c("All","All, Race-adjusted","NHW","NHB","Hispanic","NHAsian","Multiracial","Other","None of these"),
           legend_args = fpLegend(pos = "right"),#list(x = 0.5, y = -0.2), gp = gpar(cex=0.9,col = "#CCCCCC", fill = "#F9F9F9")),
           #legend=c("NHW","NHB","NHA","Mexican-American","Other Hispanic"),
           #legend_args = fpLegend(pos = list(x = 0.9, y = 0.9), gp = gpar(cex=0.9,col = "#CCCCCC", fill = "#F9F9F9")),
           xlab = "OR (95% CI)",
           title = 'Adjusted OR for Educational Attainment (Ref=College Degree) \n on Diabetes, Stratified by Race/Ethnicity',
           txt_gp = fpTxtGp(title=gpar(cex=1),xlab=gpar(cex=1), ticks=gpar(cex=1), label=gpar(cex=1)),
           vertices=TRUE,
           line.margin=0.25,
           graph.pos=2,
           clip=c(0.5,4.0), xlim=c(0.5,4.0), xlog=TRUE, grid=structure(c(0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0)),
           xticks=c(-0.693147180559957,0,0.405465108108171,0.693147180559957,0.91629073187417,1.09861228866813,1.25276296849539,1.38629436111991))
#dev.off()

aou_educ_diab_result_transpose <- t(aou_educ_diab_result)
rownames(aou_educ_diab_result_transpose)
#tiff(file='C:/Users/sarac/OneDrive/Documents/Research/Area v Individual SES/aou_forest_indiv_educ_racestrat_transpose.tiff', width=7, height=7, units='in', res=300)
png('SuppFig4E_aou_forest_indiv_diab_educcat_racestrat_transpose.png', width=7, height=7, units='in', res=300)
forestplot(c("All","All, Race-adjusted","NHW","NHB","Hispanic","NHAsian","Multiracial","Other","None of these"), 
           mean = cbind(aou_educ_diab_result_transpose[c(1,5,9,13,17,21,25,29,33),9],aou_educ_diab_result_transpose[c(1,5,9,13,17,21,25,29,33),10],aou_educ_diab_result_transpose[c(1,5,9,13,17,21,25,29,33),11]),
           lower = cbind(aou_educ_diab_result_transpose[c(2,6,10,14,18,22,26,30,34),9],aou_educ_diab_result_transpose[c(2,6,10,14,18,22,26,30,34),10],aou_educ_diab_result_transpose[c(2,6,10,14,18,22,26,30,34),11]),
           upper = cbind(aou_educ_diab_result_transpose[c(3,7,11,15,19,23,27,31,35),9],aou_educ_diab_result_transpose[c(3,7,11,15,19,23,27,31,35),10],aou_educ_diab_result_transpose[c(3,7,11,15,19,23,27,31,35),11]),
           col = fpColors(box = c('darkslategray3','lightgoldenrod','indianred3')),
           legend=c("Some College","HS Degree","Less than HS"),
           legend_args = fpLegend(pos = list(x = 0.1, y = 0.9), gp = gpar(cex=0.9,col = "#CCCCCC", fill = "#F9F9F9")),
           xlab = "OR (95% CI)",
           title = 'Adjusted OR for Educational Attainment (Ref=College Degree) \n on Diabetes, Stratified by Race/Ethnicity',
           txt_gp = fpTxtGp(title=gpar(cex=1),xlab=gpar(cex=1), ticks=gpar(cex=1), label=gpar(cex=1)),
           vertices=TRUE,
           line.margin=0.25,
           graph.pos=2,
           clip=c(0.5,4.0), xlog=T, grid=structure(c(0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0)),
           xticks=c(-0.693147180559957,0,0.405465108108171,0.693147180559957,0.91629073187417,1.09861228866813,1.25276296849539,1.38629436111991)) #xlim=c(0.5,4.0), xticks=c(0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0),
dev.off()


#tiff(file='C:/Users/sarac/OneDrive/Documents/Research/Area v Individual SES/aou_forest_indiv_inc_racestrat.tiff', width=7, height=5, units='in', res=300)
forestplot(c("$75,000-149,999","$50,000-74,999","$25,000-49,999","Under $25,000"), 
           mean = cbind(aou_inc_diab_result[9:12,1],aou_inc_diab_result[9:12,5],aou_inc_diab_result[9:12,9],
                        aou_inc_diab_result[9:12,13],aou_inc_diab_result[9:12,17],aou_inc_diab_result[9:12,21],
                        aou_inc_diab_result[9:12,25],aou_inc_diab_result[9:12,29],aou_inc_diab_result[9:12,33]),
           lower = cbind(aou_inc_diab_result[9:12,2],aou_inc_diab_result[9:12,6],aou_inc_diab_result[9:12,10],
                         aou_inc_diab_result[9:12,14],aou_inc_diab_result[9:12,18],aou_inc_diab_result[9:12,22],
                         aou_inc_diab_result[9:12,26],aou_inc_diab_result[9:12,30],aou_inc_diab_result[9:12,34]),
           upper = cbind(aou_inc_diab_result[9:12,3],aou_inc_diab_result[9:12,7],aou_inc_diab_result[9:12,11],
                         aou_inc_diab_result[9:12,15],aou_inc_diab_result[9:12,19],aou_inc_diab_result[9:12,23],
                         aou_inc_diab_result[9:12,27],aou_inc_diab_result[9:12,31],aou_inc_diab_result[9:12,35]),
           col = fpColors(box = c('gray23','gray57','dodgerblue4','darkslategray3','palegreen3','salmon1','indianred3','mediumpurple4','maroon')),
           legend=c("All","All, Race-adjusted","NHW","NHB","Hispanic","NHAsian","Multiracial","Other","None of these"),
           legend_args = fpLegend(pos = list(x = 0.9, y = 0.9), gp = gpar(cex=0.9,col = "#CCCCCC", fill = "#F9F9F9")),
           xlab = "OR (95% CI)",
           title = 'Adjusted OR for Income Category (Ref= More than $75,000) \n on Diabetes, Stratified by Race/Ethnicity',
           txt_gp = fpTxtGp(title=gpar(cex=1),xlab=gpar(cex=1), ticks=gpar(cex=1), label=gpar(cex=1)),
           vertices=TRUE,
           line.margin=0.25,
           graph.pos=2,
           clip=c(0.5,2.5), xlim=c(0.5,2.5), xlog=TRUE, grid=structure(c(0.5,1.0,1.5,2.0,2.5)),
           xticks=c(-0.693147180559957,0,0.405465108108171,0.693147180559957,0.91629073187417)) 
#dev.off()

aou_inc_diab_result_transpose <- t(aou_inc_diab_result)
rownames(aou_inc_diab_result_transpose)
#tiff(file='C:/Users/sarac/OneDrive/Documents/Research/Area v Individual SES/aou_forest_indiv_educ_racestrat_transpose.tiff', width=7, height=5, units='in', res=300)
png('SuppFig4F_aou_forest_indiv_diab_inccat_racestrat_transpose.png', width=7, height=7, units='in', res=300)
forestplot(c("All","All, Race-adjusted","NHW","NHB","Hispanic","NHAsian","Multiracial","Other","None of these"), 
           mean = cbind(aou_inc_diab_result_transpose[c(1,5,9,13,17,21,25,29,33),9],aou_inc_diab_result_transpose[c(1,5,9,13,17,21,25,29,33),10],aou_inc_diab_result_transpose[c(1,5,9,13,17,21,25,29,33),11],aou_inc_diab_result_transpose[c(1,5,9,13,17,21,25,29,33),12]),
           lower = cbind(aou_inc_diab_result_transpose[c(2,6,10,14,18,22,26,30,34),9],aou_inc_diab_result_transpose[c(2,6,10,14,18,22,26,30,34),10],aou_inc_diab_result_transpose[c(2,6,10,14,18,22,26,30,34),11],aou_inc_diab_result_transpose[c(2,6,10,14,18,22,26,30,34),12]),
           upper = cbind(aou_inc_diab_result_transpose[c(3,7,11,15,19,23,27,31,35),9],aou_inc_diab_result_transpose[c(3,7,11,15,19,23,27,31,35),10],aou_inc_diab_result_transpose[c(3,7,11,15,19,23,27,31,35),11],aou_inc_diab_result_transpose[c(3,7,11,15,19,23,27,31,35),12]),
           col = fpColors(box = c('mediumpurple4','darkslategray3','lightgoldenrod','indianred3')),
           legend=c("$75,000-149,999","$50,000-74,999","$25,000-49,999","Under $25,000"),
           legend_args = fpLegend(pos = list(x = 0.111, y = 0.9), gp = gpar(cex=0.9,col = "#CCCCCC", fill = "#F9F9F9")),
           xlab = "OR (95% CI)",
           title = 'Adjusted OR for Income (Ref=More than $150,000) \n on Diabetes, Stratified by Race/Ethnicity',
           txt_gp = fpTxtGp(title=gpar(cex=1),xlab=gpar(cex=1), ticks=gpar(cex=1), label=gpar(cex=1)),
           vertices=TRUE,
           line.margin=0.25,
           graph.pos=2,
           clip=c(0.5,4.0), xlim=c(0.5,4.0), xlog=TRUE, grid=structure(c(0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0)),
           xticks=c(-0.693147180559957,0,0.405465108108171,0.693147180559957,0.91629073187417,1.09861228866813,1.25276296849539,1.38629436111991)) 
dev.off()

#####visuals: forest plots, categorical income and educ - obesity#####
aou_educ_ob_result_transpose <- t(aou_educ_ob_result)
rownames(aou_educ_ob_result_transpose)
#tiff(file='C:/Users/sarac/OneDrive/Documents/Research/Area v Individual SES/aou_forest_indiv_educ_racestrat_transpose.tiff', width=7, height=5, units='in', res=300)
png('SuppFig4G_aou_forest_indiv_ob_educcat_racestrat_transpose.png', width=7, height=7, units='in', res=300)
forestplot(c("All","All, Race-adjusted","NHW","NHB","Hispanic","NHAsian","Multiracial","Other","None of these"), 
           mean = cbind(aou_educ_ob_result_transpose[c(1,5,9,13,17,21,25,29,33),9],aou_educ_ob_result_transpose[c(1,5,9,13,17,21,25,29,33),10],aou_educ_ob_result_transpose[c(1,5,9,13,17,21,25,29,33),11]),
           lower = cbind(aou_educ_ob_result_transpose[c(2,6,10,14,18,22,26,30,34),9],aou_educ_ob_result_transpose[c(2,6,10,14,18,22,26,30,34),10],aou_educ_ob_result_transpose[c(2,6,10,14,18,22,26,30,34),11]),
           upper = cbind(aou_educ_ob_result_transpose[c(3,7,11,15,19,23,27,31,35),9],aou_educ_ob_result_transpose[c(3,7,11,15,19,23,27,31,35),10],aou_educ_ob_result_transpose[c(3,7,11,15,19,23,27,31,35),11]),
           col = fpColors(box = c('darkslategray3','lightgoldenrod','indianred3')),
           legend=c("Some College","HS Degree","Less than HS"),
           legend_args = fpLegend(pos = list(x = 0.9, y = 0.9), gp = gpar(cex=0.9,col = "#CCCCCC", fill = "#F9F9F9")),
           xlab = "OR (95% CI)",
           title = 'Adjusted OR for Educational Attainment (Ref=College Degree) \n on Obesity, Stratified by Race/Ethnicity',
           txt_gp = fpTxtGp(title=gpar(cex=1),xlab=gpar(cex=1), ticks=gpar(cex=1), label=gpar(cex=1)),
           vertices=TRUE,
           line.margin=0.25,
           graph.pos=2,
           clip=c(0.5,4.0), xlog=T, grid=structure(c(0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0)),
           xticks=c(-0.693147180559957,0,0.405465108108171,0.693147180559957,0.91629073187417,1.09861228866813,1.25276296849539,1.38629436111991)) #xlim=c(0.5,4.0), xticks=c(0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0),
dev.off()


aou_inc_ob_result_transpose <- t(aou_inc_ob_result)
rownames(aou_inc_ob_result_transpose)
#tiff(file='C:/Users/sarac/OneDrive/Documents/Research/Area v Individual SES/aou_forest_indiv_educ_racestrat_transpose.tiff', width=7, height=5, units='in', res=300)
png('SuppFig4H_aou_forest_indiv_ob_inccat_racestrat_transpose.png', width=7, height=7, units='in', res=300)
forestplot(c("All","All, Race-adjusted","NHW","NHB","Hispanic","NHAsian","Multiracial","Other","None of these"), 
           mean = cbind(aou_inc_ob_result_transpose[c(1,5,9,13,17,21,25,29,33),9],aou_inc_ob_result_transpose[c(1,5,9,13,17,21,25,29,33),10],aou_inc_ob_result_transpose[c(1,5,9,13,17,21,25,29,33),11],aou_inc_ob_result_transpose[c(1,5,9,13,17,21,25,29,33),12]),
           lower = cbind(aou_inc_ob_result_transpose[c(2,6,10,14,18,22,26,30,34),9],aou_inc_ob_result_transpose[c(2,6,10,14,18,22,26,30,34),10],aou_inc_ob_result_transpose[c(2,6,10,14,18,22,26,30,34),11],aou_inc_ob_result_transpose[c(2,6,10,14,18,22,26,30,34),12]),
           upper = cbind(aou_inc_ob_result_transpose[c(3,7,11,15,19,23,27,31,35),9],aou_inc_ob_result_transpose[c(3,7,11,15,19,23,27,31,35),10],aou_inc_ob_result_transpose[c(3,7,11,15,19,23,27,31,35),11],aou_inc_ob_result_transpose[c(3,7,11,15,19,23,27,31,35),12]),
           col = fpColors(box = c('mediumpurple4','darkslategray3','lightgoldenrod','indianred3')),
           legend=c("$75,000-149,999","$50,000-74,999","$25,000-49,999","Under $25,000"),
           legend_args = fpLegend(pos = list(x = 0.12, y = 0.9), gp = gpar(cex=0.9,col = "#CCCCCC", fill = "#F9F9F9")),
           xlab = "OR (95% CI)",
           title = 'Adjusted OR for Income (Ref=More than $150,000) \n on Obesity, Stratified by Race/Ethnicity',
           txt_gp = fpTxtGp(title=gpar(cex=1),xlab=gpar(cex=1), ticks=gpar(cex=1), label=gpar(cex=1)),
           vertices=TRUE,
           line.margin=0.25,
           graph.pos=2,
           clip=c(0.5,4.0), xlim=c(0.5,4.0), xlog=TRUE, grid=structure(c(0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0)),
           xticks=c(-0.693147180559957,0,0.405465108108171,0.693147180559957,0.91629073187417,1.09861228866813,1.25276296849539,1.38629436111991)) 
dev.off()




#####visuals: forest plots, continuous income and educ#####
aou_educcont_diabob_result <- cbind(aou_educcont_diab_result,aou_educcont_ob_result)
#tiff(file='C:/Users/sarac/OneDrive/Documents/Research/Area v Individual SES/Fig2A_aou_forest_indiv_educcont_stratandnonstrat.tiff', width=8, height=5, units='in', res=300)
#par(xpd=T)
png('Fig2A_aou_forest_indiv_educcont_stratandnonstrat.png', width=8, height=5, units='in', res=300)
forestplot(c('Type 2 Diabetes','Obesity'), 
           mean = cbind(t(aou_educcont_diabob_result[9,c(1,37)]),t(aou_educcont_diabob_result[9,c(5,41)]),
                        t(aou_educcont_diabob_result[9,c(9,45)]),t(aou_educcont_diabob_result[9,c(13,49)]),
                        t(aou_educcont_diabob_result[9,c(17,53)]),t(aou_educcont_diabob_result[9,c(21,57)]),
                        t(aou_educcont_diabob_result[9,c(25,61)]),t(aou_educcont_diabob_result[9,c(29,65)]),
                        t(aou_educcont_diabob_result[9,c(33,69)])),
           lower = cbind(t(aou_educcont_diabob_result[9,c(2,38)]),t(aou_educcont_diabob_result[9,c(6,42)]),
                         t(aou_educcont_diabob_result[9,c(10,46)]),t(aou_educcont_diabob_result[9,c(14,50)]),
                         t(aou_educcont_diabob_result[9,c(18,54)]),t(aou_educcont_diabob_result[9,c(22,58)]),
                         t(aou_educcont_diabob_result[9,c(26,62)]),t(aou_educcont_diabob_result[9,c(30,66)]),
                         t(aou_educcont_diabob_result[9,c(34,68)])),
           upper = cbind(t(aou_educcont_diabob_result[9,c(3,39)]),t(aou_educcont_diabob_result[9,c(7,43)]),
                         t(aou_educcont_diabob_result[9,c(11,47)]),t(aou_educcont_diabob_result[9,c(15,51)]),
                         t(aou_educcont_diabob_result[9,c(19,55)]),t(aou_educcont_diabob_result[9,c(23,59)]),
                         t(aou_educcont_diabob_result[9,c(27,63)]),t(aou_educcont_diabob_result[9,c(31,67)]),
                         t(aou_educcont_diabob_result[9,c(35,71)])),
           col = fpColors(box = c('gray23','gray57','dodgerblue4','darkslategray3','palegreen3','salmon1','indianred3','mediumpurple4','maroon')),
           legend=c("All","All, Race-adjusted","NHW","NHB","Hispanic","NHAsian","Multiracial","Other","None of these"),
           legend_args = fpLegend(pos = "right"),#list(x = 0.5, y = -0.2), gp = gpar(cex=0.9,col = "#CCCCCC", fill = "#F9F9F9")),
           xlab = "OR (95% CI)",
           title = 'Adjusted OR for Continuous Education on T2D and Obesity Prevalence, \n Overall and Stratified by Race/Ethnicity',
           txt_gp = fpTxtGp(title=gpar(cex=1),xlab=gpar(cex=1), ticks=gpar(cex=1), label=gpar(cex=1)),
           vertices=TRUE,
           line.margin=0.25,
           graph.pos=2,
           clip=c(0.7,1.1), xlog=T, grid=structure(c(0.7,0.8,0.9,1.0,1.1)),
           xticks=c(-0.356674943938738,-0.223143551314213,-0.105360515657828,0,0.0953101798043265))#, xticks=c(0.7,0.8,0.9,1.0,1.1))#grid=structure(c(0.75,0.8,0.9,1.0,1.1)))
dev.off()


aou_incnum_diabob_result <- cbind(aou_incnum_diab_result,aou_incnum_ob_result)
#tiff(file='C:/Users/sarac/OneDrive/Documents/Research/Area v Individual SES/Fig2A_aou_forest_indiv_inccont_stratandnonstrat.tiff', width=8, height=5, units='in', res=300)
#par(xpd=T)
png('Fig2B_aou_forest_indiv_inccont_stratandnonstrat.png', width=8, height=5, units='in', res=300)
forestplot(c('Type 2 Diabetes','Obesity'), 
           mean = cbind(t(aou_incnum_diabob_result[9,c(1,37)]),t(aou_incnum_diabob_result[9,c(5,41)]),
                        t(aou_incnum_diabob_result[9,c(9,45)]),t(aou_incnum_diabob_result[9,c(13,49)]),
                        t(aou_incnum_diabob_result[9,c(17,53)]),t(aou_incnum_diabob_result[9,c(21,57)]),
                        t(aou_incnum_diabob_result[9,c(25,61)]),t(aou_incnum_diabob_result[9,c(29,65)]),
                        t(aou_incnum_diabob_result[9,c(33,69)])),
           lower = cbind(t(aou_incnum_diabob_result[9,c(2,38)]),t(aou_incnum_diabob_result[9,c(6,42)]),
                         t(aou_incnum_diabob_result[9,c(10,46)]),t(aou_incnum_diabob_result[9,c(14,50)]),
                         t(aou_incnum_diabob_result[9,c(18,54)]),t(aou_incnum_diabob_result[9,c(22,58)]),
                         t(aou_incnum_diabob_result[9,c(26,62)]),t(aou_incnum_diabob_result[9,c(30,66)]),
                         t(aou_incnum_diabob_result[9,c(34,68)])),
           upper = cbind(t(aou_incnum_diabob_result[9,c(3,39)]),t(aou_incnum_diabob_result[9,c(7,43)]),
                         t(aou_incnum_diabob_result[9,c(11,47)]),t(aou_incnum_diabob_result[9,c(15,51)]),
                         t(aou_incnum_diabob_result[9,c(19,55)]),t(aou_incnum_diabob_result[9,c(23,59)]),
                         t(aou_incnum_diabob_result[9,c(27,63)]),t(aou_incnum_diabob_result[9,c(31,67)]),
                         t(aou_incnum_diabob_result[9,c(35,71)])),
           col = fpColors(box = c('gray23','gray57','dodgerblue4','darkslategray3','palegreen3','salmon1','indianred3','mediumpurple4','maroon')),
           legend=c("All","All, Race-adjusted","NHW","NHB","Hispanic","NHAsian","Multiracial","Other","None of these"),
           legend_args = fpLegend(pos = "right"),#list(x = 0.5, y = -0.2), gp = gpar(cex=0.9,col = "#CCCCCC", fill = "#F9F9F9")),
           xlab = "OR (95% CI)",
           title = 'Adjusted OR for Continuous Income on T2D and Obesity Prevalence, \n Overall and Stratified by Race/Ethnicity',
           txt_gp = fpTxtGp(title=gpar(cex=1),xlab=gpar(cex=1), ticks=gpar(cex=1), label=gpar(cex=1)),
           vertices=TRUE,
           line.margin=0.25,
           graph.pos=2,
           clip=c(0.7,1.1), xlog=T,grid=structure(c(0.7,0.8,0.9,1.0,1.1)),
           xticks=c(-0.356674943938738,-0.223143551314213,-0.105360515657828,0,0.0953101798043265))#xlim=c(0.7,1.1),xticks=c(0.75,0.8,0.9,1.0,1.1), , xticks=c(0.75,0.8,0.9,1.0,1.1), xlog=TRUE, grid=structure(c(0.75,0.8,0.9,1.0,1.1)))
dev.off()


#####visuals: forest plots, QB and MP continuous income and educ#####
#tiff(file='C:/Users/sarac/OneDrive/Documents/Research/Area v Individual SES/Fig2A_aou_forest_indiv_educcont_stratandnonstrat.tiff', width=8, height=5, units='in', res=300)
#par(xpd=T)
png('SuppFig_aou_forest_indiv_educcont_stratandnonstrat_qb.png', width=8, height=5, units='in', res=300)
forestplot(c('Type 2 Diabetes','Obesity'), 
           mean = cbind(t(aou_educcont_diabob_qb_result[9,c(1,37)]),t(aou_educcont_diabob_qb_result[9,c(5,41)]),
                        t(aou_educcont_diabob_qb_result[9,c(9,45)]),t(aou_educcont_diabob_qb_result[9,c(13,49)]),
                        t(aou_educcont_diabob_qb_result[9,c(17,53)]),t(aou_educcont_diabob_qb_result[9,c(21,57)]),
                        t(aou_educcont_diabob_qb_result[9,c(25,61)]),t(aou_educcont_diabob_qb_result[9,c(29,65)]),
                        t(aou_educcont_diabob_qb_result[9,c(33,69)])),
           lower = cbind(t(aou_educcont_diabob_qb_result[9,c(2,38)]),t(aou_educcont_diabob_qb_result[9,c(6,42)]),
                         t(aou_educcont_diabob_qb_result[9,c(10,46)]),t(aou_educcont_diabob_qb_result[9,c(14,50)]),
                         t(aou_educcont_diabob_qb_result[9,c(18,54)]),t(aou_educcont_diabob_qb_result[9,c(22,58)]),
                         t(aou_educcont_diabob_qb_result[9,c(26,62)]),t(aou_educcont_diabob_qb_result[9,c(30,66)]),
                         t(aou_educcont_diabob_qb_result[9,c(34,68)])),
           upper = cbind(t(aou_educcont_diabob_qb_result[9,c(3,39)]),t(aou_educcont_diabob_qb_result[9,c(7,43)]),
                         t(aou_educcont_diabob_qb_result[9,c(11,47)]),t(aou_educcont_diabob_qb_result[9,c(15,51)]),
                         t(aou_educcont_diabob_qb_result[9,c(19,55)]),t(aou_educcont_diabob_qb_result[9,c(23,59)]),
                         t(aou_educcont_diabob_qb_result[9,c(27,63)]),t(aou_educcont_diabob_qb_result[9,c(31,67)]),
                         t(aou_educcont_diabob_qb_result[9,c(35,71)])),
           col = fpColors(box = c('gray23','gray57','dodgerblue4','darkslategray3','palegreen3','salmon1','indianred3','mediumpurple4','maroon')),
           legend=c("All","All, Race-adjusted","NHW","NHB","Hispanic","NHAsian","Multiracial","Other","None of these"),
           legend_args = fpLegend(pos = "right"),#list(x = 0.5, y = -0.2), gp = gpar(cex=0.9,col = "#CCCCCC", fill = "#F9F9F9")),
           xlab = "OR (95% CI)",
           title = 'Adjusted OR for Continuous Education on T2D and Obesity Prevalence, \n Overall and Stratified by Race/Ethnicity \n (Quasi-Binomial Regression)',
           txt_gp = fpTxtGp(title=gpar(cex=1),xlab=gpar(cex=1), ticks=gpar(cex=1), label=gpar(cex=1)),
           vertices=TRUE,
           line.margin=0.25,
           graph.pos=2,
           clip=c(0.7,1.1), xlog=T, grid=structure(c(0.7,0.8,0.9,1.0,1.1)),
           xticks=c(-0.356674943938738,-0.223143551314213,-0.105360515657828,0,0.0953101798043265))#, xticks=c(0.7,0.8,0.9,1.0,1.1))#grid=structure(c(0.75,0.8,0.9,1.0,1.1)))
dev.off()


#tiff(file='C:/Users/sarac/OneDrive/Documents/Research/Area v Individual SES/Fig2A_aou_forest_indiv_inccont_stratandnonstrat.tiff', width=8, height=5, units='in', res=300)
#par(xpd=T)
png('SuppFig_aou_forest_indiv_inccont_stratandnonstrat_qb.png', width=8, height=5, units='in', res=300)
forestplot(c('Type 2 Diabetes','Obesity'), 
           mean = cbind(t(aou_incnum_diabob_qb_result[9,c(1,37)]),t(aou_incnum_diabob_qb_result[9,c(5,41)]),
                        t(aou_incnum_diabob_qb_result[9,c(9,45)]),t(aou_incnum_diabob_qb_result[9,c(13,49)]),
                        t(aou_incnum_diabob_qb_result[9,c(17,53)]),t(aou_incnum_diabob_qb_result[9,c(21,57)]),
                        t(aou_incnum_diabob_qb_result[9,c(25,61)]),t(aou_incnum_diabob_qb_result[9,c(29,65)]),
                        t(aou_incnum_diabob_qb_result[9,c(33,69)])),
           lower = cbind(t(aou_incnum_diabob_qb_result[9,c(2,38)]),t(aou_incnum_diabob_qb_result[9,c(6,42)]),
                         t(aou_incnum_diabob_qb_result[9,c(10,46)]),t(aou_incnum_diabob_qb_result[9,c(14,50)]),
                         t(aou_incnum_diabob_qb_result[9,c(18,54)]),t(aou_incnum_diabob_qb_result[9,c(22,58)]),
                         t(aou_incnum_diabob_qb_result[9,c(26,62)]),t(aou_incnum_diabob_qb_result[9,c(30,66)]),
                         t(aou_incnum_diabob_qb_result[9,c(34,68)])),
           upper = cbind(t(aou_incnum_diabob_qb_result[9,c(3,39)]),t(aou_incnum_diabob_qb_result[9,c(7,43)]),
                         t(aou_incnum_diabob_qb_result[9,c(11,47)]),t(aou_incnum_diabob_qb_result[9,c(15,51)]),
                         t(aou_incnum_diabob_qb_result[9,c(19,55)]),t(aou_incnum_diabob_qb_result[9,c(23,59)]),
                         t(aou_incnum_diabob_qb_result[9,c(27,63)]),t(aou_incnum_diabob_qb_result[9,c(31,67)]),
                         t(aou_incnum_diabob_qb_result[9,c(35,71)])),
           col = fpColors(box = c('gray23','gray57','dodgerblue4','darkslategray3','palegreen3','salmon1','indianred3','mediumpurple4','maroon')),
           legend=c("All","All, Race-adjusted","NHW","NHB","Hispanic","NHAsian","Multiracial","Other","None of these"),
           legend_args = fpLegend(pos = "right"),#list(x = 0.5, y = -0.2), gp = gpar(cex=0.9,col = "#CCCCCC", fill = "#F9F9F9")),
           xlab = "OR (95% CI)",
           title = 'Adjusted OR for Continuous Income on T2D and Obesity Prevalence, \n Overall and Stratified by Race/Ethnicity \n (Quasi-Binomial Regression)',
           txt_gp = fpTxtGp(title=gpar(cex=1),xlab=gpar(cex=1), ticks=gpar(cex=1), label=gpar(cex=1)),
           vertices=TRUE,
           line.margin=0.25,
           graph.pos=2,
           clip=c(0.7,1.1), xlog=T,grid=structure(c(0.7,0.8,0.9,1.0,1.1)),
           xticks=c(-0.356674943938738,-0.223143551314213,-0.105360515657828,0,0.0953101798043265))#xlim=c(0.7,1.1),xticks=c(0.75,0.8,0.9,1.0,1.1), , xticks=c(0.75,0.8,0.9,1.0,1.1), xlog=TRUE, grid=structure(c(0.75,0.8,0.9,1.0,1.1)))
dev.off()


png('SuppFig_aou_forest_indiv_educcont_stratandnonstrat_mp.png', width=8, height=5, units='in', res=300)
forestplot(c('Type 2 Diabetes','Obesity'), 
           mean = cbind(t(aou_educcont_diabob_mp_result[9,c(1,37)]),t(aou_educcont_diabob_mp_result[9,c(5,41)]),
                        t(aou_educcont_diabob_mp_result[9,c(9,45)]),t(aou_educcont_diabob_mp_result[9,c(13,49)]),
                        t(aou_educcont_diabob_mp_result[9,c(17,53)]),t(aou_educcont_diabob_mp_result[9,c(21,57)]),
                        t(aou_educcont_diabob_mp_result[9,c(25,61)]),t(aou_educcont_diabob_mp_result[9,c(29,65)]),
                        t(aou_educcont_diabob_mp_result[9,c(33,69)])),
           lower = cbind(t(aou_educcont_diabob_mp_result[9,c(2,38)]),t(aou_educcont_diabob_mp_result[9,c(6,42)]),
                         t(aou_educcont_diabob_mp_result[9,c(10,46)]),t(aou_educcont_diabob_mp_result[9,c(14,50)]),
                         t(aou_educcont_diabob_mp_result[9,c(18,54)]),t(aou_educcont_diabob_mp_result[9,c(22,58)]),
                         t(aou_educcont_diabob_mp_result[9,c(26,62)]),t(aou_educcont_diabob_mp_result[9,c(30,66)]),
                         t(aou_educcont_diabob_mp_result[9,c(34,68)])),
           upper = cbind(t(aou_educcont_diabob_mp_result[9,c(3,39)]),t(aou_educcont_diabob_mp_result[9,c(7,43)]),
                         t(aou_educcont_diabob_mp_result[9,c(11,47)]),t(aou_educcont_diabob_mp_result[9,c(15,51)]),
                         t(aou_educcont_diabob_mp_result[9,c(19,55)]),t(aou_educcont_diabob_mp_result[9,c(23,59)]),
                         t(aou_educcont_diabob_mp_result[9,c(27,63)]),t(aou_educcont_diabob_mp_result[9,c(31,67)]),
                         t(aou_educcont_diabob_mp_result[9,c(35,71)])),
           col = fpColors(box = c('gray23','gray57','dodgerblue4','darkslategray3','palegreen3','salmon1','indianred3','mediumpurple4','maroon')),
           legend=c("All","All, Race-adjusted","NHW","NHB","Hispanic","NHAsian","Multiracial","Other","None of these"),
           legend_args = fpLegend(pos = "right"),#list(x = 0.5, y = -0.2), gp = gpar(cex=0.9,col = "#CCCCCC", fill = "#F9F9F9")),
           xlab = "Estimated RR (95% CI)",
           title = 'Estimated RR for Continuous Education on T2D and Obesity Prevalence, \n Overall and Stratified by Race/Ethnicity \n (Modified Poisson Regression)',
           txt_gp = fpTxtGp(title=gpar(cex=1),xlab=gpar(cex=1), ticks=gpar(cex=1), label=gpar(cex=1)),
           vertices=TRUE,
           line.margin=0.25,
           graph.pos=2,
           clip=c(0.7,1.1), xlog=T, grid=structure(c(0.7,0.8,0.9,1.0,1.1)),
           xticks=c(-0.356674943938738,-0.223143551314213,-0.105360515657828,0,0.0953101798043265))#, xticks=c(0.7,0.8,0.9,1.0,1.1))#grid=structure(c(0.75,0.8,0.9,1.0,1.1)))
dev.off()


#tiff(file='C:/Users/sarac/OneDrive/Documents/Research/Area v Individual SES/Fig2A_aou_forest_indiv_inccont_stratandnonstrat.tiff', width=8, height=5, units='in', res=300)
#par(xpd=T)
png('SuppFig_aou_forest_indiv_inccont_stratandnonstrat_mp.png', width=8, height=5, units='in', res=300)
forestplot(c('Type 2 Diabetes','Obesity'), 
           mean = cbind(t(aou_incnum_diabob_mp_result[9,c(1,37)]),t(aou_incnum_diabob_mp_result[9,c(5,41)]),
                        t(aou_incnum_diabob_mp_result[9,c(9,45)]),t(aou_incnum_diabob_mp_result[9,c(13,49)]),
                        t(aou_incnum_diabob_mp_result[9,c(17,53)]),t(aou_incnum_diabob_mp_result[9,c(21,57)]),
                        t(aou_incnum_diabob_mp_result[9,c(25,61)]),t(aou_incnum_diabob_mp_result[9,c(29,65)]),
                        t(aou_incnum_diabob_mp_result[9,c(33,69)])),
           lower = cbind(t(aou_incnum_diabob_mp_result[9,c(2,38)]),t(aou_incnum_diabob_mp_result[9,c(6,42)]),
                         t(aou_incnum_diabob_mp_result[9,c(10,46)]),t(aou_incnum_diabob_mp_result[9,c(14,50)]),
                         t(aou_incnum_diabob_mp_result[9,c(18,54)]),t(aou_incnum_diabob_mp_result[9,c(22,58)]),
                         t(aou_incnum_diabob_mp_result[9,c(26,62)]),t(aou_incnum_diabob_mp_result[9,c(30,66)]),
                         t(aou_incnum_diabob_mp_result[9,c(34,68)])),
           upper = cbind(t(aou_incnum_diabob_mp_result[9,c(3,39)]),t(aou_incnum_diabob_mp_result[9,c(7,43)]),
                         t(aou_incnum_diabob_mp_result[9,c(11,47)]),t(aou_incnum_diabob_mp_result[9,c(15,51)]),
                         t(aou_incnum_diabob_mp_result[9,c(19,55)]),t(aou_incnum_diabob_mp_result[9,c(23,59)]),
                         t(aou_incnum_diabob_mp_result[9,c(27,63)]),t(aou_incnum_diabob_mp_result[9,c(31,67)]),
                         t(aou_incnum_diabob_mp_result[9,c(35,71)])),
           col = fpColors(box = c('gray23','gray57','dodgerblue4','darkslategray3','palegreen3','salmon1','indianred3','mediumpurple4','maroon')),
           legend=c("All","All, Race-adjusted","NHW","NHB","Hispanic","NHAsian","Multiracial","Other","None of these"),
           legend_args = fpLegend(pos = "right"),#list(x = 0.5, y = -0.2), gp = gpar(cex=0.9,col = "#CCCCCC", fill = "#F9F9F9")),
           xlab = "Estimated RR (95% CI)",
           title = 'Estimated RR for Continuous Income on T2D and Obesity Prevalence, \n Overall and Stratified by Race/Ethnicity \n (Modified Poisson Regression)',
           txt_gp = fpTxtGp(title=gpar(cex=1),xlab=gpar(cex=1), ticks=gpar(cex=1), label=gpar(cex=1)),
           vertices=TRUE,
           line.margin=0.25,
           graph.pos=2,
           clip=c(0.7,1.1), xlog=T,grid=structure(c(0.7,0.8,0.9,1.0,1.1)),
           xticks=c(-0.356674943938738,-0.223143551314213,-0.105360515657828,0,0.0953101798043265))#xlim=c(0.7,1.1),xticks=c(0.75,0.8,0.9,1.0,1.1), , xticks=c(0.75,0.8,0.9,1.0,1.1), xlog=TRUE, grid=structure(c(0.75,0.8,0.9,1.0,1.1)))
dev.off()




#####age-standardized prevalence######
popage <- c(43980000,41691000,42285000,30531000,20064000,16141000,9159000) #Distribution #10: chrome-extension://efaidnbmnnnibpcajpcglclefindmkaj/https://www.cdc.gov/nchs/data/statnt/statnt20.pdf
#aou_std <- svystandardize(surv_all, by=~agecat, over=~1, population=popage)#, excluding.missing=~diab) #standardizes across entire pop, not within racecats
aou$aou_Weights <- 1
aou$aou_Strata <- sample(c('1', '2','3','4','5'), size=nrow(aou), replace=TRUE)
aou$diab <- as.factor(aou$diab)
aou_surv <- svydesign(id= ~person_id,  strata = ~aou_Strata, weights = ~aou_Weights, data=aou[!is.na(aou$person_id),], nest=T)
dim(aou_surv)

100*prop.table(table(aou$racecat, aou$diab),1)
100*(svyby(~diab, ~racecat, svymean, design=aou_surv,na.rm=T))


aou_std_diab <- svystandardize(aou_surv, by = ~ agecat, over = ~racecat, 
                               population = popage,
                               excluding.missing = make.formula(c("age", "diab")))
agest_diab <- 100*svyby(~diab, ~racecat, svymean, design=aou_std_diab,na.rm=T)

barplot(100*(svyby(~diab, ~racecat, svymean, design=aou_std_diab, na.rm=T))$diab1, main='Age-Adjusted T2D by R/E', ylim=c(0,30),
        legend=T, legend.text=rownames(table(aou$racecat)), beside=T)


aou_std_ob <- svystandardize(aou_surv, by = ~ agecat, over = ~racecat, 
                             population = popage,
                             excluding.missing = make.formula(c("age", "obese")))
agest_obese <- 100*svyby(~obese, ~racecat, svymean, design=aou_std_ob,na.rm=T)

barplot(100*(svyby(~obese, ~racecat, svymean, design=aou_std_ob, na.rm=T))$obese, main='Age-Adjusted Obesity by R/E', ylim=c(0,80),
        legend=T, legend.text=rownames(table(aou$racecat)), beside=T)

racenames <- c('NHW','NHB','Hispanic','NHAsian','Multiracial','Other','None \nof these')

#educ-diab grouped bar
race_educ_diab_agestd <- as.data.frame(svyby(~diab, ~racecat+educ_simple, svymean, design=aou_std_diab))
race_educ_diab_agestd_prev <- rbind(t(race_educ_diab_agestd$diab1[22:28]),t(race_educ_diab_agestd$diab1[15:21]),
                                    t(race_educ_diab_agestd$diab1[8:14]),t(race_educ_diab_agestd$diab1[1:7]))
race_educ_diab_agestd_prev
colnames(race_educ_diab_agestd_prev) <- rownames(table(aou$racecat))
race_educ_diab_agestd_se <- rbind(t(race_educ_diab_agestd$se.diab1[22:28]),t(race_educ_diab_agestd$se.diab1[15:21]),
                                  t(race_educ_diab_agestd$se.diab1[8:14]),t(race_educ_diab_agestd$se.diab1[1:7]))
race_educ_diab_agestd_conf <- race_educ_diab_agestd_se*qnorm(0.975)
mid_educ<-barplot(100*race_educ_diab_agestd_prev, legend=rownames(table(aou$educ_simple)), beside=TRUE)
barplot(100*race_educ_diab_agestd_prev, main="Age-Adjusted Prevalence of Type 2 Diabetes by Race/Ethnicity \n and Educational Attainment (Categorical)", xaxt='n', xlab='', ylab='Percent with Type 2 Diabetes', 
        col=brewer.pal(4,name='YlGnBu'),#col=c('darkslategray3','darkseagreen','lightgoldenrod','lightsalmon'),#,'tomato'), 
        legend.text=c('No HS','Some HS','HS Degree','College Degree'), args.legend=list(x="top", cex=0.8,ncol=2, title='Educational Attainment'),cex.lab=0.8,cex.main=0.8, ylim=c(0,50),beside=TRUE)
text(cex=0.8, x=colMeans(mid_educ)-.25, y=-3.5, racenames, xpd=T, srt=60)
arrows(x0=mid_educ, y0=100*race_educ_diab_agestd_prev-100*(race_educ_diab_agestd_conf), x1=mid_educ, y1=100*race_educ_diab_agestd_prev+100*race_educ_diab_agestd_conf, code=3, angle=90, length=0.04)


#educ-obese grouped bar
race_educ_obese_agestd <- as.data.frame(svyby(~obese, ~racecat+educ_simple, svymean, design=aou_std_ob))
race_educ_obese_agestd_prev <- rbind(t(race_educ_obese_agestd$obese[22:28]),t(race_educ_obese_agestd$obese[15:21]),
                                     t(race_educ_obese_agestd$obese[8:14]),t(race_educ_obese_agestd$obese[1:7]))
race_educ_obese_agestd_prev
colnames(race_educ_obese_agestd_prev) <- rownames(table(aou$racecat))
race_educ_obese_agestd_se <- rbind(t(race_educ_obese_agestd$se[22:28]),t(race_educ_obese_agestd$se[15:21]),
                                   t(race_educ_obese_agestd$se[8:14]),t(race_educ_obese_agestd$se[1:7]))
race_educ_obese_agestd_conf <- race_educ_obese_agestd_se*qnorm(0.975)
mid_educ<-barplot(100*race_educ_obese_agestd_prev, legend=rownames(table(aou$educ_simple)), beside=TRUE)
barplot(100*race_educ_obese_agestd_prev, main="Age-Adjusted Prevalence of Obesity by Race/Ethnicity \n and Educational Attainment (Categorical)", xaxt='n', xlab='', ylab='Percent with Type 2 obeseetes', 
        col=brewer.pal(4,name='YlGnBu'),#col=c('darkslategray3','darkseagreen','lightgoldenrod','lightsalmon'),#,'tomato'), 
        legend.text=c('No HS','Some HS','HS Degree','College Degree'), args.legend=list(x="top", cex=0.8,ncol=2, title='Educational Attainment'),cex.lab=0.8,cex.main=0.8, ylim=c(0,80),beside=TRUE)
text(cex=0.8, x=colMeans(mid_educ)-.25, y=-5, racenames, xpd=T, srt=60)
arrows(x0=mid_educ, y0=100*race_educ_obese_agestd_prev-100*(race_educ_obese_agestd_conf), x1=mid_educ, y1=100*race_educ_obese_agestd_prev+100*race_educ_obese_agestd_conf, code=3, angle=90, length=0.04)

#inc-diab grouped bar
race_inc_diab_agestd <- as.data.frame(svyby(~diab, ~racecat+inc_simple, svymean, design=aou_std_diab))
race_inc_diab_agestd_prev <- rbind(t(race_inc_diab_agestd$diab1[29:35]),t(race_inc_diab_agestd$diab1[22:28]),t(race_inc_diab_agestd$diab1[15:21]),
                                   t(race_inc_diab_agestd$diab1[8:14]),t(race_inc_diab_agestd$diab1[1:7]))
race_inc_diab_agestd_prev
colnames(race_inc_diab_agestd_prev) <- rownames(table(aou$racecat))
race_inc_diab_agestd_se <- rbind(t(race_inc_diab_agestd$se.diab1[29:35]),t(race_inc_diab_agestd$se.diab1[22:28]),t(race_inc_diab_agestd$se.diab1[15:21]),
                                 t(race_inc_diab_agestd$se.diab1[8:14]),t(race_inc_diab_agestd$se.diab1[1:7]))
race_inc_diab_agestd_conf <- race_inc_diab_agestd_se*qnorm(0.975)
mid_inc<-barplot(100*race_inc_diab_agestd_prev, legend=rownames(table(aou$inc_simple)), beside=TRUE)
barplot(100*race_inc_diab_agestd_prev, main="Age-Adjusted Prevalence of Type 2 Diabetes by Race/Ethnicity \n and Income (Categorical)", xaxt='n',xlab='', ylab='Percent with Type 2 diabetes', 
        col=brewer.pal(5,name='YlGnBu'),#col=c('darkslategray3','darkseagreen','lightgoldenrod','lightsalmon'),#,'tomato'), 
        legend.text=c("Under $25,000","$25,000-49,999","$50,000-74,999","$75,000-149,999","More than $75,000"), args.legend=list(x="top", cex=0.8,ncol=2, title='Income Category'),cex.lab=0.8,cex.main=0.8, ylim=c(0,40),beside=TRUE)
text(cex=0.8, x=colMeans(mid_inc)-.25, y=-2.5, racenames, xpd=T, srt=60)
arrows(x0=mid_inc, y0=100*race_inc_diab_agestd_prev-100*(race_inc_diab_agestd_conf), x1=mid_inc, y1=100*race_inc_diab_agestd_prev+100*race_inc_diab_agestd_conf, code=3, angle=90, length=0.04)

#inc-obese grouped bar
table(aou$inc_simple)
race_inc_obese_agestd <- as.data.frame(svyby(~obese, ~racecat+inc_simple, svymean, design=aou_std_ob))
race_inc_obese_agestd_prev <- rbind(t(race_inc_obese_agestd$obese[29:35]),t(race_inc_obese_agestd$obese[22:28]),t(race_inc_obese_agestd$obese[15:21]),
                                    t(race_inc_obese_agestd$obese[8:14]),t(race_inc_obese_agestd$obese[1:7]))
race_inc_obese_agestd_prev
colnames(race_inc_obese_agestd_prev) <- rownames(table(aou$racecat))
race_inc_obese_agestd_se <- rbind(t(race_inc_obese_agestd$se[29:35]),t(race_inc_obese_agestd$se[22:28]),t(race_inc_obese_agestd$se[15:21]),
                                  t(race_inc_obese_agestd$se[8:14]),t(race_inc_obese_agestd$se[1:7]))
race_inc_obese_agestd_conf <- race_inc_obese_agestd_se*qnorm(0.975)
mid_inc<-barplot(100*race_inc_obese_agestd_prev, legend=rownames(table(aou$inc_simple)), beside=TRUE)
barplot(100*race_inc_obese_agestd_prev, main="Age-Adjusted Prevalence of Obesity by Race/Ethnicity \n and Income (Categorical)", xaxt='n', xlab='', ylab='Percent with Type 2 obeseetes', 
        col=brewer.pal(5,name='YlGnBu'),#col=c('darkslategray3','darkseagreen','lightgoldenrod','lightsalmon'),#,'tomato'), 
        legend.text=c("Under $25,000","$25,000-49,999","$50,000-74,999","$75,000-149,999","More than $75,000"), args.legend=list(x="top", cex=0.8,ncol=2, title='Income Category'),cex.lab=0.8,cex.main=0.8, ylim=c(0,80),beside=TRUE)
text(cex=0.8, x=colMeans(mid_inc)-.25, y=-2.5, racenames, xpd=T, srt=60)
arrows(x0=mid_inc, y0=100*race_inc_obese_agestd_prev-100*(race_inc_obese_agestd_conf), x1=mid_inc, y1=100*race_inc_obese_agestd_prev+100*race_inc_obese_agestd_conf, code=3, angle=90, length=0.04)

#####save grouped bar age-adjusted prevalence figure#####
png('Fig1_aou_bar_indiv_educinc_agestddiabob.png', width=10, height=12, units='in', res=300)
par(mfrow=c(2,2))
barplot(100*race_educ_diab_agestd_prev, main="Age-Standardized Prevalence of Type 2 Diabetes by Race/Ethnicity \n and Educational Attainment (Categorical)", xaxt='n', xlab='', ylab='Percent with Type 2 Diabetes', 
        col=brewer.pal(4,name='YlGnBu'),#col=c('darkslategray3','darkseagreen','lightgoldenrod','lightsalmon'),#,'tomato'), 
        legend.text=c('No HS','Some HS','HS Degree','College Degree'), args.legend=list(x="topright", cex=0.8,ncol=2, title='Educational Attainment'),cex.lab=0.8,cex.main=0.8, ylim=c(0,50),beside=TRUE)
text(cex=0.8, x=colMeans(mid_inc)-.25, y=-7.5, racenames, xpd=T, srt=60)
arrows(x0=mid_educ, y0=100*race_educ_diab_agestd_prev-100*(race_educ_diab_agestd_conf), x1=mid_educ, y1=100*race_educ_diab_agestd_prev+100*race_educ_diab_agestd_conf, code=3, angle=90, length=0.04)

barplot(100*race_educ_obese_agestd_prev, main="Age-Standardized Prevalence of Obesity by Race/Ethnicity \n and Educational Attainment (Categorical)", xaxt='n', xlab='', ylab='Percent with Obesity', 
        col=brewer.pal(4,name='YlGnBu'),#col=c('darkslategray3','darkseagreen','lightgoldenrod','lightsalmon'),#,'tomato'), 
        legend.text=c('No HS','Some HS','HS Degree','College Degree'), args.legend=list(x="topright", cex=0.8,ncol=2, title='Educational Attainment'),cex.lab=0.8,cex.main=0.8, ylim=c(0,80),beside=TRUE)
text(cex=0.8, x=colMeans(mid_inc)-.25, y=-10, racenames, xpd=T, srt=60)
arrows(x0=mid_educ, y0=100*race_educ_obese_agestd_prev-100*(race_educ_obese_agestd_conf), x1=mid_educ, y1=100*race_educ_obese_agestd_prev+100*race_educ_obese_agestd_conf, code=3, angle=90, length=0.04)

barplot(100*race_inc_diab_agestd_prev, main="Age-Standardized Prevalence of Type 2 Diabetes by Race/Ethnicity \n and Income (Categorical)", xaxt='n',xlab='', ylab='Percent with Type 2 diabetes', 
        col=brewer.pal(5,name='YlGnBu'),#col=c('darkslategray3','darkseagreen','lightgoldenrod','lightsalmon'),#,'tomato'), 
        legend.text=c("Under $25,000","$25,000-49,999","$50,000-74,999","$75,000-149,999","More than $75,000"), args.legend=list(x="topright", cex=0.8,ncol=2, title='Income Category'),cex.lab=0.8,cex.main=0.8, ylim=c(0,40),beside=TRUE)
text(cex=0.8, x=colMeans(mid_inc)-.25, y=-5, racenames, xpd=T, srt=60)
arrows(x0=mid_inc, y0=100*race_inc_diab_agestd_prev-100*(race_inc_diab_agestd_conf), x1=mid_inc, y1=100*race_inc_diab_agestd_prev+100*race_inc_diab_agestd_conf, code=3, angle=90, length=0.04)

barplot(100*race_inc_obese_agestd_prev, main="Age-Standardized Prevalence of Obesity by Race/Ethnicity \n and Income (Categorical)", xaxt='n', xlab='', ylab='Percent with Obesity', 
        col=brewer.pal(5,name='YlGnBu'),#col=c('darkslategray3','darkseagreen','lightgoldenrod','lightsalmon'),#,'tomato'), 
        legend.text=c("Under $25,000","$25,000-49,999","$50,000-74,999","$75,000-149,999","More than $75,000"), args.legend=list(x="topright", cex=0.8,ncol=2, title='Income Category'),cex.lab=0.8,cex.main=0.8, ylim=c(0,80),beside=TRUE)
text(cex=0.8, x=colMeans(mid_inc)-.25, y=-10, racenames, xpd=T, srt=60)
arrows(x0=mid_inc, y0=100*race_inc_obese_agestd_prev-100*(race_inc_obese_agestd_conf), x1=mid_inc, y1=100*race_inc_obese_agestd_prev+100*race_inc_obese_agestd_conf, code=3, angle=90, length=0.04)

dev.off()
par(mfrow=c(1,1))




#####sensitivity analysis using manning lab T2D case/control definitions#####
diabcontrol_filepath <- "gs://fc-secure-2967d1b8-5702-42f8-a5ac-daf5e65f158d/data/Diabetes_Controls_v7_workspace_KA_8Feb2024_by_ashok_on_2024-02-12.csv"
system(paste0("gsutil cp -r ", diabcontrol_filepath, " ."),intern=TRUE)
list.files(basename(diabcontrol_filepath))
diabcontrol <- read.csv(file='Diabetes_Controls_v7_workspace_KA_8Feb2024_by_ashok_on_2024-02-12.csv', header=TRUE, sep=',')
dim(diabcontrol)

diabcase_filepath <- "gs://fc-secure-2967d1b8-5702-42f8-a5ac-daf5e65f158d/data/T2D_in_v7_workspace_AKM_6Feb2024_by_ashok_on_2024-02-06.csv"
system(paste0("gsutil cp -r ", diabcase_filepath, " ."),intern=TRUE)
list.files(basename(diabcase_filepath))
diabcase <- read.csv(file='T2D_in_v7_workspace_AKM_6Feb2024_by_ashok_on_2024-02-06.csv', header=TRUE, sep=',')
dim(diabcase)











#####OLD CODE BELOW#####





######survey design objects#####
options(survey.lonely.psu = 'adjust')
table(aou$SDMVPSU, exclude=NULL)
aou <- aou[!is.na(aou$SDMVPSU),]
aou$aou_Weights <- ifelse(aou$survyr==1999 | aou$survyr==2001, aou$WTMEC4YR*0.2, aou$WTMEC2YR*0.1)
#aou$weights_quest <- 
#aou$weights_lab
summary(aou$SDMVPSU)
surv_all_withkids <- svydesign(id = ~SDMVPSU, strata = ~ SDMVSTRA, weights = ~aou_Weights, data=aou, nest=T)
#surv_all <- subset(surv_all_withkids, age>=18 & !aou$insur_stability=='Missing')
#surv_all <- svydesign(id = ~SDMVPSU, strata = ~ SDMVSTRA, weights = ~weights, data=aou[aou$age >= 18 & !aou$insur_stability=='Missing',], nest=T)
#diab_adj <- glm(data=aou, obese ~ age + female + racecat, family=quasibinomial())#family=stats::gaussian())
#aou$pred_prev_diab <- predict(diab_adj, newdata=aou, type='response')
#obese_adj <- glm(data=aou, obese ~ age + female + racecat, family=quasibinomial())#family=stats::gaussian())
#aou$pred_prev_obese <- predict(obese_adj, newdata=aou, type='response')
#summary(aou$pred_prev_obese)
#mean(aou$pred_prev_diab)
#svymean(~pred_prev_diab, surv_all)
dim(aou)
dim(surv_all_withkids)
surv_all <- subset(surv_all_withkids, age>=18 & !is.na(aou$female) & !is.na(aou$racecat) & !is.na(aou$age) & !aou$insur_stability=='Missing' & aou$all_ses_na==0 & aou$all_diab_var_na==0 & aou$all_ob_var_na==0 & !aou$smoke_status=='Missing')
dim(surv_all)
surv_nhw <- subset(surv_all, racecat=='NHW')
surv_nhb <- subset(surv_all, racecat=='NHB')
surv_nha <- subset(surv_all, racecat=='NHAsian')
surv_mex <- subset(surv_all, racecat=='Mexican-American')
surv_his <- subset(surv_all, racecat=='Other Hispanic')
surv_oth <- subset(surv_all, racecat=='Multiracial')
surv_oth11 <- subset(surv_all, racecat=='Other')
dim(surv_all)
###do not use below - should not subset the data prior to/as part of svydesign
#surv_nhw <- svydesign(id = ~SDMVPSU, strata = ~ SDMVSTRA, weights = ~weights, data=aou[aou$age >= 18 & aou$racecat=='NHW' & !aou$insur_stability=='Missing',], nest=T)
#surv_nhb <- svydesign(id = ~SDMVPSU, strata = ~ SDMVSTRA, weights = ~weights, data=aou[aou$age >= 18 & aou$racecat=='NHB' & !aou$insur_stability=='Missing',], nest=T)
#surv_nha <- svydesign(id = ~SDMVPSU, strata = ~ SDMVSTRA, weights = ~weights, data=aou[aou$age >= 18 & aou$racecat=='NHAsian' & !aou$insur_stability=='Missing',], nest=T)
#surv_mex <- svydesign(id = ~SDMVPSU, strata = ~ SDMVSTRA, weights = ~weights, data=aou[aou$age >= 18 & aou$racecat=='Mexican-American' & !aou$insur_stability=='Missing',], nest=T)
#surv_his <- svydesign(id = ~SDMVPSU, strata = ~ SDMVSTRA, weights = ~weights, data=aou[aou$age >= 18 & aou$racecat=='Other Hispanic' & !aou$insur_stability=='Missing',], nest=T)
#surv_oth <- svydesign(id = ~SDMVPSU, strata = ~ SDMVSTRA, weights = ~weights, data=aou[aou$age >= 18 & aou$racecat=='Multiracial' & !aou$insur_stability=='Missing',], nest=T)
#surv_oth11 <- svydesign(id = ~SDMVPSU, strata = ~ SDMVSTRA, weights = ~weights, data=aou[aou$age >= 18 & aou$racecat=='Other' & !aou$insur_stability=='Missing',], nest=T)



#####table 1#####
#unweighted - overall and age >18
aou2 <- aou[age>=18 & !is.na(aou$female) & !is.na(aou$age) & !aou$insur_stability=='Missing' & aou$all_ses_na==0,]
dim(aou2)

tab1_all_unweighted <- CreateTableOne(data=aou2, #strata='hba1c.F12_missing', 
                                      vars=c('age','age_ge_16','female','survyr','racecat','hreduc','hhinc','inc_num','working','hoursworked_cat','hoursworked_num','work_35hr','insur_stability'), test=T,
                                      factorVars=c('age_ge_16','female','survyr','racecat','hreduc','hhinc','working','hoursworked_cat','work_35hr','insur_stability'))
tab1_adults_unweighted <- CreateTableOne(data=aou2, #strata='hba1c.F12_missing', 
                                         vars=c('age','female','survyr','racecat','hreduc','educ_num','hhinc','inc_num','working','hoursworked_cat','hoursworked_num','work_35hr','insur_stability'), test=T,
                                         factorVars=c('female','survyr','racecat','hreduc','hhinc','working','hoursworked_cat','work_35hr','insur_stability'))
print(tab1_adults_unweighted, quote=T, noSpaces=T)
#weighted - overall, including kids
svymean(~age+inc_num+hoursworked_num, surv_all_withkids, na.rm=T)
svysd(~age+inc_num+hoursworked_num, surv_all_withkids, na.rm=T) #can also use sqrt(svyvar())
svytotal(~age_ge_16+female+as.factor(survyr)+racecat+hreduc+hhinc+working+hoursworked_cat+work_35hr, surv_all_withkids, na.rm=T)
svymean(~age_ge_16+female+as.factor(survyr)+racecat+hreduc+hhinc+working+hoursworked_cat+work_35hr, surv_all_withkids, na.rm=T)
tab1_all_weighted <- svyCreateTableOne(data=surv_all_withkids, #strata='hba1c.F12_missing', 
                                       vars=c('age','age_ge_16','female','survyr','racecat','hreduc','hhinc','inc_num','working','hoursworked_cat','hoursworked_num','work_35hr'), test=T,
                                       factorVars=c('age_ge_16','female','survyr','racecat','hreduc','hhinc','working','hoursworked_cat','work_35hr'))

#weighted - overall, study sample
svytotal(~sample, surv_all, na.rm=T)
svymean(~age+inc_num+hoursworked_num+educ_num, surv_all, na.rm=T)
svysd(~age+inc_num+hoursworked_num+educ_num, surv_all, na.rm=T) #can also use sqrt(svyvar())
svytotal(~age_ge_16+female+as.factor(survyr)+racecat+hreduc+hhinc+working+hoursworked_cat+work_35hr+insur_stability, surv_all, na.rm=T)
svymean(~age_ge_16+female+as.factor(survyr)+racecat+hreduc+hhinc+working+hoursworked_cat+work_35hr+insur_stability, surv_all, na.rm=T)
#does not work - "error in summary.factor...'sum' not meaningful for factors"; tried changing weights variable name (as this is a widely reported fix) without success
#tab1_adults_weighted <- svyCreateTableOne(data=surv_all, #strata='racecat', 
#vars=c('age','female','survyr','racecat','hreduc','educ_num','hhinc','inc_num','working'), test=T,
#factorVars=c('female','survyr','racecat','hreduc','hhinc','working'))
#print(tab1_adults_weighted, quote=T, noSpaces=T)


#tab1_adults_weighted_racestrat <- svyCreateTableOne(data=surv_all, strata='racecat', 
#vars=c('age','female','survyr','racecat','hreduc','educ_num','hhinc','inc_num','working'), test=T,
#factorVars=c('female','survyr','racecat','hreduc','hhinc','working'))
#print(tab1_adults_weighted_racestrat, quote=T, noSpaces=T)



#####table 2-3 and violins = distribution of SES variables by race ethnicity#####
table(aou$hreduc, aou$racecat)
aou_racexeduc <- as.data.frame(100*prop.table(table(aou$hreduc, aou$racecat),2))
tiff(file='C:/Users/sarac/OneDrive/Documents/Research/Area v Individual SES/aou_stackbar_indiveduc_racestrat.tiff', width=7, height=5, units='in', res=300)
ggplot(aou_racexeduc, aes(x=Var2, y=Freq, fill=Var1)) + 
  geom_bar(position='fill', stat='identity') + ggtitle('Distribution of Individual-Level Educational Attainment \n by Race/Ethnicity of Participant') + 
  xlab('') + ylab('Proportion') + labs(fill='Educational \n Attainment') + 
  theme(legend.position='bottom', legend.text=element_text(size=4), plot.title=element_text(hjust=0.5))
dev.off()


aou_racexinc <- as.data.frame(100*prop.table(table(aou$hhinc, aou$racecat),2))
tiff(file='C:/Users/sarac/OneDrive/Documents/Research/Area v Individual SES/aou_stackbar_indivinc_racestrat.tiff', width=7, height=5, units='in', res=300)
ggplot(aou_racexinc, aes(x=Var2, y=Freq, fill=Var1)) + 
  geom_bar(position='fill', stat='identity') + ggtitle('Distribution of Individual-Level Income \n by Race/Ethnicity of Participant') + 
  xlab('') + ylab('Proportion') + labs(fill='Income') + 
  theme(legend.position='bottom', legend.text=element_text(size=5), plot.title=element_text(hjust=0.5))
dev.off()

ggplot(aou, aes(factor(x=racecat), y=inc_num, color=racecat), exclude=TRUE) + geom_violin(trim=TRUE) + geom_boxplot(width=0.1)
ggplot(aou, aes(inc_num, fill=racecat)) + geom_density(alpha=0.2) + labs(x='Income (Multiples of $25k)', fill='Race') + scale_fill_manual(values=c('lightblue2','dodgerblue1','seagreen2','green4','goldenrod','salmon'))#,'firebrick3','mediumpurple'))

violin<-ggplot(aou, aes(factor(x=racecat), y=inc_num), exclude=TRUE) + geom_violin(trim=TRUE)
violin + geom_boxplot(width=0.1)

violin<-ggplot(aou, aes(factor(x=racecat), y=inc_num, fill=factor(diab)), exclude=TRUE) + geom_violin(position=position_dodge(1))
violin + geom_boxplot(position=position_dodge(1), width=0.1)

violin<-ggplot(aou[!is.na(aou$educ_simple),], aes(factor(x=racecat), y=inc_num, fill=factor(educ_simple)), exclude=TRUE) + geom_violin(position=position_dodge(1))
violin + geom_boxplot(position=position_dodge(1), width=0.1)

aou2 <- aou[aou$age >=18 & !is.na(aou$racecat) & !is.na(aou$educ_num) & !is.na(aou$diab),]
aou3 <- aou[aou$age >=18 & !is.na(aou$racecat) & !is.na(aou$inc_num) & !is.na(aou$diab),]
dim(aou2) #53k
aou2[1:40,c('racecat','inc_num','diab')]
table(aou2$racecat, aou2$diab, exclude=NULL)
summary(aou2$inc_num)
aou2$diab <- ifelse(aou2$diab==0, 'No Diabetes','Diabetes')
aou2$diab <- factor(aou2$diab, levels=c("Diabetes","No Diabetes"))
aou3$diab <- ifelse(aou3$diab==0, 'No Diabetes','Diabetes')
aou3$diab <- factor(aou3$diab, levels=c("Diabetes","No Diabetes"))

tiff(file='C:/Users/sarac/OneDrive/Documents/Research/Area v Individual SES/aou_ridgeline_indiveduc_racestrat.tiff', width=8, height=5, units='in', res=300)
aou2 %>%
  ggplot(aes(y = fct_rev(racecat))) +
  geom_density_ridges(
    aes(x = educ_num, fill = fct_rev(diab)), 
    alpha = .7
  ) +
  labs(
    x = "Educational Attainment",
    y = "",
    title = 'Distribution of Educational Attainment \n by Race/Ethnicity and Diabetes Status'
  ) +
  scale_y_discrete(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  theme_ridges(grid = FALSE, center=T) +
  scale_fill_manual(name='',values=c('#74add1','#d73027'))
dev.off()

tiff(file='C:/Users/sarac/OneDrive/Documents/Research/Area v Individual SES/aou_ridgeline_indivinc_racestrat.tiff', width=8, height=5, units='in', res=300)
aou3 %>%
  ggplot(aes(y = fct_rev(racecat))) +
  geom_density_ridges(
    aes(x = inc_num, fill = fct_rev(diab)), 
    alpha = .7
  ) +
  labs(
    x = "Income (Multiples of $25k)",
    y = "",
    title = 'Distribution of Income (Multiples of $25k) \n by Race/Ethnicity and Diabetes Status'
  ) +
  scale_y_discrete(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  theme_ridges(grid = FALSE, center=T) + 
  scale_fill_manual(name='',values=c('#74add1','#d73027'))
dev.off()



#####analysis#####
#educ ordinal
mod_diab_educ_nhw <- glm(data=aou[aou$racecat=='NHW',], diab ~ age + female + smoke_status + insur_stability + educ_simple, family=binomial(link=logit))
mod_diab_educ_nhb <- glm(data=aou[aou$racecat=='NHB',], diab ~ age + female + smoke_status + insur_stability + educ_simple, family=binomial(link=logit))
mod_diab_educ_nha <- glm(data=aou[aou$racecat=='NHAsian',], diab ~ age + female + smoke_status + insur_stability + educ_simple, family=binomial(link=logit))
mod_diab_educ_mex <- glm(data=aou[aou$racecat=='Hispanic',], diab ~ age + female + smoke_status + insur_stability + educ_simple, family=binomial(link=logit))
mod_diab_educ_his <- glm(data=aou[aou$racecat=='Multiracial',], diab ~ age + female + smoke_status + insur_stability + educ_simple, family=binomial(link=logit))
mod_diab_educ_oth <- glm(data=aou[aou$racecat=='Other',], diab ~ age + female + smoke_status + insur_stability + educ_simple, family=binomial(link=logit))
aou_educ_result <- data.frame(exp(mod_diab_educ_nhw$coefficients),exp(confint(mod_diab_educ_nhw)),coef(summary(mod_diab_educ_nhw))[,"Pr(>|t|)"],
                              exp(mod_diab_educ_nhb$coefficients),exp(confint(mod_diab_educ_nhb)),coef(summary(mod_diab_educ_nhb))[,"Pr(>|t|)"],
                              exp(mod_diab_educ_nha$coefficients),exp(confint(mod_diab_educ_nha)),coef(summary(mod_diab_educ_nha))[,"Pr(>|t|)"],
                              exp(mod_diab_educ_mex$coefficients),exp(confint(mod_diab_educ_mex)),coef(summary(mod_diab_educ_mex))[,"Pr(>|t|)"],
                              exp(mod_diab_educ_his$coefficients),exp(confint(mod_diab_educ_his)),coef(summary(mod_diab_educ_his))[,"Pr(>|t|)"],
                              exp(mod_diab_educ_oth$coefficients),exp(confint(mod_diab_educ_oth)),coef(summary(mod_diab_educ_oth))[,"Pr(>|t|)"],stringsAsFactors = FALSE)
write.csv(aou_educ_result, 'C:/Users/sarac/OneDrive/Documents/Research/Area v Individual SES/aou_mod_indiv_educord.csv', row.names=FALSE) 
#educ continuous
mod_diab_educcont_nhw <- glm(data=aou[aou$racecat=='NHW',], diab ~ age + female + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_diab_educcont_nhb <- glm(data=aou[aou$racecat=='NHB',], diab ~ age + female + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_diab_educcont_nha <- glm(data=aou[aou$racecat=='NHAsian',], diab ~ age + female + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_diab_educcont_mex <- glm(data=aou[aou$racecat=='Hispanic',], diab ~ age + female + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_diab_educcont_his <- glm(data=aou[aou$racecat=='Multiracial',], diab ~ age + female + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_diab_educcont_oth <- glm(data=aou[aou$racecat=='Other',], diab ~ age + female + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
aou_educcont_result <- data.frame(exp(mod_diab_educcont_nhw$coefficients),exp(confint(mod_diab_educcont_nhw)),coef(summary(mod_diab_educcont_nhw))[,"Pr(>|t|)"],
                                  exp(mod_diab_educcont_nhb$coefficients),exp(confint(mod_diab_educcont_nhb)),coef(summary(mod_diab_educcont_nhb))[,"Pr(>|t|)"],
                                  exp(mod_diab_educcont_nha$coefficients),exp(confint(mod_diab_educcont_nha)),coef(summary(mod_diab_educcont_nha))[,"Pr(>|t|)"],
                                  exp(mod_diab_educcont_mex$coefficients),exp(confint(mod_diab_educcont_mex)),coef(summary(mod_diab_educcont_mex))[,"Pr(>|t|)"],
                                  exp(mod_diab_educcont_his$coefficients),exp(confint(mod_diab_educcont_his)),coef(summary(mod_diab_educcont_his))[,"Pr(>|t|)"],
                                  exp(mod_diab_educcont_oth$coefficients),exp(confint(mod_diab_educcont_oth)),coef(summary(mod_diab_educcont_oth))[,"Pr(>|t|)"],stringsAsFactors = FALSE)
write.csv(aou_educcont_result, 'C:/Users/sarac/OneDrive/Documents/Research/Area v Individual SES/aou_mod_indiv_educcont.csv', row.names=FALSE)


#income ordinal
mod_diab_inc_nhw <- glm(data=aou[aou$racecat=='NHW',], diab ~ age + female + smoke_status + insur_stability + hhinc, family=binomial(link=logit))
mod_diab_inc_nhb <- glm(data=aou[aou$racecat=='NHB',], diab ~ age + female + smoke_status + insur_stability + hhinc, family=binomial(link=logit))
mod_diab_inc_nha <- glm(data=aou[aou$racecat=='NHAsian',], diab ~ age + female + smoke_status + insur_stability + hhinc, family=binomial(link=logit))
mod_diab_inc_mex <- glm(data=aou[aou$racecat=='Hispanic',], diab ~ age + female + smoke_status + insur_stability + hhinc, family=binomial(link=logit))
mod_diab_inc_his <- glm(data=aou[aou$racecat=='Multiracial',], diab ~ age + female + smoke_status + insur_stability + hhinc, family=binomial(link=logit))
mod_diab_inc_oth <- glm(data=aou[aou$racecat=='Other',], diab ~ age + female + smoke_status + insur_stability + hhinc, family=binomial(link=logit))
x<-9999
xx<-c(9999,9999)
aou_inc_result <- data.frame(exp(mod_diab_inc_nhw$coefficients),exp(confint(mod_diab_inc_nhw)),coef(summary(mod_diab_inc_nhw))[,"Pr(>|t|)"],
                             exp(mod_diab_inc_nhb$coefficients),exp(confint(mod_diab_inc_nhb)),coef(summary(mod_diab_inc_nhb))[,"Pr(>|t|)"],
                             append(exp(mod_diab_inc_nha$coefficients),x),rbind(exp(confint(mod_diab_inc_nha)),xx),append(coef(summary(mod_diab_inc_nha))[,"Pr(>|t|)"],x),
                             exp(mod_diab_inc_mex$coefficients),exp(confint(mod_diab_inc_mex)),coef(summary(mod_diab_inc_mex))[,"Pr(>|t|)"],
                             exp(mod_diab_inc_his$coefficients),exp(confint(mod_diab_inc_his)),coef(summary(mod_diab_inc_his))[,"Pr(>|t|)"],
                             exp(mod_diab_inc_oth$coefficients),exp(confint(mod_diab_inc_oth)),coef(summary(mod_diab_inc_oth))[,"Pr(>|t|)"],stringsAsFactors = FALSE)
write.csv(aou_inc_result, 'C:/Users/sarac/OneDrive/Documents/Research/Area v Individual SES/aou_mod_indiv_incord.csv', row.names=FALSE) 
#income ordinal simple
mod_diab_incsimp_nhw <- glm(data=aou[aou$racecat=='NHW',], diab ~ age + female + smoke_status + insur_stability + inc_simple, family=binomial(link=logit))
mod_diab_incsimp_nhb <- glm(data=aou[aou$racecat=='NHB',], diab ~ age + female + smoke_status + insur_stability + inc_simple, family=binomial(link=logit))
mod_diab_incsimp_nha <- glm(data=aou[aou$racecat=='NHAsian',], diab ~ age + female + smoke_status + insur_stability + inc_simple, family=binomial(link=logit))
mod_diab_incsimp_mex <- glm(data=aou[aou$racecat=='Hispanic',], diab ~ age + female + smoke_status + insur_stability + inc_simple, family=binomial(link=logit))
mod_diab_incsimp_his <- glm(data=aou[aou$racecat=='Multiracial',], diab ~ age + female + smoke_status + insur_stability + inc_simple, family=binomial(link=logit))
mod_diab_incsimp_oth <- glm(data=aou[aou$racecat=='Other',], diab ~ age + female + smoke_status + insur_stability + inc_simple, family=binomial(link=logit))
aou_incsimp_result <- data.frame(exp(mod_diab_incsimp_nhw$coefficients),exp(confint(mod_diab_incsimp_nhw)),coef(summary(mod_diab_incsimp_nhw))[,"Pr(>|t|)"],
                                 exp(mod_diab_incsimp_nhb$coefficients),exp(confint(mod_diab_incsimp_nhb)),coef(summary(mod_diab_incsimp_nhb))[,"Pr(>|t|)"],
                                 exp(mod_diab_incsimp_nha$coefficients),exp(confint(mod_diab_incsimp_nha)),coef(summary(mod_diab_incsimp_nha))[,"Pr(>|t|)"],
                                 exp(mod_diab_incsimp_mex$coefficients),exp(confint(mod_diab_incsimp_mex)),coef(summary(mod_diab_incsimp_mex))[,"Pr(>|t|)"],
                                 exp(mod_diab_incsimp_his$coefficients),exp(confint(mod_diab_incsimp_his)),coef(summary(mod_diab_incsimp_his))[,"Pr(>|t|)"],
                                 exp(mod_diab_incsimp_oth$coefficients),exp(confint(mod_diab_incsimp_oth)),coef(summary(mod_diab_incsimp_oth))[,"Pr(>|t|)"],stringsAsFactors = FALSE)
write.csv(aou_incsimp_result, 'C:/Users/sarac/OneDrive/Documents/Research/Area v Individual SES/aou_mod_indiv_incsimpord.csv', row.names=FALSE)
#inc-to-pov continuous
mod_diab_incnum_nhw <- glm(data=aou[aou$racecat=='NHW',], diab ~ age + female + smoke_status + insur_stability + inc_num, family=binomial(link=logit))
mod_diab_incnum_nhb <- glm(data=aou[aou$racecat=='NHB',], diab ~ age + female + smoke_status + insur_stability + inc_num, family=binomial(link=logit))
mod_diab_incnum_nha <- glm(data=aou[aou$racecat=='NHAsian',], diab ~ age + female + smoke_status + insur_stability + inc_num, family=binomial(link=logit))
mod_diab_incnum_mex <- glm(data=aou[aou$racecat=='Hispanic',], diab ~ age + female + smoke_status + insur_stability + inc_num, family=binomial(link=logit))
mod_diab_incnum_his <- glm(data=aou[aou$racecat=='Multiracial',], diab ~ age + female + smoke_status + insur_stability + inc_num, family=binomial(link=logit))
mod_diab_incnum_oth <- glm(data=aou[aou$racecat=='Other',], diab ~ age + female + smoke_status + insur_stability + inc_num, family=binomial(link=logit))
aou_incnum_result <- data.frame(exp(mod_diab_incnum_nhw$coefficients),exp(confint(mod_diab_incnum_nhw)),coef(summary(mod_diab_incnum_nhw))[,"Pr(>|t|)"],
                                exp(mod_diab_incnum_nhb$coefficients),exp(confint(mod_diab_incnum_nhb)),coef(summary(mod_diab_incnum_nhb))[,"Pr(>|t|)"],
                                exp(mod_diab_incnum_nha$coefficients),exp(confint(mod_diab_incnum_nha)),coef(summary(mod_diab_incnum_nha))[,"Pr(>|t|)"],
                                exp(mod_diab_incnum_mex$coefficients),exp(confint(mod_diab_incnum_mex)),coef(summary(mod_diab_incnum_mex))[,"Pr(>|t|)"],
                                exp(mod_diab_incnum_his$coefficients),exp(confint(mod_diab_incnum_his)),coef(summary(mod_diab_incnum_his))[,"Pr(>|t|)"],
                                exp(mod_diab_incnum_oth$coefficients),exp(confint(mod_diab_incnum_oth)),coef(summary(mod_diab_incnum_oth))[,"Pr(>|t|)"],stringsAsFactors = FALSE)
write.csv(aou_incnum_result, 'C:/Users/sarac/OneDrive/Documents/Research/Area v Individual SES/aou_mod_indiv_inctopov.csv', row.names=FALSE)


#employ binary
mod_diab_emp_nhw <- glm(data=aou[aou$racecat=='NHW',], diab ~ age + female + smoke_status + insur_stability + work_yn, family=binomial(link=logit))
mod_diab_emp_nhb <- glm(data=aou[aou$racecat=='NHB',], diab ~ age + female + smoke_status + insur_stability + work_yn, family=binomial(link=logit))
mod_diab_emp_nha <- glm(data=aou[aou$racecat=='NHAsian',], diab ~ age + female + smoke_status + insur_stability + work_yn, family=binomial(link=logit))
mod_diab_emp_mex <- glm(data=aou[aou$racecat=='Hispanic',], diab ~ age + female + smoke_status + insur_stability + work_yn, family=binomial(link=logit))
mod_diab_emp_his <- glm(data=aou[aou$racecat=='Multiracial',], diab ~ age + female + smoke_status + insur_stability + work_yn, family=binomial(link=logit))
mod_diab_emp_oth <- glm(data=aou[aou$racecat=='Other',], diab ~ age + female + smoke_status + insur_stability + work_yn, family=binomial(link=logit))
aou_emp_result <- data.frame(exp(mod_diab_emp_nhw$coefficients),exp(confint(mod_diab_emp_nhw)),coef(summary(mod_diab_emp_nhw))[,"Pr(>|t|)"],
                             exp(mod_diab_emp_nhb$coefficients),exp(confint(mod_diab_emp_nhb)),coef(summary(mod_diab_emp_nhb))[,"Pr(>|t|)"],
                             exp(mod_diab_emp_nha$coefficients),exp(confint(mod_diab_emp_nha)),coef(summary(mod_diab_emp_nha))[,"Pr(>|t|)"],
                             exp(mod_diab_emp_mex$coefficients),exp(confint(mod_diab_emp_mex)),coef(summary(mod_diab_emp_mex))[,"Pr(>|t|)"],
                             exp(mod_diab_emp_his$coefficients),exp(confint(mod_diab_emp_his)),coef(summary(mod_diab_emp_his))[,"Pr(>|t|)"],
                             exp(mod_diab_emp_oth$coefficients),exp(confint(mod_diab_emp_oth)),coef(summary(mod_diab_emp_oth))[,"Pr(>|t|)"],stringsAsFactors = FALSE)
write.csv(aou_emp_result, 'C:/Users/sarac/OneDrive/Documents/Research/Area v Individual SES/aou_mod_indiv_empbin.csv', row.names=FALSE) 

#####visuals#####
tiff(file='C:/Users/sarac/OneDrive/Documents/Research/Area v Individual SES/aou_forest_indiv_educ_racestrat.tiff', width=7, height=5, units='in', res=300)
forestplot(c("HS Degree","Some HS","No HS"), 
           mean = cbind(aou_educ_result[4:6,1],aou_educ_result[4:6,5],aou_educ_result[4:6,9],
                        aou_educ_result[4:6,13],aou_educ_result[4:6,17]),
           lower = cbind(aou_educ_result[4:6,2],aou_educ_result[4:6,6],aou_educ_result[4:6,10],
                         aou_educ_result[4:6,14],aou_educ_result[4:6,18]),
           upper = cbind(aou_educ_result[4:6,3],aou_educ_result[4:6,7],aou_educ_result[4:6,11],
                         aou_educ_result[4:6,15],aou_educ_result[4:6,19]),
           col = fpColors(box = c('darkslategray3','palegreen3','lightgoldenrod','salmon1','indianred3')),
           legend=c("All","All, Race-adjusted","NHW","NHB","Mexican-American","Other Hispanic","NHA","Other/Multi-Racial \n including NHA (pre-2011)","Other/Multi-Racial \n (post-2011)"),
           legend_args = fpLegend(pos = "right"),#list(x = 0.5, y = -0.2), gp = gpar(cex=0.9,col = "#CCCCCC", fill = "#F9F9F9")),
           #legend=c("NHW","NHB","NHA","Mexican-American","Other Hispanic"),
           #legend_args = fpLegend(pos = list(x = 0.9, y = 0.9), gp = gpar(cex=0.9,col = "#CCCCCC", fill = "#F9F9F9")),
           xlab = "OR (95% CI)",
           title = 'Adjusted OR for Educational Attainment (Ref=College Degree) \n on Diabetes, Stratified by Race/Ethnicity',
           txt_gp = fpTxtGp(title=gpar(cex=1),xlab=gpar(cex=1), ticks=gpar(cex=1), label=gpar(cex=1)),
           vertices=TRUE,
           line.margin=0.25,
           graph.pos=2,
           clip=c(0.5,4.0), xlim=c(0.5,4.0), xticks=c(0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0), xlog=TRUE, grid=structure(c(0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0)))
dev.off()

aou_educ_result_transpose <- t(aou_educ_result)
rownames(aou_educ_result_transpose)
tiff(file='C:/Users/sarac/OneDrive/Documents/Research/Area v Individual SES/aou_forest_indiv_educ_racestrat_transpose.tiff', width=7, height=5, units='in', res=300)
forestplot(c("NHW","NHB","NHA","Mexican-American","Other Hispanic"), 
           mean = cbind(aou_educ_result_transpose[c(1,5,9,13,17),4],aou_educ_result_transpose[c(1,5,9,13,17),5],aou_educ_result_transpose[c(1,5,9,13,17),6]),
           lower = cbind(aou_educ_result_transpose[c(2,6,10,14,18),4],aou_educ_result_transpose[c(2,6,10,14,18),5],aou_educ_result_transpose[c(2,6,10,14,18),6]),
           upper = cbind(aou_educ_result_transpose[c(3,7,11,15,19),4],aou_educ_result_transpose[c(3,7,11,15,19),5],aou_educ_result_transpose[c(3,7,11,15,19),6]),
           col = fpColors(box = c('darkslategray3','lightgoldenrod','indianred3')),
           legend=c("HS Degree","Some HS","No HS"),
           legend_args = fpLegend(pos = list(x = 0.9, y = 0.9), gp = gpar(cex=0.9,col = "#CCCCCC", fill = "#F9F9F9")),
           xlab = "OR (95% CI)",
           title = 'Adjusted OR for Educational Attainment (Ref=College Degree) \n on Diabetes, Stratified by Race/Ethnicity',
           txt_gp = fpTxtGp(title=gpar(cex=1),xlab=gpar(cex=1), ticks=gpar(cex=1), label=gpar(cex=1)),
           vertices=TRUE,
           line.margin=0.25,
           graph.pos=2,
           clip=c(0.5,4.0), xlim=c(0.5,4.0), xticks=c(0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0), xlog=TRUE, grid=structure(c(0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0)))
dev.off()

tiff(file='C:/Users/sarac/OneDrive/Documents/Research/Area v Individual SES/aou_forest_indiv_inc_racestrat.tiff', width=7, height=5, units='in', res=300)
forestplot(c("$45,000-74,999","$20,000-44,999","Under $20,000"), 
           mean = cbind(aou_incsimp_result[4:6,1],aou_incsimp_result[4:6,5],aou_incsimp_result[4:6,9],
                        aou_incsimp_result[4:6,13],aou_incsimp_result[4:6,17]),
           lower = cbind(aou_incsimp_result[4:6,2],aou_incsimp_result[4:6,6],aou_incsimp_result[4:6,10],
                         aou_incsimp_result[4:6,14],aou_incsimp_result[4:6,18]),
           upper = cbind(aou_incsimp_result[4:6,3],aou_incsimp_result[4:6,7],aou_incsimp_result[4:6,11],
                         aou_incsimp_result[4:6,15],aou_incsimp_result[4:6,19]),
           col = fpColors(box = c('darkslategray3','palegreen3','lightgoldenrod','salmon1','indianred3')),
           legend=c("NHW","NHB","NHA","Mexican-American","Other Hispanic"),
           legend_args = fpLegend(pos = list(x = 0.9, y = 0.9), gp = gpar(cex=0.9,col = "#CCCCCC", fill = "#F9F9F9")),
           xlab = "OR (95% CI)",
           title = 'Adjusted OR for Income Category (Ref= More than $75,000) \n on Diabetes, Stratified by Race/Ethnicity',
           txt_gp = fpTxtGp(title=gpar(cex=1),xlab=gpar(cex=1), ticks=gpar(cex=1), label=gpar(cex=1)),
           vertices=TRUE,
           line.margin=0.25,
           graph.pos=2,
           clip=c(0.5,2.5), xlim=c(0.5,2.5), xticks=c(0.5,1.0,1.5,2.0,2.5), xlog=TRUE, grid=structure(c(0.5,1.0,1.5,2.0,2.5)))
dev.off()



#analyses NOT stratified by race
mod_diab_educ_all <- glm(data=aou, diab ~ age + female + educ_simple, family=binomial(link=logit))
summary(mod_diab_educ_all)
mod_diab_inc_all <- glm(data=aou, diab ~ age + female + hhinc, family=binomial(link=logit))
summary(mod_diab_inc_nhw)
mod_diab_incsimp_all <- glm(data=aou, diab ~ age + female + inc_simple, family=binomial(link=logit))
summary(mod_diab_incsimp_nhw)
mod_diab_emp_all <- glm(data=aou, diab ~ age + female + work_yn, family=binomial(link=logit))
tiff(file='C:/Users/sarac/OneDrive/Documents/Research/Area v Individual SES/aou_forest_indiv_educ_nonstrat.tiff', width=7, height=5, units='in', res=300)
forestplot(c("HS Degree","Some HS","No HS"),
           mean = exp(mod_diab_educ_all$coefficients[4:6]), 
           lower= exp(confint(mod_diab_educ_all))[4:6,1], 
           upper= exp(confint(mod_diab_educ_all))[4:6,2],
           xlab = "OR (95% CI)",
           title = 'Adjusted OR for Educational Attainment (Ref= College Degree) \n on Diabetes, Stratified by Race/Ethnicity',
           txt_gp = fpTxtGp(title=gpar(cex=1),xlab=gpar(cex=1), ticks=gpar(cex=1), label=gpar(cex=1)),
           vertices=TRUE,
           line.margin=0.25,
           graph.pos=2,
           clip=c(0.5,2.5), xlim=c(0.5,2.5), xticks=c(0.5,1.0,1.5,2.0,2.5), xlog=TRUE, grid=structure(c(0.5,1.0,1.5,2.0,2.5)))
dev.off()

tiff(file='C:/Users/sarac/OneDrive/Documents/Research/Area v Individual SES/aou_forest_indiv_inc_nonstrat.tiff', width=7, height=5, units='in', res=300)
forestplot(c("$45,000-74,999","$20,000-44,999","Under $20,000"),
           mean = exp(mod_diab_incsimp_all$coefficients[4:6]), 
           lower= exp(confint(mod_diab_incsimp_all))[4:6,1], 
           upper= exp(confint(mod_diab_incsimp_all))[4:6,2],
           xlab = "OR (95% CI)",
           title = 'Adjusted OR for Income Category (Ref= More than $75,000) \n on Diabetes, Stratified by Race/Ethnicity',
           txt_gp = fpTxtGp(title=gpar(cex=1),xlab=gpar(cex=1), ticks=gpar(cex=1), label=gpar(cex=1)),
           vertices=TRUE,
           line.margin=0.25,
           graph.pos=2,
           clip=c(0.5,2.5), xlim=c(0.5,2.5), xticks=c(0.5,1.0,1.5,2.0,2.5), xlog=TRUE, grid=structure(c(0.5,1.0,1.5,2.0,2.5)))
dev.off()



#####age-standardized prevalence######
popage <- c(43980000,41691000,42285000,30531000,20064000,16141000,9159000) #Distribution #10: chrome-extension://efaidnbmnnnibpcajpcglclefindmkaj/https://www.cdc.gov/nchs/data/statnt/statnt20.pdf
#aou_std <- svystandardize(surv_all, by=~agecat, over=~1, population=popage)#, excluding.missing=~diab) #standardizes across entire pop, not within racecats

100*prop.table(table(aou$racecat, aou$diab),1)
100*svyby(~diab, ~racecat, svymean, design=surv_all)

aou_std <- svystandardize(surv_all, by = ~ agecat, over = ~racecat, 
                          population = popage,
                          excluding.missing = make.formula(c("age", "diab")))
100*svyby(~diab, ~racecat, svymean, design=aou_std)

barplot(100*(svyby(~diab, ~racecat, svymean, design=aou_std))$diab, main='Age-Adjusted T2D by R/E', ylim=c(0,20),
        legend=T, legend.text=rownames(table(aou$racecat)), args.legend=list(x="bottom"))

#not working
aou_std <- aou %>% group_by(racecat) %>%
  svystandardize(surv_all, by=~agecat, over=~1, population=popage)
prop.table(table(aou_std$racecat, aou_std$diab))

#not working
getPrevalence <- function(over) {
  group_vars <- syms(over)
  svystandardize(surv_all, by = ~ agecat, over = make.formula(over), 
                 population = popage,
                 excluding.missing = make.formula(c(over, "age", "diab"))) %>% 
    #filter(RIDAGEYR >= 20) %>%
    group_by(!!!group_vars) %>%
    summarize(n = unweighted(n()),
              pct = survey_mean(diab == 1, na.rm=T) %>% 
                mutate_at("pct", function(x) round(100 * x, 1)) %>% 
                mutate_at("pct_se", function(x) round(100 * x, 3)))
}

getPrevalence(c("racecat","female"))


#educ-diab grouped bar
race_educ_diab_agestd <- as.data.frame(svyby(~diab, ~racecat+educ_simple, svymean, design=aou_std))
race_educ_diab_agestd_prev <- rbind(t(race_educ_diab_agestd$diab[22:28]),t(race_educ_diab_agestd$diab[15:21]),
                                    t(race_educ_diab_agestd$diab[8:14]),t(race_educ_diab_agestd$diab[1:7]))
race_educ_diab_agestd_prev
colnames(race_educ_diab_agestd_prev) <- rownames(table(aou$racecat))
race_educ_diab_agestd_se <- rbind(t(race_educ_diab_agestd$se[22:28]),t(race_educ_diab_agestd$se[15:21]),
                                  t(race_educ_diab_agestd$se[8:14]),t(race_educ_diab_agestd$se[1:7]))
race_educ_diab_agestd_conf <- race_educ_diab_agestd_se*qnorm(0.975)
mid_educ<-barplot(100*race_educ_diab_agestd_prev, legend=rownames(table(aou$educ_simple)), beside=TRUE)
barplot(100*race_educ_diab_agestd_prev, main="Age-Adjusted Prevalence of Type 2 Diabetes by Race/Ethnicity \n and Educational Attainment (Categorical)", xaxt='n', xlab='', ylab='Percent with Type 2 Diabetes', 
        col=brewer.pal(4,name='YlGnBu'),#col=c('darkslategray3','darkseagreen','lightgoldenrod','lightsalmon'),#,'tomato'), 
        legend.text=c('No HS','Some HS','HS Degree','College Degree'), args.legend=list(x="top", cex=0.8,ncol=2, title='Educational Attainment'),cex.lab=0.8,cex.main=0.8, ylim=c(0,40),beside=TRUE)
text(cex=0.8, x=colMeans(mid_inc)-.25, y=-2.5, racenames, xpd=T, srt=60)
arrows(x0=mid_educ, y0=100*race_educ_diab_agestd_prev-100*(race_educ_diab_agestd_conf), x1=mid_educ, y1=100*race_educ_diab_agestd_prev+100*race_educ_diab_agestd_conf, code=3, angle=90, length=0.04)

#educ-obese grouped bar
race_educ_obese_agestd <- as.data.frame(svyby(~obese, ~racecat+educ_simple, svymean, design=aou_std))
race_educ_obese_agestd_prev <- rbind(t(race_educ_obese_agestd$obese[22:28]),t(race_educ_obese_agestd$obese[15:21]),
                                     t(race_educ_obese_agestd$obese[8:14]),t(race_educ_obese_agestd$obese[1:7]))
race_educ_obese_agestd_prev
colnames(race_educ_obese_agestd_prev) <- rownames(table(aou$racecat))
race_educ_obese_agestd_se <- rbind(t(race_educ_obese_agestd$se[22:28]),t(race_educ_obese_agestd$se[15:21]),
                                   t(race_educ_obese_agestd$se[8:14]),t(race_educ_obese_agestd$se[1:7]))
race_educ_obese_agestd_conf <- race_educ_obese_agestd_se*qnorm(0.975)
mid_educ<-barplot(100*race_educ_obese_agestd_prev, legend=rownames(table(aou$educ_simple)), beside=TRUE)
barplot(100*race_educ_obese_agestd_prev, main="Age-Adjusted Prevalence of Obesity by Race/Ethnicity \n and Educational Attainment (Categorical)", xaxt='n', xlab='', ylab='Percent with Type 2 obeseetes', 
        col=brewer.pal(4,name='YlGnBu'),#col=c('darkslategray3','darkseagreen','lightgoldenrod','lightsalmon'),#,'tomato'), 
        legend.text=c('No HS','Some HS','HS Degree','College Degree'), args.legend=list(x="top", cex=0.8,ncol=2, title='Educational Attainment'),cex.lab=0.8,cex.main=0.8, ylim=c(0,80),beside=TRUE)
text(cex=0.8, x=colMeans(mid_inc)-.25, y=-2.5, racenames, xpd=T, srt=60)
arrows(x0=mid_educ, y0=100*race_educ_obese_agestd_prev-100*(race_educ_obese_agestd_conf), x1=mid_educ, y1=100*race_educ_obese_agestd_prev+100*race_educ_obese_agestd_conf, code=3, angle=90, length=0.04)


#inc-diab grouped bar
race_inc_diab_agestd <- as.data.frame(svyby(~diab, ~racecat+inc_simple, svymean, design=aou_std))
race_inc_diab_agestd_prev <- rbind(t(race_inc_diab_agestd$diab[22:28]),t(race_inc_diab_agestd$diab[15:21]),
                                   t(race_inc_diab_agestd$diab[8:14]),t(race_inc_diab_agestd$diab[1:7]))
race_inc_diab_agestd_prev
colnames(race_inc_diab_agestd_prev) <- rownames(table(aou$racecat))
race_inc_diab_agestd_se <- rbind(t(race_inc_diab_agestd$se[22:28]),t(race_inc_diab_agestd$se[15:21]),
                                 t(race_inc_diab_agestd$se[8:14]),t(race_inc_diab_agestd$se[1:7]))
race_inc_diab_agestd_conf <- race_inc_diab_agestd_se*qnorm(0.975)
mid_inc<-barplot(100*race_inc_diab_agestd_prev, legend=rownames(table(aou$inc_simple)), beside=TRUE)
barplot(100*race_inc_diab_agestd_prev, main="Age-Adjusted Prevalence of Type 2 Diabetes by Race/Ethnicity \n and Income (Categorical)", xaxt='n',xlab='', ylab='Percent with Type 2 diabetes', 
        col=brewer.pal(4,name='YlGnBu'),#col=c('darkslategray3','darkseagreen','lightgoldenrod','lightsalmon'),#,'tomato'), 
        legend.text=c("Under $20,000","$20,000-44,999","$45,000-74,999","More than $75,000"), args.legend=list(x="top", cex=0.8,ncol=2, title='Income Category'),cex.lab=0.8,cex.main=0.8, ylim=c(0,40),beside=TRUE)
text(cex=0.8, x=colMeans(mid_inc)-.25, y=-2.5, racenames, xpd=T, srt=60)
arrows(x0=mid_inc, y0=100*race_inc_diab_agestd_prev-100*(race_inc_diab_agestd_conf), x1=mid_inc, y1=100*race_inc_diab_agestd_prev+100*race_inc_diab_agestd_conf, code=3, angle=90, length=0.04)

#inc-obese grouped bar
race_inc_obese_agestd <- as.data.frame(svyby(~obese, ~racecat+inc_simple, svymean, design=aou_std))
race_inc_obese_agestd_prev <- rbind(t(race_inc_obese_agestd$obese[22:28]),t(race_inc_obese_agestd$obese[15:21]),
                                    t(race_inc_obese_agestd$obese[8:14]),t(race_inc_obese_agestd$obese[1:7]))
race_inc_obese_agestd_prev
colnames(race_inc_obese_agestd_prev) <- rownames(table(aou$racecat))
race_inc_obese_agestd_se <- rbind(t(race_inc_obese_agestd$se[22:28]),t(race_inc_obese_agestd$se[15:21]),
                                  t(race_inc_obese_agestd$se[8:14]),t(race_inc_obese_agestd$se[1:7]))
race_inc_obese_agestd_conf <- race_inc_obese_agestd_se*qnorm(0.975)
mid_inc<-barplot(100*race_inc_obese_agestd_prev, legend=rownames(table(aou$inc_simple)), beside=TRUE)
barplot(100*race_inc_obese_agestd_prev, main="Age-Adjusted Prevalence of Obesity by Race/Ethnicity \n and Income (Categorical)", xaxt='n', xlab='', ylab='Percent with Type 2 obeseetes', 
        col=brewer.pal(4,name='YlGnBu'),#col=c('darkslategray3','darkseagreen','lightgoldenrod','lightsalmon'),#,'tomato'), 
        legend.text=c("Under $20,000","$20,000-44,999","$45,000-74,999","More than $75,000"), args.legend=list(x="top", cex=0.8,ncol=2, title='Income Category'),cex.lab=0.8,cex.main=0.8, ylim=c(0,80),beside=TRUE)
text(cex=0.8, x=colMeans(mid_inc)-.25, y=-2.5, racenames, xpd=T, srt=60)
arrows(x0=mid_inc, y0=100*race_inc_obese_agestd_prev-100*(race_inc_obese_agestd_conf), x1=mid_inc, y1=100*race_inc_obese_agestd_prev+100*race_inc_obese_agestd_conf, code=3, angle=90, length=0.04)

#####save grouped bar age-adjusted prevalence figure#####
tiff(file='C:/Users/sarac/OneDrive/Documents/Research/Area v Individual SES/Fig1_aou_bar_indiv_educinc_agestddiabob.tiff', width=10, height=12, units='in', res=100)
racenames <- c('NHW','NHB','Mexican-\nAmerican','Other \nHispanic','NHA','Other or \nMulti-Racial \n(pre-2011)','Other or \nMulti-Racial \n(post-2011)')
par(mfrow=c(2,2))
barplot(100*race_educ_diab_agestd_prev, main="Age-Adjusted Prevalence of Type 2 Diabetes by Race/Ethnicity \n and Educational Attainment (Categorical)", xaxt='n', xlab='', ylab='Percent with Type 2 Diabetes', 
        col=brewer.pal(4,name='YlGnBu'),#col=c('darkslategray3','darkseagreen','lightgoldenrod','lightsalmon'),#,'tomato'), 
        legend.text=c('No HS','Some HS','HS Degree','College Degree'), args.legend=list(x="top", cex=0.8,ncol=2, title='Educational Attainment'),cex.lab=0.8,cex.main=0.8, ylim=c(0,40),beside=TRUE)
text(cex=0.8, x=colMeans(mid_inc)-.25, y=-2.5, racenames, xpd=T, srt=60)
arrows(x0=mid_educ, y0=100*race_educ_diab_agestd_prev-100*(race_educ_diab_agestd_conf), x1=mid_educ, y1=100*race_educ_diab_agestd_prev+100*race_educ_diab_agestd_conf, code=3, angle=90, length=0.04)

barplot(100*race_educ_obese_agestd_prev, main="Age-Adjusted Prevalence of Obesity by Race/Ethnicity \n and Educational Attainment (Categorical)", xaxt='n', xlab='', ylab='Percent with Type 2 obeseetes', 
        col=brewer.pal(4,name='YlGnBu'),#col=c('darkslategray3','darkseagreen','lightgoldenrod','lightsalmon'),#,'tomato'), 
        legend.text=c('No HS','Some HS','HS Degree','College Degree'), args.legend=list(x="top", cex=0.8,ncol=2, title='Educational Attainment'),cex.lab=0.8,cex.main=0.8, ylim=c(0,80),beside=TRUE)
text(cex=0.8, x=colMeans(mid_inc)-.25, y=-5, racenames, xpd=T, srt=60)
arrows(x0=mid_educ, y0=100*race_educ_obese_agestd_prev-100*(race_educ_obese_agestd_conf), x1=mid_educ, y1=100*race_educ_obese_agestd_prev+100*race_educ_obese_agestd_conf, code=3, angle=90, length=0.04)

barplot(100*race_inc_diab_agestd_prev, main="Age-Adjusted Prevalence of Type 2 Diabetes by Race/Ethnicity \n and Income (Categorical)", xaxt='n',xlab='', ylab='Percent with Type 2 diabetes', 
        col=brewer.pal(4,name='YlGnBu'),#col=c('darkslategray3','darkseagreen','lightgoldenrod','lightsalmon'),#,'tomato'), 
        legend.text=c("Under $20,000","$20,000-44,999","$45,000-74,999","More than $75,000"), args.legend=list(x="top", cex=0.8,ncol=2, title='Income Category'),cex.lab=0.8,cex.main=0.8, ylim=c(0,40),beside=TRUE)
text(cex=0.8, x=colMeans(mid_inc)-.25, y=-2.5, racenames, xpd=T, srt=60)
arrows(x0=mid_inc, y0=100*race_inc_diab_agestd_prev-100*(race_inc_diab_agestd_conf), x1=mid_inc, y1=100*race_inc_diab_agestd_prev+100*race_inc_diab_agestd_conf, code=3, angle=90, length=0.04)

barplot(100*race_inc_obese_agestd_prev, main="Age-Adjusted Prevalence of Obesity by Race/Ethnicity \n and Income (Categorical)", xaxt='n', xlab='', ylab='Percent with Type 2 obeseetes', 
        col=brewer.pal(4,name='YlGnBu'),#col=c('darkslategray3','darkseagreen','lightgoldenrod','lightsalmon'),#,'tomato'), 
        legend.text=c("Under $20,000","$20,000-44,999","$45,000-74,999","More than $75,000"), args.legend=list(x="top", cex=0.8,ncol=2, title='Income Category'),cex.lab=0.8,cex.main=0.8, ylim=c(0,80),beside=TRUE)
text(cex=0.8, x=colMeans(mid_inc)-.25, y=-5, racenames, xpd=T, srt=60)
arrows(x0=mid_inc, y0=100*race_inc_obese_agestd_prev-100*(race_inc_obese_agestd_conf), x1=mid_inc, y1=100*race_inc_obese_agestd_prev+100*race_inc_obese_agestd_conf, code=3, angle=90, length=0.04)

dev.off()
par(mfrow=c(1,1))






#####grouped bar - educ, diab#####
educ_unknown <- c('Do not know','Missing','Refused')
dm2_counts_educ <- table(aou$hreduc[aou$diab==1 & !aou$hreduc %in% educ_unknown], aou$racecat[aou$diab==1 & !aou$hreduc %in% educ_unknown])
dm2_counts_educ <- dm2_counts_educ[1:7,]
#dm2_counts_educ <- svymean(~diab==1, surv_all)

raceeduc_counts_dm <- table(aou$hreduc[!aou$hreduc %in% educ_unknown], aou$racecat[!aou$hreduc %in% educ_unknown]) #whichever is second will be on the x axis groupings, first will be series 1/2/3 colors
raceeduc_counts_dm <- raceeduc_counts_dm[1:7,]
mid_educ<-barplot(100*dm2_counts_educ/raceeduc_counts_dm, legend=rownames(raceeduc_counts_dm), beside=TRUE)
raceeduc_confint_dm <- aou[!aou$hreduc %in% educ_unknown,] %>%
  group_by(racecat, hreduc) %>%
  summarise( 
    n=n(),
    mean=mean(diab, na.rm=TRUE),
    sd=sd(diab, na.rm=TRUE)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( conf95=se * qt((1-0.05)/2 + .5, n-1))
raceeduc_confint_dm <- raceeduc_confint_dm[complete.cases(raceeduc_confint_dm),]
barplot(100*dm2_counts_educ/raceeduc_counts_dm, main="Prevalence of Type 2 Diabetes by Race/Ethnicity \n and Educational Attainment (Categorical)", xlab='', ylab='Percent with Type 2 Diabetes', col=brewer.pal(7,name='Spectral'),#col=c('darkslategray3','darkseagreen','lightgoldenrod','lightsalmon'),#,'tomato'), 
        legend.text=c(rownames(dm2_counts_educ)), args.legend=list(x="top", cex=0.8,ncol=2, title='Educational Attainment'),cex.lab=1.5,cex.main=1.5,ylim=c(0,50),beside=TRUE)
arrows(x0=mid_educ, y0=100*(dm2_counts_educ/raceeduc_counts_dm)-100*(raceeduc_confint_dm$conf95), x1=mid_educ, y1=100*(dm2_counts_educ/raceeduc_counts_dm)+100*(raceeduc_confint_dm$conf95), code=3, angle=90, length=0.1)



#####grouped bar - educ, diab#####
prop.table(table(aou$racecat, aou$obese, exclude=NULL),1)
#educome categorical
dm2_counts_educcat <- table(aou$educ_simple[aou$diab==1], aou$racecat[aou$diab==1])
raceeduccat_counts_dm <- table(aou$educ_simple, aou$racecat) #whichever is second will be on the x axis groupings, first will be series 1/2/3 colors
mid_educcat<-barplot(100*dm2_counts_educcat/raceeduccat_counts_dm, legend=rownames(raceeduccat_counts_dm), beside=TRUE)
raceeduccat_confint_dm <- aou %>%
  group_by(racecat, educ_simple) %>%
  summarise( 
    n=n(),
    mean=mean(diab, na.rm=TRUE),
    sd=sd(diab, na.rm=TRUE)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( conf95=se * qt((1-0.05)/2 + .5, n-1))
raceeduccat_confint_dm <- raceeduccat_confint_dm[complete.cases(raceeduccat_confint_dm),]
barplot(100*dm2_counts_educcat/raceeduccat_counts_dm, main="Prevalence of Type 2 Diabetes by Race/Ethnicity \n and Educational Attainment (Categorical)", xlab='', ylab='Percent with Type 2 Diabetes', col=c('darkslategray3','darkseagreen','lightgoldenrod','lightsalmon'),#'tomato'), 
        legend.text=c(rownames(dm2_counts_educcat)), args.legend=list(x="topleft",cex=1,ncol=1, title='Educational Attainment'),cex.lab=1.5,cex.main=1.5,ylim=c(0,40),beside=TRUE)
arrows(x0=mid_educcat, y0=100*(dm2_counts_educcat/raceeduccat_counts_dm)-100*(raceeduccat_confint_dm$conf95), x1=mid_educcat, y1=100*(dm2_counts_educcat/raceeduccat_counts_dm)+100*(raceeduccat_confint_dm$conf95), code=3, angle=90, length=0.1)


#ob_counts_educcat <- table(aou$educ_simple[aou$obese==1], aou$racecat[aou$obese==1])
#raceeduccat_counts_ob <- table(aou$educ_simple, aou$racecat) #whichever is second will be on the x axis groupings, first will be series 1/2/3 colors
#100*ob_counts_educcat/raceeduccat_counts_ob

#educ_num quintiles
aou$educ_num_quint <- ntile((aou$educ_num), 5)
summary(aou$educ_num[aou$educ_num_quint==3])
table(aou$racecat, aou$educ_num_quint, exclude=NULL)
violin<-ggplot(aou[!is.na(aou$educ_simple),], aes(factor(x=educ_num_quint), y=educ_num, fill=factor(racecat)), exclude=TRUE) + geom_violin(position=position_dodge(1))
violin + geom_boxplot(position=position_dodge(1), width=0.1)

barplot(100*prop.table(table(aou$educ_num_quint, aou$racecat),2))#/table(aou$racecat))

dm2_counts_educquint <- table(aou$educ_num_quint[aou$diab==1], aou$racecat[aou$diab==1])
raceeducquint_counts_dm <- table(aou$educ_num_quint, aou$racecat) #whichever is second will be on the x axis groupings, first will be series 1/2/3 colors
mid_educquint<-barplot(100*dm2_counts_educquint/raceeducquint_counts_dm, legend=rownames(raceeducquint_counts_dm), beside=TRUE)
raceeducquint_confint_dm <- aou %>%
  group_by(racecat, educ_num_quint) %>%
  summarise( 
    n=n(),
    mean=mean(diab, na.rm=TRUE),
    sd=sd(diab, na.rm=TRUE)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( conf95=se * qt((1-0.05)/2 + .5, n-1))
raceeducquint_confint_dm <- raceeducquint_confint_dm[complete.cases(raceeducquint_confint_dm),]
barplot(100*dm2_counts_educquint/raceeducquint_counts_dm, main="Prevalence of Type 2 Diabetes by Race/Ethnicity \n and Educational Attainment Quintile", xlab='', ylab='Percent with Type 2 Diabetes', col=c('darkslategray3','darkseagreen','lightgoldenrod','lightsalmon','tomato'), 
        legend.text=c('Quintile 1 (highest risk)','Quintile 2','Quintile 3','Quintile 4','Quintile 5 (lowest risk)'), args.legend=list(x="topleft",cex=1,ncol=1, title='Educational Attainment Quintile'),cex.lab=1.5,cex.main=1.5,ylim=c(0,40),beside=TRUE)
arrows(x0=mid_educquint, y0=100*(dm2_counts_educquint/raceeducquint_counts_dm)-100*(raceeducquint_confint_dm$conf95), x1=mid_educquint, y1=100*(dm2_counts_educquint/raceeducquint_counts_dm)+100*(raceeducquint_confint_dm$conf95), code=3, angle=90, length=0.1)


#educome quintiles WITHIN r/e group
aou <- aou %>% group_by(racecat) %>% mutate(educ_num_quintbyrace = ntile(educ_num, 5))
table(aou$racecat, aou$educ_num_quintbyrace, exclude=NULL)
violin<-ggplot(aou[!is.na(aou$educ_simple),], aes(factor(x=educ_num_quintbyrace), y=educ_num, fill=factor(racecat)), exclude=TRUE) + geom_violin(position=position_dodge(1))
violin + geom_boxplot(position=position_dodge(1), width=0.1)
violin<-ggplot(aou[!is.na(aou$educ_simple),], aes(factor(x=racecat), y=educ_num, fill=factor(educ_num_quintbyrace)), exclude=TRUE) + geom_violin(position=position_dodge(1))
violin + geom_boxplot(position=position_dodge(1), width=0.1)

dm2_counts_educquintbyrace <- table(aou$educ_num_quintbyrace[aou$diab==1], aou$racecat[aou$diab==1])
raceeducquintbyrace_counts_dm <- table(aou$educ_num_quintbyrace, aou$racecat) #whichever is second will be on the x axis groupings, first will be series 1/2/3 colors
mid_educquintbyrace<-barplot(100*dm2_counts_educquintbyrace/raceeducquintbyrace_counts_dm, legend=rownames(raceeducquintbyrace_counts_dm), beside=TRUE)
raceeducquintbyrace_confint_dm <- aou %>%
  group_by(racecat, educ_num_quintbyrace) %>%
  summarise( 
    n=n(),
    mean=mean(diab, na.rm=TRUE),
    sd=sd(diab, na.rm=TRUE)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( conf95=se * qt((1-0.05)/2 + .5, n-1))
raceeducquintbyrace_confint_dm <- raceeducquintbyrace_confint_dm[complete.cases(raceeducquintbyrace_confint_dm),]
barplot(100*dm2_counts_educquintbyrace/raceeducquintbyrace_counts_dm, main="Prevalence of Type 2 Diabetes by Race/Ethnicity \n and Race-Specific Educational Attainment Quintile", xlab='', ylab='Percent with Type 2 Diabetes', col=c('darkslategray3','darkseagreen','lightgoldenrod','lightsalmon','tomato'), 
        legend.text=c('Quintile 1 (highest risk)','Quintile 2','Quintile 3','Quintile 4','Quintile 5 (lowest risk)'), args.legend=list(x="topleft",cex=1,ncol=1, title='Educational Attainment Quintile'),cex.lab=1.5,cex.main=1.5,ylim=c(0,40),beside=TRUE)
arrows(x0=mid_educquintbyrace, y0=100*(dm2_counts_educquintbyrace/raceeducquintbyrace_counts_dm)-100*(raceeducquintbyrace_confint_dm$conf95), x1=mid_educquintbyrace, y1=100*(dm2_counts_educquintbyrace/raceeducquintbyrace_counts_dm)+100*(raceeducquintbyrace_confint_dm$conf95), code=3, angle=90, length=0.1)


#####grouped bar - income, diab#####
#income categorical
dm2_counts_inccat <- table(aou$inc_simple[aou$diab==1], aou$racecat[aou$diab==1])
raceinccat_counts_dm <- table(aou$inc_simple, aou$racecat) #whichever is second will be on the x axis groupings, first will be series 1/2/3 colors
mid_inccat<-barplot(100*dm2_counts_inccat/raceinccat_counts_dm, legend=rownames(raceinccat_counts_dm), beside=TRUE)
raceinccat_confint_dm <- aou %>%
  group_by(racecat, inc_simple) %>%
  summarise( 
    n=n(),
    mean=mean(diab, na.rm=TRUE),
    sd=sd(diab, na.rm=TRUE)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( conf95=se * qt((1-0.05)/2 + .5, n-1))
raceinccat_confint_dm <- raceinccat_confint_dm[complete.cases(raceinccat_confint_dm),]
barplot(100*dm2_counts_inccat/raceinccat_counts_dm, main="Prevalence of Type 2 Diabetes by Race/Ethnicity \n and Income (Categorical)", xlab='', ylab='Percent with Type 2 Diabetes', col=c('darkslategray3','darkseagreen','lightgoldenrod','lightsalmon'),#'tomato'), 
        legend.text=c(rownames(dm2_counts_inccat)), args.legend=list(x="topleft",cex=1,ncol=1, title='Income Category'),cex.lab=1.5,cex.main=1.5,ylim=c(0,40),beside=TRUE)
arrows(x0=mid_inccat, y0=100*(dm2_counts_inccat/raceinccat_counts_dm)-100*(raceinccat_confint_dm$conf95), x1=mid_inccat, y1=100*(dm2_counts_inccat/raceinccat_counts_dm)+100*(raceinccat_confint_dm$conf95), code=3, angle=90, length=0.1)

#income to poverty quintiles
aou$inc_num_quint <- ntile((aou$inc_num), 5)
table(aou$racecat, aou$inc_num_quint, exclude=NULL)
dm2_counts_incquint <- table(aou$inc_num_quint[aou$diab==1], aou$racecat[aou$diab==1])
raceincquint_counts_dm <- table(aou$inc_num_quint, aou$racecat) #whichever is second will be on the x axis groupings, first will be series 1/2/3 colors
mid_incquint<-barplot(100*dm2_counts_incquint/raceincquint_counts_dm, legend=rownames(raceincquint_counts_dm), beside=TRUE)
raceincquint_confint_dm <- aou %>%
  group_by(racecat, inc_num_quint) %>%
  summarise( 
    n=n(),
    mean=mean(diab, na.rm=TRUE),
    sd=sd(diab, na.rm=TRUE)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( conf95=se * qt((1-0.05)/2 + .5, n-1))
raceincquint_confint_dm <- raceincquint_confint_dm[complete.cases(raceincquint_confint_dm),]
barplot(100*dm2_counts_incquint/raceincquint_counts_dm, main="Prevalence of Type 2 Diabetes by Race/Ethnicity \n and Income Quintile", xlab='', ylab='Percent with Type 2 Diabetes', col=c('darkslategray3','darkseagreen','lightgoldenrod','lightsalmon','tomato'), 
        legend.text=c('Quintile 1 (highest risk)','Quintile 2','Quintile 3','Quintile 4','Quintile 5 (lowest risk)'), args.legend=list(x="topleft",cex=1,ncol=1, title='Income Quintile'),cex.lab=1.5,cex.main=1.5,ylim=c(0,40),beside=TRUE)
arrows(x0=mid_incquint, y0=100*(dm2_counts_incquint/raceincquint_counts_dm)-100*(raceincquint_confint_dm$conf95), x1=mid_incquint, y1=100*(dm2_counts_incquint/raceincquint_counts_dm)+100*(raceincquint_confint_dm$conf95), code=3, angle=90, length=0.1)

#income quintiles WITHIN r/e group
aou <- aou %>% group_by(racecat) %>% mutate(inc_num_quintbyrace = ntile(inc_num, 5))
table(aou$racecat, aou$inc_num_quintbyrace, exclude=NULL)

dm2_counts_incquintbyrace <- table(aou$inc_num_quintbyrace[aou$diab==1], aou$racecat[aou$diab==1])
raceincquintbyrace_counts_dm <- table(aou$inc_num_quintbyrace, aou$racecat) #whichever is second will be on the x axis groupings, first will be series 1/2/3 colors
mid_incquintbyrace<-barplot(100*dm2_counts_incquintbyrace/raceincquintbyrace_counts_dm, legend=rownames(raceincquintbyrace_counts_dm), beside=TRUE)
raceincquintbyrace_confint_dm <- aou %>%
  group_by(racecat, inc_num_quintbyrace) %>%
  summarise( 
    n=n(),
    mean=mean(diab, na.rm=TRUE),
    sd=sd(diab, na.rm=TRUE)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( conf95=se * qt((1-0.05)/2 + .5, n-1))
raceincquintbyrace_confint_dm <- raceincquintbyrace_confint_dm[complete.cases(raceincquintbyrace_confint_dm),]
barplot(100*dm2_counts_incquintbyrace/raceincquintbyrace_counts_dm, main="Prevalence of Type 2 Diabetes by Race/Ethnicity \n and Race-Specific Income Quintile", xlab='', ylab='Percent with Type 2 Diabetes', col=c('darkslategray3','darkseagreen','lightgoldenrod','lightsalmon','tomato'), 
        legend.text=c('Quintile 1 (highest risk)','Quintile 2','Quintile 3','Quintile 4','Quintile 5 (lowest risk)'), args.legend=list(x="topleft",cex=1,ncol=1, title='Income Quintile'),cex.lab=1.5,cex.main=1.5,ylim=c(0,40),beside=TRUE)
arrows(x0=mid_incquintbyrace, y0=100*(dm2_counts_incquintbyrace/raceincquintbyrace_counts_dm)-100*(raceincquintbyrace_confint_dm$conf95), x1=mid_incquintbyrace, y1=100*(dm2_counts_incquintbyrace/raceincquintbyrace_counts_dm)+100*(raceincquintbyrace_confint_dm$conf95), code=3, angle=90, length=0.1)




#####grouped bar - educ, obese#####
prop.table(table(aou$racecat, aou$obese, exclude=NULL),1)
#educ categorical
ob_counts_educcat <- table(aou$educ_simple[aou$obese==1], aou$racecat[aou$obese==1])
raceeduccat_counts_dm <- table(aou$educ_simple, aou$racecat) #whichever is second will be on the x axis groupings, first will be series 1/2/3 colors
mid_educcat<-barplot(100*ob_counts_educcat/raceeduccat_counts_dm, legend=rownames(raceeduccat_counts_dm), beside=TRUE)
raceeduccat_confint_dm <- aou %>%
  group_by(racecat, educ_simple) %>%
  summarise( 
    n=n(),
    mean=mean(obese, na.rm=TRUE),
    sd=sd(obese, na.rm=TRUE)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( conf95=se * qt((1-0.05)/2 + .5, n-1))
raceeduccat_confint_dm <- raceeduccat_confint_dm[complete.cases(raceeduccat_confint_dm),]
barplot(100*ob_counts_educcat/raceeduccat_counts_dm, main="Prevalence of Obesity by Race/Ethnicity \n and Educational Attainment (Categorical)", xlab='', ylab='Percent with Obesity', col=c('darkslategray3','darkseagreen','lightgoldenrod','lightsalmon'),#'tomato'), 
        legend.text=c(rownames(ob_counts_educcat)), args.legend=list(x="topleft",cex=1,ncol=1, title='Educational Attainment'),cex.lab=1.5,cex.main=1.5,ylim=c(0,80),beside=TRUE)
arrows(x0=mid_educcat, y0=100*(ob_counts_educcat/raceeduccat_counts_dm)-100*(raceeduccat_confint_dm$conf95), x1=mid_educcat, y1=100*(ob_counts_educcat/raceeduccat_counts_dm)+100*(raceeduccat_confint_dm$conf95), code=3, angle=90, length=0.1)


#educ_num quintiles
aou$educ_num_quint <- ntile((aou$educ_num), 5)
summary(aou$educ_num[aou$educ_num_quint==3])
table(aou$racecat, aou$educ_num_quint, exclude=NULL)
violin<-ggplot(aou[!is.na(aou$educ_simple),], aes(factor(x=educ_num_quint), y=educ_num, fill=factor(racecat)), exclude=TRUE) + geom_violin(position=position_dodge(1))
violin + geom_boxplot(position=position_dodge(1), width=0.1)

barplot(100*prop.table(table(aou$educ_num_quint, aou$racecat),2))#/table(aou$racecat))

ob_counts_educquint <- table(aou$educ_num_quint[aou$obese==1], aou$racecat[aou$obese==1])
raceeducquint_counts_dm <- table(aou$educ_num_quint, aou$racecat) #whichever is second will be on the x axis groupings, first will be series 1/2/3 colors
mid_educquint<-barplot(100*ob_counts_educquint/raceeducquint_counts_dm, legend=rownames(raceeducquint_counts_dm), beside=TRUE)
raceeducquint_confint_dm <- aou %>%
  group_by(racecat, educ_num_quint) %>%
  summarise( 
    n=n(),
    mean=mean(obese, na.rm=TRUE),
    sd=sd(obese, na.rm=TRUE)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( conf95=se * qt((1-0.05)/2 + .5, n-1))
raceeducquint_confint_dm <- raceeducquint_confint_dm[complete.cases(raceeducquint_confint_dm),]
barplot(100*ob_counts_educquint/raceeducquint_counts_dm, main="Prevalence of Obesity by Race/Ethnicity \n and Educational Attainment Quintile", xlab='', ylab='Percent with Obesity', col=c('darkslategray3','darkseagreen','lightgoldenrod','lightsalmon','tomato'), 
        legend.text=c('Quintile 1 (highest risk)','Quintile 2','Quintile 3','Quintile 4','Quintile 5 (lowest risk)'), args.legend=list(x="topleft",cex=1,ncol=1, title='Educational Attainment Quintile'),cex.lab=1.5,cex.main=1.5,ylim=c(0,40),beside=TRUE)
arrows(x0=mid_educquint, y0=100*(ob_counts_educquint/raceeducquint_counts_dm)-100*(raceeducquint_confint_dm$conf95), x1=mid_educquint, y1=100*(ob_counts_educquint/raceeducquint_counts_dm)+100*(raceeducquint_confint_dm$conf95), code=3, angle=90, length=0.1)


#educome quintiles WITHIN r/e group
aou <- aou %>% group_by(racecat) %>% mutate(educ_num_quintbyrace = ntile(educ_num, 5))
table(aou$racecat, aou$educ_num_quintbyrace, exclude=NULL)
violin<-ggplot(aou[!is.na(aou$educ_simple),], aes(factor(x=educ_num_quintbyrace), y=educ_num, fill=factor(racecat)), exclude=TRUE) + geom_violin(position=position_dodge(1))
violin + geom_boxplot(position=position_dodge(1), width=0.1)
violin<-ggplot(aou[!is.na(aou$educ_simple),], aes(factor(x=racecat), y=educ_num, fill=factor(educ_num_quintbyrace)), exclude=TRUE) + geom_violin(position=position_dodge(1))
violin + geom_boxplot(position=position_dodge(1), width=0.1)

ob_counts_educquintbyrace <- table(aou$educ_num_quintbyrace[aou$obese==1], aou$racecat[aou$obese==1])
raceeducquintbyrace_counts_dm <- table(aou$educ_num_quintbyrace, aou$racecat) #whichever is second will be on the x axis groupings, first will be series 1/2/3 colors
mid_educquintbyrace<-barplot(100*ob_counts_educquintbyrace/raceeducquintbyrace_counts_dm, legend=rownames(raceeducquintbyrace_counts_dm), beside=TRUE)
raceeducquintbyrace_confint_dm <- aou %>%
  group_by(racecat, educ_num_quintbyrace) %>%
  summarise( 
    n=n(),
    mean=mean(obese, na.rm=TRUE),
    sd=sd(obese, na.rm=TRUE)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( conf95=se * qt((1-0.05)/2 + .5, n-1))
raceeducquintbyrace_confint_dm <- raceeducquintbyrace_confint_dm[complete.cases(raceeducquintbyrace_confint_dm),]
barplot(100*ob_counts_educquintbyrace/raceeducquintbyrace_counts_dm, main="Prevalence of Obesity by Race/Ethnicity \n and Race-Specific Educational Attainment Quintile", xlab='', ylab='Percent with Obesity', col=c('darkslategray3','darkseagreen','lightgoldenrod','lightsalmon','tomato'), 
        legend.text=c('Quintile 1 (highest risk)','Quintile 2','Quintile 3','Quintile 4','Quintile 5 (lowest risk)'), args.legend=list(x="topleft",cex=1,ncol=1, title='Educational Attainment Quintile'),cex.lab=1.5,cex.main=1.5,ylim=c(0,40),beside=TRUE)
arrows(x0=mid_educquintbyrace, y0=100*(ob_counts_educquintbyrace/raceeducquintbyrace_counts_dm)-100*(raceeducquintbyrace_confint_dm$conf95), x1=mid_educquintbyrace, y1=100*(ob_counts_educquintbyrace/raceeducquintbyrace_counts_dm)+100*(raceeducquintbyrace_confint_dm$conf95), code=3, angle=90, length=0.1)



#####grouped bar - income, obese#####
#income categorical
ob_counts_inccat <- table(aou$inc_simple[aou$obese==1], aou$racecat[aou$obese==1])
raceinccat_counts_dm <- table(aou$inc_simple, aou$racecat) #whichever is second will be on the x axis groupings, first will be series 1/2/3 colors
mid_inccat<-barplot(100*ob_counts_inccat/raceinccat_counts_dm, legend=rownames(raceinccat_counts_dm), beside=TRUE)
raceinccat_confint_dm <- aou %>%
  group_by(racecat, inc_simple) %>%
  summarise( 
    n=n(),
    mean=mean(obese, na.rm=TRUE),
    sd=sd(obese, na.rm=TRUE)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( conf95=se * qt((1-0.05)/2 + .5, n-1))
raceinccat_confint_dm <- raceinccat_confint_dm[complete.cases(raceinccat_confint_dm),]
barplot(100*ob_counts_inccat/raceinccat_counts_dm, main="Prevalence of Obesity by Race/Ethnicity \n and Income (Categorical)", xlab='', ylab='Percent with Obesity', col=c('darkslategray3','darkseagreen','lightgoldenrod','lightsalmon'),#'tomato'), 
        legend.text=c(rownames(ob_counts_inccat)), args.legend=list(x="topleft",cex=1,ncol=1, title='Income Category'),cex.lab=1.5,cex.main=1.5,ylim=c(0,80),beside=TRUE)
arrows(x0=mid_inccat, y0=100*(ob_counts_inccat/raceinccat_counts_dm)-100*(raceinccat_confint_dm$conf95), x1=mid_inccat, y1=100*(ob_counts_inccat/raceinccat_counts_dm)+100*(raceinccat_confint_dm$conf95), code=3, angle=90, length=0.1)

#income to poverty quintiles
aou$inc_num_quint <- ntile((aou$inc_num), 5)
table(aou$racecat, aou$inc_num_quint, exclude=NULL)
ob_counts_incquint <- table(aou$inc_num_quint[aou$obese==1], aou$racecat[aou$obese==1])
raceincquint_counts_dm <- table(aou$inc_num_quint, aou$racecat) #whichever is second will be on the x axis groupings, first will be series 1/2/3 colors
mid_incquint<-barplot(100*ob_counts_incquint/raceincquint_counts_dm, legend=rownames(raceincquint_counts_dm), beside=TRUE)
raceincquint_confint_dm <- aou %>%
  group_by(racecat, inc_num_quint) %>%
  summarise( 
    n=n(),
    mean=mean(obese, na.rm=TRUE),
    sd=sd(obese, na.rm=TRUE)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( conf95=se * qt((1-0.05)/2 + .5, n-1))
raceincquint_confint_dm <- raceincquint_confint_dm[complete.cases(raceincquint_confint_dm),]
barplot(100*ob_counts_incquint/raceincquint_counts_dm, main="Prevalence of Obesity by Race/Ethnicity \n and Income Quintile", xlab='', ylab='Percent with Obesity', col=c('darkslategray3','darkseagreen','lightgoldenrod','lightsalmon','tomato'), 
        legend.text=c('Quintile 1 (highest risk)','Quintile 2','Quintile 3','Quintile 4','Quintile 5 (lowest risk)'), args.legend=list(x="topleft",cex=1,ncol=1, title='Income Quintile'),cex.lab=1.5,cex.main=1.5,ylim=c(0,40),beside=TRUE)
arrows(x0=mid_incquint, y0=100*(ob_counts_incquint/raceincquint_counts_dm)-100*(raceincquint_confint_dm$conf95), x1=mid_incquint, y1=100*(ob_counts_incquint/raceincquint_counts_dm)+100*(raceincquint_confint_dm$conf95), code=3, angle=90, length=0.1)

#income quintiles WITHIN r/e group
aou <- aou %>% group_by(racecat) %>% mutate(inc_num_quintbyrace = ntile(inc_num, 5))
table(aou$racecat, aou$inc_num_quintbyrace, exclude=NULL)

ob_counts_incquintbyrace <- table(aou$inc_num_quintbyrace[aou$obese==1], aou$racecat[aou$obese==1])
raceincquintbyrace_counts_dm <- table(aou$inc_num_quintbyrace, aou$racecat) #whichever is second will be on the x axis groupings, first will be series 1/2/3 colors
mid_incquintbyrace<-barplot(100*ob_counts_incquintbyrace/raceincquintbyrace_counts_dm, legend=rownames(raceincquintbyrace_counts_dm), beside=TRUE)
raceincquintbyrace_confint_dm <- aou %>%
  group_by(racecat, inc_num_quintbyrace) %>%
  summarise( 
    n=n(),
    mean=mean(obese, na.rm=TRUE),
    sd=sd(obese, na.rm=TRUE)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( conf95=se * qt((1-0.05)/2 + .5, n-1))
raceincquintbyrace_confint_dm <- raceincquintbyrace_confint_dm[complete.cases(raceincquintbyrace_confint_dm),]
barplot(100*ob_counts_incquintbyrace/raceincquintbyrace_counts_dm, main="Prevalence of Obesity by Race/Ethnicity \n and Race-Specific Income Quintile", xlab='', ylab='Percent with Obesity', col=c('darkslategray3','darkseagreen','lightgoldenrod','lightsalmon','tomato'), 
        legend.text=c('Quintile 1 (highest risk)','Quintile 2','Quintile 3','Quintile 4','Quintile 5 (lowest risk)'), args.legend=list(x="topleft",cex=1,ncol=1, title='Income Quintile'),cex.lab=1.5,cex.main=1.5,ylim=c(0,40),beside=TRUE)
arrows(x0=mid_incquintbyrace, y0=100*(ob_counts_incquintbyrace/raceincquintbyrace_counts_dm)-100*(raceincquintbyrace_confint_dm$conf95), x1=mid_incquintbyrace, y1=100*(ob_counts_incquintbyrace/raceincquintbyrace_counts_dm)+100*(raceincquintbyrace_confint_dm$conf95), code=3, angle=90, length=0.1)




####grouped bar plots - PREDICTED PREVALENCE####

#estimated prevalence - diab
?survey::predict.svyglm
diab_adj <- glm(data=aou, diab ~ age + female + racecat, family=quasibinomial())#family=stats::gaussian())
aou$pred_prev_diab <- predict(diab_adj, newdata=aou, type='response')
summary(aou$pred_prev_diab)
mean(aou$pred_prev_diab[aou$racecat=='NHAsian'])
ggplot(data = subset(aou, racecat=='NHW'), 
       aes(x = pred_prev_diab, fill = factor(racecat)))+
  geom_histogram(aes(y = ..density..))+
  geom_histogram(data = subset(aou, racecat=='NHB'), 
                 aes(x = pred_prev_diab, y = -..density.., fill=factor(racecat)))+
  xlab("Predicted T2D Prevalence")+ylab("Density")+
  ggtitle("Histogram of Predicted T2D Prevalence by NHW v NHB") + 
  scale_fill_discrete("Race: NHW v NHB")
#estimated prevalence barplot - educ diab
dm2_counts_educcat <- table(aou$educ_simple[aou$diab==1], aou$racecat[aou$diab==1])
raceeduccat_counts_dm <- table(aou$educ_simple, aou$racecat) #whichever is second will be on the x axis groupings, first will be series 1/2/3 colors
mid_educcat<-barplot(100*dm2_counts_educcat/raceeduccat_counts_dm, legend=rownames(raceeduccat_counts_dm), beside=TRUE)

raceeduccat_mean_dm <- aou %>% 
  group_by(racecat, educ_simple) %>%
  summarise(
    mean=100*mean(pred_prev_diab, na.rm=T)  )
raceeduccat_mean_dm <- raceeduccat_mean_dm[complete.cases(raceeduccat_mean_dm),]
raceeduccat_confint_dm <- aou %>%
  group_by(racecat, educ_simple) %>%
  summarise( 
    n=n(),
    mean=mean(pred_prev_diab, na.rm=TRUE),
    sd=sd(pred_prev_diab, na.rm=TRUE)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( conf95=100*se * qt((1-0.05)/2 + .5, n-1))
raceeduccat_confint_dm <- raceeduccat_confint_dm[complete.cases(raceeduccat_confint_dm),]

raceeduccat_mean_dm <- as.data.frame(raceeduccat_mean_dm[complete.cases(raceeduccat_mean_dm),])
raceeduccat_mean_dm <- reshape(raceeduccat_mean_dm, idvar='educ_simple', timevar='racecat', direction='wide')
raceeduccat_mean_dm <- as.matrix(raceeduccat_mean_dm, ncol=7)
as.numeric(raceeduccat_mean_dm)
raceeduccat_mean_dm <- matrix(as.numeric(raceeduccat_mean_dm), ncol=8)
colnames(raceeduccat_mean_dm) <- c('',colnames(raceeduccat_counts_dm))
raceeduccat_mean_dm <- raceeduccat_mean_dm[,2:8]
dim(raceeduccat_mean_dm)
class(raceeduccat_mean_dm)

bar_prevdiab_educ <- barplot(raceeduccat_mean_dm, main="Age, Sex, and Year-Adjusted Prevalence of Type 2 Diabetes \n by Race/Ethnicity and Educational Attainment (Categorical)", xlab='', ylab='Percent with Type 2 Diabetes', col=c('darkslategray3','darkseagreen','lightgoldenrod','lightsalmon'),#'tomato'), 
                             legend.text=c(rownames(dm2_counts_educcat)), args.legend=list(x="right",inset=c(-0.6,0),cex=0.8,ncol=1, title='Educational Attainment'),cex.lab=1.5,cex.main=1.5,ylim=c(0,35),beside=TRUE)
bar_prevdiab_educ + arrows(x0=mid_educcat, y0=raceeduccat_mean_dm - raceeduccat_confint_dm$conf95, x1=mid_educcat, y1=raceeduccat_mean_dm + raceeduccat_confint_dm$conf95, code=3, angle=90, length=0.1)

#estimated prevalence barplot - inc diab
dm2_counts_inccat <- table(aou$inc_simple[aou$diab==1], aou$racecat[aou$diab==1])
raceinccat_counts_dm <- table(aou$inc_simple, aou$racecat) #whichever is second will be on the x axis groupings, first will be series 1/2/3 colors
mid_inccat<-barplot(100*dm2_counts_inccat/raceinccat_counts_dm, legend=rownames(raceinccat_counts_dm), beside=TRUE)

raceinccat_mean_dm <- aou %>% 
  group_by(racecat, inc_simple) %>%
  summarise(
    mean=100*mean(pred_prev_diab, na.rm=T)  )
raceinccat_mean_dm <- raceinccat_mean_dm[complete.cases(raceinccat_mean_dm),]
raceinccat_confint_dm <- aou %>%
  group_by(racecat, inc_simple) %>%
  summarise( 
    n=n(),
    mean=mean(pred_prev_diab, na.rm=TRUE),
    sd=sd(pred_prev_diab, na.rm=TRUE)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( conf95=100*se * qt((1-0.05)/2 + .5, n-1))
raceinccat_confint_dm <- raceinccat_confint_dm[complete.cases(raceinccat_confint_dm),]

raceinccat_mean_dm <- as.data.frame(raceinccat_mean_dm[complete.cases(raceinccat_mean_dm),])
raceinccat_mean_dm <- reshape(raceinccat_mean_dm, idvar='inc_simple', timevar='racecat', direction='wide')
raceinccat_mean_dm <- as.matrix(raceinccat_mean_dm, ncol=7)
as.numeric(raceinccat_mean_dm)
raceinccat_mean_dm <- matrix(as.numeric(raceinccat_mean_dm), ncol=8)
colnames(raceinccat_mean_dm) <- c('',colnames(raceinccat_counts_dm))
raceinccat_mean_dm <- raceinccat_mean_dm[,2:8]
dim(raceinccat_mean_dm)
class(raceinccat_mean_dm)

bar_prevdiab_inc <- barplot(raceinccat_mean_dm, main="Age, Sex, and Year-Adjusted Prevalence of Type 2 Diabetes \n by Race/Ethnicity and Income (Categorical)", xlab='', ylab='Percent with Type 2 Diabetes', col=c('darkslategray3','darkseagreen','lightgoldenrod','lightsalmon'),#'tomato'), 
                            legend.text=c(rownames(dm2_counts_inccat)), args.legend=list(x="right",inset=c(-0.6,0),cex=0.8,ncol=1, title='Income Category'),cex.lab=1.5,cex.main=1.5,ylim=c(0,35),beside=TRUE)
bar_prevdiab_inc + arrows(x0=mid_inccat, y0=raceinccat_mean_dm - raceinccat_confint_dm$conf95, x1=mid_inccat, y1=raceinccat_mean_dm + raceinccat_confint_dm$conf95, code=3, angle=90, length=0.1)


#estimated prevalence - obesity
obese_adj <- glm(data=aou, obese ~ age + female + racecat, family=quasibinomial())#family=stats::gaussian())
aou$pred_prev_obese <- predict(obese_adj, newdata=aou, type='response')
summary(aou$pred_prev_obese)
mean(aou$pred_prev_obese[aou$racecat=='NHAsian'])
ggplot(data = subset(aou, racecat=='NHW'), 
       aes(x = pred_prev_obese, fill = factor(racecat)))+
  geom_histogram(aes(y = ..density..))+
  geom_histogram(data = subset(aou, racecat=='NHB'), 
                 aes(x = pred_prev_obese, y = -..density.., fill=factor(racecat)))+
  xlab("Predicted T2D Prevalence")+ylab("Density")+
  ggtitle("Histogram of Predicted T2D Prevalence by NHW v NHB") + 
  scale_fill_discrete("Race: NHW v NHB")
#estimated prevalence barplot - educ obese
ob_counts_educcat <- table(aou$educ_simple[aou$obese==1], aou$racecat[aou$obese==1])
raceeduccat_counts_dm <- table(aou$educ_simple, aou$racecat) #whichever is second will be on the x axis groupings, first will be series 1/2/3 colors
mid_educcat<-barplot(100*ob_counts_educcat/raceeduccat_counts_dm, legend=rownames(raceeduccat_counts_dm), beside=TRUE)

raceeduccat_mean_ob <- aou %>% 
  group_by(racecat, educ_simple) %>%
  summarise(
    mean=100*mean(pred_prev_obese, na.rm=T)  )
raceeduccat_mean_ob <- raceeduccat_mean_ob[complete.cases(raceeduccat_mean_ob),]
raceeduccat_confint_ob <- aou %>%
  group_by(racecat, educ_simple) %>%
  summarise( 
    n=n(),
    mean=mean(pred_prev_obese, na.rm=TRUE),
    sd=sd(pred_prev_obese, na.rm=TRUE)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( conf95=100*se * qt((1-0.05)/2 + .5, n-1))
raceeduccat_confint_ob <- raceeduccat_confint_ob[complete.cases(raceeduccat_confint_ob),]

raceeduccat_mean_ob <- as.data.frame(raceeduccat_mean_ob[complete.cases(raceeduccat_mean_ob),])
raceeduccat_mean_ob <- reshape(raceeduccat_mean_ob, idvar='educ_simple', timevar='racecat', direction='wide')
raceeduccat_mean_ob <- as.matrix(raceeduccat_mean_ob, ncol=7)
as.numeric(raceeduccat_mean_ob)
raceeduccat_mean_ob <- matrix(as.numeric(raceeduccat_mean_ob), ncol=8)
colnames(raceeduccat_mean_ob) <- c('',colnames(raceeduccat_counts_dm))
raceeduccat_mean_ob <- raceeduccat_mean_ob[,2:8]
dim(raceeduccat_mean_ob)
class(raceeduccat_mean_ob)

bar_prevob_educ <- barplot(raceeduccat_mean_ob, main="Age, Sex, and Year-Adjusted Prevalence of Obesity \n by Race/Ethnicity and Educational Attainment (Categorical)", xlab='', ylab='Percent with Obesity', col=c('darkslategray3','darkseagreen','lightgoldenrod','lightsalmon'),#'tomato'), 
                           legend.text=c(rownames(ob_counts_educcat)), args.legend=list(x="right", inset=c(-0.6,0),cex=0.8,ncol=1, title='Educational Attainment'),cex.lab=1.5,cex.main=1.5,ylim=c(0,65),beside=TRUE)
bar_prevob_educ + arrows(x0=mid_educcat, y0=raceeduccat_mean_ob - raceeduccat_confint_ob$conf95, x1=mid_educcat, y1=raceeduccat_mean_ob + raceeduccat_confint_ob$conf95, code=3, angle=90, length=0.1)

#estimated prevalence barplot - inc obese
ob_counts_inccat <- table(aou$inc_simple[aou$obese==1], aou$racecat[aou$obese==1])
raceinccat_counts_dm <- table(aou$inc_simple, aou$racecat) #whichever is second will be on the x axis groupings, first will be series 1/2/3 colors
mid_inccat<-barplot(100*ob_counts_inccat/raceinccat_counts_dm, legend=rownames(raceinccat_counts_dm), beside=TRUE)

raceinccat_mean_ob <- aou %>% 
  group_by(racecat, inc_simple) %>%
  summarise(
    mean=100*mean(pred_prev_obese, na.rm=T)  )
raceinccat_mean_ob <- raceinccat_mean_ob[complete.cases(raceinccat_mean_ob),]
raceinccat_confint_ob <- aou %>%
  group_by(racecat, inc_simple) %>%
  summarise( 
    n=n(),
    mean=mean(pred_prev_obese, na.rm=TRUE),
    sd=sd(pred_prev_obese, na.rm=TRUE)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( conf95=100*se * qt((1-0.05)/2 + .5, n-1))
raceinccat_confint_ob <- raceinccat_confint_ob[complete.cases(raceinccat_confint_ob),]

raceinccat_mean_ob <- as.data.frame(raceinccat_mean_ob[complete.cases(raceinccat_mean_ob),])
raceinccat_mean_ob <- reshape(raceinccat_mean_ob, idvar='inc_simple', timevar='racecat', direction='wide')
raceinccat_mean_ob <- as.matrix(raceinccat_mean_ob, ncol=7)
as.numeric(raceinccat_mean_ob)
raceinccat_mean_ob <- matrix(as.numeric(raceinccat_mean_ob), ncol=8)
colnames(raceinccat_mean_ob) <- c('',colnames(raceinccat_counts_dm))
raceinccat_mean_ob <- raceinccat_mean_ob[,2:8]
dim(raceinccat_mean_ob)
class(raceinccat_mean_ob)

bar_prevob_inc <- barplot(raceinccat_mean_ob, main="Age, Sex, and Year-Adjusted Prevalence of Obesity \n by Race/Ethnicity and Income (Categorical)", xlab='', ylab='Percent with Obesity', col=c('darkslategray3','darkseagreen','lightgoldenrod','lightsalmon'),#'tomato'), 
                          legend.text=c(rownames(ob_counts_inccat)), args.legend=list(x="right",inset=c(-0.6,0),cex=0.8,ncol=1, title='Income Category'),cex.lab=1.5,cex.main=1.5,ylim=c(0,65),beside=TRUE)
bar_prevob_inc + arrows(x0=mid_inccat, y0=raceinccat_mean_ob - raceinccat_confint_ob$conf95, x1=mid_inccat, y1=raceinccat_mean_ob + raceinccat_confint_ob$conf95, code=3, angle=90, length=0.1)


#####save grouped bar predicted prevalence figure#####
tiff(file='C:/Users/sarac/OneDrive/Documents/Research/Area v Individual SES/aou_bar_indiv_educinc_preddiabob.tif', width=8, height=12, units='in', res=100)
par(mfrow=c(2,2))
bar_prevdiab_educ <- barplot(raceeduccat_mean_dm, main="Age, Sex, and Year-Adjusted Prevalence of Type 2 Diabetes \n by Race/Ethnicity and Educational Attainment (Categorical)", xlab='', ylab='Percent with Type 2 Diabetes', col=c('darkslategray3','darkseagreen','lightgoldenrod','lightsalmon'),#'tomato'), 
                             legend.text=c(rownames(dm2_counts_educcat)), args.legend=list(x="right",inset=c(-0.6,0),cex=0.5,ncol=1, title='Educational Attainment'),cex.lab=0.5,cex.main=0.5,ylim=c(0,35),beside=TRUE)
bar_prevdiab_educ + arrows(x0=mid_educcat, y0=raceeduccat_mean_dm - raceeduccat_confint_dm$conf95, x1=mid_educcat, y1=raceeduccat_mean_dm + raceeduccat_confint_dm$conf95, code=3, angle=90, length=0.1)

bar_prevdiab_inc <- barplot(raceinccat_mean_dm, main="Age, Sex, and Year-Adjusted Prevalence of Type 2 Diabetes \n by Race/Ethnicity and Income (Categorical)", xlab='', ylab='Percent with Type 2 Diabetes', col=c('darkslategray3','darkseagreen','lightgoldenrod','lightsalmon'),#'tomato'), 
                            legend.text=c(rownames(dm2_counts_inccat)), args.legend=list(x="right",inset=c(-0.6,0),cex=0.5,ncol=1, title='Income Category'),cex.lab=0.5,cex.main=0.5,ylim=c(0,35),beside=TRUE)
bar_prevdiab_inc + arrows(x0=mid_inccat, y0=raceinccat_mean_dm - raceinccat_confint_dm$conf95, x1=mid_inccat, y1=raceinccat_mean_dm + raceinccat_confint_dm$conf95, code=3, angle=90, length=0.1)

bar_prevob_educ <- barplot(raceeduccat_mean_ob, main="Age, Sex, and Year-Adjusted Prevalence of Obesity \n by Race/Ethnicity and Educational Attainment (Categorical)", xlab='', ylab='Percent with Obesity', col=c('darkslategray3','darkseagreen','lightgoldenrod','lightsalmon'),#'tomato'), 
                           legend.text=c(rownames(ob_counts_educcat)), args.legend=list(x="right", inset=c(-0.6,0),cex=0.5,ncol=1, title='Educational Attainment'),cex.lab=0.5,cex.main=0.5,ylim=c(0,60),beside=TRUE)
bar_prevob_educ + arrows(x0=mid_educcat, y0=raceeduccat_mean_ob - raceeduccat_confint_ob$conf95, x1=mid_educcat, y1=raceeduccat_mean_ob + raceeduccat_confint_ob$conf95, code=3, angle=90, length=0.1)

bar_prevob_inc <- barplot(raceinccat_mean_ob, main="Age, Sex, and Year-Adjusted Prevalence of Obesity \n by Race/Ethnicity and Income (Categorical)", xlab='', ylab='Percent with Obesity', col=c('darkslategray3','darkseagreen','lightgoldenrod','lightsalmon'),#'tomato'), 
                          legend.text=c(rownames(ob_counts_inccat)), args.legend=list(x="right",inset=c(-0.6,0),cex=0.5,ncol=1, title='Income Category'),cex.lab=0.5,cex.main=0.5,ylim=c(0,60),beside=TRUE)
bar_prevob_inc + arrows(x0=mid_inccat, y0=raceinccat_mean_ob - raceinccat_confint_ob$conf95, x1=mid_inccat, y1=raceinccat_mean_ob + raceinccat_confint_ob$conf95, code=3, angle=90, length=0.1)

dev.off()
par(mfrow=c(1,1))


#predicted prevalence means by racecat
surv_all2 <- svydesign(id = ~SDMVPSU, strata = ~ SDMVSTRA, weights = ~weights, data=aou, nest=T)
aou %>% group_by(racecat) %>% summarise(mean=mean(age), na.rm=T)
summary(aou$pred_prev_diab)
mean(aou$pred_prev_diab)
pred_prev_byrace <- rbind(c(mean(aou$pred_prev_diab), svymean(~pred_prev_diab, subset(surv_all2, age >= 18 & !insur_stability=='Missing'))),
                          c(mean(aou$pred_prev_diab[aou$racecat=='NHW']),svymean(~pred_prev_diab, subset(surv_all2, age >= 18 & !insur_stability=='Missing' & racecat=='NHW'))),
                          c(mean(aou$pred_prev_diab[aou$racecat=='NHB']), svymean(~pred_prev_diab, subset(surv_all2, age >= 18 & !insur_stability=='Missing' & racecat=='NHB'))),
                          c(mean(aou$pred_prev_diab[aou$racecat=='NHAsian']), svymean(~pred_prev_diab, subset(surv_all2, age >= 18 & !insur_stability=='Missing' & racecat=='NHAsian'))),
                          c(mean(aou$pred_prev_diab[aou$racecat=='Mexican-American']), svymean(~pred_prev_diab, subset(surv_all2, age >= 18 & !insur_stability=='Missing' & racecat=='Mexican-American'))),
                          c(mean(aou$pred_prev_diab[aou$racecat=='Other Hispanic']), svymean(~pred_prev_diab, subset(surv_all2, age >= 18 & !insur_stability=='Missing' & racecat=='Other Hispanic'))),
                          c(mean(aou$pred_prev_diab[aou$racecat=='Multiracial']), svymean(~pred_prev_diab, subset(surv_all2, age >= 18 & !insur_stability=='Missing' & racecat=='Multiracial'))),
                          c(mean(aou$pred_prev_diab[aou$racecat=='Other']), svymean(~pred_prev_diab, subset(surv_all2, age >= 18 & !insur_stability=='Missing' & racecat=='Other'))))
colnames(pred_prev_byrace) <- c('mean_preddiab','svymean_preddiab')
rownames(pred_prev_byrace) <- c('Overall',rownames(table(aou$racecat, aou$female)))

?svystandardize
aou_std <- svystandardize(surv_all2, by=~age + female + survyr, over=~racecat, population=c(***))
svyby(~pred_prev_diab, ~racecat, svymean, surv_all2)


#####CONTIN EDUC, ALL DISEASE FORESTS#####
mod_diab_educcont_all <- glm(data=aou, diab ~ age + female + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
summary(mod_diab_educcont_all)
nobs(mod_diab_educcont_all)
exp(mod_diab_educcont_all$coefficients)
mod_diab_educcont_all_raceadj <- glm(data=aou, diab ~ age + female + smoke_status + insur_stability + educ_num + racecat, family=binomial(link=logit))
mod_diab_educcont_nhw <- glm(data=aou[aou$racecat=='NHW',], diab ~ age + female + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_diab_educcont_nhb <- glm(data=aou[aou$racecat=='NHB',], diab ~ age + female + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
summary(mod_diab_educcont_nhb)
exp(mod_diab_educcont_nhb$coefficients)
mod_diab_educcont_nha <- glm(data=aou[aou$racecat=='NHAsian',], diab ~ age + female + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_diab_educcont_mex <- glm(data=aou[aou$racecat=='Hispanic',], diab ~ age + female + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_diab_educcont_his <- glm(data=aou[aou$racecat=='Multiracial',], diab ~ age + female + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_diab_educcont_oth <- glm(data=aou[aou$racecat=='Other',], diab ~ age + female + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_diab_educcont_oth11 <- glm(data=aou[aou$racecat=='None of these',], diab ~ age + female + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
a <- cbind(exp(mod_diab_educcont_all$coefficients),exp(mod_diab_educcont_all_raceadj$coefficients),exp(mod_diab_educcont_nhw$coefficients),
           exp(mod_diab_educcont_nhb$coefficients),exp(mod_diab_educcont_mex$coefficients),exp(mod_diab_educcont_his$coefficients),
           exp(mod_diab_educcont_nha$coefficients),exp(mod_diab_educcont_oth$coefficients),exp(mod_diab_educcont_oth11$coefficients))

mod_diab_educcont_all_bmi <- glm(data=aou, diab ~ age + female + smoke_status + insur_stability + educ_num + bmi, family=binomial(link=logit))
summary(mod_diab_educcont_all_bmi)
exp(mod_diab_educcont_all_bmi$coefficients)
mod_diab_educcont_all_raceadj_bmi <- glm(data=aou, diab ~ age + female + smoke_status + insur_stability + educ_num + bmi + racecat, family=binomial(link=logit))
mod_diab_educcont_nhw_bmi <- glm(data=aou[aou$racecat=='NHW',], diab ~ age + female + smoke_status + insur_stability + educ_num + bmi, family=binomial(link=logit))
mod_diab_educcont_nhb_bmi <- glm(data=aou[aou$racecat=='NHB',], diab ~ age + female + smoke_status + insur_stability + educ_num + bmi, family=binomial(link=logit))
summary(mod_diab_educcont_nhb_bmi)
exp(mod_diab_educcont_nhb_bmi$coefficients)
mod_diab_educcont_nha_bmi <- glm(data=aou[aou$racecat=='NHAsian',], diab ~ age + female + smoke_status + insur_stability + educ_num + bmi, family=binomial(link=logit))
mod_diab_educcont_mex_bmi <- glm(data=aou[aou$racecat=='Hispanic',], diab ~ age + female + smoke_status + insur_stability + educ_num + bmi, family=binomial(link=logit))
mod_diab_educcont_his_bmi <- glm(data=aou[aou$racecat=='Multiracial',], diab ~ age + female + smoke_status + insur_stability + educ_num + bmi + bmi, family=binomial(link=logit))
mod_diab_educcont_oth_bmi <- glm(data=aou[aou$racecat=='Other',], diab ~ age + female + smoke_status + insur_stability + educ_num + bmi, family=binomial(link=logit))
mod_diab_educcont_oth11_bmi <- glm(data=aou[aou$racecat=='None of these',], diab ~ age + female + smoke_status + insur_stability + educ_num + bmi, family=binomial(link=logit))
b <- cbind(exp(mod_diab_educcont_all_bmi$coefficients),exp(mod_diab_educcont_all_raceadj_bmi$coefficients),exp(mod_diab_educcont_nhw_bmi$coefficients),
           exp(mod_diab_educcont_nhb_bmi$coefficients),exp(mod_diab_educcont_mex_bmi$coefficients),exp(mod_diab_educcont_his_bmi$coefficients),
           exp(mod_diab_educcont_nha_bmi$coefficients),exp(mod_diab_educcont_oth_bmi$coefficients),exp(mod_diab_educcont_oth11_bmi$coefficients))


mod_obes_educcont_all <- glm(data=aou, obese ~ age + female + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_obes_educcont_all_raceadj <- glm(data=aou, obese ~ age + female + smoke_status + insur_stability + educ_num + racecat, family=binomial(link=logit))
mod_obes_educcont_nhw <- glm(data=aou[aou$racecat=='NHW',], obese ~ age + female + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_obes_educcont_nhb <- glm(data=aou[aou$racecat=='NHB',], obese ~ age + female + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_obes_educcont_nha <- glm(data=aou[aou$racecat=='NHAsian',], obese ~ age + female + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_obes_educcont_mex <- glm(data=aou[aou$racecat=='Hispanic',], obese ~ age + female + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_obes_educcont_his <- glm(data=aou[aou$racecat=='Multiracial',], obese ~ age + female + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_obes_educcont_oth <- glm(data=aou[aou$racecat=='Other',], obese ~ age + female + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_obes_educcont_oth11 <- glm(data=aou[aou$racecat=='None of these',], obese ~ age + female + smoke_status + insur_stability + educ_num, family=binomial(link=logit))

mod_htn_educcont_all <- glm(data=aou, htn ~ age + female + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_htn_educcont_all_raceadj <- glm(data=aou, htn ~ age + female + smoke_status + insur_stability + educ_num + racecat, family=binomial(link=logit))
mod_htn_educcont_nhw <- glm(data=aou[aou$racecat=='NHW',], htn ~ age + female + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_htn_educcont_nhb <- glm(data=aou[aou$racecat=='NHB',], htn ~ age + female + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_htn_educcont_nha <- glm(data=aou[aou$racecat=='NHAsian',], htn ~ age + female + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_htn_educcont_mex <- glm(data=aou[aou$racecat=='Hispanic',], htn ~ age + female + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_htn_educcont_his <- glm(data=aou[aou$racecat=='Multiracial',], htn ~ age + female + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_htn_educcont_oth <- glm(data=aou[aou$racecat=='Other',], htn ~ age + female + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_htn_educcont_oth11 <- glm(data=aou[aou$racecat=='None of these',], htn ~ age + female + smoke_status + insur_stability + educ_num, family=binomial(link=logit))

mod_cvd_educcont_all <- glm(data=aou, cvd ~ age + female + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_cvd_educcont_all_raceadj <- glm(data=aou, cvd ~ age + female + smoke_status + insur_stability + educ_num + racecat, family=binomial(link=logit))
mod_cvd_educcont_nhw <- glm(data=aou[aou$racecat=='NHW',], cvd ~ age + female + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_cvd_educcont_nhb <- glm(data=aou[aou$racecat=='NHB',], cvd ~ age + female + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_cvd_educcont_nha <- glm(data=aou[aou$racecat=='NHAsian',], cvd ~ age + female + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_cvd_educcont_mex <- glm(data=aou[aou$racecat=='Hispanic',], cvd ~ age + female + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_cvd_educcont_his <- glm(data=aou[aou$racecat=='Multiracial',], cvd ~ age + female + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_cvd_educcont_oth <- glm(data=aou[aou$racecat=='Other',], cvd ~ age + female + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_cvd_educcont_oth11 <- glm(data=aou[aou$racecat=='None of these',], cvd ~ age + female + smoke_status + insur_stability + educ_num, family=binomial(link=logit))

mod_ckd_educcont_all <- glm(data=aou, ckd ~ age + female + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_ckd_educcont_all_raceadj <- glm(data=aou, ckd ~ age + female + smoke_status + insur_stability + educ_num + racecat, family=binomial(link=logit))
mod_ckd_educcont_nhw <- glm(data=aou[aou$racecat=='NHW',], ckd ~ age + female + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_ckd_educcont_nhb <- glm(data=aou[aou$racecat=='NHB',], ckd ~ age + female + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_ckd_educcont_nha <- glm(data=aou[aou$racecat=='NHAsian',], ckd ~ age + female + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_ckd_educcont_mex <- glm(data=aou[aou$racecat=='Hispanic',], ckd ~ age + female + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_ckd_educcont_his <- glm(data=aou[aou$racecat=='Multiracial',], ckd ~ age + female + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_ckd_educcont_oth <- glm(data=aou[aou$racecat=='Other',], ckd ~ age + female + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_ckd_educcont_oth11 <- glm(data=aou[aou$racecat=='None of these',], ckd ~ age + female + smoke_status + insur_stability + educ_num, family=binomial(link=logit))

mod_copd_educcont_all <- glm(data=aou, copd ~ age + female + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_copd_educcont_all_raceadj <- glm(data=aou, copd ~ age + female + smoke_status + insur_stability + educ_num + racecat, family=binomial(link=logit))
mod_copd_educcont_nhw <- glm(data=aou[aou$racecat=='NHW',], copd ~ age + female + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_copd_educcont_nhb <- glm(data=aou[aou$racecat=='NHB',], copd ~ age + female + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_copd_educcont_nha <- glm(data=aou[aou$racecat=='NHAsian',], copd ~ age + female + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_copd_educcont_mex <- glm(data=aou[aou$racecat=='Hispanic',], copd ~ age + female + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_copd_educcont_his <- glm(data=aou[aou$racecat=='Multiracial',], copd ~ age + female + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_copd_educcont_oth <- glm(data=aou[aou$racecat=='Other',], copd ~ age + female + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_copd_educcont_oth11 <- glm(data=aou[aou$racecat=='None of these',], copd ~ age + female + smoke_status + insur_stability + educ_num, family=binomial(link=logit))

mod_mdd_educcont_all <- glm(data=aou, mdd ~ age + female + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_mdd_educcont_all_raceadj <- glm(data=aou, mdd ~ age + female + smoke_status + insur_stability + educ_num + racecat, family=binomial(link=logit))
mod_mdd_educcont_nhw <- glm(data=aou[aou$racecat=='NHW',], mdd ~ age + female + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_mdd_educcont_nhb <- glm(data=aou[aou$racecat=='NHB',], mdd ~ age + female + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_mdd_educcont_nha <- glm(data=aou[aou$racecat=='NHAsian',], mdd ~ age + female + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_mdd_educcont_mex <- glm(data=aou[aou$racecat=='Hispanic',], mdd ~ age + female + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_mdd_educcont_his <- glm(data=aou[aou$racecat=='Multiracial',], mdd ~ age + female + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_mdd_educcont_oth <- glm(data=aou[aou$racecat=='Other',], mdd ~ age + female + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_mdd_educcont_oth11 <- glm(data=aou[aou$racecat=='None of these',], mdd ~ age + female + smoke_status + insur_stability + educ_num, family=binomial(link=logit))

mod_alldx_educcont_allrace <- data.frame(exp(mod_diab_educcont_all$coefficients),exp(confint(mod_diab_educcont_all)),coef(summary(mod_diab_educcont_all))[,"Pr(>|t|)"],
                                         exp(mod_diab_educcont_all_raceadj$coefficients)[1:11],exp(confint(mod_diab_educcont_all_raceadj))[1:11,1:2],coef(summary(mod_diab_educcont_all_raceadj))[,"Pr(>|t|)"][1:11],
                                         exp(mod_diab_educcont_nhw$coefficients),exp(confint(mod_diab_educcont_nhw)),coef(summary(mod_diab_educcont_nhw))[,"Pr(>|t|)"],
                                         exp(mod_diab_educcont_nhb$coefficients),exp(confint(mod_diab_educcont_nhb)),coef(summary(mod_diab_educcont_nhb))[,"Pr(>|t|)"],
                                         exp(mod_diab_educcont_mex$coefficients),exp(confint(mod_diab_educcont_mex)),coef(summary(mod_diab_educcont_mex))[,"Pr(>|t|)"],
                                         exp(mod_diab_educcont_his$coefficients),exp(confint(mod_diab_educcont_his)),coef(summary(mod_diab_educcont_his))[,"Pr(>|t|)"],
                                         exp(mod_diab_educcont_nha$coefficients),exp(confint(mod_diab_educcont_nha)),coef(summary(mod_diab_educcont_nha))[,"Pr(>|t|)"],
                                         exp(mod_diab_educcont_oth$coefficients),exp(confint(mod_diab_educcont_oth)),coef(summary(mod_diab_educcont_oth))[,"Pr(>|t|)"],
                                         exp(mod_diab_educcont_oth11$coefficients),exp(confint(mod_diab_educcont_oth11)),coef(summary(mod_diab_educcont_oth11))[,"Pr(>|t|)"],
                                         exp(mod_obes_educcont_all$coefficients),exp(confint(mod_obes_educcont_all)),coef(summary(mod_obes_educcont_all))[,"Pr(>|t|)"],
                                         exp(mod_obes_educcont_all_raceadj$coefficients)[1:11],exp(confint(mod_obes_educcont_all_raceadj))[1:11,1:2],coef(summary(mod_obes_educcont_all_raceadj))[,"Pr(>|t|)"][1:11],
                                         exp(mod_obes_educcont_nhw$coefficients),exp(confint(mod_obes_educcont_nhw)),coef(summary(mod_obes_educcont_nhw))[,"Pr(>|t|)"],
                                         exp(mod_obes_educcont_nhb$coefficients),exp(confint(mod_obes_educcont_nhb)),coef(summary(mod_obes_educcont_nhb))[,"Pr(>|t|)"],
                                         exp(mod_obes_educcont_mex$coefficients),exp(confint(mod_obes_educcont_mex)),coef(summary(mod_obes_educcont_mex))[,"Pr(>|t|)"],
                                         exp(mod_obes_educcont_his$coefficients),exp(confint(mod_obes_educcont_his)),coef(summary(mod_obes_educcont_his))[,"Pr(>|t|)"],
                                         exp(mod_obes_educcont_nha$coefficients),exp(confint(mod_obes_educcont_nha)),coef(summary(mod_obes_educcont_nha))[,"Pr(>|t|)"],
                                         exp(mod_obes_educcont_oth$coefficients),exp(confint(mod_obes_educcont_oth)),coef(summary(mod_obes_educcont_oth))[,"Pr(>|t|)"],
                                         exp(mod_obes_educcont_oth11$coefficients),exp(confint(mod_obes_educcont_oth11)),coef(summary(mod_obes_educcont_oth11))[,"Pr(>|t|)"],
                                         exp(mod_htn_educcont_all$coefficients),exp(confint(mod_htn_educcont_all)),coef(summary(mod_htn_educcont_all))[,"Pr(>|t|)"],
                                         exp(mod_htn_educcont_all_raceadj$coefficients)[1:11],exp(confint(mod_htn_educcont_all_raceadj))[1:11,1:2],coef(summary(mod_htn_educcont_all_raceadj))[,"Pr(>|t|)"][1:11],
                                         exp(mod_htn_educcont_nhw$coefficients),exp(confint(mod_htn_educcont_nhw)),coef(summary(mod_htn_educcont_nhw))[,"Pr(>|t|)"],
                                         exp(mod_htn_educcont_nhb$coefficients),exp(confint(mod_htn_educcont_nhb)),coef(summary(mod_htn_educcont_nhb))[,"Pr(>|t|)"],
                                         exp(mod_htn_educcont_mex$coefficients),exp(confint(mod_htn_educcont_mex)),coef(summary(mod_htn_educcont_mex))[,"Pr(>|t|)"],
                                         exp(mod_htn_educcont_his$coefficients),exp(confint(mod_htn_educcont_his)),coef(summary(mod_htn_educcont_his))[,"Pr(>|t|)"],
                                         exp(mod_htn_educcont_nha$coefficients),exp(confint(mod_htn_educcont_nha)),coef(summary(mod_htn_educcont_nha))[,"Pr(>|t|)"],
                                         exp(mod_htn_educcont_oth$coefficients),exp(confint(mod_htn_educcont_oth)),coef(summary(mod_htn_educcont_oth))[,"Pr(>|t|)"],
                                         exp(mod_htn_educcont_oth11$coefficients),exp(confint(mod_htn_educcont_oth11)),coef(summary(mod_htn_educcont_oth11))[,"Pr(>|t|)"],
                                         exp(mod_cvd_educcont_all$coefficients),exp(confint(mod_cvd_educcont_all)),coef(summary(mod_cvd_educcont_all))[,"Pr(>|t|)"],
                                         exp(mod_cvd_educcont_all_raceadj$coefficients)[1:11],exp(confint(mod_cvd_educcont_all_raceadj))[1:11,1:2],coef(summary(mod_cvd_educcont_all_raceadj))[,"Pr(>|t|)"][1:11],
                                         exp(mod_cvd_educcont_nhw$coefficients),exp(confint(mod_cvd_educcont_nhw)),coef(summary(mod_cvd_educcont_nhw))[,"Pr(>|t|)"],
                                         exp(mod_cvd_educcont_nhb$coefficients),exp(confint(mod_cvd_educcont_nhb)),coef(summary(mod_cvd_educcont_nhb))[,"Pr(>|t|)"],
                                         exp(mod_cvd_educcont_mex$coefficients),exp(confint(mod_cvd_educcont_mex)),coef(summary(mod_cvd_educcont_mex))[,"Pr(>|t|)"],
                                         exp(mod_cvd_educcont_his$coefficients),exp(confint(mod_cvd_educcont_his)),coef(summary(mod_cvd_educcont_his))[,"Pr(>|t|)"],
                                         exp(mod_cvd_educcont_nha$coefficients),exp(confint(mod_cvd_educcont_nha)),coef(summary(mod_cvd_educcont_nha))[,"Pr(>|t|)"],
                                         exp(mod_cvd_educcont_oth$coefficients),exp(confint(mod_cvd_educcont_oth)),coef(summary(mod_cvd_educcont_oth))[,"Pr(>|t|)"],
                                         exp(mod_cvd_educcont_oth11$coefficients),exp(confint(mod_cvd_educcont_oth11)),coef(summary(mod_cvd_educcont_oth11))[,"Pr(>|t|)"],
                                         exp(mod_ckd_educcont_all$coefficients),exp(confint(mod_ckd_educcont_all)),coef(summary(mod_ckd_educcont_all))[,"Pr(>|t|)"],
                                         exp(mod_ckd_educcont_all_raceadj$coefficients)[1:11],exp(confint(mod_ckd_educcont_all_raceadj))[1:11,1:2],coef(summary(mod_ckd_educcont_all_raceadj))[,"Pr(>|t|)"][1:11],
                                         exp(mod_ckd_educcont_nhw$coefficients),exp(confint(mod_ckd_educcont_nhw)),coef(summary(mod_ckd_educcont_nhw))[,"Pr(>|t|)"],
                                         exp(mod_ckd_educcont_nhb$coefficients),exp(confint(mod_ckd_educcont_nhb)),coef(summary(mod_ckd_educcont_nhb))[,"Pr(>|t|)"],
                                         exp(mod_ckd_educcont_mex$coefficients),exp(confint(mod_ckd_educcont_mex)),coef(summary(mod_ckd_educcont_mex))[,"Pr(>|t|)"],
                                         exp(mod_ckd_educcont_his$coefficients),exp(confint(mod_ckd_educcont_his)),coef(summary(mod_ckd_educcont_his))[,"Pr(>|t|)"],
                                         exp(mod_ckd_educcont_nha$coefficients),exp(confint(mod_ckd_educcont_nha)),coef(summary(mod_ckd_educcont_nha))[,"Pr(>|t|)"],
                                         exp(mod_ckd_educcont_oth$coefficients),exp(confint(mod_ckd_educcont_oth)),coef(summary(mod_ckd_educcont_oth))[,"Pr(>|t|)"],
                                         exp(mod_ckd_educcont_oth11$coefficients),exp(confint(mod_ckd_educcont_oth11)),coef(summary(mod_ckd_educcont_oth11))[,"Pr(>|t|)"],
                                         exp(mod_copd_educcont_all$coefficients),exp(confint(mod_copd_educcont_all)),coef(summary(mod_copd_educcont_all))[,"Pr(>|t|)"],
                                         exp(mod_copd_educcont_all_raceadj$coefficients)[1:11],exp(confint(mod_copd_educcont_all_raceadj))[1:11,1:2],coef(summary(mod_copd_educcont_all_raceadj))[,"Pr(>|t|)"][1:11],
                                         exp(mod_copd_educcont_nhw$coefficients),exp(confint(mod_copd_educcont_nhw)),coef(summary(mod_copd_educcont_nhw))[,"Pr(>|t|)"],
                                         exp(mod_copd_educcont_nhb$coefficients),exp(confint(mod_copd_educcont_nhb)),coef(summary(mod_copd_educcont_nhb))[,"Pr(>|t|)"],
                                         exp(mod_copd_educcont_mex$coefficients),exp(confint(mod_copd_educcont_mex)),coef(summary(mod_copd_educcont_mex))[,"Pr(>|t|)"],
                                         exp(mod_copd_educcont_his$coefficients),exp(confint(mod_copd_educcont_his)),coef(summary(mod_copd_educcont_his))[,"Pr(>|t|)"],
                                         exp(mod_copd_educcont_nha$coefficients),exp(confint(mod_copd_educcont_nha)),coef(summary(mod_copd_educcont_nha))[,"Pr(>|t|)"],
                                         exp(mod_copd_educcont_oth$coefficients),exp(confint(mod_copd_educcont_oth)),coef(summary(mod_copd_educcont_oth))[,"Pr(>|t|)"],
                                         exp(mod_copd_educcont_oth11$coefficients),exp(confint(mod_copd_educcont_oth11)),coef(summary(mod_copd_educcont_oth11))[,"Pr(>|t|)"],
                                         exp(mod_mdd_educcont_all$coefficients),exp(confint(mod_mdd_educcont_all)),coef(summary(mod_mdd_educcont_all))[,"Pr(>|t|)"],
                                         exp(mod_mdd_educcont_all_raceadj$coefficients)[1:11],exp(confint(mod_mdd_educcont_all_raceadj))[1:11,1:2],coef(summary(mod_mdd_educcont_all_raceadj))[,"Pr(>|t|)"][1:11],
                                         exp(mod_mdd_educcont_nhw$coefficients),exp(confint(mod_mdd_educcont_nhw)),coef(summary(mod_mdd_educcont_nhw))[,"Pr(>|t|)"],
                                         exp(mod_mdd_educcont_nhb$coefficients),exp(confint(mod_mdd_educcont_nhb)),coef(summary(mod_mdd_educcont_nhb))[,"Pr(>|t|)"],
                                         exp(mod_mdd_educcont_mex$coefficients),exp(confint(mod_mdd_educcont_mex)),coef(summary(mod_mdd_educcont_mex))[,"Pr(>|t|)"],
                                         exp(mod_mdd_educcont_his$coefficients),exp(confint(mod_mdd_educcont_his)),coef(summary(mod_mdd_educcont_his))[,"Pr(>|t|)"],
                                         exp(mod_mdd_educcont_nha$coefficients),exp(confint(mod_mdd_educcont_nha)),coef(summary(mod_mdd_educcont_nha))[,"Pr(>|t|)"],
                                         exp(mod_mdd_educcont_oth$coefficients),exp(confint(mod_mdd_educcont_oth)),coef(summary(mod_mdd_educcont_oth))[,"Pr(>|t|)"],
                                         exp(mod_mdd_educcont_oth11$coefficients),exp(confint(mod_mdd_educcont_oth11)),coef(summary(mod_mdd_educcont_oth11))[,"Pr(>|t|)"],stringsAsFactors = FALSE)
dim(mod_alldx_educcont_allrace)
mod_alldx_educcont_allrace_transpose <- t(mod_alldx_educcont_allrace)
mod_alldx_educcont_allrace[,c(1,37,73,109,145,181,217)]
#all disease forest
tiff(file='C:/Users/sarac/OneDrive/Documents/Research/Area v Individual SES/aou_forest_indiv_educcont_stratandnonstrat_alldx.tiff', width=8, height=5, units='in', res=300)
forestplot(c('Type 2 Diabetes','Obesity','Hypertension','CVD','CKD','COPD','Depression'), 
           mean = cbind(t(mod_alldx_educcont_allrace[11,c(1,37,73,109,145,181,217)]),t(mod_alldx_educcont_allrace[11,c(5,41,77,113,149,185,221)]),
                        t(mod_alldx_educcont_allrace[11,c(9,45,81,117,153,189,225)]),t(mod_alldx_educcont_allrace[11,c(13,49,85,121,157,193,229)]),
                        t(mod_alldx_educcont_allrace[11,c(17,53,89,125,161,197,233)]),t(mod_alldx_educcont_allrace[11,c(21,57,93,129,165,201,237)]),
                        t(mod_alldx_educcont_allrace[11,c(25,61,97,133,169,205,241)]),t(mod_alldx_educcont_allrace[11,c(29,65,101,137,173,209,245)]),
                        t(mod_alldx_educcont_allrace[11,c(33,69,105,141,177,213,249)])),
           lower = cbind(t(mod_alldx_educcont_allrace[11,c(2,38,74,110,146,182,218)]),t(mod_alldx_educcont_allrace[11,c(6,42,78,114,150,186,222)]),
                         t(mod_alldx_educcont_allrace[11,c(10,46,82,118,154,190,226)]),t(mod_alldx_educcont_allrace[11,c(14,50,86,122,158,194,230)]),
                         t(mod_alldx_educcont_allrace[11,c(18,54,90,126,162,198,234)]),t(mod_alldx_educcont_allrace[11,c(22,58,94,130,166,202,238)]),
                         t(mod_alldx_educcont_allrace[11,c(26,62,98,134,170,206,242)]),t(mod_alldx_educcont_allrace[11,c(30,66,102,138,174,210,246)]),
                         t(mod_alldx_educcont_allrace[11,c(34,70,106,142,178,214,250)])),
           upper = cbind(t(mod_alldx_educcont_allrace[11,c(3,39,75,111,147,183,219)]),t(mod_alldx_educcont_allrace[11,c(7,43,79,115,151,187,223)]),
                         t(mod_alldx_educcont_allrace[11,c(11,47,83,119,155,191,227)]),t(mod_alldx_educcont_allrace[11,c(15,51,87,123,159,195,231)]),
                         t(mod_alldx_educcont_allrace[11,c(19,55,91,127,163,199,235)]),t(mod_alldx_educcont_allrace[11,c(23,59,95,131,167,203,239)]),
                         t(mod_alldx_educcont_allrace[11,c(27,63,99,135,171,207,243)]),t(mod_alldx_educcont_allrace[11,c(31,67,103,139,175,211,247)]),
                         t(mod_alldx_educcont_allrace[11,c(35,71,107,143,179,215,251)])),
           col = fpColors(box = c('gray23','gray57','dodgerblue4','darkslategray3','palegreen3','lightgoldenrod','salmon1','indianred3','mediumpurple4')),
           legend=c("All","All, Race-adjusted","NHW","NHB","Mexican-American","Other Hispanic","NHA","Other/Multi-Racial \n including NHA (pre-2011)","Other/Multi-Racial \n (post-2011)"),
           legend_args = fpLegend(pos = "right"),#list(x = 0.5, y = -0.2), gp = gpar(cex=0.9,col = "#CCCCCC", fill = "#F9F9F9")),
           #legend=c("All","All, Race-adjusted",rownames(table(aou$racecat))),#c("All","All-Race-adjusted","NHW","NHB","NHA","Mexican-American","Other Hispanic","Other/Multiracial"),
           #legend_args = fpLegend(pos = list(x = 0.9, y = 0.9), gp = gpar(cex=0.7,col = "#CCCCCC", fill = "#F9F9F9")),
           xlab = "OR (95% CI)",
           title = 'Adjusted OR for Continuous Education on All Diseases, \n Overall and Stratified by Race/Ethnicity',
           txt_gp = fpTxtGp(title=gpar(cex=0.8),xlab=gpar(cex=0.8), ticks=gpar(cex=0.8), label=gpar(cex=0.8)),
           vertices=TRUE,
           line.margin=0.25,
           graph.pos=2,
           clip=c(0.6,1.5), xlim=c(0.6,1.5), xticks=c(0.6,0.75,1.0,1.25,1.5), xlog=TRUE, grid=structure(c(0.6,0.75,1.0,1.25,1.5)))
dev.off()

#t2d and obesity only forest
tiff(file='C:/Users/sarac/OneDrive/Documents/Research/Area v Individual SES/Fig2A_aou_forest_indiv_educcont_stratandnonstrat.tiff', width=8, height=5, units='in', res=300)
par(xpd=T)
forestplot(c('Type 2 Diabetes','Obesity'), 
           mean = cbind(t(mod_alldx_educcont_allrace[11,c(1,37)]),t(mod_alldx_educcont_allrace[11,c(5,41)]),
                        t(mod_alldx_educcont_allrace[11,c(9,45)]),t(mod_alldx_educcont_allrace[11,c(13,49)]),
                        t(mod_alldx_educcont_allrace[11,c(17,53)]),t(mod_alldx_educcont_allrace[11,c(21,57)]),
                        t(mod_alldx_educcont_allrace[11,c(25,61)]),t(mod_alldx_educcont_allrace[11,c(29,65)]),
                        t(mod_alldx_educcont_allrace[11,c(33,69)])),
           lower = cbind(t(mod_alldx_educcont_allrace[11,c(2,38)]),t(mod_alldx_educcont_allrace[11,c(6,42)]),
                         t(mod_alldx_educcont_allrace[11,c(10,46)]),t(mod_alldx_educcont_allrace[11,c(14,50)]),
                         t(mod_alldx_educcont_allrace[11,c(18,54)]),t(mod_alldx_educcont_allrace[11,c(22,58)]),
                         t(mod_alldx_educcont_allrace[11,c(26,62)]),t(mod_alldx_educcont_allrace[11,c(30,66)]),
                         t(mod_alldx_educcont_allrace[11,c(34,68)])),
           upper = cbind(t(mod_alldx_educcont_allrace[11,c(3,39)]),t(mod_alldx_educcont_allrace[11,c(7,43)]),
                         t(mod_alldx_educcont_allrace[11,c(11,47)]),t(mod_alldx_educcont_allrace[11,c(15,51)]),
                         t(mod_alldx_educcont_allrace[11,c(19,55)]),t(mod_alldx_educcont_allrace[11,c(23,59)]),
                         t(mod_alldx_educcont_allrace[11,c(27,63)]),t(mod_alldx_educcont_allrace[11,c(31,67)]),
                         t(mod_alldx_educcont_allrace[11,c(35,71)])),
           col = fpColors(box = c('gray23','gray57','dodgerblue4','darkslategray3','palegreen3','lightgoldenrod','salmon1','indianred3','mediumpurple4')),
           legend=c("All","All, Race-adjusted","NHW","NHB","Mexican-American","Other Hispanic","NHA","Other/Multi-Racial \n including NHA (pre-2011)","Other/Multi-Racial \n (post-2011)"),
           legend_args = fpLegend(pos = "right"),#list(x = 0.5, y = -0.2), gp = gpar(cex=0.9,col = "#CCCCCC", fill = "#F9F9F9")),
           xlab = "OR (95% CI)",
           title = 'Adjusted OR for Continuous Education on T2D and Obesity Prevalence, \n Overall and Stratified by Race/Ethnicity',
           txt_gp = fpTxtGp(title=gpar(cex=1),xlab=gpar(cex=1), ticks=gpar(cex=1), label=gpar(cex=1)),
           vertices=TRUE,
           line.margin=0.25,
           graph.pos=2,
           clip=c(0.75,1.1), xlim=c(0.75,1.1), xticks=c(0.75,0.8,0.9,1.0,1.1), xlog=TRUE, grid=structure(c(0.75,0.8,0.9,1.0,1.1)))
dev.off()

#t2d (bmi-adj) and obesity only forest
mod_alldx_educcont_allrace_bmiadj <- data.frame(exp(mod_diab_educcont_all_bmi$coefficients)[1:11],exp(confint(mod_diab_educcont_all_bmi))[1:11,1:2],coef(summary(mod_diab_educcont_all_bmi))[,"Pr(>|t|)"][1:11],
                                                exp(mod_diab_educcont_all_raceadj_bmi$coefficients)[1:11],exp(confint(mod_diab_educcont_all_raceadj_bmi))[1:11,1:2],coef(summary(mod_diab_educcont_all_raceadj_bmi))[,"Pr(>|t|)"][1:11],
                                                exp(mod_diab_educcont_nhw_bmi$coefficients)[1:11],exp(confint(mod_diab_educcont_nhw_bmi))[1:11,1:2],coef(summary(mod_diab_educcont_nhw_bmi))[,"Pr(>|t|)"][1:11],
                                                exp(mod_diab_educcont_nhb_bmi$coefficients)[1:11],exp(confint(mod_diab_educcont_nhb_bmi))[1:11,1:2],coef(summary(mod_diab_educcont_nhb_bmi))[,"Pr(>|t|)"][1:11],
                                                exp(mod_diab_educcont_mex_bmi$coefficients)[1:11],exp(confint(mod_diab_educcont_mex_bmi))[1:11,1:2],coef(summary(mod_diab_educcont_mex_bmi))[,"Pr(>|t|)"][1:11],
                                                exp(mod_diab_educcont_his_bmi$coefficients)[1:11],exp(confint(mod_diab_educcont_his_bmi))[1:11,1:2],coef(summary(mod_diab_educcont_his_bmi))[,"Pr(>|t|)"][1:11],
                                                exp(mod_diab_educcont_nha_bmi$coefficients)[1:11],exp(confint(mod_diab_educcont_nha_bmi))[1:11,1:2],coef(summary(mod_diab_educcont_nha_bmi))[,"Pr(>|t|)"][1:11],
                                                exp(mod_diab_educcont_oth_bmi$coefficients)[1:11],exp(confint(mod_diab_educcont_oth_bmi))[1:11,1:2],coef(summary(mod_diab_educcont_oth_bmi))[,"Pr(>|t|)"][1:11],
                                                exp(mod_diab_educcont_oth11_bmi$coefficients)[1:11],exp(confint(mod_diab_educcont_oth11_bmi))[1:11,1:2],coef(summary(mod_diab_educcont_oth11_bmi))[,"Pr(>|t|)"][1:11],
                                                exp(mod_obes_educcont_all$coefficients),exp(confint(mod_obes_educcont_all)),coef(summary(mod_obes_educcont_all))[,"Pr(>|t|)"],
                                                exp(mod_obes_educcont_all_raceadj$coefficients)[1:11],exp(confint(mod_obes_educcont_all_raceadj))[1:11,1:2],coef(summary(mod_obes_educcont_all_raceadj))[,"Pr(>|t|)"][1:11],
                                                exp(mod_obes_educcont_nhw$coefficients),exp(confint(mod_obes_educcont_nhw)),coef(summary(mod_obes_educcont_nhw))[,"Pr(>|t|)"],
                                                exp(mod_obes_educcont_nhb$coefficients),exp(confint(mod_obes_educcont_nhb)),coef(summary(mod_obes_educcont_nhb))[,"Pr(>|t|)"],
                                                exp(mod_obes_educcont_mex$coefficients),exp(confint(mod_obes_educcont_mex)),coef(summary(mod_obes_educcont_mex))[,"Pr(>|t|)"],
                                                exp(mod_obes_educcont_his$coefficients),exp(confint(mod_obes_educcont_his)),coef(summary(mod_obes_educcont_his))[,"Pr(>|t|)"],
                                                exp(mod_obes_educcont_nha$coefficients),exp(confint(mod_obes_educcont_nha)),coef(summary(mod_obes_educcont_nha))[,"Pr(>|t|)"],
                                                exp(mod_obes_educcont_oth$coefficients),exp(confint(mod_obes_educcont_oth)),coef(summary(mod_obes_educcont_oth))[,"Pr(>|t|)"],
                                                exp(mod_obes_educcont_oth11$coefficients),exp(confint(mod_obes_educcont_oth11)),coef(summary(mod_obes_educcont_oth11))[,"Pr(>|t|)"],stringsAsFactors = FALSE)
dim(mod_alldx_educcont_allrace_bmiadj)
tiff(file='C:/Users/sarac/OneDrive/Documents/Research/Area v Individual SES/aou_forest_indiv_educcont_stratandnonstrat_t2dbmiadj.tiff', width=8, height=5, units='in', res=300)
par(xpd=T)
forestplot(c('Type 2 Diabetes','Obesity'), 
           mean = cbind(t(mod_alldx_educcont_allrace_bmiadj[11,c(1,37)]),t(mod_alldx_educcont_allrace_bmiadj[11,c(5,41)]),
                        t(mod_alldx_educcont_allrace_bmiadj[11,c(9,45)]),t(mod_alldx_educcont_allrace_bmiadj[11,c(13,49)]),
                        t(mod_alldx_educcont_allrace_bmiadj[11,c(17,53)]),t(mod_alldx_educcont_allrace_bmiadj[11,c(21,57)]),
                        t(mod_alldx_educcont_allrace_bmiadj[11,c(25,61)]),t(mod_alldx_educcont_allrace_bmiadj[11,c(29,65)]),
                        t(mod_alldx_educcont_allrace_bmiadj[11,c(33,69)])),
           lower = cbind(t(mod_alldx_educcont_allrace_bmiadj[11,c(2,38)]),t(mod_alldx_educcont_allrace_bmiadj[11,c(6,42)]),
                         t(mod_alldx_educcont_allrace_bmiadj[11,c(10,46)]),t(mod_alldx_educcont_allrace_bmiadj[11,c(14,50)]),
                         t(mod_alldx_educcont_allrace_bmiadj[11,c(18,54)]),t(mod_alldx_educcont_allrace_bmiadj[11,c(22,58)]),
                         t(mod_alldx_educcont_allrace_bmiadj[11,c(26,62)]),t(mod_alldx_educcont_allrace_bmiadj[11,c(30,66)]),
                         t(mod_alldx_educcont_allrace_bmiadj[11,c(34,68)])),
           upper = cbind(t(mod_alldx_educcont_allrace_bmiadj[11,c(3,39)]),t(mod_alldx_educcont_allrace_bmiadj[11,c(7,43)]),
                         t(mod_alldx_educcont_allrace_bmiadj[11,c(11,47)]),t(mod_alldx_educcont_allrace_bmiadj[11,c(15,51)]),
                         t(mod_alldx_educcont_allrace_bmiadj[11,c(19,55)]),t(mod_alldx_educcont_allrace_bmiadj[11,c(23,59)]),
                         t(mod_alldx_educcont_allrace_bmiadj[11,c(27,63)]),t(mod_alldx_educcont_allrace_bmiadj[11,c(31,67)]),
                         t(mod_alldx_educcont_allrace_bmiadj[11,c(35,71)])),
           col = fpColors(box = c('gray23','gray57','dodgerblue4','darkslategray3','palegreen3','lightgoldenrod','salmon1','indianred3','mediumpurple4')),
           legend=c("All","All, Race-adjusted","NHW","NHB","Mexican-American","Other Hispanic","NHA","Other/Multi-Racial \n including NHA (pre-2011)","Other/Multi-Racial \n (post-2011)"),
           legend_args = fpLegend(pos = "right"),#list(x = 0.5, y = -0.2), gp = gpar(cex=0.9,col = "#CCCCCC", fill = "#F9F9F9")),
           xlab = "OR (95% CI)",
           title = 'Adjusted OR for Continuous Education on T2D and Obesity Prevalence, \n Overall and Stratified by Race/Ethnicity',
           txt_gp = fpTxtGp(title=gpar(cex=1),xlab=gpar(cex=1), ticks=gpar(cex=1), label=gpar(cex=1)),
           vertices=TRUE,
           line.margin=0.25,
           graph.pos=2,
           clip=c(0.75,1.1), xlim=c(0.75,1.1), xticks=c(0.75,0.8,0.9,1.0,1.1), xlog=TRUE, grid=structure(c(0.75,0.8,0.9,1.0,1.1)))
dev.off()
#t2d - bmi mediation
bmi_med_educall <- glm(data=aou, bmi ~ age + female + smoke_status + insur_stability + educ_num)
bmi_med_educall_raceadj <- glm(data=aou, bmi ~ age + female + smoke_status + insur_stability + educ_num + racecat)
bmi_med_educnhw <- glm(data=aou[aou$racecat=='NHW',], bmi ~ age + female + smoke_status + insur_stability + educ_num)
bmi_med_educnhb <- glm(data=aou[aou$racecat=='NHB',], bmi ~ age + female + smoke_status + insur_stability + educ_num)
bmi_med_educmex <- glm(data=aou[aou$racecat=='Hispanic',], bmi ~ age + female + smoke_status + insur_stability + educ_num)
bmi_med_educhis <- glm(data=aou[aou$racecat=='NHAsian',], bmi ~ age + female + smoke_status + insur_stability + educ_num)
bmi_med_educnha <- glm(data=aou[aou$racecat=='Multiracial',], bmi ~ age + female + smoke_status + insur_stability + educ_num)
bmi_med_educoth <- glm(data=aou[aou$racecat=='Other',], bmi ~ age + female + smoke_status + insur_stability + educ_num)
bmi_med_educoth11 <- glm(data=aou[aou$racecat=='None of these',], bmi ~ age + female + smoke_status + insur_stability + educ_num)

t2d_propmediated_bmi_educ <- rbind(cbind('All',mod_diab_educcont_all_bmi$coefficients[11] + bmi_med_educall$coefficients[11]*mod_diab_educcont_all_bmi$coefficients[12], mod_diab_educcont_all_bmi$coefficients[11], bmi_med_educall$coefficients[11]*mod_diab_educcont_all_bmi$coefficients[12], bmi_med_educall$coefficients[11]*mod_diab_educcont_all_bmi$coefficients[12]/mod_diab_educcont_all_bmi$coefficients[11]),
                                   cbind('All-Race-adjusted',mod_diab_educcont_all_raceadj_bmi$coefficients[11] + bmi_med_educall_raceadj$coefficients[11]*mod_diab_educcont_all_raceadj_bmi$coefficients[12], mod_diab_educcont_all_raceadj_bmi$coefficients[11], bmi_med_educall_raceadj$coefficients[11]*mod_diab_educcont_all_bmi$coefficients[12], bmi_med_educall$coefficients[11]*mod_diab_educcont_all_raceadj_bmi$coefficients[12]/mod_diab_educcont_all_raceadj_bmi$coefficients[11]),
                                   cbind('NHW',mod_diab_educcont_nhw_bmi$coefficients[11] + bmi_med_educnhw$coefficients[11]*mod_diab_educcont_nhw_bmi$coefficients[12], mod_diab_educcont_nhw_bmi$coefficients[11], bmi_med_educnhw$coefficients[11]*mod_diab_educcont_nhw_bmi$coefficients[12], bmi_med_educnhw$coefficients[11]*mod_diab_educcont_nhw_bmi$coefficients[12]/mod_diab_educcont_nhw_bmi$coefficients[11]),
                                   cbind('NHB',mod_diab_educcont_nhb_bmi$coefficients[11] + bmi_med_educnhb$coefficients[11]*mod_diab_educcont_nhb_bmi$coefficients[12], mod_diab_educcont_nhb_bmi$coefficients[11], bmi_med_educnhb$coefficients[11]*mod_diab_educcont_nhb_bmi$coefficients[12], bmi_med_educnhb$coefficients[11]*mod_diab_educcont_nhb_bmi$coefficients[12]/mod_diab_educcont_nhb_bmi$coefficients[11]),
                                   cbind('MEX',mod_diab_educcont_mex_bmi$coefficients[11] + bmi_med_educmex$coefficients[11]*mod_diab_educcont_mex_bmi$coefficients[12], mod_diab_educcont_mex_bmi$coefficients[11], bmi_med_educmex$coefficients[11]*mod_diab_educcont_mex_bmi$coefficients[12], bmi_med_educmex$coefficients[11]*mod_diab_educcont_mex_bmi$coefficients[12]/mod_diab_educcont_mex_bmi$coefficients[11]),
                                   cbind('HIS',mod_diab_educcont_his_bmi$coefficients[11] + bmi_med_educhis$coefficients[11]*mod_diab_educcont_his_bmi$coefficients[12], mod_diab_educcont_his_bmi$coefficients[11], bmi_med_educhis$coefficients[11]*mod_diab_educcont_his_bmi$coefficients[12], bmi_med_educhis$coefficients[11]*mod_diab_educcont_his_bmi$coefficients[12]/mod_diab_educcont_his_bmi$coefficients[11]),
                                   cbind('NHA',mod_diab_educcont_nha_bmi$coefficients[11] + bmi_med_educnha$coefficients[11]*mod_diab_educcont_nha_bmi$coefficients[12], mod_diab_educcont_nha_bmi$coefficients[11], bmi_med_educnha$coefficients[11]*mod_diab_educcont_nha_bmi$coefficients[12], bmi_med_educnha$coefficients[11]*mod_diab_educcont_nha_bmi$coefficients[12]/mod_diab_educcont_nha_bmi$coefficients[11]),
                                   cbind('OTH',mod_diab_educcont_oth_bmi$coefficients[11] + bmi_med_educoth$coefficients[11]*mod_diab_educcont_oth_bmi$coefficients[12], mod_diab_educcont_oth_bmi$coefficients[11], bmi_med_educoth$coefficients[11]*mod_diab_educcont_oth_bmi$coefficients[12], bmi_med_educoth$coefficients[11]*mod_diab_educcont_oth_bmi$coefficients[12]/mod_diab_educcont_oth_bmi$coefficients[11]),
                                   cbind('OTH11',mod_diab_educcont_oth11_bmi$coefficients[11] + bmi_med_educoth11$coefficients[11]*mod_diab_educcont_oth11_bmi$coefficients[12], mod_diab_educcont_oth11_bmi$coefficients[11], bmi_med_educoth11$coefficients[11]*mod_diab_educcont_oth11_bmi$coefficients[12], bmi_med_educoth11$coefficients[11]*mod_diab_educcont_oth11_bmi$coefficients[12]/mod_diab_educcont_oth11_bmi$coefficients[11]))
colnames(t2d_propmediated_bmi_educ) <- c('Race/Ethnicity Group','Total Effect','Direct Effect','Indirect Effect','Proportion Mediated')
t2d_propmediated_bmi_educ


#####I2 Statistic - Education#####
###calc I2 for education -> diabetes prevalence
yi <- as.vector(t(mod_alldx_educcont_allrace[11,c(9,13,17,21,25,29,33)]))
n <- as.vector(t(table(aou[aou$age >= 18 & !aou$insur_stability=='Missing',]$racecat)))
racecatname <-rownames(table(aou[aou$age >= 18 & !aou$insur_stability=='Missing',]$racecat))
sei <- as.vector(t(( mod_alldx_educcont_allrace[11,c(11,15,19,23,27,31,35)] - mod_alldx_educcont_allrace[11,c(10,14,18,22,26,30,34)] ) / 3.92))
vi <- as.vector(t((sei*sqrt(n))^2)) #variance is SD^2, SD is SE*sqrt(n)
aou_dat <- data.frame(racecatname,n,yi,vi,sei)
aou_dat$log_yi <- log(yi, base=exp(1))
res_educ_diab <- rma(yi=log_yi,sei=sei, data=aou_dat)
res_nonhw_educ_diab <- rma(yi=log_yi,sei=sei, data=aou_dat[!racecatname=='NHW',])
res_nonhb_educ_diab <- rma(yi=log_yi,sei=sei, data=aou_dat[!racecatname=='NHB',])
res_nomex_educ_diab <- rma(yi=log_yi,sei=sei, data=aou_dat[!racecatname=='Mexican-American',])
res_nohis_educ_diab <- rma(yi=log_yi,sei=sei, data=aou_dat[!racecatname=='Other Hispanic',])
res_nonha_educ_diab <- rma(yi=log_yi,sei=sei, data=aou_dat[!racecatname=='NHAsian',])
res_nooth_educ_diab <- rma(yi=log_yi,sei=sei, data=aou_dat[!racecatname=='Multiracial',])
res_nooth11_educ_diab <- rma(yi=log_yi,sei=sei, data=aou_dat[!racecatname=='Other',])
i2_educ_diab <- rbind(res_educ_diab$I2, res_nonhw_educ_diab$I2, res_nonhb_educ_diab$I2, res_nomex_educ_diab$I2, res_nohis_educ_diab$I2, res_nonha_educ_diab$I2, res_nooth_educ_diab$I2, res_nooth11_educ_diab$I2)
#I2 from raw numbers: educ->diab prevalence - USE ME
n <- as.vector(t(table(aou[aou$age >= 18 & !aou$insur_stability=='Missing',]$racecat)))
racecatname <-rownames(table(aou[aou$age >= 18 & !aou$insur_stability=='Missing',]$racecat))
ESTIM_educ_diab <- c(coef(summary(mod_diab_educcont_nhw))[11,"Estimate"],coef(summary(mod_diab_educcont_nhb))[11,"Estimate"],
                     coef(summary(mod_diab_educcont_mex))[11,"Estimate"],coef(summary(mod_diab_educcont_his))[11,"Estimate"],coef(summary(mod_diab_educcont_nha))[11,"Estimate"],
                     coef(summary(mod_diab_educcont_oth))[11,"Estimate"],coef(summary(mod_diab_educcont_oth11))[11,"Estimate"])
SE_educ_diab <- c(coef(summary(mod_diab_educcont_nhw))[11,"Std. Error"],coef(summary(mod_diab_educcont_nhb))[11,"Std. Error"],
                  coef(summary(mod_diab_educcont_mex))[11,"Std. Error"],coef(summary(mod_diab_educcont_his))[11,"Std. Error"],coef(summary(mod_diab_educcont_nha))[11,"Std. Error"],
                  coef(summary(mod_diab_educcont_oth))[11,"Std. Error"],coef(summary(mod_diab_educcont_oth11))[11,"Std. Error"])
vi <- as.vector(t((SE_educ_diab*sqrt(n))^2)) #variance is SD^2, SD is SE*sqrt(n)
aou_dat_educ_diab <- data.frame(racecatname,n,ESTIM_educ_diab,vi,SE_educ_diab)
res_educ_diab <- rma(yi=ESTIM_educ_diab,sei=SE_educ_diab, data=aou_dat_educ_diab)
res_nonhw_educ_diab <- rma(yi=ESTIM_educ_diab,sei=SE_educ_diab, data=aou_dat_educ_diab[!racecatname=='NHW',])
res_nonhb_educ_diab <- rma(yi=ESTIM_educ_diab,sei=SE_educ_diab, data=aou_dat_educ_diab[!racecatname=='NHB',])
res_nomex_educ_diab <- rma(yi=ESTIM_educ_diab,sei=SE_educ_diab, data=aou_dat_educ_diab[!racecatname=='Mexican-American',])
res_nohis_educ_diab <- rma(yi=ESTIM_educ_diab,sei=SE_educ_diab, data=aou_dat_educ_diab[!racecatname=='Other Hispanic',])
res_nonha_educ_diab <- rma(yi=ESTIM_educ_diab,sei=SE_educ_diab, data=aou_dat_educ_diab[!racecatname=='NHAsian',])
res_nooth_educ_diab <- rma(yi=ESTIM_educ_diab,sei=SE_educ_diab, data=aou_dat_educ_diab[!racecatname=='Multiracial',])
res_nooth11_educ_diab <- rma(yi=ESTIM_educ_diab,sei=SE_educ_diab, data=aou_dat_educ_diab[!racecatname=='Other',])
res_nonhwnh_educ_diaba <- rma(yi=ESTIM_educ_diab,sei=SE_educ_diab, data=aou_dat_educ_diab[!racecatname=='NHW' & !racecatname=='NHAsian',])
res_nhwnha_educ_diab <- rma(yi=ESTIM_educ_diab,sei=SE_educ_diab, data=aou_dat_educ_diab[racecatname=='NHW' | racecatname=='NHAsian',])
i2_educ_diab <- rbind(res_educ_diab$I2, res_nonhw_educ_diab$I2, res_nonhb_educ_diab$I2, res_nomex_educ_diab$I2, res_nohis_educ_diab$I2, res_nonha_educ_diab$I2, res_nooth_educ_diab$I2, res_nooth11_educ_diab$I2)
i2_educ_diab
influence(res_educ_diab, progbar=F)

###calc I2 for education -> obesity prevalence
yi <- as.vector(t(mod_alldx_educcont_allrace[11,c(45,49,53,57,61,65,69)]))
n <- as.vector(t(table(aou[aou$age >= 18 & !aou$insur_stability=='Missing',]$racecat)))
racecatname <-rownames(table(aou[aou$age >= 18 & !aou$insur_stability=='Missing',]$racecat))
sei <- as.vector(t(( mod_alldx_educcont_allrace[11,c(47,51,55,59,63,67,71)] - mod_alldx_educcont_allrace[11,c(46,50,54,58,62,66,69)] ) / 3.92))
vi <- as.vector(t((sei*sqrt(n))^2)) #variance is SD^2, SD is SE*sqrt(n)
aou_dat <- data.frame(racecatname,n,yi,vi,sei)
aou_dat$log_yi <- log(yi, base=exp(1))
res_educ_ob <- rma(yi=log_yi,sei=sei, data=aou_dat)
res_nonhw_educ_ob <- rma(yi=log_yi,sei=sei, data=aou_dat[!racecatname=='NHW',])
res_nonhb_educ_ob <- rma(yi=log_yi,sei=sei, data=aou_dat[!racecatname=='NHB',])
res_nomex_educ_ob <- rma(yi=log_yi,sei=sei, data=aou_dat[!racecatname=='Mexican-American',])
res_nohis_educ_ob <- rma(yi=log_yi,sei=sei, data=aou_dat[!racecatname=='Other Hispanic',])
res_nonha_educ_ob <- rma(yi=log_yi,sei=sei, data=aou_dat[!racecatname=='NHAsian',])
res_nooth_educ_ob <- rma(yi=log_yi,sei=sei, data=aou_dat[!racecatname=='Multiracial',])
res_nooth11_educ_ob <- rma(yi=log_yi,sei=sei, data=aou_dat[!racecatname=='Other',])
i2_educ_ob <- rbind(res_educ_ob$I2, res_nonhw_educ_ob$I2, res_nonhb_educ_ob$I2, res_nomex_educ_ob$I2, res_nohis_educ_ob$I2, res_nonha_educ_ob$I2, res_nooth_educ_ob$I2, res_nooth11_educ_ob$I2)
#I2 from raw numbers: educ->obes prevalence - USE ME
n <- as.vector(t(table(aou[aou$age >= 18 & !aou$insur_stability=='Missing',]$racecat)))
racecatname <-rownames(table(aou[aou$age >= 18 & !aou$insur_stability=='Missing',]$racecat))
ESTIM_educ_ob <- c(coef(summary(mod_obes_educcont_nhw))[11,"Estimate"],coef(summary(mod_obes_educcont_nhb))[11,"Estimate"],
                   coef(summary(mod_obes_educcont_mex))[11,"Estimate"],coef(summary(mod_obes_educcont_his))[11,"Estimate"],coef(summary(mod_obes_educcont_nha))[11,"Estimate"],
                   coef(summary(mod_obes_educcont_oth))[11,"Estimate"],coef(summary(mod_obes_educcont_oth11))[11,"Estimate"])
SE_educ_ob <- c(coef(summary(mod_obes_educcont_nhw))[11,"Std. Error"],coef(summary(mod_obes_educcont_nhb))[11,"Std. Error"],
                coef(summary(mod_obes_educcont_mex))[11,"Std. Error"],coef(summary(mod_obes_educcont_his))[11,"Std. Error"],coef(summary(mod_obes_educcont_nha))[11,"Std. Error"],
                coef(summary(mod_obes_educcont_oth))[11,"Std. Error"],coef(summary(mod_obes_educcont_oth11))[11,"Std. Error"])
vi <- as.vector(t((SE_educ_ob*sqrt(n))^2)) #variance is SD^2, SD is SE*sqrt(n)
aou_dat_educ_ob <- data.frame(racecatname,n,ESTIM_educ_ob,vi,SE_educ_ob)
res_educ_ob <- rma(yi=ESTIM_educ_ob,sei=SE_educ_ob, data=aou_dat_educ_ob)
res_nonhw_educ_ob <- rma(yi=ESTIM_educ_ob,sei=SE_educ_ob, data=aou_dat_educ_ob[!racecatname=='NHW',])
res_nonhb_educ_ob <- rma(yi=ESTIM_educ_ob,sei=SE_educ_ob, data=aou_dat_educ_ob[!racecatname=='NHB',])
res_nomex_educ_ob <- rma(yi=ESTIM_educ_ob,sei=SE_educ_ob, data=aou_dat_educ_ob[!racecatname=='Mexican-American',])
res_nohis_educ_ob <- rma(yi=ESTIM_educ_ob,sei=SE_educ_ob, data=aou_dat_educ_ob[!racecatname=='Other Hispanic',])
res_nonha_educ_ob <- rma(yi=ESTIM_educ_ob,sei=SE_educ_ob, data=aou_dat_educ_ob[!racecatname=='NHAsian',])
res_nooth_educ_ob <- rma(yi=ESTIM_educ_ob,sei=SE_educ_ob, data=aou_dat_educ_ob[!racecatname=='Multiracial',])
res_nooth11_educ_ob <- rma(yi=ESTIM_educ_ob,sei=SE_educ_ob, data=aou_dat_educ_ob[!racecatname=='Other',])
res_nonhwnh_educ_oba <- rma(yi=ESTIM_educ_ob,sei=SE_educ_ob, data=aou_dat_educ_ob[!racecatname=='NHW' & !racecatname=='NHAsian',])
res_nhwnha_educ_ob <- rma(yi=ESTIM_educ_ob,sei=SE_educ_ob, data=aou_dat_educ_ob[racecatname=='NHW' | racecatname=='NHAsian',])
i2_educ_ob <- rbind(res_educ_ob$I2, res_nonhw_educ_ob$I2, res_nonhb_educ_ob$I2, res_nomex_educ_ob$I2, res_nohis_educ_ob$I2, res_nonha_educ_ob$I2, res_nooth_educ_ob$I2, res_nooth11_educ_ob$I2)
i2_educ_ob
influence(res_educ_ob, progbar=F)

#####interaction - race*educ on diabetes and obesity prevalence#####
mod_diab_raceeducinteract <- glm(data=aou, diab ~ age + female + racecat + smoke_status + insur_stability + educ_num*racecat, family=binomial(link=logit))
summary(mod_diab_raceeducinteract)

mod_obes_raceeducinteract <- glm(data=aou, obese ~ age + female + racecat + smoke_status + insur_stability + educ_num*racecat, family=binomial(link=logit))
summary(mod_obes_raceeducinteract)

mod_diab_raceinctopovinteract <- glm(data=aou, diab ~ age + female + racecat + smoke_status + insur_stability + inc_num*racecat, family=binomial(link=logit))
summary(mod_diab_raceinctopovinteract)

mod_obes_raceinctopovinteract <- glm(data=aou, obese ~ age + female + racecat + smoke_status + insur_stability + inc_num*racecat, family=binomial(link=logit))
summary(mod_obes_raceinctopovinteract)

mod_diabob_racesesinteract <- data.frame(exp(mod_diab_raceeducinteract$coefficients),exp(confint(mod_diab_raceeducinteract)),coef(summary(mod_diab_raceeducinteract))[,"Pr(>|t|)"],
                                         exp(mod_obes_raceeducinteract$coefficients),exp(confint(mod_obes_raceeducinteract)),coef(summary(mod_obes_raceeducinteract))[,"Pr(>|t|)"],
                                         exp(mod_diab_raceinctopovinteract$coefficients),exp(confint(mod_diab_raceinctopovinteract)),coef(summary(mod_diab_raceinctopovinteract))[,"Pr(>|t|)"],
                                         exp(mod_obes_raceinctopovinteract$coefficients),exp(confint(mod_obes_raceinctopovinteract)),coef(summary(mod_obes_raceinctopovinteract))[,"Pr(>|t|)"],stringsAsFactors=F)
mod_diabob_racesesinteract
write.csv(mod_diabob_racesesinteract, 'C:/Users/sarac/OneDrive/Documents/Research/Area v Individual SES/aou_ses_racesesinteract_models.csv',row.names=TRUE)






#####INCOME-TO-POV, ALL DISEASE FOREST#####
mod_diab_incnum_all <- glm(data=aou, diab ~ age + female + smoke_status + insur_stability + inc_num, family=binomial(link=logit))
nobs(mod_diab_incnum_all)
mod_diab_incnum_all_raceadj <- glm(data=aou, diab ~ age + female + smoke_status + insur_stability + inc_num + racecat, family=binomial(link=logit))
mod_diab_incnum_nhw <- glm(data=aou[aou$racecat=='NHW',], diab ~ age + female + smoke_status + insur_stability + inc_num, family=binomial(link=logit))
mod_diab_incnum_nhb <- glm(data=aou[aou$racecat=='NHB',], diab ~ age + female + smoke_status + insur_stability + inc_num, family=binomial(link=logit))
mod_diab_incnum_nha <- glm(data=aou[aou$racecat=='NHAsian',], diab ~ age + female + smoke_status + insur_stability + inc_num, family=binomial(link=logit))
mod_diab_incnum_mex <- glm(data=aou[aou$racecat=='Hispanic',], diab ~ age + female + smoke_status + insur_stability + inc_num, family=binomial(link=logit))
mod_diab_incnum_his <- glm(data=aou[aou$racecat=='Multiracial',], diab ~ age + female + smoke_status + insur_stability + inc_num, family=binomial(link=logit))
mod_diab_incnum_oth <- glm(data=aou[aou$racecat=='Other',], diab ~ age + female + smoke_status + insur_stability + inc_num, family=binomial(link=logit))
mod_diab_incnum_oth11 <- glm(data=aou[aou$racecat=='None of these',], diab ~ age + female + smoke_status + insur_stability + inc_num, family=binomial(link=logit))

mod_diab_incnum_all_bmi <- glm(data=aou, diab ~ age + female + smoke_status + insur_stability + inc_num + bmi, family=binomial(link=logit))
summary(mod_diab_incnum_all_bmi)
exp(mod_diab_incnum_all_bmi$coefficients)
mod_diab_incnum_all_raceadj_bmi <- glm(data=aou, diab ~ age + female + smoke_status + insur_stability + inc_num + bmi + racecat, family=binomial(link=logit))
mod_diab_incnum_nhw_bmi <- glm(data=aou[aou$racecat=='NHW',], diab ~ age + female + smoke_status + insur_stability + inc_num + bmi, family=binomial(link=logit))
mod_diab_incnum_nhb_bmi <- glm(data=aou[aou$racecat=='NHB',], diab ~ age + female + smoke_status + insur_stability + inc_num + bmi, family=binomial(link=logit))
summary(mod_diab_incnum_nhb_bmi)
exp(mod_diab_incnum_nhb_bmi$coefficients)
mod_diab_incnum_nha_bmi <- glm(data=aou[aou$racecat=='NHAsian',], diab ~ age + female + smoke_status + insur_stability + inc_num + bmi, family=binomial(link=logit))
mod_diab_incnum_mex_bmi <- glm(data=aou[aou$racecat=='Hispanic',], diab ~ age + female + smoke_status + insur_stability + inc_num + bmi, family=binomial(link=logit))
mod_diab_incnum_his_bmi <- glm(data=aou[aou$racecat=='Multiracial',], diab ~ age + female + smoke_status + insur_stability + inc_num + bmi + bmi, family=binomial(link=logit))
mod_diab_incnum_oth_bmi <- glm(data=aou[aou$racecat=='Other',], diab ~ age + female + smoke_status + insur_stability + inc_num + bmi, family=binomial(link=logit))
mod_diab_incnum_oth11_bmi <- glm(data=aou[aou$racecat=='None of these',], diab ~ age + female + smoke_status + insur_stability + inc_num + bmi, family=binomial(link=logit))
b <- cbind(exp(mod_diab_incnum_all_bmi$coefficients),exp(mod_diab_incnum_all_raceadj_bmi$coefficients),exp(mod_diab_incnum_nhw_bmi$coefficients),
           exp(mod_diab_incnum_nhb_bmi$coefficients),exp(mod_diab_incnum_mex_bmi$coefficients),exp(mod_diab_incnum_his_bmi$coefficients),
           exp(mod_diab_incnum_nha_bmi$coefficients),exp(mod_diab_incnum_oth_bmi$coefficients),exp(mod_diab_incnum_oth11_bmi$coefficients))



mod_obes_incnum_all <- glm(data=aou, obese ~ age + female + smoke_status + insur_stability + inc_num, family=binomial(link=logit))
mod_obes_incnum_all_raceadj <- glm(data=aou, obese ~ age + female + smoke_status + insur_stability + inc_num + racecat, family=binomial(link=logit))
mod_obes_incnum_nhw <- glm(data=aou[aou$racecat=='NHW',], obese ~ age + female + smoke_status + insur_stability + inc_num, family=binomial(link=logit))
mod_obes_incnum_nhb <- glm(data=aou[aou$racecat=='NHB',], obese ~ age + female + smoke_status + insur_stability + inc_num, family=binomial(link=logit))
mod_obes_incnum_nha <- glm(data=aou[aou$racecat=='NHAsian',], obese ~ age + female + smoke_status + insur_stability + inc_num, family=binomial(link=logit))
mod_obes_incnum_mex <- glm(data=aou[aou$racecat=='Hispanic',], obese ~ age + female + smoke_status + insur_stability + inc_num, family=binomial(link=logit))
mod_obes_incnum_his <- glm(data=aou[aou$racecat=='Multiracial',], obese ~ age + female + smoke_status + insur_stability + inc_num, family=binomial(link=logit))
mod_obes_incnum_oth <- glm(data=aou[aou$racecat=='Other',], obese ~ age + female + smoke_status + insur_stability + inc_num, family=binomial(link=logit))
mod_obes_incnum_oth11 <- glm(data=aou[aou$racecat=='None of these',], obese ~ age + female + smoke_status + insur_stability + inc_num, family=binomial(link=logit))
#leave-one-out OR: 
surv_nonhw <- subset(surv_all, racecat=='NHW')#svydesign(id = ~SDMVPSU, strata = ~ SDMVSTRA, weights = ~weights, data=aou[aou$age >= 18 & !aou$racecat=='NHW' & !aou$insur_stability=='Missing',], nest=T)
mod_obes_incnum_nonhw <- svyglm(design = surv_nonhw, obese ~ age + female + smoke_status + insur_stability + inc_num, family=binomial(link=logit))
mod_obese_incnum_leaveoneout_nonhw <- data.frame(exp(mod_obes_incnum_all$coefficients),exp(confint(mod_obes_incnum_all)),coef(summary(mod_obes_incnum_all))[,"Pr(>|t|)"],
                                                 exp(mod_obes_incnum_nhw$coefficients),exp(confint(mod_obes_incnum_nhw)),coef(summary(mod_obes_incnum_nhw))[,"Pr(>|t|)"],
                                                 exp(mod_obes_incnum_nonhw$coefficients),exp(confint(mod_obes_incnum_nonhw)),coef(summary(mod_obes_incnum_nonhw))[,"Pr(>|t|)"],stringsAsFactors = FALSE)
mod_obese_incnum_leaveoneout_nonhw[11,]

mod_htn_incnum_all <- glm(data=aou, htn ~ age + female + smoke_status + insur_stability + inc_num, family=binomial(link=logit))
mod_htn_incnum_all_raceadj <- glm(data=aou, htn ~ age + female + smoke_status + insur_stability + inc_num + racecat, family=binomial(link=logit))
mod_htn_incnum_nhw <- glm(data=aou[aou$racecat=='NHW',], htn ~ age + female + smoke_status + insur_stability + inc_num, family=binomial(link=logit))
mod_htn_incnum_nhb <- glm(data=aou[aou$racecat=='NHB',], htn ~ age + female + smoke_status + insur_stability + inc_num, family=binomial(link=logit))
mod_htn_incnum_nha <- glm(data=aou[aou$racecat=='NHAsian',], htn ~ age + female + smoke_status + insur_stability + inc_num, family=binomial(link=logit))
mod_htn_incnum_mex <- glm(data=aou[aou$racecat=='Hispanic',], htn ~ age + female + smoke_status + insur_stability + inc_num, family=binomial(link=logit))
mod_htn_incnum_his <- glm(data=aou[aou$racecat=='Multiracial',], htn ~ age + female + smoke_status + insur_stability + inc_num, family=binomial(link=logit))
mod_htn_incnum_oth <- glm(data=aou[aou$racecat=='Other',], htn ~ age + female + smoke_status + insur_stability + inc_num, family=binomial(link=logit))
mod_htn_incnum_oth11 <- glm(data=aou[aou$racecat=='None of these',], htn ~ age + female + smoke_status + insur_stability + inc_num, family=binomial(link=logit))

mod_cvd_incnum_all <- glm(data=aou, cvd ~ age + female + smoke_status + insur_stability + inc_num, family=binomial(link=logit))
mod_cvd_incnum_all_raceadj <- glm(data=aou, cvd ~ age + female + smoke_status + insur_stability + inc_num + racecat, family=binomial(link=logit))
mod_cvd_incnum_nhw <- glm(data=aou[aou$racecat=='NHW',], cvd ~ age + female + smoke_status + insur_stability + inc_num, family=binomial(link=logit))
mod_cvd_incnum_nhb <- glm(data=aou[aou$racecat=='NHB',], cvd ~ age + female + smoke_status + insur_stability + inc_num, family=binomial(link=logit))
mod_cvd_incnum_nha <- glm(data=aou[aou$racecat=='NHAsian',], cvd ~ age + female + smoke_status + insur_stability + inc_num, family=binomial(link=logit))
mod_cvd_incnum_mex <- glm(data=aou[aou$racecat=='Hispanic',], cvd ~ age + female + smoke_status + insur_stability + inc_num, family=binomial(link=logit))
mod_cvd_incnum_his <- glm(data=aou[aou$racecat=='Multiracial',], cvd ~ age + female + smoke_status + insur_stability + inc_num, family=binomial(link=logit))
mod_cvd_incnum_oth <- glm(data=aou[aou$racecat=='Other',], cvd ~ age + female + smoke_status + insur_stability + inc_num, family=binomial(link=logit))
mod_cvd_incnum_oth11 <- glm(data=aou[aou$racecat=='None of these',], cvd ~ age + female + smoke_status + insur_stability + inc_num, family=binomial(link=logit))

mod_ckd_incnum_all <- glm(data=aou, ckd ~ age + female + smoke_status + insur_stability + inc_num, family=binomial(link=logit))
mod_ckd_incnum_all_raceadj <- glm(data=aou, ckd ~ age + female + smoke_status + insur_stability + inc_num + racecat, family=binomial(link=logit))
mod_ckd_incnum_nhw <- glm(data=aou[aou$racecat=='NHW',], ckd ~ age + female + smoke_status + insur_stability + inc_num, family=binomial(link=logit))
mod_ckd_incnum_nhb <- glm(data=aou[aou$racecat=='NHB',], ckd ~ age + female + smoke_status + insur_stability + inc_num, family=binomial(link=logit))
mod_ckd_incnum_nha <- glm(data=aou[aou$racecat=='NHAsian',], ckd ~ age + female + smoke_status + insur_stability + inc_num, family=binomial(link=logit))
mod_ckd_incnum_mex <- glm(data=aou[aou$racecat=='Hispanic',], ckd ~ age + female + smoke_status + insur_stability + inc_num, family=binomial(link=logit))
mod_ckd_incnum_his <- glm(data=aou[aou$racecat=='Multiracial',], ckd ~ age + female + smoke_status + insur_stability + inc_num, family=binomial(link=logit))
mod_ckd_incnum_oth <- glm(data=aou[aou$racecat=='Other',], ckd ~ age + female + smoke_status + insur_stability + inc_num, family=binomial(link=logit))
mod_ckd_incnum_oth11 <- glm(data=aou[aou$racecat=='None of these',], ckd ~ age + female + smoke_status + insur_stability + inc_num, family=binomial(link=logit))

mod_copd_incnum_all <- glm(data=aou, copd ~ age + female + smoke_status + insur_stability + inc_num, family=binomial(link=logit))
mod_copd_incnum_all_raceadj <- glm(data=aou, copd ~ age + female + smoke_status + insur_stability + inc_num + racecat, family=binomial(link=logit))
mod_copd_incnum_nhw <- glm(data=aou[aou$racecat=='NHW',], copd ~ age + female + smoke_status + insur_stability + inc_num, family=binomial(link=logit))
mod_copd_incnum_nhb <- glm(data=aou[aou$racecat=='NHB',], copd ~ age + female + smoke_status + insur_stability + inc_num, family=binomial(link=logit))
mod_copd_incnum_nha <- glm(data=aou[aou$racecat=='NHAsian',], copd ~ age + female + smoke_status + insur_stability + inc_num, family=binomial(link=logit))
mod_copd_incnum_mex <- glm(data=aou[aou$racecat=='Hispanic',], copd ~ age + female + smoke_status + insur_stability + inc_num, family=binomial(link=logit))
mod_copd_incnum_his <- glm(data=aou[aou$racecat=='Multiracial',], copd ~ age + female + smoke_status + insur_stability + inc_num, family=binomial(link=logit))
mod_copd_incnum_oth <- glm(data=aou[aou$racecat=='Other',], copd ~ age + female + smoke_status + insur_stability + inc_num, family=binomial(link=logit))
mod_copd_incnum_oth11 <- glm(data=aou[aou$racecat=='None of these',], copd ~ age + female + smoke_status + insur_stability + inc_num, family=binomial(link=logit))

mod_mdd_incnum_all <- glm(data=aou, mdd ~ age + female + smoke_status + insur_stability + inc_num, family=binomial(link=logit))
mod_mdd_incnum_all_raceadj <- glm(data=aou, mdd ~ age + female + smoke_status + insur_stability + inc_num + racecat, family=binomial(link=logit))
mod_mdd_incnum_nhw <- glm(data=aou[aou$racecat=='NHW',], mdd ~ age + female + smoke_status + insur_stability + inc_num, family=binomial(link=logit))
mod_mdd_incnum_nhb <- glm(data=aou[aou$racecat=='NHB',], mdd ~ age + female + smoke_status + insur_stability + inc_num, family=binomial(link=logit))
mod_mdd_incnum_nha <- glm(data=aou[aou$racecat=='NHAsian',], mdd ~ age + female + smoke_status + insur_stability + inc_num, family=binomial(link=logit))
mod_mdd_incnum_mex <- glm(data=aou[aou$racecat=='Hispanic',], mdd ~ age + female + smoke_status + insur_stability + inc_num, family=binomial(link=logit))
mod_mdd_incnum_his <- glm(data=aou[aou$racecat=='Multiracial',], mdd ~ age + female + smoke_status + insur_stability + inc_num, family=binomial(link=logit))
mod_mdd_incnum_oth <- glm(data=aou[aou$racecat=='Other',], mdd ~ age + female + smoke_status + insur_stability + inc_num, family=binomial(link=logit))
mod_mdd_incnum_oth11 <- glm(data=aou[aou$racecat=='None of these',], mdd ~ age + female + smoke_status + insur_stability + inc_num, family=binomial(link=logit))

mod_alldx_incnum_allrace <- data.frame(exp(mod_diab_incnum_all$coefficients),exp(confint(mod_diab_incnum_all)),coef(summary(mod_diab_incnum_all))[,"Pr(>|t|)"],
                                       exp(mod_diab_incnum_all_raceadj$coefficients)[1:11],exp(confint(mod_diab_incnum_all_raceadj))[1:11,1:2],coef(summary(mod_diab_incnum_all_raceadj))[,"Pr(>|t|)"][1:11],
                                       exp(mod_diab_incnum_nhw$coefficients),exp(confint(mod_diab_incnum_nhw)),coef(summary(mod_diab_incnum_nhw))[,"Pr(>|t|)"],
                                       exp(mod_diab_incnum_nhb$coefficients),exp(confint(mod_diab_incnum_nhb)),coef(summary(mod_diab_incnum_nhb))[,"Pr(>|t|)"],
                                       exp(mod_diab_incnum_mex$coefficients),exp(confint(mod_diab_incnum_mex)),coef(summary(mod_diab_incnum_mex))[,"Pr(>|t|)"],
                                       exp(mod_diab_incnum_his$coefficients),exp(confint(mod_diab_incnum_his)),coef(summary(mod_diab_incnum_his))[,"Pr(>|t|)"],
                                       exp(mod_diab_incnum_nha$coefficients),exp(confint(mod_diab_incnum_nha)),coef(summary(mod_diab_incnum_nha))[,"Pr(>|t|)"],
                                       exp(mod_diab_incnum_oth$coefficients),exp(confint(mod_diab_incnum_oth)),coef(summary(mod_diab_incnum_oth))[,"Pr(>|t|)"],
                                       exp(mod_diab_incnum_oth11$coefficients),exp(confint(mod_diab_incnum_oth11)),coef(summary(mod_diab_incnum_oth11))[,"Pr(>|t|)"],
                                       exp(mod_obes_incnum_all$coefficients),exp(confint(mod_obes_incnum_all)),coef(summary(mod_obes_incnum_all))[,"Pr(>|t|)"],
                                       exp(mod_obes_incnum_all_raceadj$coefficients)[1:11],exp(confint(mod_obes_incnum_all_raceadj))[1:11,1:2],coef(summary(mod_obes_incnum_all_raceadj))[,"Pr(>|t|)"][1:11],
                                       exp(mod_obes_incnum_nhw$coefficients),exp(confint(mod_obes_incnum_nhw)),coef(summary(mod_obes_incnum_nhw))[,"Pr(>|t|)"],
                                       exp(mod_obes_incnum_nhb$coefficients),exp(confint(mod_obes_incnum_nhb)),coef(summary(mod_obes_incnum_nhb))[,"Pr(>|t|)"],
                                       exp(mod_obes_incnum_mex$coefficients),exp(confint(mod_obes_incnum_mex)),coef(summary(mod_obes_incnum_mex))[,"Pr(>|t|)"],
                                       exp(mod_obes_incnum_his$coefficients),exp(confint(mod_obes_incnum_his)),coef(summary(mod_obes_incnum_his))[,"Pr(>|t|)"],
                                       exp(mod_obes_incnum_nha$coefficients),exp(confint(mod_obes_incnum_nha)),coef(summary(mod_obes_incnum_nha))[,"Pr(>|t|)"],
                                       exp(mod_obes_incnum_oth$coefficients),exp(confint(mod_obes_incnum_oth)),coef(summary(mod_obes_incnum_oth))[,"Pr(>|t|)"],
                                       exp(mod_obes_incnum_oth11$coefficients),exp(confint(mod_obes_incnum_oth11)),coef(summary(mod_obes_incnum_oth11))[,"Pr(>|t|)"],
                                       exp(mod_htn_incnum_all$coefficients),exp(confint(mod_htn_incnum_all)),coef(summary(mod_htn_incnum_all))[,"Pr(>|t|)"],
                                       exp(mod_htn_incnum_all_raceadj$coefficients)[1:11],exp(confint(mod_htn_incnum_all_raceadj))[1:11,1:2],coef(summary(mod_htn_incnum_all_raceadj))[,"Pr(>|t|)"][1:11],
                                       exp(mod_htn_incnum_nhw$coefficients),exp(confint(mod_htn_incnum_nhw)),coef(summary(mod_htn_incnum_nhw))[,"Pr(>|t|)"],
                                       exp(mod_htn_incnum_nhb$coefficients),exp(confint(mod_htn_incnum_nhb)),coef(summary(mod_htn_incnum_nhb))[,"Pr(>|t|)"],
                                       exp(mod_htn_incnum_mex$coefficients),exp(confint(mod_htn_incnum_mex)),coef(summary(mod_htn_incnum_mex))[,"Pr(>|t|)"],
                                       exp(mod_htn_incnum_his$coefficients),exp(confint(mod_htn_incnum_his)),coef(summary(mod_htn_incnum_his))[,"Pr(>|t|)"],
                                       exp(mod_htn_incnum_nha$coefficients),exp(confint(mod_htn_incnum_nha)),coef(summary(mod_htn_incnum_nha))[,"Pr(>|t|)"],                                         exp(mod_htn_incnum_oth$coefficients),exp(confint(mod_htn_incnum_oth)),coef(summary(mod_htn_incnum_oth))[,"Pr(>|t|)"],
                                       exp(mod_htn_incnum_oth11$coefficients),exp(confint(mod_htn_incnum_oth11)),coef(summary(mod_htn_incnum_oth11))[,"Pr(>|t|)"],
                                       exp(mod_cvd_incnum_all$coefficients),exp(confint(mod_cvd_incnum_all)),coef(summary(mod_cvd_incnum_all))[,"Pr(>|t|)"],
                                       exp(mod_cvd_incnum_all_raceadj$coefficients)[1:11],exp(confint(mod_cvd_incnum_all_raceadj))[1:11,1:2],coef(summary(mod_cvd_incnum_all_raceadj))[,"Pr(>|t|)"][1:11],
                                       exp(mod_cvd_incnum_nhw$coefficients),exp(confint(mod_cvd_incnum_nhw)),coef(summary(mod_cvd_incnum_nhw))[,"Pr(>|t|)"],
                                       exp(mod_cvd_incnum_nhb$coefficients),exp(confint(mod_cvd_incnum_nhb)),coef(summary(mod_cvd_incnum_nhb))[,"Pr(>|t|)"],
                                       exp(mod_cvd_incnum_mex$coefficients),exp(confint(mod_cvd_incnum_mex)),coef(summary(mod_cvd_incnum_mex))[,"Pr(>|t|)"],
                                       exp(mod_cvd_incnum_his$coefficients),exp(confint(mod_cvd_incnum_his)),coef(summary(mod_cvd_incnum_his))[,"Pr(>|t|)"],
                                       exp(mod_cvd_incnum_nha$coefficients),exp(confint(mod_cvd_incnum_nha)),coef(summary(mod_cvd_incnum_nha))[,"Pr(>|t|)"],
                                       exp(mod_cvd_incnum_oth$coefficients),exp(confint(mod_cvd_incnum_oth)),coef(summary(mod_cvd_incnum_oth))[,"Pr(>|t|)"],
                                       exp(mod_cvd_incnum_oth11$coefficients),exp(confint(mod_cvd_incnum_oth11)),coef(summary(mod_cvd_incnum_oth11))[,"Pr(>|t|)"],
                                       exp(mod_ckd_incnum_all$coefficients),exp(confint(mod_ckd_incnum_all)),coef(summary(mod_ckd_incnum_all))[,"Pr(>|t|)"],
                                       exp(mod_ckd_incnum_all_raceadj$coefficients)[1:11],exp(confint(mod_ckd_incnum_all_raceadj))[1:11,1:2],coef(summary(mod_ckd_incnum_all_raceadj))[,"Pr(>|t|)"][1:11],
                                       exp(mod_ckd_incnum_nhw$coefficients),exp(confint(mod_ckd_incnum_nhw)),coef(summary(mod_ckd_incnum_nhw))[,"Pr(>|t|)"],
                                       exp(mod_ckd_incnum_nhb$coefficients),exp(confint(mod_ckd_incnum_nhb)),coef(summary(mod_ckd_incnum_nhb))[,"Pr(>|t|)"],
                                       exp(mod_ckd_incnum_mex$coefficients),exp(confint(mod_ckd_incnum_mex)),coef(summary(mod_ckd_incnum_mex))[,"Pr(>|t|)"],
                                       exp(mod_ckd_incnum_his$coefficients),exp(confint(mod_ckd_incnum_his)),coef(summary(mod_ckd_incnum_his))[,"Pr(>|t|)"],
                                       exp(mod_ckd_incnum_nha$coefficients),exp(confint(mod_ckd_incnum_nha)),coef(summary(mod_ckd_incnum_nha))[,"Pr(>|t|)"],
                                       exp(mod_ckd_incnum_oth$coefficients),exp(confint(mod_ckd_incnum_oth)),coef(summary(mod_ckd_incnum_oth))[,"Pr(>|t|)"],
                                       exp(mod_ckd_incnum_oth11$coefficients),exp(confint(mod_ckd_incnum_oth11)),coef(summary(mod_ckd_incnum_oth11))[,"Pr(>|t|)"],
                                       exp(mod_copd_incnum_all$coefficients),exp(confint(mod_copd_incnum_all)),coef(summary(mod_copd_incnum_all))[,"Pr(>|t|)"],
                                       exp(mod_copd_incnum_all_raceadj$coefficients)[1:11],exp(confint(mod_copd_incnum_all_raceadj))[1:11,1:2],coef(summary(mod_copd_incnum_all_raceadj))[,"Pr(>|t|)"][1:11],
                                       exp(mod_copd_incnum_nhw$coefficients),exp(confint(mod_copd_incnum_nhw)),coef(summary(mod_copd_incnum_nhw))[,"Pr(>|t|)"],
                                       exp(mod_copd_incnum_nhb$coefficients),exp(confint(mod_copd_incnum_nhb)),coef(summary(mod_copd_incnum_nhb))[,"Pr(>|t|)"],
                                       exp(mod_copd_incnum_mex$coefficients),exp(confint(mod_copd_incnum_mex)),coef(summary(mod_copd_incnum_mex))[,"Pr(>|t|)"],
                                       exp(mod_copd_incnum_his$coefficients),exp(confint(mod_copd_incnum_his)),coef(summary(mod_copd_incnum_his))[,"Pr(>|t|)"],
                                       exp(mod_copd_incnum_nha$coefficients),exp(confint(mod_copd_incnum_nha)),coef(summary(mod_copd_incnum_nha))[,"Pr(>|t|)"],
                                       exp(mod_copd_incnum_oth$coefficients),exp(confint(mod_copd_incnum_oth)),coef(summary(mod_copd_incnum_oth))[,"Pr(>|t|)"],
                                       exp(mod_copd_incnum_oth11$coefficients),exp(confint(mod_copd_incnum_oth11)),coef(summary(mod_copd_incnum_oth11))[,"Pr(>|t|)"],
                                       exp(mod_mdd_incnum_all$coefficients),exp(confint(mod_mdd_incnum_all)),coef(summary(mod_mdd_incnum_all))[,"Pr(>|t|)"],
                                       exp(mod_mdd_incnum_all_raceadj$coefficients)[1:11],exp(confint(mod_mdd_incnum_all_raceadj))[1:11,1:2],coef(summary(mod_mdd_incnum_all_raceadj))[,"Pr(>|t|)"][1:11],
                                       exp(mod_mdd_incnum_nhw$coefficients),exp(confint(mod_mdd_incnum_nhw)),coef(summary(mod_mdd_incnum_nhw))[,"Pr(>|t|)"],
                                       exp(mod_mdd_incnum_nhb$coefficients),exp(confint(mod_mdd_incnum_nhb)),coef(summary(mod_mdd_incnum_nhb))[,"Pr(>|t|)"],
                                       exp(mod_mdd_incnum_mex$coefficients),exp(confint(mod_mdd_incnum_mex)),coef(summary(mod_mdd_incnum_mex))[,"Pr(>|t|)"],
                                       exp(mod_mdd_incnum_his$coefficients),exp(confint(mod_mdd_incnum_his)),coef(summary(mod_mdd_incnum_his))[,"Pr(>|t|)"],
                                       exp(mod_mdd_incnum_nha$coefficients),exp(confint(mod_mdd_incnum_nha)),coef(summary(mod_mdd_incnum_nha))[,"Pr(>|t|)"],
                                       exp(mod_mdd_incnum_oth$coefficients),exp(confint(mod_mdd_incnum_oth)),coef(summary(mod_mdd_incnum_oth))[,"Pr(>|t|)"],
                                       exp(mod_mdd_incnum_oth11$coefficients),exp(confint(mod_mdd_incnum_oth11)),coef(summary(mod_mdd_incnum_oth11))[,"Pr(>|t|)"],stringsAsFactors = FALSE)
mod_alldx_incnum_allrace_transpose <- t(mod_alldx_incnum_allrace)


tiff(file='C:/Users/sarac/OneDrive/Documents/Research/Area v Individual SES/aou_forest_indiv_incnum_stratandnonstrat_alldx.tiff', width=8, height=5, units='in', res=300)
forestplot(c('Type 2 Diabetes','Obesity','Hypertension','CVD','CKD','COPD','Depression'), 
           mean = cbind(t(mod_alldx_incnum_allrace[11,c(1,37,73,109,145,181,217)]),t(mod_alldx_incnum_allrace[11,c(5,41,77,113,149,185,221)]),
                        t(mod_alldx_incnum_allrace[11,c(9,45,81,117,153,189,225)]),t(mod_alldx_incnum_allrace[11,c(13,49,85,121,157,193,229)]),
                        t(mod_alldx_incnum_allrace[11,c(17,53,89,125,161,197,233)]),t(mod_alldx_incnum_allrace[11,c(21,57,93,129,165,201,237)]),
                        t(mod_alldx_incnum_allrace[11,c(25,61,97,133,169,205,241)]),t(mod_alldx_incnum_allrace[11,c(29,65,101,137,173,209,245)]),
                        t(mod_alldx_incnum_allrace[11,c(33,69,105,141,177,213,249)])),
           lower = cbind(t(mod_alldx_incnum_allrace[11,c(2,38,74,110,146,182,218)]),t(mod_alldx_incnum_allrace[11,c(6,42,78,114,150,186,222)]),
                         t(mod_alldx_incnum_allrace[11,c(10,46,82,118,154,190,226)]),t(mod_alldx_incnum_allrace[11,c(14,50,86,122,158,194,230)]),
                         t(mod_alldx_incnum_allrace[11,c(18,54,90,126,162,198,234)]),t(mod_alldx_incnum_allrace[11,c(22,58,94,130,166,202,238)]),
                         t(mod_alldx_incnum_allrace[11,c(26,62,98,134,170,206,242)]),t(mod_alldx_incnum_allrace[11,c(30,66,102,138,174,210,246)]),
                         t(mod_alldx_incnum_allrace[11,c(34,70,106,142,178,214,250)])),
           upper = cbind(t(mod_alldx_incnum_allrace[11,c(3,39,75,111,147,183,219)]),t(mod_alldx_incnum_allrace[11,c(7,43,79,115,151,187,223)]),
                         t(mod_alldx_incnum_allrace[11,c(11,47,83,119,155,191,227)]),t(mod_alldx_incnum_allrace[11,c(15,51,87,123,159,195,231)]),
                         t(mod_alldx_incnum_allrace[11,c(19,55,91,127,163,199,235)]),t(mod_alldx_incnum_allrace[11,c(23,59,95,131,167,203,239)]),
                         t(mod_alldx_incnum_allrace[11,c(27,63,99,135,171,207,243)]),t(mod_alldx_incnum_allrace[11,c(31,67,103,139,175,211,247)]),
                         t(mod_alldx_incnum_allrace[11,c(35,71,107,143,179,215,251)])),
           col = fpColors(box = c('gray23','gray57','dodgerblue4','darkslategray3','palegreen3','lightgoldenrod','salmon1','indianred3','mediumpurple4')),
           legend=c("All","All, Race-adjusted","NHW","NHB","Mexican-American","Other Hispanic","NHA","Other/Multi-Racial \n including NHA (pre-2011)","Other/Multi-Racial \n (post-2011)"),
           legend_args = fpLegend(pos = "right"),#list(x = 0.5, y = -0.2), gp = gpar(cex=0.9,col = "#CCCCCC", fill = "#F9F9F9")),
           #legend=c("All","All, Race-adjusted",rownames(table(aou$racecat))),#c("All","All-Race-adjusted","NHW","NHB","NHA","Mexican-American","Other Hispanic","Other/Multiracial"),
           #legend_args = fpLegend(pos = list(x = 0.9, y = 0.9), gp = gpar(cex=0.7,col = "#CCCCCC", fill = "#F9F9F9")),
           xlab = "OR (95% CI)",
           title = 'Adjusted OR for Continuous Income (Multiples of $25k) on All Diseases, \n Overall and Stratified by Race/Ethnicity',
           txt_gp = fpTxtGp(title=gpar(cex=0.8),xlab=gpar(cex=0.8), ticks=gpar(cex=0.8), label=gpar(cex=0.8)),
           vertices=TRUE,
           line.margin=0.25,
           graph.pos=2,
           clip=c(0.6,1.4), xlim=c(0.6,1.4), xticks=c(0.6,0.8,1.0,1.2,1.4), xlog=TRUE, grid=structure(c(0.6,0.8,1.0,1.2,1.4)))
dev.off()

tiff(file='C:/Users/sarac/OneDrive/Documents/Research/Area v Individual SES/Fig2B_aou_forest_indiv_incnum_stratandnonstrat.tiff', width=8, height=5, units='in', res=300)
forestplot(c('Type 2 Diabetes','Obesity'), 
           mean = cbind(t(mod_alldx_incnum_allrace[11,c(1,37)]),t(mod_alldx_incnum_allrace[11,c(5,41)]),
                        t(mod_alldx_incnum_allrace[11,c(9,45)]),t(mod_alldx_incnum_allrace[11,c(13,49)]),
                        t(mod_alldx_incnum_allrace[11,c(17,53)]),t(mod_alldx_incnum_allrace[11,c(21,57)]),
                        t(mod_alldx_incnum_allrace[11,c(25,61)]),t(mod_alldx_incnum_allrace[11,c(29,65)]),
                        t(mod_alldx_incnum_allrace[11,c(33,69)])),
           lower = cbind(t(mod_alldx_incnum_allrace[11,c(2,38)]),t(mod_alldx_incnum_allrace[11,c(6,42)]),
                         t(mod_alldx_incnum_allrace[11,c(10,46)]),t(mod_alldx_incnum_allrace[11,c(14,50)]),
                         t(mod_alldx_incnum_allrace[11,c(18,54)]),t(mod_alldx_incnum_allrace[11,c(22,58)]),
                         t(mod_alldx_incnum_allrace[11,c(26,62)]),t(mod_alldx_incnum_allrace[11,c(30,66)]),
                         t(mod_alldx_incnum_allrace[11,c(34,68)])),
           upper = cbind(t(mod_alldx_incnum_allrace[11,c(3,39)]),t(mod_alldx_incnum_allrace[11,c(7,43)]),
                         t(mod_alldx_incnum_allrace[11,c(11,47)]),t(mod_alldx_incnum_allrace[11,c(15,51)]),
                         t(mod_alldx_incnum_allrace[11,c(19,55)]),t(mod_alldx_incnum_allrace[11,c(23,59)]),
                         t(mod_alldx_incnum_allrace[11,c(27,63)]),t(mod_alldx_incnum_allrace[11,c(31,67)]),
                         t(mod_alldx_incnum_allrace[11,c(35,71)])),
           col = fpColors(box = c('gray23','gray57','dodgerblue4','darkslategray3','palegreen3','lightgoldenrod','salmon1','indianred3','mediumpurple4')),
           legend=c("All","All, Race-adjusted","NHW","NHB","Mexican-American","Other Hispanic","NHA","Other/Multi-Racial \n including NHA (pre-2011)","Other/Multi-Racial \n (post-2011)"),
           legend_args = fpLegend(pos = "right"),#list(x = 0.5, y = -0.2), gp = gpar(cex=0.9,col = "#CCCCCC", fill = "#F9F9F9")),
           #legend=c("All","All, Race-adjusted",rownames(table(aou$racecat))),#c("All","All-Race-adjusted","NHW","NHB","NHA","Mexican-American","Other Hispanic","Other/Multiracial"),
           #legend_args = fpLegend(pos = list(x = 0.9, y = 0.9), gp = gpar(cex=0.9,col = "#CCCCCC", fill = "#F9F9F9")),
           xlab = "OR (95% CI)",
           title = 'Adjusted OR for Income (Multiples of $25k) on T2D and Obesity Prevalence, \n Overall and Stratified by Race/Ethnicity',
           txt_gp = fpTxtGp(title=gpar(cex=1),xlab=gpar(cex=1), ticks=gpar(cex=1), label=gpar(cex=1)),
           vertices=TRUE,
           line.margin=0.25,
           graph.pos=2,
           clip=c(0.75,1.2), xlim=c(0.75,1.2), xticks=c(0.75,0.8,0.9,1.0,1.1,1.2), xlog=TRUE, grid=structure(c(0.75,0.8,0.9,1.0,1.1,1.2)))
dev.off()

#t2d (bmi-adj) and obesity only forest
mod_alldx_incnum_allrace_bmiadj <- data.frame(exp(mod_diab_incnum_all_bmi$coefficients)[1:11],exp(confint(mod_diab_incnum_all_bmi))[1:11,1:2],coef(summary(mod_diab_incnum_all_bmi))[,"Pr(>|t|)"][1:11],
                                              exp(mod_diab_incnum_all_raceadj_bmi$coefficients)[1:11],exp(confint(mod_diab_incnum_all_raceadj_bmi))[1:11,1:2],coef(summary(mod_diab_incnum_all_raceadj_bmi))[,"Pr(>|t|)"][1:11],
                                              exp(mod_diab_incnum_nhw_bmi$coefficients)[1:11],exp(confint(mod_diab_incnum_nhw_bmi))[1:11,1:2],coef(summary(mod_diab_incnum_nhw_bmi))[,"Pr(>|t|)"][1:11],
                                              exp(mod_diab_incnum_nhb_bmi$coefficients)[1:11],exp(confint(mod_diab_incnum_nhb_bmi))[1:11,1:2],coef(summary(mod_diab_incnum_nhb_bmi))[,"Pr(>|t|)"][1:11],
                                              exp(mod_diab_incnum_mex_bmi$coefficients)[1:11],exp(confint(mod_diab_incnum_mex_bmi))[1:11,1:2],coef(summary(mod_diab_incnum_mex_bmi))[,"Pr(>|t|)"][1:11],
                                              exp(mod_diab_incnum_his_bmi$coefficients)[1:11],exp(confint(mod_diab_incnum_his_bmi))[1:11,1:2],coef(summary(mod_diab_incnum_his_bmi))[,"Pr(>|t|)"][1:11],
                                              exp(mod_diab_incnum_nha_bmi$coefficients)[1:11],exp(confint(mod_diab_incnum_nha_bmi))[1:11,1:2],coef(summary(mod_diab_incnum_nha_bmi))[,"Pr(>|t|)"][1:11],
                                              exp(mod_diab_incnum_oth_bmi$coefficients)[1:11],exp(confint(mod_diab_incnum_oth_bmi))[1:11,1:2],coef(summary(mod_diab_incnum_oth_bmi))[,"Pr(>|t|)"][1:11],
                                              exp(mod_diab_incnum_oth11_bmi$coefficients)[1:11],exp(confint(mod_diab_incnum_oth11_bmi))[1:11,1:2],coef(summary(mod_diab_incnum_oth11_bmi))[,"Pr(>|t|)"][1:11],
                                              exp(mod_obes_incnum_all$coefficients),exp(confint(mod_obes_incnum_all)),coef(summary(mod_obes_incnum_all))[,"Pr(>|t|)"],
                                              exp(mod_obes_incnum_all_raceadj$coefficients)[1:11],exp(confint(mod_obes_incnum_all_raceadj))[1:11,1:2],coef(summary(mod_obes_incnum_all_raceadj))[,"Pr(>|t|)"][1:11],
                                              exp(mod_obes_incnum_nhw$coefficients),exp(confint(mod_obes_incnum_nhw)),coef(summary(mod_obes_incnum_nhw))[,"Pr(>|t|)"],
                                              exp(mod_obes_incnum_nhb$coefficients),exp(confint(mod_obes_incnum_nhb)),coef(summary(mod_obes_incnum_nhb))[,"Pr(>|t|)"],
                                              exp(mod_obes_incnum_mex$coefficients),exp(confint(mod_obes_incnum_mex)),coef(summary(mod_obes_incnum_mex))[,"Pr(>|t|)"],
                                              exp(mod_obes_incnum_his$coefficients),exp(confint(mod_obes_incnum_his)),coef(summary(mod_obes_incnum_his))[,"Pr(>|t|)"],
                                              exp(mod_obes_incnum_nha$coefficients),exp(confint(mod_obes_incnum_nha)),coef(summary(mod_obes_incnum_nha))[,"Pr(>|t|)"],
                                              exp(mod_obes_incnum_oth$coefficients),exp(confint(mod_obes_incnum_oth)),coef(summary(mod_obes_incnum_oth))[,"Pr(>|t|)"],
                                              exp(mod_obes_incnum_oth11$coefficients),exp(confint(mod_obes_incnum_oth11)),coef(summary(mod_obes_incnum_oth11))[,"Pr(>|t|)"],stringsAsFactors = FALSE)
dim(mod_alldx_incnum_allrace_bmiadj)
tiff(file='C:/Users/sarac/OneDrive/Documents/Research/Area v Individual SES/aou_forest_indiv_incnum_stratandnonstrat_t2dbmiadj.tiff', width=8, height=5, units='in', res=300)
par(xpd=T)
forestplot(c('Type 2 Diabetes','Obesity'), 
           mean = cbind(t(mod_alldx_incnum_allrace_bmiadj[11,c(1,37)]),t(mod_alldx_incnum_allrace_bmiadj[11,c(5,41)]),
                        t(mod_alldx_incnum_allrace_bmiadj[11,c(9,45)]),t(mod_alldx_incnum_allrace_bmiadj[11,c(13,49)]),
                        t(mod_alldx_incnum_allrace_bmiadj[11,c(17,53)]),t(mod_alldx_incnum_allrace_bmiadj[11,c(21,57)]),
                        t(mod_alldx_incnum_allrace_bmiadj[11,c(25,61)]),t(mod_alldx_incnum_allrace_bmiadj[11,c(29,65)]),
                        t(mod_alldx_incnum_allrace_bmiadj[11,c(33,69)])),
           lower = cbind(t(mod_alldx_incnum_allrace_bmiadj[11,c(2,38)]),t(mod_alldx_incnum_allrace_bmiadj[11,c(6,42)]),
                         t(mod_alldx_incnum_allrace_bmiadj[11,c(10,46)]),t(mod_alldx_incnum_allrace_bmiadj[11,c(14,50)]),
                         t(mod_alldx_incnum_allrace_bmiadj[11,c(18,54)]),t(mod_alldx_incnum_allrace_bmiadj[11,c(22,58)]),
                         t(mod_alldx_incnum_allrace_bmiadj[11,c(26,62)]),t(mod_alldx_incnum_allrace_bmiadj[11,c(30,66)]),
                         t(mod_alldx_incnum_allrace_bmiadj[11,c(34,68)])),
           upper = cbind(t(mod_alldx_incnum_allrace_bmiadj[11,c(3,39)]),t(mod_alldx_incnum_allrace_bmiadj[11,c(7,43)]),
                         t(mod_alldx_incnum_allrace_bmiadj[11,c(11,47)]),t(mod_alldx_incnum_allrace_bmiadj[11,c(15,51)]),
                         t(mod_alldx_incnum_allrace_bmiadj[11,c(19,55)]),t(mod_alldx_incnum_allrace_bmiadj[11,c(23,59)]),
                         t(mod_alldx_incnum_allrace_bmiadj[11,c(27,63)]),t(mod_alldx_incnum_allrace_bmiadj[11,c(31,67)]),
                         t(mod_alldx_incnum_allrace_bmiadj[11,c(35,71)])),
           col = fpColors(box = c('gray23','gray57','dodgerblue4','darkslategray3','palegreen3','lightgoldenrod','salmon1','indianred3','mediumpurple4')),
           legend=c("All","All, Race-adjusted","NHW","NHB","Mexican-American","Other Hispanic","NHA","Other/Multi-Racial \n including NHA (pre-2011)","Other/Multi-Racial \n (post-2011)"),
           legend_args = fpLegend(pos = "right"),#list(x = 0.5, y = -0.2), gp = gpar(cex=0.9,col = "#CCCCCC", fill = "#F9F9F9")),
           xlab = "OR (95% CI)",
           title = 'Adjusted OR for Continuous Education on T2D and Obesity Prevalence, \n Overall and Stratified by Race/Ethnicity',
           txt_gp = fpTxtGp(title=gpar(cex=1),xlab=gpar(cex=1), ticks=gpar(cex=1), label=gpar(cex=1)),
           vertices=TRUE,
           line.margin=0.25,
           graph.pos=2,
           clip=c(0.75,1.1), xlim=c(0.75,1.1), xticks=c(0.75,0.8,0.9,1.0,1.1), xlog=TRUE, grid=structure(c(0.75,0.8,0.9,1.0,1.1)))
dev.off()
#t2d - bmi mediation
bmi_med_inc_all <- glm(data=aou, bmi ~ age + female + smoke_status + insur_stability + inc_num)
bmi_med_inc_all_raceadj <- glm(data=aou, bmi ~ age + female + smoke_status + insur_stability + inc_num + racecat)
bmi_med_inc_nhw <- glm(data=aou[aou$racecat=='NHW',], bmi ~ age + female + smoke_status + insur_stability + inc_num)
bmi_med_inc_nhb <- glm(data=aou[aou$racecat=='NHB',], bmi ~ age + female + smoke_status + insur_stability + inc_num)
bmi_med_inc_mex <- glm(data=aou[aou$racecat=='Hispanic',], bmi ~ age + female + smoke_status + insur_stability + inc_num)
bmi_med_inc_his <- glm(data=aou[aou$racecat=='NHAsian',], bmi ~ age + female + smoke_status + insur_stability + inc_num)
bmi_med_inc_nha <- glm(data=aou[aou$racecat=='Multiracial',], bmi ~ age + female + smoke_status + insur_stability + inc_num)
bmi_med_inc_oth <- glm(data=aou[aou$racecat=='Other',], bmi ~ age + female + smoke_status + insur_stability + inc_num)
bmi_med_inc_oth11 <- glm(data=aou[aou$racecat=='None of these',], bmi ~ age + female + smoke_status + insur_stability + inc_num)

t2d_propmediated_bmi_inc <- rbind(cbind('All',mod_diab_incnum_all_bmi$coefficients[11] + bmi_med_inc_all$coefficients[11]*mod_diab_incnum_all_bmi$coefficients[12], mod_diab_incnum_all_bmi$coefficients[11], bmi_med_inc_all$coefficients[11]*mod_diab_incnum_all_bmi$coefficients[12], bmi_med_inc_all$coefficients[11]*mod_diab_incnum_all_bmi$coefficients[12]/mod_diab_incnum_all_bmi$coefficients[11]),
                                  cbind('All-Race-adjusted',mod_diab_incnum_all_raceadj_bmi$coefficients[11] + bmi_med_inc_all_raceadj$coefficients[11]*mod_diab_incnum_all_raceadj_bmi$coefficients[12], mod_diab_incnum_all_raceadj_bmi$coefficients[11], bmi_med_inc_all_raceadj$coefficients[11]*mod_diab_incnum_all_bmi$coefficients[12], bmi_med_inc_all$coefficients[11]*mod_diab_incnum_all_raceadj_bmi$coefficients[12]/mod_diab_incnum_all_raceadj_bmi$coefficients[11]),
                                  cbind('NHW',mod_diab_incnum_nhw_bmi$coefficients[11] + bmi_med_inc_nhw$coefficients[11]*mod_diab_incnum_nhw_bmi$coefficients[12], mod_diab_incnum_nhw_bmi$coefficients[11], bmi_med_inc_nhw$coefficients[11]*mod_diab_incnum_nhw_bmi$coefficients[12], bmi_med_inc_nhw$coefficients[11]*mod_diab_incnum_nhw_bmi$coefficients[12]/mod_diab_incnum_nhw_bmi$coefficients[11]),
                                  cbind('NHB',mod_diab_incnum_nhb_bmi$coefficients[11] + bmi_med_inc_nhb$coefficients[11]*mod_diab_incnum_nhb_bmi$coefficients[12], mod_diab_incnum_nhb_bmi$coefficients[11], bmi_med_inc_nhb$coefficients[11]*mod_diab_incnum_nhb_bmi$coefficients[12], bmi_med_inc_nhb$coefficients[11]*mod_diab_incnum_nhb_bmi$coefficients[12]/mod_diab_incnum_nhb_bmi$coefficients[11]),
                                  cbind('MEX',mod_diab_incnum_mex_bmi$coefficients[11] + bmi_med_inc_mex$coefficients[11]*mod_diab_incnum_mex_bmi$coefficients[12], mod_diab_incnum_mex_bmi$coefficients[11], bmi_med_inc_mex$coefficients[11]*mod_diab_incnum_mex_bmi$coefficients[12], bmi_med_inc_mex$coefficients[11]*mod_diab_incnum_mex_bmi$coefficients[12]/mod_diab_incnum_mex_bmi$coefficients[11]),
                                  cbind('HIS',mod_diab_incnum_his_bmi$coefficients[11] + bmi_med_inc_his$coefficients[11]*mod_diab_incnum_his_bmi$coefficients[12], mod_diab_incnum_his_bmi$coefficients[11], bmi_med_inc_his$coefficients[11]*mod_diab_incnum_his_bmi$coefficients[12], bmi_med_inc_his$coefficients[11]*mod_diab_incnum_his_bmi$coefficients[12]/mod_diab_incnum_his_bmi$coefficients[11]),
                                  cbind('NHA',mod_diab_incnum_nha_bmi$coefficients[11] + bmi_med_inc_nha$coefficients[11]*mod_diab_incnum_nha_bmi$coefficients[12], mod_diab_incnum_nha_bmi$coefficients[11], bmi_med_inc_nha$coefficients[11]*mod_diab_incnum_nha_bmi$coefficients[12], bmi_med_inc_nha$coefficients[11]*mod_diab_incnum_nha_bmi$coefficients[12]/mod_diab_incnum_nha_bmi$coefficients[11]),
                                  cbind('OTH',mod_diab_incnum_oth_bmi$coefficients[11] + bmi_med_inc_oth$coefficients[11]*mod_diab_incnum_oth_bmi$coefficients[12], mod_diab_incnum_oth_bmi$coefficients[11], bmi_med_inc_oth$coefficients[11]*mod_diab_incnum_oth_bmi$coefficients[12], bmi_med_inc_oth$coefficients[11]*mod_diab_incnum_oth_bmi$coefficients[12]/mod_diab_incnum_oth_bmi$coefficients[11]),
                                  cbind('OTH11',mod_diab_incnum_oth11_bmi$coefficients[11] + bmi_med_inc_oth11$coefficients[11]*mod_diab_incnum_oth11_bmi$coefficients[12], mod_diab_incnum_oth11_bmi$coefficients[11], bmi_med_inc_oth11$coefficients[11]*mod_diab_incnum_oth11_bmi$coefficients[12], bmi_med_inc_oth11$coefficients[11]*mod_diab_incnum_oth11_bmi$coefficients[12]/mod_diab_incnum_oth11_bmi$coefficients[11]))
colnames(t2d_propmediated_bmi_inc) <- c('Race/Ethnicity Group','Total Effect','Direct Effect','Indirect Effect','Proportion Mediated')
t2d_propmediated_bmi_inc



#I2 from raw numbers: inc2pov->diab prevalence - USE ME
n <- as.vector(t(table(aou[aou$age >= 18 & !aou$insur_stability=='Missing',]$racecat)))
racecatname <-rownames(table(aou[aou$age >= 18 & !aou$insur_stability=='Missing',]$racecat))
ESTIM_inc_diab <- c(coef(summary(mod_diab_incnum_nhw))[11,"Estimate"],coef(summary(mod_diab_incnum_nhb))[11,"Estimate"],coef(summary(mod_diab_incnum_nha))[11,"Estimate"],
                    coef(summary(mod_diab_incnum_mex))[11,"Estimate"],coef(summary(mod_diab_incnum_his))[11,"Estimate"],coef(summary(mod_diab_incnum_oth))[11,"Estimate"],coef(summary(mod_diab_incnum_oth11))[11,"Estimate"])
SE_inc_diab <- c(coef(summary(mod_diab_incnum_nhw))[11,"Std. Error"],coef(summary(mod_diab_incnum_nhb))[11,"Std. Error"],coef(summary(mod_diab_incnum_nha))[11,"Std. Error"],
                 coef(summary(mod_diab_incnum_mex))[11,"Std. Error"],coef(summary(mod_diab_incnum_his))[11,"Std. Error"],coef(summary(mod_diab_incnum_oth))[11,"Std. Error"],coef(summary(mod_diab_incnum_oth11))[11,"Std. Error"])
vi <- as.vector(t((SE_inc_diab*sqrt(n))^2)) #variance is SD^2, SD is SE*sqrt(n)
aou_dat_inc_diab <- data.frame(racecatname,n,ESTIM_inc_diab,vi,SE_inc_diab)
res_inc_diab <- rma(yi=ESTIM_inc_diab,sei=SE_inc_diab, data=aou_dat_inc_diab)
res_nonhw_inc_diab <- rma(yi=ESTIM_inc_diab,sei=SE_inc_diab, data=aou_dat_inc_diab[!racecatname=='NHW',])
res_nonhb_inc_diab <- rma(yi=ESTIM_inc_diab,sei=SE_inc_diab, data=aou_dat_inc_diab[!racecatname=='NHB',])
res_nomex_inc_diab <- rma(yi=ESTIM_inc_diab,sei=SE_inc_diab, data=aou_dat_inc_diab[!racecatname=='Mexican-American',])
res_nohis_inc_diab <- rma(yi=ESTIM_inc_diab,sei=SE_inc_diab, data=aou_dat_inc_diab[!racecatname=='Other Hispanic',])
res_nonha_inc_diab <- rma(yi=ESTIM_inc_diab,sei=SE_inc_diab, data=aou_dat_inc_diab[!racecatname=='NHAsian',])
res_nooth_inc_diab <- rma(yi=ESTIM_inc_diab,sei=SE_inc_diab, data=aou_dat_inc_diab[!racecatname=='Multiracial',])
res_nooth11_inc_diab <- rma(yi=ESTIM_inc_diab,sei=SE_inc_diab, data=aou_dat_inc_diab[!racecatname=='Other',])
res_nonhwnh_inc_diaba <- rma(yi=ESTIM_inc_diab,sei=SE_inc_diab, data=aou_dat_inc_diab[!racecatname=='NHW' & !racecatname=='NHAsian',])
res_nhwnha_inc_diab <- rma(yi=ESTIM_inc_diab,sei=SE_inc_diab, data=aou_dat_inc_diab[racecatname=='NHW' | racecatname=='NHAsian',])
i2_inc_diab <- rbind(res_inc_diab$I2, res_nonhw_inc_diab$I2, res_nonhb_inc_diab$I2, res_nomex_inc_diab$I2, res_nohis_inc_diab$I2, res_nonha_inc_diab$I2, res_nooth_inc_diab$I2, res_nooth11_inc_diab$I2)
i2_inc_diab
influence(res_inc_diab, progbar=F)


###calc I2 for inc2pov -> obesity prevalence
#I2 from raw numbers - USE ME:
n <- as.vector(t(table(aou[aou$age >= 18 & !aou$insur_stability=='Missing',]$racecat)))
racecatname <-rownames(table(aou[aou$age >= 18 & !aou$insur_stability=='Missing',]$racecat))
ESTIM_inc_ob <- c(coef(summary(mod_obes_incnum_nhw))[11,"Estimate"],coef(summary(mod_obes_incnum_nhb))[11,"Estimate"],
                  coef(summary(mod_obes_incnum_mex))[11,"Estimate"],coef(summary(mod_obes_incnum_his))[11,"Estimate"],coef(summary(mod_obes_incnum_nha))[11,"Estimate"],
                  coef(summary(mod_obes_incnum_oth))[11,"Estimate"],coef(summary(mod_obes_incnum_oth11))[11,"Estimate"])
SE_inc_ob <- c(coef(summary(mod_obes_incnum_nhw))[11,"Std. Error"],coef(summary(mod_obes_incnum_nhb))[11,"Std. Error"],
               coef(summary(mod_obes_incnum_mex))[11,"Std. Error"],coef(summary(mod_obes_incnum_his))[11,"Std. Error"],coef(summary(mod_obes_incnum_nha))[11,"Std. Error"],
               coef(summary(mod_obes_incnum_oth))[11,"Std. Error"],coef(summary(mod_obes_incnum_oth11))[11,"Std. Error"])
vi <- as.vector(t((SE_inc_ob*sqrt(n))^2)) #variance is SD^2, SD is SE*sqrt(n)
aou_dat_inc_ob <- data.frame(racecatname,n,ESTIM_inc_ob,vi,SE_inc_ob)
res_inc_ob <- rma(yi=ESTIM_inc_ob,sei=SE_inc_ob, data=aou_dat_inc_ob)
res_nonhw_inc_ob <- rma(yi=ESTIM_inc_ob,sei=SE_inc_ob, data=aou_dat_inc_ob[!racecatname=='NHW',])
res_nonhb_inc_ob <- rma(yi=ESTIM_inc_ob,sei=SE_inc_ob, data=aou_dat_inc_ob[!racecatname=='NHB',])
res_nomex_inc_ob <- rma(yi=ESTIM_inc_ob,sei=SE_inc_ob, data=aou_dat_inc_ob[!racecatname=='Mexican-American',])
res_nohis_inc_ob <- rma(yi=ESTIM_inc_ob,sei=SE_inc_ob, data=aou_dat_inc_ob[!racecatname=='Other Hispanic',])
res_nonha_inc_ob <- rma(yi=ESTIM_inc_ob,sei=SE_inc_ob, data=aou_dat_inc_ob[!racecatname=='NHAsian',])
res_nooth_inc_ob <- rma(yi=ESTIM_inc_ob,sei=SE_inc_ob, data=aou_dat_inc_ob[!racecatname=='Multiracial',])
res_nooth11_inc_ob <- rma(yi=ESTIM_inc_ob,sei=SE_inc_ob, data=aou_dat_inc_ob[!racecatname=='Other',])
res_nonhwnh_inc_oba <- rma(yi=ESTIM_inc_ob,sei=SE_inc_ob, data=aou_dat_inc_ob[!racecatname=='NHW' & !racecatname=='NHAsian',])
res_nhwnha_inc_ob <- rma(yi=ESTIM_inc_ob,sei=SE_inc_ob, data=aou_dat_inc_ob[racecatname=='NHW' | racecatname=='NHAsian',])
i2_inc_ob <- rbind(res_inc_ob$I2, res_nonhw_inc_ob$I2, res_nonhb_inc_ob$I2,res_nomex_inc_ob$I2, res_nohis_inc_ob$I2, res_nonha_inc_ob$I2,  res_nooth_inc_ob$I2, res_nooth11_inc_ob$I2)
i2_inc_ob
influence(res_inc_ob, progbar=F)


#####DIABETES or OBESITY DIAGNOSED, BY EDUCCONT#####
#survdesign
surv_diab <- subset(surv_all, diab==1)
surv_dm_nhw <- subset(surv_all, diab==1 & racecat=='NHW')
surv_dm_nhb <- subset(surv_all, diab==1 & racecat=='NHB')
surv_dm_nha <- subset(surv_all, diab==1 & racecat=='NHAsian')
surv_dm_mex <- subset(surv_all, diab==1 & racecat=='Mexican-American')
surv_dm_his <- subset(surv_all, diab==1 & racecat=='Other Hispanic')
surv_dm_oth <- subset(surv_all, diab==1 & racecat=='Multiracial')
surv_dm_oth11 <- subset(surv_all, diab==1 & racecat=='Other')

#surv_diab <- svydesign(id = ~SDMVPSU, strata = ~ SDMVSTRA, weights = ~weights, data=aou[aou$age >= 18 & aou$diab==1 & !aou$insur_stability=='Missing',], nest=T)
#surv_dm_nhw <- svydesign(id = ~SDMVPSU, strata = ~ SDMVSTRA, weights = ~weights, data=aou[aou$age >= 18 & aou$racecat=='NHW' & aou$diab==1 & !aou$insur_stability=='Missing',], nest=T)
#surv_dm_nhb <- svydesign(id = ~SDMVPSU, strata = ~ SDMVSTRA, weights = ~weights, data=aou[aou$age >= 18 & aou$racecat=='NHB' & aou$diab==1 & !aou$insur_stability=='Missing',], nest=T)
#surv_dm_nha <- svydesign(id = ~SDMVPSU, strata = ~ SDMVSTRA, weights = ~weights, data=aou[aou$age >= 18 & aou$racecat=='NHAsian' & aou$diab==1 & !aou$insur_stability=='Missing',], nest=T)
#surv_dm_mex <- svydesign(id = ~SDMVPSU, strata = ~ SDMVSTRA, weights = ~weights, data=aou[aou$age >= 18 & aou$racecat=='Mexican-American' & aou$diab==1 & !aou$insur_stability=='Missing',], nest=T)
#surv_dm_his <- svydesign(id = ~SDMVPSU, strata = ~ SDMVSTRA, weights = ~weights, data=aou[aou$age >= 18 & aou$racecat=='Other Hispanic' & aou$diab==1 & !aou$insur_stability=='Missing',], nest=T)
#surv_dm_oth <- svydesign(id = ~SDMVPSU, strata = ~ SDMVSTRA, weights = ~weights, data=aou[aou$age >= 18 & aou$racecat=='Multiracial' & aou$diab==1 & !aou$insur_stability=='Missing',], nest=T)
#surv_dm_oth11 <- svydesign(id = ~SDMVPSU, strata = ~ SDMVSTRA, weights = ~weights, data=aou[aou$age >= 18 & aou$racecat=='Other' & aou$diab==1 & !aou$insur_stability=='Missing',], nest=T)

surv_ob <- subset(surv_all, obese==1)
surv_ob_nhw <- subset(surv_all, obese==1 & racecat=='NHW')
surv_ob_nhb <- subset(surv_all, obese==1 & racecat=='NHB')
surv_ob_nha <- subset(surv_all, obese==1 & racecat=='NHAsian')
surv_ob_mex <- subset(surv_all, obese==1 & racecat=='Mexican-American')
surv_ob_his <- subset(surv_all, obese==1 & racecat=='Other Hispanic')
surv_ob_oth <- subset(surv_all, obese==1 & racecat=='Multiracial')
surv_ob_oth11 <- subset(surv_all, obese==1 & racecat=='Other')
#surv_ob <- svydesign(id = ~SDMVPSU, strata = ~ SDMVSTRA, weights = ~weights, data=aou[aou$age >= 18 & aou$obese==1 & !aou$insur_stability=='Missing',], nest=T)
#surv_ob_nhw <- svydesign(id = ~SDMVPSU, strata = ~ SDMVSTRA, weights = ~weights, data=aou[aou$age >= 18 & aou$racecat=='NHW' & aou$obese==1 & !aou$insur_stability=='Missing',], nest=T)
#surv_ob_nhb <- svydesign(id = ~SDMVPSU, strata = ~ SDMVSTRA, weights = ~weights, data=aou[aou$age >= 18 & aou$racecat=='NHB' & aou$obese==1 & !aou$insur_stability=='Missing',], nest=T)
#surv_ob_nha <- svydesign(id = ~SDMVPSU, strata = ~ SDMVSTRA, weights = ~weights, data=aou[aou$age >= 18 & aou$racecat=='NHAsian' & aou$obese==1 & !aou$insur_stability=='Missing',], nest=T)
#surv_ob_mex <- svydesign(id = ~SDMVPSU, strata = ~ SDMVSTRA, weights = ~weights, data=aou[aou$age >= 18 & aou$racecat=='Mexican-American' & aou$obese==1 & !aou$insur_stability=='Missing',], nest=T)
#surv_ob_his <- svydesign(id = ~SDMVPSU, strata = ~ SDMVSTRA, weights = ~weights, data=aou[aou$age >= 18 & aou$racecat=='Other Hispanic' & aou$obese==1 & !aou$insur_stability=='Missing',], nest=T)
#surv_ob_oth <- svydesign(id = ~SDMVPSU, strata = ~ SDMVSTRA, weights = ~weights, data=aou[aou$age >= 18 & aou$racecat=='Multiracial' & aou$obese==1 & !aou$insur_stability=='Missing',], nest=T)
#surv_ob_oth11 <- svydesign(id = ~SDMVPSU, strata = ~ SDMVSTRA, weights = ~weights, data=aou[aou$age >= 18 & aou$racecat=='Other' & aou$obese==1 & !aou$insur_stability=='Missing',], nest=T)

#models
mod_diabknown_educcont_all <- glm(data=aou[aou$diab==1,], diab_known ~ age + female + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_diabknown_educcont_all_raceadj <- glm(data=aou[aou$diab==1,], diab_known ~ age + female + smoke_status + insur_stability + educ_num + racecat, family=binomial(link=logit))
mod_diabknown_educcont_nhw <- glm(data=aou[aou$diab==1 & aou$racecat=='NHW',], diab_known ~ age + female + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_diabknown_educcont_nhb <- glm(data=aou[aou$diab==1 & aou$racecat=='NHB',], diab_known ~ age + female + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_diabknown_educcont_his <- glm(data=aou[aou$diab==1 & aou$racecat=='Hispanic',], diab_known ~ age + female + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_diabknown_educcont_nha <- glm(data=aou[aou$diab==1 & aou$racecat=='NHAsian',], diab_known ~ age + female + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_diabknown_educcont_multi <- glm(data=aou[aou$diab==1 & aou$racecat=='Multiracial',], diab_known ~ age + female + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_diabknown_educcont_oth <- glm(data=aou[aou$diab==1 & aou$racecat=='Other',], diab_known ~ age + female + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_diabknown_educcont_none <- glm(data=aou[aou$diab==1 & aou$racecat=='None of these',], diab_known ~ age + female + smoke_status + insur_stability + educ_num, family=binomial(link=logit))

mod_obesknown_educcont_all <- glm(data=aou[aou$obese==1,], obese_known ~ age + female + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_obesknown_educcont_all_raceadj <- svyglm(design = surv_ob, obese_known ~ age + female + smoke_status + insur_stability + educ_num + racecat, family=binomial(link=logit))
mod_obesknown_educcont_nhw <- glm(data=aou[aou$obese==1 & aou$racecat=='NHW',], obese_known ~ age + female + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_obesknown_educcont_nhb <- glm(data=aou[aou$obese==1 & aou$racecat=='NHB',], obese_known ~ age + female + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_obesknown_educcont_his <- glm(data=aou[aou$obese==1 & aou$racecat=='Hispanic',], obese_known ~ age + female + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_obesknown_educcont_nha <- glm(data=aou[aou$obese==1 & aou$racecat=='NHAsian',], obese_known ~ age + female + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_obesknown_educcont_multi <- glm(data=aou[aou$obese==1 & aou$racecat=='Multiracial',], obese_known ~ age + female + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_obesknown_educcont_oth <- glm(data=aou[aou$obese==1 & aou$racecat=='Other',], obese_known ~ age + female + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_obesknown_educcont_none <- glm(data=aou[aou$obese==1 & aou$racecat=='None of these',], obese_known ~ age + female + smoke_status + insur_stability + educ_num, family=binomial(link=logit))

mod_diabob_educcont_allrace <- data.frame(exp(mod_diabknown_educcont_all$coefficients),exp(confint(mod_diabknown_educcont_all)),coef(summary(mod_diabknown_educcont_all))[,"Pr(>|t|)"],
                                          exp(mod_diabknown_educcont_all_raceadj$coefficients)[1:11],exp(confint(mod_diabknown_educcont_all_raceadj))[1:11,1:2],coef(summary(mod_diabknown_educcont_all_raceadj))[,"Pr(>|t|)"][1:11],
                                          exp(mod_diabknown_educcont_nhw$coefficients),exp(confint(mod_diabknown_educcont_nhw)),coef(summary(mod_diabknown_educcont_nhw))[,"Pr(>|t|)"],
                                          exp(mod_diabknown_educcont_nhb$coefficients),exp(confint(mod_diabknown_educcont_nhb)),coef(summary(mod_diabknown_educcont_nhb))[,"Pr(>|t|)"],
                                          exp(mod_diabknown_educcont_mex$coefficients),exp(confint(mod_diabknown_educcont_mex)),coef(summary(mod_diabknown_educcont_mex))[,"Pr(>|t|)"],
                                          exp(mod_diabknown_educcont_his$coefficients),exp(confint(mod_diabknown_educcont_his)),coef(summary(mod_diabknown_educcont_his))[,"Pr(>|t|)"],
                                          exp(mod_diabknown_educcont_nha$coefficients),exp(confint(mod_diabknown_educcont_nha)),coef(summary(mod_diabknown_educcont_nha))[,"Pr(>|t|)"],
                                          exp(mod_diabknown_educcont_oth$coefficients),exp(confint(mod_diabknown_educcont_oth)),coef(summary(mod_diabknown_educcont_oth))[,"Pr(>|t|)"],
                                          exp(mod_diabknown_educcont_oth11$coefficients),exp(confint(mod_diabknown_educcont_oth11)),coef(summary(mod_diabknown_educcont_oth11))[,"Pr(>|t|)"],
                                          exp(mod_obesknown_educcont_all$coefficients),exp(confint(mod_obesknown_educcont_all)),coef(summary(mod_obesknown_educcont_all))[,"Pr(>|t|)"],
                                          exp(mod_obesknown_educcont_all_raceadj$coefficients)[1:11],exp(confint(mod_obesknown_educcont_all_raceadj))[1:11,1:2],coef(summary(mod_obesknown_educcont_all_raceadj))[,"Pr(>|t|)"][1:11],
                                          exp(mod_obesknown_educcont_nhw$coefficients),exp(confint(mod_obesknown_educcont_nhw)),coef(summary(mod_obesknown_educcont_nhw))[,"Pr(>|t|)"],
                                          exp(mod_obesknown_educcont_nhb$coefficients),exp(confint(mod_obesknown_educcont_nhb)),coef(summary(mod_obesknown_educcont_nhb))[,"Pr(>|t|)"],
                                          exp(mod_obesknown_educcont_mex$coefficients),exp(confint(mod_obesknown_educcont_mex)),coef(summary(mod_obesknown_educcont_mex))[,"Pr(>|t|)"],
                                          exp(mod_obesknown_educcont_his$coefficients),exp(confint(mod_obesknown_educcont_his)),coef(summary(mod_obesknown_educcont_his))[,"Pr(>|t|)"],
                                          exp(mod_obesknown_educcont_nha$coefficients),exp(confint(mod_obesknown_educcont_nha)),coef(summary(mod_obesknown_educcont_nha))[,"Pr(>|t|)"],
                                          exp(mod_obesknown_educcont_oth$coefficients),exp(confint(mod_obesknown_educcont_oth)),coef(summary(mod_obesknown_educcont_oth))[,"Pr(>|t|)"],
                                          exp(mod_obesknown_educcont_oth11$coefficients),exp(confint(mod_obesknown_educcont_oth11)),coef(summary(mod_obesknown_educcont_oth11))[,"Pr(>|t|)"],stringsAsFactors = FALSE)
mod_diabob_educcont_allrace_transpose <- t(mod_diabob_educcont_allrace)
dim(mod_diabob_educcont_allrace)
tiff(file='C:/Users/sarac/OneDrive/Documents/Research/Area v Individual SES/aou_forest_indiv_educcont_t2dobknown.tiff', width=8, height=5, units='in', res=300)
forestplot(c('Type 2 Diabetes','Obesity'), 
           mean = cbind(t(mod_diabob_educcont_allrace[11,c(1,37)]),t(mod_diabob_educcont_allrace[11,c(5,41)]),
                        t(mod_diabob_educcont_allrace[11,c(9,45)]),t(mod_diabob_educcont_allrace[11,c(13,49)]),
                        t(mod_diabob_educcont_allrace[11,c(17,53)]),t(mod_diabob_educcont_allrace[11,c(21,57)]),
                        t(mod_diabob_educcont_allrace[11,c(25,61)]),t(mod_diabob_educcont_allrace[11,c(29,65)]),
                        t(mod_diabob_educcont_allrace[11,c(33,69)])),
           lower = cbind(t(mod_diabob_educcont_allrace[11,c(2,38)]),t(mod_diabob_educcont_allrace[11,c(6,42)]),
                         t(mod_diabob_educcont_allrace[11,c(10,46)]),t(mod_diabob_educcont_allrace[11,c(14,50)]),
                         t(mod_diabob_educcont_allrace[11,c(18,54)]),t(mod_diabob_educcont_allrace[11,c(22,58)]),
                         t(mod_diabob_educcont_allrace[11,c(26,62)]),t(mod_diabob_educcont_allrace[11,c(30,66)]),
                         t(mod_diabob_educcont_allrace[11,c(34,68)])),
           upper = cbind(t(mod_diabob_educcont_allrace[11,c(3,39)]),t(mod_diabob_educcont_allrace[11,c(7,43)]),
                         t(mod_diabob_educcont_allrace[11,c(11,47)]),t(mod_diabob_educcont_allrace[11,c(15,51)]),
                         t(mod_diabob_educcont_allrace[11,c(19,55)]),t(mod_diabob_educcont_allrace[11,c(23,59)]),
                         t(mod_diabob_educcont_allrace[11,c(27,63)]),t(mod_diabob_educcont_allrace[11,c(31,67)]),
                         t(mod_diabob_educcont_allrace[11,c(35,71)])),
           col = fpColors(box = c('gray23','gray57','dodgerblue4','darkslategray3','palegreen3','lightgoldenrod','salmon1','indianred3','mediumpurple4')),
           legend=c("All","All, Race-adjusted","NHW","NHB","Mexican-American","Other Hispanic","NHA","Other/Multi-Racial \n including NHA (pre-2011)","Other/Multi-Racial \n (post-2011)"),
           legend_args = fpLegend(pos = "right"),#list(x = 0.5, y = -0.2), gp = gpar(cex=0.9,col = "#CCCCCC", fill = "#F9F9F9")),
           #legend=c("All","All, Race-adjusted",rownames(table(aou$racecat))),#c("All","All-Race-adjusted","NHW","NHB","NHA","Mexican-American","Other Hispanic","Other/Multiracial"),
           #legend_args = fpLegend(pos = list(x = 0.9, y = 0.9), gp = gpar(cex=0.9,col = "#CCCCCC", fill = "#F9F9F9")),
           xlab = "OR (95% CI)",
           title = 'Adjusted OR for Continuous Educational Attainment Ratio \n on Knowledge of Diagnosis of T2D and Obesity, \n Overall and Stratified by Race/Ethnicity',
           txt_gp = fpTxtGp(title=gpar(cex=1),xlab=gpar(cex=1), ticks=gpar(cex=1), label=gpar(cex=1)),
           vertices=TRUE,
           line.margin=0.25,
           graph.pos=2,
           clip=c(0.7,2.0), xlim=c(0.7,2.0), xticks=c(0.75,1.0,1.25,1.5,1.75,2.0), xlog=TRUE, grid=structure(c(0.75,1.0,1.25,1.5,1.75,2.0)))
dev.off()



#####DIABETES or OBESITY DIAGNOSED, BY INC2POV#####
#survey design objects are above, under educ
#models
mod_diabknown_incnum_all <- glm(data=aou[aou$diab==1,], diab_known ~ age + female + smoke_status + insur_stability + inc_num, family=binomial(link=logit))
mod_diabknown_incnum_all_raceadj <- glm(data=aou[aou$diab==1,], diab_known ~ age + female + smoke_status + insur_stability + inc_num + racecat, family=binomial(link=logit))
mod_diabknown_incnum_nhw <- glm(data=aou[aou$diab==1 & aou$racecat=='NHW',], diab_known ~ age + female + smoke_status + insur_stability + inc_num, family=binomial(link=logit))
mod_diabknown_incnum_nhb <- glm(data=aou[aou$diab==1 & aou$racecat=='NHB',], diab_known ~ age + female + smoke_status + insur_stability + inc_num, family=binomial(link=logit))
mod_diabknown_incnum_his <- glm(data=aou[aou$diab==1 & aou$racecat=='Hispanic',], diab_known ~ age + female + smoke_status + insur_stability + inc_num, family=binomial(link=logit))
mod_diabknown_incnum_nha <- glm(data=aou[aou$diab==1 & aou$racecat=='NHAsian',], diab_known ~ age + female + smoke_status + insur_stability + inc_num, family=binomial(link=logit))
mod_diabknown_incnum_multi <- glm(data=aou[aou$diab==1 & aou$racecat=='Multiracial',], diab_known ~ age + female + smoke_status + insur_stability + inc_num, family=binomial(link=logit))
mod_diabknown_incnum_oth <- glm(data=aou[aou$diab==1 & aou$racecat=='Other',], diab_known ~ age + female + smoke_status + insur_stability + inc_num, family=binomial(link=logit))
mod_diabknown_incnum_none <- glm(data=aou[aou$diab==1 & aou$racecat=='None of these',], diab_known ~ age + female + smoke_status + insur_stability + inc_num, family=binomial(link=logit))

mod_obesknown_incnum_all <- svyglm(design = surv_ob, obese_known ~ age + female + smoke_status + insur_stability + inc_num, family=binomial(link=logit))
mod_obesknown_incnum_all_raceadj <- svyglm(design = surv_ob, obese_known ~ age + female + smoke_status + insur_stability + inc_num + racecat, family=binomial(link=logit))
mod_obesknown_incnum_nhw <- glm(data=aou[aou$obese==1 & aou$racecat=='NHW',], obese_known ~ age + female + smoke_status + insur_stability + inc_num, family=binomial(link=logit))
mod_obesknown_incnum_nhb <- glm(data=aou[aou$obese==1 & aou$racecat=='NHB',], obese_known ~ age + female + smoke_status + insur_stability + inc_num, family=binomial(link=logit))
mod_obesknown_incnum_his <- glm(data=aou[aou$obese==1 & aou$racecat=='Hispanic',], obese_known ~ age + female + smoke_status + insur_stability + inc_num, family=binomial(link=logit))
mod_obesknown_incnum_nha <- glm(data=aou[aou$obese==1 & aou$racecat=='NHAsian',], obese_known ~ age + female + smoke_status + insur_stability + inc_num, family=binomial(link=logit))
mod_obesknown_incnum_multi <- glm(data=aou[aou$obese==1 & aou$racecat=='Multiracial',], obese_known ~ age + female + smoke_status + insur_stability + inc_num, family=binomial(link=logit))
mod_obesknown_incnum_oth <- glm(data=aou[aou$obese==1 & aou$racecat=='Other',], obese_known ~ age + female + smoke_status + insur_stability + inc_num, family=binomial(link=logit))
mod_obesknown_incnum_none <- glm(data=aou[aou$obese==1 & aou$racecat=='None of these',], obese_known ~ age + female + smoke_status + insur_stability + inc_num, family=binomial(link=logit))

mod_diabob_incnum_allrace <- data.frame(exp(mod_diabknown_incnum_all$coefficients),exp(confint(mod_diabknown_incnum_all)),coef(summary(mod_diabknown_incnum_all))[,"Pr(>|t|)"],
                                        exp(mod_diabknown_incnum_all_raceadj$coefficients)[1:11],exp(confint(mod_diabknown_incnum_all_raceadj))[1:11,1:2],coef(summary(mod_diabknown_incnum_all_raceadj))[,"Pr(>|t|)"][1:11],
                                        exp(mod_diabknown_incnum_nhw$coefficients),exp(confint(mod_diabknown_incnum_nhw)),coef(summary(mod_diabknown_incnum_nhw))[,"Pr(>|t|)"],
                                        exp(mod_diabknown_incnum_nhb$coefficients),exp(confint(mod_diabknown_incnum_nhb)),coef(summary(mod_diabknown_incnum_nhb))[,"Pr(>|t|)"],
                                        exp(mod_diabknown_incnum_mex$coefficients),exp(confint(mod_diabknown_incnum_mex)),coef(summary(mod_diabknown_incnum_mex))[,"Pr(>|t|)"],
                                        exp(mod_diabknown_incnum_his$coefficients),exp(confint(mod_diabknown_incnum_his)),coef(summary(mod_diabknown_incnum_his))[,"Pr(>|t|)"],
                                        exp(mod_diabknown_incnum_nha$coefficients),exp(confint(mod_diabknown_incnum_nha)),coef(summary(mod_diabknown_incnum_nha))[,"Pr(>|t|)"],
                                        exp(mod_diabknown_incnum_oth$coefficients),exp(confint(mod_diabknown_incnum_oth)),coef(summary(mod_diabknown_incnum_oth))[,"Pr(>|t|)"],
                                        exp(mod_diabknown_incnum_oth11$coefficients),exp(confint(mod_diabknown_incnum_oth11)),coef(summary(mod_diabknown_incnum_oth11))[,"Pr(>|t|)"],
                                        exp(mod_obesknown_incnum_all$coefficients),exp(confint(mod_obesknown_incnum_all)),coef(summary(mod_obesknown_incnum_all))[,"Pr(>|t|)"],
                                        exp(mod_obesknown_incnum_all_raceadj$coefficients)[1:11],exp(confint(mod_obesknown_incnum_all_raceadj))[1:11,1:2],coef(summary(mod_obesknown_incnum_all_raceadj))[,"Pr(>|t|)"][1:11],
                                        exp(mod_obesknown_incnum_nhw$coefficients),exp(confint(mod_obesknown_incnum_nhw)),coef(summary(mod_obesknown_incnum_nhw))[,"Pr(>|t|)"],
                                        exp(mod_obesknown_incnum_nhb$coefficients),exp(confint(mod_obesknown_incnum_nhb)),coef(summary(mod_obesknown_incnum_nhb))[,"Pr(>|t|)"],
                                        exp(mod_obesknown_incnum_mex$coefficients),exp(confint(mod_obesknown_incnum_mex)),coef(summary(mod_obesknown_incnum_mex))[,"Pr(>|t|)"],
                                        exp(mod_obesknown_incnum_his$coefficients),exp(confint(mod_obesknown_incnum_his)),coef(summary(mod_obesknown_incnum_his))[,"Pr(>|t|)"],
                                        exp(mod_obesknown_incnum_nha$coefficients),exp(confint(mod_obesknown_incnum_nha)),coef(summary(mod_obesknown_incnum_nha))[,"Pr(>|t|)"],
                                        exp(mod_obesknown_incnum_oth$coefficients),exp(confint(mod_obesknown_incnum_oth)),coef(summary(mod_obesknown_incnum_oth))[,"Pr(>|t|)"],
                                        exp(mod_obesknown_incnum_oth11$coefficients),exp(confint(mod_obesknown_incnum_oth11)),coef(summary(mod_obesknown_incnum_oth11))[,"Pr(>|t|)"],stringsAsFactors = FALSE)
mod_diabob_incnum_allrace_transpose <- t(mod_diabob_incnum_allrace)
dim(mod_diabob_incnum_allrace)
tiff(file='C:/Users/sarac/OneDrive/Documents/Research/Area v Individual SES/aou_forest_indiv_incnum_t2dobknown.tiff', width=8, height=5, units='in', res=300)
forestplot(c('Type 2 Diabetes','Obesity'), 
           mean = cbind(t(mod_diabob_incnum_allrace[11,c(1,37)]),t(mod_diabob_incnum_allrace[11,c(5,41)]),
                        t(mod_diabob_incnum_allrace[11,c(9,45)]),t(mod_diabob_incnum_allrace[11,c(13,49)]),
                        t(mod_diabob_incnum_allrace[11,c(17,53)]),t(mod_diabob_incnum_allrace[11,c(21,57)]),
                        t(mod_diabob_incnum_allrace[11,c(25,61)]),t(mod_diabob_incnum_allrace[11,c(29,65)]),
                        t(mod_diabob_incnum_allrace[11,c(33,69)])),
           lower = cbind(t(mod_diabob_incnum_allrace[11,c(2,38)]),t(mod_diabob_incnum_allrace[11,c(6,42)]),
                         t(mod_diabob_incnum_allrace[11,c(10,46)]),t(mod_diabob_incnum_allrace[11,c(14,50)]),
                         t(mod_diabob_incnum_allrace[11,c(18,54)]),t(mod_diabob_incnum_allrace[11,c(22,58)]),
                         t(mod_diabob_incnum_allrace[11,c(26,62)]),t(mod_diabob_incnum_allrace[11,c(30,66)]),
                         t(mod_diabob_incnum_allrace[11,c(34,68)])),
           upper = cbind(t(mod_diabob_incnum_allrace[11,c(3,39)]),t(mod_diabob_incnum_allrace[11,c(7,43)]),
                         t(mod_diabob_incnum_allrace[11,c(11,47)]),t(mod_diabob_incnum_allrace[11,c(15,51)]),
                         t(mod_diabob_incnum_allrace[11,c(19,55)]),t(mod_diabob_incnum_allrace[11,c(23,59)]),
                         t(mod_diabob_incnum_allrace[11,c(27,63)]),t(mod_diabob_incnum_allrace[11,c(31,67)]),
                         t(mod_diabob_incnum_allrace[11,c(35,71)])),
           col = fpColors(box = c('gray23','gray57','dodgerblue4','darkslategray3','palegreen3','lightgoldenrod','salmon1','indianred3','mediumpurple4')),
           legend=c("All","All, Race-adjusted","NHW","NHB","Mexican-American","Other Hispanic","NHA","Other/Multi-Racial \n including NHA (pre-2011)","Other/Multi-Racial \n (post-2011)"),
           legend_args = fpLegend(pos = "right"),#list(x = 0.5, y = -0.2), gp = gpar(cex=0.9,col = "#CCCCCC", fill = "#F9F9F9")),
           #legend=c("All","All, Race-adjusted",rownames(table(aou$racecat))),#c("All","All-Race-adjusted","NHW","NHB","NHA","Mexican-American","Other Hispanic","Other/Multiracial"),
           #legend_args = fpLegend(pos = list(x = 0.9, y = 0.9), gp = gpar(cex=0.9,col = "#CCCCCC", fill = "#F9F9F9")),
           xlab = "OR (95% CI)",
           title = 'Adjusted OR for Continuous Income (Multiples of $25k) \n on Knowledge of Diagnosis of T2D and Obesity, \n Overall and Stratified by Race/Ethnicity',
           txt_gp = fpTxtGp(title=gpar(cex=1),xlab=gpar(cex=1), ticks=gpar(cex=1), label=gpar(cex=1)),
           vertices=TRUE,
           line.margin=0.25,
           graph.pos=2,
           clip=c(0.7,1.5), xlim=c(0.7,1.5), xticks=c(0.75,1.0,1.25,1.5), xlog=TRUE, grid=structure(c(0.75,1.0,1.25,1.5)))
dev.off()





#####DIABETES CONTROLLED, BY EDUCCONT#####
#survdesign
surv_diab_known <- subset(surv_all, diab_known==1 & !is.na(LBXGH))
surv_dmknown_nhw <- subset(surv_all, diab_known==1 & !is.na(LBXGH) & racecat=='NHW')
surv_dmknown_nhb <- subset(surv_all, diab_known==1 & !is.na(LBXGH) & racecat=='NHB')
surv_dmknown_nha <- subset(surv_all, diab_known==1 & !is.na(LBXGH) & racecat=='NHAsian')
surv_dmknown_mex <- subset(surv_all, diab_known==1 & !is.na(LBXGH) & racecat=='Mexican-American')
surv_dmknown_his <- subset(surv_all, diab_known==1 & !is.na(LBXGH) & racecat=='Other Hispanic')
surv_dmknown_oth <- subset(surv_all, diab_known==1 & !is.na(LBXGH) & racecat=='Multiracial')
surv_dmknown_oth11 <- subset(surv_all, diab_known==1 & !is.na(LBXGH) & racecat=='Other')
#surv_diab_known <- svydesign(id = ~SDMVPSU, strata = ~ SDMVSTRA, weights = ~weights, data=aou[aou$age >= 18 & aou$diab_known==1 & !aou$insur_stability=='Missing' & !is.na(LBXGH),], nest=T)
#surv_dmknown_nhw <- svydesign(id = ~SDMVPSU, strata = ~ SDMVSTRA, weights = ~weights, data=aou[aou$age >= 18 & aou$racecat=='NHW' & aou$diab_known==1 & !aou$insur_stability=='Missing' & !is.na(aou$LBXGH),], nest=T)
#surv_dmknown_nhb <- svydesign(id = ~SDMVPSU, strata = ~ SDMVSTRA, weights = ~weights, data=aou[aou$age >= 18 & aou$racecat=='NHB' & aou$diab_known==1 & !aou$insur_stability=='Missing' & !is.na(aou$LBXGH),], nest=T)
#surv_dmknown_nha <- svydesign(id = ~SDMVPSU, strata = ~ SDMVSTRA, weights = ~weights, data=aou[aou$age >= 18 & aou$racecat=='NHAsian' & aou$diab_known==1 & !aou$insur_stability=='Missing' & !is.na(aou$LBXGH),], nest=T)
#surv_dmknown_mex <- svydesign(id = ~SDMVPSU, strata = ~ SDMVSTRA, weights = ~weights, data=aou[aou$age >= 18 & aou$racecat=='Mexican-American' & aou$diab_known==1 & !aou$insur_stability=='Missing' & !is.na(aou$LBXGH),], nest=T)
#surv_dmknown_his <- svydesign(id = ~SDMVPSU, strata = ~ SDMVSTRA, weights = ~weights, data=aou[aou$age >= 18 & aou$racecat=='Other Hispanic' & aou$diab_known==1 & !aou$insur_stability=='Missing' & !is.na(aou$LBXGH),], nest=T)
#surv_dmknown_oth <- svydesign(id = ~SDMVPSU, strata = ~ SDMVSTRA, weights = ~weights, data=aou[aou$age >= 18 & aou$racecat=='Multiracial' & aou$diab_known==1 & !aou$insur_stability=='Missing' & !is.na(aou$LBXGH),], nest=T)
#surv_dmknown_oth11 <- svydesign(id = ~SDMVPSU, strata = ~ SDMVSTRA, weights = ~weights, data=aou[aou$age >= 18 & aou$racecat=='Other' & aou$diab_known==1 & !aou$insur_stability=='Missing' & !is.na(aou$LBXGH),], nest=T)

#models
mod_diabctrl_educcont_all <- svyglm(design = surv_diab_known, diab_controlled ~ age + female + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_diabctrl_educcont_all_raceadj <- svyglm(design = surv_diab_known, diab_controlled ~ age + female + smoke_status + insur_stability + educ_num + racecat, family=binomial(link=logit))
mod_diabctrl_educcont_nhw <- svyglm(design = surv_dmknown_nhw, diab_controlled ~ age + female + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_diabctrl_educcont_nhb <- svyglm(design = surv_dmknown_nhb, diab_controlled ~ age + female + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_diabctrl_educcont_nha <- svyglm(design = surv_dmknown_nha, diab_controlled ~ age + female + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_diabctrl_educcont_mex <- svyglm(design = surv_dmknown_mex, diab_controlled ~ age + female + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_diabctrl_educcont_his <- svyglm(design = surv_dmknown_his, diab_controlled ~ age + female + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_diabctrl_educcont_oth <- svyglm(design = surv_dmknown_oth, diab_controlled ~ age + female + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_diabctrl_educcont_oth11 <- svyglm(design = surv_dmknown_oth11, diab_controlled ~ age + female + smoke_status + insur_stability + educ_num, family=binomial(link=logit))

mod_diabctrl8_educcont_all <- svyglm(design = surv_diab_known, diab_controlled8 ~ age + female + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_diabctrl8_educcont_all_raceadj <- svyglm(design = surv_diab_known, diab_controlled8 ~ age + female + smoke_status + insur_stability + educ_num + racecat, family=binomial(link=logit))
mod_diabctrl8_educcont_nhw <- svyglm(design = surv_dmknown_nhw, diab_controlled8 ~ age + female + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_diabctrl8_educcont_nhb <- svyglm(design = surv_dmknown_nhb, diab_controlled8 ~ age + female + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_diabctrl8_educcont_nha <- svyglm(design = surv_dmknown_nha, diab_controlled8 ~ age + female + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_diabctrl8_educcont_mex <- svyglm(design = surv_dmknown_mex, diab_controlled8 ~ age + female + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_diabctrl8_educcont_his <- svyglm(design = surv_dmknown_his, diab_controlled8 ~ age + female + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_diabctrl8_educcont_oth <- svyglm(design = surv_dmknown_oth, diab_controlled8 ~ age + female + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_diabctrl8_educcont_oth11 <- svyglm(design = surv_dmknown_oth11, diab_controlled8 ~ age + female + smoke_status + insur_stability + educ_num, family=binomial(link=logit))

mod_diabcont_educcont_allrace <- data.frame(exp(mod_diabctrl_educcont_all$coefficients),exp(confint(mod_diabctrl_educcont_all)),coef(summary(mod_diabctrl_educcont_all))[,"Pr(>|t|)"],
                                            exp(mod_diabctrl_educcont_all_raceadj$coefficients)[1:11],exp(confint(mod_diabctrl_educcont_all_raceadj))[1:11,1:2],coef(summary(mod_diabctrl_educcont_all_raceadj))[,"Pr(>|t|)"][1:11],
                                            exp(mod_diabctrl_educcont_nhw$coefficients),exp(confint(mod_diabctrl_educcont_nhw)),coef(summary(mod_diabctrl_educcont_nhw))[,"Pr(>|t|)"],
                                            exp(mod_diabctrl_educcont_nhb$coefficients),exp(confint(mod_diabctrl_educcont_nhb)),coef(summary(mod_diabctrl_educcont_nhb))[,"Pr(>|t|)"],
                                            exp(mod_diabctrl_educcont_mex$coefficients),exp(confint(mod_diabctrl_educcont_mex)),coef(summary(mod_diabctrl_educcont_mex))[,"Pr(>|t|)"],
                                            exp(mod_diabctrl_educcont_his$coefficients),exp(confint(mod_diabctrl_educcont_his)),coef(summary(mod_diabctrl_educcont_his))[,"Pr(>|t|)"],
                                            exp(mod_diabctrl_educcont_nha$coefficients),exp(confint(mod_diabctrl_educcont_nha)),coef(summary(mod_diabctrl_educcont_nha))[,"Pr(>|t|)"],
                                            exp(mod_diabctrl_educcont_oth$coefficients),exp(confint(mod_diabctrl_educcont_oth)),coef(summary(mod_diabctrl_educcont_oth))[,"Pr(>|t|)"],
                                            exp(mod_diabctrl_educcont_oth11$coefficients),exp(confint(mod_diabctrl_educcont_oth11)),coef(summary(mod_diabctrl_educcont_oth11))[,"Pr(>|t|)"],
                                            exp(mod_diabctrl8_educcont_all$coefficients),exp(confint(mod_diabctrl8_educcont_all)),coef(summary(mod_diabctrl8_educcont_all))[,"Pr(>|t|)"],
                                            exp(mod_diabctrl8_educcont_all_raceadj$coefficients)[1:11],exp(confint(mod_diabctrl8_educcont_all_raceadj))[1:11,1:2],coef(summary(mod_diabctrl8_educcont_all_raceadj))[,"Pr(>|t|)"][1:11],
                                            exp(mod_diabctrl8_educcont_nhw$coefficients),exp(confint(mod_diabctrl8_educcont_nhw)),coef(summary(mod_diabctrl8_educcont_nhw))[,"Pr(>|t|)"],
                                            exp(mod_diabctrl8_educcont_nhb$coefficients),exp(confint(mod_diabctrl8_educcont_nhb)),coef(summary(mod_diabctrl8_educcont_nhb))[,"Pr(>|t|)"],
                                            exp(mod_diabctrl8_educcont_mex$coefficients),exp(confint(mod_diabctrl8_educcont_mex)),coef(summary(mod_diabctrl8_educcont_mex))[,"Pr(>|t|)"],
                                            exp(mod_diabctrl8_educcont_his$coefficients),exp(confint(mod_diabctrl8_educcont_his)),coef(summary(mod_diabctrl8_educcont_his))[,"Pr(>|t|)"],
                                            exp(mod_diabctrl8_educcont_nha$coefficients),exp(confint(mod_diabctrl8_educcont_nha)),coef(summary(mod_diabctrl8_educcont_nha))[,"Pr(>|t|)"],
                                            exp(mod_diabctrl8_educcont_oth$coefficients),exp(confint(mod_diabctrl8_educcont_oth)),coef(summary(mod_diabctrl8_educcont_oth))[,"Pr(>|t|)"],
                                            exp(mod_diabctrl8_educcont_oth11$coefficients),exp(confint(mod_diabctrl8_educcont_oth11)),coef(summary(mod_diabctrl8_educcont_oth11))[,"Pr(>|t|)"],stringsAsFactors = FALSE)
mod_diabcont_educcont_allrace_transpose <- t(mod_diabcont_educcont_allrace)
dim(mod_diabcont_educcont_allrace)
tiff(file='C:/Users/sarac/OneDrive/Documents/Research/Area v Individual SES/aou_forest_indiv_educcont_t2dcontrol.tiff', width=8, height=5, units='in', res=300)
forestplot(c('HbA1c ??? 7.0%','HbA1c ??? 8.0%'), 
           mean = cbind(t(mod_diabcont_educcont_allrace[11,c(1,37)]),t(mod_diabcont_educcont_allrace[11,c(5,41)]),
                        t(mod_diabcont_educcont_allrace[11,c(9,45)]),t(mod_diabcont_educcont_allrace[11,c(13,49)]),
                        t(mod_diabcont_educcont_allrace[11,c(17,53)]),t(mod_diabcont_educcont_allrace[11,c(21,57)]),
                        t(mod_diabcont_educcont_allrace[11,c(25,61)]),t(mod_diabcont_educcont_allrace[11,c(29,65)]),
                        t(mod_diabcont_educcont_allrace[11,c(33,69)])),
           lower = cbind(t(mod_diabcont_educcont_allrace[11,c(2,38)]),t(mod_diabcont_educcont_allrace[11,c(6,42)]),
                         t(mod_diabcont_educcont_allrace[11,c(10,46)]),t(mod_diabcont_educcont_allrace[11,c(14,50)]),
                         t(mod_diabcont_educcont_allrace[11,c(18,54)]),t(mod_diabcont_educcont_allrace[11,c(22,58)]),
                         t(mod_diabcont_educcont_allrace[11,c(26,62)]),t(mod_diabcont_educcont_allrace[11,c(30,66)]),
                         t(mod_diabcont_educcont_allrace[11,c(34,68)])),
           upper = cbind(t(mod_diabcont_educcont_allrace[11,c(3,39)]),t(mod_diabcont_educcont_allrace[11,c(7,43)]),
                         t(mod_diabcont_educcont_allrace[11,c(11,47)]),t(mod_diabcont_educcont_allrace[11,c(15,51)]),
                         t(mod_diabcont_educcont_allrace[11,c(19,55)]),t(mod_diabcont_educcont_allrace[11,c(23,59)]),
                         t(mod_diabcont_educcont_allrace[11,c(27,63)]),t(mod_diabcont_educcont_allrace[11,c(31,67)]),
                         t(mod_diabcont_educcont_allrace[11,c(35,71)])),
           col = fpColors(box = c('gray23','gray57','dodgerblue4','darkslategray3','palegreen3','lightgoldenrod','salmon1','indianred3','mediumpurple4')),
           legend=c("All","All, Race-adjusted","NHW","NHB","Mexican-American","Other Hispanic","NHA","Other/Multi-Racial \n including NHA (pre-2011)","Other/Multi-Racial \n (post-2011)"),
           legend_args = fpLegend(pos = "right"),#list(x = 0.5, y = -0.2), gp = gpar(cex=0.9,col = "#CCCCCC", fill = "#F9F9F9")),
           #legend=c("All","All, Race-adjusted",rownames(table(aou$racecat))),#c("All","All-Race-adjusted","NHW","NHB","NHA","Mexican-American","Other Hispanic","Other/Multiracial"),
           #legend_args = fpLegend(pos = list(x = 0.9, y = 0.9), gp = gpar(cex=0.8,col = "#CCCCCC", fill = "#F9F9F9")),
           xlab = "OR (95% CI)",
           title = 'Adjusted OR for Continuous Educational Attainment on T2D Control, \n Overall and Stratified by Race/Ethnicity',
           txt_gp = fpTxtGp(title=gpar(cex=1),xlab=gpar(cex=1), ticks=gpar(cex=1), label=gpar(cex=1)),
           vertices=TRUE,
           line.margin=0.25,
           graph.pos=2,
           clip=c(0.6,1.75), xlim=c(0.6,1.75), xticks=c(0.6,0.75,1.0,1.25,1.5,1.75), xlog=TRUE, grid=structure(c(0.6,0.75,1.0,1.25,1.5,1.75)))
dev.off()



#####DIABETES CONTROLLED, BY INC2POV#####
#survdesign objects above under educ
#models
mod_diabctrl_incnum_all <- svyglm(design = surv_diab_known, diab_controlled ~ age + female + smoke_status + insur_stability + inc_num, family=binomial(link=logit))
mod_diabctrl_incnum_all_raceadj <- svyglm(design = surv_diab_known, diab_controlled ~ age + female + smoke_status + insur_stability + inc_num + racecat, family=binomial(link=logit))
mod_diabctrl_incnum_nhw <- svyglm(design = surv_dmknown_nhw, diab_controlled ~ age + female + smoke_status + insur_stability + inc_num, family=binomial(link=logit))
mod_diabctrl_incnum_nhb <- svyglm(design = surv_dmknown_nhb, diab_controlled ~ age + female + smoke_status + insur_stability + inc_num, family=binomial(link=logit))
mod_diabctrl_incnum_nha <- svyglm(design = surv_dmknown_nha, diab_controlled ~ age + female + smoke_status + insur_stability + inc_num, family=binomial(link=logit))
mod_diabctrl_incnum_mex <- svyglm(design = surv_dmknown_mex, diab_controlled ~ age + female + smoke_status + insur_stability + inc_num, family=binomial(link=logit))
mod_diabctrl_incnum_his <- svyglm(design = surv_dmknown_his, diab_controlled ~ age + female + smoke_status + insur_stability + inc_num, family=binomial(link=logit))
mod_diabctrl_incnum_oth <- svyglm(design = surv_dmknown_oth, diab_controlled ~ age + female + smoke_status + insur_stability + inc_num, family=binomial(link=logit))
mod_diabctrl_incnum_oth11 <- svyglm(design = surv_dmknown_oth11, diab_controlled ~ age + female + smoke_status + insur_stability + inc_num, family=binomial(link=logit))

mod_diabctrl8_incnum_all <- svyglm(design = surv_diab_known, diab_controlled8 ~ age + female + smoke_status + insur_stability + inc_num, family=binomial(link=logit))
mod_diabctrl8_incnum_all_raceadj <- svyglm(design = surv_diab_known, diab_controlled8 ~ age + female + smoke_status + insur_stability + inc_num + racecat, family=binomial(link=logit))
mod_diabctrl8_incnum_nhw <- svyglm(design = surv_dmknown_nhw, diab_controlled8 ~ age + female + smoke_status + insur_stability + inc_num, family=binomial(link=logit))
mod_diabctrl8_incnum_nhb <- svyglm(design = surv_dmknown_nhb, diab_controlled8 ~ age + female + smoke_status + insur_stability + inc_num, family=binomial(link=logit))
mod_diabctrl8_incnum_nha <- svyglm(design = surv_dmknown_nha, diab_controlled8 ~ age + female + smoke_status + insur_stability + inc_num, family=binomial(link=logit))
mod_diabctrl8_incnum_mex <- svyglm(design = surv_dmknown_mex, diab_controlled8 ~ age + female + smoke_status + insur_stability + inc_num, family=binomial(link=logit))
mod_diabctrl8_incnum_his <- svyglm(design = surv_dmknown_his, diab_controlled8 ~ age + female + smoke_status + insur_stability + inc_num, family=binomial(link=logit))
mod_diabctrl8_incnum_oth <- svyglm(design = surv_dmknown_oth, diab_controlled8 ~ age + female + smoke_status + insur_stability + inc_num, family=binomial(link=logit))
mod_diabctrl8_incnum_oth11 <- svyglm(design = surv_dmknown_oth11, diab_controlled8 ~ age + female + smoke_status + insur_stability + inc_num, family=binomial(link=logit))


mod_diabcont_incnum_allrace <- data.frame(exp(mod_diabctrl_incnum_all$coefficients),exp(confint(mod_diabctrl_incnum_all)),coef(summary(mod_diabctrl_incnum_all))[,"Pr(>|t|)"],
                                          exp(mod_diabctrl_incnum_all_raceadj$coefficients)[1:11],exp(confint(mod_diabctrl_incnum_all_raceadj))[1:11,1:2],coef(summary(mod_diabctrl_incnum_all_raceadj))[,"Pr(>|t|)"][1:11],
                                          exp(mod_diabctrl_incnum_nhw$coefficients),exp(confint(mod_diabctrl_incnum_nhw)),coef(summary(mod_diabctrl_incnum_nhw))[,"Pr(>|t|)"],
                                          exp(mod_diabctrl_incnum_nhb$coefficients),exp(confint(mod_diabctrl_incnum_nhb)),coef(summary(mod_diabctrl_incnum_nhb))[,"Pr(>|t|)"],
                                          exp(mod_diabctrl_incnum_mex$coefficients),exp(confint(mod_diabctrl_incnum_mex)),coef(summary(mod_diabctrl_incnum_mex))[,"Pr(>|t|)"],
                                          exp(mod_diabctrl_incnum_his$coefficients),exp(confint(mod_diabctrl_incnum_his)),coef(summary(mod_diabctrl_incnum_his))[,"Pr(>|t|)"],
                                          exp(mod_diabctrl_incnum_nha$coefficients),exp(confint(mod_diabctrl_incnum_nha)),coef(summary(mod_diabctrl_incnum_nha))[,"Pr(>|t|)"],
                                          exp(mod_diabctrl_incnum_oth$coefficients),exp(confint(mod_diabctrl_incnum_oth)),coef(summary(mod_diabctrl_incnum_oth))[,"Pr(>|t|)"],
                                          exp(mod_diabctrl_incnum_oth11$coefficients),exp(confint(mod_diabctrl_incnum_oth11)),coef(summary(mod_diabctrl_incnum_oth11))[,"Pr(>|t|)"],
                                          exp(mod_diabctrl8_incnum_all$coefficients),exp(confint(mod_diabctrl8_incnum_all)),coef(summary(mod_diabctrl8_incnum_all))[,"Pr(>|t|)"],
                                          exp(mod_diabctrl8_incnum_all_raceadj$coefficients)[1:11],exp(confint(mod_diabctrl8_incnum_all_raceadj))[1:11,1:2],coef(summary(mod_diabctrl8_incnum_all_raceadj))[,"Pr(>|t|)"][1:11],
                                          exp(mod_diabctrl8_incnum_nhw$coefficients),exp(confint(mod_diabctrl8_incnum_nhw)),coef(summary(mod_diabctrl8_incnum_nhw))[,"Pr(>|t|)"],
                                          exp(mod_diabctrl8_incnum_nhb$coefficients),exp(confint(mod_diabctrl8_incnum_nhb)),coef(summary(mod_diabctrl8_incnum_nhb))[,"Pr(>|t|)"],
                                          exp(mod_diabctrl8_incnum_mex$coefficients),exp(confint(mod_diabctrl8_incnum_mex)),coef(summary(mod_diabctrl8_incnum_mex))[,"Pr(>|t|)"],
                                          exp(mod_diabctrl8_incnum_his$coefficients),exp(confint(mod_diabctrl8_incnum_his)),coef(summary(mod_diabctrl8_incnum_his))[,"Pr(>|t|)"],
                                          exp(mod_diabctrl8_incnum_nha$coefficients),exp(confint(mod_diabctrl8_incnum_nha)),coef(summary(mod_diabctrl8_incnum_nha))[,"Pr(>|t|)"],
                                          exp(mod_diabctrl8_incnum_oth$coefficients),exp(confint(mod_diabctrl8_incnum_oth)),coef(summary(mod_diabctrl8_incnum_oth))[,"Pr(>|t|)"],
                                          exp(mod_diabctrl8_incnum_oth11$coefficients),exp(confint(mod_diabctrl8_incnum_oth11)),coef(summary(mod_diabctrl8_incnum_oth11))[,"Pr(>|t|)"],stringsAsFactors = FALSE)
mod_diabcont_incnum_allrace_transpose <- t(mod_diabcont_incnum_allrace)
dim(mod_diabcont_incnum_allrace)
mod_diabcont_incnum_allrace[11,c(4,8,12,16,20,24,28,32,36,40,44,48,52,56,60,64,68,72)]
tiff(file='C:/Users/sarac/OneDrive/Documents/Research/Area v Individual SES/aou_forest_indiv_incnum_t2dcontrol.tiff', width=8, height=5, units='in', res=300)
forestplot(c('HbA1c ??? 7.0%','HbA1c ??? 8.0%'), 
           mean = cbind(t(mod_diabcont_incnum_allrace[11,c(1,37)]),t(mod_diabcont_incnum_allrace[11,c(5,41)]),
                        t(mod_diabcont_incnum_allrace[11,c(9,45)]),t(mod_diabcont_incnum_allrace[11,c(13,49)]),
                        t(mod_diabcont_incnum_allrace[11,c(17,53)]),t(mod_diabcont_incnum_allrace[11,c(21,57)]),
                        t(mod_diabcont_incnum_allrace[11,c(25,61)]),t(mod_diabcont_incnum_allrace[11,c(29,65)]),
                        t(mod_diabcont_incnum_allrace[11,c(33,69)])),
           lower = cbind(t(mod_diabcont_incnum_allrace[11,c(2,38)]),t(mod_diabcont_incnum_allrace[11,c(6,42)]),
                         t(mod_diabcont_incnum_allrace[11,c(10,46)]),t(mod_diabcont_incnum_allrace[11,c(14,50)]),
                         t(mod_diabcont_incnum_allrace[11,c(18,54)]),t(mod_diabcont_incnum_allrace[11,c(22,58)]),
                         t(mod_diabcont_incnum_allrace[11,c(26,62)]),t(mod_diabcont_incnum_allrace[11,c(30,66)]),
                         t(mod_diabcont_incnum_allrace[11,c(34,68)])),
           upper = cbind(t(mod_diabcont_incnum_allrace[11,c(3,39)]),t(mod_diabcont_incnum_allrace[11,c(7,43)]),
                         t(mod_diabcont_incnum_allrace[11,c(11,47)]),t(mod_diabcont_incnum_allrace[11,c(15,51)]),
                         t(mod_diabcont_incnum_allrace[11,c(19,55)]),t(mod_diabcont_incnum_allrace[11,c(23,59)]),
                         t(mod_diabcont_incnum_allrace[11,c(27,63)]),t(mod_diabcont_incnum_allrace[11,c(31,67)]),
                         t(mod_diabcont_incnum_allrace[11,c(35,71)])),
           col = fpColors(box = c('gray23','gray57','dodgerblue4','darkslategray3','palegreen3','lightgoldenrod','salmon1','indianred3','mediumpurple4')),
           legend=c("All","All, Race-adjusted","NHW","NHB","Mexican-American","Other Hispanic","NHA","Other/Multi-Racial \n including NHA (pre-2011)","Other/Multi-Racial \n (post-2011)"),
           legend_args = fpLegend(pos = "right"),#list(x = 0.5, y = -0.2), gp = gpar(cex=0.9,col = "#CCCCCC", fill = "#F9F9F9")),
           #legend=c("All","All, Race-adjusted",rownames(table(aou$racecat))),#c("All","All-Race-adjusted","NHW","NHB","NHA","Mexican-American","Other Hispanic","Other/Multiracial"),
           #legend_args = fpLegend(pos = list(x = 0.9, y = 0.9), gp = gpar(cex=0.8,col = "#CCCCCC", fill = "#F9F9F9")),
           xlab = "OR (95% CI)",
           title = 'Adjusted OR for Continuous Income (Multiples of $25k) on T2D Control, \n Overall and Stratified by Race/Ethnicity',
           txt_gp = fpTxtGp(title=gpar(cex=1),xlab=gpar(cex=1), ticks=gpar(cex=1), label=gpar(cex=1)),
           vertices=TRUE,
           line.margin=0.25,
           graph.pos=2,
           clip=c(0.5,2), xlim=c(0.5,2), xticks=c(0.5,0.75,1.0,1.25,1.5,1.75,2.0), xlog=TRUE, grid=structure(c(0.5,0.75,1.0,1.25,1.5,1.75,2.0)))
dev.off()
table(aou$racecat, aou$diab_controlled, exclude=NULL)
table(aou[!is.na(aou$LBXGH),]$racecat, aou[!is.na(aou$LBXGH),]$diab_controlled, exclude=NULL)


#####all I2 and influence measures#####
i2_all <- cbind(i2_educ_diab, i2_educ_ob, i2_inc_diab, i2_inc_ob)
rownames(i2_all) <- c('None',rownames(table(aou$racecat)))
colnames(i2_all) <- c('educ_diab','educ_ob','inc_diab','inc_ob')
i2_all

influence(res_educ_diab, progbar=F)
influence(res_educ_ob, progbar=F)
influence(res_inc_diab, progbar=F)
influence(res_inc_ob, progbar=F)




#####combined educ and income - diabetes, obesity prevalence#####
mod_diab_educcont_all <- glm(data=aou, diab ~ age + female + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_diab_educinc_all <- glm(data=aou, diab ~ age + female + smoke_status + insur_stability + educ_num + inc_num, family=binomial(link=logit))
summary(mod_diab_educcont_all)
summary(mod_diab_educinc_all)
NagelkerkeR2(mod_diab_educcont_all) #0.1554
NagelkerkeR2(mod_diab_educinc_all) #0.1606
exp(mod_diab_educcont_all$coefficients)
exp(mod_diab_educinc_all$coefficients)

mod_diab_educinc_all_raceadj <- glm(data=aou, diab ~ age + female + smoke_status + insur_stability + educ_num + inc_num + racecat, family=binomial(link=logit))
mod_diab_educinc_nhw <- glm(data=aou[aou$racecat=='NHW',], diab ~ age + female + smoke_status + insur_stability + educ_num + inc_num, family=binomial(link=logit))
mod_diab_educinc_nhb <- glm(data=aou[aou$racecat=='NHB',], diab ~ age + female + smoke_status + insur_stability + educ_num + inc_num, family=binomial(link=logit))
summary(mod_diab_educinc_nhb)
exp(mod_diab_educinc_nhb$coefficients)
mod_diab_educinc_nha <- glm(data=aou[aou$racecat=='NHAsian',], diab ~ age + female + smoke_status + insur_stability + educ_num + inc_num, family=binomial(link=logit))
mod_diab_educinc_mex <- glm(data=aou[aou$racecat=='Hispanic',], diab ~ age + female + smoke_status + insur_stability + educ_num + inc_num, family=binomial(link=logit))
mod_diab_educinc_his <- glm(data=aou[aou$racecat=='Multiracial',], diab ~ age + female + smoke_status + insur_stability + educ_num + inc_num, family=binomial(link=logit))
mod_diab_educinc_oth <- glm(data=aou[aou$racecat=='Other',], diab ~ age + female + smoke_status + insur_stability + educ_num + inc_num, family=binomial(link=logit))
mod_diab_educinc_oth11 <- glm(data=aou[aou$racecat=='None of these',], diab ~ age + female + smoke_status + insur_stability + educ_num + inc_num, family=binomial(link=logit))

mod_obes_educinc_all <- glm(data=aou, obese ~ age + female + smoke_status + insur_stability + educ_num + inc_num, family=binomial(link=logit))
mod_obes_educinc_all_raceadj <- glm(data=aou, obese ~ age + female + smoke_status + insur_stability + educ_num + inc_num + racecat, family=binomial(link=logit))
mod_obes_educinc_nhw <- glm(data=aou[aou$racecat=='NHW',], obese ~ age + female + smoke_status + insur_stability + educ_num + inc_num, family=binomial(link=logit))
mod_obes_educinc_nhb <- glm(data=aou[aou$racecat=='NHB',], obese ~ age + female + smoke_status + insur_stability + educ_num + inc_num, family=binomial(link=logit))
mod_obes_educinc_nha <- glm(data=aou[aou$racecat=='NHAsian',], obese ~ age + female + smoke_status + insur_stability + educ_num + inc_num, family=binomial(link=logit))
mod_obes_educinc_mex <- glm(data=aou[aou$racecat=='Hispanic',], obese ~ age + female + smoke_status + insur_stability + educ_num + inc_num, family=binomial(link=logit))
mod_obes_educinc_his <- glm(data=aou[aou$racecat=='Multiracial',], obese ~ age + female + smoke_status + insur_stability + educ_num + inc_num, family=binomial(link=logit))
mod_obes_educinc_oth <- glm(data=aou[aou$racecat=='Other',], obese ~ age + female + smoke_status + insur_stability + educ_num + inc_num, family=binomial(link=logit))
mod_obes_educinc_oth11 <- glm(data=aou[aou$racecat=='None of these',], obese ~ age + female + smoke_status + insur_stability + educ_num + inc_num, family=binomial(link=logit))

mod_diabob_educinc_allrace <- data.frame(exp(mod_diab_educinc_all$coefficients),exp(confint(mod_diab_educinc_all)),coef(summary(mod_diab_educinc_all))[,"Pr(>|t|)"],
                                         exp(mod_diab_educinc_all_raceadj$coefficients)[1:12],exp(confint(mod_diab_educinc_all_raceadj))[1:12,1:2],coef(summary(mod_diab_educinc_all_raceadj))[,"Pr(>|t|)"][1:12],
                                         exp(mod_diab_educinc_nhw$coefficients),exp(confint(mod_diab_educinc_nhw)),coef(summary(mod_diab_educinc_nhw))[,"Pr(>|t|)"],
                                         exp(mod_diab_educinc_nhb$coefficients),exp(confint(mod_diab_educinc_nhb)),coef(summary(mod_diab_educinc_nhb))[,"Pr(>|t|)"],
                                         exp(mod_diab_educinc_mex$coefficients),exp(confint(mod_diab_educinc_mex)),coef(summary(mod_diab_educinc_mex))[,"Pr(>|t|)"],
                                         exp(mod_diab_educinc_his$coefficients),exp(confint(mod_diab_educinc_his)),coef(summary(mod_diab_educinc_his))[,"Pr(>|t|)"],
                                         exp(mod_diab_educinc_nha$coefficients),exp(confint(mod_diab_educinc_nha)),coef(summary(mod_diab_educinc_nha))[,"Pr(>|t|)"],
                                         exp(mod_diab_educinc_oth$coefficients),exp(confint(mod_diab_educinc_oth)),coef(summary(mod_diab_educinc_oth))[,"Pr(>|t|)"],
                                         exp(mod_diab_educinc_oth11$coefficients),exp(confint(mod_diab_educinc_oth11)),coef(summary(mod_diab_educinc_oth11))[,"Pr(>|t|)"],
                                         exp(mod_obes_educinc_all$coefficients),exp(confint(mod_obes_educinc_all)),coef(summary(mod_obes_educinc_all))[,"Pr(>|t|)"],
                                         exp(mod_obes_educinc_all_raceadj$coefficients)[1:12],exp(confint(mod_obes_educinc_all_raceadj))[1:12,1:2],coef(summary(mod_obes_educinc_all_raceadj))[,"Pr(>|t|)"][1:12],
                                         exp(mod_obes_educinc_nhw$coefficients),exp(confint(mod_obes_educinc_nhw)),coef(summary(mod_obes_educinc_nhw))[,"Pr(>|t|)"],
                                         exp(mod_obes_educinc_nhb$coefficients),exp(confint(mod_obes_educinc_nhb)),coef(summary(mod_obes_educinc_nhb))[,"Pr(>|t|)"],
                                         exp(mod_obes_educinc_mex$coefficients),exp(confint(mod_obes_educinc_mex)),coef(summary(mod_obes_educinc_mex))[,"Pr(>|t|)"],
                                         exp(mod_obes_educinc_his$coefficients),exp(confint(mod_obes_educinc_his)),coef(summary(mod_obes_educinc_his))[,"Pr(>|t|)"],
                                         exp(mod_obes_educinc_nha$coefficients),exp(confint(mod_obes_educinc_nha)),coef(summary(mod_obes_educinc_nha))[,"Pr(>|t|)"],
                                         exp(mod_obes_educinc_oth$coefficients),exp(confint(mod_obes_educinc_oth)),coef(summary(mod_obes_educinc_oth))[,"Pr(>|t|)"],
                                         exp(mod_obes_educinc_oth11$coefficients),exp(confint(mod_obes_educinc_oth11)),coef(summary(mod_obes_educinc_oth11))[,"Pr(>|t|)"],stringsAsFactors = FALSE)
dim(mod_diabob_educinc_allrace)

#adjusted ORs for insur_stability, educ_num, inc_num summary
fill <- c(999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,
          999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999)
r2_educcont <- rbind(NagelkerkeR2(mod_diab_educcont_all)$R2,NagelkerkeR2(mod_diab_educcont_all_raceadj)$R2,NagelkerkeR2(mod_diab_educcont_nhw)$R2,
                     NagelkerkeR2(mod_diab_educcont_nhb)$R2,NagelkerkeR2(mod_diab_educcont_mex)$R2,NagelkerkeR2(mod_diab_educcont_his)$R2,
                     NagelkerkeR2(mod_diab_educcont_nha)$R2,NagelkerkeR2(mod_diab_educcont_oth)$R2,NagelkerkeR2(mod_diab_educcont_oth11)$R2,
                     NagelkerkeR2(mod_obes_educcont_all)$R2,NagelkerkeR2(mod_obes_educcont_all_raceadj)$R2,NagelkerkeR2(mod_obes_educcont_nhw)$R2,
                     NagelkerkeR2(mod_obes_educcont_nhb)$R2,NagelkerkeR2(mod_obes_educcont_mex)$R2,
                     NagelkerkeR2(mod_obes_educcont_his)$R2,NagelkerkeR2(mod_obes_educcont_nha)$R2,NagelkerkeR2(mod_obes_educcont_oth)$R2,
                     NagelkerkeR2(mod_obes_educcont_oth11)$R2)
r2_inc2pov <- rbind(NagelkerkeR2(mod_diab_incnum_all)$R2,NagelkerkeR2(mod_diab_incnum_all_raceadj)$R2,NagelkerkeR2(mod_diab_incnum_nhw)$R2,
                    NagelkerkeR2(mod_diab_incnum_nhb)$R2,NagelkerkeR2(mod_diab_incnum_mex)$R2,NagelkerkeR2(mod_diab_incnum_his)$R2,
                    NagelkerkeR2(mod_diab_incnum_nha)$R2,NagelkerkeR2(mod_diab_incnum_oth)$R2,NagelkerkeR2(mod_diab_incnum_oth11)$R2,
                    NagelkerkeR2(mod_obes_incnum_all)$R2,NagelkerkeR2(mod_obes_incnum_all_raceadj)$R2,NagelkerkeR2(mod_obes_incnum_nhw)$R2,
                    NagelkerkeR2(mod_obes_incnum_nhb)$R2,NagelkerkeR2(mod_obes_incnum_mex)$R2,
                    NagelkerkeR2(mod_obes_incnum_his)$R2,NagelkerkeR2(mod_obes_incnum_nha)$R2,NagelkerkeR2(mod_obes_incnum_oth)$R2,
                    NagelkerkeR2(mod_obes_incnum_oth11)$R2)
r2_educinc <- rbind(NagelkerkeR2(mod_diab_educinc_all)$R2,NagelkerkeR2(mod_diab_educinc_all_raceadj)$R2,NagelkerkeR2(mod_diab_educinc_nhw)$R2,
                    NagelkerkeR2(mod_diab_educinc_nhb)$R2,NagelkerkeR2(mod_diab_educinc_mex)$R2,NagelkerkeR2(mod_diab_educinc_his)$R2,
                    NagelkerkeR2(mod_diab_educinc_nha)$R2,NagelkerkeR2(mod_diab_educinc_oth)$R2,NagelkerkeR2(mod_diab_educinc_oth11)$R2,
                    NagelkerkeR2(mod_obes_educinc_all)$R2,NagelkerkeR2(mod_obes_educinc_all_raceadj)$R2,NagelkerkeR2(mod_obes_educinc_nhw)$R2,
                    NagelkerkeR2(mod_obes_educinc_nhb)$R2,NagelkerkeR2(mod_obes_educinc_mex)$R2,NagelkerkeR2(mod_obes_educinc_his)$R2,
                    NagelkerkeR2(mod_obes_educinc_nha)$R2,NagelkerkeR2(mod_obes_educinc_oth)$R2,NagelkerkeR2(mod_obes_educinc_oth11)$R2)
results_educinc_combined <- cbind(rbind(mod_alldx_educcont_allrace[7:11,1:72], fill),
                                  rbind(mod_alldx_incnum_allrace[7:10,1:72],fill,mod_alldx_incnum_allrace[11,1:72]),
                                  mod_diabob_educinc_allrace[7:12,])
dim(results_educinc_combined)
r2_educinc_combined <- cbind(r2_educcont, r2_inc2pov, r2_educinc)

write.csv(results_educinc_combined, 'C:/Users/sarac/OneDrive/Documents/Research/Area v Individual SES/aou_educinc_combmodels.csv',row.names=TRUE)




#####models without adjustment for insurance#####
#educ only
mod_diab_educcont_noins_all <- glm(data=aou, diab ~ age + female + smoke_status + educ_num, family=binomial(link=logit))
mod_diab_educcont_noins_all_raceadj <- glm(data=aou, diab ~ age + female + smoke_status + educ_num + racecat, family=binomial(link=logit))
mod_diab_educcont_noins_nhw <- glm(data=aou[aou$racecat=='NHW',], diab ~ age + female + smoke_status + educ_num, family=binomial(link=logit))
mod_diab_educcont_noins_nhb <- glm(data=aou[aou$racecat=='NHB',], diab ~ age + female + smoke_status + educ_num, family=binomial(link=logit))
mod_diab_educcont_noins_nha <- glm(data=aou[aou$racecat=='NHAsian',], diab ~ age + female + smoke_status + educ_num, family=binomial(link=logit))
mod_diab_educcont_noins_mex <- glm(data=aou[aou$racecat=='Hispanic',], diab ~ age + female + smoke_status + educ_num, family=binomial(link=logit))
mod_diab_educcont_noins_his <- glm(data=aou[aou$racecat=='Multiracial',], diab ~ age + female + smoke_status + educ_num, family=binomial(link=logit))
mod_diab_educcont_noins_oth <- glm(data=aou[aou$racecat=='Other',], diab ~ age + female + smoke_status + educ_num, family=binomial(link=logit))
mod_diab_educcont_noins_oth11 <- glm(data=aou[aou$racecat=='None of these',], diab ~ age + female + smoke_status + educ_num, family=binomial(link=logit))

mod_obes_educcont_noins_all <- glm(data=aou, obese ~ age + female + smoke_status + educ_num, family=binomial(link=logit))
mod_obes_educcont_noins_all_raceadj <- glm(data=aou, obese ~ age + female + smoke_status + educ_num + racecat, family=binomial(link=logit))
mod_obes_educcont_noins_nhw <- glm(data=aou[aou$racecat=='NHW',], obese ~ age + female + smoke_status + educ_num, family=binomial(link=logit))
mod_obes_educcont_noins_nhb <- glm(data=aou[aou$racecat=='NHB',], obese ~ age + female + smoke_status + educ_num, family=binomial(link=logit))
mod_obes_educcont_noins_nha <- glm(data=aou[aou$racecat=='NHAsian',], obese ~ age + female + smoke_status + educ_num, family=binomial(link=logit))
mod_obes_educcont_noins_mex <- glm(data=aou[aou$racecat=='Hispanic',], obese ~ age + female + smoke_status + educ_num, family=binomial(link=logit))
mod_obes_educcont_noins_his <- glm(data=aou[aou$racecat=='Multiracial',], obese ~ age + female + smoke_status + educ_num, family=binomial(link=logit))
mod_obes_educcont_noins_oth <- glm(data=aou[aou$racecat=='Other',], obese ~ age + female + smoke_status + educ_num, family=binomial(link=logit))
mod_obes_educcont_noins_oth11 <- glm(data=aou[aou$racecat=='None of these',], obese ~ age + female + smoke_status + educ_num, family=binomial(link=logit))

mod_diabob_educcont_noins_allrace <- data.frame(exp(mod_diab_educcont_noins_all$coefficients),exp(confint(mod_diab_educcont_noins_all)),coef(summary(mod_diab_educcont_noins_all))[,"Pr(>|t|)"],
                                                exp(mod_diab_educcont_noins_all_raceadj$coefficients)[1:7],exp(confint(mod_diab_educcont_noins_all_raceadj))[1:7,1:2],coef(summary(mod_diab_educcont_noins_all_raceadj))[,"Pr(>|t|)"][1:7],
                                                exp(mod_diab_educcont_noins_nhw$coefficients),exp(confint(mod_diab_educcont_noins_nhw)),coef(summary(mod_diab_educcont_noins_nhw))[,"Pr(>|t|)"],
                                                exp(mod_diab_educcont_noins_nhb$coefficients),exp(confint(mod_diab_educcont_noins_nhb)),coef(summary(mod_diab_educcont_noins_nhb))[,"Pr(>|t|)"],
                                                exp(mod_diab_educcont_noins_mex$coefficients),exp(confint(mod_diab_educcont_noins_mex)),coef(summary(mod_diab_educcont_noins_mex))[,"Pr(>|t|)"],
                                                exp(mod_diab_educcont_noins_his$coefficients),exp(confint(mod_diab_educcont_noins_his)),coef(summary(mod_diab_educcont_noins_his))[,"Pr(>|t|)"],
                                                exp(mod_diab_educcont_noins_nha$coefficients),exp(confint(mod_diab_educcont_noins_nha)),coef(summary(mod_diab_educcont_noins_nha))[,"Pr(>|t|)"],
                                                exp(mod_diab_educcont_noins_oth$coefficients),exp(confint(mod_diab_educcont_noins_oth)),coef(summary(mod_diab_educcont_noins_oth))[,"Pr(>|t|)"],
                                                exp(mod_diab_educcont_noins_oth11$coefficients),exp(confint(mod_diab_educcont_noins_oth11)),coef(summary(mod_diab_educcont_noins_oth11))[,"Pr(>|t|)"],
                                                exp(mod_obes_educcont_noins_all$coefficients),exp(confint(mod_obes_educcont_noins_all)),coef(summary(mod_obes_educcont_noins_all))[,"Pr(>|t|)"],
                                                exp(mod_obes_educcont_noins_all_raceadj$coefficients)[1:7],exp(confint(mod_obes_educcont_noins_all_raceadj))[1:7,1:2],coef(summary(mod_obes_educcont_noins_all_raceadj))[,"Pr(>|t|)"][1:7],
                                                exp(mod_obes_educcont_noins_nhw$coefficients),exp(confint(mod_obes_educcont_noins_nhw)),coef(summary(mod_obes_educcont_noins_nhw))[,"Pr(>|t|)"],
                                                exp(mod_obes_educcont_noins_nhb$coefficients),exp(confint(mod_obes_educcont_noins_nhb)),coef(summary(mod_obes_educcont_noins_nhb))[,"Pr(>|t|)"],
                                                exp(mod_obes_educcont_noins_mex$coefficients),exp(confint(mod_obes_educcont_noins_mex)),coef(summary(mod_obes_educcont_noins_mex))[,"Pr(>|t|)"],
                                                exp(mod_obes_educcont_noins_his$coefficients),exp(confint(mod_obes_educcont_noins_his)),coef(summary(mod_obes_educcont_noins_his))[,"Pr(>|t|)"],
                                                exp(mod_obes_educcont_noins_nha$coefficients),exp(confint(mod_obes_educcont_noins_nha)),coef(summary(mod_obes_educcont_noins_nha))[,"Pr(>|t|)"],
                                                exp(mod_obes_educcont_noins_oth$coefficients),exp(confint(mod_obes_educcont_noins_oth)),coef(summary(mod_obes_educcont_noins_oth))[,"Pr(>|t|)"],
                                                exp(mod_obes_educcont_noins_oth11$coefficients),exp(confint(mod_obes_educcont_noins_oth11)),coef(summary(mod_obes_educcont_noins_oth11))[,"Pr(>|t|)"],stringsAsFactors = FALSE)

#inc2pov only
mod_diab_inc2pov_noins_all <- glm(data=aou, diab ~ age + female + smoke_status + inc_num, family=binomial(link=logit))
mod_diab_inc2pov_noins_all_raceadj <- glm(data=aou, diab ~ age + female + smoke_status + inc_num + racecat, family=binomial(link=logit))
mod_diab_inc2pov_noins_nhw <- glm(data=aou[aou$racecat=='NHW',], diab ~ age + female + smoke_status + inc_num, family=binomial(link=logit))
mod_diab_inc2pov_noins_nhb <- glm(data=aou[aou$racecat=='NHB',], diab ~ age + female + smoke_status + inc_num, family=binomial(link=logit))
mod_diab_inc2pov_noins_nha <- glm(data=aou[aou$racecat=='NHAsian',], diab ~ age + female + smoke_status + inc_num, family=binomial(link=logit))
mod_diab_inc2pov_noins_mex <- glm(data=aou[aou$racecat=='Hispanic',], diab ~ age + female + smoke_status + inc_num, family=binomial(link=logit))
mod_diab_inc2pov_noins_his <- glm(data=aou[aou$racecat=='Multiracial',], diab ~ age + female + smoke_status + inc_num, family=binomial(link=logit))
mod_diab_inc2pov_noins_oth <- glm(data=aou[aou$racecat=='Other',], diab ~ age + female + smoke_status + inc_num, family=binomial(link=logit))
mod_diab_inc2pov_noins_oth11 <- glm(data=aou[aou$racecat=='None of these',], diab ~ age + female + smoke_status + inc_num, family=binomial(link=logit))

mod_obes_inc2pov_noins_all <- glm(data=aou, obese ~ age + female + smoke_status + inc_num, family=binomial(link=logit))
mod_obes_inc2pov_noins_all_raceadj <- glm(data=aou, obese ~ age + female + smoke_status + inc_num + racecat, family=binomial(link=logit))
mod_obes_inc2pov_noins_nhw <- glm(data=aou[aou$racecat=='NHW',], obese ~ age + female + smoke_status + inc_num, family=binomial(link=logit))
mod_obes_inc2pov_noins_nhb <- glm(data=aou[aou$racecat=='NHB',], obese ~ age + female + smoke_status + inc_num, family=binomial(link=logit))
mod_obes_inc2pov_noins_nha <- glm(data=aou[aou$racecat=='NHAsian',], obese ~ age + female + smoke_status + inc_num, family=binomial(link=logit))
mod_obes_inc2pov_noins_mex <- glm(data=aou[aou$racecat=='Hispanic',], obese ~ age + female + smoke_status + inc_num, family=binomial(link=logit))
mod_obes_inc2pov_noins_his <- glm(data=aou[aou$racecat=='Multiracial',], obese ~ age + female + smoke_status + inc_num, family=binomial(link=logit))
mod_obes_inc2pov_noins_oth <- glm(data=aou[aou$racecat=='Other',], obese ~ age + female + smoke_status + inc_num, family=binomial(link=logit))
mod_obes_inc2pov_noins_oth11 <- glm(data=aou[aou$racecat=='None of these',], obese ~ age + female + smoke_status + inc_num, family=binomial(link=logit))

mod_diabob_inc2pov_noins_allrace <- data.frame(exp(mod_diab_inc2pov_noins_all$coefficients),exp(confint(mod_diab_inc2pov_noins_all)),coef(summary(mod_diab_inc2pov_noins_all))[,"Pr(>|t|)"],
                                               exp(mod_diab_inc2pov_noins_all_raceadj$coefficients)[1:7],exp(confint(mod_diab_inc2pov_noins_all_raceadj))[1:7,1:2],coef(summary(mod_diab_inc2pov_noins_all_raceadj))[,"Pr(>|t|)"][1:7],
                                               exp(mod_diab_inc2pov_noins_nhw$coefficients),exp(confint(mod_diab_inc2pov_noins_nhw)),coef(summary(mod_diab_inc2pov_noins_nhw))[,"Pr(>|t|)"],
                                               exp(mod_diab_inc2pov_noins_nhb$coefficients),exp(confint(mod_diab_inc2pov_noins_nhb)),coef(summary(mod_diab_inc2pov_noins_nhb))[,"Pr(>|t|)"],
                                               exp(mod_diab_inc2pov_noins_mex$coefficients),exp(confint(mod_diab_inc2pov_noins_mex)),coef(summary(mod_diab_inc2pov_noins_mex))[,"Pr(>|t|)"],
                                               exp(mod_diab_inc2pov_noins_his$coefficients),exp(confint(mod_diab_inc2pov_noins_his)),coef(summary(mod_diab_inc2pov_noins_his))[,"Pr(>|t|)"],
                                               exp(mod_diab_inc2pov_noins_nha$coefficients),exp(confint(mod_diab_inc2pov_noins_nha)),coef(summary(mod_diab_inc2pov_noins_nha))[,"Pr(>|t|)"],
                                               exp(mod_diab_inc2pov_noins_oth$coefficients),exp(confint(mod_diab_inc2pov_noins_oth)),coef(summary(mod_diab_inc2pov_noins_oth))[,"Pr(>|t|)"],
                                               exp(mod_diab_inc2pov_noins_oth11$coefficients),exp(confint(mod_diab_inc2pov_noins_oth11)),coef(summary(mod_diab_inc2pov_noins_oth11))[,"Pr(>|t|)"],
                                               exp(mod_obes_inc2pov_noins_all$coefficients),exp(confint(mod_obes_inc2pov_noins_all)),coef(summary(mod_obes_inc2pov_noins_all))[,"Pr(>|t|)"],
                                               exp(mod_obes_inc2pov_noins_all_raceadj$coefficients)[1:7],exp(confint(mod_obes_inc2pov_noins_all_raceadj))[1:7,1:2],coef(summary(mod_obes_inc2pov_noins_all_raceadj))[,"Pr(>|t|)"][1:7],
                                               exp(mod_obes_inc2pov_noins_nhw$coefficients),exp(confint(mod_obes_inc2pov_noins_nhw)),coef(summary(mod_obes_inc2pov_noins_nhw))[,"Pr(>|t|)"],
                                               exp(mod_obes_inc2pov_noins_nhb$coefficients),exp(confint(mod_obes_inc2pov_noins_nhb)),coef(summary(mod_obes_inc2pov_noins_nhb))[,"Pr(>|t|)"],
                                               exp(mod_obes_inc2pov_noins_mex$coefficients),exp(confint(mod_obes_inc2pov_noins_mex)),coef(summary(mod_obes_inc2pov_noins_mex))[,"Pr(>|t|)"],
                                               exp(mod_obes_inc2pov_noins_his$coefficients),exp(confint(mod_obes_inc2pov_noins_his)),coef(summary(mod_obes_inc2pov_noins_his))[,"Pr(>|t|)"],
                                               exp(mod_obes_inc2pov_noins_nha$coefficients),exp(confint(mod_obes_inc2pov_noins_nha)),coef(summary(mod_obes_inc2pov_noins_nha))[,"Pr(>|t|)"],
                                               exp(mod_obes_inc2pov_noins_oth$coefficients),exp(confint(mod_obes_inc2pov_noins_oth)),coef(summary(mod_obes_inc2pov_noins_oth))[,"Pr(>|t|)"],
                                               exp(mod_obes_inc2pov_noins_oth11$coefficients),exp(confint(mod_obes_inc2pov_noins_oth11)),coef(summary(mod_obes_inc2pov_noins_oth11))[,"Pr(>|t|)"],stringsAsFactors = FALSE)


#educ + inc2pov
mod_diab_educinc_noins_all <- glm(data=aou, diab ~ age + female + smoke_status + educ_num + inc_num, family=binomial(link=logit))
mod_diab_educinc_noins_all_raceadj <- glm(data=aou, diab ~ age + female + smoke_status + educ_num + inc_num + racecat, family=binomial(link=logit))
mod_diab_educinc_noins_nhw <- glm(data=aou[aou$racecat=='NHW',], diab ~ age + female + smoke_status + educ_num + inc_num, family=binomial(link=logit))
mod_diab_educinc_noins_nhb <- glm(data=aou[aou$racecat=='NHB',], diab ~ age + female + smoke_status + educ_num + inc_num, family=binomial(link=logit))
mod_diab_educinc_noins_nha <- glm(data=aou[aou$racecat=='NHAsian',], diab ~ age + female + smoke_status + educ_num + inc_num, family=binomial(link=logit))
mod_diab_educinc_noins_mex <- glm(data=aou[aou$racecat=='Hispanic',], diab ~ age + female + smoke_status + educ_num + inc_num, family=binomial(link=logit))
mod_diab_educinc_noins_his <- glm(data=aou[aou$racecat=='Multiracial',], diab ~ age + female + smoke_status + educ_num + inc_num, family=binomial(link=logit))
mod_diab_educinc_noins_oth <- glm(data=aou[aou$racecat=='Other',], diab ~ age + female + smoke_status + educ_num + inc_num, family=binomial(link=logit))
mod_diab_educinc_noins_oth11 <- glm(data=aou[aou$racecat=='None of these',], diab ~ age + female + smoke_status + educ_num + inc_num, family=binomial(link=logit))

mod_obes_educinc_noins_all <- glm(data=aou, obese ~ age + female + smoke_status + educ_num + inc_num, family=binomial(link=logit))
mod_obes_educinc_noins_all_raceadj <- glm(data=aou, obese ~ age + female + smoke_status + educ_num + inc_num + racecat, family=binomial(link=logit))
mod_obes_educinc_noins_nhw <- glm(data=aou[aou$racecat=='NHW',], obese ~ age + female + smoke_status + educ_num + inc_num, family=binomial(link=logit))
mod_obes_educinc_noins_nhb <- glm(data=aou[aou$racecat=='NHB',], obese ~ age + female + smoke_status + educ_num + inc_num, family=binomial(link=logit))
mod_obes_educinc_noins_nha <- glm(data=aou[aou$racecat=='NHAsian',], obese ~ age + female + smoke_status + educ_num + inc_num, family=binomial(link=logit))
mod_obes_educinc_noins_mex <- glm(data=aou[aou$racecat=='Hispanic',], obese ~ age + female + smoke_status + educ_num + inc_num, family=binomial(link=logit))
mod_obes_educinc_noins_his <- glm(data=aou[aou$racecat=='Multiracial',], obese ~ age + female + smoke_status + educ_num + inc_num, family=binomial(link=logit))
mod_obes_educinc_noins_oth <- glm(data=aou[aou$racecat=='Other',], obese ~ age + female + smoke_status + educ_num + inc_num, family=binomial(link=logit))
mod_obes_educinc_noins_oth11 <- glm(data=aou[aou$racecat=='None of these',], obese ~ age + female + smoke_status + educ_num + inc_num, family=binomial(link=logit))

mod_diabob_educinc_noins_allrace <- data.frame(exp(mod_diab_educinc_noins_all$coefficients),exp(confint(mod_diab_educinc_noins_all)),coef(summary(mod_diab_educinc_noins_all))[,"Pr(>|t|)"],
                                               exp(mod_diab_educinc_noins_all_raceadj$coefficients)[1:8],exp(confint(mod_diab_educinc_noins_all_raceadj))[1:8,1:2],coef(summary(mod_diab_educinc_noins_all_raceadj))[,"Pr(>|t|)"][1:8],
                                               exp(mod_diab_educinc_noins_nhw$coefficients),exp(confint(mod_diab_educinc_noins_nhw)),coef(summary(mod_diab_educinc_noins_nhw))[,"Pr(>|t|)"],
                                               exp(mod_diab_educinc_noins_nhb$coefficients),exp(confint(mod_diab_educinc_noins_nhb)),coef(summary(mod_diab_educinc_noins_nhb))[,"Pr(>|t|)"],
                                               exp(mod_diab_educinc_noins_mex$coefficients),exp(confint(mod_diab_educinc_noins_mex)),coef(summary(mod_diab_educinc_noins_mex))[,"Pr(>|t|)"],
                                               exp(mod_diab_educinc_noins_his$coefficients),exp(confint(mod_diab_educinc_noins_his)),coef(summary(mod_diab_educinc_noins_his))[,"Pr(>|t|)"],
                                               exp(mod_diab_educinc_noins_nha$coefficients),exp(confint(mod_diab_educinc_noins_nha)),coef(summary(mod_diab_educinc_noins_nha))[,"Pr(>|t|)"],
                                               exp(mod_diab_educinc_noins_oth$coefficients),exp(confint(mod_diab_educinc_noins_oth)),coef(summary(mod_diab_educinc_noins_oth))[,"Pr(>|t|)"],
                                               exp(mod_diab_educinc_noins_oth11$coefficients),exp(confint(mod_diab_educinc_noins_oth11)),coef(summary(mod_diab_educinc_noins_oth11))[,"Pr(>|t|)"],
                                               exp(mod_obes_educinc_noins_all$coefficients),exp(confint(mod_obes_educinc_noins_all)),coef(summary(mod_obes_educinc_noins_all))[,"Pr(>|t|)"],
                                               exp(mod_obes_educinc_noins_all_raceadj$coefficients)[1:8],exp(confint(mod_obes_educinc_noins_all_raceadj))[1:8,1:2],coef(summary(mod_obes_educinc_noins_all_raceadj))[,"Pr(>|t|)"][1:8],
                                               exp(mod_obes_educinc_noins_nhw$coefficients),exp(confint(mod_obes_educinc_noins_nhw)),coef(summary(mod_obes_educinc_noins_nhw))[,"Pr(>|t|)"],
                                               exp(mod_obes_educinc_noins_nhb$coefficients),exp(confint(mod_obes_educinc_noins_nhb)),coef(summary(mod_obes_educinc_noins_nhb))[,"Pr(>|t|)"],
                                               exp(mod_obes_educinc_noins_mex$coefficients),exp(confint(mod_obes_educinc_noins_mex)),coef(summary(mod_obes_educinc_noins_mex))[,"Pr(>|t|)"],
                                               exp(mod_obes_educinc_noins_his$coefficients),exp(confint(mod_obes_educinc_noins_his)),coef(summary(mod_obes_educinc_noins_his))[,"Pr(>|t|)"],
                                               exp(mod_obes_educinc_noins_nha$coefficients),exp(confint(mod_obes_educinc_noins_nha)),coef(summary(mod_obes_educinc_noins_nha))[,"Pr(>|t|)"],
                                               exp(mod_obes_educinc_noins_oth$coefficients),exp(confint(mod_obes_educinc_noins_oth)),coef(summary(mod_obes_educinc_noins_oth))[,"Pr(>|t|)"],
                                               exp(mod_obes_educinc_noins_oth11$coefficients),exp(confint(mod_obes_educinc_noins_oth11)),coef(summary(mod_obes_educinc_noins_oth11))[,"Pr(>|t|)"],stringsAsFactors = FALSE)

fill <- c(999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,
          999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999)
mods_noins <- cbind(rbind(mod_diabob_educcont_noins_allrace[7,],fill),
                    rbind(fill,mod_diabob_inc2pov_noins_allrace[7,]),
                    mod_diabob_educinc_noins_allrace[7:8,])

write.csv(mods_noins, 'C:/Users/sarac/OneDrive/Documents/Research/Area v Individual SES/aou_educinc_noins_combmodels.csv',row.names=TRUE)

#r2 for models without insurance
r2_educcont_noins <- rbind(NagelkerkeR2(mod_diab_educcont_noins_all)$R2,NagelkerkeR2(mod_diab_educcont_noins_all_raceadj)$R2,NagelkerkeR2(mod_diab_educcont_noins_nhw)$R2,
                           NagelkerkeR2(mod_diab_educcont_noins_nhb)$R2,NagelkerkeR2(mod_diab_educcont_noins_mex)$R2,NagelkerkeR2(mod_diab_educcont_noins_his)$R2,
                           NagelkerkeR2(mod_diab_educcont_noins_nha)$R2,NagelkerkeR2(mod_diab_educcont_noins_oth)$R2,NagelkerkeR2(mod_diab_educcont_noins_oth11)$R2,
                           NagelkerkeR2(mod_obes_educcont_noins_all)$R2,NagelkerkeR2(mod_obes_educcont_noins_all_raceadj)$R2,NagelkerkeR2(mod_obes_educcont_noins_nhw)$R2,
                           NagelkerkeR2(mod_obes_educcont_noins_nhb)$R2,NagelkerkeR2(mod_obes_educcont_noins_mex)$R2,
                           NagelkerkeR2(mod_obes_educcont_noins_his)$R2,NagelkerkeR2(mod_obes_educcont_noins_nha)$R2,NagelkerkeR2(mod_obes_educcont_noins_oth)$R2,
                           NagelkerkeR2(mod_obes_educcont_noins_oth11)$R2)
r2_inc2pov <- rbind(NagelkerkeR2(mod_diab_inc2pov_noins_all)$R2,NagelkerkeR2(mod_diab_inc2pov_noins_all_raceadj)$R2,NagelkerkeR2(mod_diab_inc2pov_noins_nhw)$R2,
                    NagelkerkeR2(mod_diab_inc2pov_noins_nhb)$R2,NagelkerkeR2(mod_diab_inc2pov_noins_mex)$R2,NagelkerkeR2(mod_diab_inc2pov_noins_his)$R2,
                    NagelkerkeR2(mod_diab_inc2pov_noins_nha)$R2,NagelkerkeR2(mod_diab_inc2pov_noins_oth)$R2,NagelkerkeR2(mod_diab_inc2pov_noins_oth11)$R2,
                    NagelkerkeR2(mod_obes_inc2pov_noins_all)$R2,NagelkerkeR2(mod_obes_inc2pov_noins_all_raceadj)$R2,NagelkerkeR2(mod_obes_inc2pov_noins_nhw)$R2,
                    NagelkerkeR2(mod_obes_inc2pov_noins_nhb)$R2,NagelkerkeR2(mod_obes_inc2pov_noins_mex)$R2,
                    NagelkerkeR2(mod_obes_inc2pov_noins_his)$R2,NagelkerkeR2(mod_obes_inc2pov_noins_nha)$R2,NagelkerkeR2(mod_obes_inc2pov_noins_oth)$R2,
                    NagelkerkeR2(mod_obes_inc2pov_noins_oth11)$R2)
r2_educinc_noins <- rbind(NagelkerkeR2(mod_diab_educinc_noins_all)$R2,NagelkerkeR2(mod_diab_educinc_noins_all_raceadj)$R2,NagelkerkeR2(mod_diab_educinc_noins_nhw)$R2,
                          NagelkerkeR2(mod_diab_educinc_noins_nhb)$R2,NagelkerkeR2(mod_diab_educinc_noins_mex)$R2,NagelkerkeR2(mod_diab_educinc_noins_his)$R2,
                          NagelkerkeR2(mod_diab_educinc_noins_nha)$R2,NagelkerkeR2(mod_diab_educinc_noins_oth)$R2,NagelkerkeR2(mod_diab_educinc_noins_oth11)$R2,
                          NagelkerkeR2(mod_obes_educinc_noins_all)$R2,NagelkerkeR2(mod_obes_educinc_noins_all_raceadj)$R2,NagelkerkeR2(mod_obes_educinc_noins_nhw)$R2,
                          NagelkerkeR2(mod_obes_educinc_noins_nhb)$R2,NagelkerkeR2(mod_obes_educinc_noins_mex)$R2,NagelkerkeR2(mod_obes_educinc_noins_his)$R2,
                          NagelkerkeR2(mod_obes_educinc_noins_nha)$R2,NagelkerkeR2(mod_obes_educinc_noins_oth)$R2,NagelkerkeR2(mod_obes_educinc_noins_oth11)$R2)
results_educinc_noins_combined <- cbind(rbind(mod_diabob_educcont_noins_allrace[7:11,1:72], fill),
                                        rbind(mod_diabob_inc2pov_noins_allrace[7:10,1:72],fill,mod_diabob_inc2pov_noins_allrace[11,1:72]),
                                        mod_diabob_educinc_noins_allrace[7:12,])
dim(results_educinc_noins_combined)
r2_educinc_noins_combined <- cbind(r2_educcont_noins, r2_inc2pov, r2_educinc_noins)



#####sub-analysis of outcome 1c (diabetes control) - mediation#####
mediation by # visits/regular HC center, med use





