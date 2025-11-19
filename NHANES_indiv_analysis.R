#####NHANES INDIVIDUAL SES ANALYSIS - with SVYGLM#####


#rm(list=ls())
#rm(pgs, pgs_dm2, pgs_old, pgs_old_dm2,newdmdata, insur, insur_df, insur_full,chf)
setwd('C:/Users/sarac/OneDrive/Documents/Research')
#update.packages()
library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggridges)
library(RColorBrewer)
library(nhanesA)
library(progress)
library(forestplot)
library(ggpubr)
library(survey)
library(metafor)
library(tableone)
library(jtools)
library(remotes)
library(haven)
library(fmsb)
library(lmtest)
#install.packages('sandwich')
library(sandwich)
#install.packages('sociome')
#library(sociome)

#####combine files#####
#d <- read.csv(file='C:/Users/sarac/OneDrive/Documents/Research/Area v Individual SES/NHANES and ACS cleaning/nhanes_demog_table.csv', header=TRUE, sep=',')
#e <- read.csv(file='C:/Users/sarac/OneDrive/Documents/Research/Area v Individual SES/NHANES and ACS cleaning/nhanes_exam_table.csv', header=TRUE, sep=',')
#q <- read.csv(file='C:/Users/sarac/OneDrive/Documents/Research/Area v Individual SES/NHANES and ACS cleaning/nhanes_quest_table.csv', header=TRUE, sep=',')
#l <- read.csv(file='C:/Users/sarac/OneDrive/Documents/Research/Area v Individual SES/NHANES and ACS cleaning/nhanes_lab_table.csv', header=TRUE, sep=',')
d <- read.csv(file='//Cifs2/lookaheadses$/NHANES_99_18/nhanes_demog_table.csv', header=TRUE, sep=',')
e <- read.csv(file='//Cifs2/lookaheadses$/NHANES_99_18/nhanes_exam_table.csv', header=TRUE, sep=',')
q <- read.csv(file='//Cifs2/lookaheadses$/NHANES_99_18/nhanes_quest_table.csv', header=TRUE, sep=',')
l <- read.csv(file='//Cifs2/lookaheadses$/NHANES_99_18/nhanes_lab_table.csv', header=TRUE, sep=',')
de <- merge(d,e, by='SEQN', all.x=T, all.y=T, sort=T)
deq <- merge (de,q, by='SEQN', all.x=T, all.y=T, sort=T)
nhanes <- merge (deq,l, by='SEQN', all.x=T, all.y=T, sort=T)
dim(nhanes)
table(nhanes$SDMVPSU, exclude=NULL)
rm(d,e,q,l,de,deq)


#####Variable cleaning#####
nhanes$sample <- 1
nhanes$survyr <- nhanes$survyr.x
nhanes$agecat <- ifelse(nhanes$age<=29,'2', 
                        ifelse(nhanes$age<=39,'3', 
                        ifelse(nhanes$age<=49, '4',
                        ifelse(nhanes$age<=59,'5',
                        ifelse(nhanes$age<=69,'6',
                        ifelse(nhanes$age<=79, '7', '8+'))))))
table(nhanes$agecat)
nhanes$racecat2 <- ifelse(nhanes$racecat=='NHW','NHW', 
                          ifelse(nhanes$racecat=='NHB','NHB', 
                          ifelse(nhanes$racecat=='NHAsian', 'NHAsian',
                          ifelse(nhanes$racecat=='Mexican-American','Mexican-American',
                          ifelse(nhanes$racecat=='Other Hispanic','Other Hispanic',
                          ifelse(nhanes$survyr<2010, "Other/Multi-Racial including NHA (pre-2011)", "Other/Multi-Racial (post-2011)"))))))
table(nhanes$racecat, nhanes$racecat2, exclude=NULL)
nhanes$racecat <- nhanes$racecat2
nhanes$diab_any <- ifelse(nhanes$told_diab==1 | nhanes$insulin==1 | nhanes$oad==1 | nhanes$oad2==1 | nhanes$LBXGH >6.4 | nhanes$LBXGLU >125 | nhanes$LB2GLU > 199 | nhanes$LB2SGL > 199, 1, 0)
table(nhanes$told_diab, nhanes$oad2, exclude=NULL)
nhanes$diab_any <- ifelse(is.na(nhanes$diab_any) | nhanes$diab_any=='<NA>',0,1)
nhanes$all_diab_var_na <- ifelse(is.na(nhanes$told_diab) & is.na(nhanes$oad) & is.na(nhanes$oad2) & is.na(nhanes$LBXGH) & is.na(nhanes$LBXGLU) & is.na(nhanes$LB2GLU) & is.na(nhanes$LB2SGL), 1, 0)
table(nhanes$all_diab_var_na, exclude=NULL)

table(nhanes$SDMVPSU, exclude=NULL)

nhanes$oad_ever <- ifelse(nhanes$oad==1 | nhanes$oad2 ==1, 1, 0)
nhanes$oad_ever <- ifelse(is.na(nhanes$oad_ever),0,1)
nhanes$age_diab_lt30 <-ifelse(nhanes$age_diab2<30,1,0)
table(nhanes$age_diab_lt30, exclude=NULL)
nhanes$age_diab_lt30 <- ifelse(is.na(nhanes$age_diab_lt30),999,nhanes$age_diab_lt30)
nhanes$insulin <- ifelse(is.na(nhanes$insulin) | nhanes$insulin=='<NA>',0, ifelse(nhanes$insulin==1,1,0))
table(nhanes$diab_any[nhanes$oad_ever==0], nhanes$insulin[nhanes$oad_ever==0], exclude=NULL)
nhanes$t1d_poss <- ifelse(nhanes$diab_any==1 & nhanes$age_diab_lt30==1 & nhanes$insulin==1 & nhanes$oad_ever==0, 1, 0)
table(nhanes$diab_any, nhanes$t1d_poss, exclude=NULL)
nhanes$diab <- ifelse(nhanes$diab_any==1 & !nhanes$t1d_poss==1,1,0)
table(nhanes$diab_any, nhanes$diab, exclude=NULL)

nhanes$diab_known <- ifelse(nhanes$told_diab==1 | nhanes$insulin==1 | nhanes$oad==1 | nhanes$oad2==1, 1, 0) ###need to add a1c when lab data ready
nhanes$diab_known <- ifelse(is.na(nhanes$diab_known) | nhanes$diab_known=='<NA>',0,1)
table(nhanes$told_diab, nhanes$insulin, exclude=NULL)
table(nhanes$diab_known, nhanes$diab, exclude=NULL)
summary(nhanes$LBXGH[nhanes$diab==1])
nhanes$diab_controlled <- ifelse(nhanes$LBXGH <=7, 1, 0)
nhanes$diab_controlled <- ifelse(is.na(nhanes$diab_controlled) | nhanes$diab_controlled=='<NA>',NA,nhanes$diab_controlled)
table(nhanes$diab_controlled, exclude=NULL)
nhanes$diab_controlled8 <- ifelse(nhanes$LBXGH <=8, 1, 0)
nhanes$diab_controlled8 <- ifelse(is.na(nhanes$diab_controlled) | nhanes$diab_controlled=='<NA>',NA,nhanes$diab_controlled8)
prop.table(table(nhanes$diab_known, nhanes$diab_controlled, exclude=NULL),1)
nhanes$bmi_calc <- nhanes$BMXWT/(nhanes$BMXHT/100)^2
hist(nhanes$bmi_calc)
table(nhanes$racecat)
nhanes$bmi <- ifelse(!is.na(nhanes$BMXBMI), nhanes$BMXBMI, nhanes$bmi_calc)
summary(nhanes$bmi)
nhanes$obese_bmi <- ifelse(nhanes$BMXBMI >= 30 | nhanes$bmi_calc >=30, 1, 0)
nhanes$obese_bmi2 <- ifelse(nhanes$racecat=='NHAsian' & nhanes$BMXBMI >= 27.5, 1, ifelse(nhanes$racecat=='NHAsian' & nhanes$bmi_calc >= 27.5, 1, nhanes$obese_bmi))
table(nhanes$obese_bmi, nhanes$obese_bmi2, exclude=NULL)
nhanes$obese <- ifelse(nhanes$told_overwt==1 | nhanes$told_overwt2==1 | nhanes$took_wtlossrx==1 | nhanes$obese_bmi2==1, 1, 0)
nhanes$obese <- ifelse(is.na(nhanes$obese), 0, ifelse(nhanes$obese==1,1,0))
nhanes$all_ob_var_na <- ifelse(is.na(nhanes$told_overwt) & is.na(nhanes$told_overwt2)& is.na(nhanes$took_wtlossrx)& is.na(nhanes$obese_bmi2),1,0)
table(nhanes$all_ob_var_na, exclude=NULL)
nhanes$obese_known <- ifelse(nhanes$told_overwt==1 | nhanes$told_overwt2==1 | nhanes$took_wtlossrx==1, 1, 0)
nhanes$obese_known <- ifelse(is.na(nhanes$obese_known), 0, ifelse(nhanes$obese_known==1,1,0))
table(nhanes$obese, nhanes$obese_known, exclude=NULL)
nhanes$cvd <- ifelse(nhanes$told_chd==1 | nhanes$told_angina==1 | nhanes$told_cva==1 | nhanes$told_mi==1,1,0)
nhanes$cvd <- ifelse(is.na(nhanes$cvd), 0, ifelse(nhanes$cvd==1,1,0))
table(nhanes$cvd)
nhanes$copd <- ifelse(nhanes$told_chronbronch==1 | nhanes$told_copd==1 | nhanes$told_emphy==1,1,0)
nhanes$copd <- ifelse(is.na(nhanes$copd), 0, ifelse(nhanes$copd==1,1,0))
table(nhanes$copd, exclude=NULL)
nhanes$creatinine <- ifelse(is.na(nhanes$LB2SCR) & is.na(nhanes$LBDSCR), nhanes$LBXSCR,
                            ifelse(is.na(nhanes$LB2SCR), nhanes$LBDSCR, nhanes$LB2SCR))
summary(nhanes$creatinine)
length(unique(nhanes$SEQN[nhanes$creatinine>=1.4]))
nhanes$ckd <- ifelse(nhanes$told_kidfail==1 | nhanes$told_kidfail2==1 | nhanes$creatinine >=1.4 | nhanes$hd_pastyr==1, 1, 0)
nhanes$ckd <- ifelse(is.na(nhanes$ckd), 0, ifelse(nhanes$ckd==1,1,0))
table(nhanes$ckd, exclude=NULL)
nhanes$mdd <- ifelse(nhanes$depress_yesno==1 | nhanes$med_depress==1 | nhanes$phq9 >=10, 1, 0)
nhanes$mdd <- ifelse(is.na(nhanes$mdd), 0, ifelse(nhanes$mdd==1,1,0))
table(nhanes$mdd, exclude=NULL)
nhanes$sbp_mean <- rowMeans(nhanes[,c('BPXSY1','BPXSY2','BPXSY3','BPXSY4')], na.rm=T)
nhanes$dbp_mean <- rowMeans(nhanes[,c('BPXDI1','BPXDI2','BPXDI3','BPXDI4')], na.rm=T)
summary(nhanes$sbp_mean)
nhanes$htn <- ifelse(nhanes$told_htn==1 | nhanes$med_htn==1 | nhanes$sbp_mean >=140 | nhanes$dbp_mean>=90, 1, 0)
nhanes$htn <- ifelse(is.na(nhanes$htn), 0, ifelse(nhanes$htn==1,1,0))
table(nhanes$htn, exclude=NULL)

table(nhanes$SDMVPSU, exclude=NULL)

table(nhanes$hreduc, nhanes$survyr, exclude=NULL)
nhanes$educ_simple <- ifelse(nhanes$hreduc=='College Degree or Higher','College Degree', 
                             ifelse(nhanes$hreduc=='High School Degree or GED or Some College or Associates Degree' | nhanes$hreduc=='High School Degree or GED' | nhanes$hreduc=='Some College or Associates Degree', 'HS Degree',
                                    ifelse(nhanes$hreduc=='9-11th Grade' | nhanes$hreduc=='Less Than High School Degree','Some HS', ifelse(nhanes$hreduc=='Less Than 9th Grade','No HS', NA))))
table(nhanes$hreduc, nhanes$educ_simple, exclude=NULL)
nhanes$educ_num <- ifelse(nhanes$hreduc=='College Degree or Higher',7, ifelse(nhanes$hreduc=='Some College or Associates Degree',6, ifelse(nhanes$hreduc=='High School Degree or GED or Some College or Associates Degree',5,
                    ifelse(nhanes$hreduc=='High School Degree or GED',4, ifelse(nhanes$hreduc=='9-11th Grade',3, ifelse(nhanes$hreduc=='Less Than High School Degree',2, ifelse(nhanes$hreduc=='Less Than 9th Grade',1, NA)))))))
boxplot_indiveduc <- ggplot(nhanes, aes(factor(x=racecat), y=educ_num, fill=factor(diab)), exclude=TRUE) + geom_boxplot(notch=T)

nhanes$inc_simple <- ifelse(nhanes$hhinc=="$0-4,999" | nhanes$hhinc=="$5,000-9,999" | nhanes$hhinc=="$10,000-14,999" | nhanes$hhinc=="$15,000-19,999" | nhanes$hhinc=="Under $20,000" | nhanes$hhinc=="$20,000-24,999", 'Under $20,000',
                            ifelse(nhanes$hhinc=="Over $20,000" | nhanes$hhinc=="$25,000-34,999" | nhanes$hhinc=="$35,000-44,999", '$20,000-44,999', 
                                   ifelse(nhanes$hhinc=="$45,000-54,999" | nhanes$hhinc=="$55,000-64,999" | nhanes$hhinc=="$65,000-74,999", '$45,000-74,999',
                                          ifelse(nhanes$hhinc=="$75,000-99,999" | nhanes$hhinc=="$75,000 or more", "More than $75,000", NA))))
table(nhanes$hhinc, nhanes$survyr, exclude=NULL)
table(nhanes$inc_simple, exclude=NULL)
nhanes$racecat <- factor(nhanes$racecat, levels=c("NHW","NHB","Mexican-American","Other Hispanic","NHAsian","Other/Multi-Racial including NHA (pre-2011)", "Other/Multi-Racial (post-2011)"))
nhanes$educ_simple <- factor(nhanes$educ_simple, levels=c("College Degree","HS Degree","Some HS","No HS"))
nhanes$hreduc <- factor(nhanes$hreduc, levels=c("Less Than 9th Grade","Less Than High School Degree", "9-11th Grade",  "High School Degree or GED", "High School Degree or GED or Some College or Associates Degree", "Some College or Associates Degree", "College Degree or Higher", "Do not know", "Missing", "Refused"))
nhanes$hhinc <- factor(nhanes$hhinc, levels=c("$100,000 or more","$75,000-99,999","$75,000 or more","$65,000-74,999","$55,000-64,999","$45,000-54,999","$35,000-44,999","$25,000-34,999","$20,000-24,999","Over $20,000","Under $20,000","$15,000-19,999","$10,000-14,999","$5,000-9,999","$0-4,999","Missing"))
nhanes$inc_simple <- factor(nhanes$inc_simple, levels=c("More than $75,000","$45,000-74,999","$20,000-44,999","Under $20,000"))
table(nhanes$hhinc)
nhanes$work_yn <- ifelse(nhanes$working=='Working',1, ifelse(nhanes$working=='Looking for work' | nhanes$working=='Not Working',0, NA))
table(nhanes$working, nhanes$work_yn, exclude=NULL)

table(nhanes$SDMVPSU, exclude=NULL)

nhanes$insur_now <- ifelse(is.na(nhanes$insur_now) | nhanes$insur_now=='<NA>','Missing', nhanes$insur_now)
table(nhanes$insur_now, exclude=NULL)
nhanes$insur_now2 <- ifelse(is.na(nhanes$insur_now2) | nhanes$insur_now2=='<NA>','Missing', nhanes$insur_now2)
nhanes$insur_private <- ifelse(is.na(nhanes$insur_private) | nhanes$insur_private=='<NA>','Missing', nhanes$insur_private)
nhanes$insur_private2 <- ifelse(is.na(nhanes$insur_private2) | nhanes$insur_private2=='<NA>','Missing', nhanes$insur_private2)
nhanes$insur_curr <- ifelse(nhanes$insur_now=='Missing', nhanes$insur_now2, nhanes$insur_now)
nhanes$insur_priv_intermedvar <- ifelse(nhanes$insur_private=='Missing', nhanes$insur_private2, nhanes$insur_private)
nhanes$insur_priv <- ifelse(nhanes$insur_priv_intermedvar==1 | nhanes$insur_priv_intermedvar==14, 1, nhanes$insur_priv_intermedvar)
table(nhanes$insur_now, nhanes$insur_curr, exclude=NULL)
table(nhanes$insur_private, nhanes$insur_priv, exclude=NULL)
table(nhanes$insur_curr, nhanes$insur_priv, exclude=NULL)
nhanes$insur_stability <- ifelse(nhanes$insur_curr==1 & nhanes$uninsur_pastyr==2 & nhanes$insur_priv==1, 'Stably Insured-Private',
                                 ifelse(nhanes$insur_curr==1 & nhanes$uninsur_pastyr==2 & !nhanes$insur_priv==1, 'Stably Insured-Other',
                                        ifelse(nhanes$insur_curr==1 & !nhanes$uninsur_pastyr==2 & nhanes$insur_priv==1, 'Unstably Insured-Private',
                                               ifelse(nhanes$insur_curr==1 & !nhanes$uninsur_pastyr==2 & !nhanes$insur_priv==1, 'Unstably Insured-Other', 
                                                      ifelse(nhanes$insur_curr==2, 'Uninsured', 'Missing')))))
nhanes$insur_stability <- ifelse(is.na(nhanes$insur_stability) | nhanes$insur_stability=='<NA>','Missing',nhanes$insur_stability)
table(nhanes$insur_stability, exclude=NULL)
addmargins(table(nhanes$insur_curr, nhanes$insur_stability, exclude=NULL)) #a few with NAs for uninsur_pastyr are listed as missing even though they are insured
addmargins(table(nhanes$insur_curr, nhanes$insur_priv))
nhanes$insur_stability <- factor(nhanes$insur_stability, levels=c("Stably Insured-Private","Stably Insured-Other","Unstably Insured-Private","Unstably Insured-Other","Uninsured","Missing"))

table(nhanes$SDMVPSU, exclude=NULL)
dim(nhanes)
nhanes$all_ses_na <- ifelse(is.na(nhanes$educ_num) & is.na(nhanes$inc_to_pov),1,0)
addmargins(table(nhanes$all_ses_na, exclude=NULL))

nhanes$smoke_status <- ifelse(nhanes$smoke_ever==2, 'Never Smoker', ifelse(nhanes$smoke_current==1|nhanes$smoke_current==2, 'Current Smoker', ifelse(nhanes$smoke_ever==1 & nhanes$smoke_current==3, 'Former Smoker', 'Missing')))
table(nhanes$smoke_ever, nhanes$smoke_status, exclude=NULL)
table(nhanes$smoke_status, nhanes$age_ge_16, exclude=NULL)
nhanes$smoke_status <- factor(nhanes$smoke_status, levels=c("Never Smoker","Former Smoker","Current Smoker","Missing"))
#flow diagram
dim(nhanes)
nhanes_nokids <- nhanes[nhanes$age>=18,]
dim(nhanes_nokids)
nhanes_nokids_fulldemog <- nhanes_nokids[!is.na(nhanes_nokids$age) & !is.na(nhanes_nokids$female) & !is.na(nhanes_nokids$racecat),]
dim(nhanes_nokids_fulldemog)
nhanes_nokids_fulldemog_fullses <- nhanes_nokids_fulldemog[nhanes_nokids_fulldemog$all_ses_na==0,]
dim(nhanes_nokids_fulldemog_fullses)
nhanes_nokids_fulldemog_fullses_dmob <- nhanes_nokids_fulldemog_fullses[nhanes_nokids_fulldemog_fullses$all_diab_var_na==0 & nhanes_nokids_fulldemog_fullses$all_ob_var_na==0,]
dim(nhanes_nokids_fulldemog_fullses_dmob)
table(nhanes_nokids_fulldemog_fullses_dmob$smoke_status, nhanes_nokids_fulldemog_fullses_dmob$insur_stability, exclude=NULL)
nhanes_nokids_fulldemog_fullses_dmob_fullcov <- nhanes_nokids_fulldemog_fullses_dmob[!nhanes_nokids_fulldemog_fullses_dmob$smoke_status=='Missing' & !nhanes_nokids_fulldemog_fullses_dmob$insur_stability=='Missing',]
dim(nhanes_nokids_fulldemog_fullses_dmob_fullcov)
#rm(nhanes_nokids, nhanes_nokids_fulldemog,nhanes_nokids_fulldemog_fullses,nhanes_nokids_fulldemog_fullses_dmob,nhanes_nokids_fulldemog_fullses_dmob_fullcov)

table(nhanes$SDMVPSU, exclude=NULL)
#nhanes <- nhanes[!nhanes$smoke_status=='Missing',]


nhanes$alc_daysperyr <- ifelse(nhanes$alc_freq_units==3, nhanes$alc_numdays, 
                                 ifelse(nhanes$alc_freq_units==2, 12*nhanes$alc_numdays,
                                        ifelse(nhanes$alc_freq_units==1, 52*nhanes$alc_numdays, NA)))
nhanes$alc_drinksperwk <- nhanes$alc_drinksday * nhanes$alc_daysperyr/52
summary(nhanes$alc_drinksperwk)


#need to work on cleaning physical activity data 2007-2018
physact_vars <- c("act_vigwork_yn","act_vigwork_days","act_vigwork_minperday","act_modwork_yn","act_modwork_days","act_modwork_minperday",
                  "act_vigrec_yn","act_vigrec_days","act_vigrec_minperday","act_modrec_yn","act_modrec_days","act_modwork_minperday",
                  "act_commute_yn","act_commute_days","act_commute_minperday")
nhanes$act_vigwork_days <- ifelse(nhanes$act_vigwork_yn==2,0, nhanes$act_vigwork_days)
nhanes$metspermo0718 <- 52/12*(ifelse(nhanes$act_vigwork_yn==1,1,0)*(nhanes$act_vigwork_days*nhanes$act_vigwork_minperday)*8 +
                                 ifelse(nhanes$act_modwork_yn==1,1,0)*(nhanes$act_modwork_days*nhanes$act_modwork_minperday)*4 +
                                 ifelse(nhanes$act_commute_yn==1,1,0)*(nhanes$act_commute_days*nhanes$act_commute_minperday)*4 + 
                                 ifelse(nhanes$act_vigrec_yn==1,1,0)*(nhanes$act_vigrec_days*nhanes$act_vigrec_minperday)*8 + 
                                 ifelse(nhanes$act_modrec_yn==1,1,0)*(nhanes$act_modrec_days*nhanes$act_modrec_minperday)*4)
summary(nhanes$metspermo0718)
dim(nhanes)

####correlation between exposures#####

cor.test(nhanes[nhanes$age>=18,]$educ_num, nhanes[nhanes$age>=18,]$inc_to_pov) #pearson's 0.446222
ggplot(nhanes[!is.na(nhanes$educ_num) & !is.na(nhanes$inc_to_pov) & nhanes$age>=18,], aes(x=racecat, y=inc_to_pov, fill=as.factor(educ_num))) + geom_violin() + 
  labs(x='', y='Income to Poverty Ratio', title='Education-Income Relationship', fill="Educational \n Attainment")

tiff(file='C:/Users/sarac/OneDrive/Documents/Research/Area v Individual SES/NHANES_SES and AoU_SES/nhanes_boxplot_educxinc_racestrat.tiff', width=8, height=3.5, units='in', res=300)
ggplot(nhanes[!is.na(nhanes$educ_num) & !is.na(nhanes$inc_to_pov) & nhanes$age>=18,], aes(x=racecat, y=inc_to_pov, fill=as.factor(educ_num))) + geom_boxplot() + 
  labs(x='Race/Ethnicity', y='Income to Poverty Ratio', title='Education-Income Relationship', fill="Educational Attainment") + 
  scale_fill_discrete(labels=c("Less Than 9th Grade", "9-11th Grade", "Less Than High School Degree", "High School Degree or GED", "High School Degree or GED or \n Some College or Associates Degree", "Some College or Associates Degree", "College Degree or Higher")) +
  scale_x_discrete(labels=c('NHW','NHB','Mex','His','NHA','Oth/Multi \n (pre-2011)','Oth/Multi \n (post-2011)')) + theme(axis.text.x=element_text(angle=30))#, vjust=0.005))
dev.off()

######survey design objects#####
options(survey.lonely.psu = 'adjust')
table(nhanes$SDMVPSU, exclude=NULL)
nhanes <- nhanes[!is.na(nhanes$SDMVPSU),]
nhanes$nhanes_Weights <- ifelse(nhanes$survyr==1999 | nhanes$survyr==2001, nhanes$WTMEC4YR*0.2, nhanes$WTMEC2YR*0.1)
#nhanes$weights_quest <- 
#nhanes$weights_lab
summary(nhanes$SDMVPSU)
surv_all_withkids <- svydesign(id = ~SDMVPSU, strata = ~ SDMVSTRA, weights = ~nhanes_Weights, data=nhanes, nest=T)
#surv_all <- subset(surv_all_withkids, age>=18 & !nhanes$insur_stability=='Missing')
#surv_all <- svydesign(id = ~SDMVPSU, strata = ~ SDMVSTRA, weights = ~weights, data=nhanes[nhanes$age >= 18 & !nhanes$insur_stability=='Missing',], nest=T)
#diab_adj <- svyglm(design=surv_all, obese ~ age + female + survyr + racecat, family=quasibinomial())#family=stats::gaussian())
#nhanes$pred_prev_diab <- predict(diab_adj, newdata=nhanes, type='response')
#obese_adj <- svyglm(design=surv_all, obese ~ age + female + survyr + racecat, family=quasibinomial())#family=stats::gaussian())
#nhanes$pred_prev_obese <- predict(obese_adj, newdata=nhanes, type='response')
#summary(nhanes$pred_prev_obese)
#mean(nhanes$pred_prev_diab)
#svymean(~pred_prev_diab, surv_all)
dim(nhanes)
dim(surv_all_withkids)
surv_all <- subset(surv_all_withkids, age>=18 & !is.na(nhanes$female) & !is.na(nhanes$racecat) & !is.na(nhanes$age) & !nhanes$insur_stability=='Missing' & nhanes$all_ses_na==0 & nhanes$all_diab_var_na==0 & nhanes$all_ob_var_na==0 & !nhanes$smoke_status=='Missing')
dim(surv_all)
surv_nhw <- subset(surv_all, racecat=='NHW')
surv_nhb <- subset(surv_all, racecat=='NHB')
surv_nha <- subset(surv_all, racecat=='NHAsian')
surv_mex <- subset(surv_all, racecat=='Mexican-American')
surv_his <- subset(surv_all, racecat=='Other Hispanic')
surv_oth <- subset(surv_all, racecat=='Other/Multi-Racial including NHA (pre-2011)')
surv_oth11 <- subset(surv_all, racecat=='Other/Multi-Racial (post-2011)')
dim(surv_all)
###do not use below - should not subset the data prior to/as part of svydesign
#surv_nhw <- svydesign(id = ~SDMVPSU, strata = ~ SDMVSTRA, weights = ~weights, data=nhanes[nhanes$age >= 18 & nhanes$racecat=='NHW' & !nhanes$insur_stability=='Missing',], nest=T)
#surv_nhb <- svydesign(id = ~SDMVPSU, strata = ~ SDMVSTRA, weights = ~weights, data=nhanes[nhanes$age >= 18 & nhanes$racecat=='NHB' & !nhanes$insur_stability=='Missing',], nest=T)
#surv_nha <- svydesign(id = ~SDMVPSU, strata = ~ SDMVSTRA, weights = ~weights, data=nhanes[nhanes$age >= 18 & nhanes$racecat=='NHAsian' & !nhanes$insur_stability=='Missing',], nest=T)
#surv_mex <- svydesign(id = ~SDMVPSU, strata = ~ SDMVSTRA, weights = ~weights, data=nhanes[nhanes$age >= 18 & nhanes$racecat=='Mexican-American' & !nhanes$insur_stability=='Missing',], nest=T)
#surv_his <- svydesign(id = ~SDMVPSU, strata = ~ SDMVSTRA, weights = ~weights, data=nhanes[nhanes$age >= 18 & nhanes$racecat=='Other Hispanic' & !nhanes$insur_stability=='Missing',], nest=T)
#surv_oth <- svydesign(id = ~SDMVPSU, strata = ~ SDMVSTRA, weights = ~weights, data=nhanes[nhanes$age >= 18 & nhanes$racecat=='Other/Multi-Racial including NHA (pre-2011)' & !nhanes$insur_stability=='Missing',], nest=T)
#surv_oth11 <- svydesign(id = ~SDMVPSU, strata = ~ SDMVSTRA, weights = ~weights, data=nhanes[nhanes$age >= 18 & nhanes$racecat=='Other/Multi-Racial (post-2011)' & !nhanes$insur_stability=='Missing',], nest=T)



#####table 1#####
#unweighted - overall and age >18
nhanes2 <- nhanes[age>=18 & !is.na(nhanes$female) & !is.na(nhanes$age) & !nhanes$insur_stability=='Missing' & nhanes$all_ses_na==0,]
dim(nhanes2)

tab1_all_unweighted <- CreateTableOne(data=nhanes2, #strata='hba1c.F12_missing', 
                                              vars=c('age','age_ge_16','female','survyr','racecat','hreduc','hhinc','inc_to_pov','working','hoursworked_cat','hoursworked_num','work_35hr','insur_stability'), test=T,
                                              factorVars=c('age_ge_16','female','survyr','racecat','hreduc','hhinc','working','hoursworked_cat','work_35hr','insur_stability'))
tab1_adults_unweighted <- CreateTableOne(data=nhanes2, #strata='hba1c.F12_missing', 
                                      vars=c('age','female','survyr','racecat','hreduc','educ_num','hhinc','inc_to_pov','working','hoursworked_cat','hoursworked_num','work_35hr','insur_stability'), test=T,
                                      factorVars=c('female','survyr','racecat','hreduc','hhinc','working','hoursworked_cat','work_35hr','insur_stability'))
print(tab1_adults_unweighted, quote=T, noSpaces=T)
#weighted - overall, including kids
svymean(~age+inc_to_pov+hoursworked_num, surv_all_withkids, na.rm=T)
svysd(~age+inc_to_pov+hoursworked_num, surv_all_withkids, na.rm=T) #can also use sqrt(svyvar())
svytotal(~age_ge_16+female+as.factor(survyr)+racecat+hreduc+hhinc+working+hoursworked_cat+work_35hr, surv_all_withkids, na.rm=T)
svymean(~age_ge_16+female+as.factor(survyr)+racecat+hreduc+hhinc+working+hoursworked_cat+work_35hr, surv_all_withkids, na.rm=T)
tab1_all_weighted <- svyCreateTableOne(data=surv_all_withkids, #strata='hba1c.F12_missing', 
                                              vars=c('age','age_ge_16','female','survyr','racecat','hreduc','hhinc','inc_to_pov','working','hoursworked_cat','hoursworked_num','work_35hr'), test=T,
                                              factorVars=c('age_ge_16','female','survyr','racecat','hreduc','hhinc','working','hoursworked_cat','work_35hr'))

#weighted - overall, study sample
svytotal(~sample, surv_all, na.rm=T)
svymean(~age+inc_to_pov+hoursworked_num+educ_num, surv_all, na.rm=T)
svysd(~age+inc_to_pov+hoursworked_num+educ_num, surv_all, na.rm=T) #can also use sqrt(svyvar())
svytotal(~age_ge_16+female+as.factor(survyr)+racecat+hreduc+hhinc+working+hoursworked_cat+work_35hr+insur_stability, surv_all, na.rm=T)
svymean(~age_ge_16+female+as.factor(survyr)+racecat+hreduc+hhinc+working+hoursworked_cat+work_35hr+insur_stability, surv_all, na.rm=T)
#does not work - "error in summary.factor...'sum' not meaningful for factors"; tried changing weights variable name (as this is a widely reported fix) without success
#tab1_adults_weighted <- svyCreateTableOne(data=surv_all, #strata='racecat', 
                                         vars=c('age','female','survyr','racecat','hreduc','educ_num','hhinc','inc_to_pov','working'), test=T,
                                         factorVars=c('female','survyr','racecat','hreduc','hhinc','working'))
#print(tab1_adults_weighted, quote=T, noSpaces=T)


#tab1_adults_weighted_racestrat <- svyCreateTableOne(data=surv_all, strata='racecat', 
                                          vars=c('age','female','survyr','racecat','hreduc','educ_num','hhinc','inc_to_pov','working'), test=T,
                                          factorVars=c('female','survyr','racecat','hreduc','hhinc','working'))
#print(tab1_adults_weighted_racestrat, quote=T, noSpaces=T)

#tab1 - strat by race
svymean(~age+inc_to_pov+educ_num, surv_nhw, na.rm=T)
svysd(~age+inc_to_pov+educ_num, surv_nhw, na.rm=T) #can also use sqrt(svyvar())
svytotal(~female+as.factor(survyr)+racecat+hreduc+hhinc+insur_stability+diab+obese, surv_nhw, na.rm=T)
svymean(~female+as.factor(survyr)+racecat+hreduc+hhinc+insur_stability+diab+obese, surv_nhw, na.rm=T)
tab1_strat <- rbind(c('racecat','mean(cont) or total(cat)','SE','SD(cont) or mean(cat)','blank(cont) or SE(cat)'),
                    as.matrix(cbind('All',as.data.frame(svymean(~age+inc_to_pov+educ_num, surv_all, na.rm=T)), svysd(~age+inc_to_pov+educ_num, surv_all, na.rm=T), 'HOLD')),
                    as.matrix(cbind('All',as.data.frame(svytotal(~female+as.factor(survyr)+racecat+hreduc+hhinc+insur_stability+diab+obese, surv_all, na.rm=T)),as.data.frame(svymean(~female+as.factor(survyr)+racecat+hreduc+hhinc+insur_stability+diab+obese, surv_all, na.rm=T)))),
                    as.matrix(cbind('NHW',as.data.frame(svymean(~age+inc_to_pov+educ_num, surv_nhw, na.rm=T)), svysd(~age+inc_to_pov+educ_num, surv_nhw, na.rm=T), 'HOLD')),
                    as.matrix(cbind('NHW',as.data.frame(svytotal(~female+as.factor(survyr)+racecat+hreduc+hhinc+insur_stability+diab+obese, surv_nhw, na.rm=T)),as.data.frame(svymean(~female+as.factor(survyr)+racecat+hreduc+hhinc+insur_stability+diab+obese, surv_nhw, na.rm=T)))),
                    as.matrix(cbind('NHB',as.data.frame(svymean(~age+inc_to_pov+educ_num, surv_nhb, na.rm=T)), svysd(~age+inc_to_pov+educ_num, surv_nhb, na.rm=T), 'HOLD')),
                    as.matrix(cbind('NHB',as.data.frame(svytotal(~female+as.factor(survyr)+racecat+hreduc+hhinc+insur_stability+diab+obese, surv_nhb, na.rm=T)),as.data.frame(svymean(~female+as.factor(survyr)+racecat+hreduc+hhinc+insur_stability+diab+obese, surv_nhb, na.rm=T)))),
                    as.matrix(cbind('MEX',as.data.frame(svymean(~age+inc_to_pov+educ_num, surv_mex, na.rm=T)), svysd(~age+inc_to_pov+educ_num, surv_mex, na.rm=T), 'HOLD')),
                    as.matrix(cbind('MEX',as.data.frame(svytotal(~female+as.factor(survyr)+racecat+hreduc+hhinc+insur_stability+diab+obese, surv_mex, na.rm=T)),as.data.frame(svymean(~female+as.factor(survyr)+racecat+hreduc+hhinc+insur_stability+diab+obese, surv_mex, na.rm=T)))),
                    as.matrix(cbind('HIS',as.data.frame(svymean(~age+inc_to_pov+educ_num, surv_his, na.rm=T)), svysd(~age+inc_to_pov+educ_num, surv_his, na.rm=T), 'HOLD')),
                    as.matrix(cbind('HIS',as.data.frame(svytotal(~female+as.factor(survyr)+racecat+hreduc+hhinc+insur_stability+diab+obese, surv_his, na.rm=T)),as.data.frame(svymean(~female+as.factor(survyr)+racecat+hreduc+hhinc+insur_stability+diab+obese, surv_his, na.rm=T)))),
                    as.matrix(cbind('NHA',as.data.frame(svymean(~age+inc_to_pov+educ_num, surv_nha, na.rm=T)), svysd(~age+inc_to_pov+educ_num, surv_nha, na.rm=T), 'HOLD')),
                    as.matrix(cbind('NHA',as.data.frame(svytotal(~female+as.factor(survyr)+racecat+hreduc+hhinc+insur_stability+diab+obese, surv_nha, na.rm=T)),as.data.frame(svymean(~female+as.factor(survyr)+racecat+hreduc+hhinc+insur_stability+diab+obese, surv_nha, na.rm=T)))),
                    as.matrix(cbind('OTH',as.data.frame(svymean(~age+inc_to_pov+educ_num, surv_oth, na.rm=T)), svysd(~age+inc_to_pov+educ_num, surv_oth, na.rm=T), 'HOLD')),
                    as.matrix(cbind('OTH',as.data.frame(svytotal(~female+as.factor(survyr)+racecat+hreduc+hhinc+insur_stability+diab+obese, surv_oth, na.rm=T)),as.data.frame(svymean(~female+as.factor(survyr)+racecat+hreduc+hhinc+insur_stability+diab+obese, surv_oth, na.rm=T)))),
                    as.matrix(cbind('OTH11',as.data.frame(svymean(~age+inc_to_pov+educ_num, surv_oth11, na.rm=T)), svysd(~age+inc_to_pov+educ_num, surv_oth11, na.rm=T), 'HOLD')),
                    as.matrix(cbind('OTH11',as.data.frame(svytotal(~female+as.factor(survyr)+racecat+hreduc+hhinc+insur_stability+diab+obese, surv_oth11, na.rm=T)),as.data.frame(svymean(~female+as.factor(survyr)+racecat+hreduc+hhinc+insur_stability+diab+obese, surv_oth11, na.rm=T)))))#, stringsAsFactors = FALSE)
#colnames(tab1_strat) <- c('racecat','mean(cont) or total(cat)','SE','SD(cont) or mean(cat)','blank(cont) or SE(cat)')
tab1_strat <- as.data.frame(tab1_strat)
dim(tab1_strat)
tab1_strat <- cbind(tab1_strat, rownames(tab1_strat))
write.csv(tab1_strat, 'C:/Users/sarac/OneDrive/Documents/Research/Area v Individual SES/NHANES_SES and AoU_SES/nhanes_tab1_stratbyrace.csv', row.names=FALSE)

#####table 2-3 and violins = distribution of SES variables by race ethnicity#####
nhanes$hreduc <- factor(nhanes$hreduc, levels=c( "College Degree or Higher", "Some College or Associates Degree","High School Degree or GED or Some College or Associates Degree", "High School Degree or GED", "9-11th Grade", "Less Than High School Degree", "Less Than 9th Grade","Do not know", "Missing", "Refused"))
table(nhanes$hreduc, nhanes$racecat)
nhanes_racexeduc <- as.data.frame(100*prop.table(table(nhanes$hreduc, nhanes$racecat),2))
tiff(file='C:/Users/sarac/OneDrive/Documents/Research/Area v Individual SES/NHANES_SES and AoU_SES/nhanes_stackbar_indiveduc_racestrat.tiff', width=7, height=5, units='in', res=300)
ggplot(nhanes_racexeduc, aes(x=Var2, y=Freq, fill=Var1)) + 
  geom_bar(position='fill', stat='identity') + ggtitle('Distribution of Individual-Level Educational Attainment \n by Race/Ethnicity of Participant') + 
  xlab('') + ylab('Proportion') + labs(fill='Educational \n Attainment') + 
  theme(legend.position='bottom', legend.text=element_text(size=4), plot.title=element_text(hjust=0.5)) +
        scale_x_discrete(labels=c('NHW','NHB','Mex','His','NHA','Oth/Multi \n (pre-2011)','Oth/Multi \n (post-2011)')) + theme(axis.text.x=element_text(angle=30))#, vjust=0.005))
dev.off()


nhanes_racexinc <- as.data.frame(100*prop.table(table(nhanes$hhinc, nhanes$racecat),2))
tiff(file='C:/Users/sarac/OneDrive/Documents/Research/Area v Individual SES/NHANES_SES and AoU_SES/nhanes_stackbar_indivinc_racestrat.tiff', width=7, height=5, units='in', res=300)
ggplot(nhanes_racexinc, aes(x=Var2, y=Freq, fill=Var1)) + 
  geom_bar(position='fill', stat='identity') + ggtitle('Distribution of Individual-Level Income \n by Race/Ethnicity of Participant') + 
  xlab('') + ylab('Proportion') + labs(fill='Income') + 
  theme(legend.position='bottom', legend.text=element_text(size=5), plot.title=element_text(hjust=0.5)) +
  scale_x_discrete(labels=c('NHW','NHB','Mex','His','NHA','Oth/Multi \n (pre-2011)','Oth/Multi \n (post-2011)')) + theme(axis.text.x=element_text(angle=30))#, vjust=0.005)))
dev.off()

nhanes$hreduc <- factor(nhanes$hreduc, levels=c("Less Than 9th Grade","Less Than High School Degree", "9-11th Grade",  "High School Degree or GED", "High School Degree or GED or Some College or Associates Degree", "Some College or Associates Degree", "College Degree or Higher", "Do not know", "Missing", "Refused"))


ggplot(nhanes, aes(factor(x=racecat), y=inc_to_pov, color=racecat), exclude=TRUE) + geom_violin(trim=TRUE) + geom_boxplot(width=0.1)
ggplot(nhanes, aes(inc_to_pov, fill=racecat)) + geom_density(alpha=0.2) + labs(x='Income-to-Poverty Ratio', fill='Race') + scale_fill_manual(values=c('lightblue2','dodgerblue1','seagreen2','green4','goldenrod','salmon','mediumpurple'))#,'firebrick3','mediumpurple'))

violin<-ggplot(nhanes, aes(factor(x=racecat), y=inc_to_pov), exclude=TRUE) + geom_violin(trim=TRUE)
violin + geom_boxplot(width=0.1)

violin<-ggplot(nhanes, aes(factor(x=racecat), y=inc_to_pov, fill=factor(diab)), exclude=TRUE) + geom_violin(position=position_dodge(1))
violin + geom_boxplot(position=position_dodge(1), width=0.1)

violin<-ggplot(nhanes[!is.na(nhanes$educ_simple),], aes(factor(x=racecat), y=inc_to_pov, fill=factor(educ_simple)), exclude=TRUE) + geom_violin(position=position_dodge(1))
violin + geom_boxplot(position=position_dodge(1), width=0.1)

nhanes2 <- nhanes[nhanes$age >=18 & !is.na(nhanes$racecat) & !is.na(nhanes$educ_num) & !is.na(nhanes$diab),]
nhanes3 <- nhanes[nhanes$age >=18 & !is.na(nhanes$racecat) & !is.na(nhanes$inc_to_pov) & !is.na(nhanes$diab),]
dim(nhanes2) #53k
nhanes2[1:40,c('racecat','inc_to_pov','diab')]
table(nhanes2$racecat, nhanes2$diab, exclude=NULL)
summary(nhanes2$inc_to_pov)
nhanes2$diab <- ifelse(nhanes2$diab==0, 'No Diabetes','Diabetes')
nhanes2$diab <- factor(nhanes2$diab, levels=c("Diabetes","No Diabetes"))
nhanes3$diab <- ifelse(nhanes3$diab==0, 'No Diabetes','Diabetes')
nhanes3$diab <- factor(nhanes3$diab, levels=c("Diabetes","No Diabetes"))

tiff(file='C:/Users/sarac/OneDrive/Documents/Research/Area v Individual SES/NHANES_SES and AoU_SES/nhanes_ridgeline_indiveduc_racestrat.tiff', width=8, height=5, units='in', res=300)
nhanes2 %>%
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

tiff(file='C:/Users/sarac/OneDrive/Documents/Research/Area v Individual SES/NHANES_SES and AoU_SES/nhanes_ridgeline_indivinc_racestrat.tiff', width=8, height=5, units='in', res=300)
nhanes3 %>%
  ggplot(aes(y = fct_rev(racecat))) +
  geom_density_ridges(
    aes(x = inc_to_pov, fill = fct_rev(diab)), 
    alpha = .7
  ) +
  labs(
    x = "Income to Poverty Ratio",
    y = "",
    title = 'Distribution of Income to Poverty Ratio \n by Race/Ethnicity and Diabetes Status'
  ) +
  scale_y_discrete(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  theme_ridges(grid = FALSE, center=T) + 
  scale_fill_manual(name='',values=c('#74add1','#d73027'))
dev.off()



#####analysis - diab ~ ses, ordinal/categorical#####
#educ ordinal
mod_diab_educ_all <- svyglm(design = surv_all, diab ~ age + female + survyr + smoke_status + insur_stability + educ_simple, family=binomial(link=logit))
mod_diab_educ_all_raceadj <- svyglm(design = surv_all, diab ~ age + female + survyr + smoke_status + insur_stability + educ_simple + racecat, family=binomial(link=logit))
mod_diab_educ_nhw <- svyglm(design = surv_nhw, diab ~ age + female + survyr + smoke_status + insur_stability + educ_simple, family=binomial(link=logit))
mod_diab_educ_nhb <- svyglm(design = surv_nhb, diab ~ age + female + survyr + smoke_status + insur_stability + educ_simple, family=binomial(link=logit))
mod_diab_educ_mex <- svyglm(design = surv_mex, diab ~ age + female + survyr + smoke_status + insur_stability + educ_simple, family=binomial(link=logit))
mod_diab_educ_his <- svyglm(design = surv_his, diab ~ age + female + survyr + smoke_status + insur_stability + educ_simple, family=binomial(link=logit))
mod_diab_educ_nha <- svyglm(design = surv_nha, diab ~ age + female + survyr + smoke_status + insur_stability + educ_simple, family=binomial(link=logit))
mod_diab_educ_oth <- svyglm(design = surv_oth, diab ~ age + female + survyr + smoke_status + insur_stability + educ_simple, family=binomial(link=logit))
mod_diab_educ_oth11 <- svyglm(design = surv_oth11, diab ~ age + female + survyr + smoke_status + insur_stability + educ_simple, family=binomial(link=logit))
nhanes_educ_result <- data.frame(exp(mod_diab_educ_all$coefficients),exp(confint(mod_diab_educ_all)),coef(summary(mod_diab_educ_all))[,"Pr(>|t|)"],
                                 exp(mod_diab_educ_all_raceadj$coefficients)[1:13],exp(confint(mod_diab_educ_all_raceadj))[1:13,1:2],coef(summary(mod_diab_educ_all_raceadj))[,"Pr(>|t|)"][1:13],
                                 exp(mod_diab_educ_nhw$coefficients),exp(confint(mod_diab_educ_nhw)),coef(summary(mod_diab_educ_nhw))[,"Pr(>|t|)"],
                                 exp(mod_diab_educ_nhb$coefficients),exp(confint(mod_diab_educ_nhb)),coef(summary(mod_diab_educ_nhb))[,"Pr(>|t|)"],
                                 exp(mod_diab_educ_mex$coefficients),exp(confint(mod_diab_educ_mex)),coef(summary(mod_diab_educ_mex))[,"Pr(>|t|)"],
                                 exp(mod_diab_educ_his$coefficients),exp(confint(mod_diab_educ_his)),coef(summary(mod_diab_educ_his))[,"Pr(>|t|)"],
                                 exp(mod_diab_educ_nha$coefficients),exp(confint(mod_diab_educ_nha)),coef(summary(mod_diab_educ_nha))[,"Pr(>|t|)"],
                                 exp(mod_diab_educ_oth$coefficients),exp(confint(mod_diab_educ_oth)),coef(summary(mod_diab_educ_oth))[,"Pr(>|t|)"],
                                 exp(mod_diab_educ_oth11$coefficients),exp(confint(mod_diab_educ_oth11)),coef(summary(mod_diab_educ_oth11))[,"Pr(>|t|)"],stringsAsFactors = FALSE)
nhanes_educ_diab_result <-nhanes_educ_result
write.csv(nhanes_educ_result, 'C:/Users/sarac/OneDrive/Documents/Research/Area v Individual SES/NHANES_SES and AoU_SES/nhanes_mod_indiv_educord.csv', row.names=FALSE) 
#educ continuous
mod_diab_educcont_nhw <- svyglm(design = surv_nhw, diab ~ age + female + survyr + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_diab_educcont_nhb <- svyglm(design = surv_nhb, diab ~ age + female + survyr + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_diab_educcont_nha <- svyglm(design = surv_nha, diab ~ age + female + survyr + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_diab_educcont_mex <- svyglm(design = surv_mex, diab ~ age + female + survyr + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_diab_educcont_his <- svyglm(design = surv_his, diab ~ age + female + survyr + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_diab_educcont_oth <- svyglm(design = surv_oth, diab ~ age + female + survyr + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
nhanes_educcont_result <- data.frame(exp(mod_diab_educcont_nhw$coefficients),exp(confint(mod_diab_educcont_nhw)),coef(summary(mod_diab_educcont_nhw))[,"Pr(>|t|)"],
                                     exp(mod_diab_educcont_nhb$coefficients),exp(confint(mod_diab_educcont_nhb)),coef(summary(mod_diab_educcont_nhb))[,"Pr(>|t|)"],
                                     exp(mod_diab_educcont_nha$coefficients),exp(confint(mod_diab_educcont_nha)),coef(summary(mod_diab_educcont_nha))[,"Pr(>|t|)"],
                                     exp(mod_diab_educcont_mex$coefficients),exp(confint(mod_diab_educcont_mex)),coef(summary(mod_diab_educcont_mex))[,"Pr(>|t|)"],
                                     exp(mod_diab_educcont_his$coefficients),exp(confint(mod_diab_educcont_his)),coef(summary(mod_diab_educcont_his))[,"Pr(>|t|)"],
                                     exp(mod_diab_educcont_oth$coefficients),exp(confint(mod_diab_educcont_oth)),coef(summary(mod_diab_educcont_oth))[,"Pr(>|t|)"],stringsAsFactors = FALSE)
write.csv(nhanes_educcont_result, 'C:/Users/sarac/OneDrive/Documents/Research/Area v Individual SES/NHANES_SES and AoU_SES/nhanes_mod_indiv_educcont.csv', row.names=FALSE)


#income ordinal
mod_diab_inc_nhw <- svyglm(design = surv_nhw, diab ~ age + female + survyr + smoke_status + insur_stability + hhinc, family=binomial(link=logit))
mod_diab_inc_nhb <- svyglm(design = surv_nhb, diab ~ age + female + survyr + smoke_status + insur_stability + hhinc, family=binomial(link=logit))
mod_diab_inc_nha <- svyglm(design = surv_nha, diab ~ age + female + survyr + smoke_status + insur_stability + hhinc, family=binomial(link=logit))
mod_diab_inc_mex <- svyglm(design = surv_mex, diab ~ age + female + survyr + smoke_status + insur_stability + hhinc, family=binomial(link=logit))
mod_diab_inc_his <- svyglm(design = surv_his, diab ~ age + female + survyr + smoke_status + insur_stability + hhinc, family=binomial(link=logit))
mod_diab_inc_oth <- svyglm(design = surv_oth, diab ~ age + female + survyr + smoke_status + insur_stability + hhinc, family=binomial(link=logit))
x<-9999
xx<-c(9999,9999)
nhanes_inc_result <- data.frame(exp(mod_diab_inc_nhw$coefficients),exp(confint(mod_diab_inc_nhw)),coef(summary(mod_diab_inc_nhw))[,"Pr(>|t|)"],
                                exp(mod_diab_inc_nhb$coefficients),exp(confint(mod_diab_inc_nhb)),coef(summary(mod_diab_inc_nhb))[,"Pr(>|t|)"],
                                append(exp(mod_diab_inc_nha$coefficients),x),rbind(exp(confint(mod_diab_inc_nha)),xx),append(coef(summary(mod_diab_inc_nha))[,"Pr(>|t|)"],x),
                                exp(mod_diab_inc_mex$coefficients),exp(confint(mod_diab_inc_mex)),coef(summary(mod_diab_inc_mex))[,"Pr(>|t|)"],
                                exp(mod_diab_inc_his$coefficients),exp(confint(mod_diab_inc_his)),coef(summary(mod_diab_inc_his))[,"Pr(>|t|)"],
                                exp(mod_diab_inc_oth$coefficients),exp(confint(mod_diab_inc_oth)),coef(summary(mod_diab_inc_oth))[,"Pr(>|t|)"],stringsAsFactors = FALSE)
write.csv(nhanes_inc_result, 'C:/Users/sarac/OneDrive/Documents/Research/Area v Individual SES/NHANES_SES and AoU_SES/nhanes_mod_indiv_incord.csv', row.names=FALSE) 
#income ordinal simple
mod_diab_incsimp_all <- svyglm(design = surv_all, diab ~ age + female + survyr + smoke_status + insur_stability + inc_simple, family=binomial(link=logit))
mod_diab_incsimp_all_raceadj <- svyglm(design = surv_all, diab ~ age + female + survyr + smoke_status + insur_stability + inc_simple + racecat, family=binomial(link=logit))
mod_diab_incsimp_nhw <- svyglm(design = surv_nhw, diab ~ age + female + survyr + smoke_status + insur_stability + inc_simple, family=binomial(link=logit))
mod_diab_incsimp_nhb <- svyglm(design = surv_nhb, diab ~ age + female + survyr + smoke_status + insur_stability + inc_simple, family=binomial(link=logit))
mod_diab_incsimp_nha <- svyglm(design = surv_nha, diab ~ age + female + survyr + smoke_status + insur_stability + inc_simple, family=binomial(link=logit))
mod_diab_incsimp_mex <- svyglm(design = surv_mex, diab ~ age + female + survyr + smoke_status + insur_stability + inc_simple, family=binomial(link=logit))
mod_diab_incsimp_his <- svyglm(design = surv_his, diab ~ age + female + survyr + smoke_status + insur_stability + inc_simple, family=binomial(link=logit))
mod_diab_incsimp_oth <- svyglm(design = surv_oth, diab ~ age + female + survyr + smoke_status + insur_stability + inc_simple, family=binomial(link=logit))
mod_diab_incsimp_oth11 <- svyglm(design = surv_oth11, diab ~ age + female + survyr + smoke_status + insur_stability + inc_simple, family=binomial(link=logit))
nhanes_incsimp_result <- data.frame(exp(mod_diab_incsimp_all$coefficients),exp(confint(mod_diab_incsimp_all)),coef(summary(mod_diab_incsimp_all))[,"Pr(>|t|)"],
                                    exp(mod_diab_incsimp_all_raceadj$coefficients)[1:13],exp(confint(mod_diab_incsimp_all_raceadj))[1:13,1:2],coef(summary(mod_diab_incsimp_all_raceadj))[,"Pr(>|t|)"][1:13],
                                    exp(mod_diab_incsimp_nhw$coefficients),exp(confint(mod_diab_incsimp_nhw)),coef(summary(mod_diab_incsimp_nhw))[,"Pr(>|t|)"],
                                    exp(mod_diab_incsimp_nhb$coefficients),exp(confint(mod_diab_incsimp_nhb)),coef(summary(mod_diab_incsimp_nhb))[,"Pr(>|t|)"],
                                    exp(mod_diab_incsimp_nha$coefficients),exp(confint(mod_diab_incsimp_nha)),coef(summary(mod_diab_incsimp_nha))[,"Pr(>|t|)"],
                                    exp(mod_diab_incsimp_mex$coefficients),exp(confint(mod_diab_incsimp_mex)),coef(summary(mod_diab_incsimp_mex))[,"Pr(>|t|)"],
                                    exp(mod_diab_incsimp_his$coefficients),exp(confint(mod_diab_incsimp_his)),coef(summary(mod_diab_incsimp_his))[,"Pr(>|t|)"],
                                    exp(mod_diab_incsimp_oth$coefficients),exp(confint(mod_diab_incsimp_oth)),coef(summary(mod_diab_incsimp_oth))[,"Pr(>|t|)"],
                                    exp(mod_diab_incsimp_oth11$coefficients),exp(confint(mod_diab_incsimp_oth11)),coef(summary(mod_diab_incsimp_oth11))[,"Pr(>|t|)"],stringsAsFactors = FALSE)
nhanes_inc_diab_result <- nhanes_incsimp_result
write.csv(nhanes_incsimp_result, 'C:/Users/sarac/OneDrive/Documents/Research/Area v Individual SES/NHANES_SES and AoU_SES/nhanes_mod_inccat_diab.csv', row.names=FALSE)
#inc-to-pov continuous
mod_diab_inctopov_nhw <- svyglm(design = surv_nhw, diab ~ age + female + survyr + smoke_status + insur_stability + inc_to_pov, family=binomial(link=logit))
mod_diab_inctopov_nhb <- svyglm(design = surv_nhb, diab ~ age + female + survyr + smoke_status + insur_stability + inc_to_pov, family=binomial(link=logit))
mod_diab_inctopov_nha <- svyglm(design = surv_nha, diab ~ age + female + survyr + smoke_status + insur_stability + inc_to_pov, family=binomial(link=logit))
mod_diab_inctopov_his <- svyglm(design = surv_his, diab ~ age + female + survyr + smoke_status + insur_stability + inc_to_pov, family=binomial(link=logit))
mod_diab_inctopov_oth <- svyglm(design = surv_oth, diab ~ age + female + survyr + smoke_status + insur_stability + inc_to_pov, family=binomial(link=logit))
nhanes_inctopov_result <- data.frame(exp(mod_diab_inctopov_nhw$coefficients),exp(confint(mod_diab_inctopov_nhw)),coef(summary(mod_diab_inctopov_nhw))[,"Pr(>|t|)"],
                                     exp(mod_diab_inctopov_nhb$coefficients),exp(confint(mod_diab_inctopov_nhb)),coef(summary(mod_diab_inctopov_nhb))[,"Pr(>|t|)"],
                                     exp(mod_diab_inctopov_nha$coefficients),exp(confint(mod_diab_inctopov_nha)),coef(summary(mod_diab_inctopov_nha))[,"Pr(>|t|)"],
                                     exp(mod_diab_inctopov_mex$coefficients),exp(confint(mod_diab_inctopov_mex)),coef(summary(mod_diab_inctopov_mex))[,"Pr(>|t|)"],
                                     exp(mod_diab_inctopov_his$coefficients),exp(confint(mod_diab_inctopov_his)),coef(summary(mod_diab_inctopov_his))[,"Pr(>|t|)"],
                                     exp(mod_diab_inctopov_oth$coefficients),exp(confint(mod_diab_inctopov_oth)),coef(summary(mod_diab_inctopov_oth))[,"Pr(>|t|)"],stringsAsFactors = FALSE)
write.csv(nhanes_inctopov_result, 'C:/Users/sarac/OneDrive/Documents/Research/Area v Individual SES/NHANES_SES and AoU_SES/nhanes_mod_indiv_inctopov.csv', row.names=FALSE)


#employ binary
mod_diab_emp_nhw <- svyglm(design = surv_nhw, diab ~ age + female + survyr + smoke_status + insur_stability + work_yn, family=binomial(link=logit))
mod_diab_emp_nhb <- svyglm(design = surv_nhb, diab ~ age + female + survyr + smoke_status + insur_stability + work_yn, family=binomial(link=logit))
mod_diab_emp_nha <- svyglm(design = surv_nha, diab ~ age + female + survyr + smoke_status + insur_stability + work_yn, family=binomial(link=logit))
mod_diab_emp_mex <- svyglm(design = surv_mex, diab ~ age + female + survyr + smoke_status + insur_stability + work_yn, family=binomial(link=logit))
mod_diab_emp_his <- svyglm(design = surv_his, diab ~ age + female + survyr + smoke_status + insur_stability + work_yn, family=binomial(link=logit))
mod_diab_emp_oth <- svyglm(design = surv_oth, diab ~ age + female + survyr + smoke_status + insur_stability + work_yn, family=binomial(link=logit))
nhanes_emp_result <- data.frame(exp(mod_diab_emp_nhw$coefficients),exp(confint(mod_diab_emp_nhw)),coef(summary(mod_diab_emp_nhw))[,"Pr(>|t|)"],
                                exp(mod_diab_emp_nhb$coefficients),exp(confint(mod_diab_emp_nhb)),coef(summary(mod_diab_emp_nhb))[,"Pr(>|t|)"],
                                exp(mod_diab_emp_nha$coefficients),exp(confint(mod_diab_emp_nha)),coef(summary(mod_diab_emp_nha))[,"Pr(>|t|)"],
                                exp(mod_diab_emp_mex$coefficients),exp(confint(mod_diab_emp_mex)),coef(summary(mod_diab_emp_mex))[,"Pr(>|t|)"],
                                exp(mod_diab_emp_his$coefficients),exp(confint(mod_diab_emp_his)),coef(summary(mod_diab_emp_his))[,"Pr(>|t|)"],
                                exp(mod_diab_emp_oth$coefficients),exp(confint(mod_diab_emp_oth)),coef(summary(mod_diab_emp_oth))[,"Pr(>|t|)"],stringsAsFactors = FALSE)
write.csv(nhanes_emp_result, 'C:/Users/sarac/OneDrive/Documents/Research/Area v Individual SES/NHANES_SES and AoU_SES/nhanes_mod_indiv_empbin.csv', row.names=FALSE) 


#####analysis - obese ~ ses, ordinal/categorical#####
#educ ordinal
mod_ob_educ_all <- svyglm(design = surv_all, obese ~ age + female + survyr + smoke_status + insur_stability + educ_simple, family=binomial(link=logit))
mod_ob_educ_all_raceadj <- svyglm(design = surv_all, obese ~ age + female + survyr + smoke_status + insur_stability + educ_simple + racecat, family=binomial(link=logit))
mod_ob_educ_nhw <- svyglm(design = surv_nhw, obese ~ age + female + survyr + smoke_status + insur_stability + educ_simple, family=binomial(link=logit))
mod_ob_educ_nhb <- svyglm(design = surv_nhb, obese ~ age + female + survyr + smoke_status + insur_stability + educ_simple, family=binomial(link=logit))
mod_ob_educ_mex <- svyglm(design = surv_mex, obese ~ age + female + survyr + smoke_status + insur_stability + educ_simple, family=binomial(link=logit))
mod_ob_educ_his <- svyglm(design = surv_his, obese ~ age + female + survyr + smoke_status + insur_stability + educ_simple, family=binomial(link=logit))
mod_ob_educ_nha <- svyglm(design = surv_nha, obese ~ age + female + survyr + smoke_status + insur_stability + educ_simple, family=binomial(link=logit))
mod_ob_educ_oth <- svyglm(design = surv_oth, obese ~ age + female + survyr + smoke_status + insur_stability + educ_simple, family=binomial(link=logit))
mod_ob_educ_oth11 <- svyglm(design = surv_oth11, obese ~ age + female + survyr + smoke_status + insur_stability + educ_simple, family=binomial(link=logit))
nhanes_educ_ob_result <- data.frame(exp(mod_ob_educ_all$coefficients),exp(confint(mod_ob_educ_all)),coef(summary(mod_ob_educ_all))[,"Pr(>|t|)"],
                                 exp(mod_ob_educ_all_raceadj$coefficients)[1:13],exp(confint(mod_ob_educ_all_raceadj))[1:13,1:2],coef(summary(mod_ob_educ_all_raceadj))[,"Pr(>|t|)"][1:13],
                                 exp(mod_ob_educ_nhw$coefficients),exp(confint(mod_ob_educ_nhw)),coef(summary(mod_ob_educ_nhw))[,"Pr(>|t|)"],
                                 exp(mod_ob_educ_nhb$coefficients),exp(confint(mod_ob_educ_nhb)),coef(summary(mod_ob_educ_nhb))[,"Pr(>|t|)"],
                                 exp(mod_ob_educ_mex$coefficients),exp(confint(mod_ob_educ_mex)),coef(summary(mod_ob_educ_mex))[,"Pr(>|t|)"],
                                 exp(mod_ob_educ_his$coefficients),exp(confint(mod_ob_educ_his)),coef(summary(mod_ob_educ_his))[,"Pr(>|t|)"],
                                 exp(mod_ob_educ_nha$coefficients),exp(confint(mod_ob_educ_nha)),coef(summary(mod_ob_educ_nha))[,"Pr(>|t|)"],
                                 exp(mod_ob_educ_oth$coefficients),exp(confint(mod_ob_educ_oth)),coef(summary(mod_ob_educ_oth))[,"Pr(>|t|)"],
                                 exp(mod_ob_educ_oth11$coefficients),exp(confint(mod_ob_educ_oth11)),coef(summary(mod_ob_educ_oth11))[,"Pr(>|t|)"],stringsAsFactors = FALSE)

write.csv(nhanes_educ_ob_result, 'C:/Users/sarac/OneDrive/Documents/Research/Area v Individual SES/NHANES_SES and AoU_SES/nhanes_mod_indiv_ob_educcat.csv', row.names=FALSE) 
#income ordinal simple
mod_ob_incsimp_all <- svyglm(design = surv_all, obese ~ age + female + survyr + smoke_status + insur_stability + inc_simple, family=binomial(link=logit))
mod_ob_incsimp_all_raceadj <- svyglm(design = surv_all, obese ~ age + female + survyr + smoke_status + insur_stability + inc_simple + racecat, family=binomial(link=logit))
mod_ob_incsimp_nhw <- svyglm(design = surv_nhw, obese ~ age + female + survyr + smoke_status + insur_stability + inc_simple, family=binomial(link=logit))
mod_ob_incsimp_nhb <- svyglm(design = surv_nhb, obese ~ age + female + survyr + smoke_status + insur_stability + inc_simple, family=binomial(link=logit))
mod_ob_incsimp_mex <- svyglm(design = surv_mex, obese ~ age + female + survyr + smoke_status + insur_stability + inc_simple, family=binomial(link=logit))
mod_ob_incsimp_his <- svyglm(design = surv_his, obese ~ age + female + survyr + smoke_status + insur_stability + inc_simple, family=binomial(link=logit))
mod_ob_incsimp_nha <- svyglm(design = surv_nha, obese ~ age + female + survyr + smoke_status + insur_stability + inc_simple, family=binomial(link=logit))
mod_ob_incsimp_oth <- svyglm(design = surv_oth, obese ~ age + female + survyr + smoke_status + insur_stability + inc_simple, family=binomial(link=logit))
mod_ob_incsimp_oth11 <- svyglm(design = surv_oth11, obese ~ age + female + survyr + smoke_status + insur_stability + inc_simple, family=binomial(link=logit))
nhanes_inc_ob_result <- data.frame(exp(mod_ob_incsimp_all$coefficients),exp(confint(mod_ob_incsimp_all)),coef(summary(mod_ob_incsimp_all))[,"Pr(>|t|)"],
                                exp(mod_ob_incsimp_all_raceadj$coefficients)[1:13],exp(confint(mod_ob_incsimp_all_raceadj))[1:13,1:2],coef(summary(mod_ob_incsimp_all_raceadj))[,"Pr(>|t|)"][1:13],
                                exp(mod_ob_incsimp_nhw$coefficients),exp(confint(mod_ob_incsimp_nhw)),coef(summary(mod_ob_incsimp_nhw))[,"Pr(>|t|)"],
                                exp(mod_ob_incsimp_nhb$coefficients),exp(confint(mod_ob_incsimp_nhb)),coef(summary(mod_ob_incsimp_nhb))[,"Pr(>|t|)"],
                                exp(mod_ob_incsimp_mex$coefficients),exp(confint(mod_ob_incsimp_mex)),coef(summary(mod_ob_incsimp_mex))[,"Pr(>|t|)"],
                                exp(mod_ob_incsimp_his$coefficients),exp(confint(mod_ob_incsimp_his)),coef(summary(mod_ob_incsimp_his))[,"Pr(>|t|)"],
                                exp(mod_ob_incsimp_nha$coefficients),exp(confint(mod_ob_incsimp_nha)),coef(summary(mod_ob_incsimp_nha))[,"Pr(>|t|)"],
                                exp(mod_ob_incsimp_oth$coefficients),exp(confint(mod_ob_incsimp_oth)),coef(summary(mod_ob_incsimp_oth))[,"Pr(>|t|)"],
                                exp(mod_ob_incsimp_oth11$coefficients),exp(confint(mod_ob_incsimp_oth11)),coef(summary(mod_ob_incsimp_oth11))[,"Pr(>|t|)"],stringsAsFactors = FALSE)
write.csv(nhanes_inc_ob_result, 'C:/Users/sarac/OneDrive/Documents/Research/Area v Individual SES/NHANES_SES and AoU_SES/nhanes_mod_indiv_ob_inccat.csv', row.names=FALSE)
                                 



#####visuals: forest plots categorical educ and income, diab and obese#####
tiff(file='C:/Users/sarac/OneDrive/Documents/Research/Area v Individual SES/NHANES_SES and AoU_SES/nhanes_forest_indiv_educ_racestrat.tiff', width=7, height=5, units='in', res=300)
forestplot(c("HS Degree","Some HS","No HS"), 
           mean = cbind(nhanes_educ_diab_result[11:13,1],nhanes_educ_diab_result[11:13,5],nhanes_educ_diab_result[11:13,9],
                        nhanes_educ_diab_result[11:13,13],nhanes_educ_diab_result[11:13,17],nhanes_educ_diab_result[11:13,21],
                        nhanes_educ_diab_result[11:13,25],nhanes_educ_diab_result[11:13,29],nhanes_educ_diab_result[11:13,33]),
           lower = cbind(nhanes_educ_diab_result[11:13,2],nhanes_educ_diab_result[11:13,6],nhanes_educ_diab_result[11:13,10],
                         nhanes_educ_diab_result[11:13,14],nhanes_educ_diab_result[11:13,18],nhanes_educ_diab_result[11:13,22],
                         nhanes_educ_diab_result[11:13,26],nhanes_educ_diab_result[11:13,30],nhanes_educ_diab_result[11:13,34]),
           upper = cbind(nhanes_educ_diab_result[11:13,3],nhanes_educ_diab_result[11:13,7],nhanes_educ_diab_result[11:13,11],
                         nhanes_educ_diab_result[11:13,15],nhanes_educ_diab_result[11:13,19],nhanes_educ_diab_result[11:13,23],
                         nhanes_educ_diab_result[11:13,27],nhanes_educ_diab_result[11:13,31],nhanes_educ_diab_result[11:13,35]),
           col = fpColors(box = c('gray23','gray57','dodgerblue4','darkslategray3','palegreen3','lightgoldenrod','salmon1','indianred3','mediumpurple4')),
           legend=c("All","All, Race-adjusted","NHW","NHB","Mexican-American","Other Hispanic","NHA","Other/Multi-Racial \n including NHA (pre-2011)","Other/Multi-Racial \n (post-2011)"),
           legend_args = fpLegend(pos = "right"),#list(x = 0.5, y = -0.2), gp = gpar(cex=0.9,col = "#CCCCCC", fill = "#F9F9F9")),
           #legend=c("NHW","NHB","NHA","Mexican-American","Other Hispanic"),
           #legend_args = fpLegend(pos = list(x = 0.9, y = 0.9), gp = gpar(cex=0.9,col = "#CCCCCC", fill = "#F9F9F9")),
           xlab = "OR (95% CI)",
           title = 'Adjusted OR for Educational Attainment (Ref=College Degree) \n on Type 2 Diabetes, Stratified by Race/Ethnicity',
           txt_gp = fpTxtGp(title=gpar(cex=1),xlab=gpar(cex=1), ticks=gpar(cex=1), label=gpar(cex=1)),
           vertices=TRUE,
           line.margin=0.25,
           graph.pos=2,
           clip=c(0.5,5.0), xlog=TRUE, grid=structure(c(0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0)),
           xticks=c(-0.693147180559957,0,0.405465108108171,0.693147180559957,0.91629073187417,1.09861228866813,1.25276296849539,1.38629436111991,1.5040773967763,1.60943791243413))#xlim=c(0.5,4.0), xticks=c(0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0), 
dev.off()

nhanes_educ_diab_result_transpose <- t(nhanes_educ_diab_result)
rownames(nhanes_educ_diab_result_transpose)
tiff(file='C:/Users/sarac/OneDrive/Documents/Research/Area v Individual SES/NHANES_SES and AoU_SES/SuppFig4A_nhanes_forest_indiv_diab_educcat_racestrat_transpose.tiff', width=7, height=7, units='in', res=300)
forestplot(c("All","All, Race-adjusted","NHW","NHB","Mexican-American","Other Hispanic","NHA","Other/Multi-Racial \n including NHA (pre-2011)","Other/Multi-Racial \n (post-2011)"), 
           mean = cbind(nhanes_educ_diab_result_transpose[c(1,5,9,13,17,21,25,29,33),11],nhanes_educ_diab_result_transpose[c(1,5,9,13,17,21,25,29,33),12],nhanes_educ_diab_result_transpose[c(1,5,9,13,17,21,25,29,33),13]),
           lower = cbind(nhanes_educ_diab_result_transpose[c(2,6,10,14,18,22,26,30,34),11],nhanes_educ_diab_result_transpose[c(2,6,10,14,18,22,26,30,34),12],nhanes_educ_diab_result_transpose[c(2,6,10,14,18,22,26,30,34),13]),
           upper = cbind(nhanes_educ_diab_result_transpose[c(3,7,11,15,19,23,27,31,35),11],nhanes_educ_diab_result_transpose[c(3,7,11,15,19,23,27,31,35),12],nhanes_educ_diab_result_transpose[c(3,7,11,15,19,23,27,31,35),13]),
           col = fpColors(box = c('darkslategray3','lightgoldenrod','indianred3')),
           legend=c("HS Degree","Some HS","No HS"),
           legend_args = fpLegend(pos = list(x = 0.9, y = 0.9), gp = gpar(cex=0.9,col = "#CCCCCC", fill = "#F9F9F9")),
           xlab = "OR (95% CI)",
           title = 'Adjusted OR for Educational Attainment (Ref=College Degree) \n on Type 2 Diabetes, Stratified by Race/Ethnicity',
           txt_gp = fpTxtGp(title=gpar(cex=1),xlab=gpar(cex=1), ticks=gpar(cex=1), label=gpar(cex=1)),
           vertices=TRUE,
           line.margin=0.25,
           graph.pos=2,
           clip=c(0.5,5.0), xlog=TRUE, grid=structure(c(0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0)),
           xticks=c(-0.693147180559957,0,0.405465108108171,0.693147180559957,0.91629073187417,1.09861228866813,1.25276296849539,1.38629436111991,1.5040773967763,1.60943791243413))#xlim=c(0.5,4.0), xticks=c(0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0), 
dev.off()

tiff(file='C:/Users/sarac/OneDrive/Documents/Research/Area v Individual SES/NHANES_SES and AoU_SES/nhanes_forest_indiv_diab_inccat_racestrat.tiff', width=7, height=5, units='in', res=300)
forestplot(c("$45,000-74,999","$20,000-44,999","Under $20,000"), 
           mean = cbind(nhanes_inc_diab_result[11:13,1],nhanes_inc_diab_result[11:13,5],nhanes_inc_diab_result[11:13,9],
                        nhanes_inc_diab_result[11:13,13],nhanes_inc_diab_result[11:13,17],nhanes_inc_diab_result[11:13,21],
                        nhanes_inc_diab_result[11:13,25],nhanes_inc_diab_result[11:13,29],nhanes_inc_diab_result[11:13,33]),
           lower = cbind(nhanes_inc_diab_result[11:13,2],nhanes_inc_diab_result[11:13,6],nhanes_inc_diab_result[11:13,10],
                         nhanes_inc_diab_result[11:13,14],nhanes_inc_diab_result[11:13,18],nhanes_inc_diab_result[11:13,22],
                         nhanes_inc_diab_result[11:13,26],nhanes_inc_diab_result[11:13,30],nhanes_inc_diab_result[11:13,34]),
           upper = cbind(nhanes_inc_diab_result[11:13,3],nhanes_inc_diab_result[11:13,7],nhanes_inc_diab_result[11:13,11],
                         nhanes_inc_diab_result[11:13,15],nhanes_inc_diab_result[11:13,19],nhanes_inc_diab_result[11:13,23],
                         nhanes_inc_diab_result[11:13,27],nhanes_inc_diab_result[11:13,31],nhanes_inc_diab_result[11:13,35]),
           col = fpColors(box = c('gray23','gray57','dodgerblue4','darkslategray3','palegreen3','lightgoldenrod','salmon1','indianred3','mediumpurple4')),
           legend=c("All","All, Race-adjusted","NHW","NHB","Mexican-American","Other Hispanic","NHA","Other/Multi-Racial \n including NHA (pre-2011)","Other/Multi-Racial \n (post-2011)"),
           legend_args = fpLegend(pos = list(x = 0.9, y = 0.9), gp = gpar(cex=0.9,col = "#CCCCCC", fill = "#F9F9F9")),
           xlab = "OR (95% CI)",
           title = 'Adjusted OR for Income Category (Ref= More than $75,000) \n on Type 2 Diabetes, Stratified by Race/Ethnicity',
           txt_gp = fpTxtGp(title=gpar(cex=1),xlab=gpar(cex=1), ticks=gpar(cex=1), label=gpar(cex=1)),
           vertices=TRUE,
           line.margin=0.25,
           graph.pos=2,
           clip=c(0.5,2.5), xlog=TRUE, grid=structure(c(0.5,1.0,1.5,2.0,2.5)),
           xticks=c(-0.693147180559957,0,0.405465108108171,0.693147180559957,0.91629073187417))#xlim=c(0.5,2.5), xticks=c(0.5,1.0,1.5,2.0,2.5), 
dev.off()

nhanes_inc_diab_result_transpose <- t(nhanes_inc_diab_result)
rownames(nhanes_inc_diab_result_transpose)
tiff(file='C:/Users/sarac/OneDrive/Documents/Research/Area v Individual SES/NHANES_SES and AoU_SES/SuppFig4B_nhanes_forest_indiv_diab_inccat_racestrat_transpose.tiff', width=7, height=7, units='in', res=300)
forestplot(c("All","All, Race-adjusted","NHW","NHB","Mexican-American","Other Hispanic","NHA","Other/Multi-Racial \n including NHA (pre-2011)","Other/Multi-Racial \n (post-2011)"), 
           mean = cbind(nhanes_inc_diab_result_transpose[c(1,5,9,13,17,21,25,29,33),11],nhanes_inc_diab_result_transpose[c(1,5,9,13,17,21,25,29,33),12],nhanes_inc_diab_result_transpose[c(1,5,9,13,17,21,25,29,33),13]),
           lower = cbind(nhanes_inc_diab_result_transpose[c(2,6,10,14,18,22,26,30,34),11],nhanes_inc_diab_result_transpose[c(2,6,10,14,18,22,26,30,34),12],nhanes_inc_diab_result_transpose[c(2,6,10,14,18,22,26,30,34),13]),
           upper = cbind(nhanes_inc_diab_result_transpose[c(3,7,11,15,19,23,27,31,35),11],nhanes_inc_diab_result_transpose[c(3,7,11,15,19,23,27,31,35),12],nhanes_inc_diab_result_transpose[c(3,7,11,15,19,23,27,31,35),13]),
           col = fpColors(box = c('darkslategray3','lightgoldenrod','indianred3')),
           legend=c("$45,000-74,999","$20,000-44,999","Under $20,000"),
           legend_args = fpLegend(pos = list(x = 0.9, y = 0.9), gp = gpar(cex=0.9,col = "#CCCCCC", fill = "#F9F9F9")),
           xlab = "OR (95% CI)",
           title = 'Adjusted OR for Income Category (Ref= More than $75,000) \n on Type 2 Diabetes, Stratified by Race/Ethnicity',
           txt_gp = fpTxtGp(title=gpar(cex=1),xlab=gpar(cex=1), ticks=gpar(cex=1), label=gpar(cex=1)),
           vertices=TRUE,
           line.margin=0.25,
           graph.pos=2,
           clip=c(0.5,5.0), xlog=TRUE, grid=structure(c(0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0)),
           xticks=c(-0.693147180559957,0,0.405465108108171,0.693147180559957,0.91629073187417,1.09861228866813,1.25276296849539,1.38629436111991,1.5040773967763,1.60943791243413))#xlim=c(0.5,4.0), xticks=c(0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0), 
dev.off()

#obesity forests by categorical SES
nhanes_educ_ob_result_transpose <- t(nhanes_educ_ob_result)
rownames(nhanes_educ_ob_result_transpose)
tiff(file='C:/Users/sarac/OneDrive/Documents/Research/Area v Individual SES/NHANES_SES and AoU_SES/SuppFig4C_nhanes_forest_indiv_ob_educcat_racestrat_transpose.tiff', width=7, height=7, units='in', res=300)
forestplot(c("All","All, Race-adjusted","NHW","NHB","Mexican-American","Other Hispanic","NHA","Other/Multi-Racial \n including NHA (pre-2011)","Other/Multi-Racial \n (post-2011)"), 
           mean = cbind(nhanes_educ_ob_result_transpose[c(1,5,9,13,17,21,25,29,33),11],nhanes_educ_ob_result_transpose[c(1,5,9,13,17,21,25,29,33),12],nhanes_educ_ob_result_transpose[c(1,5,9,13,17,21,25,29,33),13]),
           lower = cbind(nhanes_educ_ob_result_transpose[c(2,6,10,14,18,22,26,30,34),11],nhanes_educ_ob_result_transpose[c(2,6,10,14,18,22,26,30,34),12],nhanes_educ_ob_result_transpose[c(2,6,10,14,18,22,26,30,34),13]),
           upper = cbind(nhanes_educ_ob_result_transpose[c(3,7,11,15,19,23,27,31,35),11],nhanes_educ_ob_result_transpose[c(3,7,11,15,19,23,27,31,35),12],nhanes_educ_ob_result_transpose[c(3,7,11,15,19,23,27,31,35),13]),
           col = fpColors(box = c('darkslategray3','lightgoldenrod','indianred3')),
           legend=c("HS Degree","Some HS","No HS"),
           legend_args = fpLegend(pos = list(x = 0.9, y = 0.9), gp = gpar(cex=0.9,col = "#CCCCCC", fill = "#F9F9F9")),
           xlab = "OR (95% CI)",
           title = 'Adjusted OR for Educational Attainment (Ref=College Degree) \n on Obesity, Stratified by Race/Ethnicity',
           txt_gp = fpTxtGp(title=gpar(cex=1),xlab=gpar(cex=1), ticks=gpar(cex=1), label=gpar(cex=1)),
           vertices=TRUE,
           line.margin=0.25,
           graph.pos=2,
           clip=c(0.5,5.0), xlog=TRUE, grid=structure(c(0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0)),
           xticks=c(-0.693147180559957,0,0.405465108108171,0.693147180559957,0.91629073187417,1.09861228866813,1.25276296849539,1.38629436111991,1.5040773967763,1.60943791243413))#xlim=c(0.5,4.0), xticks=c(0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0), 
dev.off()

nhanes_inc_ob_result_transpose <- t(nhanes_inc_ob_result)
rownames(nhanes_inc_ob_result_transpose)
tiff(file='C:/Users/sarac/OneDrive/Documents/Research/Area v Individual SES/NHANES_SES and AoU_SES/SuppFig4D_nhanes_forest_indiv_ob_inccat_racestrat_transpose.tiff', width=7, height=7, units='in', res=300)
forestplot(c("All","All, Race-adjusted","NHW","NHB","Mexican-American","Other Hispanic","NHA","Other/Multi-Racial \n including NHA (pre-2011)","Other/Multi-Racial \n (post-2011)"), 
           mean = cbind(nhanes_inc_ob_result_transpose[c(1,5,9,13,17,21,25,29,33),11],nhanes_inc_ob_result_transpose[c(1,5,9,13,17,21,25,29,33),12],nhanes_inc_ob_result_transpose[c(1,5,9,13,17,21,25,29,33),13]),
           lower = cbind(nhanes_inc_ob_result_transpose[c(2,6,10,14,18,22,26,30,34),11],nhanes_inc_ob_result_transpose[c(2,6,10,14,18,22,26,30,34),12],nhanes_inc_ob_result_transpose[c(2,6,10,14,18,22,26,30,34),13]),
           upper = cbind(nhanes_inc_ob_result_transpose[c(3,7,11,15,19,23,27,31,35),11],nhanes_inc_ob_result_transpose[c(3,7,11,15,19,23,27,31,35),12],nhanes_inc_ob_result_transpose[c(3,7,11,15,19,23,27,31,35),13]),
           col = fpColors(box = c('darkslategray3','lightgoldenrod','indianred3')),
           legend=c("$45,000-74,999","$20,000-44,999","Under $20,000"),
           legend_args = fpLegend(pos = list(x = 0.9, y = 0.9), gp = gpar(cex=0.9,col = "#CCCCCC", fill = "#F9F9F9")),
           xlab = "OR (95% CI)",
           title = 'Adjusted OR for Income Category (Ref= More than $75,000) \n on Obesity, Stratified by Race/Ethnicity',
           txt_gp = fpTxtGp(title=gpar(cex=1),xlab=gpar(cex=1), ticks=gpar(cex=1), label=gpar(cex=1)),
           vertices=TRUE,
           line.margin=0.25,
           graph.pos=2,
           clip=c(0.5,5.0), xlog=TRUE, grid=structure(c(0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0)),
           xticks=c(-0.693147180559957,0,0.405465108108171,0.693147180559957,0.91629073187417,1.09861228866813,1.25276296849539,1.38629436111991,1.5040773967763,1.60943791243413))#xlim=c(0.5,4.0), xticks=c(0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0), 
dev.off()

#forest plots for fig
nhanes_forest_educcat_diab <- forestplot(c("All","All, Race-adjusted","NHW","NHB","Mexican-American","Other Hispanic","NHA","Other/Multi-Racial \n including NHA (pre-2011)","Other/Multi-Racial \n (post-2011)"), 
           mean = cbind(nhanes_educ_diab_result_transpose[c(1,5,9,13,17,21,25,29,33),11],nhanes_educ_diab_result_transpose[c(1,5,9,13,17,21,25,29,33),12],nhanes_educ_diab_result_transpose[c(1,5,9,13,17,21,25,29,33),13]),
           lower = cbind(nhanes_educ_diab_result_transpose[c(2,6,10,14,18,22,26,30,34),11],nhanes_educ_diab_result_transpose[c(2,6,10,14,18,22,26,30,34),12],nhanes_educ_diab_result_transpose[c(2,6,10,14,18,22,26,30,34),13]),
           upper = cbind(nhanes_educ_diab_result_transpose[c(3,7,11,15,19,23,27,31,35),11],nhanes_educ_diab_result_transpose[c(3,7,11,15,19,23,27,31,35),12],nhanes_educ_diab_result_transpose[c(3,7,11,15,19,23,27,31,35),13]),
           col = fpColors(box = c('darkslategray3','lightgoldenrod','indianred3')),
           legend=c("HS Degree","Some HS","No HS"),
           legend_args = fpLegend(pos = list(x = 0.9, y = 0.9), gp = gpar(cex=0.9,col = "#CCCCCC", fill = "#F9F9F9")),
           xlab = "OR (95% CI)",
           title = 'Adjusted OR for Educational Attainment (Ref=College Degree) \n on Type 2 Diabetes, Stratified by Race/Ethnicity',
           txt_gp = fpTxtGp(title=gpar(cex=1),xlab=gpar(cex=1), ticks=gpar(cex=1), label=gpar(cex=1)),
           vertices=TRUE,
           line.margin=0.25,
           graph.pos=2,
           clip=c(0.5,5.0), xlog=TRUE, grid=structure(c(0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0)),
           xticks=c(-0.693147180559957,0,0.405465108108171,0.693147180559957,0.91629073187417,1.09861228866813,1.25276296849539,1.38629436111991,1.5040773967763,1.60943791243413))#xlim=c(0.5,4.0), xticks=c(0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0), 
nhanes_forest_inccat_diab <- forestplot(c("All","All, Race-adjusted","NHW","NHB","Mexican-American","Other Hispanic","NHA","Other/Multi-Racial \n including NHA (pre-2011)","Other/Multi-Racial \n (post-2011)"), 
           mean = cbind(nhanes_inc_diab_result_transpose[c(1,5,9,13,17,21,25,29,33),11],nhanes_inc_diab_result_transpose[c(1,5,9,13,17,21,25,29,33),12],nhanes_inc_diab_result_transpose[c(1,5,9,13,17,21,25,29,33),13]),
           lower = cbind(nhanes_inc_diab_result_transpose[c(2,6,10,14,18,22,26,30,34),11],nhanes_inc_diab_result_transpose[c(2,6,10,14,18,22,26,30,34),12],nhanes_inc_diab_result_transpose[c(2,6,10,14,18,22,26,30,34),13]),
           upper = cbind(nhanes_inc_diab_result_transpose[c(3,7,11,15,19,23,27,31,35),11],nhanes_inc_diab_result_transpose[c(3,7,11,15,19,23,27,31,35),12],nhanes_inc_diab_result_transpose[c(3,7,11,15,19,23,27,31,35),13]),
           col = fpColors(box = c('darkslategray3','lightgoldenrod','indianred3')),
           legend=c("$45,000-74,999","$20,000-44,999","Under $20,000"),
           legend_args = fpLegend(pos = list(x = 0.9, y = 0.9), gp = gpar(cex=0.9,col = "#CCCCCC", fill = "#F9F9F9")),
           xlab = "OR (95% CI)",
           title = 'Adjusted OR for Income Category (Ref= More than $75,000) \n on Type 2 Diabetes, Stratified by Race/Ethnicity',
           txt_gp = fpTxtGp(title=gpar(cex=1),xlab=gpar(cex=1), ticks=gpar(cex=1), label=gpar(cex=1)),
           vertices=TRUE,
           line.margin=0.25,
           graph.pos=2,
           clip=c(0.5,5.0), xlog=TRUE, grid=structure(c(0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0)),
           xticks=c(-0.693147180559957,0,0.405465108108171,0.693147180559957,0.91629073187417,1.09861228866813,1.25276296849539,1.38629436111991,1.5040773967763,1.60943791243413))#xlim=c(0.5,4.0), xticks=c(0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0), 
nhanes_forest_educcat_ob <- forestplot(c("All","All, Race-adjusted","NHW","NHB","Mexican-American","Other Hispanic","NHA","Other/Multi-Racial \n including NHA (pre-2011)","Other/Multi-Racial \n (post-2011)"), 
           mean = cbind(nhanes_educ_ob_result_transpose[c(1,5,9,13,17,21,25,29,33),11],nhanes_educ_ob_result_transpose[c(1,5,9,13,17,21,25,29,33),12],nhanes_educ_ob_result_transpose[c(1,5,9,13,17,21,25,29,33),13]),
           lower = cbind(nhanes_educ_ob_result_transpose[c(2,6,10,14,18,22,26,30,34),11],nhanes_educ_ob_result_transpose[c(2,6,10,14,18,22,26,30,34),12],nhanes_educ_ob_result_transpose[c(2,6,10,14,18,22,26,30,34),13]),
           upper = cbind(nhanes_educ_ob_result_transpose[c(3,7,11,15,19,23,27,31,35),11],nhanes_educ_ob_result_transpose[c(3,7,11,15,19,23,27,31,35),12],nhanes_educ_ob_result_transpose[c(3,7,11,15,19,23,27,31,35),13]),
           col = fpColors(box = c('darkslategray3','lightgoldenrod','indianred3')),
           legend=c("HS Degree","Some HS","No HS"),
           legend_args = fpLegend(pos = list(x = 0.9, y = 0.9), gp = gpar(cex=0.9,col = "#CCCCCC", fill = "#F9F9F9")),
           xlab = "OR (95% CI)",
           title = 'Adjusted OR for Educational Attainment (Ref=College Degree) \n on Obesity, Stratified by Race/Ethnicity',
           txt_gp = fpTxtGp(title=gpar(cex=1),xlab=gpar(cex=1), ticks=gpar(cex=1), label=gpar(cex=1)),
           vertices=TRUE,
           line.margin=0.25,
           graph.pos=2,
           clip=c(0.5,5.0), xlog=TRUE, grid=structure(c(0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0)),
           xticks=c(-0.693147180559957,0,0.405465108108171,0.693147180559957,0.91629073187417,1.09861228866813,1.25276296849539,1.38629436111991,1.5040773967763,1.60943791243413))#xlim=c(0.5,4.0), xticks=c(0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0), 
nhanes_forest_inccat_ob <- forestplot(c("All","All, Race-adjusted","NHW","NHB","Mexican-American","Other Hispanic","NHA","Other/Multi-Racial \n including NHA (pre-2011)","Other/Multi-Racial \n (post-2011)"), 
           mean = cbind(nhanes_inc_ob_result_transpose[c(1,5,9,13,17,21,25,29,33),11],nhanes_inc_ob_result_transpose[c(1,5,9,13,17,21,25,29,33),12],nhanes_inc_ob_result_transpose[c(1,5,9,13,17,21,25,29,33),13]),
           lower = cbind(nhanes_inc_ob_result_transpose[c(2,6,10,14,18,22,26,30,34),11],nhanes_inc_ob_result_transpose[c(2,6,10,14,18,22,26,30,34),12],nhanes_inc_ob_result_transpose[c(2,6,10,14,18,22,26,30,34),13]),
           upper = cbind(nhanes_inc_ob_result_transpose[c(3,7,11,15,19,23,27,31,35),11],nhanes_inc_ob_result_transpose[c(3,7,11,15,19,23,27,31,35),12],nhanes_inc_ob_result_transpose[c(3,7,11,15,19,23,27,31,35),13]),
           col = fpColors(box = c('darkslategray3','lightgoldenrod','indianred3')),
           legend=c("$45,000-74,999","$20,000-44,999","Under $20,000"),
           legend_args = fpLegend(pos = list(x = 0.9, y = 0.9), gp = gpar(cex=0.9,col = "#CCCCCC", fill = "#F9F9F9")),
           xlab = "OR (95% CI)",
           title = 'Adjusted OR for Income Category (Ref= More than $75,000) \n on Obesity, Stratified by Race/Ethnicity',
           txt_gp = fpTxtGp(title=gpar(cex=1),xlab=gpar(cex=1), ticks=gpar(cex=1), label=gpar(cex=1)),
           vertices=TRUE,
           line.margin=0.25,
           graph.pos=2,
           clip=c(0.5,5.0), xlog=TRUE, grid=structure(c(0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0)),
           xticks=c(-0.693147180559957,0,0.405465108108171,0.693147180559957,0.91629073187417,1.09861228866813,1.25276296849539,1.38629436111991,1.5040773967763,1.60943791243413))#xlim=c(0.5,4.0), xticks=c(0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0), 


#SuppFig - all categorized SES forest plots#
tiff(file='C:/Users/sarac/OneDrive/Documents/Research/Area v Individual SES/NHANES_SES and AoU_SES/SuppFig4_nhanes_forest_indiv_educinccat_racestrat_transpose.tiff', width=12, height=12, units='in', res=300)
#par(mfrow=c(2,2))
nhanes_forest_educcat_diab
nhanes_forest_inccat_diab
nhanes_forest_educcat_ob
nhanes_forest_inccat_ob

#title(outer=T, main='A', adj=0.0025, cex.main=2, col='black', font=2, line=-2)
#title(outer=T, main='B', adj=0.25, cex.main=2, col='black', font=2, line=-2)
#title(outer=T, main='C', adj=0.505, cex.main=2, col='black', font=2, line=-15)
#title(outer=T, main='D', adj=0.755, cex.main=2, col='black', font=2, line=-20)

#sapply(LETTERS[1:4], function(x) {
#  plot(rnorm(100))
#  fig_label("figure region", cex=2)
#})

dev.off()
par(mfrow=c(1,1))


#original code within tiff() and dev.off()
forestplot(c("All","All, Race-adjusted","NHW","NHB","Mexican-American","Other Hispanic","NHA","Other/Multi-Racial \n including NHA (pre-2011)","Other/Multi-Racial \n (post-2011)"), 
           mean = cbind(nhanes_educ_diab_result_transpose[c(1,5,9,13,17,21,25,29,33),11],nhanes_educ_diab_result_transpose[c(1,5,9,13,17,21,25,29,33),12],nhanes_educ_diab_result_transpose[c(1,5,9,13,17,21,25,29,33),13]),
           lower = cbind(nhanes_educ_diab_result_transpose[c(2,6,10,14,18,22,26,30,34),11],nhanes_educ_diab_result_transpose[c(2,6,10,14,18,22,26,30,34),12],nhanes_educ_diab_result_transpose[c(2,6,10,14,18,22,26,30,34),13]),
           upper = cbind(nhanes_educ_diab_result_transpose[c(3,7,11,15,19,23,27,31,35),11],nhanes_educ_diab_result_transpose[c(3,7,11,15,19,23,27,31,35),12],nhanes_educ_diab_result_transpose[c(3,7,11,15,19,23,27,31,35),13]),
           col = fpColors(box = c('darkslategray3','lightgoldenrod','indianred3')),
           legend=c("HS Degree","Some HS","No HS"),
           legend_args = fpLegend(pos = list(x = 0.9, y = 0.9), gp = gpar(cex=0.9,col = "#CCCCCC", fill = "#F9F9F9")),
           xlab = "OR (95% CI)",
           title = 'Adjusted OR for Educational Attainment (Ref=College Degree) \n on Type 2 Diabetes, Stratified by Race/Ethnicity',
           txt_gp = fpTxtGp(title=gpar(cex=1),xlab=gpar(cex=1), ticks=gpar(cex=1), label=gpar(cex=1)),
           vertices=TRUE,
           line.margin=0.25,
           graph.pos=2,
           clip=c(0.5,5.0), xlog=TRUE, grid=structure(c(0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0)),
           xticks=c(-0.693147180559957,0,0.405465108108171,0.693147180559957,0.91629073187417,1.09861228866813,1.25276296849539,1.38629436111991,1.5040773967763,1.60943791243413))#xlim=c(0.5,4.0), xticks=c(0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0), 
forestplot(c("All","All, Race-adjusted","NHW","NHB","Mexican-American","Other Hispanic","NHA","Other/Multi-Racial \n including NHA (pre-2011)","Other/Multi-Racial \n (post-2011)"), 
           mean = cbind(nhanes_inc_diab_result_transpose[c(1,5,9,13,17,21,25,29,33),11],nhanes_inc_diab_result_transpose[c(1,5,9,13,17,21,25,29,33),12],nhanes_inc_diab_result_transpose[c(1,5,9,13,17,21,25,29,33),13]),
           lower = cbind(nhanes_inc_diab_result_transpose[c(2,6,10,14,18,22,26,30,34),11],nhanes_inc_diab_result_transpose[c(2,6,10,14,18,22,26,30,34),12],nhanes_inc_diab_result_transpose[c(2,6,10,14,18,22,26,30,34),13]),
           upper = cbind(nhanes_inc_diab_result_transpose[c(3,7,11,15,19,23,27,31,35),11],nhanes_inc_diab_result_transpose[c(3,7,11,15,19,23,27,31,35),12],nhanes_inc_diab_result_transpose[c(3,7,11,15,19,23,27,31,35),13]),
           col = fpColors(box = c('darkslategray3','lightgoldenrod','indianred3')),
           legend=c("$45,000-74,999","$20,000-44,999","Under $20,000"),
           legend_args = fpLegend(pos = list(x = 0.9, y = 0.9), gp = gpar(cex=0.9,col = "#CCCCCC", fill = "#F9F9F9")),
           xlab = "OR (95% CI)",
           title = 'Adjusted OR for Income Category (Ref= More than $75,000) \n on Type 2 Diabetes, Stratified by Race/Ethnicity',
           txt_gp = fpTxtGp(title=gpar(cex=1),xlab=gpar(cex=1), ticks=gpar(cex=1), label=gpar(cex=1)),
           vertices=TRUE,
           line.margin=0.25,
           graph.pos=2,
           clip=c(0.5,5.0), xlog=TRUE, grid=structure(c(0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0)),
           xticks=c(-0.693147180559957,0,0.405465108108171,0.693147180559957,0.91629073187417,1.09861228866813,1.25276296849539,1.38629436111991,1.5040773967763,1.60943791243413))#xlim=c(0.5,4.0), xticks=c(0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0), 

forestplot(c("All","All, Race-adjusted","NHW","NHB","Mexican-American","Other Hispanic","NHA","Other/Multi-Racial \n including NHA (pre-2011)","Other/Multi-Racial \n (post-2011)"), 
           mean = cbind(nhanes_educ_ob_result_transpose[c(1,5,9,13,17,21,25,29,33),11],nhanes_educ_ob_result_transpose[c(1,5,9,13,17,21,25,29,33),12],nhanes_educ_ob_result_transpose[c(1,5,9,13,17,21,25,29,33),13]),
           lower = cbind(nhanes_educ_ob_result_transpose[c(2,6,10,14,18,22,26,30,34),11],nhanes_educ_ob_result_transpose[c(2,6,10,14,18,22,26,30,34),12],nhanes_educ_ob_result_transpose[c(2,6,10,14,18,22,26,30,34),13]),
           upper = cbind(nhanes_educ_ob_result_transpose[c(3,7,11,15,19,23,27,31,35),11],nhanes_educ_ob_result_transpose[c(3,7,11,15,19,23,27,31,35),12],nhanes_educ_ob_result_transpose[c(3,7,11,15,19,23,27,31,35),13]),
           col = fpColors(box = c('darkslategray3','lightgoldenrod','indianred3')),
           legend=c("HS Degree","Some HS","No HS"),
           legend_args = fpLegend(pos = list(x = 0.9, y = 0.9), gp = gpar(cex=0.9,col = "#CCCCCC", fill = "#F9F9F9")),
           xlab = "OR (95% CI)",
           title = 'Adjusted OR for Educational Attainment (Ref=College Degree) \n on Obesity, Stratified by Race/Ethnicity',
           txt_gp = fpTxtGp(title=gpar(cex=1),xlab=gpar(cex=1), ticks=gpar(cex=1), label=gpar(cex=1)),
           vertices=TRUE,
           line.margin=0.25,
           graph.pos=2,
           clip=c(0.5,5.0), xlog=TRUE, grid=structure(c(0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0)),
           xticks=c(-0.693147180559957,0,0.405465108108171,0.693147180559957,0.91629073187417,1.09861228866813,1.25276296849539,1.38629436111991,1.5040773967763,1.60943791243413))#xlim=c(0.5,4.0), xticks=c(0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0), 

forestplot(c("All","All, Race-adjusted","NHW","NHB","Mexican-American","Other Hispanic","NHA","Other/Multi-Racial \n including NHA (pre-2011)","Other/Multi-Racial \n (post-2011)"), 
           mean = cbind(nhanes_inc_ob_result_transpose[c(1,5,9,13,17,21,25,29,33),11],nhanes_inc_ob_result_transpose[c(1,5,9,13,17,21,25,29,33),12],nhanes_inc_ob_result_transpose[c(1,5,9,13,17,21,25,29,33),13]),
           lower = cbind(nhanes_inc_ob_result_transpose[c(2,6,10,14,18,22,26,30,34),11],nhanes_inc_ob_result_transpose[c(2,6,10,14,18,22,26,30,34),12],nhanes_inc_ob_result_transpose[c(2,6,10,14,18,22,26,30,34),13]),
           upper = cbind(nhanes_inc_ob_result_transpose[c(3,7,11,15,19,23,27,31,35),11],nhanes_inc_ob_result_transpose[c(3,7,11,15,19,23,27,31,35),12],nhanes_inc_ob_result_transpose[c(3,7,11,15,19,23,27,31,35),13]),
           col = fpColors(box = c('darkslategray3','lightgoldenrod','indianred3')),
           legend=c("$45,000-74,999","$20,000-44,999","Under $20,000"),
           legend_args = fpLegend(pos = list(x = 0.9, y = 0.9), gp = gpar(cex=0.9,col = "#CCCCCC", fill = "#F9F9F9")),
           xlab = "OR (95% CI)",
           title = 'Adjusted OR for Income Category (Ref= More than $75,000) \n on Obesity, Stratified by Race/Ethnicity',
           txt_gp = fpTxtGp(title=gpar(cex=1),xlab=gpar(cex=1), ticks=gpar(cex=1), label=gpar(cex=1)),
           vertices=TRUE,
           line.margin=0.25,
           graph.pos=2,
           clip=c(0.5,5.0), xlog=TRUE, grid=structure(c(0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0)),
           xticks=c(-0.693147180559957,0,0.405465108108171,0.693147180559957,0.91629073187417,1.09861228866813,1.25276296849539,1.38629436111991,1.5040773967763,1.60943791243413))#xlim=c(0.5,4.0), xticks=c(0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0), 









#analyses NOT stratified by race
mod_diab_educ_all <- svyglm(design = surv_all, diab ~ age + female + survyr + educ_simple, family=binomial(link=logit))
summary(mod_diab_educ_all)
mod_diab_inc_all <- svyglm(design = surv_all, diab ~ age + female + survyr + hhinc, family=binomial(link=logit))
summary(mod_diab_inc_nhw)
mod_diab_incsimp_all <- svyglm(design = surv_all, diab ~ age + female + survyr + inc_simple, family=binomial(link=logit))
summary(mod_diab_incsimp_nhw)
mod_diab_emp_all <- svyglm(design = surv_all, diab ~ age + female + survyr + work_yn, family=binomial(link=logit))
tiff(file='C:/Users/sarac/OneDrive/Documents/Research/Area v Individual SES/NHANES_SES and AoU_SES/nhanes_forest_indiv_educ_nonstrat.tiff', width=7, height=5, units='in', res=300)
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

tiff(file='C:/Users/sarac/OneDrive/Documents/Research/Area v Individual SES/NHANES_SES and AoU_SES/nhanes_forest_indiv_inc_nonstrat.tiff', width=7, height=5, units='in', res=300)
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
#nhanes_std <- svystandardize(surv_all, by=~agecat, over=~1, population=popage)#, excluding.missing=~diab) #standardizes across entire pop, not within racecats

100*prop.table(table(nhanes$racecat, nhanes$diab),1)
100*svyby(~diab, ~racecat, svymean, design=surv_all)

nhanes_std <- svystandardize(surv_all, by = ~ agecat, over = ~racecat, 
                             population = popage,
                             excluding.missing = make.formula(c("age", "diab")))
100*svyby(~diab, ~racecat, svymean, design=nhanes_std)

barplot(100*(svyby(~diab, ~racecat, svymean, design=nhanes_std))$diab, main='Age-Adjusted T2D by R/E', ylim=c(0,20),
        legend=T, legend.text=rownames(table(nhanes$racecat)), args.legend=list(x="bottom"))

#not working
nhanes_std <- nhanes %>% group_by(racecat) %>%
  svystandardize(surv_all, by=~agecat, over=~1, population=popage)
prop.table(table(nhanes_std$racecat, nhanes_std$diab))

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
race_educ_diab_agestd <- as.data.frame(svyby(~diab, ~racecat+educ_simple, svymean, design=nhanes_std))
race_educ_diab_agestd_prev <- rbind(t(race_educ_diab_agestd$diab[22:28]),t(race_educ_diab_agestd$diab[15:21]),
                                    t(race_educ_diab_agestd$diab[8:14]),t(race_educ_diab_agestd$diab[1:7]))
race_educ_diab_agestd_prev
colnames(race_educ_diab_agestd_prev) <- rownames(table(nhanes$racecat))
race_educ_diab_agestd_se <- rbind(t(race_educ_diab_agestd$se[22:28]),t(race_educ_diab_agestd$se[15:21]),
                                  t(race_educ_diab_agestd$se[8:14]),t(race_educ_diab_agestd$se[1:7]))
race_educ_diab_agestd_conf <- race_educ_diab_agestd_se*qnorm(0.975)
mid_educ<-barplot(100*race_educ_diab_agestd_prev, legend=rownames(table(nhanes$educ_simple)), beside=TRUE)
barplot(100*race_educ_diab_agestd_prev, main="Age-Adjusted Prevalence of Type 2 Diabetes by Race/Ethnicity \n and Educational Attainment (Categorical)", xaxt='n', xlab='', ylab='Percent with Type 2 Diabetes', 
        col=brewer.pal(4,name='YlGnBu'),#col=c('darkslategray3','darkseagreen','lightgoldenrod','lightsalmon'),#,'tomato'), 
        legend.text=c('No HS','Some HS','HS Degree','College Degree'), args.legend=list(x="top", cex=0.8,ncol=2, title='Educational Attainment'),cex.lab=0.8,cex.main=0.8, ylim=c(0,40),beside=TRUE)
text(cex=0.8, x=colMeans(mid_educ)-.25, y=-2.5, racenames, xpd=T, srt=60)
arrows(x0=mid_educ, y0=100*race_educ_diab_agestd_prev-100*(race_educ_diab_agestd_conf), x1=mid_educ, y1=100*race_educ_diab_agestd_prev+100*race_educ_diab_agestd_conf, code=3, angle=90, length=0.04)

#educ-obese grouped bar
race_educ_obese_agestd <- as.data.frame(svyby(~obese, ~racecat+educ_simple, svymean, design=nhanes_std))
race_educ_obese_agestd_prev <- rbind(t(race_educ_obese_agestd$obese[22:28]),t(race_educ_obese_agestd$obese[15:21]),
                                    t(race_educ_obese_agestd$obese[8:14]),t(race_educ_obese_agestd$obese[1:7]))
race_educ_obese_agestd_prev
colnames(race_educ_obese_agestd_prev) <- rownames(table(nhanes$racecat))
race_educ_obese_agestd_se <- rbind(t(race_educ_obese_agestd$se[22:28]),t(race_educ_obese_agestd$se[15:21]),
                                  t(race_educ_obese_agestd$se[8:14]),t(race_educ_obese_agestd$se[1:7]))
race_educ_obese_agestd_conf <- race_educ_obese_agestd_se*qnorm(0.975)
mid_educ<-barplot(100*race_educ_obese_agestd_prev, legend=rownames(table(nhanes$educ_simple)), beside=TRUE)
barplot(100*race_educ_obese_agestd_prev, main="Age-Adjusted Prevalence of Obesity by Race/Ethnicity \n and Educational Attainment (Categorical)", xaxt='n', xlab='', ylab='Percent with Type 2 obeseetes', 
        col=brewer.pal(4,name='YlGnBu'),#col=c('darkslategray3','darkseagreen','lightgoldenrod','lightsalmon'),#,'tomato'), 
        legend.text=c('No HS','Some HS','HS Degree','College Degree'), args.legend=list(x="top", cex=0.8,ncol=2, title='Educational Attainment'),cex.lab=0.8,cex.main=0.8, ylim=c(0,80),beside=TRUE)
text(cex=0.8, x=colMeans(mid_educ)-.25, y=-2.5, racenames, xpd=T, srt=60)
arrows(x0=mid_educ, y0=100*race_educ_obese_agestd_prev-100*(race_educ_obese_agestd_conf), x1=mid_educ, y1=100*race_educ_obese_agestd_prev+100*race_educ_obese_agestd_conf, code=3, angle=90, length=0.04)


#inc-diab grouped bar
race_inc_diab_agestd <- as.data.frame(svyby(~diab, ~racecat+inc_simple, svymean, design=nhanes_std))
race_inc_diab_agestd_prev <- rbind(t(race_inc_diab_agestd$diab[22:28]),t(race_inc_diab_agestd$diab[15:21]),
                                    t(race_inc_diab_agestd$diab[8:14]),t(race_inc_diab_agestd$diab[1:7]))
race_inc_diab_agestd_prev
colnames(race_inc_diab_agestd_prev) <- rownames(table(nhanes$racecat))
race_inc_diab_agestd_se <- rbind(t(race_inc_diab_agestd$se[22:28]),t(race_inc_diab_agestd$se[15:21]),
                                  t(race_inc_diab_agestd$se[8:14]),t(race_inc_diab_agestd$se[1:7]))
race_inc_diab_agestd_conf <- race_inc_diab_agestd_se*qnorm(0.975)
mid_inc<-barplot(100*race_inc_diab_agestd_prev, legend=rownames(table(nhanes$inc_simple)), beside=TRUE)
barplot(100*race_inc_diab_agestd_prev, main="Age-Adjusted Prevalence of Type 2 Diabetes by Race/Ethnicity \n and Income (Categorical)", xaxt='n',xlab='', ylab='Percent with Type 2 diabetes', 
        col=brewer.pal(4,name='YlGnBu'),#col=c('darkslategray3','darkseagreen','lightgoldenrod','lightsalmon'),#,'tomato'), 
        legend.text=c("Under $20,000","$20,000-44,999","$45,000-74,999","More than $75,000"), args.legend=list(x="top", cex=0.8,ncol=2, title='Income Category'),cex.lab=0.8,cex.main=0.8, ylim=c(0,40),beside=TRUE)
text(cex=0.8, x=colMeans(mid_inc)-.25, y=-2.5, racenames, xpd=T, srt=60)
arrows(x0=mid_inc, y0=100*race_inc_diab_agestd_prev-100*(race_inc_diab_agestd_conf), x1=mid_inc, y1=100*race_inc_diab_agestd_prev+100*race_inc_diab_agestd_conf, code=3, angle=90, length=0.04)

#inc-obese grouped bar
race_inc_obese_agestd <- as.data.frame(svyby(~obese, ~racecat+inc_simple, svymean, design=nhanes_std))
race_inc_obese_agestd_prev <- rbind(t(race_inc_obese_agestd$obese[22:28]),t(race_inc_obese_agestd$obese[15:21]),
                                     t(race_inc_obese_agestd$obese[8:14]),t(race_inc_obese_agestd$obese[1:7]))
race_inc_obese_agestd_prev
colnames(race_inc_obese_agestd_prev) <- rownames(table(nhanes$racecat))
race_inc_obese_agestd_se <- rbind(t(race_inc_obese_agestd$se[22:28]),t(race_inc_obese_agestd$se[15:21]),
                                   t(race_inc_obese_agestd$se[8:14]),t(race_inc_obese_agestd$se[1:7]))
race_inc_obese_agestd_conf <- race_inc_obese_agestd_se*qnorm(0.975)
mid_inc<-barplot(100*race_inc_obese_agestd_prev, legend=rownames(table(nhanes$inc_simple)), beside=TRUE)
barplot(100*race_inc_obese_agestd_prev, main="Age-Adjusted Prevalence of Obesity by Race/Ethnicity \n and Income (Categorical)", xaxt='n', xlab='', ylab='Percent with Type 2 obeseetes', 
        col=brewer.pal(4,name='YlGnBu'),#col=c('darkslategray3','darkseagreen','lightgoldenrod','lightsalmon'),#,'tomato'), 
        legend.text=c("Under $20,000","$20,000-44,999","$45,000-74,999","More than $75,000"), args.legend=list(x="top", cex=0.8,ncol=2, title='Income Category'),cex.lab=0.8,cex.main=0.8, ylim=c(0,80),beside=TRUE)
text(cex=0.8, x=colMeans(mid_inc)-.25, y=-2.5, racenames, xpd=T, srt=60)
arrows(x0=mid_inc, y0=100*race_inc_obese_agestd_prev-100*(race_inc_obese_agestd_conf), x1=mid_inc, y1=100*race_inc_obese_agestd_prev+100*race_inc_obese_agestd_conf, code=3, angle=90, length=0.04)

fig1_agestdprev_forjulie <- cbind(rbind(race_educ_diab_agestd_prev, race_inc_diab_agestd_prev),
                       rbind(race_educ_obese_agestd_prev, race_inc_obese_agestd_prev))
rownames(fig1_agestdprev_forjulie) <-c("No HS","Some HS","HS Degree","College Degree",
                                       "Under $20,000","$20,000-44,999","$45,000-74,999","More than $75,000")
fig1_agestdprev_forjulie <- rbind(c("diab","diab","diab","diab","diab","diab","diab",
                                    "obese","obese","obese","obese","obese","obese","obese"),
                                    fig1_agestdprev_forjulie)
write.csv(fig1_agestdprev_forjulie, 'C:/Users/sarac/OneDrive/Documents/Research/Area v Individual SES/NHANES_SES and AoU_SES/nhanes_ses_agestdprev_forjulie.csv',row.names=TRUE)

fig1_agestdprev_ci_forjulie <- cbind(rbind(race_educ_diab_agestd_conf, race_inc_diab_agestd_conf),
                                  rbind(race_educ_obese_agestd_conf, race_inc_obese_agestd_conf))
rownames(fig1_agestdprev_ci_forjulie) <-c("No HS","Some HS","HS Degree","College Degree",
                                          "Under $20,000","$20,000-44,999","$45,000-74,999","More than $75,000")
fig1_agestdprev_ci_forjulie <- rbind(c("diab","diab","diab","diab","diab","diab","diab",
                                    "obese","obese","obese","obese","obese","obese","obese"),
                                    fig1_agestdprev_ci_forjulie)
write.csv(fig1_agestdprev_ci_forjulie, 'C:/Users/sarac/OneDrive/Documents/Research/Area v Individual SES/NHANES_SES and AoU_SES/nhanes_ses_agestdprev_ci_forjulie.csv',row.names=TRUE)


#####save grouped bar age-adjusted prevalence figure#####
tiff(file='C:/Users/sarac/OneDrive/Documents/Research/Area v Individual SES/NHANES_SES and AoU_SES/Fig1_nhanes_bar_indiv_educinc_agestddiabob.tiff', width=10, height=12, units='in', res=100)
racenames <- c('NHW','NHB','Mexican-\nAmerican','Other \nHispanic','NHA','Other or \nMulti-Racial \n(pre-2011)','Other or \nMulti-Racial \n(post-2011)')
par(mfrow=c(2,2))
barplot(100*race_educ_diab_agestd_prev, main="Age-Adjusted Prevalence of Type 2 Diabetes by Race/Ethnicity \n and Educational Attainment (Categorical)", xaxt='n', xlab='', ylab='Percent with Type 2 Diabetes', 
        col=brewer.pal(4,name='YlGnBu'),#col=c('darkslategray3','darkseagreen','lightgoldenrod','lightsalmon'),#,'tomato'), 
        legend.text=c('No HS','Some HS','HS Degree','College Degree'), args.legend=list(x="top", cex=0.8,ncol=2, title='Educational Attainment'),cex.lab=0.8,cex.main=0.8, ylim=c(0,40),beside=TRUE)
text(cex=0.8, x=colMeans(mid_educ)-.25, y=-2.5, racenames, xpd=T, srt=60)
arrows(x0=mid_educ, y0=100*race_educ_diab_agestd_prev-100*(race_educ_diab_agestd_conf), x1=mid_educ, y1=100*race_educ_diab_agestd_prev+100*race_educ_diab_agestd_conf, code=3, angle=90, length=0.04)

barplot(100*race_educ_obese_agestd_prev, main="Age-Adjusted Prevalence of Obesity by Race/Ethnicity \n and Educational Attainment (Categorical)", xaxt='n', xlab='', ylab='Percent with Type 2 obeseetes', 
        col=brewer.pal(4,name='YlGnBu'),#col=c('darkslategray3','darkseagreen','lightgoldenrod','lightsalmon'),#,'tomato'), 
        legend.text=c('No HS','Some HS','HS Degree','College Degree'), args.legend=list(x="top", cex=0.8,ncol=2, title='Educational Attainment'),cex.lab=0.8,cex.main=0.8, ylim=c(0,80),beside=TRUE)
text(cex=0.8, x=colMeans(mid_educ)-.25, y=-5, racenames, xpd=T, srt=60)
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
dm2_counts_educ <- table(nhanes$hreduc[nhanes$diab==1 & !nhanes$hreduc %in% educ_unknown], nhanes$racecat[nhanes$diab==1 & !nhanes$hreduc %in% educ_unknown])
dm2_counts_educ <- dm2_counts_educ[1:7,]
#dm2_counts_educ <- svymean(~diab==1, surv_all)

raceeduc_counts_dm <- table(nhanes$hreduc[!nhanes$hreduc %in% educ_unknown], nhanes$racecat[!nhanes$hreduc %in% educ_unknown]) #whichever is second will be on the x axis groupings, first will be series 1/2/3 colors
raceeduc_counts_dm <- raceeduc_counts_dm[1:7,]
mid_educ<-barplot(100*dm2_counts_educ/raceeduc_counts_dm, legend=rownames(raceeduc_counts_dm), beside=TRUE)
raceeduc_confint_dm <- nhanes[!nhanes$hreduc %in% educ_unknown,] %>%
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
prop.table(table(nhanes$racecat, nhanes$obese, exclude=NULL),1)
#educome categorical
dm2_counts_educcat <- table(nhanes$educ_simple[nhanes$diab==1], nhanes$racecat[nhanes$diab==1])
raceeduccat_counts_dm <- table(nhanes$educ_simple, nhanes$racecat) #whichever is second will be on the x axis groupings, first will be series 1/2/3 colors
mid_educcat<-barplot(100*dm2_counts_educcat/raceeduccat_counts_dm, legend=rownames(raceeduccat_counts_dm), beside=TRUE)
raceeduccat_confint_dm <- nhanes %>%
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


#ob_counts_educcat <- table(nhanes$educ_simple[nhanes$obese==1], nhanes$racecat[nhanes$obese==1])
#raceeduccat_counts_ob <- table(nhanes$educ_simple, nhanes$racecat) #whichever is second will be on the x axis groupings, first will be series 1/2/3 colors
#100*ob_counts_educcat/raceeduccat_counts_ob

#educ_num quintiles
nhanes$educ_num_quint <- ntile((nhanes$educ_num), 5)
summary(nhanes$educ_num[nhanes$educ_num_quint==3])
table(nhanes$racecat, nhanes$educ_num_quint, exclude=NULL)
violin<-ggplot(nhanes[!is.na(nhanes$educ_simple),], aes(factor(x=educ_num_quint), y=educ_num, fill=factor(racecat)), exclude=TRUE) + geom_violin(position=position_dodge(1))
violin + geom_boxplot(position=position_dodge(1), width=0.1)

barplot(100*prop.table(table(nhanes$educ_num_quint, nhanes$racecat),2))#/table(nhanes$racecat))

dm2_counts_educquint <- table(nhanes$educ_num_quint[nhanes$diab==1], nhanes$racecat[nhanes$diab==1])
raceeducquint_counts_dm <- table(nhanes$educ_num_quint, nhanes$racecat) #whichever is second will be on the x axis groupings, first will be series 1/2/3 colors
mid_educquint<-barplot(100*dm2_counts_educquint/raceeducquint_counts_dm, legend=rownames(raceeducquint_counts_dm), beside=TRUE)
raceeducquint_confint_dm <- nhanes %>%
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
nhanes <- nhanes %>% group_by(racecat) %>% mutate(educ_num_quintbyrace = ntile(educ_num, 5))
table(nhanes$racecat, nhanes$educ_num_quintbyrace, exclude=NULL)
violin<-ggplot(nhanes[!is.na(nhanes$educ_simple),], aes(factor(x=educ_num_quintbyrace), y=educ_num, fill=factor(racecat)), exclude=TRUE) + geom_violin(position=position_dodge(1))
violin + geom_boxplot(position=position_dodge(1), width=0.1)
violin<-ggplot(nhanes[!is.na(nhanes$educ_simple),], aes(factor(x=racecat), y=educ_num, fill=factor(educ_num_quintbyrace)), exclude=TRUE) + geom_violin(position=position_dodge(1))
violin + geom_boxplot(position=position_dodge(1), width=0.1)

dm2_counts_educquintbyrace <- table(nhanes$educ_num_quintbyrace[nhanes$diab==1], nhanes$racecat[nhanes$diab==1])
raceeducquintbyrace_counts_dm <- table(nhanes$educ_num_quintbyrace, nhanes$racecat) #whichever is second will be on the x axis groupings, first will be series 1/2/3 colors
mid_educquintbyrace<-barplot(100*dm2_counts_educquintbyrace/raceeducquintbyrace_counts_dm, legend=rownames(raceeducquintbyrace_counts_dm), beside=TRUE)
raceeducquintbyrace_confint_dm <- nhanes %>%
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
dm2_counts_inccat <- table(nhanes$inc_simple[nhanes$diab==1], nhanes$racecat[nhanes$diab==1])
raceinccat_counts_dm <- table(nhanes$inc_simple, nhanes$racecat) #whichever is second will be on the x axis groupings, first will be series 1/2/3 colors
mid_inccat<-barplot(100*dm2_counts_inccat/raceinccat_counts_dm, legend=rownames(raceinccat_counts_dm), beside=TRUE)
raceinccat_confint_dm <- nhanes %>%
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
nhanes$inc_to_pov_quint <- ntile((nhanes$inc_to_pov), 5)
table(nhanes$racecat, nhanes$inc_to_pov_quint, exclude=NULL)
dm2_counts_incquint <- table(nhanes$inc_to_pov_quint[nhanes$diab==1], nhanes$racecat[nhanes$diab==1])
raceincquint_counts_dm <- table(nhanes$inc_to_pov_quint, nhanes$racecat) #whichever is second will be on the x axis groupings, first will be series 1/2/3 colors
mid_incquint<-barplot(100*dm2_counts_incquint/raceincquint_counts_dm, legend=rownames(raceincquint_counts_dm), beside=TRUE)
raceincquint_confint_dm <- nhanes %>%
  group_by(racecat, inc_to_pov_quint) %>%
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
nhanes <- nhanes %>% group_by(racecat) %>% mutate(inc_to_pov_quintbyrace = ntile(inc_to_pov, 5))
table(nhanes$racecat, nhanes$inc_to_pov_quintbyrace, exclude=NULL)

dm2_counts_incquintbyrace <- table(nhanes$inc_to_pov_quintbyrace[nhanes$diab==1], nhanes$racecat[nhanes$diab==1])
raceincquintbyrace_counts_dm <- table(nhanes$inc_to_pov_quintbyrace, nhanes$racecat) #whichever is second will be on the x axis groupings, first will be series 1/2/3 colors
mid_incquintbyrace<-barplot(100*dm2_counts_incquintbyrace/raceincquintbyrace_counts_dm, legend=rownames(raceincquintbyrace_counts_dm), beside=TRUE)
raceincquintbyrace_confint_dm <- nhanes %>%
  group_by(racecat, inc_to_pov_quintbyrace) %>%
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
prop.table(table(nhanes$racecat, nhanes$obese, exclude=NULL),1)
#educ categorical
ob_counts_educcat <- table(nhanes$educ_simple[nhanes$obese==1], nhanes$racecat[nhanes$obese==1])
raceeduccat_counts_dm <- table(nhanes$educ_simple, nhanes$racecat) #whichever is second will be on the x axis groupings, first will be series 1/2/3 colors
mid_educcat<-barplot(100*ob_counts_educcat/raceeduccat_counts_dm, legend=rownames(raceeduccat_counts_dm), beside=TRUE)
raceeduccat_confint_dm <- nhanes %>%
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
nhanes$educ_num_quint <- ntile((nhanes$educ_num), 5)
summary(nhanes$educ_num[nhanes$educ_num_quint==3])
table(nhanes$racecat, nhanes$educ_num_quint, exclude=NULL)
violin<-ggplot(nhanes[!is.na(nhanes$educ_simple),], aes(factor(x=educ_num_quint), y=educ_num, fill=factor(racecat)), exclude=TRUE) + geom_violin(position=position_dodge(1))
violin + geom_boxplot(position=position_dodge(1), width=0.1)

barplot(100*prop.table(table(nhanes$educ_num_quint, nhanes$racecat),2))#/table(nhanes$racecat))

ob_counts_educquint <- table(nhanes$educ_num_quint[nhanes$obese==1], nhanes$racecat[nhanes$obese==1])
raceeducquint_counts_dm <- table(nhanes$educ_num_quint, nhanes$racecat) #whichever is second will be on the x axis groupings, first will be series 1/2/3 colors
mid_educquint<-barplot(100*ob_counts_educquint/raceeducquint_counts_dm, legend=rownames(raceeducquint_counts_dm), beside=TRUE)
raceeducquint_confint_dm <- nhanes %>%
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
nhanes <- nhanes %>% group_by(racecat) %>% mutate(educ_num_quintbyrace = ntile(educ_num, 5))
table(nhanes$racecat, nhanes$educ_num_quintbyrace, exclude=NULL)
violin<-ggplot(nhanes[!is.na(nhanes$educ_simple),], aes(factor(x=educ_num_quintbyrace), y=educ_num, fill=factor(racecat)), exclude=TRUE) + geom_violin(position=position_dodge(1))
violin + geom_boxplot(position=position_dodge(1), width=0.1)
violin<-ggplot(nhanes[!is.na(nhanes$educ_simple),], aes(factor(x=racecat), y=educ_num, fill=factor(educ_num_quintbyrace)), exclude=TRUE) + geom_violin(position=position_dodge(1))
violin + geom_boxplot(position=position_dodge(1), width=0.1)

ob_counts_educquintbyrace <- table(nhanes$educ_num_quintbyrace[nhanes$obese==1], nhanes$racecat[nhanes$obese==1])
raceeducquintbyrace_counts_dm <- table(nhanes$educ_num_quintbyrace, nhanes$racecat) #whichever is second will be on the x axis groupings, first will be series 1/2/3 colors
mid_educquintbyrace<-barplot(100*ob_counts_educquintbyrace/raceeducquintbyrace_counts_dm, legend=rownames(raceeducquintbyrace_counts_dm), beside=TRUE)
raceeducquintbyrace_confint_dm <- nhanes %>%
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
ob_counts_inccat <- table(nhanes$inc_simple[nhanes$obese==1], nhanes$racecat[nhanes$obese==1])
raceinccat_counts_dm <- table(nhanes$inc_simple, nhanes$racecat) #whichever is second will be on the x axis groupings, first will be series 1/2/3 colors
mid_inccat<-barplot(100*ob_counts_inccat/raceinccat_counts_dm, legend=rownames(raceinccat_counts_dm), beside=TRUE)
raceinccat_confint_dm <- nhanes %>%
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
nhanes$inc_to_pov_quint <- ntile((nhanes$inc_to_pov), 5)
table(nhanes$racecat, nhanes$inc_to_pov_quint, exclude=NULL)
ob_counts_incquint <- table(nhanes$inc_to_pov_quint[nhanes$obese==1], nhanes$racecat[nhanes$obese==1])
raceincquint_counts_dm <- table(nhanes$inc_to_pov_quint, nhanes$racecat) #whichever is second will be on the x axis groupings, first will be series 1/2/3 colors
mid_incquint<-barplot(100*ob_counts_incquint/raceincquint_counts_dm, legend=rownames(raceincquint_counts_dm), beside=TRUE)
raceincquint_confint_dm <- nhanes %>%
  group_by(racecat, inc_to_pov_quint) %>%
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
nhanes <- nhanes %>% group_by(racecat) %>% mutate(inc_to_pov_quintbyrace = ntile(inc_to_pov, 5))
table(nhanes$racecat, nhanes$inc_to_pov_quintbyrace, exclude=NULL)

ob_counts_incquintbyrace <- table(nhanes$inc_to_pov_quintbyrace[nhanes$obese==1], nhanes$racecat[nhanes$obese==1])
raceincquintbyrace_counts_dm <- table(nhanes$inc_to_pov_quintbyrace, nhanes$racecat) #whichever is second will be on the x axis groupings, first will be series 1/2/3 colors
mid_incquintbyrace<-barplot(100*ob_counts_incquintbyrace/raceincquintbyrace_counts_dm, legend=rownames(raceincquintbyrace_counts_dm), beside=TRUE)
raceincquintbyrace_confint_dm <- nhanes %>%
  group_by(racecat, inc_to_pov_quintbyrace) %>%
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
diab_adj <- svyglm(design=surv_all, diab ~ age + female + survyr + racecat, family=quasibinomial())#family=stats::gaussian())
nhanes$pred_prev_diab <- predict(diab_adj, newdata=nhanes, type='response')
summary(nhanes$pred_prev_diab)
mean(nhanes$pred_prev_diab[nhanes$racecat=='NHAsian'])
ggplot(data = subset(nhanes, racecat=='NHW'), 
       aes(x = pred_prev_diab, fill = factor(racecat)))+
  geom_histogram(aes(y = ..density..))+
  geom_histogram(data = subset(nhanes, racecat=='NHB'), 
                 aes(x = pred_prev_diab, y = -..density.., fill=factor(racecat)))+
  xlab("Predicted T2D Prevalence")+ylab("Density")+
  ggtitle("Histogram of Predicted T2D Prevalence by NHW v NHB") + 
  scale_fill_discrete("Race: NHW v NHB")
#estimated prevalence barplot - educ diab
dm2_counts_educcat <- table(nhanes$educ_simple[nhanes$diab==1], nhanes$racecat[nhanes$diab==1])
raceeduccat_counts_dm <- table(nhanes$educ_simple, nhanes$racecat) #whichever is second will be on the x axis groupings, first will be series 1/2/3 colors
mid_educcat<-barplot(100*dm2_counts_educcat/raceeduccat_counts_dm, legend=rownames(raceeduccat_counts_dm), beside=TRUE)

raceeduccat_mean_dm <- nhanes %>% 
  group_by(racecat, educ_simple) %>%
  summarise(
    mean=100*mean(pred_prev_diab, na.rm=T)  )
raceeduccat_mean_dm <- raceeduccat_mean_dm[complete.cases(raceeduccat_mean_dm),]
raceeduccat_confint_dm <- nhanes %>%
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
dm2_counts_inccat <- table(nhanes$inc_simple[nhanes$diab==1], nhanes$racecat[nhanes$diab==1])
raceinccat_counts_dm <- table(nhanes$inc_simple, nhanes$racecat) #whichever is second will be on the x axis groupings, first will be series 1/2/3 colors
mid_inccat<-barplot(100*dm2_counts_inccat/raceinccat_counts_dm, legend=rownames(raceinccat_counts_dm), beside=TRUE)

raceinccat_mean_dm <- nhanes %>% 
  group_by(racecat, inc_simple) %>%
  summarise(
    mean=100*mean(pred_prev_diab, na.rm=T)  )
raceinccat_mean_dm <- raceinccat_mean_dm[complete.cases(raceinccat_mean_dm),]
raceinccat_confint_dm <- nhanes %>%
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
obese_adj <- svyglm(design=surv_all, obese ~ age + female + survyr + racecat, family=quasibinomial())#family=stats::gaussian())
nhanes$pred_prev_obese <- predict(obese_adj, newdata=nhanes, type='response')
summary(nhanes$pred_prev_obese)
mean(nhanes$pred_prev_obese[nhanes$racecat=='NHAsian'])
ggplot(data = subset(nhanes, racecat=='NHW'), 
       aes(x = pred_prev_obese, fill = factor(racecat)))+
  geom_histogram(aes(y = ..density..))+
  geom_histogram(data = subset(nhanes, racecat=='NHB'), 
                 aes(x = pred_prev_obese, y = -..density.., fill=factor(racecat)))+
  xlab("Predicted T2D Prevalence")+ylab("Density")+
  ggtitle("Histogram of Predicted T2D Prevalence by NHW v NHB") + 
  scale_fill_discrete("Race: NHW v NHB")
#estimated prevalence barplot - educ obese
ob_counts_educcat <- table(nhanes$educ_simple[nhanes$obese==1], nhanes$racecat[nhanes$obese==1])
raceeduccat_counts_dm <- table(nhanes$educ_simple, nhanes$racecat) #whichever is second will be on the x axis groupings, first will be series 1/2/3 colors
mid_educcat<-barplot(100*ob_counts_educcat/raceeduccat_counts_dm, legend=rownames(raceeduccat_counts_dm), beside=TRUE)

raceeduccat_mean_ob <- nhanes %>% 
  group_by(racecat, educ_simple) %>%
  summarise(
    mean=100*mean(pred_prev_obese, na.rm=T)  )
raceeduccat_mean_ob <- raceeduccat_mean_ob[complete.cases(raceeduccat_mean_ob),]
raceeduccat_confint_ob <- nhanes %>%
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
ob_counts_inccat <- table(nhanes$inc_simple[nhanes$obese==1], nhanes$racecat[nhanes$obese==1])
raceinccat_counts_dm <- table(nhanes$inc_simple, nhanes$racecat) #whichever is second will be on the x axis groupings, first will be series 1/2/3 colors
mid_inccat<-barplot(100*ob_counts_inccat/raceinccat_counts_dm, legend=rownames(raceinccat_counts_dm), beside=TRUE)

raceinccat_mean_ob <- nhanes %>% 
  group_by(racecat, inc_simple) %>%
  summarise(
    mean=100*mean(pred_prev_obese, na.rm=T)  )
raceinccat_mean_ob <- raceinccat_mean_ob[complete.cases(raceinccat_mean_ob),]
raceinccat_confint_ob <- nhanes %>%
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
tiff(file='C:/Users/sarac/OneDrive/Documents/Research/Area v Individual SES/NHANES_SES and AoU_SES/nhanes_bar_indiv_educinc_preddiabob.tif', width=8, height=12, units='in', res=100)
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
surv_all2 <- svydesign(id = ~SDMVPSU, strata = ~ SDMVSTRA, weights = ~weights, data=nhanes, nest=T)
nhanes %>% group_by(racecat) %>% summarise(mean=mean(age), na.rm=T)
summary(nhanes$pred_prev_diab)
mean(nhanes$pred_prev_diab)
pred_prev_byrace <- rbind(c(mean(nhanes$pred_prev_diab), svymean(~pred_prev_diab, subset(surv_all2, age >= 18 & !insur_stability=='Missing'))),
                            c(mean(nhanes$pred_prev_diab[nhanes$racecat=='NHW']),svymean(~pred_prev_diab, subset(surv_all2, age >= 18 & !insur_stability=='Missing' & racecat=='NHW'))),
                            c(mean(nhanes$pred_prev_diab[nhanes$racecat=='NHB']), svymean(~pred_prev_diab, subset(surv_all2, age >= 18 & !insur_stability=='Missing' & racecat=='NHB'))),
                            c(mean(nhanes$pred_prev_diab[nhanes$racecat=='NHAsian']), svymean(~pred_prev_diab, subset(surv_all2, age >= 18 & !insur_stability=='Missing' & racecat=='NHAsian'))),
                            c(mean(nhanes$pred_prev_diab[nhanes$racecat=='Mexican-American']), svymean(~pred_prev_diab, subset(surv_all2, age >= 18 & !insur_stability=='Missing' & racecat=='Mexican-American'))),
                            c(mean(nhanes$pred_prev_diab[nhanes$racecat=='Other Hispanic']), svymean(~pred_prev_diab, subset(surv_all2, age >= 18 & !insur_stability=='Missing' & racecat=='Other Hispanic'))),
                            c(mean(nhanes$pred_prev_diab[nhanes$racecat=='Other/Multi-Racial including NHA (pre-2011)']), svymean(~pred_prev_diab, subset(surv_all2, age >= 18 & !insur_stability=='Missing' & racecat=='Other/Multi-Racial including NHA (pre-2011)'))),
                            c(mean(nhanes$pred_prev_diab[nhanes$racecat=='Other/Multi-Racial (post-2011)']), svymean(~pred_prev_diab, subset(surv_all2, age >= 18 & !insur_stability=='Missing' & racecat=='Other/Multi-Racial (post-2011)'))))
colnames(pred_prev_byrace) <- c('mean_preddiab','svymean_preddiab')
rownames(pred_prev_byrace) <- c('Overall',rownames(table(nhanes$racecat, nhanes$female)))

?svystandardize
nhanes_std <- svystandardize(surv_all2, by=~age + female + survyr, over=~racecat, population=c(***))
svyby(~pred_prev_diab, ~racecat, svymean, surv_all2)


#####CONTIN EDUC, ALL DISEASE FORESTS#####
mod_diab_educcont_all <- svyglm(design = surv_all, diab ~ age + female + survyr + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
summary(mod_diab_educcont_all)
nobs(mod_diab_educcont_all)
exp(mod_diab_educcont_all$coefficients)
mod_diab_educcont_all_raceadj <- svyglm(design = surv_all, diab ~ age + female + survyr + smoke_status + insur_stability + educ_num + racecat, family=binomial(link=logit))
mod_diab_educcont_nhw <- svyglm(design = surv_nhw, diab ~ age + female + survyr + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_diab_educcont_nhb <- svyglm(design = surv_nhb, diab ~ age + female + survyr + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
summary(mod_diab_educcont_nhb)
exp(mod_diab_educcont_nhb$coefficients)
mod_diab_educcont_nha <- svyglm(design = surv_nha, diab ~ age + female + survyr + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_diab_educcont_mex <- svyglm(design = surv_mex, diab ~ age + female + survyr + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_diab_educcont_his <- svyglm(design = surv_his, diab ~ age + female + survyr + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_diab_educcont_oth <- svyglm(design = surv_oth, diab ~ age + female + survyr + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_diab_educcont_oth11 <- svyglm(design = surv_oth11, diab ~ age + female + survyr + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
a <- cbind(exp(mod_diab_educcont_all$coefficients),exp(mod_diab_educcont_all_raceadj$coefficients),exp(mod_diab_educcont_nhw$coefficients),
           exp(mod_diab_educcont_nhb$coefficients),exp(mod_diab_educcont_mex$coefficients),exp(mod_diab_educcont_his$coefficients),
           exp(mod_diab_educcont_nha$coefficients),exp(mod_diab_educcont_oth$coefficients),exp(mod_diab_educcont_oth11$coefficients))

mod_diab_educcont_all_bmi <- svyglm(design = surv_all, diab ~ age + female + survyr + smoke_status + insur_stability + educ_num + bmi, family=binomial(link=logit))
summary(mod_diab_educcont_all_bmi)
exp(mod_diab_educcont_all_bmi$coefficients)
mod_diab_educcont_all_raceadj_bmi <- svyglm(design = surv_all, diab ~ age + female + survyr + smoke_status + insur_stability + educ_num + bmi + racecat, family=binomial(link=logit))
mod_diab_educcont_nhw_bmi <- svyglm(design = surv_nhw, diab ~ age + female + survyr + smoke_status + insur_stability + educ_num + bmi, family=binomial(link=logit))
mod_diab_educcont_nhb_bmi <- svyglm(design = surv_nhb, diab ~ age + female + survyr + smoke_status + insur_stability + educ_num + bmi, family=binomial(link=logit))
summary(mod_diab_educcont_nhb_bmi)
exp(mod_diab_educcont_nhb_bmi$coefficients)
mod_diab_educcont_nha_bmi <- svyglm(design = surv_nha, diab ~ age + female + survyr + smoke_status + insur_stability + educ_num + bmi, family=binomial(link=logit))
mod_diab_educcont_mex_bmi <- svyglm(design = surv_mex, diab ~ age + female + survyr + smoke_status + insur_stability + educ_num + bmi, family=binomial(link=logit))
mod_diab_educcont_his_bmi <- svyglm(design = surv_his, diab ~ age + female + survyr + smoke_status + insur_stability + educ_num + bmi + bmi, family=binomial(link=logit))
mod_diab_educcont_oth_bmi <- svyglm(design = surv_oth, diab ~ age + female + survyr + smoke_status + insur_stability + educ_num + bmi, family=binomial(link=logit))
mod_diab_educcont_oth11_bmi <- svyglm(design = surv_oth11, diab ~ age + female + survyr + smoke_status + insur_stability + educ_num + bmi, family=binomial(link=logit))
b <- cbind(exp(mod_diab_educcont_all_bmi$coefficients),exp(mod_diab_educcont_all_raceadj_bmi$coefficients),exp(mod_diab_educcont_nhw_bmi$coefficients),
           exp(mod_diab_educcont_nhb_bmi$coefficients),exp(mod_diab_educcont_mex_bmi$coefficients),exp(mod_diab_educcont_his_bmi$coefficients),
           exp(mod_diab_educcont_nha_bmi$coefficients),exp(mod_diab_educcont_oth_bmi$coefficients),exp(mod_diab_educcont_oth11_bmi$coefficients))


mod_obes_educcont_all <- svyglm(design = surv_all, obese ~ age + female + survyr + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_obes_educcont_all_raceadj <- svyglm(design = surv_all, obese ~ age + female + survyr + smoke_status + insur_stability + educ_num + racecat, family=binomial(link=logit))
mod_obes_educcont_nhw <- svyglm(design = surv_nhw, obese ~ age + female + survyr + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_obes_educcont_nhb <- svyglm(design = surv_nhb, obese ~ age + female + survyr + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_obes_educcont_nha <- svyglm(design = surv_nha, obese ~ age + female + survyr + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_obes_educcont_mex <- svyglm(design = surv_mex, obese ~ age + female + survyr + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_obes_educcont_his <- svyglm(design = surv_his, obese ~ age + female + survyr + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_obes_educcont_oth <- svyglm(design = surv_oth, obese ~ age + female + survyr + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_obes_educcont_oth11 <- svyglm(design = surv_oth11, obese ~ age + female + survyr + smoke_status + insur_stability + educ_num, family=binomial(link=logit))

mod_htn_educcont_all <- svyglm(design = surv_all, htn ~ age + female + survyr + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_htn_educcont_all_raceadj <- svyglm(design = surv_all, htn ~ age + female + survyr + smoke_status + insur_stability + educ_num + racecat, family=binomial(link=logit))
mod_htn_educcont_nhw <- svyglm(design = surv_nhw, htn ~ age + female + survyr + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_htn_educcont_nhb <- svyglm(design = surv_nhb, htn ~ age + female + survyr + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_htn_educcont_nha <- svyglm(design = surv_nha, htn ~ age + female + survyr + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_htn_educcont_mex <- svyglm(design = surv_mex, htn ~ age + female + survyr + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_htn_educcont_his <- svyglm(design = surv_his, htn ~ age + female + survyr + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_htn_educcont_oth <- svyglm(design = surv_oth, htn ~ age + female + survyr + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_htn_educcont_oth11 <- svyglm(design = surv_oth11, htn ~ age + female + survyr + smoke_status + insur_stability + educ_num, family=binomial(link=logit))

mod_cvd_educcont_all <- svyglm(design = surv_all, cvd ~ age + female + survyr + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_cvd_educcont_all_raceadj <- svyglm(design = surv_all, cvd ~ age + female + survyr + smoke_status + insur_stability + educ_num + racecat, family=binomial(link=logit))
mod_cvd_educcont_nhw <- svyglm(design = surv_nhw, cvd ~ age + female + survyr + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_cvd_educcont_nhb <- svyglm(design = surv_nhb, cvd ~ age + female + survyr + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_cvd_educcont_nha <- svyglm(design = surv_nha, cvd ~ age + female + survyr + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_cvd_educcont_mex <- svyglm(design = surv_mex, cvd ~ age + female + survyr + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_cvd_educcont_his <- svyglm(design = surv_his, cvd ~ age + female + survyr + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_cvd_educcont_oth <- svyglm(design = surv_oth, cvd ~ age + female + survyr + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_cvd_educcont_oth11 <- svyglm(design = surv_oth11, cvd ~ age + female + survyr + smoke_status + insur_stability + educ_num, family=binomial(link=logit))

mod_ckd_educcont_all <- svyglm(design = surv_all, ckd ~ age + female + survyr + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_ckd_educcont_all_raceadj <- svyglm(design = surv_all, ckd ~ age + female + survyr + smoke_status + insur_stability + educ_num + racecat, family=binomial(link=logit))
mod_ckd_educcont_nhw <- svyglm(design = surv_nhw, ckd ~ age + female + survyr + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_ckd_educcont_nhb <- svyglm(design = surv_nhb, ckd ~ age + female + survyr + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_ckd_educcont_nha <- svyglm(design = surv_nha, ckd ~ age + female + survyr + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_ckd_educcont_mex <- svyglm(design = surv_mex, ckd ~ age + female + survyr + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_ckd_educcont_his <- svyglm(design = surv_his, ckd ~ age + female + survyr + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_ckd_educcont_oth <- svyglm(design = surv_oth, ckd ~ age + female + survyr + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_ckd_educcont_oth11 <- svyglm(design = surv_oth11, ckd ~ age + female + survyr + smoke_status + insur_stability + educ_num, family=binomial(link=logit))

mod_copd_educcont_all <- svyglm(design = surv_all, copd ~ age + female + survyr + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_copd_educcont_all_raceadj <- svyglm(design = surv_all, copd ~ age + female + survyr + smoke_status + insur_stability + educ_num + racecat, family=binomial(link=logit))
mod_copd_educcont_nhw <- svyglm(design = surv_nhw, copd ~ age + female + survyr + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_copd_educcont_nhb <- svyglm(design = surv_nhb, copd ~ age + female + survyr + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_copd_educcont_nha <- svyglm(design = surv_nha, copd ~ age + female + survyr + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_copd_educcont_mex <- svyglm(design = surv_mex, copd ~ age + female + survyr + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_copd_educcont_his <- svyglm(design = surv_his, copd ~ age + female + survyr + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_copd_educcont_oth <- svyglm(design = surv_oth, copd ~ age + female + survyr + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_copd_educcont_oth11 <- svyglm(design = surv_oth11, copd ~ age + female + survyr + smoke_status + insur_stability + educ_num, family=binomial(link=logit))

mod_mdd_educcont_all <- svyglm(design = surv_all, mdd ~ age + female + survyr + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_mdd_educcont_all_raceadj <- svyglm(design = surv_all, mdd ~ age + female + survyr + smoke_status + insur_stability + educ_num + racecat, family=binomial(link=logit))
mod_mdd_educcont_nhw <- svyglm(design = surv_nhw, mdd ~ age + female + survyr + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_mdd_educcont_nhb <- svyglm(design = surv_nhb, mdd ~ age + female + survyr + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_mdd_educcont_nha <- svyglm(design = surv_nha, mdd ~ age + female + survyr + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_mdd_educcont_mex <- svyglm(design = surv_mex, mdd ~ age + female + survyr + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_mdd_educcont_his <- svyglm(design = surv_his, mdd ~ age + female + survyr + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_mdd_educcont_oth <- svyglm(design = surv_oth, mdd ~ age + female + survyr + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_mdd_educcont_oth11 <- svyglm(design = surv_oth11, mdd ~ age + female + survyr + smoke_status + insur_stability + educ_num, family=binomial(link=logit))

mod_diabob_educcont_allrace <- data.frame(exp(mod_diab_educcont_all$coefficients),exp(confint(mod_diab_educcont_all)),coef(summary(mod_diab_educcont_all))[,"Pr(>|t|)"],
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
                                         exp(mod_obes_educcont_oth11$coefficients),exp(confint(mod_obes_educcont_oth11)),coef(summary(mod_obes_educcont_oth11))[,"Pr(>|t|)"],stringsAsFactors = F)
write.csv(mod_diabob_educcont_allrace, 'C:/Users/sarac/OneDrive/Documents/Research/Area v Individual SES/NHANES_SES and AoU_SES/nhanes_ses_educ_diabob_models.csv',row.names=TRUE)

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
tiff(file='C:/Users/sarac/OneDrive/Documents/Research/Area v Individual SES/NHANES_SES and AoU_SES/nhanes_forest_indiv_educcont_stratandnonstrat_alldx.tiff', width=8, height=5, units='in', res=300)
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
           #legend=c("All","All, Race-adjusted",rownames(table(nhanes$racecat))),#c("All","All-Race-adjusted","NHW","NHB","NHA","Mexican-American","Other Hispanic","Other/Multiracial"),
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
tiff(file='C:/Users/sarac/OneDrive/Documents/Research/Area v Individual SES/NHANES_SES and AoU_SES/Fig2A_nhanes_forest_indiv_educcont_stratandnonstrat.tiff', width=8, height=5, units='in', res=300)
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
           clip=c(0.75,1.1), xlim=c(0.75,1.1), xlog=TRUE, grid=structure(c(0.75,0.8,0.9,1.0,1.1)),
           xticks=c(-0.287682072451786,-0.223143551314213,-0.105360515657828,0,0.0953101798043265))
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
tiff(file='C:/Users/sarac/OneDrive/Documents/Research/Area v Individual SES/NHANES_SES and AoU_SES/nhanes_forest_indiv_educcont_stratandnonstrat_t2dbmiadj.tiff', width=8, height=5, units='in', res=300)
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


#####sensitivity analyses - quasibinomial and modified poisson and poisson#####
#educ cont
mod_diab_educcont_all_qb <- svyglm(design = surv_all,  diab ~ age + female + smoke_status + insur_stability + educ_num, family=quasibinomial)
mod_diab_educcont_raceadj_qb <- svyglm(design = surv_all,  diab ~ age + female + smoke_status + insur_stability + educ_num + racecat, family=quasibinomial)
mod_diab_educcont_nhw_qb <- svyglm(design = surv_nhw,  diab ~ age + female + smoke_status + insur_stability + educ_num, family=quasibinomial)
mod_diab_educcont_nhb_qb <- svyglm(design = surv_nhb,  diab ~ age + female + smoke_status + insur_stability + educ_num, family=quasibinomial)
mod_diab_educcont_mex_qb <- svyglm(design = surv_mex,  diab ~ age + female + smoke_status + insur_stability + educ_num, family=quasibinomial)
mod_diab_educcont_his_qb <- svyglm(design = surv_his,  diab ~ age + female + smoke_status + insur_stability + educ_num, family=quasibinomial)
mod_diab_educcont_nha_qb <- svyglm(design = surv_nha, diab ~ age + female + smoke_status + insur_stability + educ_num, family=quasibinomial)
mod_diab_educcont_oth_qb <- svyglm(design = surv_oth, diab ~ age + female + smoke_status + insur_stability + educ_num, family=quasibinomial)
mod_diab_educcont_oth11_qb <- svyglm(design = surv_oth11,diab ~ age + female + smoke_status + insur_stability + educ_num, family=quasibinomial)
nhanes_educcont_diab_qb_result <- data.frame(exp(mod_diab_educcont_all_qb$coefficients),exp(confint(mod_diab_educcont_all_qb)),coef(summary(mod_diab_educcont_all_qb))[,"Pr(>|t|)"],
                                          exp(mod_diab_educcont_raceadj_qb$coefficients)[1:10],exp(confint(mod_diab_educcont_raceadj_qb))[1:10,1:2],coef(summary(mod_diab_educcont_raceadj_qb))[,"Pr(>|t|)"][1:10],
                                          exp(mod_diab_educcont_nhw_qb$coefficients),exp(confint(mod_diab_educcont_nhw_qb)),coef(summary(mod_diab_educcont_nhw_qb))[,"Pr(>|t|)"],
                                          exp(mod_diab_educcont_nhb_qb$coefficients),exp(confint(mod_diab_educcont_nhb_qb)),coef(summary(mod_diab_educcont_nhb_qb))[,"Pr(>|t|)"],
                                          exp(mod_diab_educcont_mex_qb$coefficients),exp(confint(mod_diab_educcont_mex_qb)),coef(summary(mod_diab_educcont_mex_qb))[,"Pr(>|t|)"],
                                          exp(mod_diab_educcont_his_qb$coefficients),exp(confint(mod_diab_educcont_his_qb)),coef(summary(mod_diab_educcont_his_qb))[,"Pr(>|t|)"],
                                          exp(mod_diab_educcont_nha_qb$coefficients),exp(confint(mod_diab_educcont_nha_qb)),coef(summary(mod_diab_educcont_nha_qb))[,"Pr(>|t|)"],
                                          exp(mod_diab_educcont_oth_qb$coefficients),exp(confint(mod_diab_educcont_oth_qb)),coef(summary(mod_diab_educcont_oth_qb))[,"Pr(>|t|)"],
                                          exp(mod_diab_educcont_oth11_qb$coefficients),exp(confint(mod_diab_educcont_oth11_qb)),coef(summary(mod_diab_educcont_oth11_qb))[,"Pr(>|t|)"],stringsAsFactors = FALSE)
nhanes_educcont_diab_qb_result
summary(mod_diab_educcont_all_qb)

mod_ob_educcont_all_qb <- svyglm(design = surv_all,  obese ~ age + female + smoke_status + insur_stability + educ_num, family=quasibinomial)
mod_ob_educcont_raceadj_qb <- svyglm(design = surv_all,  obese ~ age + female + smoke_status + insur_stability + educ_num + racecat, family=quasibinomial)
mod_ob_educcont_nhw_qb <- svyglm(design = surv_nhw,  obese ~ age + female + smoke_status + insur_stability + educ_num, family=quasibinomial)
mod_ob_educcont_nhb_qb <- svyglm(design = surv_nhb,  obese ~ age + female + smoke_status + insur_stability + educ_num, family=quasibinomial)
mod_ob_educcont_mex_qb <- svyglm(design = surv_mex,  obese ~ age + female + smoke_status + insur_stability + educ_num, family=quasibinomial)
mod_ob_educcont_his_qb <- svyglm(design = surv_his,  obese ~ age + female + smoke_status + insur_stability + educ_num, family=quasibinomial)
mod_ob_educcont_nha_qb <- svyglm(design = surv_nha, obese ~ age + female + smoke_status + insur_stability + educ_num, family=quasibinomial)
mod_ob_educcont_oth_qb <- svyglm(design = surv_oth, obese ~ age + female + smoke_status + insur_stability + educ_num, family=quasibinomial)
mod_ob_educcont_oth11_qb <- svyglm(design = surv_oth11,obese ~ age + female + smoke_status + insur_stability + educ_num, family=quasibinomial)
nhanes_educcont_ob_qb_result <- data.frame(exp(mod_ob_educcont_all_qb$coefficients),exp(confint(mod_ob_educcont_all_qb)),coef(summary(mod_ob_educcont_all_qb))[,"Pr(>|t|)"],
                                        exp(mod_ob_educcont_raceadj_qb$coefficients)[1:10],exp(confint(mod_ob_educcont_raceadj_qb))[1:10,1:2],coef(summary(mod_ob_educcont_raceadj_qb))[,"Pr(>|t|)"][1:10],
                                        exp(mod_ob_educcont_nhw_qb$coefficients),exp(confint(mod_ob_educcont_nhw_qb)),coef(summary(mod_ob_educcont_nhw_qb))[,"Pr(>|t|)"],
                                        exp(mod_ob_educcont_nhb_qb$coefficients),exp(confint(mod_ob_educcont_nhb_qb)),coef(summary(mod_ob_educcont_nhb_qb))[,"Pr(>|t|)"],
                                        exp(mod_ob_educcont_mex_qb$coefficients),exp(confint(mod_ob_educcont_mex_qb)),coef(summary(mod_ob_educcont_mex_qb))[,"Pr(>|t|)"],
                                        exp(mod_ob_educcont_his_qb$coefficients),exp(confint(mod_ob_educcont_his_qb)),coef(summary(mod_ob_educcont_his_qb))[,"Pr(>|t|)"],
                                        exp(mod_ob_educcont_nha_qb$coefficients),exp(confint(mod_ob_educcont_nha_qb)),coef(summary(mod_ob_educcont_nha_qb))[,"Pr(>|t|)"],
                                        exp(mod_ob_educcont_oth_qb$coefficients),exp(confint(mod_ob_educcont_oth_qb)),coef(summary(mod_ob_educcont_oth_qb))[,"Pr(>|t|)"],
                                        exp(mod_ob_educcont_oth11_qb$coefficients),exp(confint(mod_ob_educcont_oth11_qb)),coef(summary(mod_ob_educcont_oth11_qb))[,"Pr(>|t|)"],stringsAsFactors = FALSE)
nhanes_educcont_ob_qb_result

nhanes_educcont_diabob_qb_result <- cbind(nhanes_educcont_diab_qb_result,nhanes_educcont_ob_qb_result)
write.csv(nhanes_educcont_diabob_qb_result, 'C:/Users/sarac/OneDrive/Documents/Research/Area v Individual SES/NHANES_SES and AoU_SES/nhanes_ses_educnum_diabob_qb_models.csv',row.names=TRUE)

mod_diab_educcont_all_mp <- svyglm(design = surv_all,  diab ~ age + female + smoke_status + insur_stability + educ_num, family=poisson(link=log))
mod_diab_educcont_all_mp_robust <- as.data.frame(coeftest(mod_diab_educcont_all_mp, vcov=sandwich)[,])
mod_diab_educcont_all_mp_robust_values <- data.frame(exp(mod_diab_educcont_all_mp_robust$Estimate), exp(mod_diab_educcont_all_mp_robust$Estimate - qnorm(1-0.05)/2*mod_diab_educcont_all_mp_robust$'Std. Error'), exp(mod_diab_educcont_all_mp_robust$Estimate + qnorm(1-0.05)/2*mod_diab_educcont_all_mp_robust$'Std. Error'), mod_diab_educcont_all_mp_robust$`Pr(>|z|)`)
colnames(mod_diab_educcont_all_mp_robust_values) <- c('coef','lower CI','upper CI','p')

mod_diab_educcont_raceadj_mp <- svyglm(design = surv_all,  diab ~ age + female + smoke_status + insur_stability + educ_num + racecat, family=poisson(link=log))
mod_diab_educcont_raceadj_mp_robust <- as.data.frame(coeftest(mod_diab_educcont_raceadj_mp, vcov=sandwich)[,])
mod_diab_educcont_nhw_mp <- svyglm(design = surv_nhw,  diab ~ age + female + smoke_status + insur_stability + educ_num, family=poisson(link=log))
mod_diab_educcont_nhw_mp_robust <- as.data.frame(coeftest(mod_diab_educcont_nhw_mp, vcov=sandwich)[,])
mod_diab_educcont_nhb_mp <- svyglm(design = surv_nhb,  diab ~ age + female + smoke_status + insur_stability + educ_num, family=poisson(link=log))
mod_diab_educcont_nhb_mp_robust <- as.data.frame(coeftest(mod_diab_educcont_nhb_mp, vcov=sandwich)[,])
mod_diab_educcont_mex_mp <- svyglm(design = surv_mex,  diab ~ age + female + smoke_status + insur_stability + educ_num, family=poisson(link=log))
mod_diab_educcont_mex_mp_robust <- as.data.frame(coeftest(mod_diab_educcont_mex_mp, vcov=sandwich)[,])
mod_diab_educcont_his_mp <- svyglm(design = surv_his,  diab ~ age + female + smoke_status + insur_stability + educ_num, family=poisson(link=log))
mod_diab_educcont_his_mp_robust <- as.data.frame(coeftest(mod_diab_educcont_his_mp, vcov=sandwich)[,])
mod_diab_educcont_nha_mp <- svyglm(design = surv_nha, diab ~ age + female + smoke_status + insur_stability + educ_num, family=poisson(link=log))
mod_diab_educcont_nha_mp_robust <- as.data.frame(coeftest(mod_diab_educcont_nha_mp, vcov=sandwich)[,])
mod_diab_educcont_oth_mp <- svyglm(design = surv_oth, diab ~ age + female + smoke_status + insur_stability + educ_num, family=poisson(link=log))
mod_diab_educcont_oth_mp_robust <- as.data.frame(coeftest(mod_diab_educcont_oth_mp, vcov=sandwich)[,])
mod_diab_educcont_oth11_mp <- svyglm(design = surv_oth11,diab ~ age + female + smoke_status + insur_stability + educ_num, family=poisson(link=log))
mod_diab_educcont_oth11_mp_robust <- as.data.frame(coeftest(mod_diab_educcont_oth11_mp, vcov=sandwich)[,])

nhanes_educcont_diab_mp_result <- data.frame(exp(mod_diab_educcont_all_mp_robust$Estimate), exp(mod_diab_educcont_all_mp_robust$Estimate - qnorm(1-0.05)/2*mod_diab_educcont_all_mp_robust$'Std. Error'), exp(mod_diab_educcont_all_mp_robust$Estimate + qnorm(1-0.05)/2*mod_diab_educcont_all_mp_robust$'Std. Error'), mod_diab_educcont_all_mp_robust$`Pr(>|z|)`,
                                          exp(mod_diab_educcont_raceadj_mp_robust$Estimate)[1:10], exp(mod_diab_educcont_raceadj_mp_robust$Estimate - qnorm(1-0.05)/2*mod_diab_educcont_raceadj_mp_robust$'Std. Error')[1:10], exp(mod_diab_educcont_raceadj_mp_robust$Estimate + qnorm(1-0.05)/2*mod_diab_educcont_raceadj_mp_robust$'Std. Error')[1:10], mod_diab_educcont_raceadj_mp_robust$`Pr(>|z|)`[1:10],
                                          exp(mod_diab_educcont_nhw_mp_robust$Estimate), exp(mod_diab_educcont_nhw_mp_robust$Estimate - qnorm(1-0.05)/2*mod_diab_educcont_nhw_mp_robust$'Std. Error'), exp(mod_diab_educcont_nhw_mp_robust$Estimate + qnorm(1-0.05)/2*mod_diab_educcont_nhw_mp_robust$'Std. Error'), mod_diab_educcont_nhw_mp_robust$`Pr(>|z|)`,
                                          exp(mod_diab_educcont_nhb_mp_robust$Estimate), exp(mod_diab_educcont_nhb_mp_robust$Estimate - qnorm(1-0.05)/2*mod_diab_educcont_nhb_mp_robust$'Std. Error'), exp(mod_diab_educcont_nhb_mp_robust$Estimate + qnorm(1-0.05)/2*mod_diab_educcont_nhb_mp_robust$'Std. Error'), mod_diab_educcont_nhb_mp_robust$`Pr(>|z|)`,
                                          exp(mod_diab_educcont_mex_mp_robust$Estimate), exp(mod_diab_educcont_mex_mp_robust$Estimate - qnorm(1-0.05)/2*mod_diab_educcont_mex_mp_robust$'Std. Error'), exp(mod_diab_educcont_mex_mp_robust$Estimate + qnorm(1-0.05)/2*mod_diab_educcont_mex_mp_robust$'Std. Error'), mod_diab_educcont_mex_mp_robust$`Pr(>|z|)`,
                                          exp(mod_diab_educcont_his_mp_robust$Estimate), exp(mod_diab_educcont_his_mp_robust$Estimate - qnorm(1-0.05)/2*mod_diab_educcont_his_mp_robust$'Std. Error'), exp(mod_diab_educcont_his_mp_robust$Estimate + qnorm(1-0.05)/2*mod_diab_educcont_his_mp_robust$'Std. Error'), mod_diab_educcont_his_mp_robust$`Pr(>|z|)`,
                                          exp(mod_diab_educcont_nha_mp_robust$Estimate), exp(mod_diab_educcont_nha_mp_robust$Estimate - qnorm(1-0.05)/2*mod_diab_educcont_nha_mp_robust$'Std. Error'), exp(mod_diab_educcont_nha_mp_robust$Estimate + qnorm(1-0.05)/2*mod_diab_educcont_nha_mp_robust$'Std. Error'), mod_diab_educcont_nha_mp_robust$`Pr(>|z|)`,
                                          exp(mod_diab_educcont_oth_mp_robust$Estimate), exp(mod_diab_educcont_oth_mp_robust$Estimate - qnorm(1-0.05)/2*mod_diab_educcont_oth_mp_robust$'Std. Error'), exp(mod_diab_educcont_oth_mp_robust$Estimate + qnorm(1-0.05)/2*mod_diab_educcont_oth_mp_robust$'Std. Error'), mod_diab_educcont_oth_mp_robust$`Pr(>|z|)`,
                                          exp(mod_diab_educcont_oth11_mp_robust$Estimate), exp(mod_diab_educcont_oth11_mp_robust$Estimate - qnorm(1-0.05)/2*mod_diab_educcont_oth11_mp_robust$'Std. Error'), exp(mod_diab_educcont_oth11_mp_robust$Estimate + qnorm(1-0.05)/2*mod_diab_educcont_oth11_mp_robust$'Std. Error'), mod_diab_educcont_oth11_mp_robust$`Pr(>|z|)`, stringsAsFactors=F)
colnames(nhanes_educcont_diab_mp_result) <- c('coef','lower CI','upper CI','p','coef','lower CI','upper CI','p','coef','lower CI','upper CI','p','coef','lower CI','upper CI','p','coef','lower CI','upper CI','p','coef','lower CI','upper CI','p','coef','lower CI','upper CI','p','coef','lower CI','upper CI','p','coef','lower CI','upper CI','p')
nhanes_educcont_diab_mp_result


mod_ob_educcont_all_mp <- svyglm(design = surv_all,  obese ~ age + female + smoke_status + insur_stability + educ_num, family=poisson(link=log))
mod_ob_educcont_all_mp_robust <- as.data.frame(coeftest(mod_ob_educcont_all_mp, vcov=sandwich)[,])
mod_ob_educcont_raceadj_mp <- svyglm(design = surv_all,  obese ~ age + female + smoke_status + insur_stability + educ_num + racecat, family=poisson(link=log))
mod_ob_educcont_raceadj_mp_robust <- as.data.frame(coeftest(mod_ob_educcont_raceadj_mp, vcov=sandwich)[,])
mod_ob_educcont_nhw_mp <- svyglm(design = surv_nhw,  obese ~ age + female + smoke_status + insur_stability + educ_num, family=poisson(link=log))
mod_ob_educcont_nhw_mp_robust <- as.data.frame(coeftest(mod_ob_educcont_nhw_mp, vcov=sandwich)[,])
mod_ob_educcont_nhb_mp <- svyglm(design = surv_nhb,  obese ~ age + female + smoke_status + insur_stability + educ_num, family=poisson(link=log))
mod_ob_educcont_nhb_mp_robust <- as.data.frame(coeftest(mod_ob_educcont_nhb_mp, vcov=sandwich)[,])
mod_ob_educcont_mex_mp <- svyglm(design = surv_mex,  obese ~ age + female + smoke_status + insur_stability + educ_num, family=poisson(link=log))
mod_ob_educcont_mex_mp_robust <- as.data.frame(coeftest(mod_ob_educcont_mex_mp, vcov=sandwich)[,])
mod_ob_educcont_his_mp <- svyglm(design = surv_his,  obese ~ age + female + smoke_status + insur_stability + educ_num, family=poisson(link=log))
mod_ob_educcont_his_mp_robust <- as.data.frame(coeftest(mod_ob_educcont_his_mp, vcov=sandwich)[,])
mod_ob_educcont_nha_mp <- svyglm(design = surv_nha, obese ~ age + female + smoke_status + insur_stability + educ_num, family=poisson(link=log))
mod_ob_educcont_nha_mp_robust <- as.data.frame(coeftest(mod_ob_educcont_nha_mp, vcov=sandwich)[,])
mod_ob_educcont_oth_mp <- svyglm(design = surv_oth, obese ~ age + female + smoke_status + insur_stability + educ_num, family=poisson(link=log))
mod_ob_educcont_oth_mp_robust <- as.data.frame(coeftest(mod_ob_educcont_oth_mp, vcov=sandwich)[,])
mod_ob_educcont_oth11_mp <- svyglm(design = surv_oth11,obese ~ age + female + smoke_status + insur_stability + educ_num, family=poisson(link=log))
mod_ob_educcont_oth11_mp_robust <- as.data.frame(coeftest(mod_ob_educcont_oth11_mp, vcov=sandwich)[,])

nhanes_educcont_ob_mp_result <- data.frame(exp(mod_ob_educcont_all_mp_robust$Estimate), exp(mod_ob_educcont_all_mp_robust$Estimate - qnorm(1-0.05)/2*mod_ob_educcont_all_mp_robust$'Std. Error'), exp(mod_ob_educcont_all_mp_robust$Estimate + qnorm(1-0.05)/2*mod_ob_educcont_all_mp_robust$'Std. Error'), mod_ob_educcont_all_mp_robust$`Pr(>|z|)`,
                                        exp(mod_ob_educcont_raceadj_mp_robust$Estimate)[1:10], exp(mod_ob_educcont_raceadj_mp_robust$Estimate - qnorm(1-0.05)/2*mod_ob_educcont_raceadj_mp_robust$'Std. Error')[1:10], exp(mod_ob_educcont_raceadj_mp_robust$Estimate + qnorm(1-0.05)/2*mod_ob_educcont_raceadj_mp_robust$'Std. Error')[1:10], mod_ob_educcont_raceadj_mp_robust$`Pr(>|z|)`[1:10],
                                        exp(mod_ob_educcont_nhw_mp_robust$Estimate), exp(mod_ob_educcont_nhw_mp_robust$Estimate - qnorm(1-0.05)/2*mod_ob_educcont_nhw_mp_robust$'Std. Error'), exp(mod_ob_educcont_nhw_mp_robust$Estimate + qnorm(1-0.05)/2*mod_ob_educcont_nhw_mp_robust$'Std. Error'), mod_ob_educcont_nhw_mp_robust$`Pr(>|z|)`,
                                        exp(mod_ob_educcont_nhb_mp_robust$Estimate), exp(mod_ob_educcont_nhb_mp_robust$Estimate - qnorm(1-0.05)/2*mod_ob_educcont_nhb_mp_robust$'Std. Error'), exp(mod_ob_educcont_nhb_mp_robust$Estimate + qnorm(1-0.05)/2*mod_ob_educcont_nhb_mp_robust$'Std. Error'), mod_ob_educcont_nhb_mp_robust$`Pr(>|z|)`,
                                        exp(mod_ob_educcont_mex_mp_robust$Estimate), exp(mod_ob_educcont_mex_mp_robust$Estimate - qnorm(1-0.05)/2*mod_ob_educcont_mex_mp_robust$'Std. Error'), exp(mod_ob_educcont_mex_mp_robust$Estimate + qnorm(1-0.05)/2*mod_ob_educcont_mex_mp_robust$'Std. Error'), mod_ob_educcont_mex_mp_robust$`Pr(>|z|)`,
                                        exp(mod_ob_educcont_his_mp_robust$Estimate), exp(mod_ob_educcont_his_mp_robust$Estimate - qnorm(1-0.05)/2*mod_ob_educcont_his_mp_robust$'Std. Error'), exp(mod_ob_educcont_his_mp_robust$Estimate + qnorm(1-0.05)/2*mod_ob_educcont_his_mp_robust$'Std. Error'), mod_ob_educcont_his_mp_robust$`Pr(>|z|)`,
                                        exp(mod_ob_educcont_nha_mp_robust$Estimate), exp(mod_ob_educcont_nha_mp_robust$Estimate - qnorm(1-0.05)/2*mod_ob_educcont_nha_mp_robust$'Std. Error'), exp(mod_ob_educcont_nha_mp_robust$Estimate + qnorm(1-0.05)/2*mod_ob_educcont_nha_mp_robust$'Std. Error'), mod_ob_educcont_nha_mp_robust$`Pr(>|z|)`,
                                        exp(mod_ob_educcont_oth_mp_robust$Estimate), exp(mod_ob_educcont_oth_mp_robust$Estimate - qnorm(1-0.05)/2*mod_ob_educcont_oth_mp_robust$'Std. Error'), exp(mod_ob_educcont_oth_mp_robust$Estimate + qnorm(1-0.05)/2*mod_ob_educcont_oth_mp_robust$'Std. Error'), mod_ob_educcont_oth_mp_robust$`Pr(>|z|)`,
                                        exp(mod_ob_educcont_oth11_mp_robust$Estimate), exp(mod_ob_educcont_oth11_mp_robust$Estimate - qnorm(1-0.05)/2*mod_ob_educcont_oth11_mp_robust$'Std. Error'), exp(mod_ob_educcont_oth11_mp_robust$Estimate + qnorm(1-0.05)/2*mod_ob_educcont_oth11_mp_robust$'Std. Error'), mod_ob_educcont_oth11_mp_robust$`Pr(>|z|)`, stringsAsFactors=F)
colnames(nhanes_educcont_ob_mp_result) <- c('coef','lower CI','upper CI','p','coef','lower CI','upper CI','p','coef','lower CI','upper CI','p','coef','lower CI','upper CI','p','coef','lower CI','upper CI','p','coef','lower CI','upper CI','p','coef','lower CI','upper CI','p','coef','lower CI','upper CI','p','coef','lower CI','upper CI','p')
nhanes_educcont_ob_mp_result

nhanes_educcont_diabob_mp_result <- cbind(nhanes_educcont_diab_mp_result,nhanes_educcont_ob_mp_result)
write.csv(nhanes_educcont_diabob_mp_result, 'C:/Users/sarac/OneDrive/Documents/Research/Area v Individual SES/NHANES_SES and AoU_SES/nhanes_ses_educnum_diabob_mp_models.csv',row.names=TRUE)

mod_diab_educcont_all_pois <- svyglm(design = surv_all,  diab ~ age + female + smoke_status + insur_stability + educ_num, family=poisson(link=log))
mod_diab_educcont_raceadj_pois <- svyglm(design = surv_all,  diab ~ age + female + smoke_status + insur_stability + educ_num + racecat, family=poisson(link=log))
mod_diab_educcont_nhw_pois <- svyglm(design = surv_nhw,  diab ~ age + female + smoke_status + insur_stability + educ_num, family=poisson(link=log))
mod_diab_educcont_nhb_pois <- svyglm(design = surv_nhb,  diab ~ age + female + smoke_status + insur_stability + educ_num, family=poisson(link=log))
mod_diab_educcont_mex_pois <- svyglm(design = surv_mex,  diab ~ age + female + smoke_status + insur_stability + educ_num, family=poisson(link=log))
mod_diab_educcont_his_pois <- svyglm(design = surv_his,  diab ~ age + female + smoke_status + insur_stability + educ_num, family=poisson(link=log))
mod_diab_educcont_nha_pois <- svyglm(design = surv_nha, diab ~ age + female + smoke_status + insur_stability + educ_num, family=poisson(link=log))
mod_diab_educcont_oth_pois <- svyglm(design = surv_oth, diab ~ age + female + smoke_status + insur_stability + educ_num, family=poisson(link=log))
mod_diab_educcont_oth11_pois <- svyglm(design = surv_oth11,diab ~ age + female + smoke_status + insur_stability + educ_num, family=poisson(link=log))
nhanes_educcont_diab_pois_result <- data.frame(exp(mod_diab_educcont_all_pois$coefficients),exp(confint(mod_diab_educcont_all_pois)),coef(summary(mod_diab_educcont_all_pois))[,"Pr(>|t|)"],
                                             exp(mod_diab_educcont_raceadj_pois$coefficients)[1:10],exp(confint(mod_diab_educcont_raceadj_pois))[1:10,1:2],coef(summary(mod_diab_educcont_raceadj_pois))[,"Pr(>|t|)"][1:10],
                                             exp(mod_diab_educcont_nhw_pois$coefficients),exp(confint(mod_diab_educcont_nhw_pois)),coef(summary(mod_diab_educcont_nhw_pois))[,"Pr(>|t|)"],
                                             exp(mod_diab_educcont_nhb_pois$coefficients),exp(confint(mod_diab_educcont_nhb_pois)),coef(summary(mod_diab_educcont_nhb_pois))[,"Pr(>|t|)"],
                                             exp(mod_diab_educcont_mex_pois$coefficients),exp(confint(mod_diab_educcont_mex_pois)),coef(summary(mod_diab_educcont_mex_pois))[,"Pr(>|t|)"],
                                             exp(mod_diab_educcont_his_pois$coefficients),exp(confint(mod_diab_educcont_his_pois)),coef(summary(mod_diab_educcont_his_pois))[,"Pr(>|t|)"],
                                             exp(mod_diab_educcont_nha_pois$coefficients),exp(confint(mod_diab_educcont_nha_pois)),coef(summary(mod_diab_educcont_nha_pois))[,"Pr(>|t|)"],
                                             exp(mod_diab_educcont_oth_pois$coefficients),exp(confint(mod_diab_educcont_oth_pois)),coef(summary(mod_diab_educcont_oth_pois))[,"Pr(>|t|)"],
                                             exp(mod_diab_educcont_oth11_pois$coefficients),exp(confint(mod_diab_educcont_oth11_pois)),coef(summary(mod_diab_educcont_oth11_pois))[,"Pr(>|t|)"],stringsAsFactors = FALSE)
nhanes_educcont_diab_pois_result
summary(mod_diab_educcont_all_pois)

mod_ob_educcont_all_pois <- svyglm(design = surv_all,  obese ~ age + female + smoke_status + insur_stability + educ_num, family=poisson(link=log))
mod_ob_educcont_raceadj_pois <- svyglm(design = surv_all,  obese ~ age + female + smoke_status + insur_stability + educ_num + racecat, family=poisson(link=log))
mod_ob_educcont_nhw_pois <- svyglm(design = surv_nhw,  obese ~ age + female + smoke_status + insur_stability + educ_num, family=poisson(link=log))
mod_ob_educcont_nhb_pois <- svyglm(design = surv_nhb,  obese ~ age + female + smoke_status + insur_stability + educ_num, family=poisson(link=log))
mod_ob_educcont_mex_pois <- svyglm(design = surv_mex,  obese ~ age + female + smoke_status + insur_stability + educ_num, family=poisson(link=log))
mod_ob_educcont_his_pois <- svyglm(design = surv_his,  obese ~ age + female + smoke_status + insur_stability + educ_num, family=poisson(link=log))
mod_ob_educcont_nha_pois <- svyglm(design = surv_nha, obese ~ age + female + smoke_status + insur_stability + educ_num, family=poisson(link=log))
mod_ob_educcont_oth_pois <- svyglm(design = surv_oth, obese ~ age + female + smoke_status + insur_stability + educ_num, family=poisson(link=log))
mod_ob_educcont_oth11_pois <- svyglm(design = surv_oth11,obese ~ age + female + smoke_status + insur_stability + educ_num, family=poisson(link=log))
nhanes_educcont_ob_pois_result <- data.frame(exp(mod_ob_educcont_all_pois$coefficients),exp(confint(mod_ob_educcont_all_pois)),coef(summary(mod_ob_educcont_all_pois))[,"Pr(>|t|)"],
                                           exp(mod_ob_educcont_raceadj_pois$coefficients)[1:10],exp(confint(mod_ob_educcont_raceadj_pois))[1:10,1:2],coef(summary(mod_ob_educcont_raceadj_pois))[,"Pr(>|t|)"][1:10],
                                           exp(mod_ob_educcont_nhw_pois$coefficients),exp(confint(mod_ob_educcont_nhw_pois)),coef(summary(mod_ob_educcont_nhw_pois))[,"Pr(>|t|)"],
                                           exp(mod_ob_educcont_nhb_pois$coefficients),exp(confint(mod_ob_educcont_nhb_pois)),coef(summary(mod_ob_educcont_nhb_pois))[,"Pr(>|t|)"],
                                           exp(mod_ob_educcont_mex_pois$coefficients),exp(confint(mod_ob_educcont_mex_pois)),coef(summary(mod_ob_educcont_mex_pois))[,"Pr(>|t|)"],
                                           exp(mod_ob_educcont_his_pois$coefficients),exp(confint(mod_ob_educcont_his_pois)),coef(summary(mod_ob_educcont_his_pois))[,"Pr(>|t|)"],
                                           exp(mod_ob_educcont_nha_pois$coefficients),exp(confint(mod_ob_educcont_nha_pois)),coef(summary(mod_ob_educcont_nha_pois))[,"Pr(>|t|)"],
                                           exp(mod_ob_educcont_oth_pois$coefficients),exp(confint(mod_ob_educcont_oth_pois)),coef(summary(mod_ob_educcont_oth_pois))[,"Pr(>|t|)"],
                                           exp(mod_ob_educcont_oth11_pois$coefficients),exp(confint(mod_ob_educcont_oth11_pois)),coef(summary(mod_ob_educcont_oth11_pois))[,"Pr(>|t|)"],stringsAsFactors = FALSE)
nhanes_educcont_ob_pois_result

nhanes_educcont_diabob_pois_result <- cbind(nhanes_educcont_diab_pois_result,nhanes_educcont_ob_pois_result)
write.csv(nhanes_educcont_diabob_pois_result, 'C:/Users/sarac/OneDrive/Documents/Research/Area v Individual SES/NHANES_SES and AoU_SES/nhanes_ses_educnum_diabob_pois_models.csv',row.names=TRUE)

#inc cont, inc_to_pov
mod_diab_incnum_all_qb <- svyglm(design = surv_all,  diab ~ age + female + smoke_status + insur_stability + inc_to_pov, family=quasibinomial)
mod_diab_incnum_raceadj_qb <- svyglm(design = surv_all,  diab ~ age + female + smoke_status + insur_stability + inc_to_pov + racecat, family=quasibinomial)
mod_diab_incnum_nhw_qb <- svyglm(design = surv_nhw,  diab ~ age + female + smoke_status + insur_stability + inc_to_pov, family=quasibinomial)
mod_diab_incnum_nhb_qb <- svyglm(design = surv_nhb,  diab ~ age + female + smoke_status + insur_stability + inc_to_pov, family=quasibinomial)
mod_diab_incnum_his_qb <- svyglm(design = surv_mex,  diab ~ age + female + smoke_status + insur_stability + inc_to_pov, family=quasibinomial)
mod_diab_incnum_nha_qb <- svyglm(design = surv_his,  diab ~ age + female + smoke_status + insur_stability + inc_to_pov, family=quasibinomial)
mod_diab_incnum_multi_qb <- svyglm(design = surv_nha, diab ~ age + female + smoke_status + insur_stability + inc_to_pov, family=quasibinomial)
mod_diab_incnum_oth_qb <- svyglm(design = surv_oth, diab ~ age + female + smoke_status + insur_stability + inc_to_pov, family=quasibinomial)
mod_diab_incnum_oth11_qb <- svyglm(design = surv_oth11,diab ~ age + female + smoke_status + insur_stability + inc_to_pov, family=quasibinomial)
nhanes_incnum_diab_qb_result <- data.frame(exp(mod_diab_incnum_all_qb$coefficients),exp(confint(mod_diab_incnum_all_qb)),coef(summary(mod_diab_incnum_all_qb))[,"Pr(>|t|)"],
                                        exp(mod_diab_incnum_raceadj_qb$coefficients)[1:10],exp(confint(mod_diab_incnum_raceadj_qb))[1:10,1:2],coef(summary(mod_diab_incnum_raceadj_qb))[,"Pr(>|t|)"][1:10],
                                        exp(mod_diab_incnum_nhw_qb$coefficients),exp(confint(mod_diab_incnum_nhw_qb)),coef(summary(mod_diab_incnum_nhw_qb))[,"Pr(>|t|)"],
                                        exp(mod_diab_incnum_nhb_qb$coefficients),exp(confint(mod_diab_incnum_nhb_qb)),coef(summary(mod_diab_incnum_nhb_qb))[,"Pr(>|t|)"],
                                        exp(mod_diab_incnum_his_qb$coefficients),exp(confint(mod_diab_incnum_his_qb)),coef(summary(mod_diab_incnum_his_qb))[,"Pr(>|t|)"],
                                        exp(mod_diab_incnum_nha_qb$coefficients),exp(confint(mod_diab_incnum_nha_qb)),coef(summary(mod_diab_incnum_nha_qb))[,"Pr(>|t|)"],
                                        exp(mod_diab_incnum_multi_qb$coefficients),exp(confint(mod_diab_incnum_multi_qb)),coef(summary(mod_diab_incnum_multi_qb))[,"Pr(>|t|)"],
                                        exp(mod_diab_incnum_oth_qb$coefficients),exp(confint(mod_diab_incnum_oth_qb)),coef(summary(mod_diab_incnum_oth_qb))[,"Pr(>|t|)"],
                                        exp(mod_diab_incnum_oth11_qb$coefficients),exp(confint(mod_diab_incnum_oth11_qb)),coef(summary(mod_diab_incnum_oth11_qb))[,"Pr(>|t|)"],stringsAsFactors = FALSE)
nhanes_incnum_diab_qb_result
summary(mod_diab_incnum_all_qb)

mod_ob_incnum_all_qb <- svyglm(design = surv_all,  obese ~ age + female + smoke_status + insur_stability + inc_to_pov, family=quasibinomial)
mod_ob_incnum_raceadj_qb <- svyglm(design = surv_all,  obese ~ age + female + smoke_status + insur_stability + inc_to_pov + racecat, family=quasibinomial)
mod_ob_incnum_nhw_qb <- svyglm(design = surv_nhw,  obese ~ age + female + smoke_status + insur_stability + inc_to_pov, family=quasibinomial)
mod_ob_incnum_nhb_qb <- svyglm(design = surv_nhb,  obese ~ age + female + smoke_status + insur_stability + inc_to_pov, family=quasibinomial)
mod_ob_incnum_his_qb <- svyglm(design = surv_mex,  obese ~ age + female + smoke_status + insur_stability + inc_to_pov, family=quasibinomial)
mod_ob_incnum_nha_qb <- svyglm(design = surv_his,  obese ~ age + female + smoke_status + insur_stability + inc_to_pov, family=quasibinomial)
mod_ob_incnum_multi_qb <- svyglm(design = surv_nha, obese ~ age + female + smoke_status + insur_stability + inc_to_pov, family=quasibinomial)
mod_ob_incnum_oth_qb <- svyglm(design = surv_oth, obese ~ age + female + smoke_status + insur_stability + inc_to_pov, family=quasibinomial)
mod_ob_incnum_oth11_qb <- svyglm(design = surv_oth11,obese ~ age + female + smoke_status + insur_stability + inc_to_pov, family=quasibinomial)
nhanes_incnum_ob_qb_result <- data.frame(exp(mod_ob_incnum_all_qb$coefficients),exp(confint(mod_ob_incnum_all_qb)),coef(summary(mod_ob_incnum_all_qb))[,"Pr(>|t|)"],
                                      exp(mod_ob_incnum_raceadj_qb$coefficients)[1:10],exp(confint(mod_ob_incnum_raceadj_qb))[1:10,1:2],coef(summary(mod_ob_incnum_raceadj_qb))[,"Pr(>|t|)"][1:10],
                                      exp(mod_ob_incnum_nhw_qb$coefficients),exp(confint(mod_ob_incnum_nhw_qb)),coef(summary(mod_ob_incnum_nhw_qb))[,"Pr(>|t|)"],
                                      exp(mod_ob_incnum_nhb_qb$coefficients),exp(confint(mod_ob_incnum_nhb_qb)),coef(summary(mod_ob_incnum_nhb_qb))[,"Pr(>|t|)"],
                                      exp(mod_ob_incnum_his_qb$coefficients),exp(confint(mod_ob_incnum_his_qb)),coef(summary(mod_ob_incnum_his_qb))[,"Pr(>|t|)"],
                                      exp(mod_ob_incnum_nha_qb$coefficients),exp(confint(mod_ob_incnum_nha_qb)),coef(summary(mod_ob_incnum_nha_qb))[,"Pr(>|t|)"],
                                      exp(mod_ob_incnum_multi_qb$coefficients),exp(confint(mod_ob_incnum_multi_qb)),coef(summary(mod_ob_incnum_multi_qb))[,"Pr(>|t|)"],
                                      exp(mod_ob_incnum_oth_qb$coefficients),exp(confint(mod_ob_incnum_oth_qb)),coef(summary(mod_ob_incnum_oth_qb))[,"Pr(>|t|)"],
                                      exp(mod_ob_incnum_oth11_qb$coefficients),exp(confint(mod_ob_incnum_oth11_qb)),coef(summary(mod_ob_incnum_oth11_qb))[,"Pr(>|t|)"],stringsAsFactors = FALSE)
nhanes_incnum_ob_qb_result

nhanes_incnum_diabob_qb_result <- cbind(nhanes_incnum_diab_qb_result,nhanes_incnum_ob_qb_result)
write.csv(nhanes_incnum_diabob_qb_result, 'C:/Users/sarac/OneDrive/Documents/Research/Area v Individual SES/NHANES_SES and AoU_SES/nhanes_ses_incnum_diabob_qb_models.csv',row.names=TRUE)

mod_diab_incnum_all_mp <- svyglm(design = surv_all,  diab ~ age + female + smoke_status + insur_stability + inc_to_pov, family=poisson(link=log))
summary(mod_diab_incnum_all_mp)
mod_diab_incnum_all_mp_robust <- as.data.frame(coeftest(mod_diab_incnum_all_mp, vcov=sandwich)[,])
mod_diab_incnum_all_mp_robust_values <- data.frame(exp(mod_diab_incnum_all_mp_robust$Estimate), exp(mod_diab_incnum_all_mp_robust$Estimate - qnorm(1-0.05)/2*mod_diab_incnum_all_mp_robust$'Std. Error'), exp(mod_diab_incnum_all_mp_robust$Estimate + qnorm(1-0.05)/2*mod_diab_incnum_all_mp_robust$'Std. Error'), mod_diab_incnum_all_mp_robust$`Pr(>|z|)`)
colnames(mod_diab_incnum_all_mp_robust_values) <- c('coef','lower CI','upper CI','p')

mod_diab_incnum_raceadj_mp <- svyglm(design = surv_all,  diab ~ age + female + smoke_status + insur_stability + inc_to_pov + racecat, family=poisson(link=log))
mod_diab_incnum_raceadj_mp_robust <- as.data.frame(coeftest(mod_diab_incnum_raceadj_mp, vcov=sandwich)[,])
mod_diab_incnum_nhw_mp <- svyglm(design = surv_nhw,  diab ~ age + female + smoke_status + insur_stability + inc_to_pov, family=poisson(link=log))
mod_diab_incnum_nhw_mp_robust <- as.data.frame(coeftest(mod_diab_incnum_nhw_mp, vcov=sandwich)[,])
mod_diab_incnum_nhb_mp <- svyglm(design = surv_nhb,  diab ~ age + female + smoke_status + insur_stability + inc_to_pov, family=poisson(link=log))
mod_diab_incnum_nhb_mp_robust <- as.data.frame(coeftest(mod_diab_incnum_nhb_mp, vcov=sandwich)[,])
mod_diab_incnum_his_mp <- svyglm(design = surv_mex,  diab ~ age + female + smoke_status + insur_stability + inc_to_pov, family=poisson(link=log))
mod_diab_incnum_his_mp_robust <- as.data.frame(coeftest(mod_diab_incnum_his_mp, vcov=sandwich)[,])
mod_diab_incnum_nha_mp <- svyglm(design = surv_his,  diab ~ age + female + smoke_status + insur_stability + inc_to_pov, family=poisson(link=log))
mod_diab_incnum_nha_mp_robust <- as.data.frame(coeftest(mod_diab_incnum_nha_mp, vcov=sandwich)[,])
mod_diab_incnum_multi_mp <- svyglm(design = surv_nha, diab ~ age + female + smoke_status + insur_stability + inc_to_pov, family=poisson(link=log))
mod_diab_incnum_multi_mp_robust <- as.data.frame(coeftest(mod_diab_incnum_multi_mp, vcov=sandwich)[,])
mod_diab_incnum_oth_mp <- svyglm(design = surv_oth, diab ~ age + female + smoke_status + insur_stability + inc_to_pov, family=poisson(link=log))
mod_diab_incnum_oth_mp_robust <- as.data.frame(coeftest(mod_diab_incnum_oth_mp, vcov=sandwich)[,])
mod_diab_incnum_oth11_mp <- svyglm(design = surv_oth11,diab ~ age + female + smoke_status + insur_stability + inc_to_pov, family=poisson(link=log))
mod_diab_incnum_oth11_mp_robust <- as.data.frame(coeftest(mod_diab_incnum_oth11_mp, vcov=sandwich)[,])

nhanes_incnum_diab_mp_result <- data.frame(exp(mod_diab_incnum_all_mp_robust$Estimate), exp(mod_diab_incnum_all_mp_robust$Estimate - qnorm(1-0.05)/2*mod_diab_incnum_all_mp_robust$'Std. Error'), exp(mod_diab_incnum_all_mp_robust$Estimate + qnorm(1-0.05)/2*mod_diab_incnum_all_mp_robust$'Std. Error'), mod_diab_incnum_all_mp_robust$`Pr(>|z|)`,
                                        exp(mod_diab_incnum_raceadj_mp_robust$Estimate)[1:10], exp(mod_diab_incnum_raceadj_mp_robust$Estimate - qnorm(1-0.05)/2*mod_diab_incnum_raceadj_mp_robust$'Std. Error')[1:10], exp(mod_diab_incnum_raceadj_mp_robust$Estimate + qnorm(1-0.05)/2*mod_diab_incnum_raceadj_mp_robust$'Std. Error')[1:10], mod_diab_incnum_raceadj_mp_robust$`Pr(>|z|)`[1:10],
                                        exp(mod_diab_incnum_nhw_mp_robust$Estimate), exp(mod_diab_incnum_nhw_mp_robust$Estimate - qnorm(1-0.05)/2*mod_diab_incnum_nhw_mp_robust$'Std. Error'), exp(mod_diab_incnum_nhw_mp_robust$Estimate + qnorm(1-0.05)/2*mod_diab_incnum_nhw_mp_robust$'Std. Error'), mod_diab_incnum_nhw_mp_robust$`Pr(>|z|)`,
                                        exp(mod_diab_incnum_nhb_mp_robust$Estimate), exp(mod_diab_incnum_nhb_mp_robust$Estimate - qnorm(1-0.05)/2*mod_diab_incnum_nhb_mp_robust$'Std. Error'), exp(mod_diab_incnum_nhb_mp_robust$Estimate + qnorm(1-0.05)/2*mod_diab_incnum_nhb_mp_robust$'Std. Error'), mod_diab_incnum_nhb_mp_robust$`Pr(>|z|)`,
                                        exp(mod_diab_incnum_his_mp_robust$Estimate), exp(mod_diab_incnum_his_mp_robust$Estimate - qnorm(1-0.05)/2*mod_diab_incnum_his_mp_robust$'Std. Error'), exp(mod_diab_incnum_his_mp_robust$Estimate + qnorm(1-0.05)/2*mod_diab_incnum_his_mp_robust$'Std. Error'), mod_diab_incnum_his_mp_robust$`Pr(>|z|)`,
                                        exp(mod_diab_incnum_nha_mp_robust$Estimate), exp(mod_diab_incnum_nha_mp_robust$Estimate - qnorm(1-0.05)/2*mod_diab_incnum_nha_mp_robust$'Std. Error'), exp(mod_diab_incnum_nha_mp_robust$Estimate + qnorm(1-0.05)/2*mod_diab_incnum_nha_mp_robust$'Std. Error'), mod_diab_incnum_nha_mp_robust$`Pr(>|z|)`,
                                        exp(mod_diab_incnum_multi_mp_robust$Estimate), exp(mod_diab_incnum_multi_mp_robust$Estimate - qnorm(1-0.05)/2*mod_diab_incnum_multi_mp_robust$'Std. Error'), exp(mod_diab_incnum_multi_mp_robust$Estimate + qnorm(1-0.05)/2*mod_diab_incnum_multi_mp_robust$'Std. Error'), mod_diab_incnum_multi_mp_robust$`Pr(>|z|)`,
                                        exp(mod_diab_incnum_oth_mp_robust$Estimate), exp(mod_diab_incnum_oth_mp_robust$Estimate - qnorm(1-0.05)/2*mod_diab_incnum_oth_mp_robust$'Std. Error'), exp(mod_diab_incnum_oth_mp_robust$Estimate + qnorm(1-0.05)/2*mod_diab_incnum_oth_mp_robust$'Std. Error'), mod_diab_incnum_oth_mp_robust$`Pr(>|z|)`,
                                        exp(mod_diab_incnum_oth11_mp_robust$Estimate), exp(mod_diab_incnum_oth11_mp_robust$Estimate - qnorm(1-0.05)/2*mod_diab_incnum_oth11_mp_robust$'Std. Error'), exp(mod_diab_incnum_oth11_mp_robust$Estimate + qnorm(1-0.05)/2*mod_diab_incnum_oth11_mp_robust$'Std. Error'), mod_diab_incnum_oth11_mp_robust$`Pr(>|z|)`, stringsAsFactors=F)
colnames(nhanes_incnum_diab_mp_result) <- c('coef','lower CI','upper CI','p','coef','lower CI','upper CI','p','coef','lower CI','upper CI','p','coef','lower CI','upper CI','p','coef','lower CI','upper CI','p','coef','lower CI','upper CI','p','coef','lower CI','upper CI','p','coef','lower CI','upper CI','p','coef','lower CI','upper CI','p')
nhanes_incnum_diab_mp_result


mod_ob_incnum_all_mp <- svyglm(design = surv_all,  obese ~ age + female + smoke_status + insur_stability + inc_to_pov, family=poisson(link=log))
mod_ob_incnum_all_mp_robust <- as.data.frame(coeftest(mod_ob_incnum_all_mp, vcov=sandwich)[,])
mod_ob_incnum_raceadj_mp <- svyglm(design = surv_all,  obese ~ age + female + smoke_status + insur_stability + inc_to_pov + racecat, family=poisson(link=log))
mod_ob_incnum_raceadj_mp_robust <- as.data.frame(coeftest(mod_ob_incnum_raceadj_mp, vcov=sandwich)[,])
mod_ob_incnum_nhw_mp <- svyglm(design = surv_nhw,  obese ~ age + female + smoke_status + insur_stability + inc_to_pov, family=poisson(link=log))
mod_ob_incnum_nhw_mp_robust <- as.data.frame(coeftest(mod_ob_incnum_nhw_mp, vcov=sandwich)[,])
mod_ob_incnum_nhb_mp <- svyglm(design = surv_nhb,  obese ~ age + female + smoke_status + insur_stability + inc_to_pov, family=poisson(link=log))
mod_ob_incnum_nhb_mp_robust <- as.data.frame(coeftest(mod_ob_incnum_nhb_mp, vcov=sandwich)[,])
mod_ob_incnum_his_mp <- svyglm(design = surv_mex,  obese ~ age + female + smoke_status + insur_stability + inc_to_pov, family=poisson(link=log))
mod_ob_incnum_his_mp_robust <- as.data.frame(coeftest(mod_ob_incnum_his_mp, vcov=sandwich)[,])
mod_ob_incnum_nha_mp <- svyglm(design = surv_his,  obese ~ age + female + smoke_status + insur_stability + inc_to_pov, family=poisson(link=log))
mod_ob_incnum_nha_mp_robust <- as.data.frame(coeftest(mod_ob_incnum_nha_mp, vcov=sandwich)[,])
mod_ob_incnum_multi_mp <- svyglm(design = surv_nha, obese ~ age + female + smoke_status + insur_stability + inc_to_pov, family=poisson(link=log))
mod_ob_incnum_multi_mp_robust <- as.data.frame(coeftest(mod_ob_incnum_multi_mp, vcov=sandwich)[,])
mod_ob_incnum_oth_mp <- svyglm(design = surv_oth, obese ~ age + female + smoke_status + insur_stability + inc_to_pov, family=poisson(link=log))
mod_ob_incnum_oth_mp_robust <- as.data.frame(coeftest(mod_ob_incnum_oth_mp, vcov=sandwich)[,])
mod_ob_incnum_oth11_mp <- svyglm(design = surv_oth11,obese ~ age + female + smoke_status + insur_stability + inc_to_pov, family=poisson(link=log))
mod_ob_incnum_oth11_mp_robust <- as.data.frame(coeftest(mod_ob_incnum_oth11_mp, vcov=sandwich)[,])

nhanes_incnum_ob_mp_result <- data.frame(exp(mod_ob_incnum_all_mp_robust$Estimate), exp(mod_ob_incnum_all_mp_robust$Estimate - qnorm(1-0.05)/2*mod_ob_incnum_all_mp_robust$'Std. Error'), exp(mod_ob_incnum_all_mp_robust$Estimate + qnorm(1-0.05)/2*mod_ob_incnum_all_mp_robust$'Std. Error'), mod_ob_incnum_all_mp_robust$`Pr(>|z|)`,
                                      exp(mod_ob_incnum_raceadj_mp_robust$Estimate)[1:10], exp(mod_ob_incnum_raceadj_mp_robust$Estimate - qnorm(1-0.05)/2*mod_ob_incnum_raceadj_mp_robust$'Std. Error')[1:10], exp(mod_ob_incnum_raceadj_mp_robust$Estimate + qnorm(1-0.05)/2*mod_ob_incnum_raceadj_mp_robust$'Std. Error')[1:10], mod_ob_incnum_raceadj_mp_robust$`Pr(>|z|)`[1:10],
                                      exp(mod_ob_incnum_nhw_mp_robust$Estimate), exp(mod_ob_incnum_nhw_mp_robust$Estimate - qnorm(1-0.05)/2*mod_ob_incnum_nhw_mp_robust$'Std. Error'), exp(mod_ob_incnum_nhw_mp_robust$Estimate + qnorm(1-0.05)/2*mod_ob_incnum_nhw_mp_robust$'Std. Error'), mod_ob_incnum_nhw_mp_robust$`Pr(>|z|)`,
                                      exp(mod_ob_incnum_nhb_mp_robust$Estimate), exp(mod_ob_incnum_nhb_mp_robust$Estimate - qnorm(1-0.05)/2*mod_ob_incnum_nhb_mp_robust$'Std. Error'), exp(mod_ob_incnum_nhb_mp_robust$Estimate + qnorm(1-0.05)/2*mod_ob_incnum_nhb_mp_robust$'Std. Error'), mod_ob_incnum_nhb_mp_robust$`Pr(>|z|)`,
                                      exp(mod_ob_incnum_his_mp_robust$Estimate), exp(mod_ob_incnum_his_mp_robust$Estimate - qnorm(1-0.05)/2*mod_ob_incnum_his_mp_robust$'Std. Error'), exp(mod_ob_incnum_his_mp_robust$Estimate + qnorm(1-0.05)/2*mod_ob_incnum_his_mp_robust$'Std. Error'), mod_ob_incnum_his_mp_robust$`Pr(>|z|)`,
                                      exp(mod_ob_incnum_nha_mp_robust$Estimate), exp(mod_ob_incnum_nha_mp_robust$Estimate - qnorm(1-0.05)/2*mod_ob_incnum_nha_mp_robust$'Std. Error'), exp(mod_ob_incnum_nha_mp_robust$Estimate + qnorm(1-0.05)/2*mod_ob_incnum_nha_mp_robust$'Std. Error'), mod_ob_incnum_nha_mp_robust$`Pr(>|z|)`,
                                      exp(mod_ob_incnum_multi_mp_robust$Estimate), exp(mod_ob_incnum_multi_mp_robust$Estimate - qnorm(1-0.05)/2*mod_ob_incnum_multi_mp_robust$'Std. Error'), exp(mod_ob_incnum_multi_mp_robust$Estimate + qnorm(1-0.05)/2*mod_ob_incnum_multi_mp_robust$'Std. Error'), mod_ob_incnum_multi_mp_robust$`Pr(>|z|)`,
                                      exp(mod_ob_incnum_oth_mp_robust$Estimate), exp(mod_ob_incnum_oth_mp_robust$Estimate - qnorm(1-0.05)/2*mod_ob_incnum_oth_mp_robust$'Std. Error'), exp(mod_ob_incnum_oth_mp_robust$Estimate + qnorm(1-0.05)/2*mod_ob_incnum_oth_mp_robust$'Std. Error'), mod_ob_incnum_oth_mp_robust$`Pr(>|z|)`,
                                      exp(mod_ob_incnum_oth11_mp_robust$Estimate), exp(mod_ob_incnum_oth11_mp_robust$Estimate - qnorm(1-0.05)/2*mod_ob_incnum_oth11_mp_robust$'Std. Error'), exp(mod_ob_incnum_oth11_mp_robust$Estimate + qnorm(1-0.05)/2*mod_ob_incnum_oth11_mp_robust$'Std. Error'), mod_ob_incnum_oth11_mp_robust$`Pr(>|z|)`, stringsAsFactors=F)
colnames(nhanes_incnum_ob_mp_result) <- c('coef','lower CI','upper CI','p','coef','lower CI','upper CI','p','coef','lower CI','upper CI','p','coef','lower CI','upper CI','p','coef','lower CI','upper CI','p','coef','lower CI','upper CI','p','coef','lower CI','upper CI','p','coef','lower CI','upper CI','p','coef','lower CI','upper CI','p')
nhanes_incnum_ob_mp_result

nhanes_incnum_diabob_mp_result <- cbind(nhanes_incnum_diab_mp_result,nhanes_incnum_ob_mp_result)
write.csv(nhanes_incnum_diabob_mp_result, 'C:/Users/sarac/OneDrive/Documents/Research/Area v Individual SES/NHANES_SES and AoU_SES/nhanes_ses_incnum_diabob_mp_models.csv',row.names=TRUE)


mod_diab_incnum_all_pois <- svyglm(design = surv_all,  diab ~ age + female + smoke_status + insur_stability+ inc_to_pov, family=poisson(link=log))
mod_diab_incnum_raceadj_pois <- svyglm(design = surv_all,  diab ~ age + female + smoke_status + insur_stability+ inc_to_pov + racecat, family=poisson(link=log))
mod_diab_incnum_nhw_pois <- svyglm(design = surv_nhw,  diab ~ age + female + smoke_status + insur_stability+ inc_to_pov, family=poisson(link=log))
mod_diab_incnum_nhb_pois <- svyglm(design = surv_nhb,  diab ~ age + female + smoke_status + insur_stability+ inc_to_pov, family=poisson(link=log))
mod_diab_incnum_mex_pois <- svyglm(design = surv_mex,  diab ~ age + female + smoke_status + insur_stability+ inc_to_pov, family=poisson(link=log))
mod_diab_incnum_his_pois <- svyglm(design = surv_his,  diab ~ age + female + smoke_status + insur_stability+ inc_to_pov, family=poisson(link=log))
mod_diab_incnum_nha_pois <- svyglm(design = surv_nha, diab ~ age + female + smoke_status + insur_stability+ inc_to_pov, family=poisson(link=log))
mod_diab_incnum_oth_pois <- svyglm(design = surv_oth, diab ~ age + female + smoke_status + insur_stability+ inc_to_pov, family=poisson(link=log))
mod_diab_incnum_oth11_pois <- svyglm(design = surv_oth11,diab ~ age + female + smoke_status + insur_stability+ inc_to_pov, family=poisson(link=log))
nhanes_incnum_diab_pois_result <- data.frame(exp(mod_diab_incnum_all_pois$coefficients),exp(confint(mod_diab_incnum_all_pois)),coef(summary(mod_diab_incnum_all_pois))[,"Pr(>|t|)"],
                                               exp(mod_diab_incnum_raceadj_pois$coefficients)[1:10],exp(confint(mod_diab_incnum_raceadj_pois))[1:10,1:2],coef(summary(mod_diab_incnum_raceadj_pois))[,"Pr(>|t|)"][1:10],
                                               exp(mod_diab_incnum_nhw_pois$coefficients),exp(confint(mod_diab_incnum_nhw_pois)),coef(summary(mod_diab_incnum_nhw_pois))[,"Pr(>|t|)"],
                                               exp(mod_diab_incnum_nhb_pois$coefficients),exp(confint(mod_diab_incnum_nhb_pois)),coef(summary(mod_diab_incnum_nhb_pois))[,"Pr(>|t|)"],
                                               exp(mod_diab_incnum_mex_pois$coefficients),exp(confint(mod_diab_incnum_mex_pois)),coef(summary(mod_diab_incnum_mex_pois))[,"Pr(>|t|)"],
                                               exp(mod_diab_incnum_his_pois$coefficients),exp(confint(mod_diab_incnum_his_pois)),coef(summary(mod_diab_incnum_his_pois))[,"Pr(>|t|)"],
                                               exp(mod_diab_incnum_nha_pois$coefficients),exp(confint(mod_diab_incnum_nha_pois)),coef(summary(mod_diab_incnum_nha_pois))[,"Pr(>|t|)"],
                                               exp(mod_diab_incnum_oth_pois$coefficients),exp(confint(mod_diab_incnum_oth_pois)),coef(summary(mod_diab_incnum_oth_pois))[,"Pr(>|t|)"],
                                               exp(mod_diab_incnum_oth11_pois$coefficients),exp(confint(mod_diab_incnum_oth11_pois)),coef(summary(mod_diab_incnum_oth11_pois))[,"Pr(>|t|)"],stringsAsFactors = FALSE)
nhanes_incnum_diab_pois_result
summary(mod_diab_incnum_all_pois)

mod_ob_incnum_all_pois <- svyglm(design = surv_all,  obese ~ age + female + smoke_status + insur_stability+ inc_to_pov, family=poisson(link=log))
mod_ob_incnum_raceadj_pois <- svyglm(design = surv_all,  obese ~ age + female + smoke_status + insur_stability+ inc_to_pov + racecat, family=poisson(link=log))
mod_ob_incnum_nhw_pois <- svyglm(design = surv_nhw,  obese ~ age + female + smoke_status + insur_stability+ inc_to_pov, family=poisson(link=log))
mod_ob_incnum_nhb_pois <- svyglm(design = surv_nhb,  obese ~ age + female + smoke_status + insur_stability+ inc_to_pov, family=poisson(link=log))
mod_ob_incnum_mex_pois <- svyglm(design = surv_mex,  obese ~ age + female + smoke_status + insur_stability+ inc_to_pov, family=poisson(link=log))
mod_ob_incnum_his_pois <- svyglm(design = surv_his,  obese ~ age + female + smoke_status + insur_stability+ inc_to_pov, family=poisson(link=log))
mod_ob_incnum_nha_pois <- svyglm(design = surv_nha, obese ~ age + female + smoke_status + insur_stability+ inc_to_pov, family=poisson(link=log))
mod_ob_incnum_oth_pois <- svyglm(design = surv_oth, obese ~ age + female + smoke_status + insur_stability+ inc_to_pov, family=poisson(link=log))
mod_ob_incnum_oth11_pois <- svyglm(design = surv_oth11,obese ~ age + female + smoke_status + insur_stability+ inc_to_pov, family=poisson(link=log))
nhanes_incnum_ob_pois_result <- data.frame(exp(mod_ob_incnum_all_pois$coefficients),exp(confint(mod_ob_incnum_all_pois)),coef(summary(mod_ob_incnum_all_pois))[,"Pr(>|t|)"],
                                             exp(mod_ob_incnum_raceadj_pois$coefficients)[1:10],exp(confint(mod_ob_incnum_raceadj_pois))[1:10,1:2],coef(summary(mod_ob_incnum_raceadj_pois))[,"Pr(>|t|)"][1:10],
                                             exp(mod_ob_incnum_nhw_pois$coefficients),exp(confint(mod_ob_incnum_nhw_pois)),coef(summary(mod_ob_incnum_nhw_pois))[,"Pr(>|t|)"],
                                             exp(mod_ob_incnum_nhb_pois$coefficients),exp(confint(mod_ob_incnum_nhb_pois)),coef(summary(mod_ob_incnum_nhb_pois))[,"Pr(>|t|)"],
                                             exp(mod_ob_incnum_mex_pois$coefficients),exp(confint(mod_ob_incnum_mex_pois)),coef(summary(mod_ob_incnum_mex_pois))[,"Pr(>|t|)"],
                                             exp(mod_ob_incnum_his_pois$coefficients),exp(confint(mod_ob_incnum_his_pois)),coef(summary(mod_ob_incnum_his_pois))[,"Pr(>|t|)"],
                                             exp(mod_ob_incnum_nha_pois$coefficients),exp(confint(mod_ob_incnum_nha_pois)),coef(summary(mod_ob_incnum_nha_pois))[,"Pr(>|t|)"],
                                             exp(mod_ob_incnum_oth_pois$coefficients),exp(confint(mod_ob_incnum_oth_pois)),coef(summary(mod_ob_incnum_oth_pois))[,"Pr(>|t|)"],
                                             exp(mod_ob_incnum_oth11_pois$coefficients),exp(confint(mod_ob_incnum_oth11_pois)),coef(summary(mod_ob_incnum_oth11_pois))[,"Pr(>|t|)"],stringsAsFactors = FALSE)
nhanes_incnum_ob_pois_result

nhanes_incnum_diabob_pois_result <- cbind(nhanes_incnum_diab_pois_result,nhanes_incnum_ob_pois_result)
write.csv(nhanes_incnum_diabob_pois_result, 'C:/Users/sarac/OneDrive/Documents/Research/Area v Individual SES/NHANES_SES and AoU_SES/nhanes_ses_incnum_diabob_pois_models.csv',row.names=TRUE)



#####sensitivity analyses - forest plots of cont educ and inc with QB and MP and POIS models)
tiff(file='C:/Users/sarac/OneDrive/Documents/Research/Area v Individual SES/NHANES_SES and AoU_SES/SuppFig_nhanes_forest_indiv_educcont_stratandnonstrat_qb.tiff', width=8, height=5, units='in', res=300)
forestplot(c('Type 2 Diabetes','Obesity'), 
           mean = cbind(t(nhanes_educcont_diabob_qb_result[10,c(1,37)]),t(nhanes_educcont_diabob_qb_result[10,c(5,41)]),
                        t(nhanes_educcont_diabob_qb_result[10,c(9,45)]),t(nhanes_educcont_diabob_qb_result[10,c(13,49)]),
                        t(nhanes_educcont_diabob_qb_result[10,c(17,53)]),t(nhanes_educcont_diabob_qb_result[10,c(21,57)]),
                        t(nhanes_educcont_diabob_qb_result[10,c(25,61)]),t(nhanes_educcont_diabob_qb_result[10,c(29,65)]),
                        t(nhanes_educcont_diabob_qb_result[10,c(33,69)])),
           lower = cbind(t(nhanes_educcont_diabob_qb_result[10,c(2,38)]),t(nhanes_educcont_diabob_qb_result[10,c(6,42)]),
                         t(nhanes_educcont_diabob_qb_result[10,c(10,46)]),t(nhanes_educcont_diabob_qb_result[10,c(14,50)]),
                         t(nhanes_educcont_diabob_qb_result[10,c(18,54)]),t(nhanes_educcont_diabob_qb_result[10,c(22,58)]),
                         t(nhanes_educcont_diabob_qb_result[10,c(26,62)]),t(nhanes_educcont_diabob_qb_result[10,c(30,66)]),
                         t(nhanes_educcont_diabob_qb_result[10,c(34,68)])),
           upper = cbind(t(nhanes_educcont_diabob_qb_result[10,c(3,39)]),t(nhanes_educcont_diabob_qb_result[10,c(7,43)]),
                         t(nhanes_educcont_diabob_qb_result[10,c(11,47)]),t(nhanes_educcont_diabob_qb_result[10,c(15,51)]),
                         t(nhanes_educcont_diabob_qb_result[10,c(19,55)]),t(nhanes_educcont_diabob_qb_result[10,c(23,59)]),
                         t(nhanes_educcont_diabob_qb_result[10,c(27,63)]),t(nhanes_educcont_diabob_qb_result[10,c(31,67)]),
                         t(nhanes_educcont_diabob_qb_result[10,c(35,71)])),
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


tiff(file='C:/Users/sarac/OneDrive/Documents/Research/Area v Individual SES/NHANES_SES and AoU_SES/SuppFig_nhanes_forest_indiv_inccont_stratandnonstrat_qb.tiff', width=8, height=5, units='in', res=300)
forestplot(c('Type 2 Diabetes','Obesity'), 
           mean = cbind(t(nhanes_incnum_diabob_qb_result[10,c(1,37)]),t(nhanes_incnum_diabob_qb_result[10,c(5,41)]),
                        t(nhanes_incnum_diabob_qb_result[10,c(9,45)]),t(nhanes_incnum_diabob_qb_result[10,c(13,49)]),
                        t(nhanes_incnum_diabob_qb_result[10,c(17,53)]),t(nhanes_incnum_diabob_qb_result[10,c(21,57)]),
                        t(nhanes_incnum_diabob_qb_result[10,c(25,61)]),t(nhanes_incnum_diabob_qb_result[10,c(29,65)]),
                        t(nhanes_incnum_diabob_qb_result[10,c(33,69)])),
           lower = cbind(t(nhanes_incnum_diabob_qb_result[10,c(2,38)]),t(nhanes_incnum_diabob_qb_result[10,c(6,42)]),
                         t(nhanes_incnum_diabob_qb_result[10,c(10,46)]),t(nhanes_incnum_diabob_qb_result[10,c(14,50)]),
                         t(nhanes_incnum_diabob_qb_result[10,c(18,54)]),t(nhanes_incnum_diabob_qb_result[10,c(22,58)]),
                         t(nhanes_incnum_diabob_qb_result[10,c(26,62)]),t(nhanes_incnum_diabob_qb_result[10,c(30,66)]),
                         t(nhanes_incnum_diabob_qb_result[10,c(34,68)])),
           upper = cbind(t(nhanes_incnum_diabob_qb_result[10,c(3,39)]),t(nhanes_incnum_diabob_qb_result[10,c(7,43)]),
                         t(nhanes_incnum_diabob_qb_result[10,c(11,47)]),t(nhanes_incnum_diabob_qb_result[10,c(15,51)]),
                         t(nhanes_incnum_diabob_qb_result[10,c(19,55)]),t(nhanes_incnum_diabob_qb_result[10,c(23,59)]),
                         t(nhanes_incnum_diabob_qb_result[10,c(27,63)]),t(nhanes_incnum_diabob_qb_result[10,c(31,67)]),
                         t(nhanes_incnum_diabob_qb_result[10,c(35,71)])),
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

tiff(file='C:/Users/sarac/OneDrive/Documents/Research/Area v Individual SES/NHANES_SES and AoU_SES/SuppFig_nhanes_forest_indiv_educcont_stratandnonstrat_mp.tiff', width=8, height=5, units='in', res=300)
forestplot(c('Type 2 Diabetes','Obesity'), 
           mean = cbind(t(nhanes_educcont_diabob_mp_result[10,c(1,37)]),t(nhanes_educcont_diabob_mp_result[10,c(5,41)]),
                        t(nhanes_educcont_diabob_mp_result[10,c(9,45)]),t(nhanes_educcont_diabob_mp_result[10,c(13,49)]),
                        t(nhanes_educcont_diabob_mp_result[10,c(17,53)]),t(nhanes_educcont_diabob_mp_result[10,c(21,57)]),
                        t(nhanes_educcont_diabob_mp_result[10,c(25,61)]),t(nhanes_educcont_diabob_mp_result[10,c(29,65)]),
                        t(nhanes_educcont_diabob_mp_result[10,c(33,69)])),
           lower = cbind(t(nhanes_educcont_diabob_mp_result[10,c(2,38)]),t(nhanes_educcont_diabob_mp_result[10,c(6,42)]),
                         t(nhanes_educcont_diabob_mp_result[10,c(10,46)]),t(nhanes_educcont_diabob_mp_result[10,c(14,50)]),
                         t(nhanes_educcont_diabob_mp_result[10,c(18,54)]),t(nhanes_educcont_diabob_mp_result[10,c(22,58)]),
                         t(nhanes_educcont_diabob_mp_result[10,c(26,62)]),t(nhanes_educcont_diabob_mp_result[10,c(30,66)]),
                         t(nhanes_educcont_diabob_mp_result[10,c(34,68)])),
           upper = cbind(t(nhanes_educcont_diabob_mp_result[10,c(3,39)]),t(nhanes_educcont_diabob_mp_result[10,c(7,43)]),
                         t(nhanes_educcont_diabob_mp_result[10,c(11,47)]),t(nhanes_educcont_diabob_mp_result[10,c(15,51)]),
                         t(nhanes_educcont_diabob_mp_result[10,c(19,55)]),t(nhanes_educcont_diabob_mp_result[10,c(23,59)]),
                         t(nhanes_educcont_diabob_mp_result[10,c(27,63)]),t(nhanes_educcont_diabob_mp_result[10,c(31,67)]),
                         t(nhanes_educcont_diabob_mp_result[10,c(35,71)])),
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


tiff(file='C:/Users/sarac/OneDrive/Documents/Research/Area v Individual SES/NHANES_SES and AoU_SES/SuppFig_nhanes_forest_indiv_inccont_stratandnonstrat_mp.tiff', width=8, height=5, units='in', res=300)
forestplot(c('Type 2 Diabetes','Obesity'), 
           mean = cbind(t(nhanes_incnum_diabob_mp_result[10,c(1,37)]),t(nhanes_incnum_diabob_mp_result[10,c(5,41)]),
                        t(nhanes_incnum_diabob_mp_result[10,c(9,45)]),t(nhanes_incnum_diabob_mp_result[10,c(13,49)]),
                        t(nhanes_incnum_diabob_mp_result[10,c(17,53)]),t(nhanes_incnum_diabob_mp_result[10,c(21,57)]),
                        t(nhanes_incnum_diabob_mp_result[10,c(25,61)]),t(nhanes_incnum_diabob_mp_result[10,c(29,65)]),
                        t(nhanes_incnum_diabob_mp_result[10,c(33,69)])),
           lower = cbind(t(nhanes_incnum_diabob_mp_result[10,c(2,38)]),t(nhanes_incnum_diabob_mp_result[10,c(6,42)]),
                         t(nhanes_incnum_diabob_mp_result[10,c(10,46)]),t(nhanes_incnum_diabob_mp_result[10,c(14,50)]),
                         t(nhanes_incnum_diabob_mp_result[10,c(18,54)]),t(nhanes_incnum_diabob_mp_result[10,c(22,58)]),
                         t(nhanes_incnum_diabob_mp_result[10,c(26,62)]),t(nhanes_incnum_diabob_mp_result[10,c(30,66)]),
                         t(nhanes_incnum_diabob_mp_result[10,c(34,68)])),
           upper = cbind(t(nhanes_incnum_diabob_mp_result[10,c(3,39)]),t(nhanes_incnum_diabob_mp_result[10,c(7,43)]),
                         t(nhanes_incnum_diabob_mp_result[10,c(11,47)]),t(nhanes_incnum_diabob_mp_result[10,c(15,51)]),
                         t(nhanes_incnum_diabob_mp_result[10,c(19,55)]),t(nhanes_incnum_diabob_mp_result[10,c(23,59)]),
                         t(nhanes_incnum_diabob_mp_result[10,c(27,63)]),t(nhanes_incnum_diabob_mp_result[10,c(31,67)]),
                         t(nhanes_incnum_diabob_mp_result[10,c(35,71)])),
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


tiff(file='C:/Users/sarac/OneDrive/Documents/Research/Area v Individual SES/NHANES_SES and AoU_SES/SuppFig_nhanes_forest_indiv_educcont_stratandnonstrat_pois.tiff', width=8, height=5, units='in', res=300)
forestplot(c('Type 2 Diabetes','Obesity'), 
           mean = cbind(t(nhanes_educcont_diabob_pois_result[10,c(1,37)]),t(nhanes_educcont_diabob_pois_result[10,c(5,41)]),
                        t(nhanes_educcont_diabob_pois_result[10,c(9,45)]),t(nhanes_educcont_diabob_pois_result[10,c(13,49)]),
                        t(nhanes_educcont_diabob_pois_result[10,c(17,53)]),t(nhanes_educcont_diabob_pois_result[10,c(21,57)]),
                        t(nhanes_educcont_diabob_pois_result[10,c(25,61)]),t(nhanes_educcont_diabob_pois_result[10,c(29,65)]),
                        t(nhanes_educcont_diabob_pois_result[10,c(33,69)])),
           lower = cbind(t(nhanes_educcont_diabob_pois_result[10,c(2,38)]),t(nhanes_educcont_diabob_pois_result[10,c(6,42)]),
                         t(nhanes_educcont_diabob_pois_result[10,c(10,46)]),t(nhanes_educcont_diabob_pois_result[10,c(14,50)]),
                         t(nhanes_educcont_diabob_pois_result[10,c(18,54)]),t(nhanes_educcont_diabob_pois_result[10,c(22,58)]),
                         t(nhanes_educcont_diabob_pois_result[10,c(26,62)]),t(nhanes_educcont_diabob_pois_result[10,c(30,66)]),
                         t(nhanes_educcont_diabob_pois_result[10,c(34,68)])),
           upper = cbind(t(nhanes_educcont_diabob_pois_result[10,c(3,39)]),t(nhanes_educcont_diabob_pois_result[10,c(7,43)]),
                         t(nhanes_educcont_diabob_pois_result[10,c(11,47)]),t(nhanes_educcont_diabob_pois_result[10,c(15,51)]),
                         t(nhanes_educcont_diabob_pois_result[10,c(19,55)]),t(nhanes_educcont_diabob_pois_result[10,c(23,59)]),
                         t(nhanes_educcont_diabob_pois_result[10,c(27,63)]),t(nhanes_educcont_diabob_pois_result[10,c(31,67)]),
                         t(nhanes_educcont_diabob_pois_result[10,c(35,71)])),
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


tiff(file='C:/Users/sarac/OneDrive/Documents/Research/Area v Individual SES/NHANES_SES and AoU_SES/SuppFig_nhanes_forest_indiv_inccont_stratandnonstrat_pois.tiff', width=8, height=5, units='in', res=300)
forestplot(c('Type 2 Diabetes','Obesity'), 
           mean = cbind(t(nhanes_incnum_diabob_pois_result[10,c(1,37)]),t(nhanes_incnum_diabob_pois_result[10,c(5,41)]),
                        t(nhanes_incnum_diabob_pois_result[10,c(9,45)]),t(nhanes_incnum_diabob_pois_result[10,c(13,49)]),
                        t(nhanes_incnum_diabob_pois_result[10,c(17,53)]),t(nhanes_incnum_diabob_pois_result[10,c(21,57)]),
                        t(nhanes_incnum_diabob_pois_result[10,c(25,61)]),t(nhanes_incnum_diabob_pois_result[10,c(29,65)]),
                        t(nhanes_incnum_diabob_pois_result[10,c(33,69)])),
           lower = cbind(t(nhanes_incnum_diabob_pois_result[10,c(2,38)]),t(nhanes_incnum_diabob_pois_result[10,c(6,42)]),
                         t(nhanes_incnum_diabob_pois_result[10,c(10,46)]),t(nhanes_incnum_diabob_pois_result[10,c(14,50)]),
                         t(nhanes_incnum_diabob_pois_result[10,c(18,54)]),t(nhanes_incnum_diabob_pois_result[10,c(22,58)]),
                         t(nhanes_incnum_diabob_pois_result[10,c(26,62)]),t(nhanes_incnum_diabob_pois_result[10,c(30,66)]),
                         t(nhanes_incnum_diabob_pois_result[10,c(34,68)])),
           upper = cbind(t(nhanes_incnum_diabob_pois_result[10,c(3,39)]),t(nhanes_incnum_diabob_pois_result[10,c(7,43)]),
                         t(nhanes_incnum_diabob_pois_result[10,c(11,47)]),t(nhanes_incnum_diabob_pois_result[10,c(15,51)]),
                         t(nhanes_incnum_diabob_pois_result[10,c(19,55)]),t(nhanes_incnum_diabob_pois_result[10,c(23,59)]),
                         t(nhanes_incnum_diabob_pois_result[10,c(27,63)]),t(nhanes_incnum_diabob_pois_result[10,c(31,67)]),
                         t(nhanes_incnum_diabob_pois_result[10,c(35,71)])),
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



#####t2d - bmi mediation - education#####
bmi_med_educall <- svyglm(design=surv_all, bmi ~ age + female + survyr + smoke_status + insur_stability + educ_num)
bmi_med_educall_raceadj <- svyglm(design=surv_all, bmi ~ age + female + survyr + smoke_status + insur_stability + educ_num + racecat)
bmi_med_educnhw <- svyglm(design=surv_nhw, bmi ~ age + female + survyr + smoke_status + insur_stability + educ_num)
bmi_med_educnhb <- svyglm(design=surv_nhb, bmi ~ age + female + survyr + smoke_status + insur_stability + educ_num)
bmi_med_educmex <- svyglm(design=surv_mex, bmi ~ age + female + survyr + smoke_status + insur_stability + educ_num)
bmi_med_educhis <- svyglm(design=surv_his, bmi ~ age + female + survyr + smoke_status + insur_stability + educ_num)
bmi_med_educnha <- svyglm(design=surv_nha, bmi ~ age + female + survyr + smoke_status + insur_stability + educ_num)
bmi_med_educoth <- svyglm(design=surv_oth, bmi ~ age + female + survyr + smoke_status + insur_stability + educ_num)
bmi_med_educoth11 <- svyglm(design=surv_oth11, bmi ~ age + female + survyr + smoke_status + insur_stability + educ_num)

t2d_propmediated_bmi_educ <- rbind(cbind('All',mod_diab_educcont_all_bmi$coefficients[11] + bmi_med_educall$coefficients[11]*mod_diab_educcont_all_bmi$coefficients[12], mod_diab_educcont_all_bmi$coefficients[11], bmi_med_educall$coefficients[11]*mod_diab_educcont_all_bmi$coefficients[12], bmi_med_educall$coefficients[11]*mod_diab_educcont_all_bmi$coefficients[12]/(mod_diab_educcont_all_bmi$coefficients[11] + bmi_med_educall$coefficients[11]*mod_diab_educcont_all_bmi$coefficients[12])),
                              cbind('All-Race-adjusted',mod_diab_educcont_all_raceadj_bmi$coefficients[11] + bmi_med_educall_raceadj$coefficients[11]*mod_diab_educcont_all_raceadj_bmi$coefficients[12], mod_diab_educcont_all_raceadj_bmi$coefficients[11], bmi_med_educall_raceadj$coefficients[11]*mod_diab_educcont_all_bmi$coefficients[12], bmi_med_educall$coefficients[11]*mod_diab_educcont_all_raceadj_bmi$coefficients[12]/(mod_diab_educcont_all_raceadj_bmi$coefficients[11] + bmi_med_educall_raceadj$coefficients[11]*mod_diab_educcont_all_raceadj_bmi$coefficients[12])),
                              cbind('NHW',mod_diab_educcont_nhw_bmi$coefficients[11] + bmi_med_educnhw$coefficients[11]*mod_diab_educcont_nhw_bmi$coefficients[12], mod_diab_educcont_nhw_bmi$coefficients[11], bmi_med_educnhw$coefficients[11]*mod_diab_educcont_nhw_bmi$coefficients[12], bmi_med_educnhw$coefficients[11]*mod_diab_educcont_nhw_bmi$coefficients[12]/(mod_diab_educcont_nhw_bmi$coefficients[11] + bmi_med_educnhw$coefficients[11]*mod_diab_educcont_nhw_bmi$coefficients[12])),
                              cbind('NHB',mod_diab_educcont_nhb_bmi$coefficients[11] + bmi_med_educnhb$coefficients[11]*mod_diab_educcont_nhb_bmi$coefficients[12], mod_diab_educcont_nhb_bmi$coefficients[11], bmi_med_educnhb$coefficients[11]*mod_diab_educcont_nhb_bmi$coefficients[12], bmi_med_educnhb$coefficients[11]*mod_diab_educcont_nhb_bmi$coefficients[12]/(mod_diab_educcont_nhb_bmi$coefficients[11] + bmi_med_educnhb$coefficients[11]*mod_diab_educcont_nhw_bmi$coefficients[12])),
                              cbind('MEX',mod_diab_educcont_mex_bmi$coefficients[11] + bmi_med_educmex$coefficients[11]*mod_diab_educcont_mex_bmi$coefficients[12], mod_diab_educcont_mex_bmi$coefficients[11], bmi_med_educmex$coefficients[11]*mod_diab_educcont_mex_bmi$coefficients[12], bmi_med_educmex$coefficients[11]*mod_diab_educcont_mex_bmi$coefficients[12]/(mod_diab_educcont_mex_bmi$coefficients[11] + bmi_med_educmex$coefficients[11]*mod_diab_educcont_mex_bmi$coefficients[12])),
                              cbind('HIS',mod_diab_educcont_his_bmi$coefficients[11] + bmi_med_educhis$coefficients[11]*mod_diab_educcont_his_bmi$coefficients[12], mod_diab_educcont_his_bmi$coefficients[11], bmi_med_educhis$coefficients[11]*mod_diab_educcont_his_bmi$coefficients[12], bmi_med_educhis$coefficients[11]*mod_diab_educcont_his_bmi$coefficients[12]/(mod_diab_educcont_his_bmi$coefficients[11] + bmi_med_educhis$coefficients[11]*mod_diab_educcont_his_bmi$coefficients[12])),
                              cbind('NHA',mod_diab_educcont_nha_bmi$coefficients[11] + bmi_med_educnha$coefficients[11]*mod_diab_educcont_nha_bmi$coefficients[12], mod_diab_educcont_nha_bmi$coefficients[11], bmi_med_educnha$coefficients[11]*mod_diab_educcont_nha_bmi$coefficients[12], bmi_med_educnha$coefficients[11]*mod_diab_educcont_nha_bmi$coefficients[12]/(mod_diab_educcont_nha_bmi$coefficients[11] + bmi_med_educnha$coefficients[11]*mod_diab_educcont_nha_bmi$coefficients[12])),
                              cbind('OTH',mod_diab_educcont_oth_bmi$coefficients[11] + bmi_med_educoth$coefficients[11]*mod_diab_educcont_oth_bmi$coefficients[12], mod_diab_educcont_oth_bmi$coefficients[11], bmi_med_educoth$coefficients[11]*mod_diab_educcont_oth_bmi$coefficients[12], bmi_med_educoth$coefficients[11]*mod_diab_educcont_oth_bmi$coefficients[12]/(mod_diab_educcont_oth_bmi$coefficients[11] + bmi_med_educoth$coefficients[11]*mod_diab_educcont_oth_bmi$coefficients[12])),
                              cbind('OTH11',mod_diab_educcont_oth11_bmi$coefficients[11] + bmi_med_educoth11$coefficients[11]*mod_diab_educcont_oth11_bmi$coefficients[12], mod_diab_educcont_oth11_bmi$coefficients[11], bmi_med_educoth11$coefficients[11]*mod_diab_educcont_oth11_bmi$coefficients[12], bmi_med_educoth11$coefficients[11]*mod_diab_educcont_oth11_bmi$coefficients[12]/(mod_diab_educcont_oth11_bmi$coefficients[11] + bmi_med_educoth11$coefficients[11]*mod_diab_educcont_oth11_bmi$coefficients[12])))
colnames(t2d_propmediated_bmi_educ) <- c('Race/Ethnicity Group','Total Effect','Direct Effect','Indirect Effect','Proportion Mediated')
t2d_propmediated_bmi_educ

install.packages('htmltools')
install.packages('mediation')
library(mediation)
#CIs are tiny (<0.01 for group as a whole); takes A LOT of ram to run
mod_educ_bmi_all <- svyglm(design=surv_all, bmi ~ age + female + survyr + smoke_status + insur_stability + educ_num)
mod_educ_diab_bmiadj_all <- svyglm(design = surv_all, diab ~ age + female + survyr + smoke_status + insur_stability + educ_num + bmi, family=binomial(link=logit))
set.seed(123)
med.out_all <- mediate(mod_educ_bmi_all, mod_educ_diab_bmiadj_all, treat='educ_num', mediator='bmi', robustSE=T, sims=1000)
summary(med.out_all)
mod_educ_bmi_raceadj <- svyglm(design=surv_all, bmi ~ age + female + survyr + smoke_status + insur_stability + educ_num + racecat)
mod_educ_diab_bmiadj_raceadj <- svyglm(design = surv_all, diab ~ age + female + survyr + smoke_status + insur_stability + educ_num + bmi + racecat, family=binomial(link=logit))
med.out_raceadj <- mediate(mod_educ_bmi_raceadj, mod_educ_diab_bmiadj_raceadj, treat='educ_num', mediator='bmi', robustSE=T, sims=1000)
summary(med.out_raceadj)
mod_educ_bmi_nhw <- svyglm(design=surv_nhw, bmi ~ age + female + survyr + smoke_status + insur_stability + educ_num)
mod_educ_diab_bmiadj_nhw <- svyglm(design = surv_nhw, diab ~ age + female + survyr + smoke_status + insur_stability + educ_num + bmi, family=binomial(link=logit))
med.out_nhw <- mediate(mod_educ_bmi_nhw, mod_educ_diab_bmiadj_nhw, treat='educ_num', mediator='bmi', robustSE=T, sims=1000)
summary(med.out_nhw)
mod_educ_bmi_nhb <- svyglm(design=surv_nhb, bmi ~ age + female + survyr + smoke_status + insur_stability + educ_num)
mod_educ_diab_bmiadj_nhb <- svyglm(design = surv_nhb, diab ~ age + female + survyr + smoke_status + insur_stability + educ_num + bmi, family=binomial(link=logit))
med.out_nhb <- mediate(mod_educ_bmi_nhb, mod_educ_diab_bmiadj_nhb, treat='educ_num', mediator='bmi', robustSE=T, sims=1000)
summary(med.out_nhb)
mod_educ_bmi_mex <- svyglm(design=surv_mex, bmi ~ age + female + survyr + smoke_status + insur_stability + educ_num)
mod_educ_diab_bmiadj_mex <- svyglm(design = surv_mex, diab ~ age + female + survyr + smoke_status + insur_stability + educ_num + bmi, family=binomial(link=logit))
med.out_mex <- mediate(mod_educ_bmi_mex, mod_educ_diab_bmiadj_mex, treat='educ_num', mediator='bmi', robustSE=T, sims=1000)
summary(med.out_mex)
mod_educ_bmi_his <- svyglm(design=surv_his, bmi ~ age + female + survyr + smoke_status + insur_stability + educ_num)
mod_educ_diab_bmiadj_his <- svyglm(design = surv_his, diab ~ age + female + survyr + smoke_status + insur_stability + educ_num + bmi, family=binomial(link=logit))
med.out_his <- mediate(mod_educ_bmi_his, mod_educ_diab_bmiadj_his, treat='educ_num', mediator='bmi', robustSE=T, sims=1000)
summary(med.out_his)
mod_educ_bmi_nha <- svyglm(design=surv_nha, bmi ~ age + female + survyr + smoke_status + insur_stability + educ_num)
mod_educ_diab_bmiadj_nha <- svyglm(design = surv_nha, diab ~ age + female + survyr + smoke_status + insur_stability + educ_num + bmi, family=binomial(link=logit))
med.out_nha <- mediate(mod_educ_bmi_nha, mod_educ_diab_bmiadj_nha, treat='educ_num', mediator='bmi', robustSE=T, sims=1000)
summary(med.out_nha)
mod_educ_bmi_oth <- svyglm(design=surv_oth, bmi ~ age + female + survyr + smoke_status + insur_stability + educ_num)
mod_educ_diab_bmiadj_oth <- svyglm(design = surv_oth, diab ~ age + female + survyr + smoke_status + insur_stability + educ_num + bmi, family=binomial(link=logit))
med.out_oth <- mediate(mod_educ_bmi_oth, mod_educ_diab_bmiadj_oth, treat='educ_num', mediator='bmi', robustSE=T, sims=1000)
summary(med.out_oth)
mod_educ_bmi_oth11 <- svyglm(design=surv_oth11, bmi ~ age + female + survyr + smoke_status + insur_stability + educ_num)
mod_educ_diab_bmiadj_oth11 <- svyglm(design = surv_oth11, diab ~ age + female + survyr + smoke_status + insur_stability + educ_num + bmi, family=binomial(link=logit))
med.out_oth11 <- mediate(mod_educ_bmi_oth11, mod_educ_diab_bmiadj_oth11, treat='educ_num', mediator='bmi', robustSE=T, sims=1000)
summary(med.out_oth11)


#####I2 Statistic - Education#####
###calc I2 for education -> diabetes prevalence
yi <- as.vector(t(mod_alldx_educcont_allrace[11,c(9,13,17,21,25,29,33)]))
n <- as.vector(t(table(nhanes[nhanes$age >= 18 & !nhanes$insur_stability=='Missing',]$racecat)))
racecatname <-rownames(table(nhanes[nhanes$age >= 18 & !nhanes$insur_stability=='Missing',]$racecat))
sei <- as.vector(t(( mod_alldx_educcont_allrace[11,c(11,15,19,23,27,31,35)] - mod_alldx_educcont_allrace[11,c(10,14,18,22,26,30,34)] ) / 3.92))
vi <- as.vector(t((sei*sqrt(n))^2)) #variance is SD^2, SD is SE*sqrt(n)
nhanes_dat <- data.frame(racecatname,n,yi,vi,sei)
nhanes_dat$log_yi <- log(yi, base=exp(1))
res_educ_diab <- rma(yi=log_yi,sei=sei, data=nhanes_dat)
res_nonhw_educ_diab <- rma(yi=log_yi,sei=sei, data=nhanes_dat[!racecatname=='NHW',])
res_nonhb_educ_diab <- rma(yi=log_yi,sei=sei, data=nhanes_dat[!racecatname=='NHB',])
res_nomex_educ_diab <- rma(yi=log_yi,sei=sei, data=nhanes_dat[!racecatname=='Mexican-American',])
res_nohis_educ_diab <- rma(yi=log_yi,sei=sei, data=nhanes_dat[!racecatname=='Other Hispanic',])
res_nonha_educ_diab <- rma(yi=log_yi,sei=sei, data=nhanes_dat[!racecatname=='NHAsian',])
res_nooth_educ_diab <- rma(yi=log_yi,sei=sei, data=nhanes_dat[!racecatname=='Other/Multi-Racial including NHA (pre-2011)',])
res_nooth11_educ_diab <- rma(yi=log_yi,sei=sei, data=nhanes_dat[!racecatname=='Other/Multi-Racial (post-2011)',])
i2_educ_diab <- rbind(res_educ_diab$I2, res_nonhw_educ_diab$I2, res_nonhb_educ_diab$I2, res_nomex_educ_diab$I2, res_nohis_educ_diab$I2, res_nonha_educ_diab$I2, res_nooth_educ_diab$I2, res_nooth11_educ_diab$I2)
#I2 from raw numbers: educ->diab prevalence - USE ME
n <- as.vector(t(table(nhanes[nhanes$age >= 18 & !nhanes$insur_stability=='Missing',]$racecat)))
racecatname <-rownames(table(nhanes[nhanes$age >= 18 & !nhanes$insur_stability=='Missing',]$racecat))
ESTIM_educ_diab <- c(coef(summary(mod_diab_educcont_nhw))[11,"Estimate"],coef(summary(mod_diab_educcont_nhb))[11,"Estimate"],
                  coef(summary(mod_diab_educcont_mex))[11,"Estimate"],coef(summary(mod_diab_educcont_his))[11,"Estimate"],coef(summary(mod_diab_educcont_nha))[11,"Estimate"],
                  coef(summary(mod_diab_educcont_oth))[11,"Estimate"],coef(summary(mod_diab_educcont_oth11))[11,"Estimate"])
SE_educ_diab <- c(coef(summary(mod_diab_educcont_nhw))[11,"Std. Error"],coef(summary(mod_diab_educcont_nhb))[11,"Std. Error"],
               coef(summary(mod_diab_educcont_mex))[11,"Std. Error"],coef(summary(mod_diab_educcont_his))[11,"Std. Error"],coef(summary(mod_diab_educcont_nha))[11,"Std. Error"],
               coef(summary(mod_diab_educcont_oth))[11,"Std. Error"],coef(summary(mod_diab_educcont_oth11))[11,"Std. Error"])
vi <- as.vector(t((SE_educ_diab*sqrt(n))^2)) #variance is SD^2, SD is SE*sqrt(n)
nhanes_dat_educ_diab <- data.frame(racecatname,n,ESTIM_educ_diab,vi,SE_educ_diab)
res_educ_diab <- rma(yi=ESTIM_educ_diab,sei=SE_educ_diab, data=nhanes_dat_educ_diab)
res_nonhw_educ_diab <- rma(yi=ESTIM_educ_diab,sei=SE_educ_diab, data=nhanes_dat_educ_diab[!racecatname=='NHW',])
res_nonhb_educ_diab <- rma(yi=ESTIM_educ_diab,sei=SE_educ_diab, data=nhanes_dat_educ_diab[!racecatname=='NHB',])
res_nomex_educ_diab <- rma(yi=ESTIM_educ_diab,sei=SE_educ_diab, data=nhanes_dat_educ_diab[!racecatname=='Mexican-American',])
res_nohis_educ_diab <- rma(yi=ESTIM_educ_diab,sei=SE_educ_diab, data=nhanes_dat_educ_diab[!racecatname=='Other Hispanic',])
res_nonha_educ_diab <- rma(yi=ESTIM_educ_diab,sei=SE_educ_diab, data=nhanes_dat_educ_diab[!racecatname=='NHAsian',])
res_nooth_educ_diab <- rma(yi=ESTIM_educ_diab,sei=SE_educ_diab, data=nhanes_dat_educ_diab[!racecatname=='Other/Multi-Racial including NHA (pre-2011)',])
res_nooth11_educ_diab <- rma(yi=ESTIM_educ_diab,sei=SE_educ_diab, data=nhanes_dat_educ_diab[!racecatname=='Other/Multi-Racial (post-2011)',])
res_nonhwnh_educ_diaba <- rma(yi=ESTIM_educ_diab,sei=SE_educ_diab, data=nhanes_dat_educ_diab[!racecatname=='NHW' & !racecatname=='NHAsian',])
res_nhwnha_educ_diab <- rma(yi=ESTIM_educ_diab,sei=SE_educ_diab, data=nhanes_dat_educ_diab[racecatname=='NHW' | racecatname=='NHAsian',])
i2_educ_diab <- rbind(res_educ_diab$I2, res_nonhw_educ_diab$I2, res_nonhb_educ_diab$I2, res_nomex_educ_diab$I2, res_nohis_educ_diab$I2, res_nonha_educ_diab$I2, res_nooth_educ_diab$I2, res_nooth11_educ_diab$I2)
i2_educ_diab
influence(res_educ_diab, progbar=F)

###calc I2 for education -> obesity prevalence
yi <- as.vector(t(mod_alldx_educcont_allrace[11,c(45,49,53,57,61,65,69)]))
n <- as.vector(t(table(nhanes[nhanes$age >= 18 & !nhanes$insur_stability=='Missing',]$racecat)))
racecatname <-rownames(table(nhanes[nhanes$age >= 18 & !nhanes$insur_stability=='Missing',]$racecat))
sei <- as.vector(t(( mod_alldx_educcont_allrace[11,c(47,51,55,59,63,67,71)] - mod_alldx_educcont_allrace[11,c(46,50,54,58,62,66,69)] ) / 3.92))
vi <- as.vector(t((sei*sqrt(n))^2)) #variance is SD^2, SD is SE*sqrt(n)
nhanes_dat <- data.frame(racecatname,n,yi,vi,sei)
nhanes_dat$log_yi <- log(yi, base=exp(1))
res_educ_ob <- rma(yi=log_yi,sei=sei, data=nhanes_dat)
res_nonhw_educ_ob <- rma(yi=log_yi,sei=sei, data=nhanes_dat[!racecatname=='NHW',])
res_nonhb_educ_ob <- rma(yi=log_yi,sei=sei, data=nhanes_dat[!racecatname=='NHB',])
res_nomex_educ_ob <- rma(yi=log_yi,sei=sei, data=nhanes_dat[!racecatname=='Mexican-American',])
res_nohis_educ_ob <- rma(yi=log_yi,sei=sei, data=nhanes_dat[!racecatname=='Other Hispanic',])
res_nonha_educ_ob <- rma(yi=log_yi,sei=sei, data=nhanes_dat[!racecatname=='NHAsian',])
res_nooth_educ_ob <- rma(yi=log_yi,sei=sei, data=nhanes_dat[!racecatname=='Other/Multi-Racial including NHA (pre-2011)',])
res_nooth11_educ_ob <- rma(yi=log_yi,sei=sei, data=nhanes_dat[!racecatname=='Other/Multi-Racial (post-2011)',])
i2_educ_ob <- rbind(res_educ_ob$I2, res_nonhw_educ_ob$I2, res_nonhb_educ_ob$I2, res_nomex_educ_ob$I2, res_nohis_educ_ob$I2, res_nonha_educ_ob$I2, res_nooth_educ_ob$I2, res_nooth11_educ_ob$I2)
#I2 from raw numbers: educ->obes prevalence - USE ME
n <- as.vector(t(table(nhanes[nhanes$age >= 18 & !nhanes$insur_stability=='Missing',]$racecat)))
racecatname <-rownames(table(nhanes[nhanes$age >= 18 & !nhanes$insur_stability=='Missing',]$racecat))
ESTIM_educ_ob <- c(coef(summary(mod_obes_educcont_nhw))[11,"Estimate"],coef(summary(mod_obes_educcont_nhb))[11,"Estimate"],
                     coef(summary(mod_obes_educcont_mex))[11,"Estimate"],coef(summary(mod_obes_educcont_his))[11,"Estimate"],coef(summary(mod_obes_educcont_nha))[11,"Estimate"],
                   coef(summary(mod_obes_educcont_oth))[11,"Estimate"],coef(summary(mod_obes_educcont_oth11))[11,"Estimate"])
SE_educ_ob <- c(coef(summary(mod_obes_educcont_nhw))[11,"Std. Error"],coef(summary(mod_obes_educcont_nhb))[11,"Std. Error"],
                  coef(summary(mod_obes_educcont_mex))[11,"Std. Error"],coef(summary(mod_obes_educcont_his))[11,"Std. Error"],coef(summary(mod_obes_educcont_nha))[11,"Std. Error"],
                coef(summary(mod_obes_educcont_oth))[11,"Std. Error"],coef(summary(mod_obes_educcont_oth11))[11,"Std. Error"])
vi <- as.vector(t((SE_educ_ob*sqrt(n))^2)) #variance is SD^2, SD is SE*sqrt(n)
nhanes_dat_educ_ob <- data.frame(racecatname,n,ESTIM_educ_ob,vi,SE_educ_ob)
res_educ_ob <- rma(yi=ESTIM_educ_ob,sei=SE_educ_ob, data=nhanes_dat_educ_ob)
res_nonhw_educ_ob <- rma(yi=ESTIM_educ_ob,sei=SE_educ_ob, data=nhanes_dat_educ_ob[!racecatname=='NHW',])
res_nonhb_educ_ob <- rma(yi=ESTIM_educ_ob,sei=SE_educ_ob, data=nhanes_dat_educ_ob[!racecatname=='NHB',])
res_nomex_educ_ob <- rma(yi=ESTIM_educ_ob,sei=SE_educ_ob, data=nhanes_dat_educ_ob[!racecatname=='Mexican-American',])
res_nohis_educ_ob <- rma(yi=ESTIM_educ_ob,sei=SE_educ_ob, data=nhanes_dat_educ_ob[!racecatname=='Other Hispanic',])
res_nonha_educ_ob <- rma(yi=ESTIM_educ_ob,sei=SE_educ_ob, data=nhanes_dat_educ_ob[!racecatname=='NHAsian',])
res_nooth_educ_ob <- rma(yi=ESTIM_educ_ob,sei=SE_educ_ob, data=nhanes_dat_educ_ob[!racecatname=='Other/Multi-Racial including NHA (pre-2011)',])
res_nooth11_educ_ob <- rma(yi=ESTIM_educ_ob,sei=SE_educ_ob, data=nhanes_dat_educ_ob[!racecatname=='Other/Multi-Racial (post-2011)',])
res_nonhwnh_educ_oba <- rma(yi=ESTIM_educ_ob,sei=SE_educ_ob, data=nhanes_dat_educ_ob[!racecatname=='NHW' & !racecatname=='NHAsian',])
res_nhwnha_educ_ob <- rma(yi=ESTIM_educ_ob,sei=SE_educ_ob, data=nhanes_dat_educ_ob[racecatname=='NHW' | racecatname=='NHAsian',])
i2_educ_ob <- rbind(res_educ_ob$I2, res_nonhw_educ_ob$I2, res_nonhb_educ_ob$I2, res_nomex_educ_ob$I2, res_nohis_educ_ob$I2, res_nonha_educ_ob$I2, res_nooth_educ_ob$I2, res_nooth11_educ_ob$I2)
i2_educ_ob
influence(res_educ_ob, progbar=F)

#####interaction - race*educ or inc on diabetes and obesity prevalence#####
mod_diab_raceeducinteract <- svyglm(design = surv_all, diab ~ age + female + racecat + survyr + smoke_status + insur_stability + educ_num*racecat, family=binomial(link=logit))
summary(mod_diab_raceeducinteract)

mod_obes_raceeducinteract <- svyglm(design = surv_all, obese ~ age + female + racecat + survyr + smoke_status + insur_stability + educ_num*racecat, family=binomial(link=logit))
summary(mod_obes_raceeducinteract)

mod_diab_raceinctopovinteract <- svyglm(design = surv_all, diab ~ age + female + racecat + survyr + smoke_status + insur_stability + inc_to_pov*racecat, family=binomial(link=logit))
summary(mod_diab_raceinctopovinteract)

mod_obes_raceinctopovinteract <- svyglm(design = surv_all, obese ~ age + female + racecat + survyr + smoke_status + insur_stability + inc_to_pov*racecat, family=binomial(link=logit))
summary(mod_obes_raceinctopovinteract)

mod_diabob_racesesinteract <- data.frame(exp(mod_diab_raceeducinteract$coefficients),exp(confint(mod_diab_raceeducinteract)),coef(summary(mod_diab_raceeducinteract))[,"Pr(>|t|)"],
                                       exp(mod_obes_raceeducinteract$coefficients),exp(confint(mod_obes_raceeducinteract)),coef(summary(mod_obes_raceeducinteract))[,"Pr(>|t|)"],
                                       exp(mod_diab_raceinctopovinteract$coefficients),exp(confint(mod_diab_raceinctopovinteract)),coef(summary(mod_diab_raceinctopovinteract))[,"Pr(>|t|)"],
                                       exp(mod_obes_raceinctopovinteract$coefficients),exp(confint(mod_obes_raceinctopovinteract)),coef(summary(mod_obes_raceinctopovinteract))[,"Pr(>|t|)"],stringsAsFactors=F)
mod_diabob_racesesinteract
write.csv(mod_diabob_racesesinteract, 'C:/Users/sarac/OneDrive/Documents/Research/Area v Individual SES/NHANES_SES and AoU_SES/nhanes_ses_racesesinteract_models.csv',row.names=TRUE)

#####likelihood ratio tests#####
#need package lmtest
#error: In logLik.svyglm(mod_diab_educcont_all_raceadj) : svyglm not fitted by maximum likelihood.
teststat_diabeduc <- -2 * (as.numeric(logLik(mod_diab_educcont_all_raceadj)) - as.numeric(logLik(mod_diab_raceeducinteract)))
pchisq(teststat_diabeduc,df=1,lower.tail=F)
#Diab:Educ - Wald F; Rao-Scott LRT; or satterthwaite adjusted Wald (aka "working wald")
regTermTest(mod_diab_raceeducinteract, ~educ_num:racecat, null=NULL, df=NULL)
regTermTest(mod_diab_raceeducinteract, ~educ_num:racecat, method="Wald")
regTermTest(mod_diab_raceeducinteract, ~educ_num:racecat, method="LRT", df=Inf, lrt.approximation='satterthwaite')
regTermTest(mod_diab_raceeducinteract, ~educ_num:racecat, method="WorkingWald")
#Diab:Inc - Wald F; Rao-Scott LRT; or satterthwaite adjusted Wald (aka "working wald")
regTermTest(mod_diab_raceinctopovinteract, ~inc_to_pov:racecat, method="Wald")
regTermTest(mod_diab_raceinctopovinteract, ~inc_to_pov:racecat, method="LRT", df=Inf, lrt.approximation='satterthwaite')
regTermTest(mod_diab_raceinctopovinteract, ~inc_to_pov:racecat, method="WorkingWald")
#Ob:Educ - Wald F; Rao-Scott LRT; or satterthwaite adjusted Wald (aka "working wald")
regTermTest(mod_obes_raceeducinteract, ~educ_num:racecat, method="Wald")
regTermTest(mod_obes_raceeducinteract, ~educ_num:racecat, method="LRT", df=Inf, lrt.approximation='satterthwaite')
regTermTest(mod_obes_raceeducinteract, ~educ_num:racecat, method="WorkingWald")
#Ob:Inc - Wald F; Rao-Scott LRT; or satterthwaite adjusted Wald (aka "working wald")
regTermTest(mod_obes_raceinctopovinteract, ~inc_to_pov:racecat, method="Wald")
regTermTest(mod_obes_raceinctopovinteract, ~inc_to_pov:racecat, method="LRT", df=Inf, lrt.approximation='satterthwaite')
regTermTest(mod_obes_raceinctopovinteract, ~inc_to_pov:racecat, method="WorkingWald")
#summary - diab~race*inc not significantly improved but all other interaction terms significantly improve models, esp race*educ interactions



#####INCOME-TO-POV, ALL DISEASE FOREST#####
mod_diab_inctopov_all <- svyglm(design = surv_all, diab ~ age + female + survyr + smoke_status + insur_stability + inc_to_pov, family=binomial(link=logit))
nobs(mod_diab_inctopov_all)
mod_diab_inctopov_all_raceadj <- svyglm(design = surv_all, diab ~ age + female + survyr + smoke_status + insur_stability + inc_to_pov + racecat, family=binomial(link=logit))
mod_diab_inctopov_nhw <- svyglm(design = surv_nhw, diab ~ age + female + survyr + smoke_status + insur_stability + inc_to_pov, family=binomial(link=logit))
mod_diab_inctopov_nhb <- svyglm(design = surv_nhb, diab ~ age + female + survyr + smoke_status + insur_stability + inc_to_pov, family=binomial(link=logit))
mod_diab_inctopov_nha <- svyglm(design = surv_nha, diab ~ age + female + survyr + smoke_status + insur_stability + inc_to_pov, family=binomial(link=logit))
mod_diab_inctopov_mex <- svyglm(design = surv_mex, diab ~ age + female + survyr + smoke_status + insur_stability + inc_to_pov, family=binomial(link=logit))
mod_diab_inctopov_his <- svyglm(design = surv_his, diab ~ age + female + survyr + smoke_status + insur_stability + inc_to_pov, family=binomial(link=logit))
mod_diab_inctopov_oth <- svyglm(design = surv_oth, diab ~ age + female + survyr + smoke_status + insur_stability + inc_to_pov, family=binomial(link=logit))
mod_diab_inctopov_oth11 <- svyglm(design = surv_oth11, diab ~ age + female + survyr + smoke_status + insur_stability + inc_to_pov, family=binomial(link=logit))

mod_diab_inctopov_all_bmi <- svyglm(design = surv_all, diab ~ age + female + survyr + smoke_status + insur_stability + inc_to_pov + bmi, family=binomial(link=logit))
summary(mod_diab_inctopov_all_bmi)
exp(mod_diab_inctopov_all_bmi$coefficients)
mod_diab_inctopov_all_raceadj_bmi <- svyglm(design = surv_all, diab ~ age + female + survyr + smoke_status + insur_stability + inc_to_pov + bmi + racecat, family=binomial(link=logit))
mod_diab_inctopov_nhw_bmi <- svyglm(design = surv_nhw, diab ~ age + female + survyr + smoke_status + insur_stability + inc_to_pov + bmi, family=binomial(link=logit))
mod_diab_inctopov_nhb_bmi <- svyglm(design = surv_nhb, diab ~ age + female + survyr + smoke_status + insur_stability + inc_to_pov + bmi, family=binomial(link=logit))
summary(mod_diab_inctopov_nhb_bmi)
exp(mod_diab_inctopov_nhb_bmi$coefficients)
mod_diab_inctopov_nha_bmi <- svyglm(design = surv_nha, diab ~ age + female + survyr + smoke_status + insur_stability + inc_to_pov + bmi, family=binomial(link=logit))
mod_diab_inctopov_mex_bmi <- svyglm(design = surv_mex, diab ~ age + female + survyr + smoke_status + insur_stability + inc_to_pov + bmi, family=binomial(link=logit))
mod_diab_inctopov_his_bmi <- svyglm(design = surv_his, diab ~ age + female + survyr + smoke_status + insur_stability + inc_to_pov + bmi + bmi, family=binomial(link=logit))
mod_diab_inctopov_oth_bmi <- svyglm(design = surv_oth, diab ~ age + female + survyr + smoke_status + insur_stability + inc_to_pov + bmi, family=binomial(link=logit))
mod_diab_inctopov_oth11_bmi <- svyglm(design = surv_oth11, diab ~ age + female + survyr + smoke_status + insur_stability + inc_to_pov + bmi, family=binomial(link=logit))
b <- cbind(exp(mod_diab_inctopov_all_bmi$coefficients),exp(mod_diab_inctopov_all_raceadj_bmi$coefficients),exp(mod_diab_inctopov_nhw_bmi$coefficients),
           exp(mod_diab_inctopov_nhb_bmi$coefficients),exp(mod_diab_inctopov_mex_bmi$coefficients),exp(mod_diab_inctopov_his_bmi$coefficients),
           exp(mod_diab_inctopov_nha_bmi$coefficients),exp(mod_diab_inctopov_oth_bmi$coefficients),exp(mod_diab_inctopov_oth11_bmi$coefficients))



mod_obes_inctopov_all <- svyglm(design = surv_all, obese ~ age + female + survyr + smoke_status + insur_stability + inc_to_pov, family=binomial(link=logit))
mod_obes_inctopov_all_raceadj <- svyglm(design = surv_all, obese ~ age + female + survyr + smoke_status + insur_stability + inc_to_pov + racecat, family=binomial(link=logit))
mod_obes_inctopov_nhw <- svyglm(design = surv_nhw, obese ~ age + female + survyr + smoke_status + insur_stability + inc_to_pov, family=binomial(link=logit))
mod_obes_inctopov_nhb <- svyglm(design = surv_nhb, obese ~ age + female + survyr + smoke_status + insur_stability + inc_to_pov, family=binomial(link=logit))
mod_obes_inctopov_nha <- svyglm(design = surv_nha, obese ~ age + female + survyr + smoke_status + insur_stability + inc_to_pov, family=binomial(link=logit))
mod_obes_inctopov_mex <- svyglm(design = surv_mex, obese ~ age + female + survyr + smoke_status + insur_stability + inc_to_pov, family=binomial(link=logit))
mod_obes_inctopov_his <- svyglm(design = surv_his, obese ~ age + female + survyr + smoke_status + insur_stability + inc_to_pov, family=binomial(link=logit))
mod_obes_inctopov_oth <- svyglm(design = surv_oth, obese ~ age + female + survyr + smoke_status + insur_stability + inc_to_pov, family=binomial(link=logit))
mod_obes_inctopov_oth11 <- svyglm(design = surv_oth11, obese ~ age + female + survyr + smoke_status + insur_stability + inc_to_pov, family=binomial(link=logit))
#leave-one-out OR: 
surv_nonhw <- subset(surv_all, racecat=='NHW')#svydesign(id = ~SDMVPSU, strata = ~ SDMVSTRA, weights = ~weights, data=nhanes[nhanes$age >= 18 & !nhanes$racecat=='NHW' & !nhanes$insur_stability=='Missing',], nest=T)
mod_obes_inctopov_nonhw <- svyglm(design = surv_nonhw, obese ~ age + female + survyr + smoke_status + insur_stability + inc_to_pov, family=binomial(link=logit))
mod_obese_inctopov_leaveoneout_nonhw <- data.frame(exp(mod_obes_inctopov_all$coefficients),exp(confint(mod_obes_inctopov_all)),coef(summary(mod_obes_inctopov_all))[,"Pr(>|t|)"],
                                                   exp(mod_obes_inctopov_nhw$coefficients),exp(confint(mod_obes_inctopov_nhw)),coef(summary(mod_obes_inctopov_nhw))[,"Pr(>|t|)"],
                                                   exp(mod_obes_inctopov_nonhw$coefficients),exp(confint(mod_obes_inctopov_nonhw)),coef(summary(mod_obes_inctopov_nonhw))[,"Pr(>|t|)"],stringsAsFactors = FALSE)
mod_obese_inctopov_leaveoneout_nonhw[11,]

mod_htn_inctopov_all <- svyglm(design = surv_all, htn ~ age + female + survyr + smoke_status + insur_stability + inc_to_pov, family=binomial(link=logit))
mod_htn_inctopov_all_raceadj <- svyglm(design = surv_all, htn ~ age + female + survyr + smoke_status + insur_stability + inc_to_pov + racecat, family=binomial(link=logit))
mod_htn_inctopov_nhw <- svyglm(design = surv_nhw, htn ~ age + female + survyr + smoke_status + insur_stability + inc_to_pov, family=binomial(link=logit))
mod_htn_inctopov_nhb <- svyglm(design = surv_nhb, htn ~ age + female + survyr + smoke_status + insur_stability + inc_to_pov, family=binomial(link=logit))
mod_htn_inctopov_nha <- svyglm(design = surv_nha, htn ~ age + female + survyr + smoke_status + insur_stability + inc_to_pov, family=binomial(link=logit))
mod_htn_inctopov_mex <- svyglm(design = surv_mex, htn ~ age + female + survyr + smoke_status + insur_stability + inc_to_pov, family=binomial(link=logit))
mod_htn_inctopov_his <- svyglm(design = surv_his, htn ~ age + female + survyr + smoke_status + insur_stability + inc_to_pov, family=binomial(link=logit))
mod_htn_inctopov_oth <- svyglm(design = surv_oth, htn ~ age + female + survyr + smoke_status + insur_stability + inc_to_pov, family=binomial(link=logit))
mod_htn_inctopov_oth11 <- svyglm(design = surv_oth11, htn ~ age + female + survyr + smoke_status + insur_stability + inc_to_pov, family=binomial(link=logit))

mod_cvd_inctopov_all <- svyglm(design = surv_all, cvd ~ age + female + survyr + smoke_status + insur_stability + inc_to_pov, family=binomial(link=logit))
mod_cvd_inctopov_all_raceadj <- svyglm(design = surv_all, cvd ~ age + female + survyr + smoke_status + insur_stability + inc_to_pov + racecat, family=binomial(link=logit))
mod_cvd_inctopov_nhw <- svyglm(design = surv_nhw, cvd ~ age + female + survyr + smoke_status + insur_stability + inc_to_pov, family=binomial(link=logit))
mod_cvd_inctopov_nhb <- svyglm(design = surv_nhb, cvd ~ age + female + survyr + smoke_status + insur_stability + inc_to_pov, family=binomial(link=logit))
mod_cvd_inctopov_nha <- svyglm(design = surv_nha, cvd ~ age + female + survyr + smoke_status + insur_stability + inc_to_pov, family=binomial(link=logit))
mod_cvd_inctopov_mex <- svyglm(design = surv_mex, cvd ~ age + female + survyr + smoke_status + insur_stability + inc_to_pov, family=binomial(link=logit))
mod_cvd_inctopov_his <- svyglm(design = surv_his, cvd ~ age + female + survyr + smoke_status + insur_stability + inc_to_pov, family=binomial(link=logit))
mod_cvd_inctopov_oth <- svyglm(design = surv_oth, cvd ~ age + female + survyr + smoke_status + insur_stability + inc_to_pov, family=binomial(link=logit))
mod_cvd_inctopov_oth11 <- svyglm(design = surv_oth11, cvd ~ age + female + survyr + smoke_status + insur_stability + inc_to_pov, family=binomial(link=logit))

mod_ckd_inctopov_all <- svyglm(design = surv_all, ckd ~ age + female + survyr + smoke_status + insur_stability + inc_to_pov, family=binomial(link=logit))
mod_ckd_inctopov_all_raceadj <- svyglm(design = surv_all, ckd ~ age + female + survyr + smoke_status + insur_stability + inc_to_pov + racecat, family=binomial(link=logit))
mod_ckd_inctopov_nhw <- svyglm(design = surv_nhw, ckd ~ age + female + survyr + smoke_status + insur_stability + inc_to_pov, family=binomial(link=logit))
mod_ckd_inctopov_nhb <- svyglm(design = surv_nhb, ckd ~ age + female + survyr + smoke_status + insur_stability + inc_to_pov, family=binomial(link=logit))
mod_ckd_inctopov_nha <- svyglm(design = surv_nha, ckd ~ age + female + survyr + smoke_status + insur_stability + inc_to_pov, family=binomial(link=logit))
mod_ckd_inctopov_mex <- svyglm(design = surv_mex, ckd ~ age + female + survyr + smoke_status + insur_stability + inc_to_pov, family=binomial(link=logit))
mod_ckd_inctopov_his <- svyglm(design = surv_his, ckd ~ age + female + survyr + smoke_status + insur_stability + inc_to_pov, family=binomial(link=logit))
mod_ckd_inctopov_oth <- svyglm(design = surv_oth, ckd ~ age + female + survyr + smoke_status + insur_stability + inc_to_pov, family=binomial(link=logit))
mod_ckd_inctopov_oth11 <- svyglm(design = surv_oth11, ckd ~ age + female + survyr + smoke_status + insur_stability + inc_to_pov, family=binomial(link=logit))

mod_copd_inctopov_all <- svyglm(design = surv_all, copd ~ age + female + survyr + smoke_status + insur_stability + inc_to_pov, family=binomial(link=logit))
mod_copd_inctopov_all_raceadj <- svyglm(design = surv_all, copd ~ age + female + survyr + smoke_status + insur_stability + inc_to_pov + racecat, family=binomial(link=logit))
mod_copd_inctopov_nhw <- svyglm(design = surv_nhw, copd ~ age + female + survyr + smoke_status + insur_stability + inc_to_pov, family=binomial(link=logit))
mod_copd_inctopov_nhb <- svyglm(design = surv_nhb, copd ~ age + female + survyr + smoke_status + insur_stability + inc_to_pov, family=binomial(link=logit))
mod_copd_inctopov_nha <- svyglm(design = surv_nha, copd ~ age + female + survyr + smoke_status + insur_stability + inc_to_pov, family=binomial(link=logit))
mod_copd_inctopov_mex <- svyglm(design = surv_mex, copd ~ age + female + survyr + smoke_status + insur_stability + inc_to_pov, family=binomial(link=logit))
mod_copd_inctopov_his <- svyglm(design = surv_his, copd ~ age + female + survyr + smoke_status + insur_stability + inc_to_pov, family=binomial(link=logit))
mod_copd_inctopov_oth <- svyglm(design = surv_oth, copd ~ age + female + survyr + smoke_status + insur_stability + inc_to_pov, family=binomial(link=logit))
mod_copd_inctopov_oth11 <- svyglm(design = surv_oth11, copd ~ age + female + survyr + smoke_status + insur_stability + inc_to_pov, family=binomial(link=logit))

mod_mdd_inctopov_all <- svyglm(design = surv_all, mdd ~ age + female + survyr + smoke_status + insur_stability + inc_to_pov, family=binomial(link=logit))
mod_mdd_inctopov_all_raceadj <- svyglm(design = surv_all, mdd ~ age + female + survyr + smoke_status + insur_stability + inc_to_pov + racecat, family=binomial(link=logit))
mod_mdd_inctopov_nhw <- svyglm(design = surv_nhw, mdd ~ age + female + survyr + smoke_status + insur_stability + inc_to_pov, family=binomial(link=logit))
mod_mdd_inctopov_nhb <- svyglm(design = surv_nhb, mdd ~ age + female + survyr + smoke_status + insur_stability + inc_to_pov, family=binomial(link=logit))
mod_mdd_inctopov_nha <- svyglm(design = surv_nha, mdd ~ age + female + survyr + smoke_status + insur_stability + inc_to_pov, family=binomial(link=logit))
mod_mdd_inctopov_mex <- svyglm(design = surv_mex, mdd ~ age + female + survyr + smoke_status + insur_stability + inc_to_pov, family=binomial(link=logit))
mod_mdd_inctopov_his <- svyglm(design = surv_his, mdd ~ age + female + survyr + smoke_status + insur_stability + inc_to_pov, family=binomial(link=logit))
mod_mdd_inctopov_oth <- svyglm(design = surv_oth, mdd ~ age + female + survyr + smoke_status + insur_stability + inc_to_pov, family=binomial(link=logit))
mod_mdd_inctopov_oth11 <- svyglm(design = surv_oth11, mdd ~ age + female + survyr + smoke_status + insur_stability + inc_to_pov, family=binomial(link=logit))

mod_diabob_inctopov_allrace <- data.frame(exp(mod_diab_inctopov_all$coefficients),exp(confint(mod_diab_inctopov_all)),coef(summary(mod_diab_inctopov_all))[,"Pr(>|t|)"],
                                         exp(mod_diab_inctopov_all_raceadj$coefficients)[1:11],exp(confint(mod_diab_inctopov_all_raceadj))[1:11,1:2],coef(summary(mod_diab_inctopov_all_raceadj))[,"Pr(>|t|)"][1:11],
                                         exp(mod_diab_inctopov_nhw$coefficients),exp(confint(mod_diab_inctopov_nhw)),coef(summary(mod_diab_inctopov_nhw))[,"Pr(>|t|)"],
                                         exp(mod_diab_inctopov_nhb$coefficients),exp(confint(mod_diab_inctopov_nhb)),coef(summary(mod_diab_inctopov_nhb))[,"Pr(>|t|)"],
                                         exp(mod_diab_inctopov_mex$coefficients),exp(confint(mod_diab_inctopov_mex)),coef(summary(mod_diab_inctopov_mex))[,"Pr(>|t|)"],
                                         exp(mod_diab_inctopov_his$coefficients),exp(confint(mod_diab_inctopov_his)),coef(summary(mod_diab_inctopov_his))[,"Pr(>|t|)"],
                                         exp(mod_diab_inctopov_nha$coefficients),exp(confint(mod_diab_inctopov_nha)),coef(summary(mod_diab_inctopov_nha))[,"Pr(>|t|)"],
                                         exp(mod_diab_inctopov_oth$coefficients),exp(confint(mod_diab_inctopov_oth)),coef(summary(mod_diab_inctopov_oth))[,"Pr(>|t|)"],
                                         exp(mod_diab_inctopov_oth11$coefficients),exp(confint(mod_diab_inctopov_oth11)),coef(summary(mod_diab_inctopov_oth11))[,"Pr(>|t|)"],
                                         exp(mod_obes_inctopov_all$coefficients),exp(confint(mod_obes_inctopov_all)),coef(summary(mod_obes_inctopov_all))[,"Pr(>|t|)"],
                                         exp(mod_obes_inctopov_all_raceadj$coefficients)[1:11],exp(confint(mod_obes_inctopov_all_raceadj))[1:11,1:2],coef(summary(mod_obes_inctopov_all_raceadj))[,"Pr(>|t|)"][1:11],
                                         exp(mod_obes_inctopov_nhw$coefficients),exp(confint(mod_obes_inctopov_nhw)),coef(summary(mod_obes_inctopov_nhw))[,"Pr(>|t|)"],
                                         exp(mod_obes_inctopov_nhb$coefficients),exp(confint(mod_obes_inctopov_nhb)),coef(summary(mod_obes_inctopov_nhb))[,"Pr(>|t|)"],
                                         exp(mod_obes_inctopov_mex$coefficients),exp(confint(mod_obes_inctopov_mex)),coef(summary(mod_obes_inctopov_mex))[,"Pr(>|t|)"],
                                         exp(mod_obes_inctopov_his$coefficients),exp(confint(mod_obes_inctopov_his)),coef(summary(mod_obes_inctopov_his))[,"Pr(>|t|)"],
                                         exp(mod_obes_inctopov_nha$coefficients),exp(confint(mod_obes_inctopov_nha)),coef(summary(mod_obes_inctopov_nha))[,"Pr(>|t|)"],
                                         exp(mod_obes_inctopov_oth$coefficients),exp(confint(mod_obes_inctopov_oth)),coef(summary(mod_obes_inctopov_oth))[,"Pr(>|t|)"],
                                         exp(mod_obes_inctopov_oth11$coefficients),exp(confint(mod_obes_inctopov_oth11)),coef(summary(mod_obes_inctopov_oth11))[,"Pr(>|t|)"],stringsAsFactors = F)
write.csv(mod_diabob_inctopov_allrace, 'C:/Users/sarac/OneDrive/Documents/Research/Area v Individual SES/NHANES_SES and AoU_SES/nhanes_ses_inc_diabob_models.csv',row.names=TRUE)


mod_alldx_inctopov_allrace <- data.frame(exp(mod_diab_inctopov_all$coefficients),exp(confint(mod_diab_inctopov_all)),coef(summary(mod_diab_inctopov_all))[,"Pr(>|t|)"],
                                         exp(mod_diab_inctopov_all_raceadj$coefficients)[1:11],exp(confint(mod_diab_inctopov_all_raceadj))[1:11,1:2],coef(summary(mod_diab_inctopov_all_raceadj))[,"Pr(>|t|)"][1:11],
                                         exp(mod_diab_inctopov_nhw$coefficients),exp(confint(mod_diab_inctopov_nhw)),coef(summary(mod_diab_inctopov_nhw))[,"Pr(>|t|)"],
                                         exp(mod_diab_inctopov_nhb$coefficients),exp(confint(mod_diab_inctopov_nhb)),coef(summary(mod_diab_inctopov_nhb))[,"Pr(>|t|)"],
                                         exp(mod_diab_inctopov_mex$coefficients),exp(confint(mod_diab_inctopov_mex)),coef(summary(mod_diab_inctopov_mex))[,"Pr(>|t|)"],
                                         exp(mod_diab_inctopov_his$coefficients),exp(confint(mod_diab_inctopov_his)),coef(summary(mod_diab_inctopov_his))[,"Pr(>|t|)"],
                                         exp(mod_diab_inctopov_nha$coefficients),exp(confint(mod_diab_inctopov_nha)),coef(summary(mod_diab_inctopov_nha))[,"Pr(>|t|)"],
                                         exp(mod_diab_inctopov_oth$coefficients),exp(confint(mod_diab_inctopov_oth)),coef(summary(mod_diab_inctopov_oth))[,"Pr(>|t|)"],
                                         exp(mod_diab_inctopov_oth11$coefficients),exp(confint(mod_diab_inctopov_oth11)),coef(summary(mod_diab_inctopov_oth11))[,"Pr(>|t|)"],
                                         exp(mod_obes_inctopov_all$coefficients),exp(confint(mod_obes_inctopov_all)),coef(summary(mod_obes_inctopov_all))[,"Pr(>|t|)"],
                                         exp(mod_obes_inctopov_all_raceadj$coefficients)[1:11],exp(confint(mod_obes_inctopov_all_raceadj))[1:11,1:2],coef(summary(mod_obes_inctopov_all_raceadj))[,"Pr(>|t|)"][1:11],
                                         exp(mod_obes_inctopov_nhw$coefficients),exp(confint(mod_obes_inctopov_nhw)),coef(summary(mod_obes_inctopov_nhw))[,"Pr(>|t|)"],
                                         exp(mod_obes_inctopov_nhb$coefficients),exp(confint(mod_obes_inctopov_nhb)),coef(summary(mod_obes_inctopov_nhb))[,"Pr(>|t|)"],
                                         exp(mod_obes_inctopov_mex$coefficients),exp(confint(mod_obes_inctopov_mex)),coef(summary(mod_obes_inctopov_mex))[,"Pr(>|t|)"],
                                         exp(mod_obes_inctopov_his$coefficients),exp(confint(mod_obes_inctopov_his)),coef(summary(mod_obes_inctopov_his))[,"Pr(>|t|)"],
                                         exp(mod_obes_inctopov_nha$coefficients),exp(confint(mod_obes_inctopov_nha)),coef(summary(mod_obes_inctopov_nha))[,"Pr(>|t|)"],
                                         exp(mod_obes_inctopov_oth$coefficients),exp(confint(mod_obes_inctopov_oth)),coef(summary(mod_obes_inctopov_oth))[,"Pr(>|t|)"],
                                         exp(mod_obes_inctopov_oth11$coefficients),exp(confint(mod_obes_inctopov_oth11)),coef(summary(mod_obes_inctopov_oth11))[,"Pr(>|t|)"],
                                         exp(mod_htn_inctopov_all$coefficients),exp(confint(mod_htn_inctopov_all)),coef(summary(mod_htn_inctopov_all))[,"Pr(>|t|)"],
                                         exp(mod_htn_inctopov_all_raceadj$coefficients)[1:11],exp(confint(mod_htn_inctopov_all_raceadj))[1:11,1:2],coef(summary(mod_htn_inctopov_all_raceadj))[,"Pr(>|t|)"][1:11],
                                         exp(mod_htn_inctopov_nhw$coefficients),exp(confint(mod_htn_inctopov_nhw)),coef(summary(mod_htn_inctopov_nhw))[,"Pr(>|t|)"],
                                         exp(mod_htn_inctopov_nhb$coefficients),exp(confint(mod_htn_inctopov_nhb)),coef(summary(mod_htn_inctopov_nhb))[,"Pr(>|t|)"],
                                         exp(mod_htn_inctopov_mex$coefficients),exp(confint(mod_htn_inctopov_mex)),coef(summary(mod_htn_inctopov_mex))[,"Pr(>|t|)"],
                                         exp(mod_htn_inctopov_his$coefficients),exp(confint(mod_htn_inctopov_his)),coef(summary(mod_htn_inctopov_his))[,"Pr(>|t|)"],
                                         exp(mod_htn_inctopov_nha$coefficients),exp(confint(mod_htn_inctopov_nha)),coef(summary(mod_htn_inctopov_nha))[,"Pr(>|t|)"],                                         exp(mod_htn_inctopov_oth$coefficients),exp(confint(mod_htn_inctopov_oth)),coef(summary(mod_htn_inctopov_oth))[,"Pr(>|t|)"],
                                         exp(mod_htn_inctopov_oth11$coefficients),exp(confint(mod_htn_inctopov_oth11)),coef(summary(mod_htn_inctopov_oth11))[,"Pr(>|t|)"],
                                         exp(mod_cvd_inctopov_all$coefficients),exp(confint(mod_cvd_inctopov_all)),coef(summary(mod_cvd_inctopov_all))[,"Pr(>|t|)"],
                                         exp(mod_cvd_inctopov_all_raceadj$coefficients)[1:11],exp(confint(mod_cvd_inctopov_all_raceadj))[1:11,1:2],coef(summary(mod_cvd_inctopov_all_raceadj))[,"Pr(>|t|)"][1:11],
                                         exp(mod_cvd_inctopov_nhw$coefficients),exp(confint(mod_cvd_inctopov_nhw)),coef(summary(mod_cvd_inctopov_nhw))[,"Pr(>|t|)"],
                                         exp(mod_cvd_inctopov_nhb$coefficients),exp(confint(mod_cvd_inctopov_nhb)),coef(summary(mod_cvd_inctopov_nhb))[,"Pr(>|t|)"],
                                         exp(mod_cvd_inctopov_mex$coefficients),exp(confint(mod_cvd_inctopov_mex)),coef(summary(mod_cvd_inctopov_mex))[,"Pr(>|t|)"],
                                         exp(mod_cvd_inctopov_his$coefficients),exp(confint(mod_cvd_inctopov_his)),coef(summary(mod_cvd_inctopov_his))[,"Pr(>|t|)"],
                                         exp(mod_cvd_inctopov_nha$coefficients),exp(confint(mod_cvd_inctopov_nha)),coef(summary(mod_cvd_inctopov_nha))[,"Pr(>|t|)"],
                                         exp(mod_cvd_inctopov_oth$coefficients),exp(confint(mod_cvd_inctopov_oth)),coef(summary(mod_cvd_inctopov_oth))[,"Pr(>|t|)"],
                                         exp(mod_cvd_inctopov_oth11$coefficients),exp(confint(mod_cvd_inctopov_oth11)),coef(summary(mod_cvd_inctopov_oth11))[,"Pr(>|t|)"],
                                         exp(mod_ckd_inctopov_all$coefficients),exp(confint(mod_ckd_inctopov_all)),coef(summary(mod_ckd_inctopov_all))[,"Pr(>|t|)"],
                                         exp(mod_ckd_inctopov_all_raceadj$coefficients)[1:11],exp(confint(mod_ckd_inctopov_all_raceadj))[1:11,1:2],coef(summary(mod_ckd_inctopov_all_raceadj))[,"Pr(>|t|)"][1:11],
                                         exp(mod_ckd_inctopov_nhw$coefficients),exp(confint(mod_ckd_inctopov_nhw)),coef(summary(mod_ckd_inctopov_nhw))[,"Pr(>|t|)"],
                                         exp(mod_ckd_inctopov_nhb$coefficients),exp(confint(mod_ckd_inctopov_nhb)),coef(summary(mod_ckd_inctopov_nhb))[,"Pr(>|t|)"],
                                         exp(mod_ckd_inctopov_mex$coefficients),exp(confint(mod_ckd_inctopov_mex)),coef(summary(mod_ckd_inctopov_mex))[,"Pr(>|t|)"],
                                         exp(mod_ckd_inctopov_his$coefficients),exp(confint(mod_ckd_inctopov_his)),coef(summary(mod_ckd_inctopov_his))[,"Pr(>|t|)"],
                                         exp(mod_ckd_inctopov_nha$coefficients),exp(confint(mod_ckd_inctopov_nha)),coef(summary(mod_ckd_inctopov_nha))[,"Pr(>|t|)"],
                                         exp(mod_ckd_inctopov_oth$coefficients),exp(confint(mod_ckd_inctopov_oth)),coef(summary(mod_ckd_inctopov_oth))[,"Pr(>|t|)"],
                                         exp(mod_ckd_inctopov_oth11$coefficients),exp(confint(mod_ckd_inctopov_oth11)),coef(summary(mod_ckd_inctopov_oth11))[,"Pr(>|t|)"],
                                         exp(mod_copd_inctopov_all$coefficients),exp(confint(mod_copd_inctopov_all)),coef(summary(mod_copd_inctopov_all))[,"Pr(>|t|)"],
                                         exp(mod_copd_inctopov_all_raceadj$coefficients)[1:11],exp(confint(mod_copd_inctopov_all_raceadj))[1:11,1:2],coef(summary(mod_copd_inctopov_all_raceadj))[,"Pr(>|t|)"][1:11],
                                         exp(mod_copd_inctopov_nhw$coefficients),exp(confint(mod_copd_inctopov_nhw)),coef(summary(mod_copd_inctopov_nhw))[,"Pr(>|t|)"],
                                         exp(mod_copd_inctopov_nhb$coefficients),exp(confint(mod_copd_inctopov_nhb)),coef(summary(mod_copd_inctopov_nhb))[,"Pr(>|t|)"],
                                         exp(mod_copd_inctopov_mex$coefficients),exp(confint(mod_copd_inctopov_mex)),coef(summary(mod_copd_inctopov_mex))[,"Pr(>|t|)"],
                                         exp(mod_copd_inctopov_his$coefficients),exp(confint(mod_copd_inctopov_his)),coef(summary(mod_copd_inctopov_his))[,"Pr(>|t|)"],
                                         exp(mod_copd_inctopov_nha$coefficients),exp(confint(mod_copd_inctopov_nha)),coef(summary(mod_copd_inctopov_nha))[,"Pr(>|t|)"],
                                         exp(mod_copd_inctopov_oth$coefficients),exp(confint(mod_copd_inctopov_oth)),coef(summary(mod_copd_inctopov_oth))[,"Pr(>|t|)"],
                                         exp(mod_copd_inctopov_oth11$coefficients),exp(confint(mod_copd_inctopov_oth11)),coef(summary(mod_copd_inctopov_oth11))[,"Pr(>|t|)"],
                                         exp(mod_mdd_inctopov_all$coefficients),exp(confint(mod_mdd_inctopov_all)),coef(summary(mod_mdd_inctopov_all))[,"Pr(>|t|)"],
                                         exp(mod_mdd_inctopov_all_raceadj$coefficients)[1:11],exp(confint(mod_mdd_inctopov_all_raceadj))[1:11,1:2],coef(summary(mod_mdd_inctopov_all_raceadj))[,"Pr(>|t|)"][1:11],
                                         exp(mod_mdd_inctopov_nhw$coefficients),exp(confint(mod_mdd_inctopov_nhw)),coef(summary(mod_mdd_inctopov_nhw))[,"Pr(>|t|)"],
                                         exp(mod_mdd_inctopov_nhb$coefficients),exp(confint(mod_mdd_inctopov_nhb)),coef(summary(mod_mdd_inctopov_nhb))[,"Pr(>|t|)"],
                                         exp(mod_mdd_inctopov_mex$coefficients),exp(confint(mod_mdd_inctopov_mex)),coef(summary(mod_mdd_inctopov_mex))[,"Pr(>|t|)"],
                                         exp(mod_mdd_inctopov_his$coefficients),exp(confint(mod_mdd_inctopov_his)),coef(summary(mod_mdd_inctopov_his))[,"Pr(>|t|)"],
                                         exp(mod_mdd_inctopov_nha$coefficients),exp(confint(mod_mdd_inctopov_nha)),coef(summary(mod_mdd_inctopov_nha))[,"Pr(>|t|)"],
                                         exp(mod_mdd_inctopov_oth$coefficients),exp(confint(mod_mdd_inctopov_oth)),coef(summary(mod_mdd_inctopov_oth))[,"Pr(>|t|)"],
                                         exp(mod_mdd_inctopov_oth11$coefficients),exp(confint(mod_mdd_inctopov_oth11)),coef(summary(mod_mdd_inctopov_oth11))[,"Pr(>|t|)"],stringsAsFactors = FALSE)
mod_alldx_inctopov_allrace_transpose <- t(mod_alldx_inctopov_allrace)


tiff(file='C:/Users/sarac/OneDrive/Documents/Research/Area v Individual SES/NHANES_SES and AoU_SES/nhanes_forest_indiv_inctopov_stratandnonstrat_alldx.tiff', width=8, height=5, units='in', res=300)
forestplot(c('Type 2 Diabetes','Obesity','Hypertension','CVD','CKD','COPD','Depression'), 
           mean = cbind(t(mod_alldx_inctopov_allrace[11,c(1,37,73,109,145,181,217)]),t(mod_alldx_inctopov_allrace[11,c(5,41,77,113,149,185,221)]),
                        t(mod_alldx_inctopov_allrace[11,c(9,45,81,117,153,189,225)]),t(mod_alldx_inctopov_allrace[11,c(13,49,85,121,157,193,229)]),
                        t(mod_alldx_inctopov_allrace[11,c(17,53,89,125,161,197,233)]),t(mod_alldx_inctopov_allrace[11,c(21,57,93,129,165,201,237)]),
                        t(mod_alldx_inctopov_allrace[11,c(25,61,97,133,169,205,241)]),t(mod_alldx_inctopov_allrace[11,c(29,65,101,137,173,209,245)]),
                        t(mod_alldx_inctopov_allrace[11,c(33,69,105,141,177,213,249)])),
           lower = cbind(t(mod_alldx_inctopov_allrace[11,c(2,38,74,110,146,182,218)]),t(mod_alldx_inctopov_allrace[11,c(6,42,78,114,150,186,222)]),
                         t(mod_alldx_inctopov_allrace[11,c(10,46,82,118,154,190,226)]),t(mod_alldx_inctopov_allrace[11,c(14,50,86,122,158,194,230)]),
                         t(mod_alldx_inctopov_allrace[11,c(18,54,90,126,162,198,234)]),t(mod_alldx_inctopov_allrace[11,c(22,58,94,130,166,202,238)]),
                         t(mod_alldx_inctopov_allrace[11,c(26,62,98,134,170,206,242)]),t(mod_alldx_inctopov_allrace[11,c(30,66,102,138,174,210,246)]),
                         t(mod_alldx_inctopov_allrace[11,c(34,70,106,142,178,214,250)])),
           upper = cbind(t(mod_alldx_inctopov_allrace[11,c(3,39,75,111,147,183,219)]),t(mod_alldx_inctopov_allrace[11,c(7,43,79,115,151,187,223)]),
                         t(mod_alldx_inctopov_allrace[11,c(11,47,83,119,155,191,227)]),t(mod_alldx_inctopov_allrace[11,c(15,51,87,123,159,195,231)]),
                         t(mod_alldx_inctopov_allrace[11,c(19,55,91,127,163,199,235)]),t(mod_alldx_inctopov_allrace[11,c(23,59,95,131,167,203,239)]),
                         t(mod_alldx_inctopov_allrace[11,c(27,63,99,135,171,207,243)]),t(mod_alldx_inctopov_allrace[11,c(31,67,103,139,175,211,247)]),
                         t(mod_alldx_inctopov_allrace[11,c(35,71,107,143,179,215,251)])),
           col = fpColors(box = c('gray23','gray57','dodgerblue4','darkslategray3','palegreen3','lightgoldenrod','salmon1','indianred3','mediumpurple4')),
           legend=c("All","All, Race-adjusted","NHW","NHB","Mexican-American","Other Hispanic","NHA","Other/Multi-Racial \n including NHA (pre-2011)","Other/Multi-Racial \n (post-2011)"),
           legend_args = fpLegend(pos = "right"),#list(x = 0.5, y = -0.2), gp = gpar(cex=0.9,col = "#CCCCCC", fill = "#F9F9F9")),
           #legend=c("All","All, Race-adjusted",rownames(table(nhanes$racecat))),#c("All","All-Race-adjusted","NHW","NHB","NHA","Mexican-American","Other Hispanic","Other/Multiracial"),
           #legend_args = fpLegend(pos = list(x = 0.9, y = 0.9), gp = gpar(cex=0.7,col = "#CCCCCC", fill = "#F9F9F9")),
           xlab = "OR (95% CI)",
           title = 'Adjusted OR for Continuous Income-to-Poverty Ratio on All Diseases, \n Overall and Stratified by Race/Ethnicity',
           txt_gp = fpTxtGp(title=gpar(cex=0.8),xlab=gpar(cex=0.8), ticks=gpar(cex=0.8), label=gpar(cex=0.8)),
           vertices=TRUE,
           line.margin=0.25,
           graph.pos=2,
           clip=c(0.6,1.4), xlim=c(0.6,1.4), xticks=c(0.6,0.8,1.0,1.2,1.4), xlog=TRUE, grid=structure(c(0.6,0.8,1.0,1.2,1.4)))
dev.off()

tiff(file='C:/Users/sarac/OneDrive/Documents/Research/Area v Individual SES/NHANES_SES and AoU_SES/Fig2B_nhanes_forest_indiv_inctopov_stratandnonstrat.tiff', width=8, height=5, units='in', res=300)
forestplot(c('Type 2 Diabetes','Obesity'), 
           mean = cbind(t(mod_alldx_inctopov_allrace[11,c(1,37)]),t(mod_alldx_inctopov_allrace[11,c(5,41)]),
                        t(mod_alldx_inctopov_allrace[11,c(9,45)]),t(mod_alldx_inctopov_allrace[11,c(13,49)]),
                        t(mod_alldx_inctopov_allrace[11,c(17,53)]),t(mod_alldx_inctopov_allrace[11,c(21,57)]),
                        t(mod_alldx_inctopov_allrace[11,c(25,61)]),t(mod_alldx_inctopov_allrace[11,c(29,65)]),
                        t(mod_alldx_inctopov_allrace[11,c(33,69)])),
           lower = cbind(t(mod_alldx_inctopov_allrace[11,c(2,38)]),t(mod_alldx_inctopov_allrace[11,c(6,42)]),
                         t(mod_alldx_inctopov_allrace[11,c(10,46)]),t(mod_alldx_inctopov_allrace[11,c(14,50)]),
                         t(mod_alldx_inctopov_allrace[11,c(18,54)]),t(mod_alldx_inctopov_allrace[11,c(22,58)]),
                         t(mod_alldx_inctopov_allrace[11,c(26,62)]),t(mod_alldx_inctopov_allrace[11,c(30,66)]),
                         t(mod_alldx_inctopov_allrace[11,c(34,68)])),
           upper = cbind(t(mod_alldx_inctopov_allrace[11,c(3,39)]),t(mod_alldx_inctopov_allrace[11,c(7,43)]),
                         t(mod_alldx_inctopov_allrace[11,c(11,47)]),t(mod_alldx_inctopov_allrace[11,c(15,51)]),
                         t(mod_alldx_inctopov_allrace[11,c(19,55)]),t(mod_alldx_inctopov_allrace[11,c(23,59)]),
                         t(mod_alldx_inctopov_allrace[11,c(27,63)]),t(mod_alldx_inctopov_allrace[11,c(31,67)]),
                         t(mod_alldx_inctopov_allrace[11,c(35,71)])),
           col = fpColors(box = c('gray23','gray57','dodgerblue4','darkslategray3','palegreen3','lightgoldenrod','salmon1','indianred3','mediumpurple4')),
           legend=c("All","All, Race-adjusted","NHW","NHB","Mexican-American","Other Hispanic","NHA","Other/Multi-Racial \n including NHA (pre-2011)","Other/Multi-Racial \n (post-2011)"),
           legend_args = fpLegend(pos = "right"),#list(x = 0.5, y = -0.2), gp = gpar(cex=0.9,col = "#CCCCCC", fill = "#F9F9F9")),
           #legend=c("All","All, Race-adjusted",rownames(table(nhanes$racecat))),#c("All","All-Race-adjusted","NHW","NHB","NHA","Mexican-American","Other Hispanic","Other/Multiracial"),
           #legend_args = fpLegend(pos = list(x = 0.9, y = 0.9), gp = gpar(cex=0.9,col = "#CCCCCC", fill = "#F9F9F9")),
           xlab = "OR (95% CI)",
           title = 'Adjusted OR for Income-to-Poverty Ratio on T2D and Obesity Prevalence, \n Overall and Stratified by Race/Ethnicity',
           txt_gp = fpTxtGp(title=gpar(cex=1),xlab=gpar(cex=1), ticks=gpar(cex=1), label=gpar(cex=1)),
           vertices=TRUE,
           line.margin=0.25,
           graph.pos=2,
           clip=c(0.75,1.2), xlim=c(0.75,1.2), xlog=TRUE, grid=structure(c(0.75,0.8,0.9,1.0,1.1,1.2)),
           xticks=c(-0.287682072451786,-0.223143551314213,-0.105360515657828,0,0.0953101798043265,0.182321556793958))
dev.off()

#t2d (bmi-adj) and obesity only forest
mod_alldx_inctopov_allrace_bmiadj <- data.frame(exp(mod_diab_inctopov_all_bmi$coefficients)[1:11],exp(confint(mod_diab_inctopov_all_bmi))[1:11,1:2],coef(summary(mod_diab_inctopov_all_bmi))[,"Pr(>|t|)"][1:11],
                                                exp(mod_diab_inctopov_all_raceadj_bmi$coefficients)[1:11],exp(confint(mod_diab_inctopov_all_raceadj_bmi))[1:11,1:2],coef(summary(mod_diab_inctopov_all_raceadj_bmi))[,"Pr(>|t|)"][1:11],
                                                exp(mod_diab_inctopov_nhw_bmi$coefficients)[1:11],exp(confint(mod_diab_inctopov_nhw_bmi))[1:11,1:2],coef(summary(mod_diab_inctopov_nhw_bmi))[,"Pr(>|t|)"][1:11],
                                                exp(mod_diab_inctopov_nhb_bmi$coefficients)[1:11],exp(confint(mod_diab_inctopov_nhb_bmi))[1:11,1:2],coef(summary(mod_diab_inctopov_nhb_bmi))[,"Pr(>|t|)"][1:11],
                                                exp(mod_diab_inctopov_mex_bmi$coefficients)[1:11],exp(confint(mod_diab_inctopov_mex_bmi))[1:11,1:2],coef(summary(mod_diab_inctopov_mex_bmi))[,"Pr(>|t|)"][1:11],
                                                exp(mod_diab_inctopov_his_bmi$coefficients)[1:11],exp(confint(mod_diab_inctopov_his_bmi))[1:11,1:2],coef(summary(mod_diab_inctopov_his_bmi))[,"Pr(>|t|)"][1:11],
                                                exp(mod_diab_inctopov_nha_bmi$coefficients)[1:11],exp(confint(mod_diab_inctopov_nha_bmi))[1:11,1:2],coef(summary(mod_diab_inctopov_nha_bmi))[,"Pr(>|t|)"][1:11],
                                                exp(mod_diab_inctopov_oth_bmi$coefficients)[1:11],exp(confint(mod_diab_inctopov_oth_bmi))[1:11,1:2],coef(summary(mod_diab_inctopov_oth_bmi))[,"Pr(>|t|)"][1:11],
                                                exp(mod_diab_inctopov_oth11_bmi$coefficients)[1:11],exp(confint(mod_diab_inctopov_oth11_bmi))[1:11,1:2],coef(summary(mod_diab_inctopov_oth11_bmi))[,"Pr(>|t|)"][1:11],
                                                exp(mod_obes_inctopov_all$coefficients),exp(confint(mod_obes_inctopov_all)),coef(summary(mod_obes_inctopov_all))[,"Pr(>|t|)"],
                                                exp(mod_obes_inctopov_all_raceadj$coefficients)[1:11],exp(confint(mod_obes_inctopov_all_raceadj))[1:11,1:2],coef(summary(mod_obes_inctopov_all_raceadj))[,"Pr(>|t|)"][1:11],
                                                exp(mod_obes_inctopov_nhw$coefficients),exp(confint(mod_obes_inctopov_nhw)),coef(summary(mod_obes_inctopov_nhw))[,"Pr(>|t|)"],
                                                exp(mod_obes_inctopov_nhb$coefficients),exp(confint(mod_obes_inctopov_nhb)),coef(summary(mod_obes_inctopov_nhb))[,"Pr(>|t|)"],
                                                exp(mod_obes_inctopov_mex$coefficients),exp(confint(mod_obes_inctopov_mex)),coef(summary(mod_obes_inctopov_mex))[,"Pr(>|t|)"],
                                                exp(mod_obes_inctopov_his$coefficients),exp(confint(mod_obes_inctopov_his)),coef(summary(mod_obes_inctopov_his))[,"Pr(>|t|)"],
                                                exp(mod_obes_inctopov_nha$coefficients),exp(confint(mod_obes_inctopov_nha)),coef(summary(mod_obes_inctopov_nha))[,"Pr(>|t|)"],
                                                exp(mod_obes_inctopov_oth$coefficients),exp(confint(mod_obes_inctopov_oth)),coef(summary(mod_obes_inctopov_oth))[,"Pr(>|t|)"],
                                                exp(mod_obes_inctopov_oth11$coefficients),exp(confint(mod_obes_inctopov_oth11)),coef(summary(mod_obes_inctopov_oth11))[,"Pr(>|t|)"],stringsAsFactors = FALSE)
dim(mod_alldx_inctopov_allrace_bmiadj)
tiff(file='C:/Users/sarac/OneDrive/Documents/Research/Area v Individual SES/NHANES_SES and AoU_SES/nhanes_forest_indiv_inctopov_stratandnonstrat_t2dbmiadj.tiff', width=8, height=5, units='in', res=300)
par(xpd=T)
forestplot(c('Type 2 Diabetes','Obesity'), 
           mean = cbind(t(mod_alldx_inctopov_allrace_bmiadj[11,c(1,37)]),t(mod_alldx_inctopov_allrace_bmiadj[11,c(5,41)]),
                        t(mod_alldx_inctopov_allrace_bmiadj[11,c(9,45)]),t(mod_alldx_inctopov_allrace_bmiadj[11,c(13,49)]),
                        t(mod_alldx_inctopov_allrace_bmiadj[11,c(17,53)]),t(mod_alldx_inctopov_allrace_bmiadj[11,c(21,57)]),
                        t(mod_alldx_inctopov_allrace_bmiadj[11,c(25,61)]),t(mod_alldx_inctopov_allrace_bmiadj[11,c(29,65)]),
                        t(mod_alldx_inctopov_allrace_bmiadj[11,c(33,69)])),
           lower = cbind(t(mod_alldx_inctopov_allrace_bmiadj[11,c(2,38)]),t(mod_alldx_inctopov_allrace_bmiadj[11,c(6,42)]),
                         t(mod_alldx_inctopov_allrace_bmiadj[11,c(10,46)]),t(mod_alldx_inctopov_allrace_bmiadj[11,c(14,50)]),
                         t(mod_alldx_inctopov_allrace_bmiadj[11,c(18,54)]),t(mod_alldx_inctopov_allrace_bmiadj[11,c(22,58)]),
                         t(mod_alldx_inctopov_allrace_bmiadj[11,c(26,62)]),t(mod_alldx_inctopov_allrace_bmiadj[11,c(30,66)]),
                         t(mod_alldx_inctopov_allrace_bmiadj[11,c(34,68)])),
           upper = cbind(t(mod_alldx_inctopov_allrace_bmiadj[11,c(3,39)]),t(mod_alldx_inctopov_allrace_bmiadj[11,c(7,43)]),
                         t(mod_alldx_inctopov_allrace_bmiadj[11,c(11,47)]),t(mod_alldx_inctopov_allrace_bmiadj[11,c(15,51)]),
                         t(mod_alldx_inctopov_allrace_bmiadj[11,c(19,55)]),t(mod_alldx_inctopov_allrace_bmiadj[11,c(23,59)]),
                         t(mod_alldx_inctopov_allrace_bmiadj[11,c(27,63)]),t(mod_alldx_inctopov_allrace_bmiadj[11,c(31,67)]),
                         t(mod_alldx_inctopov_allrace_bmiadj[11,c(35,71)])),
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
#####t2d - bmi mediation - income#####
bmi_med_inc_all <- svyglm(design=surv_all, bmi ~ age + female + survyr + smoke_status + insur_stability + inc_to_pov)
bmi_med_inc_all_raceadj <- svyglm(design=surv_all, bmi ~ age + female + survyr + smoke_status + insur_stability + inc_to_pov + racecat)
bmi_med_inc_nhw <- svyglm(design=surv_nhw, bmi ~ age + female + survyr + smoke_status + insur_stability + inc_to_pov)
bmi_med_inc_nhb <- svyglm(design=surv_nhb, bmi ~ age + female + survyr + smoke_status + insur_stability + inc_to_pov)
bmi_med_inc_mex <- svyglm(design=surv_mex, bmi ~ age + female + survyr + smoke_status + insur_stability + inc_to_pov)
bmi_med_inc_his <- svyglm(design=surv_his, bmi ~ age + female + survyr + smoke_status + insur_stability + inc_to_pov)
bmi_med_inc_nha <- svyglm(design=surv_nha, bmi ~ age + female + survyr + smoke_status + insur_stability + inc_to_pov)
bmi_med_inc_oth <- svyglm(design=surv_oth, bmi ~ age + female + survyr + smoke_status + insur_stability + inc_to_pov)
bmi_med_inc_oth11 <- svyglm(design=surv_oth11, bmi ~ age + female + survyr + smoke_status + insur_stability + inc_to_pov)

t2d_propmediated_bmi_inc <- rbind(cbind('All',mod_diab_inctopov_all_bmi$coefficients[11] + bmi_med_inc_all$coefficients[11]*mod_diab_inctopov_all_bmi$coefficients[12], mod_diab_inctopov_all_bmi$coefficients[11], bmi_med_inc_all$coefficients[11]*mod_diab_inctopov_all_bmi$coefficients[12], bmi_med_inc_all$coefficients[11]*mod_diab_inctopov_all_bmi$coefficients[12]/(mod_diab_inctopov_all_bmi$coefficients[11] + bmi_med_inc_all$coefficients[11]*mod_diab_inctopov_all_bmi$coefficients[12])),
                                   cbind('All-Race-adjusted',mod_diab_inctopov_all_raceadj_bmi$coefficients[11] + bmi_med_inc_all_raceadj$coefficients[11]*mod_diab_inctopov_all_raceadj_bmi$coefficients[12], mod_diab_inctopov_all_raceadj_bmi$coefficients[11], bmi_med_inc_all_raceadj$coefficients[11]*mod_diab_inctopov_all_bmi$coefficients[12], bmi_med_inc_all$coefficients[11]*mod_diab_inctopov_all_raceadj_bmi$coefficients[12]/(mod_diab_inctopov_all_raceadj_bmi$coefficients[11] + bmi_med_inc_all_raceadj$coefficients[11]*mod_diab_inctopov_all_raceadj_bmi$coefficients[12])),
                                   cbind('NHW',mod_diab_inctopov_nhw_bmi$coefficients[11] + bmi_med_inc_nhw$coefficients[11]*mod_diab_inctopov_nhw_bmi$coefficients[12], mod_diab_inctopov_nhw_bmi$coefficients[11], bmi_med_inc_nhw$coefficients[11]*mod_diab_inctopov_nhw_bmi$coefficients[12], bmi_med_inc_nhw$coefficients[11]*mod_diab_inctopov_nhw_bmi$coefficients[12]/(mod_diab_inctopov_nhw_bmi$coefficients[11] + bmi_med_inc_nhw$coefficients[11]*mod_diab_inctopov_nhw_bmi$coefficients[12])),
                                   cbind('NHB',mod_diab_inctopov_nhb_bmi$coefficients[11] + bmi_med_inc_nhb$coefficients[11]*mod_diab_inctopov_nhb_bmi$coefficients[12], mod_diab_inctopov_nhb_bmi$coefficients[11], bmi_med_inc_nhb$coefficients[11]*mod_diab_inctopov_nhb_bmi$coefficients[12], bmi_med_inc_nhb$coefficients[11]*mod_diab_inctopov_nhb_bmi$coefficients[12]/(mod_diab_inctopov_nhb_bmi$coefficients[11] + bmi_med_inc_nhb$coefficients[11]*mod_diab_inctopov_nhw_bmi$coefficients[12])),
                                   cbind('MEX',mod_diab_inctopov_mex_bmi$coefficients[11] + bmi_med_inc_mex$coefficients[11]*mod_diab_inctopov_mex_bmi$coefficients[12], mod_diab_inctopov_mex_bmi$coefficients[11], bmi_med_inc_mex$coefficients[11]*mod_diab_inctopov_mex_bmi$coefficients[12], bmi_med_inc_mex$coefficients[11]*mod_diab_inctopov_mex_bmi$coefficients[12]/(mod_diab_inctopov_mex_bmi$coefficients[11] + bmi_med_inc_mex$coefficients[11]*mod_diab_inctopov_mex_bmi$coefficients[12])),
                                   cbind('HIS',mod_diab_inctopov_his_bmi$coefficients[11] + bmi_med_inc_his$coefficients[11]*mod_diab_inctopov_his_bmi$coefficients[12], mod_diab_inctopov_his_bmi$coefficients[11], bmi_med_inc_his$coefficients[11]*mod_diab_inctopov_his_bmi$coefficients[12], bmi_med_inc_his$coefficients[11]*mod_diab_inctopov_his_bmi$coefficients[12]/(mod_diab_inctopov_his_bmi$coefficients[11] + bmi_med_inc_his$coefficients[11]*mod_diab_inctopov_his_bmi$coefficients[12])),
                                   cbind('NHA',mod_diab_inctopov_nha_bmi$coefficients[11] + bmi_med_inc_nha$coefficients[11]*mod_diab_inctopov_nha_bmi$coefficients[12], mod_diab_inctopov_nha_bmi$coefficients[11], bmi_med_inc_nha$coefficients[11]*mod_diab_inctopov_nha_bmi$coefficients[12], bmi_med_inc_nha$coefficients[11]*mod_diab_inctopov_nha_bmi$coefficients[12]/(mod_diab_inctopov_nha_bmi$coefficients[11] + bmi_med_inc_nha$coefficients[11]*mod_diab_inctopov_nha_bmi$coefficients[12])),
                                   cbind('OTH',mod_diab_inctopov_oth_bmi$coefficients[11] + bmi_med_inc_oth$coefficients[11]*mod_diab_inctopov_oth_bmi$coefficients[12], mod_diab_inctopov_oth_bmi$coefficients[11], bmi_med_inc_oth$coefficients[11]*mod_diab_inctopov_oth_bmi$coefficients[12], bmi_med_inc_oth$coefficients[11]*mod_diab_inctopov_oth_bmi$coefficients[12]/(mod_diab_inctopov_oth_bmi$coefficients[11] + bmi_med_inc_oth$coefficients[11]*mod_diab_inctopov_oth_bmi$coefficients[12])),
                                   cbind('OTH11',mod_diab_inctopov_oth11_bmi$coefficients[11] + bmi_med_inc_oth11$coefficients[11]*mod_diab_inctopov_oth11_bmi$coefficients[12], mod_diab_inctopov_oth11_bmi$coefficients[11], bmi_med_inc_oth11$coefficients[11]*mod_diab_inctopov_oth11_bmi$coefficients[12], bmi_med_inc_oth11$coefficients[11]*mod_diab_inctopov_oth11_bmi$coefficients[12]/(mod_diab_inctopov_oth11_bmi$coefficients[11] + bmi_med_inc_oth11$coefficients[11]*mod_diab_inctopov_oth11_bmi$coefficients[12])))
colnames(t2d_propmediated_bmi_inc) <- c('Race/Ethnicity Group','Total Effect','Direct Effect','Indirect Effect','Proportion Mediated')
t2d_propmediated_bmi_inc

library(mediation)
#CIs are tiny (<0.01 for group as a whole); takes A LOT of ram to run
mod_inc_bmi_all <- svyglm(design=surv_all, bmi ~ age + female + survyr + smoke_status + insur_stability + inc_to_pov)
mod_educ_diab_bmiadj_all <- svyglm(design = surv_all, diab ~ age + female + survyr + smoke_status + insur_stability + inc_to_pov + bmi, family=binomial(link=logit))
set.seed(123)
med.out_all <- mediate(mod_inc_bmi_all, mod_educ_diab_bmiadj_all, treat='inc_to_pov', mediator='bmi', robustSE=T, sims=1000)
summary(med.out_all)
mod_inc_bmi_raceadj <- svyglm(design=surv_all, bmi ~ age + female + survyr + smoke_status + insur_stability + inc_to_pov + racecat)
mod_educ_diab_bmiadj_raceadj <- svyglm(design = surv_all, diab ~ age + female + survyr + smoke_status + insur_stability + inc_to_pov + bmi + racecat, family=binomial(link=logit))
med.out_raceadj <- mediate(mod_inc_bmi_raceadj, mod_educ_diab_bmiadj_raceadj, treat='inc_to_pov', mediator='bmi', robustSE=T, sims=1000)
summary(med.out_raceadj)
mod_inc_bmi_nhw <- svyglm(design=surv_nhw, bmi ~ age + female + survyr + smoke_status + insur_stability + inc_to_pov)
mod_educ_diab_bmiadj_nhw <- svyglm(design = surv_nhw, diab ~ age + female + survyr + smoke_status + insur_stability + inc_to_pov + bmi, family=binomial(link=logit))
med.out_nhw <- mediate(mod_inc_bmi_nhw, mod_educ_diab_bmiadj_nhw, treat='inc_to_pov', mediator='bmi', robustSE=T, sims=1000)
summary(med.out_nhw)
mod_inc_bmi_nhb <- svyglm(design=surv_nhb, bmi ~ age + female + survyr + smoke_status + insur_stability + inc_to_pov)
mod_educ_diab_bmiadj_nhb <- svyglm(design = surv_nhb, diab ~ age + female + survyr + smoke_status + insur_stability + inc_to_pov + bmi, family=binomial(link=logit))
med.out_nhb <- mediate(mod_inc_bmi_nhb, mod_educ_diab_bmiadj_nhb, treat='inc_to_pov', mediator='bmi', robustSE=T, sims=1000)
summary(med.out_nhb)
mod_inc_bmi_mex <- svyglm(design=surv_mex, bmi ~ age + female + survyr + smoke_status + insur_stability + inc_to_pov)
mod_educ_diab_bmiadj_mex <- svyglm(design = surv_mex, diab ~ age + female + survyr + smoke_status + insur_stability + inc_to_pov + bmi, family=binomial(link=logit))
med.out_mex <- mediate(mod_inc_bmi_mex, mod_educ_diab_bmiadj_mex, treat='inc_to_pov', mediator='bmi', robustSE=T, sims=1000)
summary(med.out_mex)
mod_inc_bmi_his <- svyglm(design=surv_his, bmi ~ age + female + survyr + smoke_status + insur_stability + inc_to_pov)
mod_educ_diab_bmiadj_his <- svyglm(design = surv_his, diab ~ age + female + survyr + smoke_status + insur_stability + inc_to_pov + bmi, family=binomial(link=logit))
med.out_his <- mediate(mod_inc_bmi_his, mod_educ_diab_bmiadj_his, treat='inc_to_pov', mediator='bmi', robustSE=T, sims=1000)
summary(med.out_his)
mod_inc_bmi_nha <- svyglm(design=surv_nha, bmi ~ age + female + survyr + smoke_status + insur_stability + inc_to_pov)
mod_educ_diab_bmiadj_nha <- svyglm(design = surv_nha, diab ~ age + female + survyr + smoke_status + insur_stability + inc_to_pov + bmi, family=binomial(link=logit))
med.out_nha <- mediate(mod_inc_bmi_nha, mod_educ_diab_bmiadj_nha, treat='inc_to_pov', mediator='bmi', robustSE=T, sims=1000)
summary(med.out_nha)
mod_inc_bmi_oth <- svyglm(design=surv_oth, bmi ~ age + female + survyr + smoke_status + insur_stability + inc_to_pov)
mod_educ_diab_bmiadj_oth <- svyglm(design = surv_oth, diab ~ age + female + survyr + smoke_status + insur_stability + inc_to_pov + bmi, family=binomial(link=logit))
med.out_oth <- mediate(mod_inc_bmi_oth, mod_educ_diab_bmiadj_oth, treat='inc_to_pov', mediator='bmi', robustSE=T, sims=1000)
summary(med.out_oth)
mod_inc_bmi_oth11 <- svyglm(design=surv_oth11, bmi ~ age + female + survyr + smoke_status + insur_stability + inc_to_pov)
mod_educ_diab_bmiadj_oth11 <- svyglm(design = surv_oth11, diab ~ age + female + survyr + smoke_status + insur_stability + inc_to_pov + bmi, family=binomial(link=logit))
med.out_oth11 <- mediate(mod_inc_bmi_oth11, mod_educ_diab_bmiadj_oth11, treat='inc_to_pov', mediator='bmi', robustSE=T, sims=1000)
summary(med.out_oth11)



#I2 from raw numbers: inc2pov->diab prevalence - USE ME
n <- as.vector(t(table(nhanes[nhanes$age >= 18 & !nhanes$insur_stability=='Missing',]$racecat)))
racecatname <-rownames(table(nhanes[nhanes$age >= 18 & !nhanes$insur_stability=='Missing',]$racecat))
ESTIM_inc_diab <- c(coef(summary(mod_diab_inctopov_nhw))[11,"Estimate"],coef(summary(mod_diab_inctopov_nhb))[11,"Estimate"],coef(summary(mod_diab_inctopov_nha))[11,"Estimate"],
                   coef(summary(mod_diab_inctopov_mex))[11,"Estimate"],coef(summary(mod_diab_inctopov_his))[11,"Estimate"],coef(summary(mod_diab_inctopov_oth))[11,"Estimate"],coef(summary(mod_diab_inctopov_oth11))[11,"Estimate"])
SE_inc_diab <- c(coef(summary(mod_diab_inctopov_nhw))[11,"Std. Error"],coef(summary(mod_diab_inctopov_nhb))[11,"Std. Error"],coef(summary(mod_diab_inctopov_nha))[11,"Std. Error"],
                coef(summary(mod_diab_inctopov_mex))[11,"Std. Error"],coef(summary(mod_diab_inctopov_his))[11,"Std. Error"],coef(summary(mod_diab_inctopov_oth))[11,"Std. Error"],coef(summary(mod_diab_inctopov_oth11))[11,"Std. Error"])
vi <- as.vector(t((SE_inc_diab*sqrt(n))^2)) #variance is SD^2, SD is SE*sqrt(n)
nhanes_dat_inc_diab <- data.frame(racecatname,n,ESTIM_inc_diab,vi,SE_inc_diab)
res_inc_diab <- rma(yi=ESTIM_inc_diab,sei=SE_inc_diab, data=nhanes_dat_inc_diab)
res_nonhw_inc_diab <- rma(yi=ESTIM_inc_diab,sei=SE_inc_diab, data=nhanes_dat_inc_diab[!racecatname=='NHW',])
res_nonhb_inc_diab <- rma(yi=ESTIM_inc_diab,sei=SE_inc_diab, data=nhanes_dat_inc_diab[!racecatname=='NHB',])
res_nomex_inc_diab <- rma(yi=ESTIM_inc_diab,sei=SE_inc_diab, data=nhanes_dat_inc_diab[!racecatname=='Mexican-American',])
res_nohis_inc_diab <- rma(yi=ESTIM_inc_diab,sei=SE_inc_diab, data=nhanes_dat_inc_diab[!racecatname=='Other Hispanic',])
res_nonha_inc_diab <- rma(yi=ESTIM_inc_diab,sei=SE_inc_diab, data=nhanes_dat_inc_diab[!racecatname=='NHAsian',])
res_nooth_inc_diab <- rma(yi=ESTIM_inc_diab,sei=SE_inc_diab, data=nhanes_dat_inc_diab[!racecatname=='Other/Multi-Racial including NHA (pre-2011)',])
res_nooth11_inc_diab <- rma(yi=ESTIM_inc_diab,sei=SE_inc_diab, data=nhanes_dat_inc_diab[!racecatname=='Other/Multi-Racial (post-2011)',])
res_nonhwnh_inc_diaba <- rma(yi=ESTIM_inc_diab,sei=SE_inc_diab, data=nhanes_dat_inc_diab[!racecatname=='NHW' & !racecatname=='NHAsian',])
res_nhwnha_inc_diab <- rma(yi=ESTIM_inc_diab,sei=SE_inc_diab, data=nhanes_dat_inc_diab[racecatname=='NHW' | racecatname=='NHAsian',])
i2_inc_diab <- rbind(res_inc_diab$I2, res_nonhw_inc_diab$I2, res_nonhb_inc_diab$I2, res_nomex_inc_diab$I2, res_nohis_inc_diab$I2, res_nonha_inc_diab$I2, res_nooth_inc_diab$I2, res_nooth11_inc_diab$I2)
i2_inc_diab
influence(res_inc_diab, progbar=F)


###calc I2 for inc2pov -> obesity prevalence
#I2 from raw numbers - USE ME:
n <- as.vector(t(table(nhanes[nhanes$age >= 18 & !nhanes$insur_stability=='Missing',]$racecat)))
racecatname <-rownames(table(nhanes[nhanes$age >= 18 & !nhanes$insur_stability=='Missing',]$racecat))
ESTIM_inc_ob <- c(coef(summary(mod_obes_inctopov_nhw))[11,"Estimate"],coef(summary(mod_obes_inctopov_nhb))[11,"Estimate"],
           coef(summary(mod_obes_inctopov_mex))[11,"Estimate"],coef(summary(mod_obes_inctopov_his))[11,"Estimate"],coef(summary(mod_obes_inctopov_nha))[11,"Estimate"],
           coef(summary(mod_obes_inctopov_oth))[11,"Estimate"],coef(summary(mod_obes_inctopov_oth11))[11,"Estimate"])
SE_inc_ob <- c(coef(summary(mod_obes_inctopov_nhw))[11,"Std. Error"],coef(summary(mod_obes_inctopov_nhb))[11,"Std. Error"],
        coef(summary(mod_obes_inctopov_mex))[11,"Std. Error"],coef(summary(mod_obes_inctopov_his))[11,"Std. Error"],coef(summary(mod_obes_inctopov_nha))[11,"Std. Error"],
        coef(summary(mod_obes_inctopov_oth))[11,"Std. Error"],coef(summary(mod_obes_inctopov_oth11))[11,"Std. Error"])
vi <- as.vector(t((SE_inc_ob*sqrt(n))^2)) #variance is SD^2, SD is SE*sqrt(n)
nhanes_dat_inc_ob <- data.frame(racecatname,n,ESTIM_inc_ob,vi,SE_inc_ob)
res_inc_ob <- rma(yi=ESTIM_inc_ob,sei=SE_inc_ob, data=nhanes_dat_inc_ob)
res_nonhw_inc_ob <- rma(yi=ESTIM_inc_ob,sei=SE_inc_ob, data=nhanes_dat_inc_ob[!racecatname=='NHW',])
res_nonhb_inc_ob <- rma(yi=ESTIM_inc_ob,sei=SE_inc_ob, data=nhanes_dat_inc_ob[!racecatname=='NHB',])
res_nomex_inc_ob <- rma(yi=ESTIM_inc_ob,sei=SE_inc_ob, data=nhanes_dat_inc_ob[!racecatname=='Mexican-American',])
res_nohis_inc_ob <- rma(yi=ESTIM_inc_ob,sei=SE_inc_ob, data=nhanes_dat_inc_ob[!racecatname=='Other Hispanic',])
res_nonha_inc_ob <- rma(yi=ESTIM_inc_ob,sei=SE_inc_ob, data=nhanes_dat_inc_ob[!racecatname=='NHAsian',])
res_nooth_inc_ob <- rma(yi=ESTIM_inc_ob,sei=SE_inc_ob, data=nhanes_dat_inc_ob[!racecatname=='Other/Multi-Racial including NHA (pre-2011)',])
res_nooth11_inc_ob <- rma(yi=ESTIM_inc_ob,sei=SE_inc_ob, data=nhanes_dat_inc_ob[!racecatname=='Other/Multi-Racial (post-2011)',])
res_nonhwnh_inc_oba <- rma(yi=ESTIM_inc_ob,sei=SE_inc_ob, data=nhanes_dat_inc_ob[!racecatname=='NHW' & !racecatname=='NHAsian',])
res_nhwnha_inc_ob <- rma(yi=ESTIM_inc_ob,sei=SE_inc_ob, data=nhanes_dat_inc_ob[racecatname=='NHW' | racecatname=='NHAsian',])
i2_inc_ob <- rbind(res_inc_ob$I2, res_nonhw_inc_ob$I2, res_nonhb_inc_ob$I2,res_nomex_inc_ob$I2, res_nohis_inc_ob$I2, res_nonha_inc_ob$I2,  res_nooth_inc_ob$I2, res_nooth11_inc_ob$I2)
i2_inc_ob
influence(res_inc_ob, progbar=F)


#####visuals: forest plots, categorical income and educ#####
#tiff(file='C:/Users/sarac/OneDrive/Documents/Research/Area v Individual SES/nhanes_forest_indiv_educ_racestrat.tiff', width=7, height=5, units='in', res=300)
forestplot(c("HS Degree","Some HS","No HS"), 
           mean = cbind(nhanes_educ_diab_result[9:11,1],nhanes_educ_diab_result[9:11,5],nhanes_educ_diab_result[9:11,9],
                        nhanes_educ_diab_result[9:11,13],nhanes_educ_diab_result[9:11,17],nhanes_educ_diab_result[9:11,21],
                        nhanes_educ_diab_result[9:11,25],nhanes_educ_diab_result[9:11,29],nhanes_educ_diab_result[9:11,33]),
           lower = cbind(nhanes_educ_diab_result[9:11,2],nhanes_educ_diab_result[9:11,6],nhanes_educ_diab_result[9:11,10],
                         nhanes_educ_diab_result[9:11,14],nhanes_educ_diab_result[9:11,18],nhanes_educ_diab_result[9:11,22],
                         nhanes_educ_diab_result[9:11,26],nhanes_educ_diab_result[9:11,30],nhanes_educ_diab_result[9:11,34]),
           upper = cbind(nhanes_educ_diab_result[9:11,3],nhanes_educ_diab_result[9:11,7],nhanes_educ_diab_result[9:11,11],
                         nhanes_educ_diab_result[9:11,15],nhanes_educ_diab_result[9:11,19],nhanes_educ_diab_result[9:11,23],
                         nhanes_educ_diab_result[9:11,27],nhanes_educ_diab_result[9:11,31],nhanes_educ_diab_result[9:11,35]),
           col = fpColors(box = c('gray23','gray57','dodgerblue4','darkslategray3','yellowgreen','salmon1','indianred3','mediumpurple4','maroon')),
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
#dev.off()

nhanes_educ_diab_result_transpose <- t(nhanes_educ_diab_result)
rownames(nhanes_educ_diab_result_transpose)
#tiff(file='C:/Users/sarac/OneDrive/Documents/Research/Area v Individual SES/nhanes_forest_indiv_educ_racestrat_transpose.tiff', width=7, height=5, units='in', res=300)
forestplot(c("All","All, Race-adjusted","NHW","NHB","Mexican-American","Other Hispanic","NHA","Other/Multi-Racial \n including NHA (pre-2011)","Other/Multi-Racial \n (post-2011)"), 
           mean = cbind(nhanes_educ_diab_result_transpose[c(1,5,9,13,17,21,25,29,33),9],nhanes_educ_diab_result_transpose[c(1,5,9,13,17,21,25,29,33),10],nhanes_educ_diab_result_transpose[c(1,5,9,13,17,21,25,29,33),11]),
           lower = cbind(nhanes_educ_diab_result_transpose[c(2,6,10,14,18,22,26,30,34),9],nhanes_educ_diab_result_transpose[c(2,6,10,14,18,22,26,30,34),10],nhanes_educ_diab_result_transpose[c(2,6,10,14,18,22,26,30,34),11]),
           upper = cbind(nhanes_educ_diab_result_transpose[c(3,7,11,15,19,23,27,31,35),9],nhanes_educ_diab_result_transpose[c(3,7,11,15,19,23,27,31,35),10],nhanes_educ_diab_result_transpose[c(3,7,11,15,19,23,27,31,35),11]),
           col = fpColors(box = c('darkslategray3','lightgoldenrod','indianred3')),
           legend=c("Some College","HS Degree","Less than HS"),
           legend_args = fpLegend(pos = list(x = 0.9, y = 0.9), gp = gpar(cex=0.9,col = "#CCCCCC", fill = "#F9F9F9")),
           xlab = "OR (95% CI)",
           title = 'Adjusted OR for Educational Attainment (Ref=College Degree) \n on Diabetes, Stratified by Race/Ethnicity',
           txt_gp = fpTxtGp(title=gpar(cex=1),xlab=gpar(cex=1), ticks=gpar(cex=1), label=gpar(cex=1)),
           vertices=TRUE,
           line.margin=0.25,
           graph.pos=2,
           clip=c(0.5,4.0), xlim=c(0.5,4.0), xticks=c(0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0), xlog=TRUE, grid=structure(c(0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0)))
#dev.off()


#tiff(file='C:/Users/sarac/OneDrive/Documents/Research/Area v Individual SES/nhanes_forest_indiv_inc_racestrat.tiff', width=7, height=5, units='in', res=300)
forestplot(c("$75,000-149,999","$50,000-74,999","$25,000-49,999","Under $25,000"), 
           mean = cbind(nhanes_inc_diab_result[9:12,1],nhanes_inc_diab_result[9:12,5],nhanes_inc_diab_result[9:12,9],
                        nhanes_inc_diab_result[9:12,13],nhanes_inc_diab_result[9:12,17],nhanes_inc_diab_result[9:12,21],
                        nhanes_inc_diab_result[9:12,25],nhanes_inc_diab_result[9:12,29],nhanes_inc_diab_result[9:12,33]),
           lower = cbind(nhanes_inc_diab_result[9:12,2],nhanes_inc_diab_result[9:12,6],nhanes_inc_diab_result[9:12,10],
                         nhanes_inc_diab_result[9:12,14],nhanes_inc_diab_result[9:12,18],nhanes_inc_diab_result[9:12,22],
                         nhanes_inc_diab_result[9:12,26],nhanes_inc_diab_result[9:12,30],nhanes_inc_diab_result[9:12,34]),
           upper = cbind(nhanes_inc_diab_result[9:12,3],nhanes_inc_diab_result[9:12,7],nhanes_inc_diab_result[9:12,11],
                         nhanes_inc_diab_result[9:12,15],nhanes_inc_diab_result[9:12,19],nhanes_inc_diab_result[9:12,23],
                         nhanes_inc_diab_result[9:12,27],nhanes_inc_diab_result[9:12,31],nhanes_inc_diab_result[9:12,35]),
           col = fpColors(box = c('gray23','gray57','dodgerblue4','darkslategray3','yellowgreen','salmon1','indianred3','mediumpurple4','maroon')),
           legend=c("All","All, Race-adjusted","NHW","NHB","Mexican-American","Other Hispanic","NHA","Other/Multi-Racial \n including NHA (pre-2011)","Other/Multi-Racial \n (post-2011)"),
           legend_args = fpLegend(pos = list(x = 0.9, y = 0.9), gp = gpar(cex=0.9,col = "#CCCCCC", fill = "#F9F9F9")),
           xlab = "OR (95% CI)",
           title = 'Adjusted OR for Income Category (Ref= More than $75,000) \n on Diabetes, Stratified by Race/Ethnicity',
           txt_gp = fpTxtGp(title=gpar(cex=1),xlab=gpar(cex=1), ticks=gpar(cex=1), label=gpar(cex=1)),
           vertices=TRUE,
           line.margin=0.25,
           graph.pos=2,
           clip=c(0.5,2.5), xlim=c(0.5,2.5), xticks=c(0.5,1.0,1.5,2.0,2.5), xlog=TRUE, grid=structure(c(0.5,1.0,1.5,2.0,2.5)))
#dev.off()

nhanes_inc_diab_result_transpose <- t(nhanes_inc_diab_result)
rownames(nhanes_inc_diab_result_transpose)
#tiff(file='C:/Users/sarac/OneDrive/Documents/Research/Area v Individual SES/nhanes_forest_indiv_educ_racestrat_transpose.tiff', width=7, height=5, units='in', res=300)
forestplot(c("All","All, Race-adjusted","NHW","NHB","Mexican-American","Other Hispanic","NHA","Other/Multi-Racial \n including NHA (pre-2011)","Other/Multi-Racial \n (post-2011)"), 
           mean = cbind(nhanes_inc_diab_result_transpose[c(1,5,9,13,17,21,25,29,33),9],nhanes_inc_diab_result_transpose[c(1,5,9,13,17,21,25,29,33),10],nhanes_inc_diab_result_transpose[c(1,5,9,13,17,21,25,29,33),11],nhanes_inc_diab_result_transpose[c(1,5,9,13,17,21,25,29,33),12]),
           lower = cbind(nhanes_inc_diab_result_transpose[c(2,6,10,14,18,22,26,30,34),9],nhanes_inc_diab_result_transpose[c(2,6,10,14,18,22,26,30,34),10],nhanes_inc_diab_result_transpose[c(2,6,10,14,18,22,26,30,34),11],nhanes_inc_diab_result_transpose[c(2,6,10,14,18,22,26,30,34),12]),
           upper = cbind(nhanes_inc_diab_result_transpose[c(3,7,11,15,19,23,27,31,35),9],nhanes_inc_diab_result_transpose[c(3,7,11,15,19,23,27,31,35),10],nhanes_inc_diab_result_transpose[c(3,7,11,15,19,23,27,31,35),11],nhanes_inc_diab_result_transpose[c(3,7,11,15,19,23,27,31,35),12]),
           col = fpColors(box = c('darkslategray3','lightgoldenrod','indianred3','mediumpurple4')),
           legend=c("$75,000-149,999","$50,000-74,999","$25,000-49,999","Under $25,000"),
           legend_args = fpLegend(pos = list(x = 0.9, y = 0.9), gp = gpar(cex=0.9,col = "#CCCCCC", fill = "#F9F9F9")),
           xlab = "OR (95% CI)",
           title = 'Adjusted OR for Educational Attainment (Ref=College Degree) \n on Diabetes, Stratified by Race/Ethnicity',
           txt_gp = fpTxtGp(title=gpar(cex=1),xlab=gpar(cex=1), ticks=gpar(cex=1), label=gpar(cex=1)),
           vertices=TRUE,
           line.margin=0.25,
           graph.pos=2,
           clip=c(0.5,4.0), xlim=c(0.5,4.0), xticks=c(0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0), xlog=TRUE, grid=structure(c(0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0)))
#dev.off()



#####DIABETES or OBESITY DIAGNOSED, BY EDUCCONT#####
#survdesign
surv_diab <- subset(surv_all, diab==1)
surv_dm_nhw <- subset(surv_all, diab==1 & racecat=='NHW')
surv_dm_nhb <- subset(surv_all, diab==1 & racecat=='NHB')
surv_dm_nha <- subset(surv_all, diab==1 & racecat=='NHAsian')
surv_dm_mex <- subset(surv_all, diab==1 & racecat=='Mexican-American')
surv_dm_his <- subset(surv_all, diab==1 & racecat=='Other Hispanic')
surv_dm_oth <- subset(surv_all, diab==1 & racecat=='Other/Multi-Racial including NHA (pre-2011)')
surv_dm_oth11 <- subset(surv_all, diab==1 & racecat=='Other/Multi-Racial (post-2011)')

#surv_diab <- svydesign(id = ~SDMVPSU, strata = ~ SDMVSTRA, weights = ~weights, data=nhanes[nhanes$age >= 18 & nhanes$diab==1 & !nhanes$insur_stability=='Missing',], nest=T)
#surv_dm_nhw <- svydesign(id = ~SDMVPSU, strata = ~ SDMVSTRA, weights = ~weights, data=nhanes[nhanes$age >= 18 & nhanes$racecat=='NHW' & nhanes$diab==1 & !nhanes$insur_stability=='Missing',], nest=T)
#surv_dm_nhb <- svydesign(id = ~SDMVPSU, strata = ~ SDMVSTRA, weights = ~weights, data=nhanes[nhanes$age >= 18 & nhanes$racecat=='NHB' & nhanes$diab==1 & !nhanes$insur_stability=='Missing',], nest=T)
#surv_dm_nha <- svydesign(id = ~SDMVPSU, strata = ~ SDMVSTRA, weights = ~weights, data=nhanes[nhanes$age >= 18 & nhanes$racecat=='NHAsian' & nhanes$diab==1 & !nhanes$insur_stability=='Missing',], nest=T)
#surv_dm_mex <- svydesign(id = ~SDMVPSU, strata = ~ SDMVSTRA, weights = ~weights, data=nhanes[nhanes$age >= 18 & nhanes$racecat=='Mexican-American' & nhanes$diab==1 & !nhanes$insur_stability=='Missing',], nest=T)
#surv_dm_his <- svydesign(id = ~SDMVPSU, strata = ~ SDMVSTRA, weights = ~weights, data=nhanes[nhanes$age >= 18 & nhanes$racecat=='Other Hispanic' & nhanes$diab==1 & !nhanes$insur_stability=='Missing',], nest=T)
#surv_dm_oth <- svydesign(id = ~SDMVPSU, strata = ~ SDMVSTRA, weights = ~weights, data=nhanes[nhanes$age >= 18 & nhanes$racecat=='Other/Multi-Racial including NHA (pre-2011)' & nhanes$diab==1 & !nhanes$insur_stability=='Missing',], nest=T)
#surv_dm_oth11 <- svydesign(id = ~SDMVPSU, strata = ~ SDMVSTRA, weights = ~weights, data=nhanes[nhanes$age >= 18 & nhanes$racecat=='Other/Multi-Racial (post-2011)' & nhanes$diab==1 & !nhanes$insur_stability=='Missing',], nest=T)

surv_ob <- subset(surv_all, obese==1)
surv_ob_nhw <- subset(surv_all, obese==1 & racecat=='NHW')
surv_ob_nhb <- subset(surv_all, obese==1 & racecat=='NHB')
surv_ob_nha <- subset(surv_all, obese==1 & racecat=='NHAsian')
surv_ob_mex <- subset(surv_all, obese==1 & racecat=='Mexican-American')
surv_ob_his <- subset(surv_all, obese==1 & racecat=='Other Hispanic')
surv_ob_oth <- subset(surv_all, obese==1 & racecat=='Other/Multi-Racial including NHA (pre-2011)')
surv_ob_oth11 <- subset(surv_all, obese==1 & racecat=='Other/Multi-Racial (post-2011)')
#surv_ob <- svydesign(id = ~SDMVPSU, strata = ~ SDMVSTRA, weights = ~weights, data=nhanes[nhanes$age >= 18 & nhanes$obese==1 & !nhanes$insur_stability=='Missing',], nest=T)
#surv_ob_nhw <- svydesign(id = ~SDMVPSU, strata = ~ SDMVSTRA, weights = ~weights, data=nhanes[nhanes$age >= 18 & nhanes$racecat=='NHW' & nhanes$obese==1 & !nhanes$insur_stability=='Missing',], nest=T)
#surv_ob_nhb <- svydesign(id = ~SDMVPSU, strata = ~ SDMVSTRA, weights = ~weights, data=nhanes[nhanes$age >= 18 & nhanes$racecat=='NHB' & nhanes$obese==1 & !nhanes$insur_stability=='Missing',], nest=T)
#surv_ob_nha <- svydesign(id = ~SDMVPSU, strata = ~ SDMVSTRA, weights = ~weights, data=nhanes[nhanes$age >= 18 & nhanes$racecat=='NHAsian' & nhanes$obese==1 & !nhanes$insur_stability=='Missing',], nest=T)
#surv_ob_mex <- svydesign(id = ~SDMVPSU, strata = ~ SDMVSTRA, weights = ~weights, data=nhanes[nhanes$age >= 18 & nhanes$racecat=='Mexican-American' & nhanes$obese==1 & !nhanes$insur_stability=='Missing',], nest=T)
#surv_ob_his <- svydesign(id = ~SDMVPSU, strata = ~ SDMVSTRA, weights = ~weights, data=nhanes[nhanes$age >= 18 & nhanes$racecat=='Other Hispanic' & nhanes$obese==1 & !nhanes$insur_stability=='Missing',], nest=T)
#surv_ob_oth <- svydesign(id = ~SDMVPSU, strata = ~ SDMVSTRA, weights = ~weights, data=nhanes[nhanes$age >= 18 & nhanes$racecat=='Other/Multi-Racial including NHA (pre-2011)' & nhanes$obese==1 & !nhanes$insur_stability=='Missing',], nest=T)
#surv_ob_oth11 <- svydesign(id = ~SDMVPSU, strata = ~ SDMVSTRA, weights = ~weights, data=nhanes[nhanes$age >= 18 & nhanes$racecat=='Other/Multi-Racial (post-2011)' & nhanes$obese==1 & !nhanes$insur_stability=='Missing',], nest=T)

#models
mod_diabknown_educcont_all <- svyglm(design = surv_diab, diab_known ~ age + female + survyr + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_diabknown_educcont_all_raceadj <- svyglm(design = surv_diab, diab_known ~ age + female + survyr + smoke_status + insur_stability + educ_num + racecat, family=binomial(link=logit))
mod_diabknown_educcont_nhw <- svyglm(design = surv_dm_nhw, diab_known ~ age + female + survyr + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_diabknown_educcont_nhb <- svyglm(design = surv_dm_nhb, diab_known ~ age + female + survyr + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_diabknown_educcont_nha <- svyglm(design = surv_dm_nha, diab_known ~ age + female + survyr + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_diabknown_educcont_mex <- svyglm(design = surv_dm_mex, diab_known ~ age + female + survyr + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_diabknown_educcont_his <- svyglm(design = surv_dm_his, diab_known ~ age + female + survyr + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_diabknown_educcont_oth <- svyglm(design = surv_dm_oth, diab_known ~ age + female + survyr + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_diabknown_educcont_oth11 <- svyglm(design = surv_dm_oth11, diab_known ~ age + female + survyr + smoke_status + insur_stability + educ_num, family=binomial(link=logit))

mod_obesknown_educcont_all <- svyglm(design = surv_ob, obese_known ~ age + female + survyr + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_obesknown_educcont_all_raceadj <- svyglm(design = surv_ob, obese_known ~ age + female + survyr + smoke_status + insur_stability + educ_num + racecat, family=binomial(link=logit))
mod_obesknown_educcont_nhw <- svyglm(design = surv_ob_nhw, obese_known ~ age + female + survyr + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_obesknown_educcont_nhb <- svyglm(design = surv_ob_nhb, obese_known ~ age + female + survyr + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_obesknown_educcont_nha <- svyglm(design = surv_ob_nha, obese_known ~ age + female + survyr + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_obesknown_educcont_mex <- svyglm(design = surv_ob_mex, obese_known ~ age + female + survyr + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_obesknown_educcont_his <- svyglm(design = surv_ob_his, obese_known ~ age + female + survyr + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_obesknown_educcont_oth <- svyglm(design = surv_ob_oth, obese_known ~ age + female + survyr + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_obesknown_educcont_oth11 <- svyglm(design = surv_ob_oth11, obese_known ~ age + female + survyr + smoke_status + insur_stability + educ_num, family=binomial(link=logit))

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
tiff(file='C:/Users/sarac/OneDrive/Documents/Research/Area v Individual SES/NHANES_SES and AoU_SES/nhanes_forest_indiv_educcont_t2dobknown.tiff', width=8, height=5, units='in', res=300)
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
           #legend=c("All","All, Race-adjusted",rownames(table(nhanes$racecat))),#c("All","All-Race-adjusted","NHW","NHB","NHA","Mexican-American","Other Hispanic","Other/Multiracial"),
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
mod_diabknown_inctopov_all <- svyglm(design = surv_diab, diab_known ~ age + female + survyr + smoke_status + insur_stability + inc_to_pov, family=binomial(link=logit))
mod_diabknown_inctopov_all_raceadj <- svyglm(design = surv_diab, diab_known ~ age + female + survyr + smoke_status + insur_stability + inc_to_pov + racecat, family=binomial(link=logit))
mod_diabknown_inctopov_nhw <- svyglm(design = surv_dm_nhw, diab_known ~ age + female + survyr + smoke_status + insur_stability + inc_to_pov, family=binomial(link=logit))
mod_diabknown_inctopov_nhb <- svyglm(design = surv_dm_nhb, diab_known ~ age + female + survyr + smoke_status + insur_stability + inc_to_pov, family=binomial(link=logit))
mod_diabknown_inctopov_nha <- svyglm(design = surv_dm_nha, diab_known ~ age + female + survyr + smoke_status + insur_stability + inc_to_pov, family=binomial(link=logit))
mod_diabknown_inctopov_mex <- svyglm(design = surv_dm_mex, diab_known ~ age + female + survyr + smoke_status + insur_stability + inc_to_pov, family=binomial(link=logit))
mod_diabknown_inctopov_his <- svyglm(design = surv_dm_his, diab_known ~ age + female + survyr + smoke_status + insur_stability + inc_to_pov, family=binomial(link=logit))
mod_diabknown_inctopov_oth <- svyglm(design = surv_dm_oth, diab_known ~ age + female + survyr + smoke_status + insur_stability + inc_to_pov, family=binomial(link=logit))
mod_diabknown_inctopov_oth11 <- svyglm(design = surv_dm_oth11, diab_known ~ age + female + survyr + smoke_status + insur_stability + inc_to_pov, family=binomial(link=logit))

mod_obesknown_inctopov_all <- svyglm(design = surv_ob, obese_known ~ age + female + survyr + smoke_status + insur_stability + inc_to_pov, family=binomial(link=logit))
mod_obesknown_inctopov_all_raceadj <- svyglm(design = surv_ob, obese_known ~ age + female + survyr + smoke_status + insur_stability + inc_to_pov + racecat, family=binomial(link=logit))
mod_obesknown_inctopov_nhw <- svyglm(design = surv_ob_nhw, obese_known ~ age + female + survyr + smoke_status + insur_stability + inc_to_pov, family=binomial(link=logit))
mod_obesknown_inctopov_nhb <- svyglm(design = surv_ob_nhb, obese_known ~ age + female + survyr + smoke_status + insur_stability + inc_to_pov, family=binomial(link=logit))
mod_obesknown_inctopov_nha <- svyglm(design = surv_ob_nha, obese_known ~ age + female + survyr + smoke_status + insur_stability + inc_to_pov, family=binomial(link=logit))
mod_obesknown_inctopov_mex <- svyglm(design = surv_ob_mex, obese_known ~ age + female + survyr + smoke_status + insur_stability + inc_to_pov, family=binomial(link=logit))
mod_obesknown_inctopov_his <- svyglm(design = surv_ob_his, obese_known ~ age + female + survyr + smoke_status + insur_stability + inc_to_pov, family=binomial(link=logit))
mod_obesknown_inctopov_oth <- svyglm(design = surv_ob_oth, obese_known ~ age + female + survyr + smoke_status + insur_stability + inc_to_pov, family=binomial(link=logit))
mod_obesknown_inctopov_oth11 <- svyglm(design = surv_ob_oth11, obese_known ~ age + female + survyr + smoke_status + insur_stability + inc_to_pov, family=binomial(link=logit))

mod_diabob_inctopov_allrace <- data.frame(exp(mod_diabknown_inctopov_all$coefficients),exp(confint(mod_diabknown_inctopov_all)),coef(summary(mod_diabknown_inctopov_all))[,"Pr(>|t|)"],
                                         exp(mod_diabknown_inctopov_all_raceadj$coefficients)[1:11],exp(confint(mod_diabknown_inctopov_all_raceadj))[1:11,1:2],coef(summary(mod_diabknown_inctopov_all_raceadj))[,"Pr(>|t|)"][1:11],
                                         exp(mod_diabknown_inctopov_nhw$coefficients),exp(confint(mod_diabknown_inctopov_nhw)),coef(summary(mod_diabknown_inctopov_nhw))[,"Pr(>|t|)"],
                                         exp(mod_diabknown_inctopov_nhb$coefficients),exp(confint(mod_diabknown_inctopov_nhb)),coef(summary(mod_diabknown_inctopov_nhb))[,"Pr(>|t|)"],
                                         exp(mod_diabknown_inctopov_mex$coefficients),exp(confint(mod_diabknown_inctopov_mex)),coef(summary(mod_diabknown_inctopov_mex))[,"Pr(>|t|)"],
                                         exp(mod_diabknown_inctopov_his$coefficients),exp(confint(mod_diabknown_inctopov_his)),coef(summary(mod_diabknown_inctopov_his))[,"Pr(>|t|)"],
                                         exp(mod_diabknown_inctopov_nha$coefficients),exp(confint(mod_diabknown_inctopov_nha)),coef(summary(mod_diabknown_inctopov_nha))[,"Pr(>|t|)"],
                                         exp(mod_diabknown_inctopov_oth$coefficients),exp(confint(mod_diabknown_inctopov_oth)),coef(summary(mod_diabknown_inctopov_oth))[,"Pr(>|t|)"],
                                         exp(mod_diabknown_inctopov_oth11$coefficients),exp(confint(mod_diabknown_inctopov_oth11)),coef(summary(mod_diabknown_inctopov_oth11))[,"Pr(>|t|)"],
                                         exp(mod_obesknown_inctopov_all$coefficients),exp(confint(mod_obesknown_inctopov_all)),coef(summary(mod_obesknown_inctopov_all))[,"Pr(>|t|)"],
                                         exp(mod_obesknown_inctopov_all_raceadj$coefficients)[1:11],exp(confint(mod_obesknown_inctopov_all_raceadj))[1:11,1:2],coef(summary(mod_obesknown_inctopov_all_raceadj))[,"Pr(>|t|)"][1:11],
                                         exp(mod_obesknown_inctopov_nhw$coefficients),exp(confint(mod_obesknown_inctopov_nhw)),coef(summary(mod_obesknown_inctopov_nhw))[,"Pr(>|t|)"],
                                         exp(mod_obesknown_inctopov_nhb$coefficients),exp(confint(mod_obesknown_inctopov_nhb)),coef(summary(mod_obesknown_inctopov_nhb))[,"Pr(>|t|)"],
                                         exp(mod_obesknown_inctopov_mex$coefficients),exp(confint(mod_obesknown_inctopov_mex)),coef(summary(mod_obesknown_inctopov_mex))[,"Pr(>|t|)"],
                                         exp(mod_obesknown_inctopov_his$coefficients),exp(confint(mod_obesknown_inctopov_his)),coef(summary(mod_obesknown_inctopov_his))[,"Pr(>|t|)"],
                                         exp(mod_obesknown_inctopov_nha$coefficients),exp(confint(mod_obesknown_inctopov_nha)),coef(summary(mod_obesknown_inctopov_nha))[,"Pr(>|t|)"],
                                         exp(mod_obesknown_inctopov_oth$coefficients),exp(confint(mod_obesknown_inctopov_oth)),coef(summary(mod_obesknown_inctopov_oth))[,"Pr(>|t|)"],
                                         exp(mod_obesknown_inctopov_oth11$coefficients),exp(confint(mod_obesknown_inctopov_oth11)),coef(summary(mod_obesknown_inctopov_oth11))[,"Pr(>|t|)"],stringsAsFactors = FALSE)
mod_diabob_inctopov_allrace_transpose <- t(mod_diabob_inctopov_allrace)
dim(mod_diabob_inctopov_allrace)
tiff(file='C:/Users/sarac/OneDrive/Documents/Research/Area v Individual SES/NHANES_SES and AoU_SES/nhanes_forest_indiv_inctopov_t2dobknown.tiff', width=8, height=5, units='in', res=300)
forestplot(c('Type 2 Diabetes','Obesity'), 
           mean = cbind(t(mod_diabob_inctopov_allrace[11,c(1,37)]),t(mod_diabob_inctopov_allrace[11,c(5,41)]),
                        t(mod_diabob_inctopov_allrace[11,c(9,45)]),t(mod_diabob_inctopov_allrace[11,c(13,49)]),
                        t(mod_diabob_inctopov_allrace[11,c(17,53)]),t(mod_diabob_inctopov_allrace[11,c(21,57)]),
                        t(mod_diabob_inctopov_allrace[11,c(25,61)]),t(mod_diabob_inctopov_allrace[11,c(29,65)]),
                        t(mod_diabob_inctopov_allrace[11,c(33,69)])),
           lower = cbind(t(mod_diabob_inctopov_allrace[11,c(2,38)]),t(mod_diabob_inctopov_allrace[11,c(6,42)]),
                         t(mod_diabob_inctopov_allrace[11,c(10,46)]),t(mod_diabob_inctopov_allrace[11,c(14,50)]),
                         t(mod_diabob_inctopov_allrace[11,c(18,54)]),t(mod_diabob_inctopov_allrace[11,c(22,58)]),
                         t(mod_diabob_inctopov_allrace[11,c(26,62)]),t(mod_diabob_inctopov_allrace[11,c(30,66)]),
                         t(mod_diabob_inctopov_allrace[11,c(34,68)])),
           upper = cbind(t(mod_diabob_inctopov_allrace[11,c(3,39)]),t(mod_diabob_inctopov_allrace[11,c(7,43)]),
                         t(mod_diabob_inctopov_allrace[11,c(11,47)]),t(mod_diabob_inctopov_allrace[11,c(15,51)]),
                         t(mod_diabob_inctopov_allrace[11,c(19,55)]),t(mod_diabob_inctopov_allrace[11,c(23,59)]),
                         t(mod_diabob_inctopov_allrace[11,c(27,63)]),t(mod_diabob_inctopov_allrace[11,c(31,67)]),
                         t(mod_diabob_inctopov_allrace[11,c(35,71)])),
           col = fpColors(box = c('gray23','gray57','dodgerblue4','darkslategray3','palegreen3','lightgoldenrod','salmon1','indianred3','mediumpurple4')),
           legend=c("All","All, Race-adjusted","NHW","NHB","Mexican-American","Other Hispanic","NHA","Other/Multi-Racial \n including NHA (pre-2011)","Other/Multi-Racial \n (post-2011)"),
           legend_args = fpLegend(pos = "right"),#list(x = 0.5, y = -0.2), gp = gpar(cex=0.9,col = "#CCCCCC", fill = "#F9F9F9")),
           #legend=c("All","All, Race-adjusted",rownames(table(nhanes$racecat))),#c("All","All-Race-adjusted","NHW","NHB","NHA","Mexican-American","Other Hispanic","Other/Multiracial"),
           #legend_args = fpLegend(pos = list(x = 0.9, y = 0.9), gp = gpar(cex=0.9,col = "#CCCCCC", fill = "#F9F9F9")),
           xlab = "OR (95% CI)",
           title = 'Adjusted OR for Continuous Income-to-Poverty Ratio \n on Knowledge of Diagnosis of T2D and Obesity, \n Overall and Stratified by Race/Ethnicity',
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
surv_dmknown_oth <- subset(surv_all, diab_known==1 & !is.na(LBXGH) & racecat=='Other/Multi-Racial including NHA (pre-2011)')
surv_dmknown_oth11 <- subset(surv_all, diab_known==1 & !is.na(LBXGH) & racecat=='Other/Multi-Racial (post-2011)')
#surv_diab_known <- svydesign(id = ~SDMVPSU, strata = ~ SDMVSTRA, weights = ~weights, data=nhanes[nhanes$age >= 18 & nhanes$diab_known==1 & !nhanes$insur_stability=='Missing' & !is.na(LBXGH),], nest=T)
#surv_dmknown_nhw <- svydesign(id = ~SDMVPSU, strata = ~ SDMVSTRA, weights = ~weights, data=nhanes[nhanes$age >= 18 & nhanes$racecat=='NHW' & nhanes$diab_known==1 & !nhanes$insur_stability=='Missing' & !is.na(nhanes$LBXGH),], nest=T)
#surv_dmknown_nhb <- svydesign(id = ~SDMVPSU, strata = ~ SDMVSTRA, weights = ~weights, data=nhanes[nhanes$age >= 18 & nhanes$racecat=='NHB' & nhanes$diab_known==1 & !nhanes$insur_stability=='Missing' & !is.na(nhanes$LBXGH),], nest=T)
#surv_dmknown_nha <- svydesign(id = ~SDMVPSU, strata = ~ SDMVSTRA, weights = ~weights, data=nhanes[nhanes$age >= 18 & nhanes$racecat=='NHAsian' & nhanes$diab_known==1 & !nhanes$insur_stability=='Missing' & !is.na(nhanes$LBXGH),], nest=T)
#surv_dmknown_mex <- svydesign(id = ~SDMVPSU, strata = ~ SDMVSTRA, weights = ~weights, data=nhanes[nhanes$age >= 18 & nhanes$racecat=='Mexican-American' & nhanes$diab_known==1 & !nhanes$insur_stability=='Missing' & !is.na(nhanes$LBXGH),], nest=T)
#surv_dmknown_his <- svydesign(id = ~SDMVPSU, strata = ~ SDMVSTRA, weights = ~weights, data=nhanes[nhanes$age >= 18 & nhanes$racecat=='Other Hispanic' & nhanes$diab_known==1 & !nhanes$insur_stability=='Missing' & !is.na(nhanes$LBXGH),], nest=T)
#surv_dmknown_oth <- svydesign(id = ~SDMVPSU, strata = ~ SDMVSTRA, weights = ~weights, data=nhanes[nhanes$age >= 18 & nhanes$racecat=='Other/Multi-Racial including NHA (pre-2011)' & nhanes$diab_known==1 & !nhanes$insur_stability=='Missing' & !is.na(nhanes$LBXGH),], nest=T)
#surv_dmknown_oth11 <- svydesign(id = ~SDMVPSU, strata = ~ SDMVSTRA, weights = ~weights, data=nhanes[nhanes$age >= 18 & nhanes$racecat=='Other/Multi-Racial (post-2011)' & nhanes$diab_known==1 & !nhanes$insur_stability=='Missing' & !is.na(nhanes$LBXGH),], nest=T)

#models
mod_diabctrl_educcont_all <- svyglm(design = surv_diab_known, diab_controlled ~ age + female + survyr + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_diabctrl_educcont_all_raceadj <- svyglm(design = surv_diab_known, diab_controlled ~ age + female + survyr + smoke_status + insur_stability + educ_num + racecat, family=binomial(link=logit))
mod_diabctrl_educcont_nhw <- svyglm(design = surv_dmknown_nhw, diab_controlled ~ age + female + survyr + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_diabctrl_educcont_nhb <- svyglm(design = surv_dmknown_nhb, diab_controlled ~ age + female + survyr + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_diabctrl_educcont_nha <- svyglm(design = surv_dmknown_nha, diab_controlled ~ age + female + survyr + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_diabctrl_educcont_mex <- svyglm(design = surv_dmknown_mex, diab_controlled ~ age + female + survyr + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_diabctrl_educcont_his <- svyglm(design = surv_dmknown_his, diab_controlled ~ age + female + survyr + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_diabctrl_educcont_oth <- svyglm(design = surv_dmknown_oth, diab_controlled ~ age + female + survyr + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_diabctrl_educcont_oth11 <- svyglm(design = surv_dmknown_oth11, diab_controlled ~ age + female + survyr + smoke_status + insur_stability + educ_num, family=binomial(link=logit))

mod_diabctrl8_educcont_all <- svyglm(design = surv_diab_known, diab_controlled8 ~ age + female + survyr + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_diabctrl8_educcont_all_raceadj <- svyglm(design = surv_diab_known, diab_controlled8 ~ age + female + survyr + smoke_status + insur_stability + educ_num + racecat, family=binomial(link=logit))
mod_diabctrl8_educcont_nhw <- svyglm(design = surv_dmknown_nhw, diab_controlled8 ~ age + female + survyr + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_diabctrl8_educcont_nhb <- svyglm(design = surv_dmknown_nhb, diab_controlled8 ~ age + female + survyr + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_diabctrl8_educcont_nha <- svyglm(design = surv_dmknown_nha, diab_controlled8 ~ age + female + survyr + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_diabctrl8_educcont_mex <- svyglm(design = surv_dmknown_mex, diab_controlled8 ~ age + female + survyr + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_diabctrl8_educcont_his <- svyglm(design = surv_dmknown_his, diab_controlled8 ~ age + female + survyr + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_diabctrl8_educcont_oth <- svyglm(design = surv_dmknown_oth, diab_controlled8 ~ age + female + survyr + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_diabctrl8_educcont_oth11 <- svyglm(design = surv_dmknown_oth11, diab_controlled8 ~ age + female + survyr + smoke_status + insur_stability + educ_num, family=binomial(link=logit))

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
tiff(file='C:/Users/sarac/OneDrive/Documents/Research/Area v Individual SES/NHANES_SES and AoU_SES/nhanes_forest_indiv_educcont_t2dcontrol.tiff', width=8, height=5, units='in', res=300)
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
           #legend=c("All","All, Race-adjusted",rownames(table(nhanes$racecat))),#c("All","All-Race-adjusted","NHW","NHB","NHA","Mexican-American","Other Hispanic","Other/Multiracial"),
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
mod_diabctrl_inctopov_all <- svyglm(design = surv_diab_known, diab_controlled ~ age + female + survyr + smoke_status + insur_stability + inc_to_pov, family=binomial(link=logit))
mod_diabctrl_inctopov_all_raceadj <- svyglm(design = surv_diab_known, diab_controlled ~ age + female + survyr + smoke_status + insur_stability + inc_to_pov + racecat, family=binomial(link=logit))
mod_diabctrl_inctopov_nhw <- svyglm(design = surv_dmknown_nhw, diab_controlled ~ age + female + survyr + smoke_status + insur_stability + inc_to_pov, family=binomial(link=logit))
mod_diabctrl_inctopov_nhb <- svyglm(design = surv_dmknown_nhb, diab_controlled ~ age + female + survyr + smoke_status + insur_stability + inc_to_pov, family=binomial(link=logit))
mod_diabctrl_inctopov_nha <- svyglm(design = surv_dmknown_nha, diab_controlled ~ age + female + survyr + smoke_status + insur_stability + inc_to_pov, family=binomial(link=logit))
mod_diabctrl_inctopov_mex <- svyglm(design = surv_dmknown_mex, diab_controlled ~ age + female + survyr + smoke_status + insur_stability + inc_to_pov, family=binomial(link=logit))
mod_diabctrl_inctopov_his <- svyglm(design = surv_dmknown_his, diab_controlled ~ age + female + survyr + smoke_status + insur_stability + inc_to_pov, family=binomial(link=logit))
mod_diabctrl_inctopov_oth <- svyglm(design = surv_dmknown_oth, diab_controlled ~ age + female + survyr + smoke_status + insur_stability + inc_to_pov, family=binomial(link=logit))
mod_diabctrl_inctopov_oth11 <- svyglm(design = surv_dmknown_oth11, diab_controlled ~ age + female + survyr + smoke_status + insur_stability + inc_to_pov, family=binomial(link=logit))

mod_diabctrl8_inctopov_all <- svyglm(design = surv_diab_known, diab_controlled8 ~ age + female + survyr + smoke_status + insur_stability + inc_to_pov, family=binomial(link=logit))
mod_diabctrl8_inctopov_all_raceadj <- svyglm(design = surv_diab_known, diab_controlled8 ~ age + female + survyr + smoke_status + insur_stability + inc_to_pov + racecat, family=binomial(link=logit))
mod_diabctrl8_inctopov_nhw <- svyglm(design = surv_dmknown_nhw, diab_controlled8 ~ age + female + survyr + smoke_status + insur_stability + inc_to_pov, family=binomial(link=logit))
mod_diabctrl8_inctopov_nhb <- svyglm(design = surv_dmknown_nhb, diab_controlled8 ~ age + female + survyr + smoke_status + insur_stability + inc_to_pov, family=binomial(link=logit))
mod_diabctrl8_inctopov_nha <- svyglm(design = surv_dmknown_nha, diab_controlled8 ~ age + female + survyr + smoke_status + insur_stability + inc_to_pov, family=binomial(link=logit))
mod_diabctrl8_inctopov_mex <- svyglm(design = surv_dmknown_mex, diab_controlled8 ~ age + female + survyr + smoke_status + insur_stability + inc_to_pov, family=binomial(link=logit))
mod_diabctrl8_inctopov_his <- svyglm(design = surv_dmknown_his, diab_controlled8 ~ age + female + survyr + smoke_status + insur_stability + inc_to_pov, family=binomial(link=logit))
mod_diabctrl8_inctopov_oth <- svyglm(design = surv_dmknown_oth, diab_controlled8 ~ age + female + survyr + smoke_status + insur_stability + inc_to_pov, family=binomial(link=logit))
mod_diabctrl8_inctopov_oth11 <- svyglm(design = surv_dmknown_oth11, diab_controlled8 ~ age + female + survyr + smoke_status + insur_stability + inc_to_pov, family=binomial(link=logit))


mod_diabcont_inctopov_allrace <- data.frame(exp(mod_diabctrl_inctopov_all$coefficients),exp(confint(mod_diabctrl_inctopov_all)),coef(summary(mod_diabctrl_inctopov_all))[,"Pr(>|t|)"],
                                            exp(mod_diabctrl_inctopov_all_raceadj$coefficients)[1:11],exp(confint(mod_diabctrl_inctopov_all_raceadj))[1:11,1:2],coef(summary(mod_diabctrl_inctopov_all_raceadj))[,"Pr(>|t|)"][1:11],
                                            exp(mod_diabctrl_inctopov_nhw$coefficients),exp(confint(mod_diabctrl_inctopov_nhw)),coef(summary(mod_diabctrl_inctopov_nhw))[,"Pr(>|t|)"],
                                            exp(mod_diabctrl_inctopov_nhb$coefficients),exp(confint(mod_diabctrl_inctopov_nhb)),coef(summary(mod_diabctrl_inctopov_nhb))[,"Pr(>|t|)"],
                                            exp(mod_diabctrl_inctopov_mex$coefficients),exp(confint(mod_diabctrl_inctopov_mex)),coef(summary(mod_diabctrl_inctopov_mex))[,"Pr(>|t|)"],
                                            exp(mod_diabctrl_inctopov_his$coefficients),exp(confint(mod_diabctrl_inctopov_his)),coef(summary(mod_diabctrl_inctopov_his))[,"Pr(>|t|)"],
                                            exp(mod_diabctrl_inctopov_nha$coefficients),exp(confint(mod_diabctrl_inctopov_nha)),coef(summary(mod_diabctrl_inctopov_nha))[,"Pr(>|t|)"],
                                            exp(mod_diabctrl_inctopov_oth$coefficients),exp(confint(mod_diabctrl_inctopov_oth)),coef(summary(mod_diabctrl_inctopov_oth))[,"Pr(>|t|)"],
                                            exp(mod_diabctrl_inctopov_oth11$coefficients),exp(confint(mod_diabctrl_inctopov_oth11)),coef(summary(mod_diabctrl_inctopov_oth11))[,"Pr(>|t|)"],
                                            exp(mod_diabctrl8_inctopov_all$coefficients),exp(confint(mod_diabctrl8_inctopov_all)),coef(summary(mod_diabctrl8_inctopov_all))[,"Pr(>|t|)"],
                                            exp(mod_diabctrl8_inctopov_all_raceadj$coefficients)[1:11],exp(confint(mod_diabctrl8_inctopov_all_raceadj))[1:11,1:2],coef(summary(mod_diabctrl8_inctopov_all_raceadj))[,"Pr(>|t|)"][1:11],
                                            exp(mod_diabctrl8_inctopov_nhw$coefficients),exp(confint(mod_diabctrl8_inctopov_nhw)),coef(summary(mod_diabctrl8_inctopov_nhw))[,"Pr(>|t|)"],
                                            exp(mod_diabctrl8_inctopov_nhb$coefficients),exp(confint(mod_diabctrl8_inctopov_nhb)),coef(summary(mod_diabctrl8_inctopov_nhb))[,"Pr(>|t|)"],
                                            exp(mod_diabctrl8_inctopov_mex$coefficients),exp(confint(mod_diabctrl8_inctopov_mex)),coef(summary(mod_diabctrl8_inctopov_mex))[,"Pr(>|t|)"],
                                            exp(mod_diabctrl8_inctopov_his$coefficients),exp(confint(mod_diabctrl8_inctopov_his)),coef(summary(mod_diabctrl8_inctopov_his))[,"Pr(>|t|)"],
                                            exp(mod_diabctrl8_inctopov_nha$coefficients),exp(confint(mod_diabctrl8_inctopov_nha)),coef(summary(mod_diabctrl8_inctopov_nha))[,"Pr(>|t|)"],
                                            exp(mod_diabctrl8_inctopov_oth$coefficients),exp(confint(mod_diabctrl8_inctopov_oth)),coef(summary(mod_diabctrl8_inctopov_oth))[,"Pr(>|t|)"],
                                            exp(mod_diabctrl8_inctopov_oth11$coefficients),exp(confint(mod_diabctrl8_inctopov_oth11)),coef(summary(mod_diabctrl8_inctopov_oth11))[,"Pr(>|t|)"],stringsAsFactors = FALSE)
mod_diabcont_inctopov_allrace_transpose <- t(mod_diabcont_inctopov_allrace)
dim(mod_diabcont_inctopov_allrace)
mod_diabcont_inctopov_allrace[11,c(4,8,12,16,20,24,28,32,36,40,44,48,52,56,60,64,68,72)]
tiff(file='C:/Users/sarac/OneDrive/Documents/Research/Area v Individual SES/NHANES_SES and AoU_SES/nhanes_forest_indiv_inctopov_t2dcontrol.tiff', width=8, height=5, units='in', res=300)
forestplot(c('HbA1c ??? 7.0%','HbA1c ??? 8.0%'), 
           mean = cbind(t(mod_diabcont_inctopov_allrace[11,c(1,37)]),t(mod_diabcont_inctopov_allrace[11,c(5,41)]),
                        t(mod_diabcont_inctopov_allrace[11,c(9,45)]),t(mod_diabcont_inctopov_allrace[11,c(13,49)]),
                        t(mod_diabcont_inctopov_allrace[11,c(17,53)]),t(mod_diabcont_inctopov_allrace[11,c(21,57)]),
                        t(mod_diabcont_inctopov_allrace[11,c(25,61)]),t(mod_diabcont_inctopov_allrace[11,c(29,65)]),
                        t(mod_diabcont_inctopov_allrace[11,c(33,69)])),
           lower = cbind(t(mod_diabcont_inctopov_allrace[11,c(2,38)]),t(mod_diabcont_inctopov_allrace[11,c(6,42)]),
                         t(mod_diabcont_inctopov_allrace[11,c(10,46)]),t(mod_diabcont_inctopov_allrace[11,c(14,50)]),
                         t(mod_diabcont_inctopov_allrace[11,c(18,54)]),t(mod_diabcont_inctopov_allrace[11,c(22,58)]),
                         t(mod_diabcont_inctopov_allrace[11,c(26,62)]),t(mod_diabcont_inctopov_allrace[11,c(30,66)]),
                         t(mod_diabcont_inctopov_allrace[11,c(34,68)])),
           upper = cbind(t(mod_diabcont_inctopov_allrace[11,c(3,39)]),t(mod_diabcont_inctopov_allrace[11,c(7,43)]),
                         t(mod_diabcont_inctopov_allrace[11,c(11,47)]),t(mod_diabcont_inctopov_allrace[11,c(15,51)]),
                         t(mod_diabcont_inctopov_allrace[11,c(19,55)]),t(mod_diabcont_inctopov_allrace[11,c(23,59)]),
                         t(mod_diabcont_inctopov_allrace[11,c(27,63)]),t(mod_diabcont_inctopov_allrace[11,c(31,67)]),
                         t(mod_diabcont_inctopov_allrace[11,c(35,71)])),
           col = fpColors(box = c('gray23','gray57','dodgerblue4','darkslategray3','palegreen3','lightgoldenrod','salmon1','indianred3','mediumpurple4')),
           legend=c("All","All, Race-adjusted","NHW","NHB","Mexican-American","Other Hispanic","NHA","Other/Multi-Racial \n including NHA (pre-2011)","Other/Multi-Racial \n (post-2011)"),
           legend_args = fpLegend(pos = "right"),#list(x = 0.5, y = -0.2), gp = gpar(cex=0.9,col = "#CCCCCC", fill = "#F9F9F9")),
           #legend=c("All","All, Race-adjusted",rownames(table(nhanes$racecat))),#c("All","All-Race-adjusted","NHW","NHB","NHA","Mexican-American","Other Hispanic","Other/Multiracial"),
           #legend_args = fpLegend(pos = list(x = 0.9, y = 0.9), gp = gpar(cex=0.8,col = "#CCCCCC", fill = "#F9F9F9")),
           xlab = "OR (95% CI)",
           title = 'Adjusted OR for Continuous Income-to-Poverty Ratio on T2D Control, \n Overall and Stratified by Race/Ethnicity',
           txt_gp = fpTxtGp(title=gpar(cex=1),xlab=gpar(cex=1), ticks=gpar(cex=1), label=gpar(cex=1)),
           vertices=TRUE,
           line.margin=0.25,
           graph.pos=2,
           clip=c(0.5,2), xlim=c(0.5,2), xticks=c(0.5,0.75,1.0,1.25,1.5,1.75,2.0), xlog=TRUE, grid=structure(c(0.5,0.75,1.0,1.25,1.5,1.75,2.0)))
dev.off()
table(nhanes$racecat, nhanes$diab_controlled, exclude=NULL)
table(nhanes[!is.na(nhanes$LBXGH),]$racecat, nhanes[!is.na(nhanes$LBXGH),]$diab_controlled, exclude=NULL)


#####all I2 and influence measures#####
i2_all <- cbind(i2_educ_diab, i2_educ_ob, i2_inc_diab, i2_inc_ob)
rownames(i2_all) <- c('None',rownames(table(nhanes$racecat)))
colnames(i2_all) <- c('educ_diab','educ_ob','inc_diab','inc_ob')
i2_all

influence(res_educ_diab, progbar=F)
influence(res_educ_ob, progbar=F)
influence(res_inc_diab, progbar=F)
influence(res_inc_ob, progbar=F)




#####combined educ and income - diabetes, obesity prevalence#####
mod_diab_educcont_all <- svyglm(design = surv_all, diab ~ age + female + survyr + smoke_status + insur_stability + educ_num, family=binomial(link=logit))
mod_diab_educinc_all <- svyglm(design = surv_all, diab ~ age + female + survyr + smoke_status + insur_stability + educ_num + inc_to_pov, family=binomial(link=logit))
summary(mod_diab_educcont_all)
summary(mod_diab_educinc_all)
NagelkerkeR2(mod_diab_educcont_all) #0.1554
NagelkerkeR2(mod_diab_educinc_all) #0.1606
exp(mod_diab_educcont_all$coefficients)
exp(mod_diab_educinc_all$coefficients)

mod_diab_educinc_all_raceadj <- svyglm(design = surv_all, diab ~ age + female + survyr + smoke_status + insur_stability + educ_num + inc_to_pov + racecat, family=binomial(link=logit))
mod_diab_educinc_nhw <- svyglm(design = surv_nhw, diab ~ age + female + survyr + smoke_status + insur_stability + educ_num + inc_to_pov, family=binomial(link=logit))
mod_diab_educinc_nhb <- svyglm(design = surv_nhb, diab ~ age + female + survyr + smoke_status + insur_stability + educ_num + inc_to_pov, family=binomial(link=logit))
summary(mod_diab_educinc_nhb)
exp(mod_diab_educinc_nhb$coefficients)
mod_diab_educinc_nha <- svyglm(design = surv_nha, diab ~ age + female + survyr + smoke_status + insur_stability + educ_num + inc_to_pov, family=binomial(link=logit))
mod_diab_educinc_mex <- svyglm(design = surv_mex, diab ~ age + female + survyr + smoke_status + insur_stability + educ_num + inc_to_pov, family=binomial(link=logit))
mod_diab_educinc_his <- svyglm(design = surv_his, diab ~ age + female + survyr + smoke_status + insur_stability + educ_num + inc_to_pov, family=binomial(link=logit))
mod_diab_educinc_oth <- svyglm(design = surv_oth, diab ~ age + female + survyr + smoke_status + insur_stability + educ_num + inc_to_pov, family=binomial(link=logit))
mod_diab_educinc_oth11 <- svyglm(design = surv_oth11, diab ~ age + female + survyr + smoke_status + insur_stability + educ_num + inc_to_pov, family=binomial(link=logit))

mod_obes_educinc_all <- svyglm(design = surv_all, obese ~ age + female + survyr + smoke_status + insur_stability + educ_num + inc_to_pov, family=binomial(link=logit))
mod_obes_educinc_all_raceadj <- svyglm(design = surv_all, obese ~ age + female + survyr + smoke_status + insur_stability + educ_num + inc_to_pov + racecat, family=binomial(link=logit))
mod_obes_educinc_nhw <- svyglm(design = surv_nhw, obese ~ age + female + survyr + smoke_status + insur_stability + educ_num + inc_to_pov, family=binomial(link=logit))
mod_obes_educinc_nhb <- svyglm(design = surv_nhb, obese ~ age + female + survyr + smoke_status + insur_stability + educ_num + inc_to_pov, family=binomial(link=logit))
mod_obes_educinc_nha <- svyglm(design = surv_nha, obese ~ age + female + survyr + smoke_status + insur_stability + educ_num + inc_to_pov, family=binomial(link=logit))
mod_obes_educinc_mex <- svyglm(design = surv_mex, obese ~ age + female + survyr + smoke_status + insur_stability + educ_num + inc_to_pov, family=binomial(link=logit))
mod_obes_educinc_his <- svyglm(design = surv_his, obese ~ age + female + survyr + smoke_status + insur_stability + educ_num + inc_to_pov, family=binomial(link=logit))
mod_obes_educinc_oth <- svyglm(design = surv_oth, obese ~ age + female + survyr + smoke_status + insur_stability + educ_num + inc_to_pov, family=binomial(link=logit))
mod_obes_educinc_oth11 <- svyglm(design = surv_oth11, obese ~ age + female + survyr + smoke_status + insur_stability + educ_num + inc_to_pov, family=binomial(link=logit))

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

#adjusted ORs for insur_stability, educ_num, inc_to_pov summary
fill <- c(999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,
          999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999)
r2_educcont <- rbind(NagelkerkeR2(mod_diab_educcont_all)$R2,NagelkerkeR2(mod_diab_educcont_all_raceadj)$R2,NagelkerkeR2(mod_diab_educcont_nhw)$R2,
                     NagelkerkeR2(mod_diab_educcont_nhb)$R2,NagelkerkeR2(mod_diab_educcont_mex)$R2,NagelkerkeR2(mod_diab_educcont_his)$R2,
                     NagelkerkeR2(mod_diab_educcont_nha)$R2,NagelkerkeR2(mod_diab_educcont_oth)$R2,NagelkerkeR2(mod_diab_educcont_oth11)$R2,
                     NagelkerkeR2(mod_obes_educcont_all)$R2,NagelkerkeR2(mod_obes_educcont_all_raceadj)$R2,NagelkerkeR2(mod_obes_educcont_nhw)$R2,
                     NagelkerkeR2(mod_obes_educcont_nhb)$R2,NagelkerkeR2(mod_obes_educcont_mex)$R2,
                     NagelkerkeR2(mod_obes_educcont_his)$R2,NagelkerkeR2(mod_obes_educcont_nha)$R2,NagelkerkeR2(mod_obes_educcont_oth)$R2,
                     NagelkerkeR2(mod_obes_educcont_oth11)$R2)
r2_inc2pov <- rbind(NagelkerkeR2(mod_diab_inctopov_all)$R2,NagelkerkeR2(mod_diab_inctopov_all_raceadj)$R2,NagelkerkeR2(mod_diab_inctopov_nhw)$R2,
                     NagelkerkeR2(mod_diab_inctopov_nhb)$R2,NagelkerkeR2(mod_diab_inctopov_mex)$R2,NagelkerkeR2(mod_diab_inctopov_his)$R2,
                     NagelkerkeR2(mod_diab_inctopov_nha)$R2,NagelkerkeR2(mod_diab_inctopov_oth)$R2,NagelkerkeR2(mod_diab_inctopov_oth11)$R2,
                     NagelkerkeR2(mod_obes_inctopov_all)$R2,NagelkerkeR2(mod_obes_inctopov_all_raceadj)$R2,NagelkerkeR2(mod_obes_inctopov_nhw)$R2,
                     NagelkerkeR2(mod_obes_inctopov_nhb)$R2,NagelkerkeR2(mod_obes_inctopov_mex)$R2,
                     NagelkerkeR2(mod_obes_inctopov_his)$R2,NagelkerkeR2(mod_obes_inctopov_nha)$R2,NagelkerkeR2(mod_obes_inctopov_oth)$R2,
                     NagelkerkeR2(mod_obes_inctopov_oth11)$R2)
r2_educinc <- rbind(NagelkerkeR2(mod_diab_educinc_all)$R2,NagelkerkeR2(mod_diab_educinc_all_raceadj)$R2,NagelkerkeR2(mod_diab_educinc_nhw)$R2,
                     NagelkerkeR2(mod_diab_educinc_nhb)$R2,NagelkerkeR2(mod_diab_educinc_mex)$R2,NagelkerkeR2(mod_diab_educinc_his)$R2,
                     NagelkerkeR2(mod_diab_educinc_nha)$R2,NagelkerkeR2(mod_diab_educinc_oth)$R2,NagelkerkeR2(mod_diab_educinc_oth11)$R2,
                     NagelkerkeR2(mod_obes_educinc_all)$R2,NagelkerkeR2(mod_obes_educinc_all_raceadj)$R2,NagelkerkeR2(mod_obes_educinc_nhw)$R2,
                     NagelkerkeR2(mod_obes_educinc_nhb)$R2,NagelkerkeR2(mod_obes_educinc_mex)$R2,NagelkerkeR2(mod_obes_educinc_his)$R2,
                      NagelkerkeR2(mod_obes_educinc_nha)$R2,NagelkerkeR2(mod_obes_educinc_oth)$R2,NagelkerkeR2(mod_obes_educinc_oth11)$R2)
results_educinc_combined <- cbind(rbind(mod_alldx_educcont_allrace[7:11,1:72], fill),
                                  rbind(mod_alldx_inctopov_allrace[7:10,1:72],fill,mod_alldx_inctopov_allrace[11,1:72]),
                                  mod_diabob_educinc_allrace[7:12,])
dim(results_educinc_combined)
r2_educinc_combined <- cbind(r2_educcont, r2_inc2pov, r2_educinc)

write.csv(results_educinc_combined, 'C:/Users/sarac/OneDrive/Documents/Research/Area v Individual SES/NHANES_SES and AoU_SES/nhanes_educinc_combmodels.csv',row.names=TRUE)




#####models without adjustment for insurance#####
#educ only
mod_diab_educcont_noins_all <- svyglm(design = surv_all, diab ~ age + female + survyr + smoke_status + educ_num, family=binomial(link=logit))
mod_diab_educcont_noins_all_raceadj <- svyglm(design = surv_all, diab ~ age + female + survyr + smoke_status + educ_num + racecat, family=binomial(link=logit))
mod_diab_educcont_noins_nhw <- svyglm(design = surv_nhw, diab ~ age + female + survyr + smoke_status + educ_num, family=binomial(link=logit))
mod_diab_educcont_noins_nhb <- svyglm(design = surv_nhb, diab ~ age + female + survyr + smoke_status + educ_num, family=binomial(link=logit))
mod_diab_educcont_noins_nha <- svyglm(design = surv_nha, diab ~ age + female + survyr + smoke_status + educ_num, family=binomial(link=logit))
mod_diab_educcont_noins_mex <- svyglm(design = surv_mex, diab ~ age + female + survyr + smoke_status + educ_num, family=binomial(link=logit))
mod_diab_educcont_noins_his <- svyglm(design = surv_his, diab ~ age + female + survyr + smoke_status + educ_num, family=binomial(link=logit))
mod_diab_educcont_noins_oth <- svyglm(design = surv_oth, diab ~ age + female + survyr + smoke_status + educ_num, family=binomial(link=logit))
mod_diab_educcont_noins_oth11 <- svyglm(design = surv_oth11, diab ~ age + female + survyr + smoke_status + educ_num, family=binomial(link=logit))

mod_obes_educcont_noins_all <- svyglm(design = surv_all, obese ~ age + female + survyr + smoke_status + educ_num, family=binomial(link=logit))
mod_obes_educcont_noins_all_raceadj <- svyglm(design = surv_all, obese ~ age + female + survyr + smoke_status + educ_num + racecat, family=binomial(link=logit))
mod_obes_educcont_noins_nhw <- svyglm(design = surv_nhw, obese ~ age + female + survyr + smoke_status + educ_num, family=binomial(link=logit))
mod_obes_educcont_noins_nhb <- svyglm(design = surv_nhb, obese ~ age + female + survyr + smoke_status + educ_num, family=binomial(link=logit))
mod_obes_educcont_noins_nha <- svyglm(design = surv_nha, obese ~ age + female + survyr + smoke_status + educ_num, family=binomial(link=logit))
mod_obes_educcont_noins_mex <- svyglm(design = surv_mex, obese ~ age + female + survyr + smoke_status + educ_num, family=binomial(link=logit))
mod_obes_educcont_noins_his <- svyglm(design = surv_his, obese ~ age + female + survyr + smoke_status + educ_num, family=binomial(link=logit))
mod_obes_educcont_noins_oth <- svyglm(design = surv_oth, obese ~ age + female + survyr + smoke_status + educ_num, family=binomial(link=logit))
mod_obes_educcont_noins_oth11 <- svyglm(design = surv_oth11, obese ~ age + female + survyr + smoke_status + educ_num, family=binomial(link=logit))

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
mod_diab_inc2pov_noins_all <- svyglm(design = surv_all, diab ~ age + female + survyr + smoke_status + inc_to_pov, family=binomial(link=logit))
mod_diab_inc2pov_noins_all_raceadj <- svyglm(design = surv_all, diab ~ age + female + survyr + smoke_status + inc_to_pov + racecat, family=binomial(link=logit))
mod_diab_inc2pov_noins_nhw <- svyglm(design = surv_nhw, diab ~ age + female + survyr + smoke_status + inc_to_pov, family=binomial(link=logit))
mod_diab_inc2pov_noins_nhb <- svyglm(design = surv_nhb, diab ~ age + female + survyr + smoke_status + inc_to_pov, family=binomial(link=logit))
mod_diab_inc2pov_noins_nha <- svyglm(design = surv_nha, diab ~ age + female + survyr + smoke_status + inc_to_pov, family=binomial(link=logit))
mod_diab_inc2pov_noins_mex <- svyglm(design = surv_mex, diab ~ age + female + survyr + smoke_status + inc_to_pov, family=binomial(link=logit))
mod_diab_inc2pov_noins_his <- svyglm(design = surv_his, diab ~ age + female + survyr + smoke_status + inc_to_pov, family=binomial(link=logit))
mod_diab_inc2pov_noins_oth <- svyglm(design = surv_oth, diab ~ age + female + survyr + smoke_status + inc_to_pov, family=binomial(link=logit))
mod_diab_inc2pov_noins_oth11 <- svyglm(design = surv_oth11, diab ~ age + female + survyr + smoke_status + inc_to_pov, family=binomial(link=logit))

mod_obes_inc2pov_noins_all <- svyglm(design = surv_all, obese ~ age + female + survyr + smoke_status + inc_to_pov, family=binomial(link=logit))
mod_obes_inc2pov_noins_all_raceadj <- svyglm(design = surv_all, obese ~ age + female + survyr + smoke_status + inc_to_pov + racecat, family=binomial(link=logit))
mod_obes_inc2pov_noins_nhw <- svyglm(design = surv_nhw, obese ~ age + female + survyr + smoke_status + inc_to_pov, family=binomial(link=logit))
mod_obes_inc2pov_noins_nhb <- svyglm(design = surv_nhb, obese ~ age + female + survyr + smoke_status + inc_to_pov, family=binomial(link=logit))
mod_obes_inc2pov_noins_nha <- svyglm(design = surv_nha, obese ~ age + female + survyr + smoke_status + inc_to_pov, family=binomial(link=logit))
mod_obes_inc2pov_noins_mex <- svyglm(design = surv_mex, obese ~ age + female + survyr + smoke_status + inc_to_pov, family=binomial(link=logit))
mod_obes_inc2pov_noins_his <- svyglm(design = surv_his, obese ~ age + female + survyr + smoke_status + inc_to_pov, family=binomial(link=logit))
mod_obes_inc2pov_noins_oth <- svyglm(design = surv_oth, obese ~ age + female + survyr + smoke_status + inc_to_pov, family=binomial(link=logit))
mod_obes_inc2pov_noins_oth11 <- svyglm(design = surv_oth11, obese ~ age + female + survyr + smoke_status + inc_to_pov, family=binomial(link=logit))

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
mod_diab_educinc_noins_all <- svyglm(design = surv_all, diab ~ age + female + survyr + smoke_status + educ_num + inc_to_pov, family=binomial(link=logit))
mod_diab_educinc_noins_all_raceadj <- svyglm(design = surv_all, diab ~ age + female + survyr + smoke_status + educ_num + inc_to_pov + racecat, family=binomial(link=logit))
mod_diab_educinc_noins_nhw <- svyglm(design = surv_nhw, diab ~ age + female + survyr + smoke_status + educ_num + inc_to_pov, family=binomial(link=logit))
mod_diab_educinc_noins_nhb <- svyglm(design = surv_nhb, diab ~ age + female + survyr + smoke_status + educ_num + inc_to_pov, family=binomial(link=logit))
mod_diab_educinc_noins_nha <- svyglm(design = surv_nha, diab ~ age + female + survyr + smoke_status + educ_num + inc_to_pov, family=binomial(link=logit))
mod_diab_educinc_noins_mex <- svyglm(design = surv_mex, diab ~ age + female + survyr + smoke_status + educ_num + inc_to_pov, family=binomial(link=logit))
mod_diab_educinc_noins_his <- svyglm(design = surv_his, diab ~ age + female + survyr + smoke_status + educ_num + inc_to_pov, family=binomial(link=logit))
mod_diab_educinc_noins_oth <- svyglm(design = surv_oth, diab ~ age + female + survyr + smoke_status + educ_num + inc_to_pov, family=binomial(link=logit))
mod_diab_educinc_noins_oth11 <- svyglm(design = surv_oth11, diab ~ age + female + survyr + smoke_status + educ_num + inc_to_pov, family=binomial(link=logit))

mod_obes_educinc_noins_all <- svyglm(design = surv_all, obese ~ age + female + survyr + smoke_status + educ_num + inc_to_pov, family=binomial(link=logit))
mod_obes_educinc_noins_all_raceadj <- svyglm(design = surv_all, obese ~ age + female + survyr + smoke_status + educ_num + inc_to_pov + racecat, family=binomial(link=logit))
mod_obes_educinc_noins_nhw <- svyglm(design = surv_nhw, obese ~ age + female + survyr + smoke_status + educ_num + inc_to_pov, family=binomial(link=logit))
mod_obes_educinc_noins_nhb <- svyglm(design = surv_nhb, obese ~ age + female + survyr + smoke_status + educ_num + inc_to_pov, family=binomial(link=logit))
mod_obes_educinc_noins_nha <- svyglm(design = surv_nha, obese ~ age + female + survyr + smoke_status + educ_num + inc_to_pov, family=binomial(link=logit))
mod_obes_educinc_noins_mex <- svyglm(design = surv_mex, obese ~ age + female + survyr + smoke_status + educ_num + inc_to_pov, family=binomial(link=logit))
mod_obes_educinc_noins_his <- svyglm(design = surv_his, obese ~ age + female + survyr + smoke_status + educ_num + inc_to_pov, family=binomial(link=logit))
mod_obes_educinc_noins_oth <- svyglm(design = surv_oth, obese ~ age + female + survyr + smoke_status + educ_num + inc_to_pov, family=binomial(link=logit))
mod_obes_educinc_noins_oth11 <- svyglm(design = surv_oth11, obese ~ age + female + survyr + smoke_status + educ_num + inc_to_pov, family=binomial(link=logit))

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
  
write.csv(mods_noins, 'C:/Users/sarac/OneDrive/Documents/Research/Area v Individual SES/NHANES_SES and AoU_SES/nhanes_educinc_noins_combmodels.csv',row.names=TRUE)

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







colnames(nhanes)
write.csv(nhanes, '//Cifs2/lookaheadses$/NHANES_99_18/nhanes_df_full.csv', row.names=FALSE) 

#####make data set smaller chunks#####
nhanes9902 <- nhanes[nhanes$survyr== 1999 | nhanes$survyr == 2001,]
nhanes0306 <- nhanes[nhanes$survyr== 2003 | nhanes$survyr == 2005,]
nhanes0710 <- nhanes[nhanes$survyr== 2007 | nhanes$survyr == 2009,]
nhanes1114 <- nhanes[nhanes$survyr== 2011 | nhanes$survyr == 2013,]
nhanes1518 <- nhanes[nhanes$survyr== 2015 | nhanes$survyr == 2017,]
write.csv(nhanes9902, '//Cifs2/lookaheadses$/NHANES_99_18/nhanes_df_9902.csv', row.names=FALSE) 
write.csv(nhanes0306, '//Cifs2/lookaheadses$/NHANES_99_18/nhanes_df_0306.csv', row.names=FALSE) 
write.csv(nhanes0710, '//Cifs2/lookaheadses$/NHANES_99_18/nhanes_df_0710.csv', row.names=FALSE) 
write.csv(nhanes1114, '//Cifs2/lookaheadses$/NHANES_99_18/nhanes_df_1114.csv', row.names=FALSE) 
write.csv(nhanes1518, '//Cifs2/lookaheadses$/NHANES_99_18/nhanes_df_1518.csv', row.names=FALSE) 
rm(nhanes9902,nhanes0306,nhanes0710,nhanes1114,nhanes1518)