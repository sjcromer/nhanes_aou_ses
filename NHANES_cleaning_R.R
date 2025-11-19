#####Indiv v area-level SES in Geo-NHANES v ACS#####

setwd('C:/Users/sarac/OneDrive/Documents/Research')
#update.packages()
library(tidyverse)
library(dplyr)
library(ggplot2)
library(nhanesA)
library(progress)
#install.packages('sociome')
#library(sociome)


#####TABLES DOWNLOAD#####
# download nhanes tables using nhanesA
#create functions
download_component_table <- function(componentname, year) {
  table_names <- nhanesTables(componentname, year, namesonly = T)  
  tabs <- lapply(table_names, nhanes)
  names(tabs) <- table_names
  tabs
}

download_variable_names <- function(componentname, year) {
  table_names <- nhanesTables(componentname, year)
  map(table_names$Data.File.Name,~nhanesTableVars(componentname, .x, details=TRUE, nchar=50))
}

progress_bar_nh <- function(bar_length) {
  progress_bar$new(clear=FALSE, width=100, total = bar_length, format = "  downloading [:bar] :percent eta: :eta elapsed: :elapsed")
} # R will report % progress of downloads

## years: 1999-2018
years <- c(1999, 2001, 2003, 2005, 2007, 2009, 2011, 2013, 2015, 2017)
yearslab <- c(1999, 2001, 2003, 2005, 2007, 2009, 2011, 2013, 2015, 2017)
yearsexam <- c(1999, 2001, 2013, 2015, 2017)


pb <- progress_bar_nh(length(years))
demo_tables <- map(years, ~{pb$tick(); download_component_table("DEMO", .x)}) # index maps to each year 
demo_tables[1:10]

pb <- progress_bar_nh(length(years))
lab_tables <- map(yearslab, ~{pb$tick(); download_component_table("LABORATORY", .x)}) # index maps to each year 
lab_tables[1:10]

pb <- progress_bar_nh(length(years))
questionnaire_tables <- map(years, ~{pb$tick(); download_component_table("QUESTIONNAIRE", .x)}) # index maps to each year 

pb <- progress_bar_nh(length(years))
exam_tables <- map(years, ~{pb$tick(); download_component_table("EXAM", .x)}) # index maps to each year 


dim(exam_tables)
colnames(exam_tables)

exam_tables_relevant <- exam_tables[,c()]

require(foreign) #converts .xpt to R data file

save(demo_tables, lab_tables, questionnaire_tables, exam_tables, file='nhanes_99_18.Rdata')


#####VARIABLE LIST#####

vars_to_get <- rbind(
  tibble(varname=c(
    "SEQN","SDDSRVYR",
    "RIDAGEYR",
    "CBD770","RIAGENDR",
    "RIDRETH1",'RIDRETH2','RIDRETH3',
    "SDMVSTRA",'SDMVPSU',
    "WTINT2YR",'WTINT4YR','WTMEC2YR','WTMEC4YR','WTSAF2YR','WTSAF4YR','WTSAFPRP','WTXB2YR','WTSPH2YR','WTSSBH2Y','WTSA2YR','WTSH2YR','WTSAPRP',
    "WTSPH01","WTSPH02","WTSPH03","WTSPH04","WTSPH05","WTSPH06","WTSPH07","WTSPH08","WTSPH09","WTSPH10",
    "WTSPH11","WTSPH12","WTSPH13","WTSPH14","WTSPH15","WTSPH16","WTSPH17","WTSPH18","WTSPH19","WTSPH20",
    "WTSPH21","WTSPH22","WTSPH23","WTSPH24","WTSPH25","WTSPH26","WTSPH27","WTSPH28","WTSPH29","WTSPH30",
    "WTSPH31","WTSPH32","WTSPH33","WTSPH34","WTSPH35","WTSPH36","WTSPH37","WTSPH38","WTSPH39","WTSPH40",
    "WTSPH41","WTSPH42","WTSPH43","WTSPH44","WTSPH45","WTSPH46","WTSPH47","WTSPH48","WTSPH49","WTSPH50","WTSPH51","WTSPH52",
    "DMDEDUC2","DMDHREDU","DMDHSEDU",'DMDHREDZ','INDFMIN2','INDFMINC','INDHHIN2','INDHHINC','INDFMPIR',
    'OCQ150','OCD160','OCD180','OCD210',
    "LBDLDL",
    "LBXGH"), module="DEMO"),
  tibble(varname = c(
    'SEQN',"LBXGH",
    "LB2GLU","LB2SGL","LBXGLU",
    "LBXGB","LBXHCT","LB2HCT","LBXMCVSI","LB2MCVSI","LBXMC","LB2MC","LBXRDW","LB2RDW","LBXWBCSI","LBXNEPCT",
    "LB2NEPCT","LBXLYPCT","LB2LYPCT","LBXEOPCT","LB2EOPCT","LBXPLTSI","LB2PLTSI",
    "LBXSNASI","LB2SNASI","LBXSKSI","LBXSCLSI","LB2SCLSI","LBXSC3SI","LB2SC3SI","LBXSBU","SB2SBU","LB2SCR",
    "LBDSCR","LBXSCR","LBXSAL","LB2SAL","LBXSTP","LB2STP","LBXSASSI","LB2SASSI","LBXSATSI","LB2SATSI","LBXSTB","LB2STB","LBDSTB",
    "LBXTC","LB2TC","LBXHDL","LB2HDL","LBXLDL","LB2LDL","LBDLDL","LBXTR","LB2TR","LB2STR","LBXSTR",
    "SSECPT","SSMHHT","SSMONP","SSURHIBP","SSURMHBP","URXCNP","URXCOP","URXDAZ","URXDMA","URXECP","URXECPT","URXEQU",
    "URXETD","URXETL","URXGNS","URXHIBP","URXMBP","URXMC1","URXMCOH","URXMCP","URXMEP","URXMHBP","URXMHH","URXMHHT",
    "URXMHNC","URXMHP","URXMIB","URXMNM","URXMNP","URXMOH","URXMONP","URXMOP","URXMZP",
    "URXP01","URXP02","URXP03","URXP04","URXP05","URXP06","URXP07","URXP09","URXP10","URXUCR",
    "HRDHG","HRXHG","LB2THG","LBXTHG","LBDTHGSI","LBDBGE","LBDBGM","LBDIHG","LBXIHG","LBXBGE","LBXBGM",
    "URXUHG","LB2BCD","LBDBCD","LBXBCD","LB2BPB","LBXBPB","LBDBMN","LBDBSE","LBXBSE","LBDSEL","LBXSEL","LBXBMN",
    "LB2FOL","LBDFOL","LBXFOL"," LB2RBF","LBXRBF","LB2B12","LBDB12","LBXB12","LB2HCY","LBDHCY","LBXHCY","LB2MMA",
    "LBXMMA","LBDVIASI","LBXVIA","LBDVIESI","LBXVIE","LB2COT","LBXCOT","LBDGTC","LBXGTC","LBDRPLSI","LBXRPL","LBDRSTSI","LBXRST","LBXEPP",
    "LB2IN",
    "LBXCRP","LB2CRP","LBXHSCRP","LBDHRPLC",
    "LBDBAP","LBXBAP",
    "LBXFB","LBDFBSI",
    "LBXPT21",
    "LBDHI",
    "LBDAPBSI",
    "LBDIRN","LBXIRN","LB2FER","LBXFER","LBDTIBSI","LBXTIB","LBXPCT"), module="LABORATORY"),
  tibble(varname=c(
    'SEQN',"MCQ080",'MCQ160j',
    'DIQ160','DIQ010',
    'DID040','DIQ040G','DIQ040Q',
    'DIQ170','DIQ172',
    'DIQ175A','DIQ175B','DIQ175C','DIQ175D','DIQ175E','DIQ175F','DIQ175G','DIQ175H','DIQ175I','DIQ175J','DIQ175K','DIQ175L',
    'DIQ175M','DIQ175N','DIQ175O','DIQ175P','DIQ175Q','DIQ175R','DIQ175S','DIQ175T','DIQ175U','DIQ175V','DIQ175W','DIQ175X',
    'MCQ100','BPQ030',
    'MCQ160C',
    'KIQ020','OHQ144',
    'MCQ160p','MCQ160o','MCQ160g','MCQ160k',
    'WHD010','WHD020',
    'CIDGSCOR',
    'DIQ050','DIQ070','DID070',
    'WHD080I','WHD080J',
    'MCQ110','BPQ040',
    'MCQ051',
    'CIQDPB'), module="QUESTIONNAIRE"),
  tibble(varname = c(
    'SEQN',"BMXBMI","BMXWT","BMXHT","BMI",
    "BPXSY1","BPXSY2","BPXSY3","BPXSAR",
    'BPXDI1','BPXDI2','BPXDI3','BPXDAR'), module="EXAMINATION")
)


load('./nhanes_99_18.Rdata')
years <- c(1999, 2001, 2003, 2005, 2007, 2009, 2011, 2013, 2015, 2017)


#####DEMO/SES DATA CLEANING - age/gender/race/educ/income/occup#####
## flattening the demo table
demo_table <- lapply(demo_tables, function(arr) {arr[[1]]}) %>% bind_rows()
colnames(demo_table)
vars_to_get_demo <- c("SEQN","SDDSRVYR", "RIDAGEYR", "CBD770", "RIAGENDR","RIDRETH1",'RIDRETH2','RIDRETH3', "SDMVSTRA",'SDMVPSU',
                      "WTINT2YR",'WTINT4YR','WTMEC2YR','WTMEC4YR','WTSAF2YR','WTSAF4YR','WTSAFPRP','WTXB2YR','WTSPH2YR','WTSSBH2Y','WTSA2YR','WTSH2YR','WTSAPRP',
                      "WTSPH01","WTSPH02","WTSPH03","WTSPH04","WTSPH05","WTSPH06","WTSPH07","WTSPH08","WTSPH09","WTSPH10",
                      "WTSPH11","WTSPH12","WTSPH13","WTSPH14","WTSPH15","WTSPH16","WTSPH17","WTSPH18","WTSPH19","WTSPH20",
                      "WTSPH21","WTSPH22","WTSPH23","WTSPH24","WTSPH25","WTSPH26","WTSPH27","WTSPH28","WTSPH29","WTSPH30",
                      "WTSPH31","WTSPH32","WTSPH33","WTSPH34","WTSPH35","WTSPH36","WTSPH37","WTSPH38","WTSPH39","WTSPH40",
                      "WTSPH41","WTSPH42","WTSPH43","WTSPH44","WTSPH45","WTSPH46","WTSPH47","WTSPH48","WTSPH49","WTSPH50","WTSPH51","WTSPH52",
                      "DMDEDUC2","DMDHREDU",'DMDHREDZ',"DMDHSEDU",'INDFMIN2','INDFMINC','INDHHIN2','INDHHINC','INDFMPIR',
                      'OCQ150','OCD160','OCD180','OCD210',
                      "LBDLDL",
                      "LBXGH")
demo_table_short <- demo_table[, (names(demo_table) %in% vars_to_get_demo)]
colnames(demo_table_short)

#did not work: demo_table_short[demo_table_short=="<NA>"] <- NA
#did not work: demo_table_short[demo_table_short=="NA"] <- NA
demo_table_short$age <- demo_table_short$RIDAGEYR
demo_table_short$age_ge_16 <- ifelse(demo_table_short$age >=16,1,0)
demo_table_short$female <- ifelse(demo_table_short$RIAGENDR==2,1, ifelse(demo_table_short$RIAGENDR==1,0, 99))
demo_table_short$racecat <- ifelse(demo_table_short$RIDRETH1==3,'NHW', ifelse(demo_table_short$RIDRETH1==4,'NHB', ifelse(demo_table_short$RIDRETH1==1,'Mexican-American', ifelse(demo_table_short$RIDRETH1==2,'Other Hispanic', ifelse(demo_table_short$RIDRETH1==5 & demo_table_short$RIDRETH3==6,'NHAsian', ifelse(demo_table_short$RIDRETH1==5,'Other/Multi-Racial', 'Missing'))))))
demo_table_short$racecat <- ifelse(is.na(demo_table_short$racecat) | demo_table_short$racecat=='<NA','Other/Multi-Racial', demo_table_short$racecat)
table(demo_table_short$racecat, exclude=NULL)
table(demo_table_short$RIDRETH1, demo_table_short$RIDRETH3, exclude=NULL)
table(demo_table_short$survyr, demo_table_short$respondeduc, exclude=NULL)
summary(demo_table_short$DMDEDUC2)
demo_table_short$survyr <- ifelse(demo_table_short$SDDSRVYR==1,1999, ifelse(demo_table_short$SDDSRVYR==2,2001, ifelse(demo_table_short$SDDSRVYR==3,2003, ifelse(demo_table_short$SDDSRVYR==4,2005, ifelse(demo_table_short$SDDSRVYR==5,2007, ifelse(demo_table_short$SDDSRVYR==6,2009, ifelse(demo_table_short$SDDSRVYR==7,2011, ifelse(demo_table_short$SDDSRVYR==8,2013, ifelse(demo_table_short$SDDSRVYR==9,2015, ifelse(demo_table_short$SDDSRVYR==10,2017,'Missing'))))))))))
demo_table_short$respondeduc <- ifelse(demo_table_short$DMDEDUC2=='<NA>' | is.na(demo_table_short$DMDEDUC2),'Missing', ifelse(demo_table_short$DMDEDUC2==1,'Less Than 9th Grade', ifelse(demo_table_short$DMDEDUC2==2,'9-11th Grade', ifelse(demo_table_short$DMDEDUC2==3,'High School Degree or GED', ifelse(demo_table_short$DMDEDUC2==4,'Some College or Associates Degree', ifelse(demo_table_short$DMDEDUC2==5,'College Degree or Higher', ifelse(demo_table_short$DMDEDUC2==7,'Refused', ifelse(demo_table_short$DMDEDUC2==9,'Do not know', 'Missing'))))))))
demo_table_short$hreduc_99_16 <- ifelse(demo_table_short$DMDHREDU=='<NA>' | is.na(demo_table_short$DMDHREDU),'Missing', ifelse(demo_table_short$DMDHREDU==1,'Less Than 9th Grade', ifelse(demo_table_short$DMDHREDU==2,'9-11th Grade', ifelse(demo_table_short$DMDHREDU==3,'High School Degree or GED', ifelse(demo_table_short$DMDHREDU==4,'Some College or Associates Degree', ifelse(demo_table_short$DMDHREDU==5,'College Degree or Higher', ifelse(demo_table_short$DMDHREDU==7,'Refused', ifelse(demo_table_short$DMDHREDU==9,'Do not know', 'Missing'))))))))
demo_table_short$hreduc_17_18 <- ifelse(demo_table_short$DMDHREDZ=='<NA>' | is.na(demo_table_short$DMDHREDZ),'Missing', ifelse(demo_table_short$DMDHREDZ==1,'Less Than High School Degree', ifelse(demo_table_short$DMDHREDZ==2,'High School Degree or GED or Some College or Associates Degree', ifelse(demo_table_short$DMDHREDZ==3,'College Degree or Higher', ifelse(demo_table_short$DMDHREDZ==7,'Refused', ifelse(demo_table_short$DMDHREDZ==9,'Do not know', 'Missing'))))))
demo_table_short$hreduc <-ifelse(demo_table_short$survyr==2017, demo_table_short$hreduc_17_18, demo_table_short$hreduc_99_16)
table(demo_table_short$hreduc, exclude=NULL)
summary(aov(demo_table_short$RIDAGEYR ~ demo_table_short$respondeduc))
mean(demo_table_short$RIDAGEYR[demo_table_short$respondeduc=='Missing'], na.rm=T)

demo_table_short$hhinc_99_06 <- ifelse(demo_table_short$INDHHINC=='<NA>' | is.na(demo_table_short$INDHHINC),'Missing', ifelse(demo_table_short$INDHHINC==1,'$0-4,999', ifelse(demo_table_short$INDHHINC==2,'$5,000-9,999',
                                ifelse(demo_table_short$INDHHINC==3,'$10,000-14,999', ifelse(demo_table_short$INDHHINC==4,'$15,000-19,999', ifelse(demo_table_short$INDHHINC==5,'$20,000-24,999', ifelse(demo_table_short$INDHHINC==6,'$25,000-34,999',
                                ifelse(demo_table_short$INDHHINC==7,'$35,000-44,999', ifelse(demo_table_short$INDHHINC==8,'$45,000-54,999', ifelse(demo_table_short$INDHHINC==9,'$55,000-64,999', ifelse(demo_table_short$INDHHINC==10,'$65,000-74,999', 
                                ifelse(demo_table_short$INDHHINC==11,'$75,000 or more', ifelse(demo_table_short$INDHHINC==12,'Over $20,000', ifelse(demo_table_short$INDHHINC==13,'Under $20,000', ifelse(demo_table_short$INDHHINC==77 | demo_table_short$INDHHINC==99,'Missing', 'Missing')))))))))))))))
table(demo_table_short$INDHHINC, demo_table_short$hhinc_99_06)
table(demo_table_short$INDHHIN2, exclude=NULL)
demo_table_short$hhinc_07_18 <- ifelse(demo_table_short$INDHHIN2=='<NA>' | is.na(demo_table_short$INDHHIN2),'Missing', ifelse(demo_table_short$INDHHIN2==1,'$0-4,999', ifelse(demo_table_short$INDHHIN2==2,'$5,000-9,999',
                                ifelse(demo_table_short$INDHHIN2==3,'$10,000-14,999', ifelse(demo_table_short$INDHHIN2==4,'$15,000-19,999', ifelse(demo_table_short$INDHHIN2==5,'$20,000-24,999', ifelse(demo_table_short$INDHHIN2==6,'$25,000-34,999',
                                ifelse(demo_table_short$INDHHIN2==7,'$35,000-44,999', ifelse(demo_table_short$INDHHIN2==8,'$45,000-54,999', ifelse(demo_table_short$INDHHIN2==9,'$55,000-64,999', ifelse(demo_table_short$INDHHIN2==10,'$65,000-74,999', 
                                ifelse(demo_table_short$INDHHIN2==14,'$75,000-99,999', ifelse(demo_table_short$INDHHIN2==15,'$100,000 or more', ifelse(demo_table_short$INDHHIN2==12,'Over $20,000', ifelse(demo_table_short$INDHHIN2==13,'Under $20,000', ifelse(demo_table_short$INDHHIN2==77 | demo_table_short$INDHHIN2==99,'Missing', 'Missing'))))))))))))))))
table(demo_table_short$INDHHIN2, demo_table_short$hhinc_07_18)
table(demo_table_short$hhinc_99_06, demo_table_short$hhinc_07_18)
demo_table_short$hhinc <- ifelse(is.na(demo_table_short$INDHHINC), demo_table_short$hhinc_07_18, demo_table_short$hhinc_99_06)
table(demo_table_short$hhinc, exclude=NULL)
demo_table_short$inc_to_pov <- demo_table_short$INDFMPIR
dim(demo_table_short)

ocq_99 <- nhanes('OCQ')[,c('SEQN','OCQ150','OCQ180','OCQ210')]
colnames(ocq_99)
ocq_01 <- nhanes('OCQ_B')[,c('SEQN','OCD150','OCD180','OCQ210')]
colnames(ocq_99) <- c('SEQN','OCD150','OCQ180','OCQ210')
colnames(ocq_01) <- c('SEQN','OCD150','OCQ180','OCQ210')
ocq_03 <- nhanes('OCQ_C')[,c('SEQN','OCD150','OCQ180','OCQ210')]
ocq_05 <- nhanes('OCQ_D')[,c('SEQN','OCD150','OCQ180','OCQ210')]
ocq_07 <- nhanes('OCQ_E')[,c('SEQN','OCD150','OCQ180','OCQ210')]
ocq_09 <- nhanes('OCQ_F')[,c('SEQN','OCD150','OCQ180','OCQ210')]
ocq_11 <- nhanes('OCQ_G')[,c('SEQN','OCD150','OCQ180','OCQ210')]
ocq_13 <- nhanes('OCQ_H')[,c('SEQN','OCD150','OCQ180','OCQ210')]
ocq_15 <- nhanes('OCQ_I')[,c('SEQN','OCD150','OCQ180','OCQ210')]
ocq_17 <- nhanes('OCQ_J')[,c('SEQN','OCD150','OCQ180','OCQ210')]
ocq_all <-rbind(ocq_99, ocq_01, ocq_03, ocq_05, ocq_07, ocq_09, ocq_11, ocq_13, ocq_15, ocq_17)
dim(ocq_all)

demo_table_all <- merge(demo_table_short, ocq_all, by='SEQN', all.x=T, all.y=T, sort=T)
demo_table_all$OCD150
demo_table_all$working <- ifelse(demo_table_all$age<16,'Age <16', ifelse(demo_table_all$OCD150=='<NA>' | is.na(demo_table_all$OCD150),'Missing', 
                          ifelse(demo_table_all$OCD150==1,'Working', ifelse(demo_table_all$OCD150==3,'Looking for work', ifelse(demo_table_all$OCD150==4,'Not Working', ifelse(demo_table_all$OCD150==2,'At a business but not at work', 
                          ifelse(demo_table_all$OCD150==7,'Refused', ifelse(demo_table_all$OCD150==9,'Dont Know','Missing'))))))))
table(demo_table_all$working, exclude=NULL)
demo_table_all$hoursworked_cat <- ifelse(demo_table_all$age<16,'Age <16', ifelse(demo_table_all$OCQ180=='<NA>' | is.na(demo_table_all$OCQ180),'Missing', 
                              ifelse(demo_table_all$OCQ180==77777,'Refused', ifelse(demo_table_all$OCQ180==99999,'Dont Know', 'Hours Listed'))))
demo_table_all$hoursworked_num <- ifelse(demo_table_all$hoursworked_cat=='Hours Listed', demo_table_all$OCQ180, NA)
demo_table_all$hoursworked_num[1:10]
demo_table_all$work_35hr <- ifelse(demo_table_all$age<16,'Age <16', ifelse(demo_table_all$OCQ210=='<NA>' | is.na(demo_table_all$OCQ210),'Missing', 
                            ifelse(demo_table_all$OCQ210==1,1, ifelse(demo_table_all$OCQ210==2,0,  
                            ifelse(demo_table_all$OCQ210==7,'Refused', ifelse(demo_table_all$OCQ210==9,'Dont Know','Missing'))))))
dim(demo_table_all)
colnames(demo_table_all)

library(tableone)
nhanes_tab1_bc <- CreateTableOne(data=demo_table_all, #strata='hba1c.F12_missing', 
                                 vars=c('age','age_ge_16','female','survyr','racecat','hreduc','hhinc','inc_to_pov','working','hoursworked_cat','hoursworked_num','work_35hr'), test=T,
                                  factorVars=c('age_ge_16','female','survyr','racecat','hreduc','hhinc','working','hoursworked_cat','work_35hr'))


demo_table_all2 <- demo_table_all[,c('SEQN','survyr','age','age_ge_16','female','racecat','hreduc','hhinc','inc_to_pov','working','hoursworked_cat','hoursworked_num','work_35hr','WTINT2YR','WTINT4YR','WTMEC2YR','WTMEC4YR','SDMVPSU','SDMVSTRA')]
dim(demo_table_all2)
write.csv(demo_table_all2, 'C:/Users/sarac/OneDrive/Documents/Research/Area v Individual SES/nhanes_demog_table.csv', row.names=FALSE)



#####EXAM DATA CLEANING - Ht/Wt/BMI/BP#####
#flatten exam table
exam_table <- lapply(exam_tables, function(arr) {arr[[1]]}) %>% bind_rows()
colnames(exam_table)

vars_to_get_exam <- c("SEQN","BMXBMI","BMXWT","BMXHT","BMI", "BMXWAIST","BMXHIP",
  "BPXSY1","BPXSY2","BPXSY3","BPXSY4",#"BPXSAR",
  'BPXDI1','BPXDI2','BPXDI3','BPXDI4')#,'BPXDAR')
exam_table_short <- exam_table[, (names(exam_table) %in% vars_to_get_exam)]
colnames(exam_table_short)
exam_table_short[1:10,]
#write.csv(exam_table_short, 'C:/Users/sarac/OneDrive/Documents/Research/Area v Individual SES/nhanes_bp_table.csv', row.names=FALSE)

bpx_99 <- nhanes('BPX')
bpx_99_short <- bpx_99[, (names(bpx_99) %in% vars_to_get_exam)]
bpx_01 <- nhanes('BPX_B')
bpx_01_short <- bpx_01[, (names(bpx_01) %in% vars_to_get_exam)]
bpx_03 <- nhanes('BPX_C')
bpx_03_short <- bpx_03[, (names(bpx_03) %in% vars_to_get_exam)]
bpx_05 <- nhanes('BPX_D')
bpx_05_short <- bpx_05[, (names(bpx_05) %in% vars_to_get_exam)]
bpx_07 <- nhanes('BPX_E')
bpx_07_short <- bpx_07[, (names(bpx_07) %in% vars_to_get_exam)]
bpx_09 <- nhanes('BPX_F')
bpx_09_short <- bpx_09[, (names(bpx_09) %in% vars_to_get_exam)]
bpx_11 <- nhanes('BPX_G')
bpx_11_short <- bpx_11[, (names(bpx_11) %in% vars_to_get_exam)]
bpx_13 <- nhanes('BPX_H')
bpx_13_short <- bpx_13[, (names(bpx_13) %in% vars_to_get_exam)]
bpx_15 <- nhanes('BPX_I')
bpx_15_short <- bpx_15[, (names(bpx_15) %in% vars_to_get_exam)]
bpx_17 <- nhanes('BPX_J')
bpx_17_short <- bpx_17[, (names(bpx_17) %in% vars_to_get_exam)]
colnames(bpx_03_short)
bpx_all <-rbind(bpx_99_short, bpx_01_short, bpx_03_short, bpx_05_short, bpx_07_short, bpx_09_short, bpx_11_short, bpx_13_short, bpx_15_short, bpx_17_short)
dim(bpx_all)

bmx_99 <- nhanes('BMX')
bmx_99_short <- bmx_99[, (names(bmx_99) %in% vars_to_get_exam)]
bmx_01 <- nhanes('BMX_B')
bmx_01_short <- bmx_01[, (names(bmx_01) %in% vars_to_get_exam)]
bmx_03 <- nhanes('BMX_C')
bmx_03_short <- bmx_03[, (names(bmx_03) %in% vars_to_get_exam)]
bmx_05 <- nhanes('BMX_D')
bmx_05_short <- bmx_05[, (names(bmx_05) %in% vars_to_get_exam)]
bmx_07 <- nhanes('BMX_E')
bmx_07_short <- bmx_07[, (names(bmx_07) %in% vars_to_get_exam)]
bmx_09 <- nhanes('BMX_F')
bmx_09_short <- bmx_09[, (names(bmx_09) %in% vars_to_get_exam)]
bmx_11 <- nhanes('BMX_G')
bmx_11_short <- bmx_11[, (names(bmx_11) %in% vars_to_get_exam)]
bmx_13 <- nhanes('BMX_H')
bmx_13_short <- bmx_13[, (names(bmx_13) %in% vars_to_get_exam)]
bmx_15 <- nhanes('BMX_I')
bmx_15_short <- bmx_15[, (names(bmx_15) %in% vars_to_get_exam)]
bmx_17 <- nhanes('BMX_J')
bmx_17_short <- bmx_17[, (names(bmx_17) %in% vars_to_get_exam)]
colnames(bmx_17_short)
bmx_17_short <- bmx_17_short[,1:5]


bmx_all <-rbind(bmx_99_short, bmx_01_short, bmx_03_short, bmx_05_short, bmx_07_short, bmx_09_short, bmx_11_short, bmx_13_short, bmx_15_short, bmx_17_short)
dim(bmx_all)
#write.csv(bmx_all, 'C:/Users/sarac/OneDrive/Documents/Research/Area v Individual SES/nhanes_bmi_table.csv', row.names=FALSE)

exam_all <- merge(bmx_all, bpx_all, by='SEQN', all.x=T, all.y=T, sort=T)
dim(exam_all)
write.csv(exam_all, 'C:/Users/sarac/OneDrive/Documents/Research/Area v Individual SES/nhanes_exam_table.csv', row.names=FALSE)


#####LAB DATA CLEANING - most occurred in HMS O2#####

lab_table <- lapply(lab_tables, function(arr) {arr[[1]]}) %>% bind_rows()
colnames(lab_table)
vars_to_get_lab <- c('SEQN',"LBXGH","LB2GLU","LB2SGL","LBXGLU", "LBXHGB","LBXHCT","LB2HCT","LBXMCVSI","LB2MCVSI","LBXMC","LB2MC","LBXRDW","LB2RDW","LBXWBCSI","LBXNEPCT",
                     "LB2NEPCT","LBXLYPCT","LB2LYPCT","LBXEOPCT","LB2EOPCT","LBXPLTSI","LB2PLTSI",
                     "LBXSNASI","LB2SNASI","LBXSKSI","LBXSCLSI","LB2SCLSI","LBXSC3SI","LB2SC3SI","LBXSBU","LB2SBU","LB2SCR",
                     "LBDSCR","LBXSCR","LBXSAL","LB2SAL","LBXSTP","LB2STP","LBXSASSI","LB2SASSI","LBXSATSI","LB2SATSI","LBXSTB","LB2STB","LBDSTB",
                     "LBXTC","LB2TC","LB2HDL","LB2LDL","LBDLDL","LBXTR","LB2TR","LB2STR","LBXSTR",
                     "SSECPT","SSMHHT","SSMONP","SSURHIBP","SSURMHBP","URXCNP","URXCOP","URXDAZ","URXDMA","URXECP","URXECPT","URXEQU",
                     "URXETD","URXETL","URXGNS","URXHIBP","URXMBP","URXMC1","URXMCOH","URXMCP","URXMEP","URXMHBP","URXMHH","URXMHHT",
                     "URXMHNC","URXMHP","URXMIB","URXMNM","URXMNP","URXMOH","URXMONP","URXMOP","URXMZP",
                     "URXP01","URXP02","URXP03","URXP04","URXP05","URXP06","URXP07","URXP09","URXP10","URXUCR",
                     "HRDHG","HRXHG","LB2THG","LBXTHG","LBDTHGSI","LBDBGESI","LBDBGMSI","LBDIHGSI","LBXIHG","LBXBGE","LBXBGM",
                     "URXUHG","LB2BCD","LBDBCDSI","LBXBCD","LB2BPB","LBXBPB","LBDBMNSI","LBDBSESI","LBXBSE","LBDSELSI","LBXSEL","LBXBMN",
                     "LB2FOL","LBDFOL","LBXFOL","LB2RBF","LBXRBF","LB2B12","LBDB12","LBXB12","LB2HCY","LBDHCY","LBXHCY","LB2MMA",
                     "LBXMMA","LBDVIASI","LBXVIA","LBDVIESI","LBXVIE","LB2COT","LBXCOT","LBDGTCSI","LBXGTC","LBDRPLSI","LBXRPL","LBDRSTSI","LBXRST","LBXEPP",
                     "LB2IN", "LBXCRP","LB2CRP","LBXHSCRP","LBDHRPLC","LBDBAP","LBXBAP", "LBXFB","LBDFBSI","LBXPT21","LBDHI","LBDAPBSI",
                     "LBDIRNSI","LBXIRN","LB2FER","LBXFER","LBDTIBSI","LBXTIB","LBXPCT")
lab_table_short <- lab_table[, (names(lab_table) %in% vars_to_get_lab)]
colnames(lab_table_short)

labs_2017 <- map(2017, ~{pb$tick(); download_component_table("LABORATORY", .x)}) # index maps to each year 
labs_2017[1:10]
lab_2017 <- lapply(labs_2017, function(arr) {arr[[1]]}) %>% bind_rows()
dim(lab_2017)
lab_2017[1:10,]




lab_tablenames <- nhanesTables('LABORATORY',2017, namesonly=T)
labs_2017 <- lapply(lab_tablenames, nhanes)
names(labs_2017) <- lab_tablenames
lab_17 <- enframe(labs_2017) %>% unnest(value)
dim(lab_17)
colnames(lab_17)
table(lab_17$name)
lab17 <- lab_17 %>% dplyr::group_by(SEQN) %>% tidyr::fill(3:555) %>% tidyr::fill(3:555, .direction='up') %>% dplyr::distinct()
dim(lab17)
lab17_short <- as.data.frame(lab17[!duplicated(lab17$SEQN),])
write.csv(lab17_short, '/home/sc592/R/nhanes_lab17.csv', row.names=FALSE)

lab_tablenames <- nhanesTables('LABORATORY',2015, namesonly=T)
labs_2015 <- lapply(lab_tablenames, nhanes)
lab_15 <- enframe(labs_2015) %>% unnest(value)
dim(lab_15)
lab15 <- lab_15 %>% dplyr::group_by(SEQN) %>% tidyr::fill(3:838) %>% tidyr::fill(3:838, .direction='up') %>% dplyr::distinct()
dim(lab15)
lab15_short <- as.data.frame(lab15[!duplicated(lab15$SEQN),])
write.csv(lab15_short, '/home/sc592/R/nhanes_lab15.csv', row.names=FALSE)

lab_tablenames <- nhanesTables('LABORATORY',2013, namesonly=T)
labs_2013 <- lapply(lab_tablenames, nhanes)
lab_13 <- enframe(labs_2013) %>% unnest(value)
dim(lab_13)
lab13 <- lab_13 %>% dplyr::group_by(SEQN) %>% tidyr::fill(3:954) %>% tidyr::fill(3:954, .direction='up') %>% dplyr::distinct()
dim(lab13)
lab13_short <- as.data.frame(lab13[!duplicated(lab13$SEQN),])
write.csv(lab13_short, '/home/sc592/R/nhanes_lab13.csv', row.names=FALSE)


lab_tablenames <- nhanesTables('LABORATORY',2011, namesonly=T)
labs_2011 <- lapply(lab_tablenames, nhanes)
lab_11 <- enframe(labs_2011) %>% unnest(value)
dim(lab_11)
lab11 <- lab_11 %>% dplyr::group_by(SEQN) %>% tidyr::fill(3:907) %>% tidyr::fill(3:907, .direction='up') %>% dplyr::distinct()
dim(lab11)
lab11_short <- as.data.frame(lab11[!duplicated(lab11$SEQN),])
write.csv(lab11_short, '/home/sc592/R/nhanes_lab11.csv', row.names=FALSE)
rm(labs_2011)
rm(lab_11)
rm(lab11)

lab_tablenames <- nhanesTables('LABORATORY',2009, namesonly=T)
labs_2009 <- lapply(lab_tablenames, nhanes)
lab_09 <- enframe(labs_2009) %>% unnest(value)
dim(lab_09)
lab09 <- lab_09 %>% dplyr::group_by(SEQN) %>% tidyr::fill(3:819) %>% tidyr::fill(3:819, .direction='up') %>% dplyr::distinct()
dim(lab09)
lab09_short <- as.data.frame(lab09[!duplicated(lab09$SEQN),])
write.csv(lab09_short, '/home/sc592/R/nhanes_lab09.csv', row.names=FALSE)


lab_tablenames <- nhanesTables('LABORATORY',2007, namesonly=T)
labs_2007 <- lapply(lab_tablenames, nhanes)
lab_07 <- enframe(labs_2007) %>% unnest(value)
dim(lab_07)
lab07 <- lab_07 %>% dplyr::group_by(SEQN) %>% tidyr::fill(3:840) %>% tidyr::fill(3:840, .direction='up') %>% dplyr::distinct()
dim(lab07)
lab07_short <- as.data.frame(lab07[!duplicated(lab07$SEQN),])
write.csv(lab07_short, '/home/sc592/R/nhanes_lab07.csv', row.names=FALSE)


lab_tablenames <- nhanesTables('LABORATORY',2005, namesonly=T)
labs_2005 <- lapply(lab_tablenames, nhanes)
lab_05 <- enframe(labs_2005) %>% unnest(value)
dim(lab_05)
lab05 <- lab_05 %>% dplyr::group_by(SEQN) %>% tidyr::fill(3:968) %>% tidyr::fill(3:968, .direction='up') %>% dplyr::distinct()
dim(lab05)
lab05_short <- as.data.frame(lab05[!duplicated(lab05$SEQN),])
write.csv(lab05_short, '/home/sc592/R/nhanes_lab05.csv', row.names=FALSE)


lab_tablenames <- nhanesTables('LABORATORY',2003, namesonly=T)
labs_2003 <- lapply(lab_tablenames, nhanes)
lab_03 <- enframe(labs_2003) %>% unnest(value)
dim(lab_03)
lab03 <- lab_03 %>% dplyr::group_by(SEQN) %>% tidyr::fill(3:1024) %>% tidyr::fill(3:1024, .direction='up') %>% dplyr::distinct()
dim(lab03)
lab03_short <- as.data.frame(lab03[!duplicated(lab03$SEQN),])
write.csv(lab03_short, '/home/sc592/R/nhanes_lab03.csv', row.names=FALSE)

lab_tablenames <- nhanesTables('LABORATORY',2001, namesonly=T)
labs_2001 <- lapply(lab_tablenames, nhanes)
lab_01 <- enframe(labs_2001) %>% unnest(value)
dim(lab_01)
lab01 <- lab_01 %>% dplyr::group_by(SEQN) %>% tidyr::fill(3:838) %>% tidyr::fill(3:838, .direction='up') %>% dplyr::distinct()
dim(lab01)
lab01_short <- as.data.frame(lab01[!duplicated(lab01$SEQN),])
write.csv(lab01_short, '/home/sc592/R/nhanes_lab01.csv', row.names=FALSE)

lab_tablenames <- nhanesTables('LABORATORY',1999, namesonly=T)
labs_1999 <- lapply(lab_tablenames, nhanes)
lab_99 <- enframe(labs_1999) %>% unnest(value)
dim(lab_99)
lab99 <- lab_99 %>% dplyr::group_by(SEQN) %>% tidyr::fill(3:858) %>% tidyr::fill(3:858, .direction='up') %>% dplyr::distinct()
dim(lab99)
lab99_short <- as.data.frame(lab99[!duplicated(lab99$SEQN),])
write.csv(lab99_short, '/home/sc592/R/nhanes_lab99.csv', row.names=FALSE)


#####combine lab data#####
lab99 <- read.csv(file='/home/sc592/R/nhanes_lab99.csv', header=TRUE, sep=',')
lab01 <- read.csv(file='/home/sc592/R/nhanes_lab01.csv', header=TRUE, sep=',')
lab03 <- read.csv(file='/home/sc592/R/nhanes_lab03.csv', header=TRUE, sep=',')
lab05 <- read.csv(file='/home/sc592/R/nhanes_lab05.csv', header=TRUE, sep=',')
lab07 <- read.csv(file='/home/sc592/R/nhanes_lab07.csv', header=TRUE, sep=',')
lab09 <- read.csv(file='/home/sc592/R/nhanes_lab09.csv', header=TRUE, sep=',')
lab11 <- read.csv(file='/home/sc592/R/nhanes_lab11.csv', header=TRUE, sep=',')
lab13 <- read.csv(file='/home/sc592/R/nhanes_lab13.csv', header=TRUE, sep=',')
lab15 <- read.csv(file='/home/sc592/R/nhanes_lab15.csv', header=TRUE, sep=',')
lab17 <- read.csv(file='/home/sc592/R/nhanes_lab17.csv', header=TRUE, sep=',')
lab99_01 <- dplyr::bind_rows(lab99,lab01)
lab99_03 <- dplyr::bind_rows(lab99_01, lab03)
lab99_05 <- dplyr::bind_rows(lab99_03, lab05)
lab99_07 <- dplyr::bind_rows(lab99_05, lab07)
lab09$KID221 <- as.character(lab09$KID221)
lab99_09 <- dplyr::bind_rows(lab99_07, lab09)
lab99_11 <- dplyr::bind_rows(lab99_09, lab11)
lab99_13 <- dplyr::bind_rows(lab99_11, lab13)
lab99_15 <- dplyr::bind_rows(lab99_13, lab15)
lab17$name <- as.integer(lab17$name)
lab_all <- dplyr::bind_rows(lab99_15, lab17)
write.csv(lab_all, '/home/sc592/R/nhanes_lab_ALL.csv', row.names=FALSE)

lab_short <- lab_all[, (names(lab_all) %in% vars_to_get_lab)]
dim(lab_short)
vars_in_lab <- colnames(lab_short)
setdiff(vars_to_get_lab, vars_in_lab)
write.csv(lab_short, '/home/sc592/R/nhanes_lab_relevant.csv', row.names=FALSE)
rm(list=ls())
lab <- read.csv(file='/home/sc592/R/nhanes_lab_relevant.csv', header=TRUE, sep=',')
write.csv(lab, '/home/sc592/R/nhanes_lab_relevant.csv', row.names=FALSE)




#####QUESTIONNAIRE DATA CLEANING#####
vars_to_get_quest <- c('SEQN',"MCQ080",'MCQ160J', 'DIQ160','DIQ010','DID040','DIQ040G','DIQ040Q', 'DIQ170','DIQ172', #MCQ=MED CONDITIONS, DIQ/DID=DIABETES-RELATED
                       'DIQ175A','DIQ175B','DIQ175C','DIQ175D','DIQ175E','DIQ175F','DIQ175G','DIQ175H','DIQ175I','DIQ175J','DIQ175K','DIQ175L',
                       'DIQ175M','DIQ175N','DIQ175O','DIQ175P','DIQ175Q','DIQ175R','DIQ175S','DIQ175T','DIQ175U','DIQ175V','DIQ175W','DIQ175X',
                       'MCQ100','BPQ030','MCQ160C','MCQ160D','MCQ160E','MCQ160F','KIQ020','KIQ022','KIQ025','MCQ160P','MCQ160O','MCQ160G','MCQ160K','WHD010','WHD020','CIDGSCOR',#KIQ=KIDNEY-RELATED, WHD=WEIGHT HISTORY, CIDGSCOR=DEPRESSION OR GAD?
                       'DIQ050','DIQ070','DID070','WHD080I','WHD080J','MCQ110','BPQ040','MCQ051','CIQDPB','CIDDSCOR',#BPQ=BLOOD PRESSURE, CIQDPB/CIDDSCOR=DEPRESSION
                       'ALQ120Q','ALQ120U','ALQ130','SMQ020','SMQ040', #TOBACCO, ALCOHOL
                       'CDQ010','DBD090','DBD270D','DBQ300','HHFDSEC', #CDQ=CV FITNESS - SOB ON STAIRS, DBQ=NUTRITION, FSQ=food security
                       'HID010','HIQ011','HID030A','HIQ031A','HIQ210','HUQ010','HUQ030','HUQ040','HUQ050','HUQ070','HUQ080',#HIQ=HEALTH INSURANCE, HUQ=overall health rating and healthcare utilization
                       'HOD010','PADACTIV','PADTIMES','PADDURAT','PADMETS',#HOQ=housing type, PAQIAF=physical activity 1999-2006
                       'PAQ605','PAQ610','PAD615','PAQ620','PAQ625','PAD630','PAQ635','PAQ640','PAD645','PAQ650','PAQ655','PAD660','PAQ665','PAQ670','PAD675')#PAQ=physical activity, 2007-2018

diq_full <- lapply(c('DIQ','DIQ_B','DIQ_C','DIQ_D','DIQ_E','DIQ_F','DIQ_G','DIQ_H','DIQ_I','DIQ_J'), nhanes)
diq_df <- enframe(diq_full) %>% unnest(value)
colnames(diq_df)
diq <- as.data.frame(diq_df[, (names(diq_df) %in% vars_to_get_quest)])
colnames(diq)

mcq_full <- lapply(c('MCQ','MCQ_B','MCQ_C','MCQ_D','MCQ_E','MCQ_F','MCQ_G','MCQ_H','MCQ_I','MCQ_J'), nhanes)
mcq_df <- enframe(mcq_full) %>% unnest(value)
colnames(mcq_df)
mcq <- as.data.frame(mcq_df[, (names(mcq_df) %in% vars_to_get_quest)])
colnames(mcq)

bpq_full <- lapply(c('BPQ','BPQ_B','BPQ_C','BPQ_D','BPQ_E','BPQ_F','BPQ_G','BPQ_H','BPQ_I','BPQ_J'), nhanes)
bpq_df <- enframe(bpq_full) %>% unnest(value)
colnames(bpq_df)
bpq <- as.data.frame(bpq_df[, (names(bpq_df) %in% vars_to_get_quest)])
colnames(bpq)

kiq_full <- lapply(c('KIQ','KIQ_U_B','KIQ_U_C','KIQ_U_D','KIQ_U_E','KIQ_U_F','KIQ_U_G','KIQ_U_H','KIQ_U_I','KIQ_U_J'), nhanes)
kiq_df <- enframe(kiq_full) %>% unnest(value)
colnames(kiq_df)
kiq <- as.data.frame(kiq_df[, (names(kiq_df) %in% vars_to_get_quest)])
colnames(kiq)

whq_full <- lapply(c('WHQ','WHQ_B','WHQ_C','WHQ_D','WHQ_E','WHQ_F','WHQ_G','WHQ_H','WHQ_I','WHQ_J'), nhanes)
whq_df <- enframe(whq_full) %>% unnest(value)
colnames(whq_df)
whq <- as.data.frame(whq_df[, (names(whq_df) %in% vars_to_get_quest)])
colnames(whq)

#depression, 1999-2003
ciq_full <- lapply(c('CIQMDEP','CIQDEP_B','CIQDEP_C','CIQDEP_D','CIQDEP_E','CIQDEP_F','CIQDEP_G','CIQDEP_H','CIQDEP_I','CIQDEP_J'), nhanes)
ciq_df <- enframe(ciq_full) %>% unnest(value)
colnames(ciq_df)
ciq <- as.data.frame(ciq_df[, (names(ciq_df) %in% vars_to_get_quest)])
colnames(ciq)

#depression 2005-2018
dpq_full <- lapply(c('DPQ','DPQ_B','DPQ_C','DPQ_D','DPQ_E','DPQ_F','DPQ_G','DPQ_H','DPQ_I','DPQ_J'), nhanes)
dpq_df <- enframe(dpq_full) %>% unnest(value)
colnames(dpq_df)
dpq_df_short <- dpq_df
class(dpq_df$DPQ010)
dpq_df$phq9 <- rowSums(dpq_df[,3:11]*(dpq_df[,3:11]<4))
summary(dpq_df$phq9)
dpq <- dpq_df[,c('SEQN','phq9')]
colnames(dpq)

alc_full <- lapply(c('ALQ','ALQ_B','ALQ_C','ALQ_D','ALQ_E','ALQ_F','ALQ_G','ALQ_H','ALQ_I','ALQ_J'), nhanes)
alc_df <- enframe(alc_full) %>% unnest(value)
colnames(alc_df)
alc <- as.data.frame(alc_df[, (names(alc_df) %in% vars_to_get_quest)])
colnames(alc)

tob_full <- lapply(c('SMQ','SMQ_B','SMQ_C','SMQ_D','SMQ_E','SMQ_F','SMQ_G','SMQ_H','SMQ_I','SMQ_J'), nhanes)
tob_df <- enframe(tob_full) %>% unnest(value)
colnames(tob_df)
tob <- as.data.frame(tob_df[, (names(tob_df) %in% vars_to_get_quest)])
colnames(tob)

cvfit_full <- lapply(c('CDQ','CDQ_B','CDQ_C','CDQ_D','CDQ_E','CDQ_F','CDQ_G','CDQ_H','CDQ_I','CDQ_J'), nhanes)
cvfit_df <- enframe(cvfit_full) %>% unnest(value)
colnames(cvfit_df)
cvfit <- as.data.frame(cvfit_df[, (names(cvfit_df) %in% vars_to_get_quest)])
colnames(cvfit)

nutr_full <- lapply(c('DBQ','DBQ_B','DBQ_C','DBQ_D','DBQ_E','DBQ_F','DBQ_G','DBQ_H','DBQ_I','DBQ_J'), nhanes)
nutr_df <- enframe(nutr_full) %>% unnest(value)
colnames(nutr_df)
nutr <- as.data.frame(nutr_df[, (names(nutr_df) %in% vars_to_get_quest)])
colnames(nutr)

fdsec_full <- lapply(c('FSQ','FSQ_B','FSQ_C','FSQ_D','FSQ_E','FSQ_F','FSQ_G','FSQ_H','FSQ_I','FSQ_J'), nhanes)
fdsec_df <- enframe(fdsec_full) %>% unnest(value)
colnames(fdsec_df)
fdsec <- as.data.frame(fdsec_df[, (names(fdsec_df) %in% vars_to_get_quest)])
colnames(fdsec)

insur_full <- lapply(c('HIQ','HIQ_B','HIQ_C','HIQ_D','HIQ_E','HIQ_F','HIQ_G','HIQ_H','HIQ_I','HIQ_J'), nhanes)
insur_df <- enframe(insur_full) %>% unnest(value)
colnames(insur_df)
insur <- as.data.frame(insur_df[, (names(insur_df) %in% vars_to_get_quest)])
colnames(insur)
table(insur$HID010, insur$HIQ011, exclude=NULL)

hcutil_full <- lapply(c('HUQ','HUQ_B','HUQ_C','HUQ_D','HUQ_E','HUQ_F','HUQ_G','HUQ_H','HUQ_I','HUQ_J'), nhanes)
hcutil_df <- enframe(hcutil_full) %>% unnest(value)
colnames(hcutil_df)
hcutil <- as.data.frame(hcutil_df[, (names(hcutil_df) %in% vars_to_get_quest)])
colnames(hcutil)

hous_full <- lapply(c('HOQ','HOQ_B','HOQ_C','HOQ_D','HOQ_E','HOQ_F','HOQ_G','HOQ_H','HOQ_I','HOQ_J'), nhanes)
hous_df <- enframe(hous_full) %>% unnest(value)
colnames(hous_df)
hous <- as.data.frame(hous_df[, (names(hous_df) %in% vars_to_get_quest)])
colnames(hous)

physact_full <- lapply(c('PAQIAF','PAQIAF_B','PAQIAF_C','PAQIAF_D','PAQ_E','PAQ_F','PAQ_G','PAQ_H','PAQ_I','PAQ_J'), nhanes)
physact_df <- enframe(physact_full) %>% unnest(value)
colnames(physact_df)
physact <- as.data.frame(physact_df[, (names(physact_df) %in% vars_to_get_quest)])
colnames(physact)
dim(physact)
length(unique(physact$SEQN))
physact$activity_number <- ave(physact$PADACTIV, physact$SEQN, FUN=seq_along)
summary(physact$activity_number)
summary(physact$PADDURAT)
physact$tot_metsmin_30d_peractivity <- physact$PADTIMES*physact$PADMETS*physact$PADDURAT
physact <- as.data.frame(physact %>% group_by(SEQN) %>% mutate(tot_metsmin_30d = sum(tot_metsmin_30d_peractivity)))
summary(physact$tot_metsmin_30d)
physact[1:20,]
physact <- physact[,c("SEQN","PAQ605","PAQ610","PAD615","PAQ620","PAQ625","PAD630","PAQ635","PAQ640","PAD645","PAQ650","PAQ655","PAD660","PAQ665","PAQ670","PAD675","tot_metsmin_30d" )]
physact <- physact[!duplicated(physact[,c('SEQN')]),]


quest9 <- merge(diq, mcq, by='SEQN', all.x=T, all.y=T, sort=T)
quest8 <- merge(quest9, bpq, by='SEQN', all.x=T, all.y=T, sort=T)
quest7 <- merge(quest8, kiq, by='SEQN', all.x=T, all.y=T, sort=T)
quest6 <- merge(quest7, whq, by='SEQN', all.x=T, all.y=T, sort=T)
quest5 <- merge(quest6, ciq, by='SEQN', all.x=T, all.y=T, sort=T)
quest4 <- merge(quest5, dpq, by='SEQN', all.x=T, all.y=T, sort=T)
quest3 <- merge(quest4, alc, by='SEQN', all.x=T, all.y=T, sort=T)
quest2 <- merge(quest3, tob, by='SEQN', all.x=T, all.y=T, sort=T)
quest1 <- merge(quest2, cvfit, by='SEQN', all.x=T, all.y=T, sort=T)
quest0 <- merge(quest1, nutr, by='SEQN', all.x=T, all.y=F, sort=T)
quest01 <- merge(quest0, fdsec, by='SEQN', all.x=T, all.y=F, sort=T)
quest02 <- merge(quest01, insur, by='SEQN', all.x=T, all.y=F, sort=T)
quest03 <- merge(quest02, hcutil, by='SEQN', all.x=T, all.y=F, sort=T)
quest04 <- merge(quest03, hous, by='SEQN', all.x=T, all.y=F, sort=T)
quest_all <- merge(quest04, physact, by='SEQN', all.x=T, all.y=T, sort=T)
dim(quest_all)
colnames(quest_all)
colnames(quest_all) <- c('SEQN','told_diab','age_diab','yrs_diab','insulin','oad','age_diab2','told_prediab','told_riskdiab','oad2','feel_riskdiab','riskdiab_fh','riskdiab_wt',
                         'riskdiab_age','riskdiab_diet','riskdiab_race','riskdiab_lgababy','riskdiab_sedent','riskdiab_htn','riskdiab_bg','riskdiab_chol','riskdiab_hypogly','riskdiab_hunger','riskdiab_pares','riskdiab_blurvis','riskdiab_fatigue',
                         'riskdiab_anyone','riskdiab_drwarn','riskdiab_other','riskdiab_gdm','riskdiab_polyuria','riskdiab_polydips','riskdiab_craveswt','riskdiab_medrisk','riskdiab_pcos','told_overwt','told_htn','med_htn','told_chd',
                         'told_angina','told_mi','told_cva','told_emphy','told_overwt2','told_chronbronch','med_asthma','told_copd','told_htn_x2+','told_kidfail','told_kidfail2','hd_pastyr','selfreport_ht',
                         'selfreport_wt','took_wtlossrx','took_wtlosssupp','med_depress','depress_yesno','phq9',
                         'alc_numdays','alc_freq_units','alc_drinksday','smoke_ever','smoke_current',
                         'sob_incline','num_restaur_weekly','veg_per_day','gov_meals','hh_foodsec',
                         'insur_now','insur_private','uninsur_pastyr','insur_now2','insur_private2','health_rating','hc_routine_place','hc_place_type','hc_times_pastyr','hc_admissions_yn','hc_admissions_num','home_type',
                         'act_vigwork_yn','act_vigwork_days','act_vigwork_minperday','act_modwork_yn','act_modwork_days','act_modwork_minperday','act_commute_yn','act_commute_days','act_commute_minperday','act_vigrec_yn','act_vigrec_days','act_vigrec_minperday','act_modrec_yn','act_modrec_days','act_modrec_minperday','tot_metsmin_30d')

write.csv(quest_all, 'C:/Users/sarac/OneDrive/Documents/Research/Area v Individual SES/nhanes_quest_table.csv', row.names=FALSE)




#####combine files#####
d <- read.csv(file='C:/Users/sarac/OneDrive/Documents/Research/Area v Individual SES/nhanes_demog_table.csv', header=TRUE, sep=',')
e <- read.csv(file='C:/Users/sarac/OneDrive/Documents/Research/Area v Individual SES/nhanes_exam_table.csv', header=TRUE, sep=',')
q <- read.csv(file='C:/Users/sarac/OneDrive/Documents/Research/Area v Individual SES/nhanes_quest_table.csv', header=TRUE, sep=',')
l <- read.csv(file='C:/Users/sarac/OneDrive/Documents/Research/Area v Individual SES/nhanes_lab_table.csv', header=TRUE, sep=',')
de <- merge(d,e, by='SEQN', all.x=T, all.y=T, sort=T)
deq <- merge (de,q, by='SEQN', all.x=T, all.y=T, sort=T)
nhanes <- merge (deq,l, by='SEQN', all.x=T, all.y=T, sort=T)
dim(nhanes)
nhanes <- nhanes[!(is.na(nhanes$told_diab) & is.na(nhanes$insulin) & is.na(nhanes$told_chd) & is.na(nhanes$told_kidfail)),]
nhanes$survyr <- nhanes$survyr.x
  
nhanes$diab <- ifelse(nhanes$told_diab==1 | nhanes$insulin==1 | nhanes$oad==1 | nhanes$oad2==1 | nhanes$LBXGH >6.4 | nhanes$LBXGLU >125 | nhanes$LB2GLU > 199 | nhanes$LB2SGL > 199, 1, 0)
nhanes$diab <- ifelse(is.na(nhanes$diab) | nhanes$diab=='<NA>',0,1)
table(nhanes$told_diab, nhanes$diab, exclude=NULL)
table(nhanes$told_diab)
nhanes$bmi_calc <- nhanes$BMXWT/(nhanes$BMXHT/100)^2
hist(nhanes$bmi_calc)
nhanes$obese <- ifelse(nhanes$told_overwt==1 | nhanes$told_overwt2==1 | nhanes$took_wtlossrx==1 | nhanes$BMXBMI >= 30 | nhanes$bmi_calc >=30, 1, 0)
nhanes$obese <- ifelse(is.na(nhanes$obese), 0, ifelse(nhanes$obese==1,1,0))
table(nhanes$obese, exclude=NULL)
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
nhanes$ckd <- ifelse(nhanes$told_kidfail==1 | nhanes$told_kidfail2==1 | nhanes$creatinine >=1.4, 1, 0)
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

table(nhanes$hreduc, exclude=NULL)
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

table(nhanes$inc_simple, exclude=NULL)
nhanes$racecat <- factor(nhanes$racecat, levels=c("NHW","NHB","NHAsian","Mexican-American","Other Hispanic","Other/Multi-Racial"))
nhanes$educ_simple <- factor(nhanes$educ_simple, levels=c("College Degree","HS Degree","Some HS","No HS"))
nhanes$hhinc <- factor(nhanes$hhinc, levels=c("$75,000 or more","$75,000-99,999","$65,000-74,999","$55,000-64,999","$45,000-54,999","$35,000-44,999","$25,000-34,999","$20,000-24,999","Over $20,000","Under $20,000","$15,000-19,999","$10,000-14,999","$5,000-9,999","$0-4,999","Missing"))
nhanes$inc_simple <- factor(nhanes$inc_simple, levels=c("More than $75,000","$45,000-74,999","$20,000-44,999","Under $20,000"))
table(nhanes$hhinc)



#####analysis#####
library(survey)
library(metafor)
#survey design objects
surv_all <- svydesign(id = ~SDMVPSU, strata = ~ SDMVSTRA, weights = ~WTMEC2YR, data=nhanes, nest=T)
surv_nhw <- subset(surv_all, racecat=='NHW')
surv_nhw <- svydesign(id = ~SDMVPSU, strata = ~ SDMVSTRA, weights = ~WTMEC2YR, data=nhanes[nhanes$racecat=='NHW',], nest=T)
surv_nhb <- svydesign(id = ~SDMVPSU, strata = ~ SDMVSTRA, weights = ~WTMEC2YR, data=nhanes[nhanes$racecat=='NHB',], nest=T)
surv_nha <- svydesign(id = ~SDMVPSU, strata = ~ SDMVSTRA, weights = ~WTMEC2YR, data=nhanes[nhanes$racecat=='NHAsian',], nest=T)
surv_mex <- svydesign(id = ~SDMVPSU, strata = ~ SDMVSTRA, weights = ~WTMEC2YR, data=nhanes[nhanes$racecat=='Mexican-American',], nest=T)
surv_his <- svydesign(id = ~SDMVPSU, strata = ~ SDMVSTRA, weights = ~WTMEC2YR, data=nhanes[nhanes$racecat=='Other Hispanic',], nest=T)
surv_oth <- svydesign(id = ~SDMVPSU, strata = ~ SDMVSTRA, weights = ~WTMEC2YR, data=nhanes[nhanes$racecat=='Other/Multi-Racial',], nest=T)
#educ ordinal
mod_diab_educ_nhw <- glm(data=nhanes[nhanes$racecat=='NHW',], diab ~ age + female + educ_simple, family=binomial(link=logit))
mod_diab_educ_nhb <- glm(data=nhanes[nhanes$racecat=='NHB',], diab ~ age + female + educ_simple, family=binomial(link=logit))
mod_diab_educ_nha <- glm(data=nhanes[nhanes$racecat=='NHAsian',], diab ~ age + female + educ_simple, family=binomial(link=logit))
mod_diab_educ_mex <- glm(data=nhanes[nhanes$racecat=='Mexican-American',], diab ~ age + female + educ_simple, family=binomial(link=logit))
mod_diab_educ_his <- glm(data=nhanes[nhanes$racecat=='Other Hispanic',], diab ~ age + female + educ_simple, family=binomial(link=logit))
mod_diab_educ_oth <- glm(data=nhanes[nhanes$racecat=='Other/Multi-Racial',], diab ~ age + female + educ_simple, family=binomial(link=logit))
nhanes_educ_result <- data.frame(exp(mod_diab_educ_nhw$coefficients),exp(confint(mod_diab_educ_nhw)),coef(summary(mod_diab_educ_nhw))[,"Pr(>|z|)"],
                                 exp(mod_diab_educ_nhb$coefficients),exp(confint(mod_diab_educ_nhb)),coef(summary(mod_diab_educ_nhb))[,"Pr(>|z|)"],
                                 exp(mod_diab_educ_nha$coefficients),exp(confint(mod_diab_educ_nha)),coef(summary(mod_diab_educ_nha))[,"Pr(>|z|)"],
                                 exp(mod_diab_educ_mex$coefficients),exp(confint(mod_diab_educ_mex)),coef(summary(mod_diab_educ_mex))[,"Pr(>|z|)"],
                                 exp(mod_diab_educ_his$coefficients),exp(confint(mod_diab_educ_his)),coef(summary(mod_diab_educ_his))[,"Pr(>|z|)"],
                                 exp(mod_diab_educ_oth$coefficients),exp(confint(mod_diab_educ_oth)),coef(summary(mod_diab_educ_oth))[,"Pr(>|z|)"],stringsAsFactors = FALSE)
write.csv(nhanes_educ_result, 'C:/Users/sarac/OneDrive/Documents/Research/Area v Individual SES/nhanes_mod_indiv_educord.csv', row.names=FALSE) 
#educ continuous
mod_diab_educcont_nhw <- glm(data=nhanes[nhanes$racecat=='NHW',], diab ~ age + female + scale(educ_num), family=binomial(link=logit))
mod_diab_educcont_nhb <- glm(data=nhanes[nhanes$racecat=='NHB',], diab ~ age + female + scale(educ_num), family=binomial(link=logit))
mod_diab_educcont_nha <- glm(data=nhanes[nhanes$racecat=='NHAsian',], diab ~ age + female + scale(educ_num), family=binomial(link=logit))
mod_diab_educcont_mex <- glm(data=nhanes[nhanes$racecat=='Mexican-American',], diab ~ age + female + scale(educ_num), family=binomial(link=logit))
mod_diab_educcont_his <- glm(data=nhanes[nhanes$racecat=='Other Hispanic',], diab ~ age + female + scale(educ_num), family=binomial(link=logit))
mod_diab_educcont_oth <- glm(data=nhanes[nhanes$racecat=='Other/Multi-Racial',], diab ~ age + female + scale(educ_num), family=binomial(link=logit))
nhanes_educcont_result <- data.frame(exp(mod_diab_educcont_nhw$coefficients),exp(confint(mod_diab_educcont_nhw)),coef(summary(mod_diab_educcont_nhw))[,"Pr(>|z|)"],
                                 exp(mod_diab_educcont_nhb$coefficients),exp(confint(mod_diab_educcont_nhb)),coef(summary(mod_diab_educcont_nhb))[,"Pr(>|z|)"],
                                 exp(mod_diab_educcont_nha$coefficients),exp(confint(mod_diab_educcont_nha)),coef(summary(mod_diab_educcont_nha))[,"Pr(>|z|)"],
                                 exp(mod_diab_educcont_mex$coefficients),exp(confint(mod_diab_educcont_mex)),coef(summary(mod_diab_educcont_mex))[,"Pr(>|z|)"],
                                 exp(mod_diab_educcont_his$coefficients),exp(confint(mod_diab_educcont_his)),coef(summary(mod_diab_educcont_his))[,"Pr(>|z|)"],
                                 exp(mod_diab_educcont_oth$coefficients),exp(confint(mod_diab_educcont_oth)),coef(summary(mod_diab_educcont_oth))[,"Pr(>|z|)"],stringsAsFactors = FALSE)
write.csv(nhanes_educcont_result, 'C:/Users/sarac/OneDrive/Documents/Research/Area v Individual SES/nhanes_mod_indiv_educcont.csv', row.names=FALSE)


#income ordinal
mod_diab_inc_nhw <- glm(data=nhanes[nhanes$racecat=='NHW',], diab ~ age + female + hhinc, family=binomial(link=logit))
mod_diab_inc_nhb <- glm(data=nhanes[nhanes$racecat=='NHB',], diab ~ age + female + hhinc, family=binomial(link=logit))
mod_diab_inc_nha <- glm(data=nhanes[nhanes$racecat=='NHAsian',], diab ~ age + female + hhinc, family=binomial(link=logit))
mod_diab_inc_mex <- glm(data=nhanes[nhanes$racecat=='Mexican-American',], diab ~ age + female + hhinc, family=binomial(link=logit))
mod_diab_inc_his <- glm(data=nhanes[nhanes$racecat=='Other Hispanic',], diab ~ age + female + hhinc, family=binomial(link=logit))
mod_diab_inc_oth <- glm(data=nhanes[nhanes$racecat=='Other/Multi-Racial',], diab ~ age + female + hhinc, family=binomial(link=logit))
x<-9999
xx<-c(9999,9999)
nhanes_inc_result <- data.frame(exp(mod_diab_inc_nhw$coefficients),exp(confint(mod_diab_inc_nhw)),coef(summary(mod_diab_inc_nhw))[,"Pr(>|z|)"],
                                 exp(mod_diab_inc_nhb$coefficients),exp(confint(mod_diab_inc_nhb)),coef(summary(mod_diab_inc_nhb))[,"Pr(>|z|)"],
                                 append(exp(mod_diab_inc_nha$coefficients),x),rbind(exp(confint(mod_diab_inc_nha)),xx),append(coef(summary(mod_diab_inc_nha))[,"Pr(>|z|)"],x),
                                 exp(mod_diab_inc_mex$coefficients),exp(confint(mod_diab_inc_mex)),coef(summary(mod_diab_inc_mex))[,"Pr(>|z|)"],
                                 exp(mod_diab_inc_his$coefficients),exp(confint(mod_diab_inc_his)),coef(summary(mod_diab_inc_his))[,"Pr(>|z|)"],
                                 exp(mod_diab_inc_oth$coefficients),exp(confint(mod_diab_inc_oth)),coef(summary(mod_diab_inc_oth))[,"Pr(>|z|)"],stringsAsFactors = FALSE)
write.csv(nhanes_inc_result, 'C:/Users/sarac/OneDrive/Documents/Research/Area v Individual SES/nhanes_mod_indiv_incord.csv', row.names=FALSE) 
#income ordinal simple
mod_diab_incsimp_nhw <- glm(data=nhanes[nhanes$racecat=='NHW',], diab ~ age + female + inc_simple, family=binomial(link=logit))
mod_diab_incsimp_nhb <- glm(data=nhanes[nhanes$racecat=='NHB',], diab ~ age + female + inc_simple, family=binomial(link=logit))
mod_diab_incsimp_nha <- glm(data=nhanes[nhanes$racecat=='NHAsian',], diab ~ age + female + inc_simple, family=binomial(link=logit))
mod_diab_incsimp_mex <- glm(data=nhanes[nhanes$racecat=='Mexican-American',], diab ~ age + female + inc_simple, family=binomial(link=logit))
mod_diab_incsimp_his <- glm(data=nhanes[nhanes$racecat=='Other Hispanic',], diab ~ age + female + inc_simple, family=binomial(link=logit))
mod_diab_incsimp_oth <- glm(data=nhanes[nhanes$racecat=='Other/Multi-Racial',], diab ~ age + female + inc_simple, family=binomial(link=logit))
nhanes_incsimp_result <- data.frame(exp(mod_diab_incsimp_nhw$coefficients),exp(confint(mod_diab_incsimp_nhw)),coef(summary(mod_diab_incsimp_nhw))[,"Pr(>|z|)"],
                                exp(mod_diab_incsimp_nhb$coefficients),exp(confint(mod_diab_incsimp_nhb)),coef(summary(mod_diab_incsimp_nhb))[,"Pr(>|z|)"],
                                exp(mod_diab_incsimp_nha$coefficients),exp(confint(mod_diab_incsimp_nha)),coef(summary(mod_diab_incsimp_nha))[,"Pr(>|z|)"],
                                exp(mod_diab_incsimp_mex$coefficients),exp(confint(mod_diab_incsimp_mex)),coef(summary(mod_diab_incsimp_mex))[,"Pr(>|z|)"],
                                exp(mod_diab_incsimp_his$coefficients),exp(confint(mod_diab_incsimp_his)),coef(summary(mod_diab_incsimp_his))[,"Pr(>|z|)"],
                                exp(mod_diab_incsimp_oth$coefficients),exp(confint(mod_diab_incsimp_oth)),coef(summary(mod_diab_incsimp_oth))[,"Pr(>|z|)"],stringsAsFactors = FALSE)
write.csv(nhanes_incsimp_result, 'C:/Users/sarac/OneDrive/Documents/Research/Area v Individual SES/nhanes_mod_indiv_incsimpord.csv', row.names=FALSE)
#inc-to-pov continuous
mod_diab_inctopov_nhw <- glm(data=nhanes[nhanes$racecat=='NHW',], diab ~ age + female + scale(inc_to_pov), family=binomial(link=logit))
mod_diab_inctopov_nhb <- glm(data=nhanes[nhanes$racecat=='NHB',], diab ~ age + female + scale(inc_to_pov), family=binomial(link=logit))
mod_diab_inctopov_nha <- glm(data=nhanes[nhanes$racecat=='NHAsian',], diab ~ age + female + scale(inc_to_pov), family=binomial(link=logit))
mod_diab_inctopov_mex <- glm(data=nhanes[nhanes$racecat=='Mexican-American',], diab ~ age + female + scale(inc_to_pov), family=binomial(link=logit))
mod_diab_inctopov_his <- glm(data=nhanes[nhanes$racecat=='Other Hispanic',], diab ~ age + female + scale(inc_to_pov), family=binomial(link=logit))
mod_diab_inctopov_oth <- glm(data=nhanes[nhanes$racecat=='Other/Multi-Racial',], diab ~ age + female + scale(inc_to_pov), family=binomial(link=logit))
nhanes_inctopov_result <- data.frame(exp(mod_diab_inctopov_nhw$coefficients),exp(confint(mod_diab_inctopov_nhw)),coef(summary(mod_diab_inctopov_nhw))[,"Pr(>|z|)"],
                                    exp(mod_diab_inctopov_nhb$coefficients),exp(confint(mod_diab_inctopov_nhb)),coef(summary(mod_diab_inctopov_nhb))[,"Pr(>|z|)"],
                                    exp(mod_diab_inctopov_nha$coefficients),exp(confint(mod_diab_inctopov_nha)),coef(summary(mod_diab_inctopov_nha))[,"Pr(>|z|)"],
                                    exp(mod_diab_inctopov_mex$coefficients),exp(confint(mod_diab_inctopov_mex)),coef(summary(mod_diab_inctopov_mex))[,"Pr(>|z|)"],
                                    exp(mod_diab_inctopov_his$coefficients),exp(confint(mod_diab_inctopov_his)),coef(summary(mod_diab_inctopov_his))[,"Pr(>|z|)"],
                                    exp(mod_diab_inctopov_oth$coefficients),exp(confint(mod_diab_inctopov_oth)),coef(summary(mod_diab_inctopov_oth))[,"Pr(>|z|)"],stringsAsFactors = FALSE)
write.csv(nhanes_inctopov_result, 'C:/Users/sarac/OneDrive/Documents/Research/Area v Individual SES/nhanes_mod_indiv_inctopov.csv', row.names=FALSE)


#employ binary
nhanes$work_yn <- ifelse(nhanes$working=='Working',1, ifelse(nhanes$working=='Looking for work' | nhanes$working=='Not Working',0, NA))
table(nhanes$working, nhanes$work_yn, exclude=NULL)
mod_diab_emp_nhw <- glm(data=nhanes[nhanes$racecat=='NHW',], diab ~ age + female + work_yn, family=binomial(link=logit))
mod_diab_emp_nhb <- glm(data=nhanes[nhanes$racecat=='NHB',], diab ~ age + female + work_yn, family=binomial(link=logit))
mod_diab_emp_nha <- glm(data=nhanes[nhanes$racecat=='NHAsian',], diab ~ age + female + work_yn, family=binomial(link=logit))
mod_diab_emp_mex <- glm(data=nhanes[nhanes$racecat=='Mexican-American',], diab ~ age + female + work_yn, family=binomial(link=logit))
mod_diab_emp_his <- glm(data=nhanes[nhanes$racecat=='Other Hispanic',], diab ~ age + female + work_yn, family=binomial(link=logit))
mod_diab_emp_oth <- glm(data=nhanes[nhanes$racecat=='Other/Multi-Racial',], diab ~ age + female + work_yn, family=binomial(link=logit))
nhanes_emp_result <- data.frame(exp(mod_diab_emp_nhw$coefficients),exp(confint(mod_diab_emp_nhw)),coef(summary(mod_diab_emp_nhw))[,"Pr(>|z|)"],
                                 exp(mod_diab_emp_nhb$coefficients),exp(confint(mod_diab_emp_nhb)),coef(summary(mod_diab_emp_nhb))[,"Pr(>|z|)"],
                                 exp(mod_diab_emp_nha$coefficients),exp(confint(mod_diab_emp_nha)),coef(summary(mod_diab_emp_nha))[,"Pr(>|z|)"],
                                 exp(mod_diab_emp_mex$coefficients),exp(confint(mod_diab_emp_mex)),coef(summary(mod_diab_emp_mex))[,"Pr(>|z|)"],
                                 exp(mod_diab_emp_his$coefficients),exp(confint(mod_diab_emp_his)),coef(summary(mod_diab_emp_his))[,"Pr(>|z|)"],
                                 exp(mod_diab_emp_oth$coefficients),exp(confint(mod_diab_emp_oth)),coef(summary(mod_diab_emp_oth))[,"Pr(>|z|)"],stringsAsFactors = FALSE)
write.csv(nhanes_emp_result, 'C:/Users/sarac/OneDrive/Documents/Research/Area v Individual SES/nhanes_mod_indiv_empbin.csv', row.names=FALSE) 

#####visuals#####
library(forestplot)
forestplot(c("HS Degree","Some HS","No HS"), 
           mean = cbind(nhanes_educ_result[4:6,1],nhanes_educ_result[4:6,5],nhanes_educ_result[4:6,9],
                        nhanes_educ_result[4:6,13],nhanes_educ_result[4:6,17]),
           lower = cbind(nhanes_educ_result[4:6,2],nhanes_educ_result[4:6,6],nhanes_educ_result[4:6,10],
                         nhanes_educ_result[4:6,14],nhanes_educ_result[4:6,18]),
           upper = cbind(nhanes_educ_result[4:6,3],nhanes_educ_result[4:6,7],nhanes_educ_result[4:6,11],
                         nhanes_educ_result[4:6,15],nhanes_educ_result[4:6,19]),
           col = fpColors(box = c('darkslategray3','palegreen3','lightgoldenrod','salmon1','indianred3')),
           legend=c("NHW","NHB","NHA","Mexican-American","Other Hispanic"),
           legend_args = fpLegend(pos = list(x = 0.9, y = 0.9), gp = gpar(cex=0.9,col = "#CCCCCC", fill = "#F9F9F9")),
           xlab = "OR (95% CI)",
           title = 'Age and Gender-Adjusted OR for Educational Attainment (Ref=College Degree) \n on Diabetes, Stratified by Race/Ethnicity',
           txt_gp = fpTxtGp(title=gpar(cex=1),xlab=gpar(cex=1), ticks=gpar(cex=1), label=gpar(cex=1)),
           vertices=TRUE,
           line.margin=0.25,
           graph.pos=2,
           clip=c(0.5,4.0), xlim=c(0.5,4.0), xticks=c(0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0), xlog=TRUE, grid=structure(c(0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0)))

race_sens_foresttext <- c("HS Degree","Some HS","No HS")
race_sens_foresttext_sens <- c("NHW","NHB","NHA","Mexican-American","Other Hispanic")
race_sens_forest_variables <- c("HS Degree","Some HS","No HS")
nhanes_educ_result_transpose <- t(nhanes_educ_result)
rownames(nhanes_educ_result_transpose)
forestplot(c("NHW","NHB","NHA","Mexican-American","Other Hispanic"), 
           mean = cbind(nhanes_educ_result_transpose[c(1,5,9,13,17),4],nhanes_educ_result_transpose[c(1,5,9,13,17),5],nhanes_educ_result_transpose[c(1,5,9,13,17),6]),
           lower = cbind(nhanes_educ_result_transpose[c(2,6,10,14,18),4],nhanes_educ_result_transpose[c(2,6,10,14,18),5],nhanes_educ_result_transpose[c(2,6,10,14,18),6]),
           upper = cbind(nhanes_educ_result_transpose[c(3,7,11,15,19),4],nhanes_educ_result_transpose[c(3,7,11,15,19),5],nhanes_educ_result_transpose[c(3,7,11,15,19),6]),
           col = fpColors(box = c('darkslategray3','lightgoldenrod','indianred3')),
           legend=c("HS Degree","Some HS","No HS"),
           legend_args = fpLegend(pos = list(x = 0.9, y = 0.9), gp = gpar(cex=0.9,col = "#CCCCCC", fill = "#F9F9F9")),
           xlab = "OR (95% CI)",
           title = 'Age and Gender-Adjusted OR for Educational Attainment (Ref=College Degree) \n on Diabetes, Stratified by Race/Ethnicity',
           txt_gp = fpTxtGp(title=gpar(cex=1),xlab=gpar(cex=1), ticks=gpar(cex=1), label=gpar(cex=1)),
           vertices=TRUE,
           line.margin=0.25,
           graph.pos=2,
           clip=c(0.5,4.0), xlim=c(0.5,4.0), xticks=c(0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0), xlog=TRUE, grid=structure(c(0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0)))


library(forestplot)
forestplot(c("$45,000-74,999","$20,000-44,999","Under $20,000"), 
           mean = cbind(nhanes_incsimp_result[4:6,1],nhanes_incsimp_result[4:6,5],nhanes_incsimp_result[4:6,9],
                        nhanes_incsimp_result[4:6,13],nhanes_incsimp_result[4:6,17]),
           lower = cbind(nhanes_incsimp_result[4:6,2],nhanes_incsimp_result[4:6,6],nhanes_incsimp_result[4:6,10],
                         nhanes_incsimp_result[4:6,14],nhanes_incsimp_result[4:6,18]),
           upper = cbind(nhanes_incsimp_result[4:6,3],nhanes_incsimp_result[4:6,7],nhanes_incsimp_result[4:6,11],
                         nhanes_incsimp_result[4:6,15],nhanes_incsimp_result[4:6,19]),
           col = fpColors(box = c('darkslategray3','palegreen3','lightgoldenrod','salmon1','indianred3')),
           legend=c("NHW","NHB","NHA","Mexican-American","Other Hispanic"),
           legend_args = fpLegend(pos = list(x = 0.9, y = 0.9), gp = gpar(cex=0.9,col = "#CCCCCC", fill = "#F9F9F9")),
           xlab = "OR (95% CI)",
           title = 'Age and Gender-Adjusted OR for Income Category (Ref= More than $75,000) \n on Diabetes, Stratified by Race/Ethnicity',
           txt_gp = fpTxtGp(title=gpar(cex=1),xlab=gpar(cex=1), ticks=gpar(cex=1), label=gpar(cex=1)),
           vertices=TRUE,
           line.margin=0.25,
           graph.pos=2,
           clip=c(0.5,2.5), xlim=c(0.5,2.5), xticks=c(0.5,1.0,1.5,2.0,2.5), xlog=TRUE, grid=structure(c(0.5,1.0,1.5,2.0,2.5)))




#analyses NOT stratified by race
mod_diab_educ_all <- glm(data=nhanes, diab ~ age + female + educ_simple, family=binomial(link=logit))
summary(mod_diab_educ_all)
mod_diab_inc_all <- glm(data=nhanes, diab ~ age + female + hhinc, family=binomial(link=logit))
summary(mod_diab_inc_nhw)
mod_diab_incsimp_all <- glm(data=nhanes, diab ~ age + female + inc_simple, family=binomial(link=logit))
summary(mod_diab_incsimp_nhw)
mod_diab_emp_all <- glm(data=nhanes, diab ~ age + female + work_yn, family=binomial(link=logit))
forestplot(c("HS Degree","Some HS","No HS"),
           mean = exp(mod_diab_educ_all$coefficients[4:6]), 
           lower= exp(confint(mod_diab_educ_all))[4:6,1], 
           upper= exp(confint(mod_diab_educ_all))[4:6,2],
           xlab = "OR (95% CI)",
           title = 'Age and Gender-Adjusted OR for Educational Attainment (Ref= More than $75,000) \n on Diabetes, Stratified by Race/Ethnicity',
           txt_gp = fpTxtGp(title=gpar(cex=1),xlab=gpar(cex=1), ticks=gpar(cex=1), label=gpar(cex=1)),
           vertices=TRUE,
           line.margin=0.25,
           graph.pos=2,
           clip=c(0.5,2.5), xlim=c(0.5,2.5), xticks=c(0.5,1.0,1.5,2.0,2.5), xlog=TRUE, grid=structure(c(0.5,1.0,1.5,2.0,2.5)))
forestplot(c("$45,000-74,999","$20,000-44,999","Under $20,000"),
           mean = exp(mod_diab_incsimp_all$coefficients[4:6]), 
           lower= exp(confint(mod_diab_incsimp_all))[4:6,1], 
           upper= exp(confint(mod_diab_incsimp_all))[4:6,2],
           xlab = "OR (95% CI)",
           title = 'Age and Gender-Adjusted OR for Income Category (Ref= More than $75,000) \n on Diabetes, Stratified by Race/Ethnicity',
           txt_gp = fpTxtGp(title=gpar(cex=1),xlab=gpar(cex=1), ticks=gpar(cex=1), label=gpar(cex=1)),
           vertices=TRUE,
           line.margin=0.25,
           graph.pos=2,
           clip=c(0.5,2.5), xlim=c(0.5,2.5), xticks=c(0.5,1.0,1.5,2.0,2.5), xlog=TRUE, grid=structure(c(0.5,1.0,1.5,2.0,2.5)))




#####COMBINED INCOME FOREST#####
mod_diab_educcont_all <- glm(data=nhanes, diab ~ age + female + scale(educ_num), family=binomial(link=logit))

mod_obes_educcont_all <- glm(data=nhanes, obese ~ age + female + scale(educ_num), family=binomial(link=logit))
mod_obes_educcont_nhw <- glm(data=nhanes[nhanes$racecat=='NHW',], obese ~ age + female + scale(educ_num), family=binomial(link=logit))
mod_obes_educcont_nhb <- glm(data=nhanes[nhanes$racecat=='NHB',], obese ~ age + female + scale(educ_num), family=binomial(link=logit))
mod_obes_educcont_nha <- glm(data=nhanes[nhanes$racecat=='NHAsian',], obese ~ age + female + scale(educ_num), family=binomial(link=logit))
mod_obes_educcont_mex <- glm(data=nhanes[nhanes$racecat=='Mexican-American',], obese ~ age + female + scale(educ_num), family=binomial(link=logit))
mod_obes_educcont_his <- glm(data=nhanes[nhanes$racecat=='Other Hispanic',], obese ~ age + female + scale(educ_num), family=binomial(link=logit))
mod_obes_educcont_oth <- glm(data=nhanes[nhanes$racecat=='Other/Multi-Racial',], obese ~ age + female + scale(educ_num), family=binomial(link=logit))

mod_htn_educcont_all <- glm(data=nhanes, htn ~ age + female + scale(educ_num), family=binomial(link=logit))
mod_htn_educcont_nhw <- glm(data=nhanes[nhanes$racecat=='NHW',], htn ~ age + female + scale(educ_num), family=binomial(link=logit))
mod_htn_educcont_nhb <- glm(data=nhanes[nhanes$racecat=='NHB',], htn ~ age + female + scale(educ_num), family=binomial(link=logit))
mod_htn_educcont_nha <- glm(data=nhanes[nhanes$racecat=='NHAsian',], htn ~ age + female + scale(educ_num), family=binomial(link=logit))
mod_htn_educcont_mex <- glm(data=nhanes[nhanes$racecat=='Mexican-American',], htn ~ age + female + scale(educ_num), family=binomial(link=logit))
mod_htn_educcont_his <- glm(data=nhanes[nhanes$racecat=='Other Hispanic',], htn ~ age + female + scale(educ_num), family=binomial(link=logit))
mod_htn_educcont_oth <- glm(data=nhanes[nhanes$racecat=='Other/Multi-Racial',], htn ~ age + female + scale(educ_num), family=binomial(link=logit))

mod_cvd_educcont_all <- glm(data=nhanes, cvd ~ age + female + scale(educ_num), family=binomial(link=logit))
mod_cvd_educcont_nhw <- glm(data=nhanes[nhanes$racecat=='NHW',], cvd ~ age + female + scale(educ_num), family=binomial(link=logit))
mod_cvd_educcont_nhb <- glm(data=nhanes[nhanes$racecat=='NHB',], cvd ~ age + female + scale(educ_num), family=binomial(link=logit))
mod_cvd_educcont_nha <- glm(data=nhanes[nhanes$racecat=='NHAsian',], cvd ~ age + female + scale(educ_num), family=binomial(link=logit))
mod_cvd_educcont_mex <- glm(data=nhanes[nhanes$racecat=='Mexican-American',], cvd ~ age + female + scale(educ_num), family=binomial(link=logit))
mod_cvd_educcont_his <- glm(data=nhanes[nhanes$racecat=='Other Hispanic',], cvd ~ age + female + scale(educ_num), family=binomial(link=logit))
mod_cvd_educcont_oth <- glm(data=nhanes[nhanes$racecat=='Other/Multi-Racial',], cvd ~ age + female + scale(educ_num), family=binomial(link=logit))

mod_ckd_educcont_all <- glm(data=nhanes, ckd ~ age + female + scale(educ_num), family=binomial(link=logit))
mod_ckd_educcont_nhw <- glm(data=nhanes[nhanes$racecat=='NHW',], ckd ~ age + female + scale(educ_num), family=binomial(link=logit))
mod_ckd_educcont_nhb <- glm(data=nhanes[nhanes$racecat=='NHB',], ckd ~ age + female + scale(educ_num), family=binomial(link=logit))
mod_ckd_educcont_nha <- glm(data=nhanes[nhanes$racecat=='NHAsian',], ckd ~ age + female + scale(educ_num), family=binomial(link=logit))
mod_ckd_educcont_mex <- glm(data=nhanes[nhanes$racecat=='Mexican-American',], ckd ~ age + female + scale(educ_num), family=binomial(link=logit))
mod_ckd_educcont_his <- glm(data=nhanes[nhanes$racecat=='Other Hispanic',], ckd ~ age + female + scale(educ_num), family=binomial(link=logit))
mod_ckd_educcont_oth <- glm(data=nhanes[nhanes$racecat=='Other/Multi-Racial',], ckd ~ age + female + scale(educ_num), family=binomial(link=logit))

mod_copd_educcont_all <- glm(data=nhanes, copd ~ age + female + scale(educ_num), family=binomial(link=logit))
mod_copd_educcont_nhw <- glm(data=nhanes[nhanes$racecat=='NHW',], copd ~ age + female + scale(educ_num), family=binomial(link=logit))
mod_copd_educcont_nhb <- glm(data=nhanes[nhanes$racecat=='NHB',], copd ~ age + female + scale(educ_num), family=binomial(link=logit))
mod_copd_educcont_nha <- glm(data=nhanes[nhanes$racecat=='NHAsian',], copd ~ age + female + scale(educ_num), family=binomial(link=logit))
mod_copd_educcont_mex <- glm(data=nhanes[nhanes$racecat=='Mexican-American',], copd ~ age + female + scale(educ_num), family=binomial(link=logit))
mod_copd_educcont_his <- glm(data=nhanes[nhanes$racecat=='Other Hispanic',], copd ~ age + female + scale(educ_num), family=binomial(link=logit))
mod_copd_educcont_oth <- glm(data=nhanes[nhanes$racecat=='Other/Multi-Racial',], copd ~ age + female + scale(educ_num), family=binomial(link=logit))

mod_mdd_educcont_all <- glm(data=nhanes, mdd ~ age + female + scale(educ_num), family=binomial(link=logit))
mod_mdd_educcont_nhw <- glm(data=nhanes[nhanes$racecat=='NHW',], mdd ~ age + female + scale(educ_num), family=binomial(link=logit))
mod_mdd_educcont_nhb <- glm(data=nhanes[nhanes$racecat=='NHB',], mdd ~ age + female + scale(educ_num), family=binomial(link=logit))
mod_mdd_educcont_nha <- glm(data=nhanes[nhanes$racecat=='NHAsian',], mdd ~ age + female + scale(educ_num), family=binomial(link=logit))
mod_mdd_educcont_mex <- glm(data=nhanes[nhanes$racecat=='Mexican-American',], mdd ~ age + female + scale(educ_num), family=binomial(link=logit))
mod_mdd_educcont_his <- glm(data=nhanes[nhanes$racecat=='Other Hispanic',], mdd ~ age + female + scale(educ_num), family=binomial(link=logit))
mod_mdd_educcont_oth <- glm(data=nhanes[nhanes$racecat=='Other/Multi-Racial',], mdd ~ age + female + scale(educ_num), family=binomial(link=logit))

mod_alldx_educcont_allrace <- data.frame(exp(mod_diab_educcont_all$coefficients),exp(confint(mod_diab_educcont_all)),coef(summary(mod_diab_educcont_all))[,"Pr(>|z|)"],
                                         exp(mod_diab_educcont_nhw$coefficients),exp(confint(mod_diab_educcont_nhw)),coef(summary(mod_diab_educcont_nhw))[,"Pr(>|z|)"],
                                         exp(mod_diab_educcont_nhb$coefficients),exp(confint(mod_diab_educcont_nhb)),coef(summary(mod_diab_educcont_nhb))[,"Pr(>|z|)"],
                                         exp(mod_diab_educcont_nha$coefficients),exp(confint(mod_diab_educcont_nha)),coef(summary(mod_diab_educcont_nha))[,"Pr(>|z|)"],
                                         exp(mod_diab_educcont_mex$coefficients),exp(confint(mod_diab_educcont_mex)),coef(summary(mod_diab_educcont_mex))[,"Pr(>|z|)"],
                                         exp(mod_diab_educcont_his$coefficients),exp(confint(mod_diab_educcont_his)),coef(summary(mod_diab_educcont_his))[,"Pr(>|z|)"],
                                         exp(mod_diab_educcont_oth$coefficients),exp(confint(mod_diab_educcont_oth)),coef(summary(mod_diab_educcont_oth))[,"Pr(>|z|)"],
                                         exp(mod_obes_educcont_all$coefficients),exp(confint(mod_obes_educcont_all)),coef(summary(mod_obes_educcont_all))[,"Pr(>|z|)"],
                                         exp(mod_obes_educcont_nhw$coefficients),exp(confint(mod_obes_educcont_nhw)),coef(summary(mod_obes_educcont_nhw))[,"Pr(>|z|)"],
                                         exp(mod_obes_educcont_nhb$coefficients),exp(confint(mod_obes_educcont_nhb)),coef(summary(mod_obes_educcont_nhb))[,"Pr(>|z|)"],
                                         exp(mod_obes_educcont_nha$coefficients),exp(confint(mod_obes_educcont_nha)),coef(summary(mod_obes_educcont_nha))[,"Pr(>|z|)"],
                                         exp(mod_obes_educcont_mex$coefficients),exp(confint(mod_obes_educcont_mex)),coef(summary(mod_obes_educcont_mex))[,"Pr(>|z|)"],
                                         exp(mod_obes_educcont_his$coefficients),exp(confint(mod_obes_educcont_his)),coef(summary(mod_obes_educcont_his))[,"Pr(>|z|)"],
                                         exp(mod_obes_educcont_oth$coefficients),exp(confint(mod_obes_educcont_oth)),coef(summary(mod_obes_educcont_oth))[,"Pr(>|z|)"],
                                         exp(mod_htn_educcont_all$coefficients),exp(confint(mod_htn_educcont_all)),coef(summary(mod_htn_educcont_all))[,"Pr(>|z|)"],
                                         exp(mod_htn_educcont_nhw$coefficients),exp(confint(mod_htn_educcont_nhw)),coef(summary(mod_htn_educcont_nhw))[,"Pr(>|z|)"],
                                         exp(mod_htn_educcont_nhb$coefficients),exp(confint(mod_htn_educcont_nhb)),coef(summary(mod_htn_educcont_nhb))[,"Pr(>|z|)"],
                                         exp(mod_htn_educcont_nha$coefficients),exp(confint(mod_htn_educcont_nha)),coef(summary(mod_htn_educcont_nha))[,"Pr(>|z|)"],
                                         exp(mod_htn_educcont_mex$coefficients),exp(confint(mod_htn_educcont_mex)),coef(summary(mod_htn_educcont_mex))[,"Pr(>|z|)"],
                                         exp(mod_htn_educcont_his$coefficients),exp(confint(mod_htn_educcont_his)),coef(summary(mod_htn_educcont_his))[,"Pr(>|z|)"],
                                         exp(mod_htn_educcont_oth$coefficients),exp(confint(mod_htn_educcont_oth)),coef(summary(mod_htn_educcont_oth))[,"Pr(>|z|)"],
                                         exp(mod_cvd_educcont_all$coefficients),exp(confint(mod_cvd_educcont_all)),coef(summary(mod_cvd_educcont_all))[,"Pr(>|z|)"],
                                         exp(mod_cvd_educcont_nhw$coefficients),exp(confint(mod_cvd_educcont_nhw)),coef(summary(mod_cvd_educcont_nhw))[,"Pr(>|z|)"],
                                         exp(mod_cvd_educcont_nhb$coefficients),exp(confint(mod_cvd_educcont_nhb)),coef(summary(mod_cvd_educcont_nhb))[,"Pr(>|z|)"],
                                         exp(mod_cvd_educcont_nha$coefficients),exp(confint(mod_cvd_educcont_nha)),coef(summary(mod_cvd_educcont_nha))[,"Pr(>|z|)"],
                                         exp(mod_cvd_educcont_mex$coefficients),exp(confint(mod_cvd_educcont_mex)),coef(summary(mod_cvd_educcont_mex))[,"Pr(>|z|)"],
                                         exp(mod_cvd_educcont_his$coefficients),exp(confint(mod_cvd_educcont_his)),coef(summary(mod_cvd_educcont_his))[,"Pr(>|z|)"],
                                         exp(mod_cvd_educcont_oth$coefficients),exp(confint(mod_cvd_educcont_oth)),coef(summary(mod_cvd_educcont_oth))[,"Pr(>|z|)"],
                                         exp(mod_ckd_educcont_all$coefficients),exp(confint(mod_ckd_educcont_all)),coef(summary(mod_ckd_educcont_all))[,"Pr(>|z|)"],
                                         exp(mod_ckd_educcont_nhw$coefficients),exp(confint(mod_ckd_educcont_nhw)),coef(summary(mod_ckd_educcont_nhw))[,"Pr(>|z|)"],
                                         exp(mod_ckd_educcont_nhb$coefficients),exp(confint(mod_ckd_educcont_nhb)),coef(summary(mod_ckd_educcont_nhb))[,"Pr(>|z|)"],
                                         exp(mod_ckd_educcont_nha$coefficients),exp(confint(mod_ckd_educcont_nha)),coef(summary(mod_ckd_educcont_nha))[,"Pr(>|z|)"],
                                         exp(mod_ckd_educcont_mex$coefficients),exp(confint(mod_ckd_educcont_mex)),coef(summary(mod_ckd_educcont_mex))[,"Pr(>|z|)"],
                                         exp(mod_ckd_educcont_his$coefficients),exp(confint(mod_ckd_educcont_his)),coef(summary(mod_ckd_educcont_his))[,"Pr(>|z|)"],
                                         exp(mod_ckd_educcont_oth$coefficients),exp(confint(mod_ckd_educcont_oth)),coef(summary(mod_ckd_educcont_oth))[,"Pr(>|z|)"],
                                         exp(mod_copd_educcont_all$coefficients),exp(confint(mod_copd_educcont_all)),coef(summary(mod_copd_educcont_all))[,"Pr(>|z|)"],
                                         exp(mod_copd_educcont_nhw$coefficients),exp(confint(mod_copd_educcont_nhw)),coef(summary(mod_copd_educcont_nhw))[,"Pr(>|z|)"],
                                         exp(mod_copd_educcont_nhb$coefficients),exp(confint(mod_copd_educcont_nhb)),coef(summary(mod_copd_educcont_nhb))[,"Pr(>|z|)"],
                                         exp(mod_copd_educcont_nha$coefficients),exp(confint(mod_copd_educcont_nha)),coef(summary(mod_copd_educcont_nha))[,"Pr(>|z|)"],
                                         exp(mod_copd_educcont_mex$coefficients),exp(confint(mod_copd_educcont_mex)),coef(summary(mod_copd_educcont_mex))[,"Pr(>|z|)"],
                                         exp(mod_copd_educcont_his$coefficients),exp(confint(mod_copd_educcont_his)),coef(summary(mod_copd_educcont_his))[,"Pr(>|z|)"],
                                         exp(mod_copd_educcont_oth$coefficients),exp(confint(mod_copd_educcont_oth)),coef(summary(mod_copd_educcont_oth))[,"Pr(>|z|)"],
                                         exp(mod_mdd_educcont_all$coefficients),exp(confint(mod_mdd_educcont_all)),coef(summary(mod_mdd_educcont_all))[,"Pr(>|z|)"],
                                         exp(mod_mdd_educcont_nhw$coefficients),exp(confint(mod_mdd_educcont_nhw)),coef(summary(mod_mdd_educcont_nhw))[,"Pr(>|z|)"],
                                         exp(mod_mdd_educcont_nhb$coefficients),exp(confint(mod_mdd_educcont_nhb)),coef(summary(mod_mdd_educcont_nhb))[,"Pr(>|z|)"],
                                         exp(mod_mdd_educcont_nha$coefficients),exp(confint(mod_mdd_educcont_nha)),coef(summary(mod_mdd_educcont_nha))[,"Pr(>|z|)"],
                                         exp(mod_mdd_educcont_mex$coefficients),exp(confint(mod_mdd_educcont_mex)),coef(summary(mod_mdd_educcont_mex))[,"Pr(>|z|)"],
                                         exp(mod_mdd_educcont_his$coefficients),exp(confint(mod_mdd_educcont_his)),coef(summary(mod_mdd_educcont_his))[,"Pr(>|z|)"],
                                         exp(mod_mdd_educcont_oth$coefficients),exp(confint(mod_mdd_educcont_oth)),coef(summary(mod_mdd_educcont_oth))[,"Pr(>|z|)"],stringsAsFactors = FALSE)
mod_alldx_educcont_allrace_transpose <- t(mod_alldx_educcont_allrace)
forestplot(c('Type 2 Diabetes','Obesity','Hypertension','CVD','CKD','COPD','Depression'), 
           mean = cbind(t(mod_alldx_educcont_allrace[4,c(1,29,57,85,113,141,169)]),t(mod_alldx_educcont_allrace[4,c(5,33,61,89,117,145,173)]),
                        t(mod_alldx_educcont_allrace[4,c(9,37,65,93,121,149,177)]),t(mod_alldx_educcont_allrace[4,c(13,41,69,97,125,153,181)]),
                        t(mod_alldx_educcont_allrace[4,c(17,45,73,101,129,157,185)]),t(mod_alldx_educcont_allrace[4,c(21,49,77,105,133,161,189)]),t(mod_alldx_educcont_allrace[4,c(25,53,81,109,137,165,193)])),
           lower = cbind(t(mod_alldx_educcont_allrace[4,c(2,30,58,86,114,142,170)]),t(mod_alldx_educcont_allrace[4,c(6,34,62,90,118,146,174)]),
                         t(mod_alldx_educcont_allrace[4,c(10,38,66,94,122,150,178)]),t(mod_alldx_educcont_allrace[4,c(14,42,70,98,126,154,182)]),
                         t(mod_alldx_educcont_allrace[4,c(18,46,74,102,130,158,186)]),t(mod_alldx_educcont_allrace[4,c(22,50,78,106,134,162,190)]),t(mod_alldx_educcont_allrace[4,c(26,54,82,110,138,166,194)])),
           upper = cbind(t(mod_alldx_educcont_allrace[4,c(3,31,59,87,115,143,171)]),t(mod_alldx_educcont_allrace[4,c(7,35,63,91,119,147,175)]),
                         t(mod_alldx_educcont_allrace[4,c(11,39,67,95,123,151,179)]),t(mod_alldx_educcont_allrace[4,c(15,43,71,99,127,155,183)]),
                         t(mod_alldx_educcont_allrace[4,c(19,47,75,103,131,159,187)]),t(mod_alldx_educcont_allrace[4,c(23,51,79,107,135,163,191)]),t(mod_alldx_educcont_allrace[4,c(27,55,83,111,139,167,195)])),
           col = fpColors(box = c('gray23','darkslategray3','palegreen3','lightgoldenrod','salmon1','indianred3','mediumpurple4')),
           legend=c("All","NHW","NHB","NHA","Mexican-American","Other Hispanic","Other/Multiracial"),
           legend_args = fpLegend(pos = list(x = 0.9, y = 0.9), gp = gpar(cex=0.9,col = "#CCCCCC", fill = "#F9F9F9")),
           xlab = "OR (95% CI)",
           title = 'Age and Gender-Adjusted OR for Continuous Education on All Diseases, \n Overall and Stratified by Race/Ethnicity',
           txt_gp = fpTxtGp(title=gpar(cex=1),xlab=gpar(cex=1), ticks=gpar(cex=1), label=gpar(cex=1)),
           vertices=TRUE,
           line.margin=0.25,
           graph.pos=2,
           clip=c(0.6,1.4), xlim=c(0.6,1.4), xticks=c(0.6,0.8,1.0,1.2,1.4), xlog=TRUE, grid=structure(c(0.6,0.8,1.0,1.2,1.4)))



#####COMBINED INCOME FOREST#####
mod_diab_inctopov_all <- glm(data=nhanes, diab ~ age + female + inc_to_pov, family=binomial(link=logit))
mod_diab_inctopov_nhw <- glm(data=nhanes[nhanes$racecat=='NHW',], diab ~ age + female + inc_to_pov, family=binomial(link=logit))
mod_diab_inctopov_nhb <- glm(data=nhanes[nhanes$racecat=='NHB',], diab ~ age + female + inc_to_pov, family=binomial(link=logit))
mod_diab_inctopov_nha <- glm(data=nhanes[nhanes$racecat=='NHAsian',], diab ~ age + female + inc_to_pov, family=binomial(link=logit))
mod_diab_inctopov_mex <- glm(data=nhanes[nhanes$racecat=='Mexican-American',], diab ~ age + female + inc_to_pov, family=binomial(link=logit))
mod_diab_inctopov_his <- glm(data=nhanes[nhanes$racecat=='Other Hispanic',], diab ~ age + female + inc_to_pov, family=binomial(link=logit))
mod_diab_inctopov_oth <- glm(data=nhanes[nhanes$racecat=='Other/Multi-Racial',], diab ~ age + female + inc_to_pov, family=binomial(link=logit))

mod_obes_inctopov_all <- glm(data=nhanes, obese ~ age + female + inc_to_pov, family=binomial(link=logit))
mod_obes_inctopov_nhw <- glm(data=nhanes[nhanes$racecat=='NHW',], obese ~ age + female + inc_to_pov, family=binomial(link=logit))
mod_obes_inctopov_nhb <- glm(data=nhanes[nhanes$racecat=='NHB',], obese ~ age + female + inc_to_pov, family=binomial(link=logit))
mod_obes_inctopov_nha <- glm(data=nhanes[nhanes$racecat=='NHAsian',], obese ~ age + female + inc_to_pov, family=binomial(link=logit))
mod_obes_inctopov_mex <- glm(data=nhanes[nhanes$racecat=='Mexican-American',], obese ~ age + female + inc_to_pov, family=binomial(link=logit))
mod_obes_inctopov_his <- glm(data=nhanes[nhanes$racecat=='Other Hispanic',], obese ~ age + female + inc_to_pov, family=binomial(link=logit))
mod_obes_inctopov_oth <- glm(data=nhanes[nhanes$racecat=='Other/Multi-Racial',], obese ~ age + female + inc_to_pov, family=binomial(link=logit))

mod_htn_inctopov_all <- glm(data=nhanes, htn ~ age + female + inc_to_pov, family=binomial(link=logit))
mod_htn_inctopov_nhw <- glm(data=nhanes[nhanes$racecat=='NHW',], htn ~ age + female + inc_to_pov, family=binomial(link=logit))
mod_htn_inctopov_nhb <- glm(data=nhanes[nhanes$racecat=='NHB',], htn ~ age + female + inc_to_pov, family=binomial(link=logit))
mod_htn_inctopov_nha <- glm(data=nhanes[nhanes$racecat=='NHAsian',], htn ~ age + female + inc_to_pov, family=binomial(link=logit))
mod_htn_inctopov_mex <- glm(data=nhanes[nhanes$racecat=='Mexican-American',], htn ~ age + female + inc_to_pov, family=binomial(link=logit))
mod_htn_inctopov_his <- glm(data=nhanes[nhanes$racecat=='Other Hispanic',], htn ~ age + female + inc_to_pov, family=binomial(link=logit))
mod_htn_inctopov_oth <- glm(data=nhanes[nhanes$racecat=='Other/Multi-Racial',], htn ~ age + female + inc_to_pov, family=binomial(link=logit))

mod_cvd_inctopov_all <- glm(data=nhanes, cvd ~ age + female + inc_to_pov, family=binomial(link=logit))
mod_cvd_inctopov_nhw <- glm(data=nhanes[nhanes$racecat=='NHW',], cvd ~ age + female + inc_to_pov, family=binomial(link=logit))
mod_cvd_inctopov_nhb <- glm(data=nhanes[nhanes$racecat=='NHB',], cvd ~ age + female + inc_to_pov, family=binomial(link=logit))
mod_cvd_inctopov_nha <- glm(data=nhanes[nhanes$racecat=='NHAsian',], cvd ~ age + female + inc_to_pov, family=binomial(link=logit))
mod_cvd_inctopov_mex <- glm(data=nhanes[nhanes$racecat=='Mexican-American',], cvd ~ age + female + inc_to_pov, family=binomial(link=logit))
mod_cvd_inctopov_his <- glm(data=nhanes[nhanes$racecat=='Other Hispanic',], cvd ~ age + female + inc_to_pov, family=binomial(link=logit))
mod_cvd_inctopov_oth <- glm(data=nhanes[nhanes$racecat=='Other/Multi-Racial',], cvd ~ age + female + inc_to_pov, family=binomial(link=logit))

mod_ckd_inctopov_all <- glm(data=nhanes, ckd ~ age + female + inc_to_pov, family=binomial(link=logit))
mod_ckd_inctopov_nhw <- glm(data=nhanes[nhanes$racecat=='NHW',], ckd ~ age + female + inc_to_pov, family=binomial(link=logit))
mod_ckd_inctopov_nhb <- glm(data=nhanes[nhanes$racecat=='NHB',], ckd ~ age + female + inc_to_pov, family=binomial(link=logit))
mod_ckd_inctopov_nha <- glm(data=nhanes[nhanes$racecat=='NHAsian',], ckd ~ age + female + inc_to_pov, family=binomial(link=logit))
mod_ckd_inctopov_mex <- glm(data=nhanes[nhanes$racecat=='Mexican-American',], ckd ~ age + female + inc_to_pov, family=binomial(link=logit))
mod_ckd_inctopov_his <- glm(data=nhanes[nhanes$racecat=='Other Hispanic',], ckd ~ age + female + inc_to_pov, family=binomial(link=logit))
mod_ckd_inctopov_oth <- glm(data=nhanes[nhanes$racecat=='Other/Multi-Racial',], ckd ~ age + female + inc_to_pov, family=binomial(link=logit))

mod_copd_inctopov_all <- glm(data=nhanes, copd ~ age + female + inc_to_pov, family=binomial(link=logit))
mod_copd_inctopov_nhw <- glm(data=nhanes[nhanes$racecat=='NHW',], copd ~ age + female + inc_to_pov, family=binomial(link=logit))
mod_copd_inctopov_nhb <- glm(data=nhanes[nhanes$racecat=='NHB',], copd ~ age + female + inc_to_pov, family=binomial(link=logit))
mod_copd_inctopov_nha <- glm(data=nhanes[nhanes$racecat=='NHAsian',], copd ~ age + female + inc_to_pov, family=binomial(link=logit))
mod_copd_inctopov_mex <- glm(data=nhanes[nhanes$racecat=='Mexican-American',], copd ~ age + female + inc_to_pov, family=binomial(link=logit))
mod_copd_inctopov_his <- glm(data=nhanes[nhanes$racecat=='Other Hispanic',], copd ~ age + female + inc_to_pov, family=binomial(link=logit))
mod_copd_inctopov_oth <- glm(data=nhanes[nhanes$racecat=='Other/Multi-Racial',], copd ~ age + female + inc_to_pov, family=binomial(link=logit))

mod_mdd_inctopov_all <- glm(data=nhanes, mdd ~ age + female + inc_to_pov, family=binomial(link=logit))
mod_mdd_inctopov_nhw <- glm(data=nhanes[nhanes$racecat=='NHW',], mdd ~ age + female + inc_to_pov, family=binomial(link=logit))
mod_mdd_inctopov_nhb <- glm(data=nhanes[nhanes$racecat=='NHB',], mdd ~ age + female + inc_to_pov, family=binomial(link=logit))
mod_mdd_inctopov_nha <- glm(data=nhanes[nhanes$racecat=='NHAsian',], mdd ~ age + female + inc_to_pov, family=binomial(link=logit))
mod_mdd_inctopov_mex <- glm(data=nhanes[nhanes$racecat=='Mexican-American',], mdd ~ age + female + inc_to_pov, family=binomial(link=logit))
mod_mdd_inctopov_his <- glm(data=nhanes[nhanes$racecat=='Other Hispanic',], mdd ~ age + female + inc_to_pov, family=binomial(link=logit))
mod_mdd_inctopov_oth <- glm(data=nhanes[nhanes$racecat=='Other/Multi-Racial',], mdd ~ age + female + inc_to_pov, family=binomial(link=logit))

mod_alldx_inctopov_allrace <- data.frame(exp(mod_diab_inctopov_all$coefficients),exp(confint(mod_diab_inctopov_all)),coef(summary(mod_diab_inctopov_all))[,"Pr(>|z|)"],
                                         exp(mod_diab_inctopov_nhw$coefficients),exp(confint(mod_diab_inctopov_nhw)),coef(summary(mod_diab_inctopov_nhw))[,"Pr(>|z|)"],
                                         exp(mod_diab_inctopov_nhb$coefficients),exp(confint(mod_diab_inctopov_nhb)),coef(summary(mod_diab_inctopov_nhb))[,"Pr(>|z|)"],
                                         exp(mod_diab_inctopov_nha$coefficients),exp(confint(mod_diab_inctopov_nha)),coef(summary(mod_diab_inctopov_nha))[,"Pr(>|z|)"],
                                         exp(mod_diab_inctopov_mex$coefficients),exp(confint(mod_diab_inctopov_mex)),coef(summary(mod_diab_inctopov_mex))[,"Pr(>|z|)"],
                                         exp(mod_diab_inctopov_his$coefficients),exp(confint(mod_diab_inctopov_his)),coef(summary(mod_diab_inctopov_his))[,"Pr(>|z|)"],
                                         exp(mod_diab_inctopov_oth$coefficients),exp(confint(mod_diab_inctopov_oth)),coef(summary(mod_diab_inctopov_oth))[,"Pr(>|z|)"],
                                         exp(mod_obes_inctopov_all$coefficients),exp(confint(mod_obes_inctopov_all)),coef(summary(mod_obes_inctopov_all))[,"Pr(>|z|)"],
                                         exp(mod_obes_inctopov_nhw$coefficients),exp(confint(mod_obes_inctopov_nhw)),coef(summary(mod_obes_inctopov_nhw))[,"Pr(>|z|)"],
                                         exp(mod_obes_inctopov_nhb$coefficients),exp(confint(mod_obes_inctopov_nhb)),coef(summary(mod_obes_inctopov_nhb))[,"Pr(>|z|)"],
                                         exp(mod_obes_inctopov_nha$coefficients),exp(confint(mod_obes_inctopov_nha)),coef(summary(mod_obes_inctopov_nha))[,"Pr(>|z|)"],
                                         exp(mod_obes_inctopov_mex$coefficients),exp(confint(mod_obes_inctopov_mex)),coef(summary(mod_obes_inctopov_mex))[,"Pr(>|z|)"],
                                         exp(mod_obes_inctopov_his$coefficients),exp(confint(mod_obes_inctopov_his)),coef(summary(mod_obes_inctopov_his))[,"Pr(>|z|)"],
                                         exp(mod_obes_inctopov_oth$coefficients),exp(confint(mod_obes_inctopov_oth)),coef(summary(mod_obes_inctopov_oth))[,"Pr(>|z|)"],
                                         exp(mod_htn_inctopov_all$coefficients),exp(confint(mod_htn_inctopov_all)),coef(summary(mod_htn_inctopov_all))[,"Pr(>|z|)"],
                                         exp(mod_htn_inctopov_nhw$coefficients),exp(confint(mod_htn_inctopov_nhw)),coef(summary(mod_htn_inctopov_nhw))[,"Pr(>|z|)"],
                                         exp(mod_htn_inctopov_nhb$coefficients),exp(confint(mod_htn_inctopov_nhb)),coef(summary(mod_htn_inctopov_nhb))[,"Pr(>|z|)"],
                                         exp(mod_htn_inctopov_nha$coefficients),exp(confint(mod_htn_inctopov_nha)),coef(summary(mod_htn_inctopov_nha))[,"Pr(>|z|)"],
                                         exp(mod_htn_inctopov_mex$coefficients),exp(confint(mod_htn_inctopov_mex)),coef(summary(mod_htn_inctopov_mex))[,"Pr(>|z|)"],
                                         exp(mod_htn_inctopov_his$coefficients),exp(confint(mod_htn_inctopov_his)),coef(summary(mod_htn_inctopov_his))[,"Pr(>|z|)"],
                                         exp(mod_htn_inctopov_oth$coefficients),exp(confint(mod_htn_inctopov_oth)),coef(summary(mod_htn_inctopov_oth))[,"Pr(>|z|)"],
                                         exp(mod_cvd_inctopov_all$coefficients),exp(confint(mod_cvd_inctopov_all)),coef(summary(mod_cvd_inctopov_all))[,"Pr(>|z|)"],
                                         exp(mod_cvd_inctopov_nhw$coefficients),exp(confint(mod_cvd_inctopov_nhw)),coef(summary(mod_cvd_inctopov_nhw))[,"Pr(>|z|)"],
                                         exp(mod_cvd_inctopov_nhb$coefficients),exp(confint(mod_cvd_inctopov_nhb)),coef(summary(mod_cvd_inctopov_nhb))[,"Pr(>|z|)"],
                                         exp(mod_cvd_inctopov_nha$coefficients),exp(confint(mod_cvd_inctopov_nha)),coef(summary(mod_cvd_inctopov_nha))[,"Pr(>|z|)"],
                                         exp(mod_cvd_inctopov_mex$coefficients),exp(confint(mod_cvd_inctopov_mex)),coef(summary(mod_cvd_inctopov_mex))[,"Pr(>|z|)"],
                                         exp(mod_cvd_inctopov_his$coefficients),exp(confint(mod_cvd_inctopov_his)),coef(summary(mod_cvd_inctopov_his))[,"Pr(>|z|)"],
                                         exp(mod_cvd_inctopov_oth$coefficients),exp(confint(mod_cvd_inctopov_oth)),coef(summary(mod_cvd_inctopov_oth))[,"Pr(>|z|)"],
                                         exp(mod_ckd_inctopov_all$coefficients),exp(confint(mod_ckd_inctopov_all)),coef(summary(mod_ckd_inctopov_all))[,"Pr(>|z|)"],
                                         exp(mod_ckd_inctopov_nhw$coefficients),exp(confint(mod_ckd_inctopov_nhw)),coef(summary(mod_ckd_inctopov_nhw))[,"Pr(>|z|)"],
                                         exp(mod_ckd_inctopov_nhb$coefficients),exp(confint(mod_ckd_inctopov_nhb)),coef(summary(mod_ckd_inctopov_nhb))[,"Pr(>|z|)"],
                                         exp(mod_ckd_inctopov_nha$coefficients),exp(confint(mod_ckd_inctopov_nha)),coef(summary(mod_ckd_inctopov_nha))[,"Pr(>|z|)"],
                                         exp(mod_ckd_inctopov_mex$coefficients),exp(confint(mod_ckd_inctopov_mex)),coef(summary(mod_ckd_inctopov_mex))[,"Pr(>|z|)"],
                                         exp(mod_ckd_inctopov_his$coefficients),exp(confint(mod_ckd_inctopov_his)),coef(summary(mod_ckd_inctopov_his))[,"Pr(>|z|)"],
                                         exp(mod_ckd_inctopov_oth$coefficients),exp(confint(mod_ckd_inctopov_oth)),coef(summary(mod_ckd_inctopov_oth))[,"Pr(>|z|)"],
                                         exp(mod_copd_inctopov_all$coefficients),exp(confint(mod_copd_inctopov_all)),coef(summary(mod_copd_inctopov_all))[,"Pr(>|z|)"],
                                         exp(mod_copd_inctopov_nhw$coefficients),exp(confint(mod_copd_inctopov_nhw)),coef(summary(mod_copd_inctopov_nhw))[,"Pr(>|z|)"],
                                         exp(mod_copd_inctopov_nhb$coefficients),exp(confint(mod_copd_inctopov_nhb)),coef(summary(mod_copd_inctopov_nhb))[,"Pr(>|z|)"],
                                         exp(mod_copd_inctopov_nha$coefficients),exp(confint(mod_copd_inctopov_nha)),coef(summary(mod_copd_inctopov_nha))[,"Pr(>|z|)"],
                                         exp(mod_copd_inctopov_mex$coefficients),exp(confint(mod_copd_inctopov_mex)),coef(summary(mod_copd_inctopov_mex))[,"Pr(>|z|)"],
                                         exp(mod_copd_inctopov_his$coefficients),exp(confint(mod_copd_inctopov_his)),coef(summary(mod_copd_inctopov_his))[,"Pr(>|z|)"],
                                         exp(mod_copd_inctopov_oth$coefficients),exp(confint(mod_copd_inctopov_oth)),coef(summary(mod_copd_inctopov_oth))[,"Pr(>|z|)"],
                                         exp(mod_mdd_inctopov_all$coefficients),exp(confint(mod_mdd_inctopov_all)),coef(summary(mod_mdd_inctopov_all))[,"Pr(>|z|)"],
                                         exp(mod_mdd_inctopov_nhw$coefficients),exp(confint(mod_mdd_inctopov_nhw)),coef(summary(mod_mdd_inctopov_nhw))[,"Pr(>|z|)"],
                                         exp(mod_mdd_inctopov_nhb$coefficients),exp(confint(mod_mdd_inctopov_nhb)),coef(summary(mod_mdd_inctopov_nhb))[,"Pr(>|z|)"],
                                         exp(mod_mdd_inctopov_nha$coefficients),exp(confint(mod_mdd_inctopov_nha)),coef(summary(mod_mdd_inctopov_nha))[,"Pr(>|z|)"],
                                         exp(mod_mdd_inctopov_mex$coefficients),exp(confint(mod_mdd_inctopov_mex)),coef(summary(mod_mdd_inctopov_mex))[,"Pr(>|z|)"],
                                         exp(mod_mdd_inctopov_his$coefficients),exp(confint(mod_mdd_inctopov_his)),coef(summary(mod_mdd_inctopov_his))[,"Pr(>|z|)"],
                                         exp(mod_mdd_inctopov_oth$coefficients),exp(confint(mod_mdd_inctopov_oth)),coef(summary(mod_mdd_inctopov_oth))[,"Pr(>|z|)"],stringsAsFactors = FALSE)
mod_alldx_inctopov_allrace_transpose <- t(mod_alldx_inctopov_allrace)

forestplot(c('Type 2 Diabetes','Obesity','Hypertension','CVD','CKD','COPD','Depression'), 
           mean = cbind(t(mod_alldx_inctopov_allrace[4,c(1,29,57,85,113,141,169)]),t(mod_alldx_inctopov_allrace[4,c(5,33,61,89,117,145,173)]),
                        t(mod_alldx_inctopov_allrace[4,c(9,37,65,93,121,149,177)]),t(mod_alldx_inctopov_allrace[4,c(13,41,69,97,125,153,181)]),
                        t(mod_alldx_inctopov_allrace[4,c(17,45,73,101,129,157,185)]),t(mod_alldx_inctopov_allrace[4,c(21,49,77,105,133,161,189)]),t(mod_alldx_inctopov_allrace[4,c(25,53,81,109,137,165,193)])),
           lower = cbind(t(mod_alldx_inctopov_allrace[4,c(2,30,58,86,114,142,170)]),t(mod_alldx_inctopov_allrace[4,c(6,34,62,90,118,146,174)]),
                         t(mod_alldx_inctopov_allrace[4,c(10,38,66,94,122,150,178)]),t(mod_alldx_inctopov_allrace[4,c(14,42,70,98,126,154,182)]),
                         t(mod_alldx_inctopov_allrace[4,c(18,46,74,102,130,158,186)]),t(mod_alldx_inctopov_allrace[4,c(22,50,78,106,134,162,190)]),t(mod_alldx_inctopov_allrace[4,c(26,54,82,110,138,166,194)])),
           upper = cbind(t(mod_alldx_inctopov_allrace[4,c(3,31,59,87,115,143,171)]),t(mod_alldx_inctopov_allrace[4,c(7,35,63,91,119,147,175)]),
                         t(mod_alldx_inctopov_allrace[4,c(11,39,67,95,123,151,179)]),t(mod_alldx_inctopov_allrace[4,c(15,43,71,99,127,155,183)]),
                         t(mod_alldx_inctopov_allrace[4,c(19,47,75,103,131,159,187)]),t(mod_alldx_inctopov_allrace[4,c(23,51,79,107,135,163,191)]),t(mod_alldx_inctopov_allrace[4,c(27,55,83,111,139,167,195)])),
           col = fpColors(box = c('gray23','darkslategray3','palegreen3','lightgoldenrod','salmon1','indianred3','mediumpurple4')),
           legend=c("All","NHW","NHB","NHA","Mexican-American","Other Hispanic","Other/Multiracial"),
           legend_args = fpLegend(pos = list(x = 0.9, y = 0.9), gp = gpar(cex=0.9,col = "#CCCCCC", fill = "#F9F9F9")),
           xlab = "OR (95% CI)",
           title = 'Age and Gender-Adjusted OR for Continuous Income-to-Poverty Ratio on All Diseases, \n Overall and Stratified by Race/Ethnicity',
           txt_gp = fpTxtGp(title=gpar(cex=1),xlab=gpar(cex=1), ticks=gpar(cex=1), label=gpar(cex=1)),
           vertices=TRUE,
           line.margin=0.25,
           graph.pos=2,
           clip=c(0.6,1.4), xlim=c(0.6,1.4), xticks=c(0.6,0.8,1.0,1.2,1.4), xlog=TRUE, grid=structure(c(0.6,0.8,1.0,1.2,1.4)))









#####CODE NOT USED#####
#other code from Chirag
table_names_for_year <- function(year, table_list) {
  tibble(table_names=names(table_list[[which(years==year)]]), year=year)
}

var_names_for_year <- function(year, table_list) {
  tabs_for_year <- table_list[[which(years==year)]] ## list of tables for the year
  table_names <- names(tabs_for_year)
  y <- function(table_name, table_data) {
    tibble(table_name=table_name, varname=colnames(table_data))
  }
  map2(table_names, tabs_for_year, ~y(.x, .y)) %>% bind_rows() %>% mutate(year=year)
}

table_data_for_year <- function(year, table_name, table_list) {
  table_list[[which(years==year)]][[table_name]]
}

demo_table_names <- map(years, ~table_names_for_year(.x, demo_tables)) %>% bind_rows()
lab_table_names <- map(years, ~table_names_for_year(.x, lab_tables)) %>% bind_rows()
exam_table_names <- map(years, ~table_names_for_year(.x, exam_tables)) %>% bind_rows()
questionnaire_table_names <- map(years, ~table_names_for_year(.x, questionnaire_tables)) %>% bind_rows()

demo_var_names <- map(years, ~var_names_for_year(.x, demo_tables)) %>% bind_rows() %>% mutate(module="DEMO")
lab_var_names <- map(years, ~var_names_for_year(.x, lab_tables)) %>% bind_rows() %>% mutate(module="LABORATORY")
exam_var_names <- map(years, ~var_names_for_year(.x, exam_tables)) %>% bind_rows() %>% mutate(module="EXAMINATION")
qn_var_names <- map(years, ~var_names_for_year(.x, questionnaire_tables)) %>% bind_rows() %>% mutate(module="QUESTIONNAIRE")

var_names <- rbind(
  demo_var_names,
  lab_var_names,
  exam_var_names,
  qn_var_names
)



tables_to_get <- var_names %>% inner_join(vars_to_get %>% select(-module), by="varname") %>% select(table_name, year, module) %>% unique()

big_table_for_module <- function(tables_to_get, module_name, demo_tables_list, module_tables_list) {
  # tables_to_get: tibble with table_name, year, and module_name
  uniq_years <- unique(tables_to_get$year)
  labs <- vector(mode="list", length(uniq_years))
  for(yr in seq_along(uniq_years)) {
    yrs <- uniq_years[yr]
    demo_seqn <- demo_tables_list[[yr]][[1]] %>% select(SEQN)
    lb <- tables_to_get %>% filter(module == module_name, year == yrs)
    labs[[yr]] <- map(lb$table_name, ~table_data_for_year(yrs, .x, append(demo_seqn, module_tables_list, after = 0))) %>% join_all(by="SEQN", type="full")
  }
  
  module_table <- labs %>% bind_rows()
}


labs_all <- big_table_for_module(tables_to_get, "LABORATORY", demo_tables, lab_tables)
exam_all <- big_table_for_module(tables_to_get, "EXAMINATION", demo_tables, exam_tables)
questionnaire_all <- big_table_for_module(tables_to_get, "QUESTIONNAIRE", demo_tables, questionnaire_tables)

big_table <- demo_table %>% left_join(labs_all, by="SEQN") %>% left_join(exam_all, by="SEQN") %>% left_join(questionnaire_all, by="SEQN")

save(big_table, var_names, file = "nhanes_big_table_090322.Rdata")