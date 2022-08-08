#Load packages:
library(foreign)
library(psych)
library(car)
library(stringr)
library(ggplot2)
library(likert) 
library(here) # package to have relative paths, for ex.: file = here("data_analysis/plots/age.pdf")

# Load the data:
dat_complete <- read.csv("/home/cornelius/Documents/sustainability/mental_health/data_protected/preprocessed_data_v1.csv")
dat <- read.csv("/home/cornelius/Documents/sustainability/mental_health/data_protected/preprocessed_coarsed_data_v1.csv")

# JF:
#setwd("~/sustainAbility/Mental Health/Daten")
#dat_complete <- read.csv("preprocessed_data_v1.csv")
#dat <- read.csv("preprocessed_coarsed_data_v1.csv")

############################################################
# Sociodemographics
# currently: depending on dat (coarsed) AND dat_complete (preprocessed version)
############################################################

# Gender: report female
N = 589
sum(dat_complete$SD01==0,na.rm=TRUE)/N

# Age:  
describe(dat_complete$age)

# Nationalities: report Germans, Europeans
#N <- sum(table(dat$german))
sum(dat$german,na.rm = TRUE)/N
#N <- sum(table(dat$europe))
sum(dat$europe==1,na.rm = TRUE)/N

# Children
#N<-sum(table(dat$SD13))
sum(dat$SD13,na.rm = TRUE)/N

# Faculty
# 1 = Science  
# 2 = Economic and Social Sciences  
# 3 = Humanities
# 4 = Medicine 
# 5 = Law
# 6 = Theology
# 7 = Two faculties
#N<-sum(table(dat$faculty_all))
table(dat_complete$faculty_all, useNA = "always")/N *100

# Workload

#PhD Work/week
describe(dat$AP03_01)
# total Work/week
describe(dat$AP05_01)

# Phd Stage
describe(dat_complete$phd_month)
# "distribution"
# N = sum(table(dat$phdstage))
sum(dat$phdstage==1 |dat$phdstage==2,na.rm = TRUE)/N
sum(dat$phdstage==3 |dat$phdstage==4,na.rm = TRUE)/N
sum(dat$phdstage==5 |dat$phdstage==6,na.rm = TRUE)/N

# contract type
# N = sum(table(dat$EF01))
table(dat$EF01)/N *100
# contract length
describe(dat_complete$EF02_01)
# % of contract
describe(dat_complete$EF03_01)

##################################################
# Descriptives of Content Questions
# this should be all based on dat (coarsed data)
##################################################

# Section EV
# Todo#2: check which one and how to accumulate and insert to table 1 
#EV06, EV07, EV08: Job satisfaction (Hellgren et al.) @JF
JS <- rowMeans(subset(dat, select=c(EV06,EV07,EV08)))
describe(JS)

#### Section WG: Working Group (WG01 and WG02) ####
# give relative numbers 
# WG01: professional support
describe(dat$WG01)
prop.table(table(dat$WG01))
table(dat$WG01,useNA = 'always')

# WG02: emotional support
describe(dat$WG02)
prop.table(table(dat$WG02))
table(dat$WG02,useNA = 'always')
# Section GH
# Todo#4: check which one and how to accumulate and insert to table 1 
#GH02, GH03 recode! @JF
#GH01, GH02, GH03, GH04: Perceived Stress Scale (Cohen) @JF
dat$GH02R <- car::recode(dat$GH02,"1=5; 2=4; 3=3; 4=2;5=1; NA=NA")
dat$GH03R <- car::recode(dat$GH03,"1=5; 2=4; 3=3; 4=2;5=1; NA=NA")
PSS <- rowMeans(subset(dat, select=c(GH01,GH02R,GH03R,GH04)))
describe(PSS)

#### Section OR: Other responsibilities ####

# OR1: Other responsibilities

# Teaching
temp <- dat$OR01_01
describe(temp)
prop.table(table(temp))
table(temp,useNA = 'always')

# Supervision
temp <- dat$OR01_02
describe(temp)
prop.table(table(temp))
table(temp,useNA = 'always')

# Administration
temp <- dat$OR01_03
describe(temp)
prop.table(table(temp))
table(temp,useNA = 'always')

# Unrelated Research
temp <- dat$OR01_04
describe(temp)
prop.table(table(temp))
table(temp,useNA = 'always')

# Others
temp <- dat$OR01_08
describe(temp)
prop.table(table(temp))
table(temp,useNA = 'always')

# No further responsibilities
temp <- dat$OR01_09
describe(temp)
prop.table(table(temp))
table(temp,useNA = 'always')


### OR02: Time other responsibilities in % 
temp <- dat$OR02
describe(temp)
table(temp, useNA = 'always')

# Section ST: Stressors
# Todo#6: check which one and how to accumulate and insert to table 1 
#ST13, ST14, ST15: Job insecurity (Hellgren et al.) @JF
JI <- rowMeans(subset(dat, select=c(ST13,ST14,ST15)))
describe(JI)

# Section SH: Seeking Help
# Todo#7: check which one and how to accumulate and insert to table 1 

#### Section MH: Mental health ####

# MH01: feeled stressed
temp <- dat$MH01
describe(temp)
prop.table(table(temp))
table(temp,useNA = 'always')

# MH02: increased stress
temp <- dat$MH02
describe(temp)
prop.table(table(temp))
table(temp,useNA = 'always')

# MH03: Causes of stress
# TODO
temp <- dat$MH03
describe(temp)
prop.table(table(temp))
table(temp,useNA = 'always')

# MH04: Consultant for work related stress
# Nobody
temp <- dat$MH04_01
prop.table(table(temp))
table(temp,useNA = 'always')

# Colleagues
temp <- dat$MH04_02
prop.table(table(temp))
table(temp,useNA = 'always')

# Admins/Coordination
temp <- dat$MH04_03
prop.table(table(temp))
table(temp,useNA = 'always')

# Supervisor
temp <- dat$MH04_04
prop.table(table(temp))
table(temp,useNA = 'always')

# Others
temp <- dat$MH04_05
prop.table(table(temp))
table(temp,useNA = 'always')

#MH05: MH declined 
# TODO: to continue. until MH20+ 
# Todo#8: check which one and how to accumulate and insert to table 1 


####################################################
# Correlation analysis
####################################################
# Todo#9:  correlation(outcomes, predictors) => 4xn correlation table 
# “Outcomes”: PHQ-Depression (PHQ_2_1 + PHQ_2_2) and 
#PHQ-Anxiety (PHQ_9_1 to PHQ_9_6) (or combined?), 
#General Health (GH01 to GH04) 
# Predictors”: Sociodemographics (Gender, Age, Nationality, Family education background, new in Tübingen, care-taking, living-arrangement), 
# Phd characteristics (faculty, choice of topic, hours on PhD, working hours, phd stage, contract type, length of contract/scholarship, percentage of financing, other employment), 
# Working group characteristics (Professional support, emotional support, Other responsibilities), Stressors (Institutional, systemic, job insecurity) 


####################################################
# Analysis of open questions
####################################################
# Todo#10: prepare already for the data to come

######################################################
# Some plots
######################################################
# plot the age
pdf(file = here("data_analysis/plots/age.pdf"),  width = 4, height = 4)
hist(dat$age)
dev.off()

# make some more analysis and save plots....

