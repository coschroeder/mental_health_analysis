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

# Section WG
# Todo#3: check which one and how to accumulate and insert to table 1 

# Section GH
# Todo#4: check which one and how to accumulate and insert to table 1 

# Section OR
# Todo#5: check which one and how to accumulate and insert to table 1 

# Section ST: Stressors
# Todo#6: check which one and how to accumulate and insert to table 1 

# Section SH: Seeking Help
# Todo#7: check which one and how to accumulate and insert to table 1 

# Section MH: Mental health
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

