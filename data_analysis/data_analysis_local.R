# Load packages:
library(foreign)
library(psych)
library(car)
library(stringr)
library(ggplot2)
library(likert) 
library(here) # package to have relative paths, for ex.: file = here("data_analysis/plots/age.pdf")
library(apaTables)
library(ordinal)

# Load the data:
#dat_complete <- read.csv("/Users/ninaeffenberger/SustainAbility/data_v2/preprocessed_data_v1.csv")
#dat <- read.csv("/Users/ninaeffenberger/SustainAbility/data_v2/preprocessed_coarsed_data_v1.csv")

# JF:
#setwd("~/sustainAbility/Mental Health/Daten")
#dat_complete <- read.csv("preprocessed_data_v2.csv")
#dat <- read.csv("preprocessed_coarsed_data_v2.csv")

# CS:
# Load the data:
dat_complete <- read.csv("/home/cornelius/Documents/sustainability/mental_health/data_protected/preprocessed_data_v2.csv")
dat <- read.csv("/home/cornelius/Documents/sustainability/mental_health/data_protected/preprocessed_coarsed_data_v2.csv")


# set options for decimal:
options(scipen=999)
options(digits=3)

############################################################
#### Sociodemographics ####
# currently: depending on dat (coarsed) AND dat_complete (preprocessed version)
############################################################

# Gender: report female
N = 589
sum(dat_complete$SD01==0,na.rm=TRUE)/N
table(dat_complete$SD01, useNA = 'always')


# Age:  
describe(dat_complete$age)
table(dat_complete$age, useNA = 'always')

# Nationalities: report Germans, Europeans
#N <- sum(table(dat$german))
sum(dat$german,na.rm = TRUE)/N
#N <- sum(table(dat$europe))
sum(dat$europe==1,na.rm = TRUE)/N
table(dat$german, useNA = 'always')

# Children
#N<-sum(table(dat$SD13))
sum(dat$SD13,na.rm = TRUE)/N
table(dat$SD13, useNA = 'always')

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




# Phd Stage
describe(dat_complete$phd_month)
table(dat_complete$phd_month,useNA = 'always')
# "distribution" one interval is 6 months
# N = sum(table(dat$phdstage))
sum(dat$phdstage==1 |dat$phdstage==2,na.rm = TRUE)/N
sum(dat$phdstage==3 |dat$phdstage==4,na.rm = TRUE)/N
sum(dat$phdstage==5 |dat$phdstage==6,na.rm = TRUE)/N
sum(dat$phdstage==7 |dat$phdstage==8,na.rm = TRUE)/N


#### contract type ####
# N = sum(table(dat$EF01))
table(dat$EF01,useNA = 'always')/N *100
table(dat$EF01,useNA = 'always')
# contract length
describe(dat_complete$EF02_01)
table(dat_complete$EF02_01,useNA = 'always')
# people with less than 12 months in %:
table(dat$EF02_01)/sum(table(dat$EF02_01))
# % of contract
describe(dat_complete$EF03_01)
table(dat_complete$EF03_01,useNA = 'always')

#### Workload ####

#PhD Work/week
describe(dat$AP03_01)
table(dat$AP03_01, useNA = 'always')
# total Work/week
describe(dat$AP05_01)
table(dat$AP05_01, useNA = 'always')

# extra work 
table(dat$AP05_01-dat$AP03_01, useNA = 'always')
describe((dat$AP05_01-dat$AP03_01)[dat$AP05_01-dat$AP03_01>0])


##################################################
# Descriptives of Content Questions
# this should be all based on dat (coarsed data)
##################################################

#### Section EV ####
# Todo#2: Done. 
describe(dat$EV01)
describe(dat$EV04)
describe(dat$EV09)
table(dat$EV05,useNA="always")
prop.table(table(dat$EV05))

#EV06, EV07, EV08: Job satisfaction (Hellgren et al., 1997)
JS <- rowMeans(subset(dat, select=c(EV06,EV07,EV08)))
describe(JS)
dat$JS <- JS

# Cronbach's alpha job satisfaction scale
alphaJS <- cbind(EV06=dat$EV06,EV07=dat$EV07,EV08=dat$EV08)
alphaJS <- psych::alpha(alphaJS)
summary(alphaJS)
alphaJS$total$std.alpha #0.86

#### Section WG: Working Group (WG01 and WG02) ####
# WG01: professional support
describe(dat$WG01)
prop.table(table(dat$WG01))
table(dat$WG01,useNA = 'always')

# WG02: emotional support
describe(dat$WG02)
prop.table(table(dat$WG02))
table(dat$WG02,useNA = 'always')

#### Section GH ####
# Todo#4: Done.
#GH02, GH03 recode! @JF
#GH01, GH02, GH03, GH04: Perceived Stress Scale (Cohen, 1984; Büssing, 2011)
dat$GH02R <- car::recode(dat$GH02,"1=5; 2=4; 3=3; 4=2;5=1; NA=NA")
dat$GH03R <- car::recode(dat$GH03,"1=5; 2=4; 3=3; 4=2;5=1; NA=NA")
PSS <- rowMeans(subset(dat, select=c(GH01,GH02R,GH03R,GH04)))
describe(PSS)
# Cronbach's alpha perceived stress scale
alphaPSS <- cbind(GH01=dat$GH01,GH02R=dat$GH02R,GH03R=dat$GH03R,GH04=dat$GH04)
alphaPSS <- psych::alpha(alphaPSS)
summary(alphaPSS)
alphaPSS$total$std.alpha #0.79

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

#### Section ST: Stressors ####
# Todo#6: Done. 
#ST13, ST14, ST15: Job insecurity (Hellgren et al., 1999)
JI <- rowMeans(subset(dat, select=c(ST13,ST14,ST15)))
describe(JI)
dat$JI <- JI


# Cronbach's alpha job insecurity scale
alphaJI <- cbind(ST13=dat$ST13,ST14=dat$ST14,ST15=dat$ST15)
alphaJI <- psych::alpha(alphaJI)
summary(alphaJI)
alphaJI$total$std.alpha #0.80

# Institutional Stressors (supervisor), positive wording
# ST02,ST03,ST05,ST07
ISpositive <- rowMeans(subset(dat, select=c(ST02,ST03,ST05,ST07)))
describe(ISpositive)

# Cronbach's alpha stressors, positive
alphaISpositive <- cbind(ST02=dat$ST02,ST03=dat$ST03,ST05=dat$ST05,ST07=dat$ST07)
alphaISpositive <- psych::alpha(alphaISpositive)
summary(alphaISpositive)
alphaISpositive$total$std.alpha #0.85

# Institutional Stressors (supervisor), negative wording
# ST01,ST04,ST06,ST08
ISnegative <- rowMeans(subset(dat, select=c(ST01,ST04,ST06,ST08)))
describe(ISnegative)

# Cronbach's alpha stressors, negative
alphaISnegative <- cbind(ST01=dat$ST01,ST04=dat$ST04,ST06=dat$ST06,ST08=dat$ST08)
alphaISnegative <- psych::alpha(alphaISnegative)
summary(alphaISnegative)
alphaISnegative$total$std.alpha #0.85

# Other institutional stressors
# ST17: Mistreated by colleagues
describe(dat$ST17)
# Regular meetings supervisor
describe(dat$ST09)
# ST09: Frequency of meetings, Attention: other scale
describe(dat$ST16)
# ST11: Worrying lack of long-term contracts
describe(dat$ST11)
# ST12: Find a good job
describe(dat$ST12)



#### Section MH: Mental health ####

# MH01: feeled stressed
temp <- dat$MH01
describe(temp)
table(temp,useNA = 'always')

# MH02: increased stress
temp <- dat$MH02
describe(temp)
prop.table(table(temp))
table(temp,useNA = 'always')

# MH03: Causes of stress
# TODO
temp <- dat$MH03
#describe(temp)
#prop.table(table(temp))
#table(temp,useNA = 'always')

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
temp <- dat$MH05
describe(temp)
prop.table(table(temp))
table(temp,useNA = 'always')

#MH07: Other PhDs who are struggling
temp <- dat$MH07
describe(temp)
prop.table(table(temp))
table(temp,useNA = 'always')

#MH08: Does mh affect work quality negatively
temp <- dat$MH08
describe(temp)
prop.table(table(temp))
table(temp,useNA = 'always')

#MH09: special services
# No
temp <- dat$MH09_01
prop.table(table(temp))
table(temp,useNA = 'always')

# personalised coaching
temp <- dat$MH09_02
prop.table(table(temp))
table(temp,useNA = 'always')

# mentoring
temp <- dat$MH09_03
prop.table(table(temp))
table(temp,useNA = 'always')

#Time management courses
temp <- dat$MH09_04
prop.table(table(temp))
table(temp,useNA = 'always')

# stress management courses
temp <- dat$MH09_05
prop.table(table(temp))
table(temp,useNA = 'always')

# structural changes
temp <- dat$MH09_08
prop.table(table(temp))
table(temp,useNA = 'always')

# Other
temp <- dat$MH09_09
prop.table(table(temp))
table(temp,useNA = 'always')
# Todo#8: check which one and how to accumulate and insert to table 1 

# MH10: currently in psychotherapy
temp <- dat$MH10
prop.table(table(temp))
table(temp,useNA = 'always')

# MH11: ever diagnosed a mental disorder
temp <- dat$MH11
prop.table(table(temp))
table(temp,useNA = 'always')

#### Subsection PHQ: Perceived Health Questionnaire ####

#MH12: little interest in doing things, past 2 weeks
describe(dat$MH12)
prop.table(table(dat$MH12))
table(dat$MH12,useNA = 'always')

#MH14: feeling down, depressed or hopeless, past 2 weeks
describe(dat$MH14)
prop.table(table(dat$MH14))
table(dat$MH14,useNA = 'always')

PHQ2 <- rowMeans(subset(dat, select=c(MH12,MH14)))
describe(PHQ2)

# insert to data table
dat$PHQ2 <- PHQ2

#MH15: feeling nervous, anxious, on edge, past 4 weeks
#if "not at all", MH16-21 skipped
describe(dat$MH15)
prop.table(table(dat$MH12))
table(dat$MH15,useNA = 'always')

GAD7 <- rowSums(subset(dat, select=c(MH15,MH16,MH17,MH18,MH19,MH20,MH21)))
describe(GAD7)
table(GAD7, useNA = 'always')

dat$GAD7 <- GAD7

# MH16: being so restless that it's hard to sit still 
describe(dat$MH16)
prop.table(table(dat$MH16))
table(dat$MH16,useNA = 'always')

# MH17: being tired easily
describe(dat$MH17)
prop.table(table(dat$MH17))
table(dat$MH17,useNA = 'always')

# MH18: muscle tension or muscle pain
describe(dat$MH18)
prop.table(table(dat$MH18))
table(dat$MH18,useNA = 'always')

# MH19: trouble falling asleep or sleeping through 
describe(dat$MH19)
prop.table(table(dat$MH19))
table(dat$MH19,useNA = 'always')

#MH20: trouble on concentrating
describe(dat$MH20)
prop.table(table(dat$MH20))
table(dat$MH20,useNA = 'always')

#MH21: becoming easily annoyed or irritable
describe(dat$MH21)
prop.table(table(dat$MH21))
table(dat$MH21,useNA = 'always')

#### Section SH: Seeking Help ####
# Todo#7: Done. 
# SH01: tried to improve situation
temp <- dat$SH01
describe(temp)
table(temp,useNA = 'always')

# SH02: aware of consultation services at uni
temp <- dat$SH02
prop.table(table(temp))
table(temp,useNA = 'always')

# SH04: how much knowledge about consultation service
temp <- dat$SH04
describe(temp)
table(temp,useNA = 'always')

# SH05: Sought for help
temp <- dat$SH05
prop.table(table(temp))
table(temp,useNA = 'always')

# SH06: anyone to talk to
temp <- dat$SH06
prop.table(table(temp))
table(temp,useNA = 'always')

# SH10: Covid: affect general situation
temp <- dat$SH10
table(temp,useNA = 'always')

# SH11: Covid: affect answers of this survey
temp <- dat$SH11
describe(temp)
table(temp,useNA = 'always')


####################################################
#### Comparison of faculties for XXX ####
####################################################
# Faculty
# 1 = Science  
# 2 = Economic and Social Sciences  
# 3 = Humanities
# 4 = Medicine 
# 5 = Law
# 6 = Theology
# 7 = Two faculties
faculty_labels <- c('Science','Economic & Social Sc.','Humanities',
                    'Medicine','Law','Theology','Two faculties')

# http://www.sthda.com/english/wiki/kruskal-wallis-test-in-r
library(dplyr)
library(ggpubr)

# PHQ 2
## first view of means 
for (i in 1:7){
  print(i)
  print(mean(dat$PHQ2[dat_complete$faculty_all==i],na.rm = TRUE))
}

# make faculties to factors:
dat_complete$faculty_all <- factor(dat_complete$faculty_all)

# insert PHQ2 and GAD7 to dat_complete
dat_complete$PHQ2 <- dat$PHQ2
dat_complete$GAD7 <- dat$GAD7
dat_complete$JI <- dat$JI


# check levels
levels(dat_complete$faculty_all)

# PHQ2
group_by(dat_complete, faculty_all) %>%
  summarise(
    count = n(),
    mean = mean(PHQ2, na.rm = TRUE),
    sd = sd(PHQ2, na.rm = TRUE),
    median = median(PHQ2, na.rm = TRUE),
    IQR = IQR(PHQ2, na.rm = TRUE)
  )

kruskal.test(PHQ2 ~ faculty_all, data = dat_complete)

# GAD7 
group_by(dat_complete, faculty_all) %>%
  summarise(
    count = n(),
    mean = mean(GAD7, na.rm = TRUE),
    sd = sd(GAD7, na.rm = TRUE),
    median = median(GAD7, na.rm = TRUE),
    IQR = IQR(GAD7, na.rm = TRUE)
  )

kruskal.test(GAD7 ~ faculty_all, data = dat_complete)

pairwise.wilcox.test(dat_complete$GAD7, dat_complete$faculty_all,
                     p.adjust.method = "BH")


# JI 
group_by(dat_complete, faculty_all) %>%
  summarise(
    count = n(),
    mean = mean(JI, na.rm = TRUE),
    sd = sd(JI, na.rm = TRUE),
    median = median(JI, na.rm = TRUE),
    IQR = IQR(JI, na.rm = TRUE)
  )

kruskal.test(JI ~ faculty_all, data = dat_complete)

pairwise.wilcox.test(dat_complete$JI, dat_complete$faculty_all,
                     p.adjust.method = "BH")

# create boxplot
ggboxplot(na.omit(dat_complete[c("faculty_all","JI")]), 
          x = "faculty_all", 
          y = "JI", 
          #color = "group", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
          #order = c("ctrl", "trt1", "trt2"),
          title = 'Job insecurity',
          ylab = "JI", 
          xlab = "Faculty",
          notch = FALSE,
          x.text.angle=60)+
scale_x_discrete(labels=faculty_labels)





####################################################
#### Correlation analysis ####
####################################################
# Todo#9:  correlation(outcomes, predictors) => 4xn correlation table 
# “Outcomes”: PHQ-Depression (PHQ_2_1 + PHQ_2_2) and 
#PHQ-Anxiety (PHQ_9_1 to PHQ_9_6) (or combined?), 
#General Health (GH01 to GH04) 
# Predictors”: Sociodemographics (Gender, Age, Nationality, Family education background, new in Tübingen, care-taking, living-arrangement), 
# Phd characteristics (faculty, choice of topic, hours on PhD, working hours, phd stage, contract type, length of contract/scholarship, percentage of financing, other employment), 
# Working group characteristics (Professional support, emotional support, Other responsibilities), Stressors (Institutional, systemic, job insecurity) 

#Correlations
correlationstable <- cbind(dat$PHQ2, dat$GAD7)
apa.cor.table(correlationstable, filename="TablePHQ.doc", table.number=1)

# linear Regression
#Stressors -> Perceived Stress Scale
model1 <- lm(PSS ~ age + SD01 + ST01 + ST02 + ST03 + ST04 + ST17 + ST05 + ST07 + ST09 , data=dat)
summary(model1)

#Predictors Job insecurity, job satisfaction, life satisfaction, Perceived Stress Scale
model2 <- lm(PHQ2 ~ age + SD01 + JI + JS + EV09 + PSS, data=dat)
summary(model2)
#F(6, 485)=71.29, p < .01, R² = 46.9%

model3 <- lm(GAD7 ~ age + SD01 + JI + JS + EV09 + PSS, data=dat)
summary(model3)
#F(6, 395)=36.15, p < .01, R² = 35.5%

# Job insecurity
#Predictors: age, gender, phdstage, contract_length, percentage_of_employment
model4 <- lm(JI ~ age + SD01 +  phdstage + EF02_01 + EF03_01, data=dat)
summary(model4)

# Job satisfaction
#Predictors: age, gender, phdstage, contract_length, percentage_of_employment, 
# professional_support, emotional_support
model5 <- lm(JS ~ age + SD01 +  phdstage + EF02_01 + EF03_01 + WG01 + WG02, data=dat)
summary(model5)





#### ordinal logistic regression ####
# https://cran.r-project.org/web/packages/ordinal/ordinal.pdf

# check proportional odds assumption
p <-as.numeric(proportions(table(dat$PHQ2) ))

for (i in c(1:(length(p)-1))){
  print( log(sum(p[1:i]) /sum(p[(i+1):length(p)])))
  }

# convert PHQ2 to ordered factor
PHQ2_factor_ordered <- factor(PHQ2, ordered = TRUE, 
                                levels = c(0, 0.5, 1,1.5, 2, 2.5 ,3))
fm1 <- clm(PHQ2_factor_ordered ~ age + JI + JS, data=dat)
summary(fm1)


####################################################
#### Analysis of open questions ####
####################################################
# Todo#10: prepare already for the data to come

######################################################
#### Some plots #### 
######################################################
# plot the age
#pdf(file = here("data_analysis/plots/age.pdf"),  width = 4, height = 4)
#hist(dat$age)
#dev.off()

# make some more analysis and save plots....

#rm(list = ls())
