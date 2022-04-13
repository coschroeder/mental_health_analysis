#######################################################
# R script Mental Health                              #
# sustainAbility PhD Initivative                      #
# R version                                           #
#######################################################

### Activate packages 
library(foreign)
library(psych)
library(car)
library(stringr)
library(ggplot2)
library(likert) 
recode <- car::recode

#######################################################
### Read data set 
#######################################################

# Read data including the labels
# dat<-read.spss("C:/Daten/Mental/Mental_Health_Rawdata_v1.sav",use.value.labels=T, to.data.frame=T,use.missings=T)
dat<-read.spss("/home/cornelius/Documents/sustainability/mental_health/data_protected/Mental_Health_Rawdata_v1.sav",
               use.value.labels=T, to.data.frame=T,use.missings=T)

# Read data w/o labels
dat_n<-read.spss("/home/cornelius/Documents/sustainability/mental_health/data_protected/Mental_Health_Rawdata_v1.sav",
               use.value.labels=F, to.data.frame=T,use.missings=T)

##################################################### 
### Rough filtering out non-answering participants
#####################################################
### suggestion: 
### drop rows if the first 3 variables are missings AND total time (time_sum) is<=180s
dat$drop_filter <- NA
dat$drop_filter <- ifelse(is.na(dat$SD01== T) & is.na(dat$SD02_01== T) & is.na(dat$SD08== T) & dat$TIME_SUM <= 180, 0,1)
dat_n$drop_filter <- dat$drop_filter  
table(dat$drop_filter, useNA = "always") # N = 64 will be dropped as they have not started the questionnaire
dat <- subset(dat, drop_filter==1)
dat_n <- subset(dat_n, drop_filter==1)

# Drop test run with SD02_01 (age) == 1
mask <-  ((!is.na(dat$SD02_01==1) & dat$SD02_01==1))
dat <- dat[!mask,]
dat_n <- dat_n[!mask,]

# Define empty list, in which all variables to delete are stored
data_to_drop <- c()

# Create completely new table which will be the final one to share
dat_new <- data.frame(dat$CASE)
names(dat_new)[1] <- "CASE"

###############################################################
### Section SD: Sociodemografics
###############################################################

###############################################################
# [SD01] Gender "Which gender do you identify with?"
        ### 1 = diverse; 2 = female; 3 = male; 4 = rather not say; NA = Not answered
dat_new$SD01 <- dat_n$SD01 


###############################################################
# [SD02] Age "How old are you?" SD02_01 I am ... years old

dat$age <- as.numeric(dat$SD02)
table(dat$age, deparse.level = 2, useNA = "always")
# check: value 3 is invalid -> recode
dat$age <- ifelse( dat$age ==3, NA,dat$age)
dat_new$age <- dat$age

###############################################################
# [SD08] Nationality "What is your nationality?"
# Recode to 4 variables:
# continent: "Europe","North America","South America","Asia","Africa","Australia"
# europe: with 1=Europe 0=Non-Europe
# german:  1=German, 0=Non-German
# continent_coarsed: "Europe","Asia","Other"

# Assign countries to continents
# Labels: 1=Europe 2=North America  3=South America 4=Asia 5= Africa 6=Australia
dat$continent <- recode(dat$SD08, 
                        "'Afghanestan'=	4;
                        'Albania'=	1;
                        'Argentina'=	3;
                        'Australia'=	6;
                        'Austria'=	1;
                        'Bangladesch'=	4;
                        'Belgium'=	1;
                        'Brasil'=	3;
                        'Burundi'=	5;
                        'Cameroons'=	5;
                        'Canada'=	2;
                        'Chile'=	3;
                        'China (People`s Republic of China)'=	4;
                        'Taiwan'=	4;
                        'Colombia'=	3;
                        'Costa Rica'=	3;
                        'Croatia'=	1;
                        'Cyprus'=	1;
                        'Czech Republic'=	1;
                        'Denmark'=	1;
                        'Ecuador'=	3;
                        'France'=	1;
                        'Georgia'=	1;
                        'Germany'=	1;
                        'Greece'=	1;
                        'Hungary'=	1;
                        'India'=	4;
                        'Iran'=	4;
                        'Ireland'=	1;
                        'Italia'=	1;
                        'Japan'=	4;
                        'Kenya'=	5;
                        'North Korea'=	4;
                        'South Korea'=	4;
                        'Lebanon'=	4;
                        'Luxembourg'=	1;
                        'MÃ©xico'=	3;
                        'Netherlands'=	1;
                        'Pakistan'=	4;
                        'Pitcairn Islands'=	6;
                        'Portugal'=	1;
                        'Russia'=	1;
                        'Serbia'=	1;
                        'Slovakia'=	1;
                        'Vietnam'=	4;
                        'Spain'=	1;
                        'Sudan'=	5;
                        'Sweden'=	1;
                        'Switzerland'=	1;
                        'Syria'=	4;
                        'Turkey'=	4;
                        'Ukraine'=	1;
                        'Macedonia'=	1;
                        'Egypt'=	5;
                        'United Kingdom'=	1;
                        'United States'=	2;
                        'Yemen'=	5;
                        'Kosovo'=	1;
                        'Latvia'= 1;
                        'New Zealand'= 6;
                        'Philippines'= 4;
                        'Poland'= 1;
                        'Romania'= 1;")
table(dat$continent, useNA = "always", deparse.level = 2)

# assign value labels
dat$continent <- factor(dat$continent,
                        levels = c(1,2,3,4,5,6),
                        labels = c("Europe","North America","South America","Asia","Africa","Australia"))
table(dat$continent, useNA ="always")

# Create new variable continent_coarsed with coarsed continent
    ### 1 = Europe
    ### 2 = Asia
    ### 3 = Others
dat$continent_coarsed <- recode(dat$continent, "'Europe' = 1; 'North America'= 3; 'South America'= 3;
                     'Asia'= 2; 'Australia'= 3; 'Africa' = 3; 'Australia' = 3")

#Create new variable europe
    ### 1=Europe 
    ### 0=Non-Europe
dat$europe <- recode(dat$continent, "'Europe' = 1; 'North America'= 0; 'South America'= 0;
                     'Asia'= 0; 'Australia'= 0; 'Africa' = 0; 'Australia' = 0;")
# check 
table(dat$europe, useNA ="always")

#Create new variable german with 
    ### 1=German, 
    ### 0=Non-German, 
    ### NA remain NA
dat$german <- ifelse(dat$SD08 == 'Germany', 1,0)
table(dat$german, useNA = "always")

# add to new dataframe
dat_new <- cbind(dat_new, dat[c('german', 'europe', 'continent_coarsed')])

# Country will be dropped in the end
data_to_drop <- append(data_to_drop, "SD08")

# Todo #1: decide which level to keep in the end. 

###############################################################
# [SD10] Family education background "What is your family's academic background?"
        ### 1 = academic (at least one parent/legal guardian has a university degree)
        ### 2 = non-academic (none of my parents/legal guardians has a university degree)
        ### NA = Not answered
# recode to 
        ### 0 = non-academic
        ### 1 = academic
table(dat_n$SD10, useNA = "always")
dat_new$SD10 <- recode(dat_n$SD10, "'1'=1; '2'=0;")

###############################################################
# [SD11] Formale Bildung (einfach) "Did you move to T?bingen for your Ph.D.?"
        ### 1 = no
        ### 9 = yes, from another place in Germany
        ### 3 = yes, from another country
        ### NA = Not answered
# recode to 
        ### 0 = no
        ### 1 = yes, from another place in Germany
        ### 2 = yes, from another country
        ### NA = Not answered
table(dat_n$SD11, useNA = "always")
dat_new$SD11 <- recode(dat_n$SD11, "'1'=0; '9'=1;'3'=2;'NA'=-1")
table(dat_new$SD11, useNA = "always")
table(dat$SD11, deparse.level = 2, useNA = "always")


###############################################################
# [SD12] Beruflicher Bildungsabschluss "What professional qualifications do you have?"
        ### 11 = University degree
        ### 10 = Vocational university/university of applied sciences degree
        ### 9 = Master/technical certification or equivalent vocational qualification
        ### 8 = General vocational qualification
        ### 7 = Specialized vocational qualification
        ### 2 = Vocational training period with diploma
        ### 3 = (Compact) vocational training program/course
        ### 4 = Completed industrial or agricultural apprenticeship
        ### 5 = Completed commerical traineeship
        ### 6 = Internship, practical training
        ### 1 = No professional qualifications
        ### 12 = Other degree:
        ###  NA = Not answered
        ### SD12_12 Other degree. Free text

# This variable was not in the final survey. 

###############################################################
# [SD13] Care-taking "Do you have a child/a person in your family you have to take care of?"
        ### 1 = no; 2 = yes; NA = Not answered
# recode to 
  ### 0 = no
  ### 1 = yes, from another place in Germany
  ### -1 = Not answered
dat_new$SD13 <- recode(dat_n$SD13, "'1'=0; '2'=1;'-9'=-1")

table(dat$SD13, deparse.level = 2, useNA = "always")

###############################################################
# [SD14] Living-arrangement "What is your housing situation?" (MC)
        ### SD14_01 Alone
        ### SD14_02 Together with parents
        ### SD14_03 Together with partner
        ### SD14_04 Student accommodation
        ### SD14_05 Shared flat
        ### SD14_08 Other, please specify
        ### 1 = Not checked
        ### 2 = Checked
        ### SD14_08a Other, please specify (free text)
# new variable: living
      ### 1: Alone
      ### 2: Together with parents
      ### 3: Together with partner
      ### 4: Student accommodation
      ### 5: Shared flat
      ### 6: together with family


table(dat$SD14, deparse.level = 2, useNA = "always") 
dat_new$living[dat$SD14_01 == 'ausgewählt'] <- '1' 
dat_new$living[dat$SD14_02 == 'ausgewählt'] <- '2' 
dat_new$living[dat$SD14_03 == 'ausgewählt'] <- '3' 
dat_new$living[dat$SD14_04 == 'ausgewählt'] <- '4' 
dat_new$living[dat$SD14_05 == 'ausgewählt'] <- '5' 
dat_new$living[dat$SD14_08 == 'ausgewählt'] <- '6' 
dat_new$living[dat$CASE == '1141'] <- '3' 
table(dat_new$living, deparse.level = 2, useNA = "always") 

# Todo #2: include factors?
# assign value labels 
#dat$living <- factor(dat$living, 
#                     levels = c(1,2,3,4,5,6), 
#                     labels = c("Alone","Together with parents","Together with partner","Student accommodation","Shared flat","Together with family")) 
table(dat$living, useNA ="always") 



table(dat$SD14, deparse.level = 2, useNA = "always")



###############################################################
### Section AP: About your PhD
###############################################################

###############################################################
# [AP01] Faculty "Which faculty are your studies associated with?" (MC)
        ### AP01_01 Protestant Theology
        ### AP01_02 Catholic Theology
        ### AP01_03 Law
        ### AP01_04 Medicine
        ### AP01_05 Humanities
        ### AP01_06 Economics and Social Sciences
        ### AP01_07 Science
        ### AP01_08 Center for Islamic Theology (ZITh)
        ### AP01_09 Other, please specify
        ### 1 = Not checked
        ### 2 = Checked
        ### AP01_09a Other, please specify (free text)

# Create a subset and transform to numeric
sub_fac <- c("AP01_01","AP01_02","AP01_03","AP01_04","AP01_05","AP01_06","AP01_07","AP01_08","AP01_09")
sub_fac <- dat[sub_fac]
sub_fac[] <- lapply(sub_fac, function(x) as.numeric(x))

### Frequencies for each faculty
table(sub_fac$AP01_01, useNA = "always") # Protestant Theology n = 11
table(sub_fac$AP01_02, useNA = "always") # Catholic Theology n = 4
table(sub_fac$AP01_03, useNA = "always") # Law n = 20
table(sub_fac$AP01_04, useNA = "always") # Medicine n = 34
table(sub_fac$AP01_05, useNA = "always") # Humanities n = 77
table(sub_fac$AP01_06, useNA = "always") # Economics and Social Sciences n = 68
table(sub_fac$AP01_07, useNA = "always") # Science n = 353
table(sub_fac$AP01_08, useNA = "always") # Center for Islamic Theology (ZITh) n = 0
table(sub_fac$AP01_09, useNA = "always") # other n = 18

# Check multiple responses
sub_fac$check_sum <- rowSums(sub_fac[,1:9],na.rm=F) 
table(sub_fac$check_sum, useNA = "always") # N= 26 have more than one subject

# Create one new variable with  1 = Science  # 2 = Economic and Social Sciences  # 3 = Humanities
# 4 = Medicine  # 5 = Law  # 6 = Others  # 7 = Two faculties

sub_fac$faculty <- NA
sub_fac$faculty <- ifelse(sub_fac$AP01_01 == 2 | sub_fac$AP01_02 == 2, 6,sub_fac$faculty)
table(sub_fac$faculty, useNA = "always")
sub_fac$faculty <- ifelse(is.na(sub_fac$faculty == T) & sub_fac$AP01_03 == 2, 5,sub_fac$faculty)
sub_fac$faculty <- ifelse(is.na(sub_fac$faculty == T) & sub_fac$AP01_04 == 2, 4,sub_fac$faculty)
sub_fac$faculty <- ifelse(is.na(sub_fac$faculty == T) & sub_fac$AP01_05 == 2, 3,sub_fac$faculty)
sub_fac$faculty <- ifelse(is.na(sub_fac$faculty == T) & sub_fac$AP01_06 == 2, 2,sub_fac$faculty)
sub_fac$faculty <- ifelse(is.na(sub_fac$faculty == T) & sub_fac$AP01_07 == 2, 1,sub_fac$faculty)
dat$faculty <- sub_fac$faculty 
table(dat$continent, dat$faculty, deparse.level = 2, useNA = "always")

# Find rows with multiple responses
which(sub_fac == 12, arr.ind=TRUE)
### NOTE: To be continued
# TODO #3

### Faculty: Additional open answer
table(dat$AP01_09a, useNA = "always")
# Remove all special characters
dat$AP01_09a_re <- str_replace_all(dat$AP01_09a, "[^[:alnum:]]", "") 
# Recode open answers to faculties
dat$AP01_09a_re <- recode(dat$AP01_09a_re,
                          "'AICenter'= 1; 'Biochemistry'= 1; 'CIN'= 1; 'geoscience'= 1;
  'Geoscience'= 1; 'GraduateTrainingCentreforNeuroscience'= 1; 'GraduateTrainingCentreofNeuroscience'= 1;
  'HistoryCulturalStudies'= 3; 'HistoryCulturalTheory'= 3; 'Kunstgeschichte'= 3;
  'mathematicsandnaturalsciencesMNF'= 1; 'mathematischnaturwissenschaftlichefakultaet'= 1;
  'movedtoJenaUniversityclinicsforPsychiatrystillgraduatingfromGTC'= 1;
  'PharmaceuticalSciencesinImmunology'= 4; 
  'Pharmacy'= 4;'PhilosophischeFakultät'= 3;'Psychology'= 1;'sportsandscience'= 2;") 
table(dat$AP01_09a_re)
### NOTE: To be continued
# TODO #4

# TODO #5 add to new dataframe

###############################################################
# [AP02] Choice of Topic "Did you choose the topic of your Ph.D. yourself?"
        ### 1 = no 2 = yes NA = Not answered
# recode to   
    ### 0 = no
    ### 1 = yes
    ### NA = Not answered

table(dat_n$AP02, useNA = "always")
dat_new$AP02 <- recode(dat_n$AP02, "'1'=0; '2'=1;'-9'=-1")

###############################################################
# [AP03] Hours per week Ph.D "How many hours a week do you spend on your Ph.D.?"
        ### AP03_01 ... hours/week
dat$AP03_01 <- str_trim(dat$AP03_01)
dat$AP03_01 <- car::recode(dat$AP03_01, "'26-30' = '28';
                                    'totally different, over the last years, main profession is that of a teacher - more hours durin holidays, fewer during term' = '';
                                    '34-40' = '37';
                                    '50 hours/week' = '50';
                                    '50-60' = '55';
                                    '35h' = '35';
                                    '5-10' = '7';
                                    '40-45' = '42';
                                    '45 - 50' = '47';
                                    '35-40' = '37';
                                    '50+' = '50';
                                    '40-50' = '45';
                                    '20-45' = '32';
                                    '18-20' = '19';
                                    '45-50' = '47';
                                    '50 or more' = '50';
                                    '40-52' = '46';
                                    '50+' = '50';
                                    '45-55' = '50';
                                    '35-40' = '37';
                                    'ca 35' = '35';
                                    '60-90' = '75';
                                    '24-30' = '27';
                                    '40-60' = '50';
                                    '50-60' = '55';
                                    '39.6' = '39';
                                    '40-42' = '41';
                                    '30-40' = '35';
                                    '50-70' = '60';
                                    '45-60' = '52';
                                    '40+' = '40';
                                    '20/30' = '25';
                                    '42.5' = '42';
                                    '35-50' = '42';
                                    '15-25' = '20';
                                    '~30' = '30';
                                    '>50' = '50';
                                    '15-20' = '17';
                                    '16 (unpaid)' = '16';
                                    '18-20' = '19'")
dat_new$AP03_01 <- as.numeric(dat$AP03_01)
describe(dat_new$AP03_01)

###############################################################
#[AP05] Hours per week work "How many hours a week do you work in total?"
      ### AP05_01 ... hours/week
dat$AP05_01 <- str_trim(dat$AP05_01)
dat$AP05_01 <- car::recode(dat$AP05_01, "'26-30' = '28';
                                    '50 hours/week' = '50';
                                    ''35' = '35';
                                    '50-60' = '55';
                                    '40-45' = '42';
                                    '35-40' = '37';
                                    '45-50' = '47';
                                    '35-40' = '37';
                                    '50+' = '50';
                                    '20-45' = '32';
                                    '50 or more' = '50';
                                    '40-52' = '46';
                                    '37-42' = '39';
                                    '45-55' = '50';
                                    'ca 40' = '40';
                                    '60-90' = '75';
                                    '36-40' = '38';
                                    '40-60' = '50';
                                    '40-50' = '45';
                                    '54-74' = '64';
                                    '40 (paid)' = '40';
                                    '50-70' = '60';
                                    '40+' = '40';
                                    '40/50' = '45';
                                    '42.5' = '42';
                                    '>50' = '50';
                                    ")
dat_new$AP05_01 <- as.numeric(dat$AP05_01)
describe(dat_new$AP05_01)

# new variable: timedif
    ## build difference between phd wporking hours and total working hours
dat_new$timedif <- (dat_new$AP05_01 - dat_new$AP03_01)

describe(dat_new$timedif)
table(dat$timedif, useNA = "always")      

###############################################################
# [AP04] Start Ph.D."When did you start your Ph.D.?"
      ### AP04_01 
# create new variables to coarse the data:
  ## startyear: year of the start of the PhD
  ## phdstage: Time spent as a phD 
    ### 1 = 0-6months of PhD; 
    ### 2 = 7-12 months; 
    ### 3 = 13-18months; etc.

table(dat$AP04_01,useNA = "always")
dat$AP04_01 <- as.character(dat$AP04_01)
dat$AP04_01 <- str_replace_all(dat$AP04_01, "[^[:alnum:]]", "") 
dat$startyear <- recode(dat$AP04_01,"'01012017' = 2017;
                         '01012018' = 2018;
                         '01012021' = 2021;
                         '01022019' = 2019;
                         '01022020' = 2020;
                         '01052015' = 2015;
                         '01062019' = 2019;
                         '01082019' = 2019;
                         '012021' = 2021;
                         '01012015' = 2015;
                         '01042021' = 2021;
                         '01102019' = 2019;
                         '0120' = 2020;
                         '012014' = 2014;
                         '012015' = 2015;
                         '012016' = 2016;
                         '012017' = 2017;
                         '012018' = 2018;
                         '012019' = 2019;
                         '012020' = 2020;
                         '012021' = 2021;
                         '022019' = 2019;
                         '022021' = 2021;
                         '022013' = 2013;
                         '022016' = 2016;
                         '022017' = 2017;
                         '022018' = 2018;
                         '022019' = 2019;
                         '022020' = 2020;
                         '022021' = 2021;
                         '03042019' = 2019;
                         '032018' = 2018;
                         '032017' = 2017;
                         '032018' = 2018;
                         '032019' = 2019;
                         '032020' = 2020;
                         '032021' = 2021;
                         '042013' = 2013;
                         '042021' = 2021;
                         '042013' = 2013;
                         '042014' = 2014;
                         '042015' = 2015;
                         '042016' = 2016;
                         '042017' = 2017;
                         '042018' = 2018;
                         '042019' = 2019;
                         '042020' = 2020;
                         '042021' = 2021;
                         '0421' = 2021;
                         '052021' = 2021;
                         '052014' = 2014;
                         '052015' = 2015;
                         '052016' = 2016;
                         '052018' = 2018;
                         '052019' = 2019;
                         '052020' = 2020;
                         '052021' = 2021;
                         '062011' = 2011;
                         '062013' = 2013;
                         '062016' = 2016;
                         '062017' = 2017;
                         '062018' = 2018;
                         '062019' = 2019;
                         '062020' = 2020;
                         '062021' = 2021;
                         '072014' = 2014;
                         '072017' = 2017;
                         '072018' = 2018;
                         '072019' = 2019;
                         '072020' = 2020;
                         '072021' = 2021;
                         '072020' = 2020;
                         '082015' = 2015;
                         '082016' = 2016;
                         '082017' = 2017;
                         '082018' = 2018;
                         '082019' = 2019;
                         '082020' = 2020;
                         '092019' = 2019;
                         '092021' = 2021;
                         '092011' = 2011;
                         '092015' = 2015;
                         '092016' = 2016;
                         '092017' = 2017;
                         '092018' = 2018;
                         '092019' = 2019;
                         '092020' = 2020;
                         '092021' = 2021;
                         '0920216' = 2016;
                         '1' = NA;
                         '12020' = 2020;
                         '102019' = 2019;
                         '102020' = 2020;
                         '1018' = 2018;
                         '102010' = 2010;
                         '102014' = 2014;
                         '102015' = 2015;
                         '102016' = 2016;
                         '102017' = 2017;
                         '102018' = 2018;
                         '102019' = 2019;
                         '102020' = 2020;
                         '102021' = 2021;
                         '1021' = 2021;
                         '102018' = 2018;
                         '102019' = 2019;
                         '111220' = 2020;
                         '112019' = 2019;
                         '112015' = 2015;
                         '112016' = 2016;
                         '112017' = 2017;
                         '112018' = 2018;
                         '112019' = 2019;
                         '112020' = 2020;
                         '112021' = 2021;
                         '112019' = 2019;
                         '122017' = 2017;
                         '122012' = 2012;
                         '122016' = 2016;
                         '122017' = 2017;
                         '122018' = 2018;
                         '122019' = 2019;
                         '122020' = 2020;
                         '15032018' = 2018;
                         '15052015' = 2015;
                         '151116' = 2016;
                         '2014' = NA; # no months
                         '2015' = NA;
                         '2016' = NA;
                         '2017' = NA;
                         '2018' = NA;
                         '2019' = NA;
                         '2020' = NA;
                         '62018' = 2018;
                          '' = NA;")
table(dat$startyear, useNA = "always")


dat$startmonth <- recode(dat$AP04_01,"'01012017' = 1;
                         '01012018' =1;
                         '01012021' =1;
                         '01022019' =2;
                         '01022020' =2;
                         '01052015' =5;
                         '01062019' =6;
                         '01082019' =8;
                         '012021' = 1;
                         '01012015' = 1;
                         '01042021' = 4;
                         '01102019' = 10;
                         '0120' = 1;
                         '012014' = 1;
                         '012015' = 1;
                         '012016' = 1;
                         '012017' = 1;
                         '012018' = 1;
                         '012019' = 1;
                         '012020' = 1;
                         '012021' = 1;
                         '022019' = 2;
                         '022021' = 2;
                         '022013' = 2;
                         '022016' = 2;
                         '022017' = 2;
                         '022018' = 2;
                         '022019' = 2;
                         '022020' = 2;
                         '022021' = 2;
                         '03042019' = 4;
                         '032018' = 3;
                         '032017' = 3;
                         '032018' = 3;
                         '032019' = 3;
                         '032020' = 3;
                         '032021' = 3;
                         '042013' = 4;
                         '042021' = 4;
                         '042013' = 4;
                         '042014' = 4;
                         '042015' = 4;
                         '042016' = 4;
                         '042017' = 4;
                         '042018' = 4;
                         '042019' = 4;
                         '042020' = 4;
                         '042021' = 4;
                         '0421' = 4;
                         '052021' = 5;
                         '052014' = 5;
                         '052015' = 5;
                         '052016' = 5;
                         '052018' = 5;
                         '052019' = 5;
                         '052020' = 5;
                         '052021' = 5;
                         '062011' = 6;
                         '062013' = 6;
                         '062016' = 6;
                         '062017' = 6;
                         '062018' = 6;
                         '062019' = 6;
                         '062020' = 6;
                         '062021' = 6;
                         '072014' = 7;
                         '072017' = 7;
                         '072018' = 7;
                         '072019' = 7;
                         '072020' = 7;
                         '072021' = 7;
                         '072020' = 7;
                         '082015' = 8;
                         '082016' = 8;
                         '082017' = 8;
                         '082018' = 8;
                         '082019' = 8;
                         '082020' = 8;
                         '092019' = 9;
                         '092021' = 9;
                         '092011' = 9;
                         '092015' = 9;
                         '092016' = 9;
                         '092017' = 9;
                         '092018' = 9;
                         '092019' = 9;
                         '092020' = 9;
                         '092021' = 9;
                         '0920216' = 9;
                         '1' = NA;
                         '12020' = 1;
                         '102019' = 10;
                         '102020' = 10;
                         '1018' = 10;
                         '102010' = 10;
                         '102014' = 10;
                         '102015' = 10;
                         '102016' = 10;
                         '102017' = 10;
                         '102018' = 10;
                         '102019' = 10;
                         '102020' = 10;
                         '102021' = 10;
                         '1021' = 10; 
                         '102018' = 10;
                         '102019' = 10;
                         '111220' = 11;
                         '112019' = 11;
                         '112015' = 11;
                         '112016' = 11;
                         '112017' = 11;
                         '112018' = 11;
                         '112019' = 11;
                         '112020' = 11;
                         '112021' = 11;
                         '112019' = 11;
                         '122017' = 12;
                         '122012' = 12;
                         '122016' = 12;
                         '122017' = 12;
                         '122018' = 12;
                         '122019' = 12;
                         '122020' = 12;
                         '15032018' = 3;
                         '15052015' = 5;
                         '151116' = 11;
                         '2014' = NA;
                         '2015' = NA;
                         '2016' = NA;
                         '2017' = NA;
                         '2018' = NA;
                         '2019' = NA;
                         '2020' = NA;
                         '62018' = 6;
                          '' = NA;")
table(dat$startmonth, useNA = "always")

# Date of the survey 2021*12+11 = 24263 months Note: 11 for Nov. because survey took place between Oct.-Dec.
dat$yearstotal <- (dat$startyear*12)
dat$phd_month <- (24263 - (dat$yearstotal +dat$startmonth))

table(dat$phd_month, useNA = "always")

#Create new variable: Time spent as a phD 
      ### 1 = 0-6months of PhD; 
      ### 2 = 7-12 months; 
      ### 3 = 13-18months; etc.

dat$phdstage <- NA
dat$phdstage <- ifelse(dat$phd_month <=6, 1,dat$phdstage)
dat$phdstage <- ifelse(is.na(dat$phdstage == T) & dat$phd_month <=12, 2, dat$phdstage)
dat$phdstage <- ifelse(is.na(dat$phdstage == T) & dat$phd_month <=18, 3, dat$phdstage)
dat$phdstage <- ifelse(is.na(dat$phdstage == T) & dat$phd_month <=24, 4, dat$phdstage)
dat$phdstage <- ifelse(is.na(dat$phdstage == T) & dat$phd_month <=30, 5, dat$phdstage)
dat$phdstage <- ifelse(is.na(dat$phdstage == T) & dat$phd_month <=36, 6, dat$phdstage)
dat$phdstage <- ifelse(is.na(dat$phdstage == T) & dat$phd_month <=42, 7, dat$phdstage)
dat$phdstage <- ifelse(is.na(dat$phdstage == T) & dat$phd_month <=48, 8, dat$phdstage)
dat$phdstage <- ifelse(is.na(dat$phdstage == T) & dat$phd_month <=54, 9, dat$phdstage)
dat$phdstage <- ifelse(is.na(dat$phdstage == T) & dat$phd_month <=60, 10, dat$phdstage)
dat$phdstage <- ifelse(is.na(dat$phdstage == T) & dat$phd_month <=66, 11, dat$phdstage)
dat$phdstage <- ifelse(is.na(dat$phdstage == T) & dat$phd_month <=72, 12, dat$phdstage)
dat$phdstage <- ifelse(is.na(dat$phdstage == T) & dat$phd_month <=78, 13, dat$phdstage)
dat$phdstage <- ifelse(is.na(dat$phdstage == T) & dat$phd_month >=79, 14, dat$phdstage)
table(dat$phdstage, useNA = "always")

# append to new data
dat_new$phdstage <- dat$phdstage

data_to_drop <- append(data_to_drop, "AP04_01")
data_to_drop <- append(data_to_drop, "startmonth")
data_to_drop <- append(data_to_drop, "phdmonth")


###############################################################
### Section EF: Employment/Financial Situation
###############################################################

###############################################################
# [EF01] Contract Type "How are you financed? (Contract type only related to your Ph.D.)"
      ### 1 = Permanent employment
      ### 2 = Temporary employment
      ### 3 = Hourly employment
      ### 6 = Scholarship (employed, paying social security)
      ### 7 = Scholarship (not employed)
      ### 5 = Not employed
      ### 8 = Other, please specify:
      ###NA = Not answered
      ### EF01_08 Other, please specify
      ### if multiple matches in open answers: other

dat$EF01 <- dat_n$EF01

# match open answers to categories
dat$EF01_08 <- str_trim(dat$EF01_08)
#dat$EF01[dat$EF01_08 == 'first three years Scholarship (not employed), now temporary part time employment (50%)'] <- ''
dat$EF01[dat$EF01_08 == "Grant based stipend"] <- 6
#dat$EF01[dat$EF01_08 == "travelling fees"] <- ''
#dat$EF01[dat$EF01_08 == "cooperation with industry"] <- ''
#dat$EF01[dat$EF01_08 == "bis vor kurzem mit Vollstipendium, jetzt Hiwi-Vertrag"] <- ''
#dat$EF01[dat$EF01_08 == "lecture contract"] <- ''
dat$EF01[dat$EF01_08 == "working half time out of university"] <- 5
#dat$EF01[dat$EF01_08 == "external Ph.D.student"] <- 5
dat$EF01[dat$EF01_08 == "ALG1" ] <- 5
#dat$EF01[dat$EF01_08 == "12 months scholarship for the whole PhD duration"  ] <- ''
dat$EF01[dat$EF01_08 == "mini-job" ] <- 5
dat$EF01[dat$EF01_08 == "previously with a DFG grant (3 y) and short contract through Senckenberg"  ] <- 2
dat$EF01[dat$EF01_08 ==  "Private money"   ] <- 5
#dat$EF01[dat$EF01_08 == "temporary employment, now unemployed"  ] <- 5
dat$EF01[dat$EF01_08 == "50 % Temporary employment"  ] <- 2
dat$EF01[dat$EF01_08 == "DFG Grant, I dont know how it fits on the ones above"  ] <- 2
dat$EF01[dat$EF01_08 == "self-founded"   ] <- 5
dat$EF01[dat$EF01_08 ==  "Freelance employment" ] <- 5
dat$EF01[dat$EF01_08 == 'family (husband) financial support'] <- 5

table(dat$EF01, useNA = "always")

dat_new$EF01 <- dat$EF01

###############################################################
# [EF02] Length of Contract "If you only have a temporary employment: What is the total length of your contract (in months)?"
      ### EF02_01 ... months
dat$EF02_01 <- str_trim(dat$EF02_01)
dat$EF02_01 <- recode(dat$EF02_01, "'36 months' = '36'; 
                      '24 (posdoc/Marie Curie)' = '24';
                      '28 Months' = '28';
                      '36 + 12 +5' = '53';
                      '36 + 15' = '51';
                      '36+12'='48';
                      '1-3' = '2';
                      '0' = '' ")

dat_new$EF02_01 <- as.numeric(dat$EF02_01)
table(dat_new$EF02_01, useNA = "always")

###############################################################
# [EF06] Length of scholarship "If you have a scholarship: What is the total length of your current scholarship (in months)?"
     ### EF06_01 ... months
dat$EF06_01 <- str_trim(dat$EF06_01)
dat$EF06_01 <- recode(dat$EF06_01, "'36 months' = '36'; 
                      '36-48' = '42';
                      '23 months' = '23';
                      '36 (DFG) previously' = '36';
                      '0' = '' ")
dat_new$EF06_01 <- as.numeric(dat$EF06_01)

###############################################################
# [EF03] % Full time "What is the percentage your contract covers in terms of full-time employment?"
      ### EF03_01 ... %
dat$EF03_01 <- str_trim(dat$EF03_01)
dat$EF03_01 <- recode(dat$EF03_01, "'< 50' = '50'; 
                      'DFG was 60%' = '60';
                      '65 %, 75% since 2021' = '70';
                      '87,5' = '87.5';
                      '60 and 50' = '55';
                      '57,5' = '57.5';
                      '0' = '-';")
dat_new$EF03_01 <- as.numeric(dat$EF03_01)

table(dat_new$EF03_01, useNA = "always")

###############################################################
# [EF04] Other employment "Do you have any other paid employment which is unrelated to your Ph.D.?"
      ### EF04 Other employment
      ### 1 = no  2 = yes 
# recode to 0 = no, 1 = yes, NA = Now answered 

dat_new$EF04 <- recode(dat_n$EF04, "'1'=0; '2'=1; ")

table(dat_new$EF04, useNA = "always")

###############################################################
# [EF05] Reason other employment "Why did you decide to have a side-job?"
      ### EF05 shows how often 1,2,3 etc. options where ticked.
      ### EF05_01 Earning money
      ### EF05_02 For the CV
      ### EF05_03 Personal interest
      ### EF05_04 Other, please specify
      ### 1 = Not checked
      ### 2 = Checked
      ### EF05_04a Other, please specify (free text)
# TODO #6
# see open question procedure

###############################################################
### Section EV: Evaluation of your Ph.D.
###############################################################
   ### scale in EV01, EV04, EV06, EV07, EV08
      # (EV04 was shifted by one. now fixed.) 
      ### 1 = strongly disagree
      ### 2 = disagree
      ### 3 = neither agree nor disagree
      ### 4 = agree
      ### 5 = strongly agree
      ### -1 = not sure
      ### NA = Not answered

# [EV01] PhD again "Looking back, if I had not started my Ph.D. yet, I would do it again."
# [EV04] Regret "I regret having started a Ph.D."

### Please evaluate in the scale the following statements regarding your Ph.D.:

# [EV06] Job-satisfaction1 "I enjoy being at work." 
# [EV07] Job-satisfaction2 "I am content with the job I have."
# [EV08] Job-satisfaction3 "I am satisfied with my job."

dat_new <- cbind(dat_new, dat_n[c('EV01', 'EV06', 'EV07','EV08')])
dat_new$EV04 <- dat_n$EV04-1 



# [EV09] Life-satisfaction "How satisfied are you with your life at the moment?"
    ### 1 = not satisfied at all
    ### 2 = slightly satisfied
    ### 3 = moderately satisfied
    ### 4 = very satisfied
    ### 5 = totally satisfied
    ### NA = Not answered^

dat_new$EV09 <- dat_n$EV09
table(dat_n$EV09)

# [EV05] PhD somewhere else "Do you think doing your Ph.D. in/at a different country/university would offer better conditions?"
    ### 1 = no; 2 = yes; 3 = not sure; NA = Not answered
# recode to
    ### 0 = no; 1 = yes; 2 = not sure; NA = Not answered
dat_new$EV05 <- recode(dat_n$EV05, "'1'=0; '2'=1;'3'=2 ")
table(dat$EV05)

###############################################################
### Section WG: Structure of working group
###############################################################

# [WG01] Professional support "Are there colleagues in your department/research center that support you professionally?"
    ### 1 = no;  2 = yes;  NA = Not answered
# recode to
    ### 0 = no; 1 = yes;  NA = Not answered
dat_new$WG01 <- recode(dat_n$WG01, "'1'=0; '2'=1; ")
table(dat$WG01, useNA = 'always')

# [WG02] Emotional support "Are there colleagues in your department/research center that support you emotionally?"
    ### 1 = no;  2 = yes;  NA = Not answered
# recode to
    ### 0 = no; 1 = yes; NA = Not answered
dat_new$WG02 <- recode(dat_n$WG02, "'1'=0; '2'=1; ")
table(dat$WG02, useNA = 'always')

# [WG03] Work alone "Do you mainly work alone on your project?"
    ### 1 = no;  2 = yes;  NA = Not answered
# recode to
    ### 0 = no; 1 = yes; NA = Not answered
# TODO #7 : this variable is missing in the data??????
# also missing in sosci survey!



###############################################################
### Section GH: General Health
###############################################################
  ### 1 = never
  ### 2 = almost never
  ### 3 = sometimes
  ### 4 = fairly often
  ### 5 = very often
  ### NA = Not answered

# [GH01] Stress1 "...how often have you felt that you were unable to control the important things in your life?"

# [GH02] Stress2 "...how often have you felt confident about your ability to handle your personal problems?"

# [GH03] Stress3 "...how often have you felt that things were going your way?"

# [GH04] Stress4 "...how often have you felt difficulties were piling up so high that you could not overcome them?"

dat_new <- cbind(dat_new, dat_n[c('GH01', 'GH02', 'GH03','GH04')])

table(dat_new$GH04, useNA = "always")

###############################################################
### Section OR: Other responsibilities
###############################################################

# [OR01] Reponsibilities besides PhD "What other responsibilities do you have besides working on your research topic/project?" (MC)
    ### OR01_01 Teaching
    ### OR01_02 Supervision of other students
    ### OR01_03 Administration
    ### OR01_04 Research unrelated to your topic
    ### OR01_08 Other, please specify
    ### OR01_09 No further responsibilities
    ### 1 = Not checked
    ### 2 = Checked
    ### OR01_08a Other, please specify (free text)

# TODO #8
# see open question procedure
# leave them and add other columns for open question. potentially merge them. 

table(dat_n$OR01)

###############################################################

# [OR02] Time other responsibilities "How much time do you spend on other responsibilities besides your research activities?"
    ### OR02_01 ... %

dat$OR02_01 <- str_trim(dat$OR02_01)
dat$OR02_01 <- recode(dat$OR02_01, "'15-30' = '22.5'; 
                      '20-30' = '25';
                      '20-60' = '40';
                      '30-70' = '50';
                      '30%' = '30';
                      '5-10' = '7.5';
                      '60%' = '60';
                      '66,67' = '66.67';")
dat_new$OR02_01 <- as.numeric(dat$OR02_01)

###############################################################
### Section ST: Stressors
###############################################################
### Scale for [ST01] to [ST09]
    ### 1 = not at all
    ### 2 = rarely
    ### 3 = some of the time
    ### 4 = most of the time
    ### 5 = all of the time
    ### NA = Not answered
# [ST01] Institutional-stressor1 "Have you ever felt looked down on by your supervisor?"
# [ST02] Institutional-stressor2 "Do you feel supported by your supervisor?"
# [ST03] Institutional-stressor3 "Do you feel your supervisor is interested in your topic?"
# [ST04] Institutional-stressor4 "Have you ever felt mistreated (e.g., insulted, shouted at, undervalued) by your supervisor?"
# [ST17] Institutional-stressor4_coll "Have you ever felt mistreated (e.g., insulted, shouted at, undervalued) by your colleagues?"
# [ST05] Institutional-stressor5 "Does your supervisor encourage you to participate in career-building opportunities such as conferences, summ..."
# [ST06] Institutional-stressor6 "Based on your experiences in academia so far, do you reconsider your career path?"
# [ST07] Institutional-stressor7 "Do you feel comfortable contacting your supervisor when you need help?"
# [ST08] Institutional-stressor8 "Has the behavior of your supervisor ever made you consider quitting your Ph.D.?"
# [ST09] Institutional-stressor9 "Do you have regular meetings with your supervisor(s)?"

dat_new <- cbind(dat_new, dat_n[c('ST01', 'ST02', 'ST03','ST04','ST05','ST06','ST07','ST08','ST09', 'ST17')])
table(dat$ST17, useNA = "always")

# [ST16] Institutional-stressor10 "How often do you have meetings with your supervisor(s)?"
    ### 1 = at least once a week
    ### 2 = at least once a month
    ### 3 = at least every three months
    ### 4 = at least every six months
    ### 5 = at least once a year
    ### 6 = less than once a year
    ### NA = Not answered
dat_new$ST16 <- dat_n$ST16
table(dat_new$ST16, useNA = 'always')

### Scale for [ST11] to [ST15]
    ### 1 = strongly disagree
    ### 2 = disagree
    ### 3 = neither agree nor disagree
    ### 4 = agree
    ### 5 = strongly agree
    ### NA = Not answered

# [ST11] Systemic-stressor1 "The lack of permanent/long-term contracts in academi..."

# [ST12] Systemic-stressor2 I believe having a Ph.D. will help me to find a good job.""

# [ST13] Job-insecurity1 "Please evaluate in the scale the following statements regarding your Ph.D.: "I am worried about having to le..."

# [ST14] Job-insecurity2 "There is a risk that I will have to leave my present job in the year to come."

# [ST15] Job-insecurity3 "I feel uneasy about losing my job in the near future."

dat_new <- cbind(dat_new, dat_n[c('ST11', 'ST12', 'ST13','ST14','ST15')])
table(dat_n$ST15, useNA = "always")


###############################################################
### Section MH: Mental Health
###############################################################

# [MH01] Mental health 1 "How often do you feel stressed since you have started your Ph.D.?"
      ### 1 = not at all
      ### 2 = rarely
      ### 3 = some of the time
      ### 4 = most of the time
      ### 5 = all of the time
      ### NA = Not answered
dat_new$MH01 <- dat_n$MH01
table(dat_new$MH01, useNA = 'always')

# [MH02] Mental health 2 "Has your stress level increased since you started your Ph.D.?"
    ### 0 = No
    ### 1 = Yes
dat_new$MH02 <- recode(dat_n$MH02, "'1'=0; '2'=1; ")
table(dat_n$MH02, useNA = 'always')

# [MH03] Mental health 3 "What is/are the cause(s) of your stress?"
# [MH03_01]

# TODO #9: include open answer preprocessing


# [MH04] Mental health 4 "Do you have anyone at your institute to consult about your work-related stress?" (MC)
      ### MH04_01 Nobody
      ### MH04_02 Colleagues
      ### MH04_03 Admins / Coordination
      ### MH04_04 Supervisor
      ### MH04_05 Other, please specify
      ### 0 = Not checked
      ### 1 = Checked
      ### MH04_05a Other, please specify (free text)

dat_new$MH04_01 <- recode(dat_n$MH04_01, "'1'=0; '2'=1; ")
dat_new$MH04_02 <- recode(dat_n$MH04_02, "'1'=0; '2'=1; ")
dat_new$MH04_03 <- recode(dat_n$MH04_03, "'1'=0; '2'=1; ")
dat_new$MH04_04 <- recode(dat_n$MH04_04, "'1'=0; '2'=1; ")
dat_new$MH04_05 <- recode(dat_n$MH04_05, "'1'=0; '2'=1; ")

# MH04_05a Other, please specify (free text)
# TODO #10: open answer preprocessing


# [MH05] Mental health 5 "Do you feel your mental health has declined due to the Ph.D.?"
      ### 1 = not at all
      ### 2 = somehow
      ### 3 = plenty
      ### 4 = severely
      ### NA = Not answered

dat_new$MH05 <- dat_n$MH05
table(dat$MH05, useNA = 'always')


# [MH06] Mental health 6 "What would need to change to improve your mental health status?" 
      ### MH06_01 Free Text

# TODO #11: include open answer preprocessing

# [MH07] Mental health 7 "Do you know other Ph.D. students who are struggling with their mental health?"
      ### 1 = nobody
      ### 2 = some
      ### 3 = many
      ### 4 = most
      ### 5 = all
      ### NA = Not answered

dat_new$MH07 <- dat_n$MH07
table(dat_new$MH07, useNA = 'always')


# [MH08] Mental health 8 "Do you think your mental health problems negatively affect your quality of work?" 
      ### 1 = never
      ### 2 = almost never
      ### 3 = sometimes
      ### 4 = fairly often
      ### 5 = very often
      ### NA = Not answered

dat_new$MH08 <- dat_n$MH08
table(dat$MH08, useNA = 'always')

# [MH09] Mental health 9 "Do you think that some special services (e.g., time management courses, coaching, etc.) or structural change..." (MC)
      ### MH09_01 no
      ### MH09_02 personalised coaching
      ### MH09_03 mentoring
      ### MH09_04 time management courses
      ### MH09_05 stress management courses
      ### MH09_08 structural changes
      ### MH09_09 Other, please specify
      ### 0 = Not checked; 1 = Checked
      ### MH09_09a Other, please specify (free text)

dat_new$MH09_01 <- recode(dat_n$MH09_01, "'1'=0; '2'=1; ")
dat_new$MH09_02 <- recode(dat_n$MH09_02, "'1'=0; '2'=1; ")
dat_new$MH09_03 <- recode(dat_n$MH09_03, "'1'=0; '2'=1; ")
dat_new$MH09_04 <- recode(dat_n$MH09_04, "'1'=0; '2'=1; ")
dat_new$MH09_05 <- recode(dat_n$MH09_05, "'1'=0; '2'=1; ")
dat_new$MH09_08 <- recode(dat_n$MH09_08, "'1'=0; '2'=1; ")
dat_new$MH09_09 <- recode(dat_n$MH09_09, "'1'=0; '2'=1; ")

### MH09_09a Other, please specify (free text)
# TODO #12: open answer


# [MH10] Psychotherapy "Are you currently in psychotherapy?"
      ### 0 = no; 1 = yes; NA = Not answered
dat_new$MH10 <- recode(dat_n$MH10, "'1'=0; '2'=1; ")
table(dat_new$MH10, useNA = 'always')


# [MH11] Diagnosis "Have you ever been diagnosed with a mental disorder?"
      ### 0 = no; 1 = yes; NA = Not answered
dat_new$MH11 <- recode(dat_n$MH11, "'1'=0; '2'=1; ")
table(dat$MH11, useNA = 'always')


############### PHQ
# [MH12] PHQ_2_1 "Little interest or pleasure in doing things"
      # same scale for  'MH12', 'MH14','MH15','MH16','MH17','MH18','MH19','MH20','MH21'
      ### 1 = Not at all
      ### 2 = Several days
      ### 3 = More than half the days
      ### 4 = Nearly every day
      ### NA = Not answered

# [MH15] PHQ_9_Filter "Feeling nervous, anxious, or on edge?"
    # scale for  [MH15] 
    ### 1 = Not at all
    ### 2 = Several days
    ### 3 = More than half the days
    ### NA = Not answered

# [MH16] PHQ_9_1 "Being so restless that it's hard to sit still"

# [MH17] PHQ_9_2 "Being tired easily"

# [MH18] PHQ_9_3 "Muscle tension or muscle pain"

# [MH19] PHQ_9_4 "Trouble falling asleep or sleeping through"

# [MH20] PHQ_9_5 "Trouble concentrating, e.g., on reading or watching TV"

# [MH21] PHQ_9_6 "Becoming easily annoyed or irritable"

# [MH14] PHQ_2_2 "Feeling down, depressed or hopeless"

dat_new <- cbind(dat_new, dat_n[c('MH12', 'MH14','MH15','MH16','MH17','MH18','MH19',
                                  'MH20','MH21')])

table(dat_new$MH21, useNA = "always")

###############################################################
### Section SH: Seeking Help
###############################################################

# [SH01] Seeking-help1 "Have you already tried to improve your situation?" 
    ### 1 = never
    ### 2 = almost never
    ### 3 = sometimes
    ### 4 = fairly often
    ### 5 = very often
    ### NA = Not answered
dat_new$SH01 <- dat_n$SH01

table(dat_new$SH01,useNA = 'always')

# [SH02] Seeking-help2 "Have you heard of the consultation services at the university?"
    ### 1 = no; 2 = yes; NA = Not answered
dat_new$SH02 <- recode(dat_n$SH02, "'1'=0; '2'=1; ")
table(dat_new$SH02, useNA = 'always')

# [SH04] Seeking-help3 "How much do you know about the consultation services at the university?"
    ### 1 = nothing
    ### 2 = almost nothing
    ### 3 = something
    ### 4 = plenty
    ### 5 = a lot
    ### NA = Not answered 
dat_new$SH04 <- dat_n$SH04
table(dat_new$SH04, useNA = 'always')


# [SH05] Seeking-help4 "Have you sought help from a specialist?"
    ### 1 = no; 2 = yes; NA = Not answered
dat_new$SH05 <- recode(dat_n$SH05, "'1'=0; '2'=1; ")
table(dat$SH05, useNA = 'always')

# [SH10] Corona "Did the Covid-19 pandemic affect your general situation?"
    ### 1 = yes, it improved my general situation
    ### 2 = yes, it worsened my general situation
    ### 3 = yes, but it neither worsened nor improved my general situation
    ### 4 = no
    ### NA = Not answered
dat_new$SH10 <- dat_n$SH10
table(dat_n$SH10, useNA = 'always')

# [SH06] Seeking-help5 "Do you have anyone to talk to about your issues?"
    ### 1 = no; 2 = yes; NA = Not answered
dat_new$SH06 <- recode(dat_n$SH06, "'1'=0; '2'=1; ")
table(dat_new$SH06, useNA = 'always')

# [SH07] Seeking-help6 "What could be done to improve your situation? Feel free to express your opinion and feelings here."
    ### SH07_01 [01] Free text
# todo #13 : open question

# [SH09]Seeking-help7 "Do you have any further comments?"
    ### SH09_01 [01] Free text
# todo #14 : open question

# [SH11] Corona2 "Do you think the answers you have provided in this survey have been affected by the Covid-19 pandemic?"
    ### 1 = Very likely
    ### 2 = Likely
    ### 3 = Neutral
    ### 4 = Not likely
    ### 5 = Very unlikely
    ### NA = Not answered
dat_new$SH11 <- dat_n$SH11
table(dat$SH11, useNA = 'always')


###############################################################
### Section CO: Informed consent
###############################################################

# [CO05] Consent: Residual option (negative) or number of selected options
### CO05_01 I have been informed about the study and the procedure. I hereby agree to the collection
### and processing of my above-mentioned data. Consequently, I would like to voluntarily participate in this study.
### recoded: 0 = Not checked; 1 = Checked
dat_new$CO05_01 <- recode(dat_n$CO05_01, "'1'=0; '2'=1; ")
table(dat_new$CO05_01, useNA = 'always')



# [CO07] ConfirmationPHD: Residual option (negative) or number of selected options
### CO07_01 I hereby confirm, that I am a Ph.D. student at the University of T?bingen.
### recoded: 1 = Not checked; 2 = Checked
dat_new$CO07_01 <- recode(dat_n$CO07_01, "'1'=0; '2'=1; ")
table(dat$CO07_01, useNA = 'always')

############################### END ###########################
### Export fully anonymous data set as SAV and CSV file    ###
write.csv(dat_new,"/home/cornelius/Documents/sustainability/mental_health/data_protected/preprocessed_data_v0.csv", row.names = FALSE)

rm(list = ls())
