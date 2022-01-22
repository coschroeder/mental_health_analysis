#######################################################
# R script Mental Health                              #
# sustainAbility PhD Initivative                      #
# R version 3.1.3 (2015-03-09)                        #
#######################################################


### activate packages 
library(foreign)
library(psych)
library(car)
library(stringr)


### read data set    
###dat<-read.spss("/home/cornelius/Documents/sustainability/mental_health/data_protected/Mental_Health_Rawdata_v1.sav",               use.value.labels=T, to.data.frame=T,use.missings=T)

dat<-read.spss("C:/Daten/Mental/Mental_Health_Rawdata_v1.sav",use.value.labels=T, to.data.frame=T,use.missings=T)

### filter out test run
# everywhere 1 was entered, especially at the age SD02_01
mask <-  ( (!is.na(dat$SD02_01==1) & dat$SD02_01==1) )
dat <- dat[!mask,]

### Filter for finished data
# (p. 35: 'do you have further comments?')
mask = dat$LASTPAGE == 35 | dat$LASTPAGE == 34 
sprintf("%s out of %s  finished the survey",sum(mask),length(dat[,1]))    
dat <- dat[mask,]

########################
### Data preparation ###
########################

#################################################
### Age 
dat$age <- as.numeric(dat$SD02)
table(dat$age, deparse.level = 2, useNA = "always")
# check: values 1 and 3 are invalid -> recode
dat$age <- ifelse(dat$age ==1 | dat$age ==3, NA,dat$age)
table(dat$age, deparse.level = 2, useNA = "always")
hist(dat$age)
describe(dat$age)

#################################################
### Nationality                       
table(dat$SD08, deparse.level = 2, useNA = "always")

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
'Cuba'=	2;
'Cyprus'=	1;
'Czech Republic'=	1;
'Benin'=	5;
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
'Lesotho'=	5;
'Luxembourg'=	1;
'México'=	3;
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

# assign value labels
dat$continent <- factor(dat$continent,
                    levels = c(1,2,3,4,5,6),
                    labels = c("Europe","North America","South America","Asia","Africa","Australia"))
table(dat$continent, useNA ="always")

#Create new variable with 1=Europe 0=Non-Europe
dat$europe <- recode(dat$continent, "1=1; 2=0; 3=0; 4=0; 5=0; 6=0;")
# check 
table(dat$europe, useNA ="always")

#Create new variable with 1=German, 0=Non-German, NA remain NA
dat$german <- ifelse(dat$SD08 == 'Germany', 1,0)
table(dat$german, useNA = "always")

#################################################
### Faculty 
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

##############################################################                 
### PhD-Startdate         
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
describe(dat$phd_month)
hist(dat$phd_month)

#Create new variable: Time spent as a phD 1 = 0-6months of PhD; 2= 7-12months; 3 = 13-18months; etc.
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
dat$phdstage <- ifelse(is.na(dat$phdstage == T) & dat$phd_month <=66, 12, dat$phdstage)
dat$phdstage <- ifelse(is.na(dat$phdstage == T) & dat$phd_month <=72, 13, dat$phdstage)
dat$phdstage <- ifelse(is.na(dat$phdstage == T) & dat$phd_month <=78, 14, dat$phdstage)
dat$phdstage <- ifelse(is.na(dat$phdstage == T) & dat$phd_month >=79, 15, dat$phdstage)
table(dat$phdstage, useNA = "always")
hist(dat$phdstage)

# 2nd variable: before/after Corona
# MaK
                    

# AP03 working hours PhD                 table(dat$AP03)
# one number 
# 60-70: mean, or lower boundary
# MoKo
dat$AP03_01 <- str_trim(dat$AP03_01)
dat$AP03_01 <- recode(dat$AP03_01, "'26-30' = '28';
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
dat$AP03_01 <- as.numeric(dat$AP03_01)
describe(dat$AP03_01)

# AP05 working hours Total               table(dat$AP05)
# Moko
dat$AP05_01 <- str_trim(dat$AP05_01)
dat$AP05_01 <- recode(dat$AP05_01, "'26-30' = '28';
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
dat$AP05_01 <- as.numeric(dat$AP05_01)
describe(dat$AP05_01)


# EF01_08 Finance                           table(dat$EF01_08)
# dat$EF01_08[dat$EF01_08!=""]
# CS
# Q: what to do if multiple matches? currently: other
dat$EF01_08 <- str_trim(dat$EF01_08)
#dat$EF01[dat$EF01_08 == 'first three years Scholarship (not employed), now temporary part time employment (50%)'] <- ''
dat$EF01[dat$EF01_08 == "Grant based stipend"] <- 'Scholarship (employed, paying social security)'
#dat$EF01[dat$EF01_08 == "travelling fees"] <- ''
#dat$EF01[dat$EF01_08 == "cooperation with industry"] <- ''
#dat$EF01[dat$EF01_08 == "bis vor kurzem mit Vollstipendium, jetzt Hiwi-Vertrag"] <- ''
#dat$EF01[dat$EF01_08 == "lecture contract"] <- ''
dat$EF01[dat$EF01_08 == "working half time out of university"] <- 'Not employed'
#dat$EF01[dat$EF01_08 == "external Ph.D.student"] <- 'Not employed'
dat$EF01[dat$EF01_08 == "ALG1" ] <- 'Not employed'
#dat$EF01[dat$EF01_08 == "12 months scholarship for the whole PhD duration"  ] <- ''
dat$EF01[dat$EF01_08 == "mini-job" ] <- 'Not employed'
dat$EF01[dat$EF01_08 == "previously with a DFG grant (3 y) and short contract through Senckenberg"  ] <- 'Temporary employment'
dat$EF01[dat$EF01_08 ==  "Private money"   ] <- 'Not employed'
#dat$EF01[dat$EF01_08 == "temporary employment, now unemployed"  ] <- 'Not employed'
dat$EF01[dat$EF01_08 == "50 % Temporary employment"  ] <- 'Temporary employment'
dat$EF01[dat$EF01_08 == "DFG Grant, I dont know how it fits on the ones above"  ] <- 'Temporary employment'
dat$EF01[dat$EF01_08 == "self-founded"   ] <- 'Not employed'
dat$EF01[dat$EF01_08 ==  "Freelance employment" ] <- 'Not employed'
dat$EF01[dat$EF01_08 == 'family (husband) financial support'] <- 'Not employed'


# EF02 filter: temporary contract length         table(dat$EF02, deparse.level=2, useNA = "always")
# CS check: 262 nans
dat$EF02_01 <- str_trim(dat$EF02_01)
dat$EF02_01 <- recode(dat$EF02_01, "'36 months' = '36'; 
                      '24 (posdoc/Marie Curie)' = '24';
                      '28 Months' = '28';
                      '36 + 12 +5' = '53';
                      '36 + 15' = '51';
                      '36+12'='48';
                      '1-3' = '2';
                      '0' = '' ")
dat$EF02_01 <- as.numeric(dat$EF02_01)

# EF06 filter: temporary scholarship length      table(dat$EF06)
# CS check: 581 nans
dat$EF06_01 <- str_trim(dat$EF06_01)
dat$EF06_01 <- recode(dat$EF06_01, "'36 months' = '36'; 
                      '36-48' = '42';
                      '23 months' = '23';
                      '36 (DFG) previously' = '36';
                      '0' = '' ")
dat$EF06_01 <- as.numeric(dat$EF06_01)

# EF03 contract percentage               table(dat$EF03)
# CS: check: 192 nans
# before cleaning:   sum(dat$EF03_01=='') + sum(dat$EF03_01=='-') + sum(dat$EF03_01=='?')++ sum(dat$EF03_01=='0') = 192
dat$EF03_01 <- str_trim(dat$EF03_01)
dat$EF03_01 <- recode(dat$EF03_01, "'< 50' = '50'; 
                      'DFG was 60%' = '60';
                      '65 %, 75% since 2021' = '70';
                      '87,5' = '87.5';
                      '60 and 50' = '55';
                      '57,5' = '57.5';
                      '0' = '-';")
dat$EF03_01 <- as.numeric(dat$EF03_01)

# EF05 side job                          table(dat$EF05)
# CS: EF05 shows how often 1,2,3 etc. options where ticked.
# EF05_04a open question. not sure how to proceede.  table(dat$EF05_04a)

# OR01 other responsibilities            table(dat$OR01)

# OR02 time for other responsibilities   table(dat$OR02)
# CS: check 205 nans
dat$OR02_01 <- str_trim(dat$OR02_01)
dat$OR02_01 <- recode(dat$OR02_01, "'15-30' = '22.5'; 
                      '20-30' = '25';
                      '20-60' = '40';
                      '30-70' = '50';
                      '30%' = '30';
                      '5-10' = '7.5';
                      '60%' = '60';
                      '66,67' = '66.67';")
dat$OR02_01 <- as.numeric(dat$OR02_01)

# MH03 causes stress                     table(dat$MH03)
# MH04 consulted someone about stress    table(dat$MH04)
# MH06 need to change to reduce stress   table(dat$MH06)
# MH09 changes are needed                table(dat$MH09)
# SH07 suggestions                       table(dat$SH07)



###############################################################
# potentially useful code snippets:


#dat_sub <- data.frame(dat$v1,dat$v2,dat$v3)
#psych::alpha(T_dis_sub[,1:3]) # std. alpha = 0.85
#dat$v1_s <- rowMeans(dat_sub[,1:3],na.rm=T) 
#dat$v1 <- dat_sub$var1_s
#rm(dat_sub)
#describe(dat$v1_s)

#names(dat.tfb)[names(dat.tfb)=="bftemp1"] <-"T_temp"

# recode item bfent1
#table(dat.tfb$bfent1, useNA = "always")
#dat.tfb$bfent1re<- recode(dat.tfb$bfent1, "1=4; 2=3; 3=2; 4=1")
# check
#table(dat.tfb$bfent1re, useNA = "always")

#describeBy(dat.tfb$T_ent, dat.tfb$bfc)

#tfb_sub <- c("CLASSID", "bfc", "bftid")
#tfb <- dat.tfb[tfb_sub]

#T_teach <- subset(tfb, bfc==1) 

# table(dat.mo$s1ai4,dat.mo$group, useNA = "always", deparse.level = 2)

### descriptives for each treatment group (Pre-Test) and export to excel
#tabi_pre <- as.data.frame(describe(datipre,na.rm=T)[1:44,1:9])
#round(tabi_pre,2)

##############################################################################

#### helper functions

get_n_rotten_entries <- function(data) {
  # returns the number of ('', '-', '?', '0') entries
  s <- sum(data=='') + sum(data=='-') + sum(data=='?')+ sum(data=='0')
  return(s)
}
