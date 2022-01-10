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


### recode items with an open answer format
# Age 
dat$age <- as.numeric(dat$SD02)
describe(dat$age)
table(dat$age, deparse.level = 2, useNA = "always")

#move from another city
table(dat$SD11, deparse.level = 2, useNA = "always")

# SD08 Nationality                       table(dat$SD08)
# two levels: as integers
# 1. EU, non-EU
# 2. Germany, EU, Asia, Africa, South A., Nord A., Australia
# MaK

# AP01_09a faculty                           table(dat$AP01_09a)
# MaK
# sort open questions to faculties
# result: one variable 1-7
# table(dat$AP01_01, dat$AP01_02, deparse.level=2, useNA = "always")

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

# AP04 PhD-Startdate                     table(dat$AP04) -> recode
# 6 month categories, only year: group into second half ?
# 2nd variable: before/after Corona
# MaK

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
