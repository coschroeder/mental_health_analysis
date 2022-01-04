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
dat<-read.spss("/home/cornelius/Documents/sustainability/mental_health/data_protected/Mental_Health_Rawdata_v1.sav",
               use.value.labels=T, to.data.frame=T,use.missings=T)

### recode items with an open answer format
# Age 
dat$age <- as.numeric(dat$SD02)
describe(dat$age)
table(dat$age, deparse.level = 2, useNA = "always")

#move from another city
table(dat$SD11, deparse.level = 2, useNA = "always")

# Nationality                       table(dat$SD08)
# two levels: as integers
# 1. EU, non-EU
# 2. Germany, EU, Asia, Africa, South A., Nord A., Australia
# MaK

# faculty                           table(dat$AP01_09a)
# MaK
# sort open questions to faculties
# result: one variable 1-7
# table(dat$AP01_01, dat$AP01_02, deparse.level=2, useNA = "always")

# working hours PhD                 table(dat$AP03)
# one number 
# 60-70: mean, or lower boundary
# MoKo

# working hours Total               table(dat$AP05)
# Moko

# PhD-Startdate                     table(dat$AP04) -> recode
# 6 month categories, only year: group into second half ?
# 2nd variable: before/after Corona
# MaK

# Finance                           table(dat$EF01_08)
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




# filter: temporary contract length         table(dat$EF02, deparse.level=2, useNA = "always")
# CS check: 260 nans
dat$EF02_01 <- str_trim(dat$EF02_01)
dat$EF02_01 <- recode(dat$EF02_01, "'36 months' = '36'; 
                      '24 (posdoc/Marie Curie)' = '24';
                      '28 Months' = '28';
                      '36 + 12 +5' = '53';
                      '36 + 15' = '51';
                      '36+12'='48';
                      '1-3' = '2';
                      ")
dat$EF02_01 <- as.numeric(dat$EF02_01)

# filter: temporary scholarship length      table(dat$EF06)
# CS check: 577 nans
dat$EF06_01 <- str_trim(dat$EF06_01)
dat$EF06_01 <- recode(dat$EF06_01, "'36 months' = '36'; 
                      '36-48' = '42';
                      '23 months' = '23';
                      '36 (DFG) previously' = '36';
                      ")
dat$EF06_01 <- as.numeric(dat$EF06_01)


# contract percentage               table(dat$EF03)
# side job                          table(dat$EF05)
# other responsibilities            table(dat$OR01)
# time for other responsibilities   table(dat$OR02)
# causes stress                     table(dat$MH03)
# consulted someone about stress    table(dat$MH04)
# need to change to reduce stress   table(dat$MH06)
# changes are needed                table(dat$MH09)
# suggestions                       table(dat$SH07)








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

