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

### Read data set 
# dat<-read.spss("C:/Daten/Mental/Mental_Health_Rawdata_v1.sav",use.value.labels=T, to.data.frame=T,use.missings=T)
dat<-read.spss("/home/cornelius/Documents/sustainability/mental_health/data_protected/Mental_Health_Rawdata_v1.sav",
               use.value.labels=T, to.data.frame=T,use.missings=T)

### suggestion: 
### drop rows if the first 3 variables are missings AND total time (time_sum) is<=180s
dat$drop_filter <- NA
dat$drop_filter <- ifelse(is.na(dat$SD01== T) & is.na(dat$SD02_01== T) & is.na(dat$SD08== T) & dat$TIME_SUM <= 180, 0,1)
table(dat$drop_filter, useNA = "always") # N = 64 will be dropped as they have not started the questionnaire
dat <- subset(dat, drop_filter==1)
# Drop test run with SD02_01 (age) == 1
mask <-  ((!is.na(dat$SD02_01==1) & dat$SD02_01==1))
dat <- dat[!mask,]



###############################################################
### Section SD: Sociodemografics
###############################################################

###############################################################
# [SD01] Gender "Which gender do you identify with?"
        ### 1 = diverse; 2 = female; 3 = male; 4 = rather not say; -9 = Not answered



###############################################################
# [SD02] Age "How old are you?" SD02_01 I am ... years old

dat$age <- as.numeric(dat$SD02)
table(dat$age, deparse.level = 2, useNA = "always")
# check: values 1 and 3 are invalid -> recode
dat$age <- ifelse(dat$age ==1 | dat$age ==3, NA,dat$age)
table(dat$age, deparse.level = 2, useNA = "always")
hist(dat$age)
describe(dat$age)


###############################################################
# [SD08] Nationality "What is your nationality?"
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

#Create new variable with 1=Europe 0=Non-Europe
dat$europe <- recode(dat$continent, "'Europe' = 'Europe'; 'North America'= 'Others'; 'South America'= 'Others';
                     'Asia'= 'Others'; 'Australia'= 'Others'; 'Africa' = 'Others'; 'Australia' = 'Others';")
# check 
table(dat$europe, useNA ="always")

#Create new variable with 1=German, 0=Non-German, NA remain NA
dat$german <- ifelse(dat$SD08 == 'Germany', 1,0)
table(dat$german, useNA = "always")


###############################################################
# [SD10] Family education background "What is your family's academic background?"
        ### 1 = academic (at least one parent/legal guardian has a university degree)
        ### 2 = non-academic (none of my parents/legal guardians has a university degree)
        ### -9 = Not answered
table(dat$SD10, useNA = "always")
dat$SD10_re <- recode(dat$SD10, "'1'=2; '2'=0;")
table(dat$SD10_re, useNA = "always")

###############################################################
# [SD11] Formale Bildung (einfach) "Did you move to T?bingen for your Ph.D.?"
        ### 1 = no
        ### 9 = yes, from another place in Germany
        ### 3 = yes, from another country
        ### -9 = Not answered


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
        ###  -9 = Not answered
        ### SD12_12 Other degree. Free text


###############################################################
# [SD13] Care-taking "Do you have a child/a person in your family you have to take care of?"
        ### 1 = no; 2 = yes; -9 = Not answered


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



###############################################################
### Section AP: About your PhD
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


# [AP02] Choice of Topic "Did you choose the topic of your Ph.D. yourself?"
        ### 1 = no 2 = yes -9 = Not answered


# [AP03] Hours per week Ph.D "How many hours a week do you spend on your Ph.D.?"
        ### AP03_01 ... hours/week



#[AP05] Hours per week work "How many hours a week do you work in total?"
      ### AP05_01 ... hours/week



# [AP04] Start Ph.D."When did you start your Ph.D.?"
      ### AP04_01 


###############################################################
### Section EF: Employment/Financial Situation
###############################################################

# [EF01] Contract Type "How are you financed? (Contract type only related to your Ph.D.)"
      ### 1 = Permanent employment
      ### 2 = Temporary employment
      ### 3 = Hourly employment
      ### 6 = Scholarship (employed, paying social security)
      ### 7 = Scholarship (not employed)
      ### 5 = Not employed
      ### 8 = Other, please specify:
      ###-9 = Not answered
      ### EF01_08 Other, please specify


# [EF02] Length of Contract "If you only have a temporary employment: What is the total length of your contract (in months)?"
      ### EF02_01 ... months


# [EF06] Length of scholarship "If you have a scholarship: What is the total length of your current scholarship (in months)?"
     ### EF06_01 ... months


# [EF03] % Full time "What is the percentage your contract covers in terms of full-time employment?"
      ### EF03_01 ... %

# [EF04] Other employment "Do you have any other paid employment which is unrelated to your Ph.D.?"
      ### EF04 Other employment
      ### 1 = no  2 = yes -9 = Not answered

# [EF05] Reason other employment "Why did you decide to have a side-job?"
      ### EF05_01 Earning money
      ### EF05_02 For the CV
      ### EF05_03 Personal interest
      ### EF05_04 Other, please specify
      ### 1 = Not checked
      ### 2 = Checked
      ### EF05_04a Other, please specify (free text)


###############################################################
### Section EV: Evaluation of your Ph.D.
###############################################################
   ### scale until EV08
      ### 1 = strongly disagree
      ### 2 = disagree
      ### 3 = neither agree nor disagree
      ### 4 = agree
      ### 5 = strongly agree
      ### -1 = not sure
      ### -9 = Not answered


# [EV01] PhD again "Looking back, if I had not started my Ph.D. yet, I would do it again."


# [EV04] Regret "I regret having started a Ph.D."

### Please evaluate in the scale the following statements regarding your Ph.D.:

# [EV06] Job-satisfaction1 "I enjoy being at work." 

# [EV07] Job-satisfaction2 "I am content with the job I have."

# [EV08] Job-satisfaction3 "I am satisfied with my job."

# [EV09] Life-satisfaction "How satisfied are you with your life at the moment?"

    ### 1 = not satisfied at all
    ### 2 = slightly satisfied
    ### 3 = moderately satisfied
    ### 4 = very satisfied
    ### 5 = totally satisfied
    ### -9 = Not answered

# [EV05] PhD somewhere else "Do you think doing your Ph.D. in/at a different country/university would offer better conditions?"
    ### 1 = no; 2 = yes; 3 = not sure; -9 = Not answered


###############################################################
### Section WG: Structure of working group
###############################################################

# [WG01] Professional support "Are there colleagues in your department/research center that support you professionally?"
    ### 1 = no;  2 = yes;  -9 = Not answered

# [WG02] Emotional support "Are there colleagues in your department/research center that support you emotionally?"
    ### 1 = no;  2 = yes;  -9 = Not answered

# [WG03] Work alone "Do you mainly work alone on your project?"
    ### 1 = no;  2 = yes;  -9 = Not answered


###############################################################
### Section GH: General Health
###############################################################
  ### 1 = never
  ### 2 = almost never
  ### 3 = sometimes
  ### 4 = fairly often
  ### 5 = very often
  ### -9 = Not answered


# [GH01] Stress1 "...how often have you felt that you were unable to control the important things in your life?"

# [GH02] Stress2 "...how often have you felt confident about your ability to handle your personal problems?"

# [GH03] Stress3 "...how often have you felt that things were going your way?"

# [GH04] Stress4 "...how often have you felt difficulties were piling up so high that you could not overcome them?"


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

# [OR02] Time other responsibilities "How much time do you spend on other responsibilities besides your research activities?"
    ### OR02_01 ... %


###############################################################
### Section ST: Stressors
###############################################################
### Scale for [ST01] to [ST09]
    ### 1 = not at all
    ### 2 = rarely
    ### 3 = some of the time
    ### 4 = most of the time
    ### 5 = all of the time
    ###-9 = Not answered

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

# [ST16] Institutional-stressor10 "How often do you have meetings with your supervisor(s)?"
    ### 1 = at least once a week
    ### 2 = at least once a month
    ### 3 = at least every three months
    ### 4 = at least every six months
    ### 5 = at least once a year
    ### 6 = less than once a year
    ###-9 = Not answered



### Scale for [ST11] to [ST15]
    ### 1 = strongly disagree
    ### 2 = disagree
    ### 3 = neither agree nor disagree
    ### 4 = agree
    ### 5 = strongly agree
    ### -9 = Not answered

# [ST11] Systemic-stressor1 "The lack of permanent/long-term contracts in academi..."

# [ST12] Systemic-stressor2 I believe having a Ph.D. will help me to find a good job.""

# [ST13] Job-insecurity1 "Please evaluate in the scale the following statements regarding your Ph.D.: "I am worried about having to le..."

# [ST14] Job-insecurity2 "There is a risk that I will have to leave my present job in the year to come."

# [ST15] Job-insecurity3 "I feel uneasy about losing my job in the near future."


###############################################################
### Section MH: Mental Health
###############################################################

# [MH01] Mental health 1 "How often do you feel stressed since you have started your Ph.D.?"
      ### 1 = not at all
      ### 2 = rarely
      ### 3 = some of the time
      ### 4 = most of the time
      ### 5 = all of the time
      ### -9 = Not answered


# [MH02] Mental health 2 "Has your stress level increased since you started your Ph.D.?"


# [MH03] Mental health 3 "What is/are the cause(s) of your stress?"
# [MH03_01]


# [MH04] Mental health 4 "Do you have anyone at your institute to consult about your work-related stress?" (MC)
      ### MH04_01 Nobody
      ### MH04_02 Colleagues
      ### MH04_03 Admins / Coordination
      ### MH04_04 Supervisor
      ### MH04_05 Other, please specify
      ### 1 = Not checked
      ### 2 = Checked
      ### MH04_05a Other, please specify (free text)

# [MH05] Mental health 5 "Do you feel your mental health has declined due to the Ph.D.?"
      ### 1 = not at all
      ### 2 = somehow
      ### 3 = plenty
      ### 4 = severely
      ### -9 = Not answered

# [MH06] Mental health 6 "What would need to change to improve your mental health status?" 
      ### MH06_01 Free Text

# [MH07] Mental health 7 "Do you know other Ph.D. students who are struggling with their mental health?"
      ### 1 = nobody
      ### 2 = some
      ### 3 = many
      ### 4 = most
      ### 5 = all
      ### -9 = Not answered

# [MH08] Mental health 8 "Do you think your mental health problems negatively affect your quality of work?" 
      ### 1 = never
      ### 2 = almost never
      ### 3 = sometimes
      ### 4 = fairly often
      ### 5 = very often
      ### -9 = Not answered


# [MH09] Mental health 9 "Do you think that some special services (e.g., time management courses, coaching, etc.) or structural change..." (MC)
      ### MH09_01 no
      ### MH09_02 personalised coaching
      ### MH09_03 mentoring
      ### MH09_04 time management courses
      ### MH09_05 stress management courses
      ### MH09_08 structural changes
      ### MH09_09 Other, please specify
      ### 1 = Not checked; 2 = Checked
      ### MH09_09a Other, please specify (free text)


# [MH10] Psychotherapy "Are you currently in psychotherapy?"
      ### 1 = no; 2 = yes; -9 = Not answered


# [MH11] Diagnosis "Have you ever been diagnosed with a mental disorder?"
      ### 1 = no; 2 = yes; -9 = Not answered


# [MH12] PHQ_2_1 "Little interest or pleasure in doing things"
      # same scale until 
      ### 1 = Not at all
      ### 2 = Several days
      ### 3 = More than half the days
      ### 4 = Nearly every day
      ### -9 = Not answered

# [MH15] PHQ_9_Filter "Feeling nervous, anxious, or on edge?"

# [MH16] PHQ_9_1 "Being so restless that it's hard to sit still"

# [MH17] PHQ_9_2 "Being tired easily"

# [MH18] PHQ_9_3 "Muscle tension or muscle pain"

# [MH20] PHQ_9_5 "Trouble concentrating, e.g., on reading or watching TV"

# [MH21] PHQ_9_6 "Becoming easily annoyed or irritable"

# [MH14] PHQ_2_2 "Feeling down, depressed or hopeless"


###############################################################
### Section SH: Seeking Help
###############################################################

# [SH01] Seeking-help1 "Have you already tried to improve your situation?" 
    ### 1 = never
    ### 2 = almost never
    ### 3 = sometimes
    ### 4 = fairly often
    ### 5 = very often
    ### -9 = Not answered

# [SH02] Seeking-help2 "Have you heard of the consultation services at the university?"
    ### 1 = no; 2 = yes; -9 = Not answered

# [SH04] Seeking-help3 "How much do you know about the consultation services at the university?"
    ### 1 = nothing
    ### 2 = almost nothing
    ### 3 = something
    ### 4 = plenty
    ### 5 = a lot
    ### -9 = Not answered 

# [SH05] Seeking-help4 "Have you sought help from a specialist?"
    ### 1 = no; 2 = yes; -9 = Not answered

# [SH10] Corona "Did the Covid-19 pandemic affect your general situation?"
    ### 1 = yes, it improved my general situation
    ### 2 = yes, it worsened my general situation
    ### 3 = yes, but it neither worsened nor improved my general situation
    ### 4 = no
    ### -9 = Not answered

# [SH06] Seeking-help5 "Do you have anyone to talk to about your issues?"
    ### 1 = no; 2 = yes; -9 = Not answered

# [SH07] Seeking-help6 "What could be done to improve your situation? Feel free to express your opinion and feelings here."
    ### SH07_01 [01] Free text

# [SH09]Seeking-help7 "Do you have any further comments?"
    ### SH09_01 [01] Free text

# [SH11] Corona2 "Do you think the answers you have provided in this survey have been affected by the Covid-19 pandemic?"
    ### 1 = Very likely
    ### 2 = Likely
    ### 3 = Neutral
    ### 4 = Not likely
    ### 5 = Very unlikely
    ### -9 = Not answered



###############################################################
### Section CO: Informed consent
###############################################################

# [CO05] Consent: Residual option (negative) or number of selected options
### CO05_01 I have been informed about the study and the procedure. I hereby agree to the collection
### and processing of my above-mentioned data. Consequently, I would like to voluntarily participate in this study.
### 1 = Not checked; 2 = Checked

# [CO07] ConfirmationPHD: Residual option (negative) or number of selected options
### CO07_01 I hereby confirm, that I am a Ph.D. student at the University of T?bingen.
### 1 = Not checked; 2 = Checked




############################### END ###########################
### Export fully anonymous data set as SAV and CSV file    ###



##############################################################
### Descriptive Summaries, Plots, Explorative Analyses
