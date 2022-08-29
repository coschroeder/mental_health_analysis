# Nina made thos for visualization only, these plots don't look nice. 

# Load packages:
library(foreign)
library(psych)
library(car)
library(stringr)
library(ggplot2)
library(likert) 
library(here) # package to have relative paths, for ex.: file = here("data_analysis/plots/age.pdf")

# plot the number of PhD working hours per week 
pdf(file = here("data_analysis/plots/working_hours.pdf"),  width = 4, height = 4)
hist(dat$AP03_01)
dev.off()

# plot the number of total working hours per week 
pdf(file = here("data_analysis/plots/working_hours_all.pdf"),  width = 4, height = 4)
hist(dat$AP05_01)
dev.off()

# plot the percentages of the contract
hist(dat$EF03_01)
dev.off()

# plot the length of the contract
hist(dat$EF02_01)
dev.off()

#MH12: little interest in doing things 
hist(dat$MH12)
dev.off()

# MH16: Being so restless that it's hard to sit still 
hist(dat$MH16)
dev.off()

# MH17: Being tired easily
hist(dat$MH17)
dev.off()

# MH18: Muscle tension or muscle pain
hist(dat$MH18)
dev.off()

# MH19: Trouble falling asleep or sleeping through 
hist(dat$MH19)
dev.off()

#MH20: Trouble on concentrating
hist(dat$MH20)
dev.off()

#MH21: becoming easily annoyed or irritable
hist(dat$MH21)
dev.off()

#MH14: Feeling down
hist(dat$MH14)
dev.off()

#MH15: Feeling nervous, anxious, on edge
hist(dat$MH15)
dev.off()