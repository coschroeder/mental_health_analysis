#Load packages:
library(foreign)
library(psych)
library(car)
library(stringr)
library(ggplot2)
library(likert) 
library(here) # package to have relative paths, for ex.: file = here("data_analysis/plots/age.pdf")

# Load the data:
dat <- read.csv("/home/cornelius/Documents/sustainability/mental_health/data_protected/preprocessed_data_v0.csv")

# plot the age
pdf(file = here("data_analysis/plots/age.pdf"),  width = 4, height = 4)
hist(dat$age)
dev.off()

# make some more analysis and save plots....

