install.packages("tidyverse")
install.packages("zoo")
library(tidyverse)
library(zoo)

###
### Evaluating Complex Interventions. Practical exercise 3.1. Dif-in-Dif ###
#Intervention in Midlands region, 1st of Jan 2020 to improve bed management => decrease number of DTOC days

DTOC_raw <- read.csv("~/projects/DSU course/DTOC.csv") #change according to your working directory
names(DTOC_raw)[1] <- "Region"
names(DTOC_raw)[3] <- "DTOC_days"
DTOC_raw$Region <- as.factor(DTOC_raw$Region)
DTOC_raw$Provider <- as.factor(DTOC_raw$Provider)
DTOC <- DTOC_raw[DTOC_raw$Month=="Jan-20" |DTOC_raw$Month=="Dec-19", ]


#descriptive statistics


 #get % for months

 #get counts or % for providers

#pre-treatment stats

#post-trearment mean 


####dif-in-dif - subtracting differences


# Control group (London) before treatment


# Treatment group (Midlands) before treatment


# Control group (London) after treatment


# Treatment group (Midlands) after treatment


#calculate the difference in treatment group


#calculate the difference in control group

#calculate the difference in difference


#dif-in-dif visually
# Calculate counterfactual outcome



# Combine data



#dif-in-dif - regression 

 #create dummy

 #run lm, time:treated is dif-in-dif estimator


##is it a good method?

##Placebo test if we have time
