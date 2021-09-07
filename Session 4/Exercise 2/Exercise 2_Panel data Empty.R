install.packages("tidyverse")
install.packages("plm")
install.packages("lmtest")
install.packages("car")
library(tidyverse)
library(plm)
library(lmtest)
library(car)


###Panel data exercise
#Import data
df <- read.csv("~/paneldata_clean.csv")

df <- df %>%
  mutate(Urban=as.factor(Urban),
         Ethnicity=as.factor(Ethnicity),
         MaritalStatus=as.factor(MaritalStatus),
         Education=as.factor(Education),
         Employment=as.factor(Employment))



#explore panel


#check variables relationships


#Pooled model


#Fixed Effects model
#LSDV


#Within Estimator


#Random effects model