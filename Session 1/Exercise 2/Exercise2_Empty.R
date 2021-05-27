library(ggplot2)
library(fastDummies)
library(dplyr)
library(car)
library(lmtest)
library(ModelMetrics)
library(margins)


### Evaluating Complex Interventions. Practical exercise 1.2.Generalised Linear Model ###
# Using the same dataset, we predict death

### LOS data: load and wrangle a bit


##Research question 1: what drives risk of death?
# The columns we have are:
# LOS  length of stay 
# Age
# Male  (0=female, 1=male)
# IMD_Decile (index of multiple deprivation, from 1 (most deprived) to 10 (least deprived))
# Ethnic Group
# MLTC (multiple long term conditions)
# BMI (body-mass index)
# Death (0 if discharged alive)

####### Step 1. Descriptive statistics and relationship between variables  ##########



####### Step 2. Build GLM and assess the quality  ##########
##Start from the univariable regression similarly to the linear example


## Adding more variables to the model


## How to choose a better model?

# AIC:




## AUC using the ModelMetrics


####### Step 3. Predict  ##########


