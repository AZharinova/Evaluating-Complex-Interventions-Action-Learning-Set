library(ggplot2)
library(fastDummies)
library(dplyr)
library(car)

###
### Evaluating Complex Interventions. Practical exercise 1.1. Multilinear modelling ###

### LOS data: load and wrangle a bit



##Research question 1: what drives length of stay?
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


 #correlation matrix

 #check missing values  


#explore dependent variable


#explore dependent VS some independent variable - scatterplot

#try different variables

####### Step 2. Linear regression model  ##########

## univariable regression



#adding more variables

# Demographics only: Age, Gender, Ethnicity


 #restricted VS unrestricted


# Add socio-economic factors


# Try to add everything else


#any risk of multicollinearity?


## Any interaction terms?
#Interaction between two categorical variables


#Interaction between categorical variable and continous variable


#non-linear relationship for age variable?


#final model specification


####### Step 3. Assess the model and predict  ##########


#not nicely represented on a screen. Can try to fit all 4 plots on 1 chart



