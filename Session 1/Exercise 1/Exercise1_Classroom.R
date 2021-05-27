install.packages("ggplot2") 
install.packages("fastDummies")
install.packages("dplyr")
install.packages("car")
install.packages("lmtest")
install.packages("ModelMetrics")
install.packages("margins")


library(ggplot2)
library(fastDummies)
library(dplyr)
library(car)

###
### Evaluating Complex Interventions. Practical exercise 1.1. Multilinear modelling ###

### LOS data: load and wrangle a bit
LOSdata <- read.csv("LOSdata.csv")
names(LOSdata) [1] <- "LOS"
LOSdata$EthnicGroup <- as.factor(LOSdata$EthnicGroup)

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
summary(LOSdata)
View(LOSdata)

cor(LOSdata) #correlation matrix
cor(LOSdata[-c(5)])

LOSdata1 <- dummy_cols(LOSdata$EthnicGroup) %>% 
                    cbind(LOSdata) %>% 
                      subset(select=-c(1,11))

cor(LOSdata1)

#check missing values  
is.na(LOSdata)
sum(is.na(LOSdata))

#explore dependent/outcome variable (LOS)
ggplot(LOSdata, aes(x=LOS)) + geom_histogram()
ggplot(LOSdata, aes(y=LOS)) + geom_boxplot()

#explore dependent VS some independent variable - scatterplot
ggplot(LOSdata, aes(x=Age, y=LOS)) + geom_point()
ggplot(LOSdata, aes(x=Age, y=LOS)) + geom_point() + geom_smooth(method="lm")

#try different variables
ggplot(LOSdata, aes(x=MLTC, y=LOS)) + geom_point()

####### Step 2. Linear regression model  ##########

## univariable regression 
lm1 <- lm(LOS ~ Age, LOSdata)
summary(lm1)
summary(lm1)$coefficients

#adding more variables
# Demographics only: Age, Gender, Ethnicity
lm2 <- lm(LOS ~ Age + Male + EthnicGroup, data=LOSdata)
summary(lm2)  
lm2 <- lm(LOS ~ Age + Male + relevel(EthnicGroup, ref=5), data=LOSdata)
summary(lm2) 
  
#restricted VS unrestricted
anova(lm1,lm2)

# Add socio-economic factors
lm3 <- lm(LOS~Age+relevel(EthnicGroup, ref=5)+IMD_Decile, data=LOSdata)
summary(lm3)

# Try to add everything else
lm4 <- lm(LOS~Age+relevel(EthnicGroup, ref=5)+ BMI + MLTC, data=LOSdata)
summary(lm4)

#any risk of multicollinearity?
vif(lm4)

## Any interaction terms?
#Interaction between two categorical variables
lm5 <- lm(LOS~Male*EthnicGroup, data=LOSdata)
summary(lm5)

#Interaction between categorical variable and continous variable
lm6 <- lm(LOS~Age*EthnicGroup, data=LOSdata)
summary(lm6)

#non-linear relationship for age variable?
ggplot(LOSdata, aes(x=Age, y=LOS)) + geom_point() + geom_smooth(method=lm, formula=y~poly(x,2))

#final model specification

lm7 <- lm(LOS~poly(Age,2, raw=TRUE)+Age*relevel(EthnicGroup, ref=5) + BMI + MLTC, data=LOSdata)
summary(lm7)


####### Step 3. Assess the model and predict  ##########
plot(lm7)

#not nicely represented on a screen. Can try to fit all 4 plots on 1 chart
par(mfrow=c(2,2))
plot(lm7)

LOSdata$pred1 <- predict(lm1, LOSdata)
LOSdata$pred <- predict(lm7, LOSdata)

summary(LOSdata$pred)
summary(LOSdata$LOS)
