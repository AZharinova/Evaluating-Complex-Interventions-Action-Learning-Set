library(ggplot2)
library(fastDummies)
library(dplyr)
library(car)

###
### Evaluating Complex Interventions. Practical exercise 1.1. Multilinear modelling ###

### LOS data: load and wrangle a bit

LOSdata <- read.csv("~/projects/DSU course/LOSdata.csv")
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

View(LOSdata)

summary(LOSdata) 

cor(LOSdata) #correlation matrix

LOSdata1<-dummy_cols(LOSdata$EthnicGroup) %>%
              cbind(LOSdata) %>%
                subset(select=-c(1,11))
cor(LOSdata1)
cor(LOSdata[-c(5)])

is.na(LOSdata)
sum(is.na(LOSdata)) #check missing values  
  
  
#explore dependent variable
ggplot(LOSdata, aes(x=LOS))+
  geom_histogram()

ggplot(LOSdata, aes(y=LOS))+
  geom_boxplot() 

#explore dependent VS some independent variable - scatterplot
ggplot(LOSdata, aes(x=Age, y=LOS)) + geom_point()
ggplot(LOSdata, aes(x=Age, y=LOS)) + geom_point() +  geom_smooth(method="lm", col="red")

ggplot(LOSdata, aes(x=MLTC, y=LOS)) + geom_point() #try different variables


####### Step 2. Linear regression model  ##########

## univariable regression

lm1 <- lm(LOS ~ Age, data = LOSdata)
summary(lm1)
summary(lm1)$coef

#adding more variables

# Demographics only: Age, Gender, Ethnicity
lm2 <- lm(LOS ~ Age + Male + EthnicGroup, data = LOSdata)
summary(lm2) #notice that R removed categorical variable automatically because it is factor variable. But if it was coded, things were not as easy

anova(lm1, lm2) #restricted VS unrestricted

lm2 <- lm(LOS ~ Age + Male + relevel(EthnicGroup, ref=5), data = LOSdata)
summary(lm2)

# Add socio-economic factors
lm3 <- lm(LOS ~ Age + EthnicGroup + IMD_Decile, data = LOSdata)
summary(lm3) 




#remove IMD 
lm4 <- lm(LOS ~ Age + EthnicGroup, data = LOSdata)
summary(lm4)

# Try to add everything else

lm5 <- lm(LOS ~ Age + EthnicGroup + MLTC + BMI, data = LOSdata)
summary(lm5)


#any risk of multicollinearity?
vif(lm5)

## Any interaction terms?
#Interaction between two categorical variables
lm6 <- lm(LOS ~ Male*EthnicGroup, data=LOSdata)
summary(lm6) #automatically adds default variables

#Interaction between categorical variable and continous variable
lm7 <- lm(LOS ~ Age*EthnicGroup, data=LOSdata)
summary(lm7)

#non-linear relationship for age variable?
ggplot(LOSdata, aes(x=Age, y=LOS)) + geom_point() +  geom_smooth(method="lm", formula=y ~ poly(x,2, raw=TRUE), col="red")
lm8 <- lm(LOS ~ poly(Age,2, raw=TRUE) + EthnicGroup + MLTC + BMI, data = LOSdata)
summary(lm8)

#final model specification
lm9 <- lm(LOS ~ poly(Age,2, raw=TRUE) + Age*EthnicGroup + MLTC + BMI, data=LOSdata)
summary(lm9)

####### Step 3. Assess the model and predict  ##########
plot(lm9)

#not nicely represented on a screen. Can try to fit all 4 plots on 1 chart
par(mfrow = c(2, 2))
plot(lm9)



LOSdata$pred <- predict(lm9, LOSdata)
summary(LOSdata$pred)
ggplot(LOSdata, aes(y=LOS, x=pred))+
  geom_point()+
  geom_smooth(col="red")
  

LOSdata$pred <- ifelse(pred<0,0, pred)
summary(LOSdata$pred)
summary(LOSdata$LOS)