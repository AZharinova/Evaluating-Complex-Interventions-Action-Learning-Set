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
LOSdata <- read.csv("~/projects/DSU course/LOSdata.csv")
names(LOSdata) [1] <- "LOS"
LOSdata$EthnicGroup <- as.factor(LOSdata$EthnicGroup)

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
summary(LOSdata)

ggplot(LOSdata, aes(x=Age, y=Death)) + geom_point()

LOSdata %>% 
  group_by(Death) %>% 
    summarise(meanAge=mean(Age),
              meanLOS=mean(LOS),
              maxLOS=max(LOS),
              meanMLTC=mean(MLTC))

####### Step 2. Build GLM and assess the quality  ##########
##Start from the univariable regression similarly to the linear example
glm1 <- glm(Death ~ Age, data=LOSdata, family="binomial")
summary(glm1)
exp(coef(glm1)) #odd ratios
margins(glm1)

## Adding more variables to the model
glm2 <- glm(Death ~ Age + MLTC, data=LOSdata, family="binomial")
summary(glm2)

## How to choose a better model?
lrtest(glm1, glm2)
anova(glm1, glm2, test="Chisq")

# AIC:
cbind(AgeOnly=AIC(glm1), AgeandMLTC=AIC(glm2))


glm3 <- glm(Death ~ Age*MLTC, data=LOSdata, family="binomial")
summary(glm3)
lrtest(glm2, glm3)

glm4 <- glm(Death ~ Age + MLTC + IMD_Decile, data=LOSdata, family="binomial")
summary(glm4)

## AUC using the ModelMetrics
auc(glm2)

####### Step 3. Predict  ##########
LOSdata$DeathPred <- predict(glm2, LOSdata, type="response")
summary(LOSdata$DeathPred)


ggplot(LOSdata, aes(x=factor(Death), group=Death, y=DeathPred)) + geom_boxplot()
ggplot(LOSdata, aes(x=factor(Death), group=Death, y=DeathPred)) + geom_violin()



###extra test
## Adding more variables to the model
glm5 <- glm(Death ~ Age + MLTC, data=LOSdata, family="binomial")
summary(glm5)

LOSdata$AgeInt <- ifelse(LOSdata$Age<=30, "20-30", ifelse(LOSdata$Age<=40, "30-40", 
                                                          ifelse(LOSdata$Age<=50, "40-50",
                                                                 ifelse(LOSdata$Age<=60, "50-60",
                                                                        ifelse(LOSdata$Age<=70, "60-70",
                                                                               "70+")))))
glm6 <- glm(Death ~ AgeInt + MLTC, data=LOSdata, family="binomial")
summary(glm6)
