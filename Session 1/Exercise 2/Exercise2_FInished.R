library(ggplot2)
library(fastDummies)
library(dplyr)
library(car)
library(lmtest)
library(ModelMetrics)
library(margins)

install.packages("ggplot2")
### Evaluating Complex Interventions. Practical exercise 1.2.Generalised Linear Model ###
# Using the same dataset, we predict death

### LOS data: load and wrangle a bit
LOSdata <- read.csv("~/projects/DSU course/LOSdata.csv")
names(LOSdata) [1] <- "LOS"
LOSdata$EthnicGroup <- as.factor(LOSdata$EthnicGroup)

####### Step 1. Descriptive statistics and relationship between variables  ##########
View(LOSdata)
summary(LOSdata) 

LOSdata %>% 
  group_by(Death) %>% 
  summarize(meanLOS = mean(LOS),
            minLOS= min(LOS),
            maxLOS= max(LOS),
            meanAge = mean(Age),
            minAge= min(Age),
            maxAge= max(Age),)

ggplot(LOSdata, aes(x=Age, y=Death)) + geom_point()


####### Step 2. Build GLM and assess the quality  ##########
##Start from the univariable regression similarly to the linear example

glm1<- glm(Death ~ Age, data=LOSdata, family="binomial")
summary(glm1) #need to take exp to understand coefficints
exp(coef(glm1))

margins(glm1)

## Adding more variables to the model

glm2<- glm(Death ~ Age + MLTC, data=LOSdata, family="binomial")
summary(glm2)
exp(coef(glm2))
margins(glm2)

## How to choose a better model?

# AIC:

cbind(glm=AIC(glm1), glm3=AIC(glm2)) #the smaller the better
anova(glm1, glm2, test="Chisq") # Likelihood ratio test
lrtest(glm1, glm2)


glm3 <- glm(Death ~ Age * MLTC, data = LOSdata, family="binomial")
summary(glm3)
lrtest(glm2, glm3)

## Is our model any good? Check the area under curve(AUC), using the ModelMetrics
glm2<- glm(Death ~ Age + MLTC + IMD_Decile + LOS, data=LOSdata, family="binomial")
auc(glm2)
margins(glm2)

####### Step 3. Predict  ##########
LOSdata$DeathPred <- predict(glm2, newdata=LOSdata, type="response")
summary(LOSdata$DeathPred)

ggplot(LOSdata, aes(y=Death, x=DeathPred))+
  geom_point()+
  geom_smooth(col="red")  ##not useful because of the values


ggplot(LOSdata, aes(x=factor(Death), group=Death, y=DeathPred))+
  geom_boxplot()+
  scale_x_discrete(name="", breaks=c(0,1), labels=c("Survived", "Died")) + ylim(0,1)

ggplot(LOSdata, aes(x=factor(Death), group=Death, y=DeathPred))+
  geom_violin()+
  scale_x_discrete(name="", breaks=c(0,1), labels=c("Survived", "Died")) #not good prediction, but higher probabilityies in the dead group



