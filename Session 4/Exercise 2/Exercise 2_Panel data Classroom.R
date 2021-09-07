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
summary(df)
str(df)

pdim(df, "Id")
df_balanced1 <- make.pbalanced(df, balance.type="fill")
df_balanced2 <- make.pbalanced(df, balance.type="shared.individuals")

pd <- pdata.frame(df_balanced2, index=c("Id", "Year"))
summary(pd)

#check variables relationships
cor(pd[c("Age", "Wellbeing", "MonthlyIncome", "Children", "HouseholdSize")])

plot(Age~Wellbeing, data=pd)
plot(Children~Wellbeing, data=pd)

plot(Ethnicity~Wellbeing, data=pd)
plot(Employment~Wellbeing, data=pd)
plot(Wellbeing~Ethnicity, data=pd)
plot(Wellbeing~Employment, data=pd)

#Pooled model
pm_pooled <- plm(Wellbeing ~ Urban+Age+Sex+Ethnicity+BornUK+HouseholdSize+MaritalStatus+
                   Children+Education+Employment+MonthlyIncome,
                   model="pooling", data=pd)
summary(pm_pooled)
pm_pooled$coefficients

#Fixed Effects model
#LSDV
pm_lsdv <- lm(Wellbeing ~ Urban+Age+Sex+Ethnicity+BornUK+HouseholdSize+MaritalStatus+
                   Children+Education+Employment+MonthlyIncome+factor(Id), data=pd)
summary(pm_lsdv)

#Within Estimator
pm_fixed <- plm(Wellbeing ~ Urban+Age+Sex+Ethnicity+BornUK+HouseholdSize+MaritalStatus+
                   Children+Education+Employment+MonthlyIncome,
                 model="within", data=pd)
summary(pm_fixed)

#Random effects model
pm_random <- plm(Wellbeing ~ Urban+Age+Sex+Ethnicity+BornUK+HouseholdSize+MaritalStatus+
                  Children+Education+Employment+MonthlyIncome,
                model="random", data=pd)
summary(pm_random)

#fixed vs pooled test
pFtest(pm_fixed, pm_pooled) #significant effects of the id/year => cannot use pooled data


#random vs fixed
phtest(pm_fixed, pm_random) #alternative hypothesis about inconsistency of RE model => fixed effects model

#random vs pooled
plmtest(pm_pooled, type=c("bp")) #random effects are better than pooled

#test which fixed effects you have (individual, time, so on)
plmtest(pm_fixed, effect=c("individual")) #there are indiv effects
plmtest(pm_fixed, effect=c("time"))  #there are no time effects effects

#adding intervention variable in - 2012 
pd$Intervention <- ifelse(pd$Year==2009 | pd$Year==2010 | pd$Year==2011, 0, 1) 

pm_fixed2 <- plm(Wellbeing ~ Urban+Age+Sex+Ethnicity+BornUK+HouseholdSize+MaritalStatus+
                  Children+Education+Employment+MonthlyIncome+Intervention,
                model="within", data=pd)
summary(pm_fixed2)

pd$Crisis <- ifelse(pd$Year==2009, 1, 0) #if intervention/event if one off

pm_fixed3 <- plm(Wellbeing ~ Urban+Age+Sex+Ethnicity+BornUK+HouseholdSize+MaritalStatus+
                   Children+Education+Employment+MonthlyIncome+Crisis,
                 model="within", data=pd)
summary(pm_fixed3)


