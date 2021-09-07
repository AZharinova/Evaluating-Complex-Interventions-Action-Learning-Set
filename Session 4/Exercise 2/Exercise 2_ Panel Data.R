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
summary(df)

df <- df %>%
  mutate(Urban=as.factor(Urban),
         Ethnicity=as.factor(Ethnicity),
         MaritalStatus=as.factor(MaritalStatus),
         Education=as.factor(Education),
         Employment=as.factor(Employment),
         Wellbeing=as.numeric(Wellbeing))

summary(df)
str(df)

#explore panel
pdim(df, "Id") #what is structure
df_balanced1 <- make.pbalanced(df, balance.type = "fill")
df_balanced2 <- make.pbalanced(df, balance.type = "shared.individuals")

pd <- pdata.frame(df_balanced2, index=c("Id", "Year"))
summary(pd)

#check variable relationships
cor(pd[c("Age",  "Wellbeing", "MonthlyIncome", "Children", "HouseholdSize")])

plot(Age ~ Wellbeing, data=pd) 
plot(Age ~ Wellbeing, data=pd) 

plot(Ethnicity ~ Wellbeing, data=pd) 
plot(Employment ~ Wellbeing, data=pd) 

#Pooled model
pm_pooled <-  plm(Wellbeing~Urban+Age+Sex+
                            Ethnicity+BornUK+HouseholdSize+MaritalStatus+Children + Education + Employment+MonthlyIncome , 
                            model="pooling", data=pd)

summary(pm_pooled)


#Fixed Effects model
#LSDV
pm_lsdv <-  lm(Wellbeing~Urban+Age+Sex+
                    Ethnicity+BornUK+HouseholdSize+MaritalStatus+Children + Education + Employment+MonthlyIncome + factor(Id), data=pd)

summary(pm_lsdv) #have a look at coefficients in front of Id's => individual effects

#Within Estimator
pm_fixed <-  plm(Wellbeing~Urban+Age+Sex+
                   Ethnicity+BornUK+HouseholdSize+MaritalStatus+Children + Education + Employment+MonthlyIncome , 
                 model="within", data=pd)

summary(pm_fixed)



#Random effects model
pm_random <-  plm(Wellbeing~Urban+Age+Sex+
                   Ethnicity+BornUK+HouseholdSize+MaritalStatus+Children + Education + Employment+MonthlyIncome , 
                 model="random", data=pd)
summary(pm_random)

#fixed vs pooled
pFtest(pm_fixed, pm_pooled)

#random vs fixed - Hausman test
phtest(pm_fixed, pm_random) #random would not work, but we can sort endogeneity using instrumental variables or something - more about this next year

#random vs pooled
plmtest(pm_pooled, type=c("bp")) #significant random effects

##Let's pretend the big wellbeing programme launched in 2012
pd$Intervention <- ifelse(pd$Year==2009 |pd$Year==2010 | pd$Year==2011, 0, 1)
pm_fixed <-  plm(Wellbeing~Urban+Age+Sex+
                   Ethnicity+BornUK+HouseholdSize+MaritalStatus+Children + Education + Employment+MonthlyIncome +Intervention , 
                 model="within", data=pd)

summary(pm_fixed)

#test whether effects are individual or time or both
plmtest(pm_fixed, effect="time")
plmtest(pm_fixed, effect="individual")


#find the best modification
pm_fixed <-  plm(Wellbeing~Urban+Age^2+MaritalStatus+Children + Education + Employment+MonthlyIncome +Intervention, effect="individual",
                 model="within", data=pd)

summary(pm_fixed)

