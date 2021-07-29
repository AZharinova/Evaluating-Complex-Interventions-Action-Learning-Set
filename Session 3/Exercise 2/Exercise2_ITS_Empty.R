install.packages("tidyverse")
install.packages("forecast")
install.packages("CausalImpact")
install.packages("zoo")
library(tidyverse)
library(forecast)
library(CausalImpact)
library(zoo)


###
### Evaluating Complex Interventions. Practical exercise 3.2. ITS (part1) ###
#Impact of lockdown/covid-19 on A&E attendances (interruption point - 1/02/2020)

AEdata <- read.csv("~/projects/DSU course/AEdata.csv") #might change working directory
names(AEdata) [1] <- "Period"
AEdata$Period <- as.Date(AEdata$Period, format="%d/%m/%Y")



###ARIMA/SARIMA ITS 








###Causal Impact
## as a vector
set.seed(111)


## working with dates


##adjust model



##create custom bsts model


