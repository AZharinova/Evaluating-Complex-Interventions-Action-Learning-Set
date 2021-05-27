install.packages("ggplot2") 
install.packages("lubridate")
install.packages("lmtest")
install.packages("tseries")
install.packages("forecast")
install.packages("astsa")

library("ggplot2") 
library("lubridate")
library("lmtest")
library("tseries")
library("forecast")
library("astsa")

par(mfrow = c(1, 1))  ### if you still have the plot setting with 4 windows

###
### Evaluating Complex Interventions. Practical exercise 1.3 Time-Series Modelling ###

### AE data: load and wrangle a bit


##Research question: forecasting A&E attendances
# The columns we have are:
# Period - weekly data from 2010-11-07   
# AE - number of A&E attendances
# AE4h - number of attendances which breached 4 hours target


####### Step 1. Explore the time series  ##########


#in R, it might be easier to work with time-series object ts





 #moving average
 #look at trend
 #look at seasonal effect





####### Step 2. Test stationarity  ##########



####### Step 3. Find parametres and build the model  ##########


##potential seasonal ARIMA? - SARIMA


####### Step 4. Assess the model and predict  ##########


