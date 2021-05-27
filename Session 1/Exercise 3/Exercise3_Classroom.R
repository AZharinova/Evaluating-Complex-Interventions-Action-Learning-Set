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
AEdata <- read.csv("~/projects/DSU course/AEdata.csv")
names(AEdata) [1] <- "Period"
AEdata$Period <- as.Date(AEdata$Period, format="%d/%m/%Y")

##Research question: forecasting A&E attendances
# The columns we have are:
# Period - weekly data from 2010-11-07   
# AE - number of A&E attendances
# AE4h - number of attendances which breached 4 hours target


####### Step 1. Explore the time series  ##########
ggplot(AEdata, aes(Period, AE)) + geom_line() +
  scale_x_date(limits=c(as.Date("2010-11-07"), as.Date("2019-04-01")), date_breaks = "3 month", date_labels="%b %Y") + theme(axis.text.x=element_text(angle=90)) +
  geom_smooth(method=lm)

#in R, it might be easier to work with time-series object ts
AE <- ts(data=AEdata$AE, start=c(2010, 8), frequency=12)
AE

plot(AE) 

ma <- c(1/2, rep(1,times=11), 1/2)/12 #filter/weights for ma trend
AE_trend <- stats::filter(AE, filter=ma, method="convo", sides=2) #moving average
plot.ts(AE_trend) #look at trend

AE_seas <- AE-AE_trend
plot(AE_seas)

 #look at seasonal effect for each month separately
ll <- length(AE_seas)
ff <- frequency(AE_seas)

index <- seq(1,ll,by=ff)- 1 
mm <- numeric(ff)

#get numeric 0
#get mean by month
for(i in 1:ff) { 
  mm[i]=mean(AE_seas[index+i], na.rm=TRUE)
  }
mm <- mm - mean(mm)

plot.ts(mm)

####### Step 2. Test stationarity  ##########
adf.test(AE)
Box.test(AE, type="Ljung-Box")
kpss.test(AE, null="Trend")
kpss.test(AE, null="Level")

acf(AE)
pacf(AE)

acf(diff(AE,1))

####### Step 3. Find parametres and build the model  ##########
arima1 <- Arima(AE, order=c(1,1,1))
arima2 <- Arima(AE, order=c(1,2,1))
coeftest(arima2)

coeftest(Arima(AE, order=c(1,2,1)))
arima3 <- Arima(AE, order=c(2,2,1))
summary(arima3)
coeftest(arima3)

arima4 <- auto.arima(AE, seasonal=F)
summary(arima4)
coeftest(arima4)

##potential seasonal ARIMA? - SARIMA
arima5 <- auto.arima(AE, seasonal=T) #this is sarima, the best model
summary(arima5)
coeftest(arima5)

#other way to do sarima models
arima6 <- sarima(AE, 1, 0, 2, 1, 1, 1, 12)  #this is sarima using astsa package

####### Step 4. Assess the model and predict  ##########
checkresiduals(arima5)
checkresiduals(arima2)

fc1 <- forecast(arima5, h=12)
autoplot(fc1)


fc2 <- sarima.for(AE,12, 1, 0, 2, 1, 1, 1, 12) #this is sarima prediction using astsa package
fc2

