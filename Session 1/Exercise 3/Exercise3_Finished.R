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

###
### Evaluating Complex Interventions. Practical exercise 1.3 Time-Series Modelling ###

### AE data: load and wrangle a bit
AEdata <- read.csv("~/projects/DSU course/AEdata.csv")
names(AEdata) [1] <-"Period"
AEdata$Period <- as.Date(AEdata$Period, "%d/%m/%Y")

##Research question: forecasting A&E attendances
# The columns we have are:
# Period - weekly data from 2010-11-07   
# AE - number of A&E attendances
# AE4h - number of attendances which breached 4 hours target


####### Step 1. Explore the time series  ##########
ggplot(AEdata, aes(Period, AE)) + geom_line() + ylab("Number of attendances") + ggtitle("A&E Activity: 2011-2015") +
  scale_x_date(limits=c(as.Date("2010-11-07"), as.Date("2019-04-01")), date_breaks = "1 month", date_labels="%b %Y") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_smooth(method=lm, formula=y~x) 

#in R, it might be easier to work with time-series object ts

AE <- ts(data = AEdata$AE, start=c(2010, 8), frequency=12)          #make time-series, monthly data starts at 2010
AE

par(mfrow = c(1, 1))  ### if you still have the same setting with 4 windows
plot(AE)


fltr <- c(1/2, rep(1, times = 11), 1/2)/12 #moving average
AE_trend <- stats::filter(AE, filter = fltr, method = "convo", 
                           sides = 2) #look at trend
AE_seas <- AE - AE_trend #look at seasonal effect
plot.ts(AE_trend)
plot.ts(AE_seas) # not so helpful as still yearly

## length of ts
ll <- length(AE_seas)
#frequency of ts
ff <- frequency(AE_seas)
## index of cumulative month
index <- seq(1, ll, by = ff) - 1
## get mean by month
mm <- numeric(ff)
for (i in 1:ff) {
  mm[i] <- mean(AE_seas[index + i], na.rm = TRUE)
}
## subtract mean to make overall mean = 0
mm <- mm - mean(mm)
plot.ts(mm, xlab = "Month")
        

####### Step 2. Test stationarity  ##########
#We can see the trend, but it is not as 
Box.test(AE, type="Ljung-Box") #Ljung-Box test, test stationary signal
adf.test(AE) #ADF test
kpss.test(AE, null="Trend") #KPSS test testing b1 coefficient - make sure to add Trend! 
kpss.test(AE, null="Level") #KPSS test testing b0 coefficient - not important for us

#the data looks stationary, hence we dont need to perform differencing yet

####### Step 3. Find parametres and build the model  ##########
acf(AE)
pacf(AE)

acf(diff(AE, 12)) #try acf for differences

#build using ARIMA function and found orders
Arima(AE, order=c(1,0,1))
Arima(AE, order=c(1,1,1))
Arima(AE, order=c(2,1,1))

#build using automated ARIMA function
arima1 <- auto.arima(AE, seasonal=F)
summary(arima1)

##potential seasonal ARIMA? - SARIMA
arima(AE, order=c(1,0,2), seasonal=order(c(1,1,1)))
sarima1 <- auto.arima(AE, seasonal=T)
summary(sarima1)
coeftest(sarima1)
(1-pnorm(abs(sarima1$coef)/sqrt(diag(sarima1$var.coef))))*2

#other way to do SARIMA - using sarima package
sarima2 <- sarima(AE, 1,0,2,1,1,1,12)


####### Step 4. Assess the model and predict  ##########
checkresiduals(arima1)
coeftest(arima1)
(1-pnorm(abs(arima1$coef)/sqrt(diag(arima1$var.coef))))*2

checkresiduals(sarima1)
coeftest(sarima1)
(1-pnorm(abs(sarima1$coef)/sqrt(diag(sarima1$var.coef))))*2

fc1 <- forecast(arima1, h=12)
autoplot(fc1)


fc2 <- sarima.for(AE, 12, 1,0,2,1,1,1,12) #using sarima forecast package
