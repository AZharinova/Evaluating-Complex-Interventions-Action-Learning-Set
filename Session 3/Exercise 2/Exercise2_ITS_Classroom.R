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
ggplot(AEdata, aes(Period, AE)) + geom_line() +
  scale_x_date(limits=c(as.Date("2010-08-01"),as.Date("2021-06-01")),date_breaks = "3 month", date_labels="%b %Y") +
  theme(axis.text.x=element_text(angle=90))

AEdata_counter <- AEdata[AEdata$Period<"2020-02-01",]

AE_counter <- ts(data=AEdata_counter$AE, start=c(2010, 8), frequency=12)
AE_counter

components <- decompose(AE_counter)
plot(components)

arima <- auto.arima(AE_counter, seasonal=F)
summary(arima)
checkresiduals(arima)

sarima <- auto.arima(AE_counter, seasonal=T)
summary(sarima)
checkresiduals(sarima)

fc <- forecast(sarima, h=17)
plot(fc)
fc$mean

counter <- ts(c(AE_counter, fc$mean), start=start(AE_counter), frequency=frequency(AE_counter))
AEdata$pred <- as.vector(counter)

AEdata$diff <- AEdata$AE - AEdata$pred
lower <- as.data.frame(fc$lower)
AEdata$lower <- c(rep(0,114), lower$`95%`)
upper <- as.data.frame(fc$upper)
AEdata$upper <- c(rep(0,114), upper$`95%`)

ggplot(aes(Period, AE), data=AEdata) + geom_line() + geom_line(aes(Period, pred), data=AEdata[AEdata$Period>="2020-02-01",], color="blue") + 
  geom_vline(xintercept = as.Date("2020-02-01"), linetype="dashed") + geom_ribbon(aes(ymin=lower, ymax=upper),data=AEdata[AEdata$Period>="2020-02-01",], alpha=0.2, color="grey" )

missedAE <- sum(AEdata$diff)
missedAE 



###Causal Impact
## as a vector
set.seed(111)

## working with dates
pre.period <- c(1, 114)
post.period <- c(115, 131)

impact1 <- CausalImpact(AEdata$AE, pre.period, post.period)
plot(impact1)
summary(impact1)

pre.period <- as.Date(c("2010-08-01", "2020-01-01"))
post.period <- as.Date(c("2020-02-01", "2021-06-01"))
time.points <- seq.Date(as.Date("2010-08-01"), by="month", length.out=131)
data <- zoo(AEdata$AE, time.points)

impact2 <- CausalImpact(data, pre.period, post.period)
plot(impact2)

##adjust model
pre.period <- c(1, 114)
post.period <- c(115, 131)

impact3 <- CausalImpact(AEdata$AE, pre.period, post.period, model.args=list(niter=5000, nseasons=12))
plot(impact3, "original")
summary(impact3)
summary(impact3, "report")
impact3$series

##create custom bsts model
AEdata <- read.csv("~/projects/DSU course/AEdata.csv") #might change working directory
names(AEdata) [1] <- "Period"
AEdata$Period <- as.Date(AEdata$Period, format="%d/%m/%Y")
post.period.response <- AEdata$AE[post.period[1]:post.period[2]]
AEdata$AE[post.period[1]:post.period[2]] <- NA
ss <- AddLocalLinearTrend(list(), AEdata$AE) 
ss <- AddSeasonal(ss, AEdata$AE, nseasons=12)
bsts.model1 <- bsts(AEdata$AE, ss, niter=1000)

impact4 <- CausalImpact(bsts.model=bsts.model1, post.period.response=post.period.response)
plot(impact4)
summary(impact4)

bsts.prediction.errors(bsts.model1)
