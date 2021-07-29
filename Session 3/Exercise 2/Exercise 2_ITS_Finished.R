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

AEdata <- read.csv("~/projects/DSU course/AEdata.csv")
names(AEdata) [1] <- "Period"
AEdata$Period <- as.Date(AEdata$Period, format="%d/%m/%Y")

ggplot(AEdata, aes(Period, AE)) + geom_line() +
  scale_x_date(limits=c(as.Date("2010-11-07"), as.Date("2021-06-01")), date_breaks = "3 month", date_labels="%b %Y") + theme(axis.text.x=element_text(angle=90)) 


AEdata_counter <- AEdata[AEdata$Period<"2020-02-01",]
AE_counter <- ts(data=AEdata_counter$AE, start=c(2010, 8), frequency=12)
components_counter <- decompose(AE_counter)
plot(components_counter)

###ARIMA/SARIMA prediction
arima <- auto.arima(AE_counter, seasonal=F)
summary(arima)
checkresiduals(arima)

sarima <- auto.arima(AE_counter, seasonal=T)
summary(sarima)
checkresiduals(sarima)

fc <- forecast(sarima, h=17)
autoplot(fc)
fc$mean


counter <- ts(c(AE_counter, fc$mean),               # Combined time series object
   start = start(AE_counter),
   frequency = frequency(AE_counter))

AEdata$pred <- as.vector(counter) 
AEdata$diff <- AEdata$AE-AEdata$pred 

ggplot(aes(Period, AE), data=AEdata) + geom_line() + 
  geom_line(aes(Period, pred), data=AEdata[AEdata$Period>="2020-02-01",], colour="orange") + ggtitle("A&E attendances: impact of covid-19 pandemic/lockdown")

#add confidence intervals
lower <- as.data.frame(fc$lower)
AEdata$lower <- c(rep(0,114), lower$`95%`)
upper <- as.data.frame(fc$upper)
AEdata$upper <- c(rep(0,114), upper$`95%`)

ggplot(aes(Period, AE), data=AEdata) + geom_line() + 
  geom_line(aes(Period, pred), data=AEdata[AEdata$Period>="2020-02-01",], colour="blue") + ggtitle("A&E attendances: impact of covid-19 pandemic/lockdown") +
  geom_ribbon(aes(ymin = lower, ymax = upper),data=AEdata[AEdata$Period>="2020-02-01",], 
              alpha=0.2, 
              linetype="dashed",
              color="grey") + geom_vline(xintercept = as.Date("2020-02-02"),linetype="dashed")


#get sum of an effect over time, multiply by money
missedAE <- sum(AEdata$diff)
missedAE

cost_missedAE <- 150*missedAE
cost_missedAE

###Causal Impact
set.seed(111)
pre.period <- c(1, 114)
post.period <- c(115, 131)

impact <- CausalImpact(AEdata$AE, pre.period, post.period)
plot(impact)
summary(impact)
summary(impact, "report")
impact$summary

#try the same with the dates
pre.period <- as.Date(c("2010-08-01", "2020-01-01"))
post.period <- as.Date(c("2020-02-01", "2021-06-01"))
time.points <- seq.Date(as.Date("2010-08-01"), by = "month", length.out = 131)
data <- zoo(AEdata$AE, time.points)
impact1 <- CausalImpact(data, pre.period, post.period)
plot(impact1)
summary(impact1)

#adjust model
pre.period <- c(1, 114)
post.period <- c(115, 131)
impact2 <- CausalImpact(AEdata$AE, pre.period, post.period, model.args = list(niter = 5000, nseasons = 12))
plot(impact2)
summary(impact2)

#create custom bsts model
post.period <- c(115, 131)
post.period.response <- AEdata$AE[post.period[1] : post.period[2]]
AEdata <- read.csv("~/projects/DSU course/AEdata.csv")
names(AEdata) [1] <- "Period"
AEdata$Period <- as.Date(AEdata$Period, format="%d/%m/%Y")
AEdata$AE[post.period[1] : post.period[2]] <- NA


ss <- AddSemilocalLinearTrend(list(), AEdata$AE)
ss <- AddSeasonal(ss, AEdata$AE, nseasons = 12)

bsts.model1 <- bsts(AEdata$AE, ss, niter = 1000)

impact3 <- CausalImpact(bsts.model = bsts.model1,
                       post.period.response = post.period.response)

summary(impact3)
plot(impact3)

impact3$summary
impact3$series

#try with Christmas
AEdata <- read.csv("~/projects/DSU course/AEdata.csv")
names(AEdata) [1] <- "Period"
AEdata$Period <- as.Date(AEdata$Period, format="%d/%m/%Y")
ss <- AddSemilocalLinearTrend(list(), AEdata$AE)
ss <- AddSeasonal(ss, AEdata$AE, nseasons = 12)
holiday <- FixedDateHoliday("Christmas",
                            month = "December",
                            25,
                            days.before = 0,
                            days.after = 2)

ss <- AddRandomWalkHoliday(ss,
                           AEdata$AE,time0 = as.Date("2010-08-01"),
                           holiday )


bsts.model2 <- bsts(AEdata$AE, ss, niter = 1000)
impact4 <- CausalImpact(bsts.model = bsts.model1,
                        post.period.response = post.period.response)

summary(impact4)
plot(impact4)

impact4$summary
impact4$series

## find the best model

bsts.prediction.errors(bsts.model1)
bsts.prediction.errors(bsts.model2)
CompareBstsModels(list("Model 1" = bsts.model1,
                       "Model 2" = bsts.model2),
                  colors = c("black", "red", "blue"))
