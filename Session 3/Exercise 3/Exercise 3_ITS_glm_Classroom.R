install.packages("tidyverse")
install.packages("season")
install.packages("Epi")
install.packages("tsModel")
install.packages("zoo")
install.packages("car")
library("tidyverse")
library("season") 
library("Epi")
library("tsModel")
library("zoo")
library("car")

###
### Evaluating Complex Interventions. Practical exercise 3.3. ITS (part2 - using glm) ###
#Impact of smoking ban in Sicily on acute coronary episodes
# time = elapsed time since the start of the study
# aces = monthly count of acute coronary episodes
# smokban = smoking ban (0 - before intervention, 1 after)
# pop = total population
# stdpop = age standardised population

sicily <- read.csv("~/projects/DSU course/sicilyDataset.csv") #might have to change directory
sicily
sicily$t <- seq(1, nrow(sicily)) #if you dont have time variable

# explore the timeseries
sicily$dt <- as.yearmon(paste(sicily$year, sicily$month, sep="-"))
summary(sicily)

#plot prior to intervention
ggplot(data=sicily[sicily$smokban==0,], aes(x=dt, y=aces)) + geom_line() + scale_y_continuous(limits=c(500, 1000)) 

#plot full data series
ggplot(data=sicily, aes(x=time, y=aces)) + geom_point() + geom_vline(xintercept=36.5, color="grey") + geom_smooth(method="loess")

summary(sicily$stdpop)
ggplot(data=sicily, aes(x=dt, y=pop)) + geom_line() + scale_y_continuous(limits=c(0, 370000))

#investigate timeseries (counterfactual)
sicily_counter <- sicily[sicily$smokban==0,]
sicilyTS <- ts(sicily_counter$aces, frequency = 12)
components1 <- decompose(sicilyTS)
plot(components1)

components2 <- stl(sicilyTS, s.window="periodic")
plot(components2)

acf(sicilyTS)
pacf(sicilyTS)

# build ITS  - level change
#start from pre-intervention period only
modelPre <- glm(aces ~ time, family=gaussian, data=sicily %>% filter(smokban==0))
modelPre <- glm(aces ~ time, family=gaussian, data=sicily[sicily$smokban==0,])
summary(modelPre)

#create counteractual
sicilyPre <- sicily %>% 
  filter(smokban==0) %>% 
  mutate(preTrend=predict(modelPre, type="response"))

ggplot(data=sicilyPre, aes(x=time, y=aces)) + geom_point() + geom_line(aes(x=time, y=preTrend), color="blue") #trend for pre-intervention

sicilyCF <- sicily %>% 
  mutate(preCF = predict(modelPre, type="response", newdata=sicily))

ggplot(data=sicilyCF, aes(x=time, y=aces)) + geom_point() + 
  geom_line(aes(x=time, y=preCF), color="blue", linetype="dashed") + geom_vline(xintercept=36.5)

#add dummy of an intervention in 
modelLevelChg <- glm(aces ~ time + smokban, family="gaussian", data=sicily)
summary(modelLevelChg)
confint(modelLevelChg)

sicilyLevelChg <- sicily %>% 
  mutate(levelChg = predict(modelLevelChg, type="response"))

ggplot(data=sicilyLevelChg, aes(x=time, y=aces)) + geom_point() + 
  geom_line(aes(x=time, y=levelChg), color="blue") + geom_vline(xintercept=36.5) 

sicilyLevelChg$levelChgCF <- predict(modelLevelChg, type="response", newdata=sicilyLevelChg %>% mutate(smokban==0)) 

########double checked correct plot
ggplot(data = sicilyLevelChg) +
  geom_point(aes(x = time, y = aces)) +
  geom_line(aes(x = time, y = levelChg), color = "red") +
  geom_line(aes(x = time, y = levelChgCf), color = "blue") +
  geom_vline(xintercept = 36.5) +
  scale_x_continuous(name = "time", breaks = seq(.5, 60.5, by = 6), 
                     labels = c("", rbind(2002:2006, ""))) 


# check model
par(mfrow=c(2,2))
plot(modelLevelChg)
res <- residuals(modelLevelChg, type="deviance")

#check autocorrelation before choosing different modification
par(mfrow=c(1,1))
acf(sicilyTS)
pacf(sicilyTS)
Box.test(sicilyTS, type="Ljung-Box") #data is not stationary
dwtest(modelLevelChg)


# build ITS  - level and slope change 
#if you want to add variable P similarly to the presentation
sicily$P <- as.vector(ifelse(sicily$smokban==0, 0, sicily$t-36))  #DOUBLE CHECK THE SECOND WAY

#we are going to treat slop as interaction team
modelSlopeLevel <- glm(aces~time + smokban*time, data=sicily)
summary(modelSlopeLevel)
confint(modelSlopeLevel)

# Add seasons/months
modelLevel1 <- glm(aces ~ time + smokban + as.factor(month), family=gaussian, data=sicily)
summary(modelLevel1) 
confint(modelLevel1)

summary(modelLevelChg)

# poisson
modelLevel2 <- glm(aces ~ time + smokban, family=poisson, data=sicily)
summary(modelLevel2)
confint(modelLevel2)
ci.lin(modelLevel2 , Exp=T)

# poisson model with harmoinic for seasonality
modelLevel3 <- glm(aces ~ time + smokban + harmonic(month, 2, 12), family=poisson, data=sicily)
summary(modelLevel3)
ci.lin(modelLevel3, Exp=T)


#adding control  - did not improve model
modelLevel4 <- glm(aces ~ time + smokban + harmonic(month, 2, 12) + stdpop, family=poisson, data=sicily)
summary(modelLevel4)
vif(modelLevel4) #no multicollinearity


summary(modelLevel3)$dispersion #no overdispersion


# quasipoisson model with harmoinic for seasonality
modelLevel5 <- glm(aces ~ time + smokban + harmonic(month, 2, 12), family=quasipoisson, data=sicily)
summary(modelLevel5)
summary(modelLevel5)$dispersion #actually makes things worse

# plot final model of choice - number 3 (poisson)
ggplot(data = sicily %>% mutate(mod3=predict(modelLevel3, type="response", data=sicily))) + 
  geom_point(aes(x=time,y=aces)) + geom_line(aes(x=time, y=mod3), color="blue") + 
  geom_vline(xintercept=36.5, "grey", linetype="dashed")  +
  scale_x_continuous(name="time", breaks=seq(0.5, 60.5, by=6), labels=c("", rbind(2002:2006, "")))
