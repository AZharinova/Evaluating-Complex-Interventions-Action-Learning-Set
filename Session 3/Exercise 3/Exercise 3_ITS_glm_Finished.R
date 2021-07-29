# required packages
install.packages("tidyverse")
install.packages("season")
install.packages("Epi")
install.packages("tsModel")
install.packages("zoo")
library("tidyverse")
library("season") 
library("Epi")
library("tsModel")
library("zoo") 
# also requires "Epi", "lmtest" and "tsModel" - these just need to be installed not loaded


###
### Evaluating Complex Interventions. Practical exercise 3.3. ITS (part2 - using glm) ###
#Impact of smoking ban in Sicily on acute coronary episodes
# year and month as separate columns
# time = elapsed time since the start of the study
# aces = monthly count of acute coronary episodes
# smokban = smoking ban (0 - before intervention, 1 after)
# pop = total population
# stdpop = age standardised population

sicily <- read.csv("~/projects/DSU course/sicilyDataset.csv") #might have to change directory
sicily


# explore the Sicily dataset 
sicily <- sicily %>%
  mutate(dt = as.yearmon(paste(sicily$year, sicily$month, sep = "-")))

summary(sicily)

# plot admissions prior to intervention
ggplot(data = sicily[sicily$smokban == 0,],aes(x = dt, y = aces) ) +
  geom_line() +
  scale_x_yearmon(name = "time", format = "%b-%y") +
  scale_y_continuous(limits = c(0, 1200))

# plot full series and intervention
ggplot(data = sicily, aes(x = time, y = aces)) +
  geom_point() +
  geom_vline(xintercept = 36.5, color = "grey") +
  scale_y_continuous(limits = c(0, 1200))

# plot population
ggplot(data = sicily, aes(x = dt, y = pop)) +
  geom_line() +
  scale_x_yearmon(name = "time", format = "%b-%y") +
  scale_y_continuous(limits = c(0, max(sicily$pop)))


# investigate timeseries seasonality
sicily_counter <- sicily[sicily$smokban == 0,]
sicilyTS <- ts(sicily_counter$aces, frequency = 12)
components1 <- decompose(sicilyTS) #decompose function
plot(components1)

components2 <- stl(sicilyTS, s.window = "periodic") #stl function
plot(components2)


# level change impact model 

# fit a simple linear trend model to the pre intervention period
modelPre <- glm(aces ~ time, family = gaussian, data = sicily %>% filter(smokban == 0))
summary(modelPre)

sicilyPre <- sicily %>% 
  filter(smokban == 0) %>% 
  mutate(preTrend = predict(modelPre, type = "response"))

# plot the linear trend model
ggplot(data = sicilyPre) +
  geom_point(aes(x = time, y = aces)) +
  geom_line(aes(x = time, y = preTrend), color = "red") +
 scale_x_continuous(name = "time", breaks = seq(.5, 60.5, by = 12), labels = 2002:2007) 

# use the above model to predict admissions as if the ban was never implemented i.e. to generate a counterfactual
sicilyCf <- sicily %>%
  mutate(preCf = predict(modelPre, type = "response", newdata = sicily))

# plot the counterfactual
ggplot(data = sicilyCf) +
  geom_point(aes(x = time, y = aces)) +
  geom_line(aes(x = time, y = preCf), color = "red", linetype = "dashed") +
  geom_vline(xintercept = 36.5, color = "grey") +
 scale_x_continuous(name = "time", breaks = seq(.5, 60.5, by = 6), labels = c("", rbind(2002:2006, ""))) 

# impact model for a change in level
modelLevelChg <- glm(aces ~ time + smokban, family = gaussian, data = sicily)
# note - smokan coefficient suggests a drop of 89 admissions associated with the smoking ban
summary(modelLevelChg)
# print confidence intervals
confint(modelLevelChg)


# calculate predicted admissions from the level change model
sicilyLevelChg <- sicily %>% 
  mutate(levelChg = predict(modelLevelChg, type = "response"))

# plot the level change model
ggplot(data = sicilyLevelChg) +
  geom_point(aes(x = time, y = aces)) +
  geom_line(aes(x = time, y = levelChg), color = "red") +
  geom_vline(xintercept = 36.5) +
  scale_x_continuous(
    name = "time", breaks = seq(.5, 60.5, by = 6)
    , labels = c("", rbind(2002:2006, ""))) 

# create a counterfactual using the level change model
sicilyLevelChg <- sicilyLevelChg %>% 
  mutate(levelChgCf = predict(
    modelLevelChg, type = "response", newdata = sicily %>% mutate(smokban = 0)))

# plot counterfactual and level change impact model
ggplot(data = sicilyLevelChg) +
  geom_point(aes(x = time, y = aces)) +
  geom_line(aes(x = time, y = levelChg), color = "red") +
  geom_line(aes(x = time, y = levelChgCf), color = "blue") +
  geom_vline(xintercept = 36.5) +
  scale_x_continuous(name = "time", breaks = seq(.5, 60.5, by = 6), 
                     labels = c("", rbind(2002:2006, ""))) 


# model checks 
# check level change model residuals by plotting against time
par(mfrow = c(2, 2))
plot(modelLevelChg)

res <- residuals(modelLevelChg, type = "deviance")
ggplot() +
  geom_point(aes(x = sicilyLevelChg$time, y = res)) +
  scale_x_continuous() +
  scale_y_continuous(limits = c(min(res * 1.5), max(res * 1.5)))

# check for autocorrelation
par(mfrow = c(1, 1))
acf(res)
pacf(res)
Box.test(sicilyTS, type="Ljung-Box")
lmtest::dwtest(modelLevelChg)  # Durbin-Watson test suggests presence of autocorrelation


# level and slope change impact model 
modelSlopeLevel <- glm(aces ~ time + smokban*time, data = sicily)
summary(modelSlopeLevel)
confint(modelSlopeLevel)


# generate predictions from this model for plotting
sicilySlopeLevel <- sicily %>% 
  mutate(slopeLevel = predict(modelSlopeLevel, type = "response"))

ggplot(data = sicilySlopeLevel) +
  geom_point(aes(x = time, y = aces)) +
  geom_line(aes(x = time, y = slopeLevel), color = "red") +
  geom_vline(xintercept = 36.5, color = "grey") +
  scale_x_continuous(name = "time", breaks = seq(.5, 60.5, by = 6), labels = c("", rbind(2002:2006, "")))


# more models 
# month as a categorical variable to account for seasonality
m01 <- glm(aces ~ time + smokban + as.factor(month), family = gaussian, data = sicily)
summary(m01)

# poisson model for count variables
m02 <- glm(aces ~ time + smokban, family = poisson, sicily)
summary(m02)
ci.lin(m02, Exp = T)

# poisson model with harmoinic for seasonality
m03 <- glm(aces ~ time + smokban + harmonic(month, 2, 12), family = poisson, sicily)
summary(m03)
ci.lin(m03, Exp = T)

# suggests no over dispersion
summary(m03)$dispersion

# plot model 3
ggplot(data = sicily %>% mutate(m04 = predict(m04, type = "response", data = sicily))) +
  geom_point(aes(x = time, y = aces)) +
  geom_line(aes(x = time, y = m04), color = "red") +
  geom_vline(xintercept = 36.5, color = "grey") +
  scale_x_continuous(name = "time", breaks = seq(.5, 60.5, by = 6), labels = c("", rbind(2002:2006, ""))) +
  scale_y_continuous(limits = c(0, 1200))
