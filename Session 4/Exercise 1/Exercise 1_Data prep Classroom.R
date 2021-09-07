install.packages("tidyverse")
install.packages("psych")
install.packages("effectsize")
install.packages("mice")
library(tidyverse)
library(psych)
library(effectsize)
library(mice)

## import dataset
paneldata <- read.csv("~/projects/DSU course/paneldata.csv")

## view the data
view(paneldata)
str(paneldata)


## summary statistics
describe(paneldata)
describeBy(paneldata[-c(2)],group=paneldata$Year, mat=TRUE)

summary(paneldata)
summary(as.factor(paneldata$Education))
as.factor(paneldata$Education) %>%
  summary()

table(paneldata$Year, paneldata$Urban)

paneldata %>% 
  select(Age, Year) %>% 
  group_by(Year) %>% 
  summarise(avg=mean(Age), 
            min=min(Age),
            max=max(Age))

plot(paneldata$Age)
plot(Age ~ Wellbeing, data=paneldata)
plot(MonthlyIncome ~ Wellbeing, data=paneldata)

## detecting outliers (visually)
plot(paneldata$Age)
plot(paneldata$HouseholdSize)
plot(paneldata$MonthlyIncome)

boxplot(paneldata$MonthlyIncome)

paneldata %>% 
  filter(MonthlyIncome <=5000) %>% 
  select(MonthlyIncome) %>% 
  boxplot()

boxplot(Age ~ Wellbeing, data=paneldata)

## detecting outliers (statistically)
paneldata$z_MonthlyIncome <- (paneldata$MonthlyIncome - mean(paneldata$MonthlyIncome))/sd(paneldata$MonthlyIncome)
summary(paneldata$z_MonthlyIncome)

paneldata %>% 
  filter(paneldata$z_MonthlyIncome>3.29) %>% 
  count()

paneldata$z_MonthlyIncome <- standardize(paneldata$MonthlyIncome)
summary(paneldata$z_MonthlyIncome)

## dealing with data quality
#outliers 
#replace outliers with NA 
paneldata$MonthlyIncome_rem <- ifelse(paneldata$z_MonthlyIncome >3.29 | paneldata$MonthlyIncome<0, NA, paneldata$MonthlyIncome)

#winsoring - replacing extreme values with the next highest/lowest value
winsored <- paneldata %>% 
  mutate(MonthlyIncome_w=winsor(MonthlyIncome, trim=0.02))

#filter 
filtered <- subset(paneldata, paneldata$z_MonthlyIncome<3.29)
plot(filtered$MonthlyIncome)

## imputing missing values
#identify missing values
summary(paneldata)
mean(is.na(paneldata$Education))
mean(is.na(paneldata$BMI))

mean(complete.cases(paneldata$Education))

paneldata %>% 
  filter(is.na(paneldata$BMI)) %>% 
  group_by(Year) %>% 
  count(BMI)

paneldata %>% 
  group_by(Year) %>% 
  count()

paneldata %>%
  select(Year, BMI) %>%
  group_by(Year) %>%
  summarise(mean_missing = mean(is.na(BMI)))

#dropping missing data
missing_dropped <- paneldata %>% 
                   drop_na()

#mean imputation
mean_imputation <- paneldata %>% 
  mutate(BMI_imp=ifelse(is.na(BMI), mean(BMI, na.rm=TRUE), BMI))

#find ID in the previous time units
paneldata %>% 
  filter(is.na(Education)) %>%  
  group_by(Id)

paneldata %>% 
  filter(Id==340462419) 

#categorical value  - most frequent
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}   #alternatively, we can use modeest package

freq_imputation <- paneldata %>% 
  mutate(MaritalStatus_imp=ifelse(is.na(MaritalStatus), getmode(MaritalStatus), MaritalStatus), 
         Education_imp=ifelse(is.na(Education), getmode(Education), Education),
         Employment_imp=ifelse(is.na(Employment), getmode(Employment), Employment))

freq_imputation %>% 
  filter(is.na(MaritalStatus) | is.na(Education) )

#multiple imputation
md.pattern(paneldata[c("Ethnicity", "MaritalStatus", "Education", "BMI")])
#come back if we have time and interest


##combine all missing values techniques
paneldata$Education <- freq_imputation$Education_imp
paneldata$Employment <- freq_imputation$Employment_imp
paneldata$MaritalStatus <- freq_imputation$MaritalStatus_imp
summary(paneldata)
summary(freq_imputation)

#rename values and change categories
paneldata <- freq_imputation %>% 
  mutate(Ethnicity=case_when(
    Ethnicity<5 ~ "White",
    Ethnicity>4 & Ethnicity <9 ~ "Mixed",
    Ethnicity>=9 & Ethnicity <14 ~ "Asian",
    Ethnicity>=14 & Ethnicity<17 ~ "Black",
    TRUE ~ "Other"
  ) , 
        Education=case_when(
          Education < 3 ~ "High", 
          Education == 3 ~ "College",
          Education == 4 ~ "School", 
          Education == 5 ~ "Other",
          Education == 9 ~ "None"
        ),
  MaritalStatus= case_when(
    MaritalStatus == 1 ~ "Single",
    MaritalStatus == 2 | MaritalStatus == 5 ~ "Partner",
    MaritalStatus == 3  ~ "Separated", 
    MaritalStatus == 4  ~ "Widowed", 
    TRUE ~ "Other"))

paneldata$Employment <- ifelse(freq_imputation$Employment < 3, "Employed", 
                               ifelse(freq_imputation$Employment==3, "Unemployed", 
                                      ifelse(freq_imputation$Employment<11, "Inactive", "Other")
))
paneldata$Urban <- ifelse(paneldata$Urban==1, "Urban", "Rural")
paneldata$MonthlyIncome <- ifelse(paneldata$MonthlyIncome<0, winsored$MonthlyIncome_w, paneldata$MonthlyIncome)
summary(paneldata)
write.csv(paneldata, "paneldata_clean.csv")

#optional - reverse order of variable
summary(paneldata$Wellbeing)
key <- c(-1)
paneldata$Wellbeing_rev <- reverse.code(key, paneldata$Wellbeing)

summary(paneldata)
