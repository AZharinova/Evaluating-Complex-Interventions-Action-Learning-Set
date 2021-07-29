library(tidyverse)
library(zoo)

###
### Evaluating Complex Interventions. Practical exercise 3.1. Dif-in-Dif ###
#Intervention in Midlands region, 1st of Jan 2020 to improve bed management => decrease number of DTOC days

DTOC_raw <- read.csv("~/projects/DSU course/DTOC.csv")
names(DTOC_raw)[1] <- "Region"
names(DTOC_raw)[3] <- "DTOC_days"
DTOC_raw$Region <- as.factor(DTOC_raw$Region)
DTOC_raw$Provider <- as.factor(DTOC_raw$Provider)
DTOC <- DTOC_raw[DTOC_raw$Month=="Jan-20" |DTOC_raw$Month=="Dec-19", ]


#descriptive statistics
summary(DTOC)

DTOC %>%
  select(Region, Month) %>%
  table() %>%
  prop.table(margin = 2)  %>%
  apply(MARGIN = 2,
        FUN = scales::percent_format(accuracy = 0.1)) %>%
  noquote  #get % for months

DTOC %>%
  select(Region, Provider) %>%
  group_by(Region) %>%
  count() #get counts or % for providers

#pre-treatment stats
DTOC %>%
  filter(Month == "Dec-19") %>%
  group_by(Region) %>%
  summarise(mean_DTOC = mean(DTOC_days, na.rm = TRUE),
            max_DTOC  = max(DTOC_days, na.rm = TRUE),
            min_DTOC = min(DTOC_days, na.rm = TRUE))
#post-trearment mean 
DTOC %>%
  filter(Month == "Jan-20") %>%
  group_by(Region) %>%
  summarise(mean_DTOC = mean(DTOC_days, na.rm = TRUE),
            max_DTOC  = max(DTOC_days, na.rm = TRUE),
            min_DTOC = min(DTOC_days, na.rm = TRUE))


####dif-in-dif - subtracting differences
differences <- DTOC %>%
  group_by(Month, Region) %>%
  summarise(DTOC_days = mean(DTOC_days, na.rm = TRUE))
differences

# Control group (London) before treatment
londec <- differences[1,3]

# Treatment group (Midlands) before treatment
middec <- differences[2,3]

# Control group (London) after treatment
lonjan <- differences[3,3]

# Treatment group (Midlands) after treatment
midjan <- differences[4,3]

#calculate the difference in treatment group
d1 <- midjan - middec
d1

#calculate the difference in control group
d2 <- lonjan - londec
d2

#calculate the difference in difference
dd <- d1-d2
dd


#dif-in-dif visually
# Calculate counterfactual outcome
DTOC_counter <- tibble(
  Month = c("Dec-19","Jan-20"), 
  Region = c("Midlands (Counterfactual)","Midlands (Counterfactual)"),
  DTOC_days = as.numeric(c(middec, middec-(londec-lonjan)))
) 


# Combine data
did_plotdata <- bind_rows(differences, 
                          DTOC_counter)

did_plotdata %>%
  mutate(label = if_else(Month == "Jan 2020", as.character(Region), NA_character_)) %>%
  ggplot(aes(x=Month,y=DTOC_days, group=Region)) +
  geom_line(aes(color=Region), size=1.2) +
  #geom_vline(xintercept = "Intervention", linetype="dotted", 
   #          color = "black", size=1.1) + 
  scale_color_brewer(palette = "Accent") +
  scale_y_continuous(limits = c(300,900)) 
 

#dif-in-dif - regression 
DTOC <- mutate(DTOC,
               time = ifelse(Month == "Jan-20", 1, 0),
               treated = ifelse(Region == "Midlands", 1, 0)
) #create dummy

did_model <- lm(DTOC_days ~ time + treated + time:treated, data = DTOC)
summary(did_model) #run lm, time@treated is dif-in-dif estimator


##is it a good method?
##placebo test if we have time
DTOC <- DTOC_raw[DTOC_raw$Month=="Nov-19" |DTOC_raw$Month=="Dec-19", ]

DTOC <- mutate(DTOC,
               time = ifelse(Month == "Dec-19", 1, 0),
               treated = ifelse(Region == "Midlands", 1, 0)
) #create dummy

did_model <- lm(DTOC_days ~ time + treated + time:treated, data = DTOC)
summary(did_model) #run lm, time@treated is dif-in-dif estimator

#how it looks visually
DTOC_raw$Month <- as.yearmon(DTOC_raw$Month, format="%b-%y")
DTOC_raw %>% 
  group_by(Region, Month) %>% 
  summarise(mean_days=mean(DTOC_days)) %>% 
  ggplot(aes(x=Month, y=mean_days, group=Region)) +  geom_line(aes(color=Region))  + scale_y_continuous(limits = c(300,900)) 

