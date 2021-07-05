install.packages("readr")

library(readr)

Data <- read_delim("C:/Users/bruno.Petrungaro/Documents/ALS Quasi experiments/processed.cleveland.txt", col_names = FALSE, delim = ",")

names(Data) <- make.names(c("age",
                            "sex",
                            "cp",
                            "trestbps",
                            "chol",
                            "fbs",
                            "restecg",
                            "thalach",
                            "exang",
                            "oldpeak",
                            "slope",
                            "ca",
                            "thal",
                            "num"))

# 1. Missing Data? We do
# 2. Class imbalance? Not a concern
# 3. Outliers? We do
# 4. Discretize !!!

summary(Data)

unique(Data$ca)
unique(Data$thal)

hd <- function(x){
  if(x>0){
    return(1)
  } else {
    return(0)
  }
}

for (i in 1:nrow(Data)){
  Data[i,c("num")] <- hd(Data[i,c("num")])
}

## 1. Missing Data -------------------------------------

# delete rows NO
# missForest
# MICE

Data$ca[Data$ca=="?"] <- NA

library(dplyr)

value_label_map_ca <- Data %>%
  select(ca) %>%
  mutate(ca_num = as.numeric(as.factor(ca))) %>%
  distinct()

Data <- Data %>%
  mutate(ca = as.numeric(as.factor(ca)))

Data$thal[Data$thal =="?"] <- NA

value_label_map_thal <- Data %>%
  select(thal) %>%
  mutate(thal_num = as.numeric(as.factor(thal))) %>%
  distinct()

Data <- Data %>%
  mutate(thal = as.numeric(as.factor(thal)))

Missing <- data.frame(is.na(Data))
Prop_Missing <- apply(Missing, 2, mean)
names(Missing) <- colnames(Data)

print(round(Prop_Missing,3))

install.packages("mice")

library(mice)

feature_selection <- quickpred(Data, method = 'spearman')

Imputation <- mice(Data, m=1, method = "pmm", visitSequence = "monotone", predictorMatrix = feature_selection)

Data_Imputation <- c()

Data_Imputation <- rbind(Data_Imputation, complete(Imputation, action = "long"))

Data <- data.frame(Data_Imputation)

unique(Data$thal)

Data_1 <- Data[,c("age",
                  "trestbps",
                  "chol",
                  "thalach",
                  "oldpeak")]

Data_1 <- bnlearn::discretize(Data_1, method = "interval", breaks = 5)

Data$age <- Data_1$age

Data$trestbps <- Data_1$trestbps

Data$chol <- Data_1$chol

Data$thalach <- Data_1$thalach

Data$oldpeak <- Data_1$oldpeak

Data <- Data[,!(names(Data) %in% c(".imp", ".id"))]

Data <- Data %>%  mutate_all(as.factor)

### tabu, hc, pc.stable

library(bnlearn)

str_tabu <- tabu(Data)

graphviz.plot(str_tabu)

str_hc <- hc(Data)

graphviz.plot(str_hc)

# not use
str_pc <- pc.stable(Data)
graphviz.plot(str_pc)

variables <- data.frame(matrix(c("age","sex"), nrow=1, ncol=2, byrow=T))

str_tabu <- tabu(Data, whitelist =variables)

graphviz.plot(str_tabu)

library(igraph)
library(causaleffect)
dag <- graph.formula(thal +- sex,
                     num +- thal)

ce <- causaleffect::causal.effect(y = "num", x = "sex", z = NULL, G=dag, expr =TRUE)

library(latex2exp)

plot(TeX(ce), cex = 2)

str_tabu <- tabu(Data)

fitted <- bn.fit(str_tabu, Data)

bn.fit.barchart(fitted$num)

fitted

# added code

BiocManager::install("RBGL")
install.packages("gRain")
library(gRain)

# implementing the junction tree algorithm so we can do inference in our Bayesian Network

jtree <- compile(as.grain(fitted))

# introducing intervention by altering the distribution of a variable, setting variable to a state in this case 1

jprop <- setFinding(jtree, nodes = "sex", state = "1")

# probability distribution of num (hd) with no intervention

querygrain(jtree, nodes = "num")$num

# with intervention

querygrain(jprop, nodes = "num")$num