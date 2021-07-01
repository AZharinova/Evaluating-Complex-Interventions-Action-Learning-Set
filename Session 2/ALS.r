# Install and load required packages --------------------------------------------------------------------------------------------------

install.packages("tidyverse")
install.packages("dagitty")
install.packages("ggdag")
install.packages("bnlearn")
install.packages("Rgraphviz")
install.packages("causaleffect")
install.packages("igraph")
install.packages("latex2exp")

library(tidyverse)
library(dagitty)
library(ggdag)
library(bnlearn)
library(Rgraphviz)
library(causaleffect)
library(igraph)
library(latex2exp)



# Slide 12. Common Effect example -----------------------------------------------------------------------------------------------------

talent <- rnorm(10000, 0, 1)
hardwork <- rnorm(10000, 0, 1)
df <- data.frame(talent, hardwork)

# Compute the correlation between the variables

df %>% summarize(correlation = cor(talent, hardwork))

# Success is equal to one if the sum of talent and hardwork is above the 80th percentile in the population -----------

x <- talent + hardwork
success <- 1*(x > quantile(x, c(.8)))
df <- cbind(df, success)

df %>% filter(success == 1) %>% summarize(correlation = cor(talent, hardwork))

# Slide 17. Correlation doesn't imply causation ---------------------------------------------------------------------

# Error terms

e_x <- rnorm(10000)
e_y <- rnorm(10000)
e_z <- rnorm(10000)

# Create nodes for the DAG: y <- x, y <- z, x <- z  

z <- 1*(e_z > 0)
x <- 1*(z + e_x > 0.5)
y <- 1*(x + z + e_y > 2)
y_dox <- 1*(1 + z + e_y > 2)

# We see that P(y|do(x=1)) is not equal to P(y|x=1)

mean(y_dox)
mean(y[x==1])

# Slide 17. Visualizing DAGs with ggdag -----------------------------------------------------------------------------

coords <- data.frame(matrix(c("x",0,0,
                              "y",2,0,
                              "z",1,1,
                              "w",1,0), nrow=4, ncol=3, byrow=T))

colnames(coords) <- c("name","x","y")

# "y ~ w" means that w is a parent of y

dag <- ggdag:: dagify(y ~ w, 
                      y ~ z,
                      w ~ x,
                      x ~ z,
              exposure = "x",
              outcome = "y",
              coords=coords)

# Plot the DAG

ggdag::ggdag(dag)

# Find all parents of outcome

ggdag:: ggdag_parents(dag, "y")


# Slide 20.Testable implications of graphs --------------------------------------------------------------------------

coords <- data.frame(matrix(c("A",0,0,
                              "B",0,1,
                              "C",1,2,
                              "D",2,1,
                              "E",2,0), nrow=5, ncol=3, byrow=T))

colnames(coords) <- c("name","x","y")

# "A ~ B" means that B is a parent of A

dag <- ggdag:: dagify(A ~ B, 
                      C ~ A,
                      C ~ B,
                      E ~ A,
                      C ~ D,
                      E ~ C,
                      E ~ D,
                      exposure = "A",
                      outcome = "E",
                      coords=coords)

# Plot the DAG

ggdag::ggdag(dag)


# Find all d-separation relationships in a DAG

impliedConditionalIndependencies(dag)

# Slide 24. PC Algorithm --------------------------------------------------------------------------------------------

# Simulate data : z <- x, w <- x, w <- y, w <- z 

x <- rnorm(10000, 0, 1)
y <- rnorm(10000, 0, 1)
z <- x + rnorm(10000, 0, 1)
w <- x + y + z + rnorm(1000, 0, 1)

df <- data.frame(x,y,z,w)

# PC Algorithm

str <- pc.stable(df, test = "cor", alpha = 0.05, undirected = FALSE)

graphviz.plot(str)

# Slide 29. Backdoor Criterion --------------------------------------------------------------------------------------

coords <- data.frame(matrix(c("x",0,0,
                              "w",1,0,
                              "y",2,0,
                              "z",1,1), nrow=4, ncol=3, byrow=T))

colnames(coords) <- c("name","x","y")

# "x ~ z" means that z is a parent of x

dag <- ggdag:: dagify(x ~ z, 
                      w ~ x,
                      y ~ w,
                      y ~ z,
                      exposure = "x",
                      outcome = "y",
                      coords=coords)

# Plot the DAG

ggdag::ggdag(dag)


# List all direct causal effects that are identifiable via backdoor adjustment

for(n in names(dag)){
  for(m in dagitty::children(dag,n)){
    a <- adjustmentSets(dag, n, m, effect="direct")
    if(length(a) > 0){
      cat("The causal effect of ",n," on ",m,
          " is identifiable controlling for:\n",sep="")
      print(a, prefix=" * ")
    }
  }
}

# Example 

# Error terms

e_x <- rnorm(10000)
e_y <- rnorm(10000)
e_z <- rnorm(10000)

# Create nodes for the DAG: y <- x, y <- z, x <- z  

z <- 1*(e_z > 0)
x <- 1*(z + e_x > 0.5)
y <- 1*(x + z + e_y > 2)

y_dox <- 1*(1 + z + e_y > 2)


mean(y_dox)

# Adjustment formula: P(y|do(x=1)) = P(y|x=1, z=1)*P(z=1) + P(y|x=1, z=0)*P(z=0)

mean(y[x==1 & z==1]) * mean(z==1) + mean(y[x==1 & z==0]) * mean(z==0)

# Do-Calculus ------------------------------------------------------------------------------------------------------

# The causaleffect package uses different syntax than "dagitty", note the +- instead of ~

dag <- graph.formula(y +- z5, y +- z6, y +- z1,
                     z6 +- x,
                     z5 +- z4,
                     z2 +- z3,
                     z1 +- z4, z1 +- z3,
                     x +- z2, x +- z1, 
                     simplify = FALSE)

# Shpitser and Pearl (2006) algorithm for the rules of do-calculus. If the effect is identifiable, the function
# returns an expression for the causal effect.

ce_backdoor <- causal.effect(y = "y", x = "x", z = NULL, G = dag, expr = TRUE)
plot(TeX(ce_backdoor), cex=2)
