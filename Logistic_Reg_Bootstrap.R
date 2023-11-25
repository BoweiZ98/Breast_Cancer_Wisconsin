###### Setting Up ######
Sys.setLanguage(lang = "en")
library(MASS)
library(car)
library(class)
library(tidyverse)
library(caret)
library(boot)

train = read.csv("datasets/train.csv")
val = read.csv("datasets/val.csv")
xnams = dput(colnames(train)[colnames(train) != "Class"])

###### Logistic Regression ######
fml = as.formula(paste("Class ~", paste(xnams, collapse = "+")))
fit.logi = glm(formula = fml,
               data = train,
               family = "binomial")
summary(fit.logi)

###### Logistic Regression with Bootstrap ######

B = 1000; n = nrow(train)
coef.boot <- matrix(0, B, 10)

set.seed(2023)
for (b in 1:B){
  ind = sample(n, n, T)
  sample = train[ind,]
  fit.boot = glm(formula = fml,
                 data = sample,
                 family = "binomial")
  coef.boot[b,] = fit.boot$coefficients
}

colnames(coef.boot) = c("intecept", xnams)
head(coef.boot)

# SE
getSE = function(v){
  sd(v)/sqrt(length(v))
}
se = apply(coef.boot, 2, getSE)

# Calculate new z-statistics and p-value

coef.table = summary(fit.logi)$coefficients
coef.table[,1] = colMeans(coef.boot)
coef.table[,2] = se
coef.table[,3] = coef.table[,1]/coef.table[,2]
coef.table[,4] = 2*(1-pnorm(coef.table[,3]))
