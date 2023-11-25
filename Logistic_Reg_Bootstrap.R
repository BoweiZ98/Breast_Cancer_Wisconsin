###### Setting Up ######
Sys.setLanguage(lang = "en")
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


log_reg = function(data, indices){
  sample_data <- data[indices, ]
  model <- glm(fml, data = sample_data, family = binomial)
  return(coef(model))
}

bootstrap_res = boot(data = train, statistic = log_reg, R = 1000)

# Obtain CI for coefficients
boot_ci = boot.ci(bootstrap_res, type = "bca")
print(boot_ci)

# new

sample_coef_intercept = NULL
sample_coef_x = NULL
set.seed(2023)
for (i in 1:1000){
  n = nrow(train)
  ind = sample(n, n, T)
  sample = train[ind,]
  fit = glm(formula = fml,
            data = sample,
            family = "binomial")
  sample_coef_intercept = c(sample_coef_intercept, fit$coefficients[1])
  sample_coef_x = rbind(sample_coef_x, fit$coefficients[-1])
}

colMeans(sample_coef_x)
