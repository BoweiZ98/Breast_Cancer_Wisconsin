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

logi.pred = predict(fit.logi,
                   newdata = val,
                    type = "response")
logi.pred.y = if_else(logi.pred >= 0.5,
                     1, 
                     0)

# Confusion Matrix
logi_conf_matrix = confusionMatrix(data = as.factor(logi.pred.y),
                                  reference = as.factor(val$Class))
print(logi_conf_matrix) # Accuracy = 0.9714 (0.9285, 0.9922)
