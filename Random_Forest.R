###### Setting Up ######
Sys.setLanguage(lang = "en")
library(MASS)
library(class)
library(tidyverse)
library(caret)
library(car)
library(tree)
library(randomForest)

train = read.csv("datasets/train.csv")
val = read.csv("datasets/val.csv")
xnams = dput(colnames(train)[colnames(train) != "Class"])

###### Random Forest ######
fit.rf = randomForest(Class ~ .,
                  data = train,
                  mtry = 3,
                  importance = TRUE)
fit.rf

###### Apply to Validation ######
rf.pred = if_else(
  predict(fit.rf, newdata = val, type = "response") >= 0.5,
  1,
  0
)

###### Confusion Matrix ######
rf_conf_matrix = confusionMatrix(data = as.factor(rf.pred),
                                  reference = as.factor(val[,"Class"]))
print(rf_conf_matrix) #0.9786 (0.9387, 0.9956)
