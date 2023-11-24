###### Setting Up ######
Sys.setLanguage(lang = "en")
library(MASS)
library(class)
library(tidyverse)
library(caret)
library(car)

train = read.csv("datasets/train.csv")
val = read.csv("datasets/val.csv")
xnams = dput(colnames(train)[colnames(train) != "Class"])

###### LDA fit ######
lda.fit = lda(Class ~ .,
              data = train)
lda.pred = predict(lda.fit,
                   newdata = val)
lda.pred.y = if_else(lda.pred$posterior[,2] >= 0.5,
                     1, 
                     0)
# Confusion Matrix
lda_conf_matrix = confusionMatrix(data = as.factor(lda.pred.y),
                                  reference = as.factor(val$Class))
print(lda_conf_matrix) # Accuracy = 0.9643 (0.9186, 0.9883)

###### QDA fit ######
qda.fit = qda(Class ~ .,
              data = train)
qda.pred = predict(qda.fit,
                   newdata = val)
qda.pred.y = if_else(qda.pred$posterior[,2] >= 0.5,
                     1, 
                     0)
# Confusion Matrix
qda_conf_matrix = confusionMatrix(data = as.factor(qda.pred.y),
                                  reference = as.factor(val$Class))
print(qda_conf_matrix) # Accuracy 0.9571 (0.9091, 0.9841)


