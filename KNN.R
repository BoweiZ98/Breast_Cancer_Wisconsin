###### Setting Up ######
Sys.setLanguage(lang = "en")
library(MASS)
library(class)
library(tidyverse)
library(caret)
library(car)

Train = read.csv("datasets/train.csv")
Val = read.csv("datasets/val.csv")
xnams = dput(colnames(Train)[colnames(Train) != "Class"])

###### KNN ######

#### Find Optimal K ####
# 5 Folds
set.seed(1)
folds = createFolds(Train[,"Class"], k = 5, list = T, returnTrain = F)
table.accuracy = data.frame(k = 1:10)
for (k in 1:10){
  
  accu = 0
  for(f in 1:5){
    # split test/val
    ind = folds[[f]]
    train = Train[-ind,]
    val = Train[ind,]
    
    # run knn.cv
    knn.pred = knn(train = train[,xnams],
                   test = val[,xnams],
                   cl = train[,"Class"],
                   k = k)
    curr_accu = mean(knn.pred == val[,"Class"])
    accu = accu + curr_accu
  }
  
  table.accuracy$accuracy[table.accuracy$k == k] = accu/5
  
}

table.accuracy # k = 5

# plot accuracy vs. k
with(table.accuracy, plot(k, accuracy, type = 'l',
                          main = "Accuracy vs. k with CV5"))

#### Apply to Validation ####

knn.5 = knn(train = Train[,xnams],
            test = Val[,xnams],
            cl = Train[, "Class"],
            k = 5)
mean(knn.5 == Val[,"Class"])

# Confusion Matrix
knn5_conf_matrix = confusionMatrix(data = as.factor(knn.5),
                                   reference = as.factor(Val[,"Class"]))
print(knn5_conf_matrix) # Accuracy: 0.9786 (0.9387, 0.9956)

