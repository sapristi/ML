setwd("~/bigdata/ML/experiments/svm_tree")

source(file="../multiplot.R")
source(file="measures.R")

library(ggplot2)
library(e1071)


train.raw <- read.csv("../titanic_set/titanic_train.csv")
test.raw <- read.csv("../titanic_set/titanic_test.csv")


features <- c("Pclass", "Sex", "Age", "SibSp", "Parch")
features.string = paste(features, collapse="+")

train.selected <- train.raw[, c(features, "Survived")]
test.selected <- test.raw[, c(features, "Survived")]

train.selected$SurvivedF <- as.factor(train.selected$Survived)
test.selected$SurvivedF <- as.factor(test.selected$Survived)



param.degrees = c(2,3,4,5,6)
param.costs = c(10,100)

params = expand.grid(cost = param.costs, degree = param.degrees)




split <- function(params, data) {
  
  
  res <- data.frame()
  for (i in 1:nrow(params)) {
    #print(params[i,])
    model <- svm(SurvivedF ~ Pclass + Sex + Age + SibSp + Parch, data= train.selected, 
                 kernel = "polynomial", degree = params[i,]$degree, cost = params[i,]$cost)
    
    #print(model)
    
    pred <- predict(model, train.selected)
    
    res[i,1] <- gini_process(pred, train.selected$SurvivedF)
    res[i,2] <- information_gain(pred, train.selected$SurvivedF)
    
    best <- which.max(res[,2])
  }
  
  model <- svm(SurvivedF ~ Pclass + Sex + Age + SibSp + Parch, data= train.selected, 
               kernel = "polynomial", degree = params[best,]$degree, cost = params[best,]$cost)
  
  pred <- predict(model, train.selected)
  train.lclass <- train.selected[pred == 0,]
  train.rclass <- train.selected[pred == 1,]
  
  pred.lclass <- pred[pred == 0]
  pred.rclass <- pred[pred == 0]
  list(model, params[best,], list(train.lclass, train.rclass, pred.lclass, pred.rclass))
}
a<- split(params, train.selected)
