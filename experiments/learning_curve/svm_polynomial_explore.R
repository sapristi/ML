setwd("~/bigdata/ML/experiments/learning_curve")

source(file="learning_curve.R")
source(file="../multiplot.R")


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




param.degrees = c(2,3,4)
param.costs = c(0.1,1,10,100)

plots = list()
i = 1
for (deg in param.degrees) {
  for (cost in param.costs) {
    
    
    svm.poly.model_fun  <- function(formula, train) {
      return(svm(as.formula(formula), 
                 data = train, kernel="polynomial", degree=deg, cost=cost, coef0=1,
                 type="C-classification"))
    }
    
    svm.poly.plot <- learning_curve.plot(train.selected, test.selected, 
                                         target = "SurvivedF", features = features,  
                                         svm.poly.model_fun, step=20,
                                         title = paste("svm polynomial", 
                                                       "deg=", toString(deg), 
                                                       "cost=", toString(cost), sep=" "))
    plots[[i]] <- svm.poly.plot
    i <- i+1
  }
}

multiplot(plotlist = plots, cols=3)

