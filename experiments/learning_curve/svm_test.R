setwd("~/bigdata/ML/experiments/learning_curve")

source(file="functions.R")
source(file="../multiplot.R")


library(ggplot2)
library(SDMTools)

train.raw <- read.csv("../titanic_set/titanic_train.csv")
test.raw <- read.csv("../titanic_set/titanic_test.csv")


features <- c("Pclass", "Sex", "Age", "SibSp", "Parch")

train.selected <- train.raw[, c(features, "Survived")]
test.selected <- test.raw[, c(features, "Survived")]

train2 <- train.selected
train2$Survived <- as.factor(train2$Survived)

m1 <- svm(Survived ~ ., data = train.selected, kernel = "linear")
m2 <- svm(Survived ~ ., data = train2, kernel = "linear")


discretize <- function(data, threshold=0.5) {
  res <- map_dbl(data, function(e) {if (e < threshold) {return(0)} else {return(1)}});
  return(res);
}
p1 <- discretize(predict(m1, test.selected))
p2 <- predict(m2, test.selected)

accuracy(p2, test.selected$Survived)
accuracy(p1, test.selected$Survived, threshold = 0.5)
