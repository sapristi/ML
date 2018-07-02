library(here)

source(file="misc_functions.R")
source(file="experiments/learning_curve/learning_curve.R")

train.raw <- read.csv("datasets/titanic_na/train.csv")
test.raw <- read.csv("datasets/titanic_na/test.csv")
train.ff <- read.csv("datasets/titanic_na/train.ff.csv")
train.age_imp <- read.csv("datasets/titanic_na/age_imp.csv")


train_features <- c("Age", "Title", "Parch", "SibSp", "Fare")

# train with factor variables
train.final.factors <- df.make.empty(nrow(train.raw), c("Survived", train_features))
train.final.factors$Survived <- as.factor(train.raw$Survived)
train.final.factors$Age <- as.factor(train.age_imp$Age.factor)
train.final.factors$Title <- train.ff$Title
train.final.factors$Fare <- as.factor(train.ff$Fare)
train.final.factors$Parch <-  as.factor(train.raw$Parch)
train.final.factors$SibSp <-  as.factor(train.raw$SibSp)


# train with continuous variables
train.final.continuous <- df.make.empty(nrow(train.raw), c("Survived", train_features))
train.final.continuous$Survived <- train.raw$Survived
train.final.continuous$Age <- train.age_imp$Age
train.final.continuous$Title <- train.ff$Title
train.final.continuous$Fare <- train.raw$Fare
train.final.continuous$Parch <- train.raw$Parch
train.final.factors$SibSp <-  train.raw$SibSp



# plots
library(ggplot2)
library(RColorBrewer)
plot.empty <- ggplot() + scale_colour_brewer("Set2")
plot.steps <- 20




# rf
library(randomForest)

rf.model_fun <- function(formula, train) {
  randomForest(as.formula(formula), 
               data = train, na.action = na.roughfix, type="classification")
}


rf.plot <- learning_curve.plot(train.selected, test.selected, 
                               target = "Survived", features = train_features,  
                               rf.model_fun, step=plot.steps,
                               title = "random forest", 
                               previous_plot = plot.empty)


# svm logi
svm.logi.model_fun  <- function(formula, train) {
  return(svm(as.formula(formula), 
             data = train, kernel="sigmoid",  cost=50, coef0=1,
             type="C-classification"))
}

svm.logi.predict_fun <- function(model, data) {
  discretize(plogis(predict(model, data)))
}
svm.logi.plot <- learning_curve.plot(train.selected, test.selected, 
                                     target = "SurvivedF", features = features, 
                                     svm.logi.model_fun,  step=plot.steps,
                                     title = "svm sigmoid")