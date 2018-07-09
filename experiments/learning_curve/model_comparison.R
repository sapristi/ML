library(here)

source(file="experiments/learning_curve/learning_curve.R")

library(ggplot2)


train.raw <- read.csv("datasets/titanic/train.csv")
test.raw <- read.csv("datasets/titanic/test.csv")


features <- c("Pclass", "Sex", "Age", "SibSp", "Parch")
features.string = paste(features, collapse="+")

train.selected <- train.raw[, c(features, "Survived")]
test.selected <- test.raw[, c(features, "Survived")]

train.selected$SurvivedF <- as.factor(train.selected$Survived)
test.selected$SurvivedF <- as.factor(test.selected$Survived)


plot.steps  <- 20

families <- c( binomial(link = "logit"),
              gaussian(link = "identity"),
              Gamma(link = "inverse"),
              inverse.gaussian(link = "1/mu^2"),
              poisson(link = "log"))



#########
### logistic regression
#########
logreg.model_fun <- function(formula, train) {
  return(glm(as.formula(formula), data = train, family = binomial(link = "logit")))
}

plot <- learning_curve$plot(train.selected, test.selected, 
                            target = "Survived", features = features,
                            model_fun = logreg.model_fun, predict_fun =  learning_curve$logreg_predict_fun,
                            title = "logistic regression", steps = plot.steps)

#######
### svm linear
#######

library(e1071)


svm.linear.model_fun  <- function(formula, train) {
  return(svm(as.formula(formula), 
             data = train, kernel="linear", cost=50, coef0=1,
             type="C-classification"))
}

plot <- learning_curve$plot(train.selected, test.selected, 
                                       target = "SurvivedF", features = features, 
                                       svm.linear.model_fun, steps=plot.steps,
                                       title = "svm linear", previous_plot = plot)

#######
### svm logistic
#######


svm.logi.model_fun  <- function(formula, train) {
  return(svm(as.formula(formula), 
             data = train, kernel="sigmoid",  cost=50, coef0=1,
             type="C-classification"))
}

plot <- learning_curve$plot(train.selected, test.selected, 
                                     target = "SurvivedF", features = features, 
                    svm.logi.model_fun,  steps=plot.steps,
                    title = "svm sigmoid", previous_plot = plot)

#######
### svm polynomial
#######


svm.poly.model_fun  <- function(formula, train) {
  return(svm(as.formula(formula), 
             data = train, kernel="polynomial", degree=3, cost=50, coef0=1,
             type="C-classification"))
}

plot <- learning_curve$plot(train.selected, test.selected, 
                                     target = "SurvivedF", features = features,  
                                     svm.poly.model_fun, steps=plot.steps,
                    title = "svm polynomial", previous_plot = plot)


########
### random forest
#######

library(randomForest)

rf.model_fun <- function(formula, train) {
  randomForest(as.formula(formula), 
               data = train, na.action = na.roughfix, type="classification")
}

plot <- learning_curve$plot(train.selected, test.selected, 
                               target = "SurvivedF", features = features,  
                               rf.model_fun, steps=plot.steps,
                               title = "random forest", previous_plot = plot)


###################"
### plots

# multiplot(logreg.plot, svm.linear.plot, svm.logi.plot, svm.poly.plot, rf.plot, cols=2)

# rf.plot <- rf.plot + labs(x = "training set size", y = "accuracy")

print(plot)
ggsave("experiments/learning_curve/model comparison.png", plot = last_plot(), device = "png",
       scale = 1, width = 30, height = 20, units =  "cm",
       dpi = 300)

