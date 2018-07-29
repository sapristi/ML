library(here)
source(file="experiments/learning_curve/learning_curve.R")


library(ggplot2)



train.raw <- read.csv("datasets/titanic_na/train.csv", 
                      colClasses = c("Sex" = "factor", "Embarked" = "factor"))
features <- c("Pclass", "Sex", "Age", "SibSp", "Parch", "Fare")



#### aux functions
# library(purrr)
discretize <- function(data, threshold=0.5) {
  res <- lapply(data, function(e) {if (e < threshold) {return(0)} else {return(1)}});
  return(res);
}

###### xgboost
####
library(xgboost)

xgb.model_fun <- function(train, target, features) {
  train.data <-  data.matrix(train[,  features])
  xgboost(data = train.data, label = train[[target]], nrounds = 20, verbose = 0,
          objective = "binary:logistic",eval.metric = "logloss")
}

xgb.predict_fun  <- function(model, data, target, features) {
  test.data <-  data.matrix(data[,  features])
  learning_curve$discretize(predict(model, as.matrix(test.data)))
  }




plot <- learning_curve$plot.advanced(train.raw, 0.1, 
                            target = "Survived", features = features,
                            model_fun = xgb.model_fun, predict_fun = xgb.predict_fun, variations = 1,
                            title = "xgboost", step = 20)

print(plot)

#########
### logistic regression
#########

families <- c( binomial(link = "logit"),
               gaussian(link = "identity"),
               Gamma(link = "inverse"),
               inverse.gaussian(link = "1/mu^2"),
               poisson(link = "log"))


logreg.model_fun <- function(train, target, features) {
  formula <- paste(target, paste(features, collapse = "+"), sep="~")  
  return(glm(as.formula(formula), data = train, family = binomial(link = "logit")))
}
logreg.predict_fun <- function(model, data,target, features) {
  plogis(predict(model, data))
}

plot <- learning_curve$plot.advanced(train.raw, 0.1, 
                    target = "Survived", features = features,
                    logreg.model_fun, logreg.predict_fun,
                    title = "logistic regression", step = 20, previous_plot = plot)

print(plot)
#######
### svm linear
#######


library(e1071)


svm.linear.model_fun  <- function(train, target, features) {
  formula <- paste(target, paste(features, collapse = "+"), sep="~")  
  return(svm(as.formula(formula), 
             data = train, kernel="linear", cost=50, coef0=1,
             type="C-classification"))
}

plot <- learning_curve$plot.advanced(train.raw, 0.1, 
                                       target = "Survived", features = features, 
                                       svm.linear.model_fun, step=20,
                                        title = "svm linear", previous_plot = plot)
print(plot)
#######
### svm logistic
#######


svm.logi.model_fun  <- function(train, target, features) {
  formula <- paste(target, paste(features, collapse = "+"), sep="~")  
  return(svm(as.formula(formula), 
             data = train, kernel="sigmoid",  cost=50, coef0=1,
             type="C-classification"))
}

svm.logi.predict_fun <- function(model, data) {
  plogis(predict(model, data))
}
plot <- learning_curve$plot.advanced(train.raw, 0.1, 
                                     target = "Survived", features = features, 
                    svm.logi.model_fun,  step=20,
                    title = "svm sigmoid", previous_plot = plot)

print(plot)
#######
### svm polynomial
#######


svm.poly.model_fun  <- function(train, target, features) {
  formula <- paste(target, paste(features, collapse = "+"), sep="~") 
  return(svm(as.formula(formula), 
             data = train, kernel="polynomial", degree=3, cost=50, coef0=1,
             type="C-classification"))
}

plot <- learning_curve$plot.advanced(train.raw, 0.1, 
                                     target = "Survived", features = features,  
                                     svm.poly.model_fun, step=20,
                    title = "svm polynomial", previous_plot = plot)


print(plot)
########
### random forest
#######

library(randomForest)

rf.model_fun <- function(train, target, features) {
  formula <- paste(target, paste(features, collapse = "+"), sep="~") 
  randomForest(as.formula(formula), 
               data = train, na.action = na.roughfix, type="classification")
}

plot <- learning_curve$plot.advanced(train.raw, 0.1, 
                               target = "Survived", features = features,  
                               rf.model_fun, step=20,
                               title = "random forest", previous_plot = plot)



print(plot)
