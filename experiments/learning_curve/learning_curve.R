
library(ggplot2)
library(SDMTools)


# plots the learning curve of a model over selected data
# parameters : 
#   - train : training data.frame
#   - test :  test data.frame
#   - target : column name of target variable in train data.frame
#   - features : vector containing the names of the columns to use as features for prediction
#   - model_fun : function(formula, train) -> returns the model 
#            e.g : logreg.model_fun <- function(formula, train) {
#                       return(glm(as.formula(formula), data = train, family = binomial(link = "logit")))
#                  }
#               where formula is a string representation of the formula to use
#   - predict_fun : function(model, data)  ->  {predict(model, data)}
#   - steps : number of learning points to calculate
#   - limit : limit to the number of data points to use
#   - randomize : randomizes the input rows (to implement)
#   - previous_plot : previous plot to add new curves to
#
#   to_implement:
#      - randomize : randomizes training rows (in case training data is sorted)
#

learning_curve <- list()

learning_curve$default_predict_fun = function(model, data) {predict(model, data)}


# returns a data.frame containing the columns : 
#   - V1 : number of rows used in model training
#   - V2 : score of prediction over training set
#   - V3 : score of predictio over test set
learning_curve$make_data_points <- function(train, test, target, features, model_fun, 
                                            predict_fun, steps, limit)
  {
    res <- data.frame()
    c <- 1
#    for (i in seq(10, min(nrow(train),limit), by=steps)) {
    for (i in seq(from=10, to= min(nrow(train),limit), length.out=steps)) {
      train.sample <- train[sample(nrow(train), i), ]
      
      formula <- paste(target, paste(features, collapse = "+"), sep="~")
      
      model <- model_fun(formula, train.sample)
      
      predicted.train <- predict_fun(model, train.sample)
      predicted.test <- predict_fun(model, test)
      
      res[c,1] <- i
      res[c, 2] <- accuracy(predicted.train, train.sample[[target]])$prop.correct
      res[c, 3] <- accuracy(predicted.test, test[[target]])$prop.correct
      c <- c+1
    }
    return(res)
  }

learning_curve$plot <- function(train, test, target, features, model_fun, 
                             predict_fun = learning_curve$default_predict_fun, 
                             steps = 10, limit=500, title="", previous_plot=ggplot())
{
  plot.data <- learning_curve$make_data_points(train, test, target, features, model_fun, 
                                               predict_fun, steps, limit)
  g <- previous_plot
  g <- g + geom_line(data=plot.data, aes_(x=~V1, y=~V2, col=title))
  g <- g + geom_line(data=plot.data, aes_(x=~V1, y=~V3, col=title))
  return(g)
}

  
  
# 
# learning_curve.plot <- function(train, test, target, 
#                                 features, model_fun, 
#                                 predict_fun = default_predict_fun, 
#                                 steps = 5, limit=500, title="")
#   
#   {
#   res <- data.frame()
#   c <- 1
#   for (i in seq(10, min(nrow(train),limit), by=steps)) {
#     train.sample <- train[sample(nrow(train), i), ]
#     
#     formula <- paste(target, paste(features, collapse = "+"), sep="~")
#     
#     model <- model_fun(formula, train.sample)
#     
#     predicted.train <- predict_fun(model, train.sample)
#     predicted.test <- predict_fun(model, test)
#     
#     
#     res[c,1] <- i
#     res[c, 2] <- accuracy(predicted.train, train.sample[[target]])$prop.correct
#     res[c, 3] <- accuracy(predicted.test, test[[target]])$prop.correct
#     #res[c, 2] <- accuracy.bin(train.sample[[target_name]], predicted.train)
#     #res[c, 3] <- accuracy.bin(test[[target_name]], predicted.test)
#     c <- c+1
#   }
#   print(summary(model))
#   
#   g <- ggplot(data=res, aes(x=V1, y=V2, color="train"))
#   g <- g + geom_line()
#   g <- g + geom_line(aes(x=V1, y=V3, color = "test"))
#   g <- g + labs(x = "training set size", y = "accuracy", colour="data set", title =title)
#   return(g)
# }
# 
# learning_curve.plot2 <- function(train, test, target, 
#                                 features, model_fun, 
#                                 predict_fun = default_predict_fun, 
#                                 steps = 5, limit=500, title="", previous_plot=ggplot())
# {
#   res <- data.frame()
#   c <- 1
#   for (i in seq(10, min(nrow(train),limit), by=steps)) {
#     train.sample <- train[sample(nrow(train), i), ]
#     
#     formula <- paste(target, paste(features, collapse = "+"), sep="~")
#     
#     model <- model_fun(formula, train.sample)
#     
#     predicted.train <- predict_fun(model, train.sample)
#     predicted.test <- predict_fun(model, test)
#     
#     
#     res[c,1] <- i
#     res[c, 2] <- accuracy(predicted.train, train.sample[[target]])$prop.correct
#     res[c, 3] <- accuracy(predicted.test, test[[target]])$prop.correct
#     #res[c, 2] <- accuracy.bin(train.sample[[target_name]], predicted.train)
#     #res[c, 3] <- accuracy.bin(test[[target_name]], predicted.test)
#     c <- c+1
#   }
#   
#   print(summary(model))
#   g <- previous_plot
#   g <- g + geom_line(data=res, aes_(x=~V1, y=~V2, col=title))#, color=title))
#   g <- g + geom_line(data=res, aes_(x=~V1, y=~V3, col=title)) #, color=title))
# #  g <- g + labs(x = "training set size", y = "accuracy", colour="data set", title =title)
#   return(g)
# }