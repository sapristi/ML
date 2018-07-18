
library(ggplot2)
library(SDMTools)



learning_curve <- list()

#' default prediction function : applies the model to the data
learning_curve$default_predict_fun <- function(model, data) {predict(model, data)}


#' function to discretize output of logistic models
#' but we don't really need it since accuracy takes likelihood as input
learning_curve$discretize <- function(data, threshold=0.5) {
  res <- sapply(data, function(e) {if (e < threshold) {return(0)} else {return(1)}});
  return(res);
}

learning_curve$logreg_predict_fun <- function(model, data) 
  {plogis(predict(model, data))}


#' auxilary function used by plot
#' Look at plot documentation for arg description
#' @return a data.frame containing the columns : 
#'   - V1 : number of rows used in model training
#'   - V2 : score of prediction over training set
#'   - V3 : score of predictio over test set
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
      
      if (class(train.sample[[target]]) == "factor") {
        res[c,2] <- sum(predicted.train == train.sample[[target]])/length(train.sample[[target]])
        res[c,3] <- sum(predicted.test == test[[target]])/length(test[[target]])
      } else {
        res[c, 2] <- accuracy(train.sample[[target]], predicted.train)$prop.correct
        res[c, 3] <- accuracy(test[[target]], predicted.test)$prop.correct
      }
      c <- c+1
    }
    return(res)
  }

#' plots the learning curve of a model over selected data
#' parameters : 
#' @param train : training data.frame
#' @param test :  model testing parameter. Can be :
#'            - a data.frame on which to perform the tests
#'            - a string parameter : 
#' @param target : column name of target variable in train data.frame
#' @param features : vector containing the names of the columns to use as features for prediction
#' @param model_fun : function(formula, train) -> returns the model 
#'            e.g : logreg.model_fun <- function(formula, train) {
#'                       return(glm(as.formula(formula), data = train, family = binomial(link = "logit")))
#'                  }
#'               where formula is a string representation of the formula to use
#' @param predict_fun : function(model, data)  ->  {predict(model, data)}
#' @param steps : number of learning points to calculate
#' @param limit : limit to the number of data points to use
#' @param title : legend associated with the curves
#' @param previous_plot : ggplot object to add the new curves to
#'                     if no parameter is provided, the function will 
#'                     create a new empty ggplot object
#'
#' @return a ggplot object ready to plot
#'   to_implement:
#'      - randomize : randomizes training rows (in case training data is sorted)
#'      - other testing parameters
#'
learning_curve$plot <- function(train, test, target, features, model_fun, 
                                predict_fun = NULL, 
                                steps = 10, limit=500, title="", previous_plot = NULL)
{
  
  if (is.null(predict_fun)) {predict_fun <- learning_curve$default_predict_fun}
  
  
  switch(class(test),
         data.frame={print("test data.frame supplied"); 
           train.df <- train;
           test.df <- test},
         numeric = {
           if (0 < test & test < 1) {
             sprintf("valid numeric test parameter supplied : %f", test);
             n <- nrow(train);
             split_limit <- as.integer(n * test);
             sprintf("splitting data set at row %d", split_limit)
             test.df <- train[0:(split_limit-1),];
             train.df <- train[split_limit:(n-1),]
           }
         },
         {error("can not interpret test parameter ")})
  
  plot.data <- learning_curve$make_data_points(train.df, test.df, target, features, model_fun, 
                                               predict_fun, steps, limit)
  
  if (is.null(previous_plot)) {
    g <- ggplot() + labs(x = "training set size", y = "accuracy")
  } else {g <- previous_plot}
  
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
#   g <- g + labs(x = "training set size", y = "accuracy", colour="data set", title =title)
#   return(g)
# }