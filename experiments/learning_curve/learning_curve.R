
library(ggplot2)
library(SDMTools)
library(Hmisc)


learning_curve <- list()

#' default prediction function : applies the model to the data
learning_curve$default_predict_fun <- function(model, data, target, features) {predict(model, data)}


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
learning_curve$make_data_points <- function(train, test, target, features, 
                                            model_fun, predict_fun,
                                            steps, limit)
{
  res <- data.frame()
  if (is.null(limit)) {limit <- nrow(train)}
  
  c <- 1
  for (i in seq(from=20, to= min(nrow(train),limit), length.out=steps)) {
    sampled_rows <- sample(nrow(train), i)
    train.sample <- train[sampled_rows, ]
    
    model <- model_fun(train.sample, target, features)
    
    predicted.train <- predict_fun(model, train.sample, target, features)
    predicted.test <- predict_fun(model, test, target, features)
    res[c,1] <- i
    
    if (class(predicted.train) == "factor") {
#      print(sprintf("pred.train %d, train.target, %d", length(predicted.train), length(train.sample[[target]])))
      
#      res[c,2] <- sum(predicted.train == train.sample[[target]])/length(train.sample[[target]])
      res[c,2] <- sum(predicted.train == train.sample[names(predicted.train), c(target)])/length(train.sample[[target]])
      
#      print(sprintf("pred.test %d, test.target, %d", length(predicted.test), length(test[[target]])))
      res[c,3] <- sum(predicted.test == test[names(predicted.test), c(target)])/length(test[[target]])
      
      #print(sprintf("%d NA in train prediction, %d NA in test prediction", sum(is.na(res[c,2])), sum(is.na(res[c,3]))))
      
       #print(sprintf("pred : %f %f", res[c,2], res[c,3]))
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
learning_curve$plot.simple <- function(train, test, target, features, 
                                       model_fun, predict_fun = NULL,
                                       steps = 10, limit=NULL, title="", previous_plot = NULL)
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
    g <- ggplot()
  } else {g <- previous_plot}
  
  g <- g + geom_line(data=plot.data, aes_(x=~V1, y=~V2, col=title, fill=title))
  g <- g + geom_line(data=plot.data, aes_(x=~V1, y=~V3, col=title, fill=title))
  
  return(g)
}

learning_curve$plot.advanced <- 
  function(train, test, target, features, 
           model_fun, predict_fun = NULL,
           steps = 10, limit=NULL, 
           variations = 5, shuffle = TRUE, 
           title="", previous_plot = NULL)
    #                 smoothing = TRUE, random.seed = NULL
  {
    
    if (is.null(predict_fun)) {predict_fun <- learning_curve$default_predict_fun}
    
    ###      test and train row selection for the different variations
    ###########
    switch(class(test),
           data.frame={print("test data.frame supplied"); 
             error("broken feature; do not use; call support for help; or fix it yourself")
             train.df <- train;
             test.df <- test},
           numeric = {
             if (0 < test & test < 1) {
               #print(sprintf("%f of the train data will be used as test set for each variation ", test*100))
               n <- nrow(train);
               
               train_rows = list()
               test_rows = list()
               test_size <- as.integer(n * test)
               all_rows = seq(from=1, to=nrow(train))
               print(sprintf("%d rows, %d for training", nrow(train), test_size))
               for (i in 1:variations) {
                 if (shuffle) { rows <- all_rows[sample(nrow(train))] }
                 else { rows <- all_rows}
                 
                 split_limit <- (i -1) * floor( (nrow(train) * (1-test)) / variations)
                 
                 # print(sprintf("splitting data set at >= %d < %d", split_limit, split_limit + test_size))
                 test_rows_ind <- (all_rows >= split_limit & all_rows < split_limit + test_size)
                 
                 train_rows[[i]] <- rows[!test_rows_ind]
                 test_rows[[i]] <- rows[test_rows_ind]
               }
               
             } else {
               error("invalid numeric test argument")
             }
           },
           {error("can not interpret test parameter ")})
    
    
    ### plots construction
    #############
    plot.data <- data.frame()
    
    for (i in 1:variations) {
      plot.data.temp <- learning_curve$make_data_points(train[train_rows[[i]],], train[test_rows[[i]],], target, features, 
                                                        model_fun, predict_fun, steps, limit)
      plot.data <- rbind(plot.data, plot.data.temp)
    }
    
#    print(plot.data)
#    print(max(plot.data$V2))
#    print(max(plot.data$V3))
    
    if (is.null(previous_plot)) {
      g <- ggplot()
    } else {g <- previous_plot}
    
    # if (smoothing) {
    # g <- g + stat_smooth(data=plot.data, aes_(x=~V1, y=~V2, col=title, fill=title), 
    #                      span=smoothing_span, method="gam", formula = y~x)
    # g <- g + stat_smooth(data=plot.data, aes_(x=~V1, y=~V3, col=title, fill=title), 
    #                      span=smoothing_span, method="gam", formula = y~x)
    # } else ...
    if (variations > 1) {
      
      g <- g   + 
        stat_summary_bin(data=plot.data, aes_(x=~V1, y=~V2, color=title), 
                         fun.data=mean_cl_normal, fun.args=list(conf.int=0.5), alpha=0.5) +
        #scale_fill_hue(c=40,l=80) +
        stat_summary_bin(data=plot.data, aes_(x=~V1, y=~V2, color=title), geom="line", fun.y=mean, linetype="solid")
      
      
      g <- g   + 
        stat_summary_bin(data=plot.data, aes_(x=~V1, y=~V3, color=title), 
                         fun.data=mean_cl_normal, fun.args=list(conf.int=0.5), alpha=0.5)  +
        stat_summary_bin(data=plot.data, aes_(x=~V1, y=~V3, color=title), geom="line", fun.y=mean, linetype="solid")
     
    } else {
      g <- g + geom_line(data=plot.data, aes_(x=~V1, y=~V2, col=title))
      g <- g + geom_line(data=plot.data, aes_(x=~V1, y=~V3, col=title))
    }
    return(g)
  }


learning_curve$make_decor <- function(g, title=NULL, ymin=NULL) {
  g <- g + labs(x = "training set size", y = "accuracy")
  g <- g + theme(legend.position="bottom", legend.box = "horizontal")
  if (!is.null(title)) {
    g <- g + ggtitle(title)  
  }
  if (!is.null(ymin)) {
    g <- g + ylim(ymin, 1)
  }
  return(g)
}

