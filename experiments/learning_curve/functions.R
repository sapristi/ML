
library(ggplot2)
library(SDMTools)

default_predict_fun = function(model, data) {predict(model, data)}

learning_curve.plot <- function(train, test, target, 
                                features, model_fun, 
                                predict_fun = default_predict_fun, 
                                step = 5, limit=500, title="") 
  {
  res <- data.frame()
  c <- 1
  for (i in seq(10, min(nrow(train),limit), by=step)) {
    train.sample <- train[sample(nrow(train), i), ]
    
    formula <- paste(target, paste(features, collapse = "+"), sep="~")
    
    model <- model_fun(formula, train.sample)
    
    predicted.train <- predict_fun(model, train.sample)
    predicted.test <- predict_fun(model, test)
    
    
    res[c,1] <- i
    res[c, 2] <- accuracy(predicted.train, train.sample[[target]])$prop.correct
    res[c, 3] <- accuracy(predicted.test, test[[target]])$prop.correct
    #res[c, 2] <- accuracy.bin(train.sample[[target_name]], predicted.train)
    #res[c, 3] <- accuracy.bin(test[[target_name]], predicted.test)
    c <- c+1
  }
  print(summary(res))
  g <- ggplot(data=res, aes(x=V1, y=V2, color="train"))
  g <- g + geom_line()
  g <- g + geom_line(aes(x=V1, y=V3, color = "test"))
  g <- g + labs(x = "training set size", y = "accuracy", colour="data set", title =title)
  return(g)
}
