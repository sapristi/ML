
library(mice)

imp <- list()

imp$age <- function(df) {
  

  imp.pmm <- mice(df, m=10, method='pmm', printFlag=FALSE, 
                   formulas = list(Age = Age ~ SibSp + Parch + Sex + Fare + Title))
  
  complete
}