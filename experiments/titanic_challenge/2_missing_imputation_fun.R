
library(mice)

imp <- list()

imp$age <- function(df) {
  

  imp.pmm <- mice(df, m=10, method='pmm', printFlag=FALSE, 
                   formulas = list(Age.simp = Age.simp ~ SibSp + Parch + Sex + Fare + Title))
  
  complete
}