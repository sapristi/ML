library(here)
source(file="misc_functions.R")

fe <- list()
fe$auxfunctions <- list()

fe$aux$title <- function(name) {
  title <- sub(".*?, (\\w*).*", "\\1", name)
  
  if (title %in% c("Mlle", "Ms") ) {
    return("Miss")
  }
  if (title == "Mme") {return("Mrs");}
  
  if (title %in% c("Master", "Miss", "Mrs", "Mr")) {return(title);}
  
  # if something else : 
  return("Special")
}

fe$aux$fare.NA <- function(x) {if (x==0) {return(NA)} else {return(x)}}
fe$aux$fare.sqrt <- function(x) {return(as.integer(sqrt(x)/3))}
fe$aux$fare.log <- function(x) {return(as.integer(log(x)))}
fe$aux$fare <- function(x) {if (x < 20) {return(0)}; if (x<40) {return(1)}; if (x<100) {return(2)}; return(3)}

fe$aux$age.simple <- function(x) {
  if (is.na(x)) return(NA);
  if (x <= 15) return("youth")
  if (x <= 51) return("adult")
  return("old")
}
fe$aux$age.discrete3 <- function(x) {
  return(as.integer(x / 3))
}
fe$aux$age.discrete5 <- function(x) {
  return(as.integer(x / 5))
}


fe$aux$deck <- function(cabin_str, pclass) {
  if (pclass == 1) {
    if (cabin_str != "") {
      return(substring(cabin_str,0,1))
    } else  { return("NO") }
  } else { return("NO") }
}


fe$forge_features <- function(df) {
  res <- misc_funs$df.make.empty(nrow(df))
  
  res$Title <- as.factor(sapply(df$Name, fe$aux$title))
  
  res$Fare <- sapply(df$Fare, fe$aux$fare.NA)
  res$Fare.sqrt <- sapply(res$Fare, fe$aux$fare.sqrt)
  res$Fare.log <- sapply(res$Fare, fe$aux$fare.log)
  
  res$Age <- sapply(df$Age, as.integer)
  res$Age.simple <- sapply(df$Age, fe$aux$age.simple)
  res$Age.disc3 <- sapply(df$Age, fe$aux$age.discrete3)
  res$Age.disc5 <- sapply(df$Age, fe$aux$age.discrete5)
  
  res$Deck <- mapply(fe$aux$deck, df$Cabin, df$Pclass)
  return(res)
}

fe$forge_features_add <- function(df) {
  res <- data.frame(df)
  
  res$Title <- as.factor(sapply(df$Name, fe$aux$title))
  
  res$Fare <- sapply(df$Fare, fe$aux$fare.NA)
  res$Fare.sqrt <- sapply(res$Fare, fe$aux$fare.sqrt)
  res$Fare.log <- sapply(res$Fare, fe$aux$fare.log)
  
  res$Age <- sapply(df$Age, as.integer)
  res$Age.simple <- as.factor(sapply(df$Age, fe$aux$age.simple))
  res$Age.disc3 <- sapply(df$Age, fe$aux$age.discrete3)
  res$Age.disc5 <- sapply(df$Age, fe$aux$age.discrete5)
  
  res$Deck <- mapply(fe$aux$deck, df$Cabin, df$Pclass)
  return(res)
}
  
  
  
  

