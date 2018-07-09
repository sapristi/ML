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


fe$aux$fare <- function(x) {if (x < 20) {return(0)}; if (x<40) {return(1)}; if (x<100) {return(2)}; return(3)}


fe$aux$age <- function(x) {if (is.na(x)) return(NA); if (x<=5) return(0); if (x <= 16) return(1); 
  if (x<= 50) return(2); return(3) }

fe$aux$deck <- function(cabin_str, pclass) {
  if (pclass == 1) {
    if (cabin_str != "") {
      return(substring(cabin_str,0,1))
    } else  { return("NO") }
  } else { return("NO") }
}


fe$forge_features <- function(df) {
  res <- misc_funs$df.make.empty(nrow(df))
  res$Title <- sapply(df$Name, fe$aux$title)
  res$Fare <- sapply(df$Fare, fe$aux$fare)
  res$Age <- sapply(df$Age, fe$aux$age)
  res$Deck <- mapply(fe$aux$deck, df$Cabin, df$Pclass)
  return(res)
}

