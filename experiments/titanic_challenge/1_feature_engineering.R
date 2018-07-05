library(here)
source(file="misc_functions.R")

fe <- list()
fe$auxfunctions <- list()

fe$auxfunctions$title <- function(name) {
  title <- sub(".*?, (\\w*).*", "\\1", name)
  
  if (title %in% c("Mlle", "Ms") ) {
    return("Miss")
  }
  if (title == "Mme") {return("Mrs");}
  
  if (title %in% c("Master", "Miss", "Mrs", "Mr")) {return(title);}
  
  # if something else : 
  return("Special")
}


fe$auxfunctions$fare <- function(x) {if (x < 20) {return(0)}; if (x<40) {return(1)}; if (x<100) {return(2)}; return(3)}


fe$auxfunctions$age <- function(x) {if (is.na(x)) return(NA); if (x<=5) return(0); if (x <= 16) return(1); 
  if (x<= 50) return(2); return(3) }

fe$auxfunctions$deck <- function(cabin_str, pclass) {
  if (pclass == 1) {
    if (cabin_str != "") {
      return(substring(cabin_str,0,1))
    } else  { return("NO") }
  } else { return("NO") }
}


fe$forge_features <- function(df) {
  res <- aux_fun$df.make.empty(nrow(df))
  res$Title <- sapply(df$Name, make_title)
  res$Fare <- sapply(df$Fare, simp_fare)
  res$Age <- sapply(df$Age, simp_age)
  res$Cabin <- sapply(df$Cabin, simp_cabin_deck)
  return(res)
}

