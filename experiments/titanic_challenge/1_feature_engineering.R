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

fe$aux$fare.NA <- function(x) {if (is.na(x) | x==0) {return(NA)} else {return(x)}}
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


fe$calculate_real_fare <- function(ticket, fare, ticket_vector) {
  nb <- sum(ticket_vector == ticket)
#  print(sprintf("%d tickets numbered %s ticket, in %d total tickets %s", nb, ticket, length(ticket_vector), class(ticket_vector)))
  real_fare <- fare / nb
  return(real_fare)
}


fe$forge_features <- function(df, more_tickets = c()) {
  
  res <- misc_funs$df.make.empty(nrow(df))
  
  res$PassengerId <- df$PassengerId
  
  res$Age <- sapply(df$Age, as.integer)
  res$Age.simple <- as.factor(sapply(df$Age, fe$aux$age.simple))
#  res$Age.disc3 <- sapply(df$Age, fe$aux$age.discrete3)
  res$Age.disc5 <- sapply(df$Age, fe$aux$age.discrete5)
  
  res$Title <- as.factor(sapply(df$Name, fe$aux$title))
  
  res$Fare.old <- df$Fare
  tickets = c(as.character(df$Ticket), as.character(more_tickets))
  res$Fare <- mapply(fe$calculate_real_fare, df$Ticket, df$Fare, MoreArgs = list(ticket_vector = tickets))
  
  res$Fare <- sapply(res$Fare, fe$aux$fare.NA)
  res$Fare.sqrt <- sapply(res$Fare, fe$aux$fare.sqrt)
  res$Fare.log <- sapply(res$Fare, fe$aux$fare.log)
  
  #res$Deck <- mapply(fe$aux$deck, df$Cabin, df$Pclass)
  
  
  return(res)
}

fe$forge_features_add <- function(df,  more_tickets = c()) {
  res <- data.frame(df)
  ff <- fe$forge_features(df, more_tickets)
  
  for (f in c("Age", "Age.simple", "Age.disc5", "Title", "Fare.old", "Fare", "Fare.sqrt", "Fare.log")) {
    res[[f]] <- ff[[f]]
  }
  return(res)
}
  

