library(here)
source(file="misc_functions.R")

train.raw <- read.csv("datasets/titanic_na/train.csv")
train.raw$Survived <- as.factor(train.raw$Survived)
summary(train.raw)



# feature creation

cols.notna <- !is.na(train.raw$Age)
train.notna <- train.raw[cols.notna,]
library(lattice)


##  simple age
plot(density(train.notna$Age))
densityplot(~Age| Survived + Sex, data = train.notna)
histogram(~Age| Survived + Sex, data = train.notna, type="count", nint=20)
train.notna$Age.disc1 <- sapply(train.notna$Age, function(age){return(as.integer(age / 1))})
train.notna$Age.disc2 <- sapply(train.notna$Age, function(age){return(as.integer(age / 2))})
train.notna$Age.disc4 <- sapply(train.notna$Age, function(age){return(as.integer(age / 4))})


histogram(~Age.disc|Survived + Sex, data = train.notna, type="count")
histogram(~Survived | Age.disc1 + Sex , data = train.notna)


simp_age <- function(x) {if (is.na(x)) return(NA); if (x<=5) return(0); if (x <= 16) return(1); 
  if (x<= 50) return(2); return(3) }
train.notna$Age.discrete <- sapply(train.notna$Age, simp_age)


## simple fare
plot(density(train.raw$Fare))
simp_fare <- function(x) {if (x < 20) {return(0)}; if (x<40) {return(1)}; if (x<100) {return(2)}; return(3)}
train.ff$Fare.discrete <- sapply(train.raw$Fare, simp_fare)
table(train.ff$Fare.discrete, train.raw$Survived)
table(train.ff$Fare.discrete, train.raw$Pclass)



# function to apply to any dataframe

forge_features <- function(df) {
  res <- df.make.empty(nrow(df))
  res$Title <- sapply(df$Name, make_title)
  res$Fare <- sapply(df$Fare, simp_fare)
  res$Age <- sapply(df$Age, simp_age)
  res$Cabin <- sapply(df$Cabin, simp_cabin_deck)
  return(res)
}



# save the forged features
write.csv(train.ff, file="datasets/titanic_na/train.ff.csv", row.names = FALSE)



