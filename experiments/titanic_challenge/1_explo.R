library(here)

train.raw <- read.csv("datasets/titanic_na/train.csv")
test.raw <- read.csv("datasets/titanic_na/test.csv")

summary(train.raw)



# feature creation

### dataframe to hold the forged features
train.ff <- data.frame(matrix(NA, nrow=nrow(train.raw), ncol=0))

## title

train.ff$Title.raw <- sub(".*?, (\\w*).*", "\\1", train.raw$Name)
table(train.ff$Title.raw)
boxplot(train.raw$Survived ~ train.ff$Title.raw)
t <- table(train.raw$Survived, train.ff$Title.raw)
prop.table(t, 2)

simp_title <- function(title) {
  if (title %in% c("Mlle", "Ms") ) {
    return("Miss")
  }
  if (title == "Mme") {return("Mrs");}
  
  if (title %in% c("Master", "Miss", "Mrs", "Mr")) {return(title);}
  return("Special")
}
train.ff$Title <- sapply(train.ff$Title.raw, simp_title)
table(train.ff$Title)


## simple fare
plot(density(train.raw$Fare))
simp_fare <- function(x) {if (x < 20) {return(0)}; if (x<40) {return(1)}; if (x<100) {return(2)}; return(3)}
train.ff$Fare.factor <- sapply(train.raw$Fare, simp_fare)
table(train.ff$Fare.factor, train.raw$Survived)
table(train.ff$Fare.factor, train.raw$Pclass)


##  simple age
plot(density(train.raw$Age[!is.na(train.raw$Age)]))
simp_age <- function(x) {if (is.na(x)) return(NA); if (x<=5) return(0); if (x <= 16) return(1); 
                         if (x<= 50) return(2); return(3) }
train.ff$Age.factor <- sapply(train.raw$Age, simp_age)

## cabines 
# données incomplètes, on peut oublier
# sauf peut-être pour les 1ère classes
simp_cabin_deck <- function(cabin_str, pclass) {
  if (pclass == 1) {
    if (cabin_str != "") {
      return(substring(cabin_str,0,1))
    } else  { return("NO") }
  } else { return("NO") }
}

train.ff$Cabin.simp <- mapply(simp_cabin_deck, train.raw$Cabin, train.raw$Pclass)


# save the forged features
write.csv(train.ff, file="datasets/titanic_na/train.ff.csv", row.names = FALSE)
