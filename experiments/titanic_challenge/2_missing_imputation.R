library(here)

train.raw <- read.csv("datasets/titanic_na/train.csv")
test.raw <- read.csv("datasets/titanic_na/test.csv")
train.ff <- read.csv("datasets/titanic_na/train.ff.csv")

# train.ff$Fare.factor <- as.factor(train.ff$Fare.discrete)
# train.ff$Age.factor <- as.factor(train.ff$Age.discrete)

# mice : 
# http://web.maths.unsw.edu.au/~dwarton/missingDataLab.html
# https://www.kaggle.com/captcalculator/imputing-missing-data-with-the-mice-package-in-r
library(mice)
md.pattern(train.raw)

train.mi1.input <- data.frame(matrix(NA, nrow = nrow(train.raw), ncol = 0))

to_copy.raw <- c("Age", "SibSp", "Parch", "Sex")
test.df = data.frame(matrix(vector(), nrow(train.raw), 4,
                        dimnames=list(c(), to_copy.raw)),
                        stringsAsFactors=TRUE)


for (colname in to_copy.raw) {test.df[,colname] <- train.raw[,colname]}


train.mi2.input$Sib

imp <- mice(train.raw, m=5, maxit = 40, seed=2525)
