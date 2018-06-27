library(here)


# http://www.rdatamining.com/docs/data-exploration-and-visualization-with-r

train.raw <- read.csv("datasets/titanic/train.csv")
test.raw <- read.csv("datasets/titanic/test.csv")


# values and types
train.raw[1:5,]
head(train.raw)
summary(train.raw)

# thorough data description
library(Hmisc)
describe(train.raw)


# histogram/density of a numerical variable
hist(train.raw$Age)
plot(density(train.raw$Age))

# repartition of a categorical variable
summary(train.raw$Embarked)
table(train.raw$Embarked)
pie(table(train.raw$Embarked))
barplot(table(train.raw$Embarked))

# variable interactions
boxplot(train.raw$Age ~train.raw$Pclass)
boxplot(train.raw$Fare ~ train.raw$Pclass)
boxplot(train.raw$Fare ~ train.raw$Age)

# repartition for all variables
pairs(train.raw)
pairs(train.raw)
