library(here)
source(file="misc_functions.R")


train.raw <- read.csv("datasets/titanic_na/train.csv")
test.raw <- read.csv("datasets/titanic_na/test.csv")
train.ff <- read.csv("datasets/titanic_na/train.ff.csv")

# train.ff$Fare.factor <- as.factor(train.ff$Fare.discrete)
# train.ff$Age.factor <- as.factor(train.ff$Age.discrete)

# mice : 
# http://web.maths.unsw.edu.au/~dwarton/missingDataLab.html
# https://www.kaggle.com/captcalculator/imputing-missing-data-with-the-mice-package-in-r
# https://gerkovink.github.io/miceVignettes/Convergence_pooling/Convergence_and_pooling.html
library(mice)
md.pattern(train.raw)



# continuous age imputation

cols.to_copy <- c("Age", "SibSp", "Parch", "Sex", "Fare")
train.mi1.input <- df.make.from_df(train.raw, cols.to_copy)
train.mi1.input$Title <- train.ff$Title


imp1.cart <- mice(train.mi1.input, m=5, method='cart', printFlag=FALSE)
xyplot(imp1.cart, Age ~ Title | .imp)
densityplot(imp1.cart)

imp1.pmm <- mice(train.mi1.input, m=10, method='pmm', printFlag=FALSE)
xyplot(imp1.pmm, Age ~ Title)
densityplot(imp1.pmm)

imp1.auto <-  mice(train.mi1.input, m=5, printFlag=FALSE)
imp1.auto$method


# discrete age imputation

cols.to_copy <- c("SibSp", "Parch", "Sex")
train.mi2.input <- df.make.from_df(train.raw, cols.to_copy)
train.mi2.input$Title <- train.ff$Title
train.mi2.input$Age <- train.ff$Age.discrete
train.mi2.input$Fare <- train.ff$Fare.discrete


imp2.cart <- mice(train.mi2.input, m=5, method='cart', printFlag=FALSE)
xyplot(imp2.cart, Age ~ Fare | .imp)
densityplot(imp2.cart)

imp2.pmm <- mice(train.mi2.input, m=5, method='pmm', printFlag=FALSE)
xyplot(imp2.pmm, Age ~ Title)
densityplot(imp2.pmm)


imp2.auto <-  mice(train.mi2.input, m=5, printFlag=FALSE)
imp2.auto$method



# imputations


imp1 <- complete(imp1.pmm)
imp2 <- complete(imp2.pmm)

### with    +    pool   -> ???   
fit1.mi <- with(data = imp1.pmm, exp= lm(Age ~ Title + Sex + Parch + SibSp))
combFit <- pool(fit1.mi)
round(summary(combFit),2)
### with    +    pool   -> ???   

NA_row <- which(is.na(train.raw[,"Age"]))
imp1.age.factor <- sapply( imp1$Age, simp_age )



# save imputations

to_save <- df.make.empty(nrow(train.raw), c("Age", "Age.factor"))
to_save$Age <- imp1$Age
to_save$Age.factor <- imp2$Age
write.csv(to_save, file="datasets/titanic_na/age_imp.csv")
