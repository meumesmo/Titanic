devtools::install_github("rstudio/tensorflow")
install.packages("randomForest")
install.packages("rattle")
install.packages("rpart.plot")
install.packages("RColorBrewer")
install.packages("party")

library(tensorflow)
library(dplyr)
library(rpart)
library(randomForest)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(party)


train <- read.csv('train.csv', stringsAsFactors = FALSE)
View(train)
glimpse(train)

train$Sex <- factor(train$Sex)

Agefit <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked, data = train[!is.na(train$Age),], method = "anova")
train$Age[is.na(train$Age)] <- predict(Agefit, train[is.na(train$Age),])

train$Age2 <- "old"
train$Age2[train$Age <= 7] <- "baby"
train$Age2[train$Age > 7 & train$Age <= 12] <- "child"
train$Age2[train$Age > 12 & train$Age <= 18] <- "young"
train$Age2[train$Age > 18 & train$Age <= 65] <- "adult"

train$Age2 <- factor(train$Age2)

which(train$Embarked == '')
train$Embarked[c(62, 830)] = "S" 
train$Embarked <- factor(train$Embarked)

which(is.na(train$Fare))

train$Title <- sapply(train$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
train$Title[train$Title %in% c('Mme', 'Mlle')] <- 'Mlle'
train$Title[train$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
train$Title[train$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'

train$Title <- factor(train$Title)

train$FamilySize <- train$SibSp + train$Parch + 1

train$Surname <- sapply(train$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})

train$FamilyID <- paste(as.character(train$FamilySize), train$Surname, sep="")

train$FamilyID[train$FamilySize <= 3] <- 'Small'

famIDs <- data.frame(table(train$FamilyID))
train$FamilyID[train$FamilyID %in% famIDs$Var1] <- 'Small'
train$FamilyID <- factor(train$FamilyID)

set.seed(1)
n <- nrow(train)
shuffled <- train[sample(n),]

train_indices <- 1:round(0.7 * n)
train2 <- shuffled[train_indices,]
test_indices <- (round(0.7 * n) + 1) : n
test <- shuffled[test_indices,]

fit <- cforest(as.factor(Survived) ~ Pclass + Sex + Age2 + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID + Surname, data = train2, controls = cforest_unbiased(ntree = 2000, mtry = 3))
