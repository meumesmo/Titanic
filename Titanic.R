devtools::install_github("rstudio/tensorflow")
install.packages("randomForest")
install.packages("rattle")
install.packages("rpart.plot")
install.packages("RColorBrewer")
install.packages("party")
install.packages("caret")
install.packages("gbm")
install.packages("cvAUC")

library(tensorflow)
library(dplyr)
library(rpart)
library(randomForest)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(party)
library(caret)
library(gbm)
library(cvAUC)

train <- read.csv('train.csv', stringsAsFactors = FALSE)
test <- read.csv('test.csv', stringsAsFactors = FALSE)

test$Survived <- 0

combi <- rbind(train, test)

combi$Sex <- factor(combi$Sex)

Agefit <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked, data = combi[!is.na(combi$Age),], method = "anova")
combi$Age[is.na(combi$Age)] <- predict(Agefit, combi[is.na(combi$Age),])

combi$Age <- as.integer(combi$Age)

combi$Age2 <- "old"
combi$Age2[combi$Age <= 12] <- "child"
combi$Age2[combi$Age > 12 & combi$Age <= 21] <- "young"
combi$Age2[combi$Age > 21 & combi$Age <= 65] <- "adult"

combi$Age2 <- factor(combi$Age2)

which(combi$Embarked == '')
combi$Embarked[c(62, 830)] = "S" 
combi$Embarked <- factor(combi$Embarked)

which(is.na(combi$Fare))

Farefit <- rpart(Fare ~ Pclass + Sex + SibSp + Parch + Fare + Embarked, data = combi[!is.na(combi$Fare),], method = "anova")
combi$Fare[is.na(combi$Fare)] <- predict(Farefit, combi[is.na(combi$Fare),])

combi$Title <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
combi$Title[combi$Title %in% c('Mme', 'Mlle')] <- 'Mlle'
combi$Title[combi$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
combi$Title[combi$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'

allvalues <- unique(combi$Title)

combi$Title <- factor(combi$Title)

combi$FamilySize <- combi$SibSp + combi$Parch + 1

combi$Surname <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})

combi$FamilyID <- paste(as.character(combi$FamilySize), combi$Surname, sep="")

combi$FamilyID[combi$FamilySize <= 2] <- 'Small'

famIDs <- data.frame(table(combi$FamilyID))
famIDs <- famIDs[famIDs$Freq <= 2,]
combi$FamilyID[combi$FamilyID %in% famIDs$Var1] <- 'Small'

combi$FamilyID <- factor(combi$FamilyID)

test <- combi[892:1309,]
train <- combi[1:891,]

train$Name <- NULL
test$Name <- NULL

train$Ticket <- NULL
test$Ticket <- NULL

train$Cabin <- NULL
test$Cabin <- NULL

train$Surname <- NULL
test$Surname <- NULL

train$Title <- factor(train$Title, levels = allvalues)
test$Title <- factor(test$Title, levels = allvalues)

inTrain <- createDataPartition(y = train$Survived, p = 0.75, list = FALSE)

training <- train[inTrain,]
testing <- train[-inTrain,]

modFit <- train(as.factor(Survived) ~., method = "gbm", data = train, verbose = FALSE)
print(modFit)

preds <- predict(modFit, newdata = test)
sol <- data.frame(PassengerId = test$PassengerId , Survived = preds)
write.csv(sol, file = "titanic_solution.csv", row.names = FALSE)
