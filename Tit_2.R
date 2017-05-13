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
test <- read.csv('test.csv', stringsAsFactors = FALSE)

test$Survived <- 0

comb <- rbind(train, test)

Agefit <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked, data = comb[!is.na(comb$Age),], method = "anova")
comb$Age[is.na(comb$Age)] <- predict(Agefit, comb[is.na(comb$Age),])

Farefit <- rpart(Fare ~ Pclass + Age + Sex + SibSp + Parch + Embarked, data = comb[!is.na(comb$Fare),], method = "anova")
comb$Fare[is.na(comb$Fare)] <- predict(Farefit, comb[is.na(comb$Fare),])


comb$Age2 <- "old"
comb$Age2[comb$Age <= 7] <- "baby"
comb$Age2[comb$Age > 7 & comb$Age <= 12] <- "child"
comb$Age2[comb$Age > 12 & comb$Age <= 18] <- "young"
comb$Age2[comb$Age > 18 & comb$Age <= 65] <- "adult"

comb$Age2 <- factor(comb$Age2)


comb$Title <- sapply(comb$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
comb$Title[comb$Title %in% c('Mme', 'Mlle')] <- 'Mlle'
comb$Title[comb$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
comb$Title[comb$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'

comb$Title <- factor(comb$Title)

comb$FamilySize <- comb$SibSp + comb$Parch + 1

comb$Surname <- sapply(comb$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})

comb$FamilyID <- paste(as.character(comb$FamilySize), comb$Surname, sep="")

comb$FamilyID[comb$FamilySize <= 3] <- 'Small'

comb$FamilyID2 <- comb$FamilyID
comb$FamilyID2 <- as.character(comb$FamilyID2)
comb$FamilyID2[comb$FamilySize <= 3] <- 'Small'
comb$FamilyID2 <- factor(comb$FamilyID2)

comb$Sex <- factor(comb$Sex)
comb$Embarked <- factor(comb$Embarked)

train <- comb[1:891,]
test <- comb[892:1309,]

fit <- cforest(as.factor(Survived) ~ Pclass + Sex  + Age2 + SibSp + Parch + Fare + Embarked  + FamilyID2, data = train, controls = cforest_unbiased(ntree = 2000, mtry = 3))
Prediction <- predict(fit, test, OOB = TRUE, type = "response")

submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "fifthhsubmit.csv", row.names = FALSE)
