library(rpart)
library(rpart.plot)
library(caret)
library(randomForest)
library(adabag)
library(jtools)
library(sandwich)

setwd("Desktop/CSU Global Data Analytics/MIS510/Module 3/")
Toyota.ct <- read.csv("ToyotaCorolla.csv")

#Dimension of the frame
dim(Toyota.ct) 

#First six rows
head(Toyota.ct) 

#Print the list in a useful column format
t(t(names(Toyota.ct)))

#Summary Statistics for each varaible
summary(Toyota.ct)

#Missing values
data.frame(miss.val=sapply(Toyota.ct, function(x) 
  sum(length(which(is.na(x))))))

#Multiple Regression
Toyota.ct <- read.csv("ToyotaCorolla.csv")
Toyota.ct <- Toyota.ct[c(3,4,7,8,9,12,14,17,19,21,25,26,28,30,34,39)]
ToyReg <- lm(Price~ ., data = Toyota.ct)
summ(ToyReg, scale = TRUE, part.corr = TRUE, digits = 3)

#Default Regression Tree
ToyReg.tree <- rpart(Price ~ ., data = Toyota.ct, method = "anova", cp = 0.001, maxdepth = 30)
length(ToyReg.tree$frame$var[ToyReg.tree$frame$var == "<leaf>"])
prp(ToyReg.tree, type = 1, extra = 1, under = TRUE, split.font = 1, varlen = -10, digits=-6)

#Deep Regression Tree
ToyReg.tree <- rpart(Price ~ ., data = Toyota.ct, method = "anova", cp = 0, maxdepth = 30)
length(ToyReg.tree$frame$var[ToyReg.tree$frame$var == "<leaf>"])

#Pruned Regression Tree
Toy.ct <- rpart(Price ~ ., data = Toyota.ct, method = "anova",
                cp = 0.001, minsplit = 5, xval = 5)
length(Toy.ct$frame$var[Toy.ct$frame$var == "<leaf>"])
printcp(Toy.ct)

pruned.ct <- prune(Toy.ct,
                   cp = Toy.ct$cptable[which.min(Toy.ct$cptable[,"xerror"]), "CP"])
length(pruned.ct$frame$var[pruned.ct$frame$var == "<leaf>"])
prp(pruned.ct, type = 1, extra = 1, under = TRUE, split.font = 1, varlen = -10, digits=-6)