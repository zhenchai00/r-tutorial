#------------------
# Data Preparation
#------------------

#Read datasets
#Download the data from http://www.saedsayad.com/datasets/CreditData.zip
# get current working directory
getwd()

# set working directory
setwd("C:/Users/GIGABYTE/Documents/project/r/saedsayad-data-mining")
train <- read.csv("Credit_train.csv")
test <- read.csv("Credit_test.csv")

#Rows and Cols
dim(train)
dim(test)

#Columns name
colnames(train)
colnames(test)

#Show  
head(train)
head(test)


#----------
# Bayesian
#----------
library(caret)
library(e1071)
library(AUC)
library(pROC)

#Y=1 and N=0
train$DEFAULT <- as.factor(ifelse(train$DEFAULT=="Y", 1, 0))
test$DEFAULT <- as.factor(ifelse(test$DEFAULT=="Y", 1, 0))

#train
model.Bayes <- naiveBayes(DEFAULT~., data = train)
model.Bayes

#test
pc <-NULL
pc <- predict(model.Bayes, test, type = "class")
summary(pc)
head(pc)
xtab <- table(pc, test$DEFAULT)
head(xtab)
caret::confusionMatrix(xtab, positive = "1")

#lift chart
pb <-NULL
pb <- predict(model.Bayes, test, type = "raw")
pb <- as.data.frame(pb)
head(pb)
pred.Bayes <- data.frame(test$DEFAULT,pb$"1")
colnames(pred.Bayes) <- c("target","score")
pred.Bayes$target <- as.factor(pred.Bayes$target)
lift.Bayes <- lift(target ~ score, data = pred.Bayes, cuts=10, class="1")
xyplot(lift.Bayes, main="Bayesian Classifier - Lift Chart", type=c("l","g"), lwd=2
       , scales=list(x=list(alternating=FALSE,tick.number = 10)
                     ,y=list(alternating=FALSE,tick.number = 10)))

#roc chart
predictions <- pred.Bayes$score
auc(roc(predictions, labels), min = 0, max = 1)
plot(roc(predictions, labels), min=0, max=1, type="l", main="Bayesian Classifier - ROC Chart")
