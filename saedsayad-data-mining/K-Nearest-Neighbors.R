#------------------
# Data Preparation
#------------------

#Read datasets
#Download the data from http://www.saedsayad.com/datasets/CreditData.zip
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


#---------------------
# K Nearest Neighbors
#---------------------
install.packages("kknn")
library(caret)
library(kknn)
library(AUC)

#remove all records with missing values
train <- na.omit(train)
test <- na.omit(test)

#train
model.KNN <- kknn(DEFAULT~., train, test, k=5, distance = 2, scale=FALSE)
summary(model.KNN)

#confusion matrix
pc <- NULL
pc <- predict(model.KNN, test, type="raw")
xtab <- table(pc, test$DEFAULT)
caret::confusionMatrix(xtab, positive="Y")

#lift chart
pb <- NULL
pb <- predict(model.KNN, test, type="prob")
pb <- as.data.frame(pb)
pred.KNN <- data.frame(test$DEFAULT, pb$Y)
colnames(pred.KNN) <- c("target","score")
lift.KNN <- lift(target ~ score, data = pred.KNN, cuts=10, class="Y")
xyplot(lift.KNN, main="KNN - Lift Chart", type=c("l","g"), lwd=2
       , scales=list(x=list(alternating=FALSE,tick.number = 10)
                     ,y=list(alternating=FALSE,tick.number = 10)))

#roc chart
labels <- as.factor(ifelse(pred.KNN$target=="Y", 1, 0))
predictions <- pred.KNN$score
auc(roc(predictions, labels), min = 0, max = 1)
plot(roc(predictions, labels), min=0, max=1, type="l", main="KNN - ROC Chart")

