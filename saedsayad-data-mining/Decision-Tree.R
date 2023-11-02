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

str(train)
str(test)

View(train)
View(test)

#Columns name
colnames(train)
colnames(test)

#Show  
head(train)
head(test)


#---------------
# Decision tree
#---------------
install.packages("caret")
install.packages("rpart.plot")
install.packages("AUC")
library(caret)
library(rpart)
library(rpart.plot)	
library(AUC)

#train
model.Dtree <- rpart(DEFAULT~., data = train, method="class")
prp(model.Dtree)

#lift chart
pb <- NULL
pb <- predict(model.Dtree, test)
head(pb)
pb <- as.data.frame(pb)
head(pb)
pred.Dtree <- data.frame(test$DEFAULT, pb$Y)
head(pred.Dtree)
colnames(pred.Dtree) <- c("target","score")
pred.Dtree$target <- as.factor(pred.Dtree$target)
head(pred.Dtree)
lift.Dtree <- lift(target ~ score, data = pred.Dtree, cuts=10, class="Y")
lift.Dtree
xyplot(lift.Dtree, main="Decision Tree - Lift Chart", type=c("l","g"), lwd=2
       , scales=list(x=list(alternating=FALSE,tick.number = 10)
                     ,y=list(alternating=FALSE,tick.number = 10)))

#confusion matrix
pc <- NULL
pc <- ifelse(pb$N > pb$Y, "N", "Y")
head(pc)
summary(as.data.frame(pc))
xtab <- table(pc, test$DEFAULT)
head(xtab)
caret::confusionMatrix(xtab, positive = "Y")

#roc chart
labels <- as.factor(ifelse(pred.Dtree$target=="Y", 1, 0))
predictions <- pred.Dtree$score
auc(roc(predictions, labels), min = 0, max = 1)
plot(roc(predictions, labels), min=0, max=1, type="l", main="Decision Tree - ROC Chart")

