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



#--------------------
# Logistc regression
#--------------------
library(caret)
library(MASS)	
library(AUC)

#remove all records with missing values
train <- na.omit(train)
test <- na.omit(test)

#Binary target
train$DEFAULT <- as.factor(ifelse(train$DEFAULT=="Y", 1, 0))
test$DEFAULT <- as.factor(ifelse(test$DEFAULT=="Y", 1, 0))
head(train)
head(test)

table(train$DEFAULT)
table(test$DEFAULT)
head(train)
head(test)


#train
model.LogReg <- glm(DEFAULT~BUSAGE+MAXLINEUTIL+DAYSDELQ+TOTACBAL,family=binomial(logit), data=train)
model.LogReg
summary(model.LogReg)


#lift chart
pb <- NULL
pb <- predict(model.LogReg, test)
pb
pb <- as.data.frame(pb)
pred.LogReg <- data.frame(test$DEFAULT, 1/(1+(exp(1)^-pb)))
pred.LogReg
colnames(pred.LogReg) <- c("target","score")
lift.LogReg <- lift(target ~ score, data = pred.LogReg, cuts=10, class="1")
xyplot(lift.LogReg, main="Logistic Regression - Lift Chart", type=c("l","g"), lwd=2
       , scales=list(x=list(alternating=FALSE,tick.number = 10)
                     ,y=list(alternating=FALSE,tick.number = 10)))

#confusion matrix
pc <- NULL
pc <- ifelse(pb > 0.5,"1","0")
summary(pc)
xtab <- table(pc, test$DEFAULT)
caret::confusionMatrix(xtab, positive = "1")

#roc chart
labels <- pred.LogReg$target
predictions <- pred.LogReg$score
auc(roc(predictions, labels), min = 0, max = 1)
plot(roc(predictions, labels), min=0, max=1, type="l", main="Logistic Regression - ROC Chart")



