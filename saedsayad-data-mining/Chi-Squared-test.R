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

#---------------------------------------------------------------------
# Data Exploration - Bivariate analysis - Categorical and Categorical
#---------------------------------------------------------------------

#DEFAULT and BUSTYPE
x <- xtabs(~BUSTYPE+DEFAULT, data=train)
plot(x, main="Type of small business", sub="train", col="darkgreen")

y <- xtabs(~BUSTYPE+DEFAULT, data=test)
plot(y, main="Type of small business", sub="test", col="brown")


#Chi Squared test
tbl <- table(train$DEFAULT, train$BUSTYPE)
chisq.test(tbl)
