# get current working directory
getwd()

# set working directory
setwd("C:/Users/GIGABYTE/Documents/project/r/assignment")

# install and load necessary packages
install.packages("ggplot2")
install.packages("dplyr")
install.packages("tidyverse")
library(ggplot2)
library(tidyverse)
library(dplyr)

# retrieve Data from given dataset csv file
studentData = read.csv("student_prediction.csv")

# view dataset
view(studentData)

# validate dataset is data frame
is.data.frame(studentData)

# structure or datatype of dataset
str(studentData)

# column name
names(studentData)

# number of rows and columns
dim(studentData)

# summary of the dataset
summary(studentData)

# check dataset have any NA value 
sum(is.na(studentData))

# check each column have any NA value
anyColNA = sapply(studentData, anyNA)
anyColNA[1:33]
