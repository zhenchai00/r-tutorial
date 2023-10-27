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


# check how frequent student listening in class
ggplot(data = studentData) + 
    geom_bar(mapping = aes(x = as.factor(LISTENS))) +
    labs(title = "Distribution of Student Listening in Classes", x = "Listen in Class", y = "Count") +
    scale_x_discrete(labels = c("1" = "Never", "2" = "Sometimes", "3" = "Always"))

# check how frequent student taking notes in class
ggplot(data = studentData) + 
    geom_bar(mapping = aes(x = as.factor(NOTES))) +
    labs(title = "Distribution of Student Taking Notes in Classes", x = "Take Notes", y = "Count") +
    scale_x_discrete(labels = c("1" = "Never", "2" = "Sometimes", "3" = "Always"))

# check how many student taking the scholarship
ggplot(data = studentData) + 
    geom_bar(mapping = aes(x = as.factor(SCHOLARSHIP))) +
    labs(title = "Distribution of Student Having Scholarship Type", x = "Scholarship Type", y = "Count") +
    scale_x_discrete(labels = c("1" = "None", "2" = "25%", "3" = "50%", "4" = "75%", "5" = "Full"))

# check how many student attend to the seminars / conference related to the department
ggplot(data = studentData) + 
    geom_bar(mapping = aes(x = as.factor(ATTEND_DEPT))) +
    labs(title = "Distribution of Student Having Attendence to The Seminars/Conferences Related to the Department", x = "Attend", y = "Count") +
    scale_x_discrete(labels = c("1" = "Yes", "2" = "No"))

# check how many student having the Expected Cumulative GPA in graduation
ggplot(data = studentData) + 
    geom_bar(mapping = aes(x = as.factor(EXP_GPA))) +
    labs(title = "Distribution of Student Expected Cumulative grade point average in the graduation", x = "Expected CGPA", y = "Count") +
    scale_x_discrete(labels = c("1" = "< 2.00", "2" = "2.00 - 2.49", "3" = "2.50 - 2.99", "4" = "3.00 - 3.49", "5" = "Above 3.49"))



# check dataset have any NA value 
sum(is.na(studentData))

# check each column have any NA value
anyColNA = sapply(studentData, anyNA)
anyColNA[1:33]

sapply(studentData, function(x) sum(is.na(x)))
sapply(studentData, function(x) mean(is.na(x)))



SDF = studentData %>% 
    select(-STUDENTID)
round(cor(SDF), digits = 2)

# 
DFQ4 = studentData %>% 
    select(ATTEND_DEPT, EXP_GPA)
print(DFQ4)
cor_result = cor.test(DFQ4$ATTEND_DEPT, DFQ4$EXP_GPA)
print(cor_result)

DFQ4.1 = table(SDF$ATTEND_DEPT, SDF$EXP_GPA)
print(chisq.test(DFQ4.1))

studentData %>% filter(CUML_GPA == 4)


#
att = SDF$ATTEND_DEPT
egpa = SDF$EXP_GPA

tbl = table(att, egpa)
chisq.test(tbl)

library(ggplot2)
ggplot(studentData, aes(x = ATTEND_DEPT, y = EXP_GPA)) +
    geom_bar(position = "dodge") +
    labs(title = "abc", x = "Attend Seminar", y = "EXP GPA") + 
    theme_minimal()