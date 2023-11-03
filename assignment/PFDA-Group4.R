# TP072943 CHONG CAI ZHEN
# TP064041 GOH WEN QING
# TP067697 JOVIN GAZALI
# TP072316 CHONG PHUY LE


### Data Import
# get current working directory
getwd()

# set working directory
setwd("C:/Users/GIGABYTE/Documents/project/r/assignment")

# install and load necessary packages
install.packages("ggplot2")
install.packages("dplyr")
install.packages("tidyverse")
install.packages("validate")
install.packages("polycor")
install.packages("caret")
install.packages("rpart")
install.packages("rpart.plot")
install.packages("AUC")
install.packages("rattle")
install.packages("rpart.plot")
install.packages("RColorBrewer")
library(ggplot2)
library(dplyr)
library(tidyverse)
library(validate)
library(corrplot)
library(polycor)
library(caret)
library(AUC)
library(rpart)
library(rpart.plot)	
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(MASS)

# retrieve Data from given dataset csv file
student.Data <- read.csv("student_prediction.csv")

# view dataset
view(student.Data)


### Data Cleaning
# check dataset have any NA value
sum(is.na(student.Data))

# check each column have any NA value
anyColNA <- sapply(student.Data, anyNA)
anyColNA[1:33]

sapply(student.Data, function(x) sum(is.na(x)))
sapply(student.Data, function(x) mean(is.na(x)))


# validate dataset is data frame
is.data.frame(student.Data)

# structure or datatype of dataset
str(student.Data)

# column name
names(student.Data)

# number of rows and columns
dim(student.Data)

# summary of the dataset
summary(student.Data)


# Validate each of relevant columns 
if (all(in_range(student.Data$LISTENS, min = "1", max = "3") == TRUE)) {
    print("No factors other than 1 to 3 is detected in LISTENs.")
}
if (all(in_range(student.Data$SCHOLARSHIP, min = "1", max = "5") == TRUE)) {
    print("No factors other than 1 to 5 is detected in SCHOLARSHIP.")
}
if (all(in_range(student.Data$NOTES, min = "1", max = "3") == TRUE)) {
    print("No factors other than 1 to 3 is detected in NOTES.")
}
if (all(in_range(student.Data$ATTEND_DEPT, min = "1", max = "2") == TRUE)) {
    print("No factors other than 1 to 2 is detected in ATTEND_DEPT.")
}
if (all(in_range(student.Data$EXP_GPA, min = "1", max = "5") == TRUE)) {
    print("No factors other than 1 to 5 is detected in EXP_GPA.")
}


# Variation of Student Expected Cumulative Grade Point Average 
bar.Data <- student.Data
bar.Data$EXP_GPA <- factor(
    bar.Data$EXP_GPA,
    levels = c("1", "2", "3", "4", "5"),
    labels = c("< 2.00", "2.00 - 2.49", "2.50 - 2.99", "3.00 - 3.49", "Above 3.49")
)

ggplot(data = bar.Data, aes(x = EXP_GPA, fill = EXP_GPA)) +
    geom_bar() +
    geom_text(
        aes(y = ..count.., label = ..count..),
        stat = "count",
        position = position_dodge(width = 0.9),
        vjust = -0.5,
        size = 3
    ) +
    labs(
        title = "Variation of Student Expected Cumulative Grade Point Average in Graduation",
        x = "Expected CGPA",
        y = "Count"
    ) +
    scale_fill_manual(
        values = c("red", "orange", "yellow", "green", "blue"),
        breaks = c("< 2.00", "2.00 - 2.49", "2.50 - 2.99", "3.00 - 3.49", "Above 3.49"),
        labels = c("< 2.00", "2.00 - 2.49", "2.50 - 2.99", "3.00 - 3.49", "Above 3.49")
    ) +
    scale_x_discrete(
        limits = c("< 2.00", "2.00 - 2.49", "2.50 - 2.99", "3.00 - 3.49", "Above 3.49")
    )

# Research Question 4: Does student who attends seminars or conferences related to the department, 
# more likely to achieve a higher expected cumulative grade point average (GPA).

# Variation of student attend to seminars or conference related to the department
bar.Data <- student.Data
bar.Data$ATTEND_DEPT = as.factor(bar.Data$ATTEND_DEPT)
head(bar.Data)
ggplot(data = bar.Data) +
    geom_bar(
        mapping = aes(x = as.factor(ATTEND_DEPT), fill = ATTEND_DEPT)
    ) +
    geom_text(
        aes(x = as.factor(ATTEND_DEPT), y = ..count.., label = ..count..),
        stat = "count",
        position = position_dodge(width = 0.9),
        vjust = -0.5,
        size = 3
    ) +
    labs(
        title = "Variant of Student Having Attendence to The Seminars/Conferences Related to the Department",
        x = "Attend",
        y = "Count"
    ) +
    scale_x_discrete(
        labels = c("1" = "Yes", "2" = "No"),
        limits = c("1", "2")
    ) +
    scale_fill_manual(
        values = c("1" = "green", "2" = "blue"),
        breaks = c("1", "2"),
        labels = c("Yes", "No")
    )


# Covariation between EXP_GPA and ATTEND_DEPT
# Check all the related columns for our hypothesis
data.Set <- student.Data %>%
    select(EXP_GPA, LISTENS, NOTES, SCHOLARSHIP, ATTEND_DEPT)

cor.BarData = data.Set
cor.BarData$EXP_GPA = as.factor(cor.BarData$EXP_GPA)
cor.BarData$ATTEND_DEPT = as.factor(cor.BarData$ATTEND_DEPT)

# BarChart Plotting for Covariation between EXP_GPA and ATTEND_DEPT
ggplot(data = cor.BarData, aes(x = EXP_GPA, fill = ATTEND_DEPT)) +
    geom_bar(position = "dodge") +
    geom_text(
        aes(x = EXP_GPA, y = ..count.., label = ..count..),
        stat = "count",
        position = position_dodge(width = 0.9),
        vjust = -0.5,
        size = 3
    ) +
    labs(
        title = "Covariation between Expected CGPA in Graduation and Attending Seminar/Conferences related to Department",
        x = "Expected CGPA",
        y = "Count"
    ) +
    scale_x_discrete(
        labels = c("1" = "< 2.00", "2" = "2.00 - 2.49", "3" = "2.50 - 2.99", "4" = "3.00 - 3.49")
    ) +
    scale_fill_discrete(
        name = "Attend Seminar/Conference",
        labels = c("1" = "Yes", "2" = "No")
    )

# Point Area Chart Plotting for Covariation between EXP_GPA and ATTEND_DEPT
ggplot(data = cor.BarData, mapping = aes(x = ATTEND_DEPT, y = EXP_GPA)) +
    geom_count(mapping = aes(x = ATTEND_DEPT, y = EXP_GPA)) +
    labs(
        title = "Covariation between Expected CGPA in Graduation and Attending Seminar/Conferences related to Department",
        x = "Attend Seminar/Conference",
        y = "Expected CGPA"
    ) +
    scale_x_discrete(
        labels = c("1" = "Yes", "2" = "No")) +
    scale_y_discrete(labels = c("1" = "< 2.00", "2" = "2.00 - 2.49", "3" = "2.50 - 2.99", "4" = "3.00 - 3.49", "5" = "Above 3.49")) +
    scale_fill_discrete(name = "Attend Seminar/Conference")


# get the relevant data 
exp.GPA.Value <- student.Data$EXP_GPA
attendDept.Value <- student.Data$ATTEND_DEPT

# check the correlation with Polychoric method
polychor(exp.GPA.Value, attendDept.Value)

# Chi Square Test
tbl <- table(student.Data$EXP_GPA, student.Data$ATTEND_DEPT)
chisq.test(tbl)