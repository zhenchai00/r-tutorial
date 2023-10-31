### Data Import
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
studentData <- read.csv("student_prediction.csv")

# view dataset
view(studentData)


### Data Cleaning
# check dataset have any NA value
sum(is.na(studentData))

# check each column have any NA value
anyColNA <- sapply(studentData, anyNA)
anyColNA[1:33]

sapply(studentData, function(x) sum(is.na(x)))
sapply(studentData, function(x) mean(is.na(x)))



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


# Check related columns correlation
library(corrplot)
filtered_data <- studentData %>%
    select(EXP_GPA, LISTENS, NOTES, SCHOLARSHIP, ATTEND_DEPT)

# Calculate the correlation matrix
correlation_matrix <- cor(filtered_data)

# Create the correlation plot
corrplot(correlation_matrix,
    method = "color",
    tl.col = "black", tl.srt = 45,
    diag = FALSE, addCoef.col = "black"
)


# Check all the related columns for our hypothesis
dataSet <- studentData %>%
    select(EXP_GPA, LISTENS, NOTES, SCHOLARSHIP, ATTEND_DEPT)
print(dataSet)

# line method plot
ggplot(data = dataSet, mapping = aes(x = EXP_GPA)) +
    geom_freqpoly(mapping = aes(colour = as.factor(ATTEND_DEPT)), binwidth = 0.5)

# BarChart Plotting for Covariation between EXP_GPA and ATTEND_DEPT
ggplot(data = dataSet, mapping = aes(x = as.factor(EXP_GPA))) +
    geom_bar(mapping = aes(colour = as.factor(ATTEND_DEPT)), position = "dodge") +
    labs(
        title = "Covariation between Expected CGPA in Graduation and Attending Seminar/Conferences related to Department",
        x = "Expected CGPA",
        y = "Count"
    ) +
    scale_x_discrete(labels = c("1" = "< 2.00", "2" = "2.00 - 2.49", "3" = "2.50 - 2.99", "4" = "3.00 - 3.49", "5" = "Above 3.49")) +
    scale_color_discrete(name = "Attend Seminar/Conference", labels = c("1" = "Yes", "2" = "No"))

# Point Area Chart Plotting for Covariation between EXP_GPA and ATTEND_DEPT
ggplot(data = dataSet, mapping = aes(x = as.factor(ATTEND_DEPT), y = as.factor(EXP_GPA))) +
    geom_count(mapping = aes(x = as.factor(ATTEND_DEPT), y = as.factor(EXP_GPA))) +
    labs(
        title = "Covariation between Expected CGPA in Graduation and Attending Seminar/Conferences related to Department",
        x = "Attend Seminar/Conference",
        y = "Expected CGPA"
    ) +
    scale_x_discrete(labels = c("1" = "Yes", "2" = "No")) +
    scale_y_discrete(labels = c("1" = "< 2.00", "2" = "2.00 - 2.49", "3" = "2.50 - 2.99", "4" = "3.00 - 3.49", "5" = "Above 3.49")) +
    scale_fill_discrete(name = "Attend Seminar/Conference")


# ggplot(data = dataSet, mapping = aes(x = EXP_GPA)) +
#     geom_freqpoly(mapping = aes(colour = as.factor(LISTENS)), binwidth = 0.5) +
#     labs(title = "Covariation between Expected Cumulative Grade POint Average in Graduation and Listening in Classes", x = "Expected CGPA") +
#     scale_x_discrete(labels = c("1" = "< 2.00", "2" = "2.00 - 2.49", "3" = "2.50 - 2.99", "4" = "3.00 - 3.49", "5" = "Above 3.49"))



# ggplot(data = dataSet, mapping = aes(x = as.factor(LISTENS), y = EXP_GPA)) +
#     geom_violin(aes(fill = as.factor(LISTENS)), trim = FALSE) +
#     labs(title = "Covariation between Expected Cumulative Grade Point Average in Graduation and Listening in Classes",
#          x = "Listen in Classes", y = "Expected CGPA") +
#     scale_x_discrete(labels = c("1" = "Never", "2" = "Sometimes", "3" = "Always")) +
#     scale_fill_discrete(name = "Listen in Classes")




SDF <- studentData %>%
    select(-STUDENTID)
round(cor(SDF), digits = 2)

#
DFQ4 <- studentData %>%
    select(ATTEND_DEPT, EXP_GPA)
print(DFQ4)
cor_result <- cor.test(DFQ4$ATTEND_DEPT, DFQ4$EXP_GPA)
print(cor_result)

DFQ4.1 <- table(SDF$ATTEND_DEPT, SDF$EXP_GPA)
print(chisq.test(DFQ4.1))

studentData %>% filter(CUML_GPA == 4)


#
att <- SDF$ATTEND_DEPT
egpa <- SDF$EXP_GPA

tbl <- table(att, egpa)
chisq.test(tbl)

library(ggplot2)
ggplot(studentData, aes(x = ATTEND_DEPT, y = EXP_GPA)) +
    geom_bar(position = "dodge") +
    labs(title = "abc", x = "Attend Seminar", y = "EXP GPA") +
    theme_minimal()
