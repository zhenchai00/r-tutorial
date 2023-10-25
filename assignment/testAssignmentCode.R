# get current working directory
getwd()

# set working directory
setwd("C:/Users/GIGABYTE/Documents/project/r/assignment")



# retrieve Data from given dataset csv file
studentDF = read.csv("student_prediction.csv")
view(studentDF)

# validate data set is data frame and any other null value
sum(is.na(studentDF))
anyColNA = sapply(studentDF, anyNA)
anyColNA[1:33]
print(is.data.frame(studentDF))
print(ncol(data))
print(nrow(data))

head(studentDF)
names(studentDF)
dim(studentDF)
str(studentDF)
summary(studentDF)
colSums(is.na(studentDF))

install.packages("GGally")
library(GGally)
library(dplyr)
stdtdata = select(studentDF, -1, -11:-33)
ggpairs(stdtdata)


# Summary Statistic
summary(studentDF)

# Histograms
hist(studentDF$AGE, main="Historgram", xlab="Value")

# Boxplots
boxplot(studentDF$AGE, main="Boxplot", ylab="Value")

# Density Plots
plot(density(studentDF$AGE), main="Density Plot", ylab="Density")

# Bar Plost
barplot(table(studentDF$AGE), main="Bar Plot")

# Summary Statistics by Groups
tapply(studentDF$AGE, studentDF$STUDENTID, summary)

# Quantile-Quantile (Q-Q) Plots
qqnorm(studentDF$AGE)



# Reference from Lecturer
num.cols = sapply(studentDF, is.numeric)
num.cols
cor.data = cor(studentDF[,num.cols])
cor.data
install.packages("corrplot")
library("corrplot")
corrplot(cor.data, method='color')



# reference from GPT
library(corrplot)
library(dplyr)
studentDataSet = read.csv("C:\\Users\\GIGABYTE\\Documents\\project\\r\\assignment\\student_prediction.csv")
str(studentDataSet) 
stdtNum = sapply(studentDataSet, is.numeric)
correlationMatrix = cor(studentDataSet[,stdtNum])
corrplot(correlationMatrix, method = "number")

# Find the correlation with more than 0.6 - 1 / -0.6 - -1
studentNew = studentDataSet %>% 
    select(WORK, ACTIVITY, LISTENS, CUML_GPA, ATTEND)
stdtNum = sapply(studentNew, is.numeric)
studentNew = cor(studentNew[,stdtNum])
corrplot(studentNew, method = "number")


# Count
library(dplyr)
library(tidyverse)
library(ggplot2)
studentDataSet = read.csv("C:\\Users\\GIGABYTE\\Documents\\project\\r\\assignment\\student_prediction.csv")
# studentDataSet %>% 
#     group_by(CUML_GPA) %>% 
#     summarise(n())
# studentDataSet %>% 
#     group_by(EXP_GPA) %>% 
#     summarise(n())
# studentDataSet %>% 
#     group_by(GRADE) %>% 
#     summarise(n())
studentDataSet %>% 
    group_by(COURSE.ID) %>% 
    summarise(n())



# ggplot get the correlation
library(dplyr)
library(tidyverse)
library(ggplot2)

studentDataSet = read.csv("C:\\Users\\GIGABYTE\\Documents\\project\\r\\assignment\\student_prediction.csv")

ggplot(data = studentDataSet, mapping = aes(x = WORK, y = ATTEND, color = WORK)) + 
    geom_point() + 
    labs(x = "Work", y = "Attend", color = "work")


# Recode the WORK and ATTEND variables with meaningful labels
tempData <- studentDataSet %>%
    mutate(WORK = ifelse(WORK == 1, "Yes", "No"),
           ATTEND = case_when(
               ATTEND == 1 ~ "Always",
               ATTEND == 2 ~ "Sometimes",
               ATTEND == 3 ~ "Never"
           ))

# Create the plot
ggplot(data = tempData, mapping = aes(x = WORK, y = ATTEND, color = WORK)) +
    geom_jitter(position = position_jitter(width = 0.2, height = 0.2), size = 3) +
    labs(x = "Work", y = "Attend", color = "Work") +
    scale_x_discrete(labels = c("Yes" = "Yes", "No" = "No")) +
    scale_y_discrete(labels = c("Always" = "Always", "Sometimes" = "Sometimes", "Never" = "Never")) +
    theme_minimal()


#####################################################
# Work, Attend

# import the library
library(corrplot)
library(dplyr)

# Read your dataset
studentDataSet <- read.csv("C:\\Users\\GIGABYTE\\Documents\\project\\r\\assignment\\student_prediction.csv")

# Create a small data frame with WORK and ATTEND variables
correlation_data <- studentDataSet %>% 
    select(WORK, ATTEND)

# Calculate the correlation matrix
temNumValue = sapply(correlation_data, is.numeric)
correlation_matrix <- cor(correlation_data[,temNumValue])

# Create the correlation plot
corrplot(correlation_matrix, method = "color", 
         tl.col = "black", tl.srt = 45, 
         diag = FALSE, addCoef.col = "black")

#####################################################

#####################################################
# Attend, activity

# import the library
library(corrplot)
library(dplyr)

# Read your dataset
studentDataSet <- read.csv("C:\\Users\\GIGABYTE\\Documents\\project\\r\\assignment\\student_prediction.csv")

# Create a small data frame with ACTIVITY and ATTEND variables
correlation_data <- studentDataSet %>% 
    select(ACTIVITY, ATTEND)

# Calculate the correlation matrix
temNumValue = sapply(correlation_data, is.numeric)
correlation_matrix <- cor(correlation_data[,temNumValue])

# Create the correlation plot
corrplot(correlation_matrix, method = "color", 
         tl.col = "black", tl.srt = 45, 
         diag = FALSE, addCoef.col = "black")

#####################################################

#####################################################
# Attend, activity

# import the library
library(corrplot)
library(dplyr)

# Read your dataset
studentDataSet <- read.csv("C:\\Users\\GIGABYTE\\Documents\\project\\r\\assignment\\student_prediction.csv")

# Create a small data frame with LISTENS and ATTEND variables
correlation_data <- studentDataSet %>% 
    select(LISTENS, ATTEND)

# Calculate the correlation matrix
temNumValue = sapply(correlation_data, is.numeric)
correlation_matrix <- cor(correlation_data[,temNumValue])

# Create the correlation plot
corrplot(correlation_matrix, method = "color", 
         tl.col = "black", tl.srt = 45, 
         diag = FALSE, addCoef.col = "black")

#####################################################

#####################################################
# Attend, CGPA

# import the library
library(corrplot)
library(dplyr)

# Read your dataset
studentDataSet <- read.csv("C:\\Users\\GIGABYTE\\Documents\\project\\r\\assignment\\student_prediction.csv")

# Create a small data frame with CUML_GPA and ATTEND variables
correlation_data <- studentDataSet %>% 
    select(CUML_GPA, ATTEND)

# Calculate the correlation matrix
temNumValue = sapply(correlation_data, is.numeric)
correlation_matrix <- cor(correlation_data[,temNumValue])

# Create the correlation plot
corrplot(correlation_matrix, method = "color", 
         tl.col = "black", tl.srt = 45, 
         diag = FALSE, addCoef.col = "black")

#####################################################

#####################################################

# import the library
library(corrplot)
library(dplyr)

# Read your dataset
studentDataSet <- read.csv("C:\\Users\\GIGABYTE\\Documents\\project\\r\\assignment\\student_prediction.csv")

# Create a small data frame with CUML_GPA and ATTEND variables
correlation_data <- studentDataSet %>% 
    select(WORK, ACTIVITY, LISTENS, CUML_GPA, ATTEND)

# Calculate the correlation matrix
temNumValue = sapply(correlation_data, is.numeric)
correlation_matrix <- cor(correlation_data[,temNumValue])
correlation_matrix

# Create the correlation plot
corrplot(correlation_matrix, method = "color", 
         tl.col = "black", tl.srt = 45, 
         diag = FALSE, addCoef.col = "black")
corrplot(
    correlation_matrix, 
    method = "number",
    type = "upper",
    tl.col = "black",
    tl.cex = 2,
    col = colorRampPalette(c("red", "green"))(200)
)


#####################################################

#####################################################

# Import the library
library(corrplot)
library(dplyr)

# Read your dataset
studentDataSet <- read.csv("C:\\Users\\GIGABYTE\\Documents\\project\\r\\assignment\\student_prediction.csv")

# Filter data for students who sometimes attend class (ATTEND = 2)
filtered_data <- studentDataSet %>% 
    filter(ATTEND == 2)

# Create a small data frame with WORK, ACTIVITY, LISTENS, CUML_GPA variables
correlation_data <- filtered_data %>% 
    select(WORK)

# Calculate the correlation matrix
correlation_matrix <- cor(correlation_data)

# Create the correlation plot
corrplot(correlation_matrix, method = "color", 
         tl.col = "black", tl.srt = 45, 
         diag = FALSE, addCoef.col = "black")


#####################################################

#####################################################

# Import the library
library(corrplot)
library(dplyr)

# Read your dataset
studentDataSet <- read.csv("C:\\Users\\GIGABYTE\\Documents\\project\\r\\assignment\\student_prediction.csv")

# Filter data for students who sometimes attend class (ATTEND = 2)
filtered_data <- studentDataSet %>% 
    select(COURSE.ID, EXP_GPA, STUDY_HRS, READ_FREQ, READ_FREQ_SCI, NOTES)

# Calculate the correlation matrix
correlation_matrix <- cor(filtered_data)

# Create the correlation plot
corrplot(correlation_matrix, method = "color", 
         tl.col = "black", tl.srt = 45, 
         diag = FALSE, addCoef.col = "black")


#####################################################

# import the library
library(corrplot)
library(dplyr)

# Read your dataset
studentDataSet <- read.csv("C:\\Users\\GIGABYTE\\Documents\\project\\r\\assignment\\student_prediction.csv")

# Create a small data frame with WORK and ATTEND variables
correlation_data <- studentDataSet %>% 
    select(EXP_GPA, LIKES_DISCUSS, STUDY_HRS, PREP_EXAM, PREP_STUDY,FATHER_EDU, MOTHER_EDU)

# Calculate the correlation matrix
temNumValue = sapply(correlation_data, is.numeric)
correlation_matrix <- cor(correlation_data[,temNumValue])

# Create the correlation plot
corrplot(correlation_matrix, method = "color", 
         tl.col = "black", tl.srt = 45, 
         diag = FALSE, addCoef.col = "black")



