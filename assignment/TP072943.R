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


# install packeges
install.packages("validate")
library(validate)

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


# check how frequent student listening in class
ggplot(data = student.Data) +
    geom_bar(mapping = aes(x = as.factor(LISTENS))) +
    labs(title = "Distribution of Student Listening in Classes", x = "Listen in Class", y = "Count") +
    scale_x_discrete(labels = c("1" = "Never", "2" = "Sometimes", "3" = "Always"))

# check how frequent student taking notes in class
ggplot(data = student.Data) +
    geom_bar(mapping = aes(x = as.factor(NOTES))) +
    labs(title = "Distribution of Student Taking Notes in Classes", x = "Take Notes", y = "Count") +
    scale_x_discrete(labels = c("1" = "Never", "2" = "Sometimes", "3" = "Always"))

# check how many student taking the scholarship
ggplot(data = student.Data) +
    geom_bar(mapping = aes(x = as.factor(SCHOLARSHIP))) +
    labs(title = "Distribution of Student Having Scholarship Type", x = "Scholarship Type", y = "Count") +
    scale_x_discrete(labels = c("1" = "None", "2" = "25%", "3" = "50%", "4" = "75%", "5" = "Full"))

# check how many student attend to the seminars / conference related to the department
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

# check how many student having the Expected Cumulative GPA in graduation
ggplot(data = student.Data) +
    geom_bar(
        mapping = aes(x = as.factor(EXP_GPA),fill = as.factor(EXP_GPA)),
        position = "dodge"
    ) +
    labs(
        title = "Distribution of Student Expected Cumulative grade point average in the graduation",
        x = "Expected CGPA",
        y = "Count"
    ) +
    scale_x_discrete(labels = c("1" = "< 2.00", "2" = "2.00 - 2.49", "3" = "2.50 - 2.99", "4" = "3.00 - 3.49", "5" = "Above 3.49"))


# Variation of Student Expected Cumulative Grade Point Average 
bar.Data <- student.Data
bar.Data$EXP_GPA <- factor(bar.Data$EXP_GPA, levels = c("1", "2", "3", "4", "5"),
                           labels = c("< 2.00", "2.00 - 2.49", "2.50 - 2.99", "3.00 - 3.49", "Above 3.49"))

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

# Check related columns correlation
library(corrplot)
filtered.Data <- student.Data %>%
    select(EXP_GPA, LISTENS, NOTES, SCHOLARSHIP, ATTEND_DEPT)

# Calculate the correlation matrix
correlation.Matrix <- cor(filtered.Data)

# Create the correlation plot
corrplot(correlation.Matrix,
    method = "color",
    tl.col = "black", tl.srt = 45,
    diag = FALSE, addCoef.col = "black"
)



# Check related columns correlation
install.packages("polycor")
library(polycor)

# get the relevant data 
exp.GPA.Value <- student.Data$EXP_GPA
listen.Value <- student.Data$LISTENS
notes.Value <- student.Data$NOTES
scholarship.Value <- student.Data$SCHOLARSHIP
attendDept.Value <- student.Data$ATTEND_DEPT

# check the correlation with Polychoric method
polychor(exp.GPA.Value, listen.Value)
polychor(exp.GPA.Value, notes.Value)
polychor(exp.GPA.Value, scholarship.Value)
polychor(exp.GPA.Value, attendDept.Value)


# Check related columns correlation
install.packages("polycor")
library(polycor)

# get the relevant data 
exp.GPA.Value <- student.Data$EXP_GPA
attendDept.Value <- student.Data$ATTEND_DEPT

# check the correlation with Polychoric method
polychor(exp.GPA.Value, attendDept.Value)

# install.packages("psych")
# library(psych)
# polychoric(exp.GPA.Value, attendDept.Value)
# 
# install.packages("rcompanion")
# library(rcompanion)
# install.packages("vcd")
# library(vcd)

# # Select the relevant columns from the dataset
# test_data <- student.Data %>% 
#     select(EXP_GPA, LISTENS, NOTES, SCHOLARSHIP, ATTEND_DEPT)
# 
# # Calculate Cramer's V
# # association_stats <- assocstats(table(test_data$EXP_GPA, test_data$LISTENS, test_data$NOTES, test_data$SCHOLARSHIP, test_data$ATTEND_DEPT))
# association_stats <- assocstats(table(test_data$EXP_GPA, test_data$LISTENS))
# cramer_v <- association_stats$summary$chisq / sum(association_stats$summary$chisq)
# 
# # Print Cramer's V
# print(cramer_v)


# Check all the related columns for our hypothesis
data.Set <- student.Data %>%
    select(EXP_GPA, LISTENS, NOTES, SCHOLARSHIP, ATTEND_DEPT)
print(data.Set)

# line method plot
ggplot(data = data.Set, mapping = aes(x = EXP_GPA)) +
    geom_freqpoly(mapping = aes(colour = as.factor(ATTEND_DEPT)), binwidth = 0.5)


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

ggplot(data = cor.BarData, aes(x = EXP_GPA, fill = ATTEND_DEPT)) +
    geom_bar(position = "fill") +
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

ggplot(data = studentData) +
    labs(title = "The co-variance between Parental Status and Expected CGPA at Graduation",
         x = "Expected CGPA at Graduation", y = "Frequency",
         fill = "Parental Status") +
    geom_bar(position = "fill", width = 0.7, 
             mapping = aes(x = EXP_GPA, fill = as.factor(PARENTAL_STATUS))) +
    scale_fill_manual(values = c("red", "green", "blue"),
                      labels = c("Married", "Divorced", "Died - one of them or both")) +
    scale_x_discrete(drop = FALSE) +
    theme_minimal()


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


# ggplot(data = data.Set, mapping = aes(x = EXP_GPA)) +
#     geom_freqpoly(mapping = aes(colour = as.factor(LISTENS)), binwidth = 0.5) +
#     labs(title = "Covariation between Expected Cumulative Grade POint Average in Graduation and Listening in Classes", x = "Expected CGPA") +
#     scale_x_discrete(labels = c("1" = "< 2.00", "2" = "2.00 - 2.49", "3" = "2.50 - 2.99", "4" = "3.00 - 3.49", "5" = "Above 3.49"))



# ggplot(data = data.Set, mapping = aes(x = as.factor(LISTENS), y = EXP_GPA)) +
#     geom_violin(aes(fill = as.factor(LISTENS)), trim = FALSE) +
#     labs(title = "Covariation between Expected Cumulative Grade Point Average in Graduation and Listening in Classes",
#          x = "Listen in Classes", y = "Expected CGPA") +
#     scale_x_discrete(labels = c("1" = "Never", "2" = "Sometimes", "3" = "Always")) +
#     scale_fill_discrete(name = "Listen in Classes")


# Chi Square Test
tbl <- table(student.Data$EXP_GPA, student.Data$ATTEND_DEPT)
chisq.test(tbl)

data.Set$EXP_GPA <- factor(data.Set$EXP_GPA, levels = c("1", "2", "3", "4", "5"),
                           labels = c("< 2.00", "2.00 - 2.49", "2.50 - 2.99", "3.00 - 3.49", "Above 3.49"))

# Reshape the data into long format
data.long <- data.Set %>%
    gather(key = "Variable", value = "Value", -EXP_GPA)

# Plot multivariate grouped bar chart
ggplot(data = data.long, aes(x = EXP_GPA, fill = Value)) +
    geom_bar(position = "fill") +
    labs(
        title = "Multivariate Grouped Bar Chart",
        x = "Expected CGPA",
        y = "Count"
    ) +
    scale_fill_manual(
        values = c("red", "green", "blue", "orange"), # Define colors for different variable levels
        breaks = c("1", "2", "Yes", "No"), # Specify unique levels from the gathered data
        labels = c("< 2.00", "2.00 - 2.49", "Yes", "No") # Specify labels for the legend
    ) +
    scale_x_discrete(drop = FALSE) +
    theme_minimal()

ggplot(data = studentData) +
    labs(title = "The co-variance between Parental Status and Expected CGPA at Graduation",
         x = "Expected CGPA at Graduation", y = "Frequency",
         fill = "Parental Status") +
    geom_bar(position = "fill", width = 0.7, 
             mapping = aes(x = EXP_GPA, fill = as.factor(PARENTAL_STATUS))) +
    scale_fill_manual(values = c("red", "green", "blue"),
                      labels = c("Married", "Divorced", "Died - one of them or both")) +
    scale_x_discrete(drop = FALSE) +
    theme_minimal()

data.long <- data.Set %>%
    gather(key = "Variable", value = "Value", -EXP_GPA)
head(data.long)

# Plot multivariate line chart
ggplot(data = data.long, aes(x = EXP_GPA, y = Value, color = Variable, group = Variable)) +
    geom_line() +
    labs(
        title = "Multivariate Line Chart",
        x = "Expected CGPA",
        y = "Value"
    ) +
    scale_color_manual(
        values = c("LISTENS" = "red", "NOTES" = "green", "SCHOLARSHIP" = "blue", "ATTEND_DEPT" = "orange")
    ) +
    theme_minimal()

ggplot(data = data.long, aes(x = EXP_GPA, fill = Variable)) +
    geom_bar(position = "dodge") +
    labs(
        title = "Multivariate Bar Chart",
        x = "Expected CGPA",
        y = "Count"
    ) +
    scale_fill_manual(values = c("LISTENS" = "red", "NOTES" = "green", "SCHOLARSHIP" = "blue", "ATTEND_DEPT" = "orange")) +
    theme_minimal()

ggplot(data = data.long, aes(x = EXP_GPA, fill = Variable)) +
    geom_bar() +
    labs(
        title = "Stacked Bar Chart",
        x = "Expected CGPA",
        y = "Count"
    ) +
    scale_fill_manual(values = c("LISTENS" = "red", "NOTES" = "green", "SCHOLARSHIP" = "blue", "ATTEND_DEPT" = "orange")) +
    theme_minimal()
mosaicplot(~ EXP_GPA + LISTENS + NOTES + SCHOLARSHIP + ATTEND_DEPT, data = data.Set, color = TRUE)



# Decision Tree Test
library(caret)
library(rpart)
library(rpart.plot)	
library(AUC)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
mytree <- rpart(
    EXP_GPA ~ HS_TYPE + SCHOLARSHIP + KIDS + WORK, 
    data = student.Data, 
    method = "class",
    minsplit = 1,
    minbucket = 1,
    cp = 0.002
)

mytree

prp(mytree,
    extra = 8, #display number of observations for each terminal node
    roundint=F, #don't round to integers in output
    digits=1)

tree.str <- rpart(
    EXP_GPA ~ NOTES + SCHOLARSHIP + LISTENS + ATTEND_DEPT, 
    data = student.Data, 
    method = "class",
    minsplit = 1,
    minbucket = 1,
    cp = 0.01
)

tree.str

prp(tree.str,
    extra = 8, #display number of observations for each terminal node
    roundint=F, #don't round to integers in output
    digits=1)

dt1.Data <- student.Data
# Convert numeric levels to factor labels for appropriate columns in student.Data
dt1.Data$ATTEND_DEPT <- factor(
    dt1.Data$ATTEND_DEPT,
    levels = c(1, 2),
    labels = c("Yes", "No")
)

dt1.Data$NOTES <- factor(
    dt1.Data$NOTES,
    levels = c(1, 2, 3),
    labels = c("Never", "Sometimes", "Always")
)

dt1.Data$LISTENS <- factor(
    dt1.Data$LISTENS,
    levels = c(1, 2, 3),
    labels = c("Never", "Sometimes", "Always")
)

dt1.Data$SCHOLARSHIP <- factor(
    dt1.Data$SCHOLARSHIP,
    levels = c(1, 2, 3, 4, 5),
    labels = c("None", "25%", "50%", "75%", "Full")
)

dt1.Data$EXP_GPA <- factor(
    dt1.Data$EXP_GPA,
    levels = c(1, 2, 3, 4, 5),
    labels = c("<2.00", "2.00-2.49", "2.50-2.99", "3.00-3.49", "above 3.49")
)

# Now create the decision tree using the modified dt1.Data
tree.str <- rpart(
    EXP_GPA ~ NOTES + SCHOLARSHIP + LISTENS + ATTEND_DEPT, 
    data = dt1.Data, 
    method = "class",
    minsplit = 1,
    minbucket = 1,
    # cp = 0.002
    cp = 0.01
)

# Display the decision tree
tree.str
fancyRpartPlot(tree.str, caption = NULL)


# Decision Tree Test
library(caret)
library(rpart)
library(rpart.plot)	
library(AUC)
install.packages("rattle")
install.packages("rpart.plot")
install.packages("RColorBrewer")
library(rattle)
library(rpart.plot)
library(RColorBrewer)

# dt.Set = student.Data %>% 
#     select(EXP_GPA, LISTENS, NOTES, SCHOLARSHIP, ATTEND_DEPT)
# 
# model.Dtree <- rpart(
#     EXP_GPA ~ ATTEND_DEPT,
#     data = dt.Set,
#     method="class",
#     minsplit = 2,
#     minbucket = 2
# )
# model.Dtree

# dt.Set <- student.Data %>% 
#     select(EXP_GPA, LISTENS, NOTES, ATTEND_DEPT)
# 
# dt.Set <- student.Data %>% 
#     select(EXP_GPA, ATTEND_DEPT)

# Decision Tree Test
library(caret)
library(rpart)
library(rpart.plot)	
library(AUC)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
dt.Set <- student.Data %>% 
    select(EXP_GPA, LISTENS, NOTES, SCHOLARSHIP, ATTEND_DEPT)

# Convert columns to factor with meaningful labels
dt.Set$ATTEND_DEPT <- factor(
    dt.Set$ATTEND_DEPT,
    levels = c(1, 2),
    labels = c("Yes", "No")
)
dt.Set$NOTES <- factor(
    dt.Set$NOTES,
    levels = c(1, 2, 3),
    labels = c("Never", "Sometimes", "Always")
)
dt.Set$LISTENS <- factor(
    dt.Set$LISTENS,
    levels = c(1, 2, 3),
    labels = c("Never", "Sometimes", "Always")
)
dt.Set$SCHOLARSHIP <- factor(
    dt.Set$SCHOLARSHIP,
    levels = c(1, 2, 3, 4, 5),
    labels = c("None", "25%", "50%", "75%", "Full")
)
dt.Set$EXP_GPA <- factor(
    dt.Set$EXP_GPA,
    levels = c(1, 2, 3, 4, 5),
    labels = c("<2.00", "2.00-2.49", "2.50-2.99", "3.00-3.49", "above 3.49")
)

# Build the decision tree model
model.Dtree <- rpart(
    EXP_GPA ~ .,
    data = dt.Set,
    method = "class",
    minsplit = 1,
    minbucket = 1,
)

# display tree plot
model.Dtree
prp(model.Dtree)
fancyRpartPlot(model.Dtree, caption = NULL)

pb <- NULL
pb <- predict(model.Dtree, dt.Set, type = "class")
pb <- as.data.frame(pb)

#lift chart
pred.Dtree <- data.frame(dt.Set$ATTEND_DEPT, pb$pb)
colnames(pred.Dtree) <- c("target","score")
pred.Dtree$target <- as.factor(pred.Dtree$target)
lift.Dtree <- lift(target ~ score, data = pred.Dtree, cuts=10)
lift.Dtree
xyplot(lift.Dtree, main="Decision Tree - Lift Chart", type=c("l","g"), lwd=2
       , scales=list(x=list(alternating=FALSE,tick.number = 10)
                     ,y=list(alternating=FALSE,tick.number = 10)))




# Logistic Regression
library(caret)
library(MASS)	
library(AUC)

install.packages("nnet")
library(nnet) 
library(ggplot2)
library(tidyverse)
library(dplyr)
library(caret)
library(rpart)
library(rpart.plot)	
library(AUC)
library(rattle)

lr.Set <- student.Data %>% 
    select(EXP_GPA, LISTENS, NOTES, SCHOLARSHIP, ATTEND_DEPT)
head(lr.Set)
# model <- glm(EXP_GPA ~ LISTENS+NOTES+SCHOLARSHIP+ATTEND_DEPT, family = binomial(logit), data = lr.Set)
# model

lr.Set$EXP_GPA <- factor(lr.Set$EXP_GPA, levels = c(1, 2, 3, 4), labels = c("<2.00", "2.00-2.49", "2.50-2.99", "3.00-3.49"))
lr.Set$EXP_GPA_binary <- ifelse(lr.Set$EXP_GPA == "3.00-3.49", "3.00-3.49", "not 3.00-3.49")

# Fit multinomial logistic regression model
model.LogReg <- multinom(EXP_GPA_binary ~ LISTENS + NOTES + SCHOLARSHIP + ATTEND_DEPT, data = lr.Set)
head(model.LogReg)

# View model summary
summary(model.LogReg)

#lift chart
pb <- NULL
pb <- predict(model.LogReg, data = lr.Set, type = "probs")

pred.LogReg <- data.frame(
    target = lr.Set$EXP_GPA_binary,
    score = pb
)
head(pred.LogReg)
pred.LogReg$target <- as.factor(pred.LogReg$target)
pred.LogReg$score <- as.factor(pred.LogReg$score)

lift.LogReg <- lift(target ~ score, data = pred.LogReg, cuts = 10)
lift.LogReg
xyplot(lift.LogReg, main="Logistic Regression - Lift Chart", type=c("l","g"), lwd=2
       , scales=list(x=list(alternating=FALSE,tick.number = 10)
                     ,y=list(alternating=FALSE,tick.number = 10)))


# Logistic Regression
# Load libraries
library(caret)
library(MASS)
library(AUC)

# Load the data
lr.Set <- student.Data %>% 
    select(EXP_GPA, LISTENS, NOTES, SCHOLARSHIP, ATTEND_DEPT)

# Convert columns to factor with meaningful labels
lr.Set$ATTEND_DEPT <- factor(
    lr.Set$ATTEND_DEPT,
    levels = c(1, 2),
    labels = c("Yes", "No")
)
lr.Set$NOTES <- factor(
    lr.Set$NOTES,
    levels = c(1, 2, 3),
    labels = c("Never", "Sometimes", "Always")
)
lr.Set$LISTENS <- factor(
    lr.Set$LISTENS,
    levels = c(1, 2, 3),
    labels = c("Never", "Sometimes", "Always")
)
lr.Set$SCHOLARSHIP <- factor(
    lr.Set$SCHOLARSHIP,
    levels = c(1, 2, 3, 4, 5),
    labels = c("None", "25%", "50%", "75%", "Full")
)
lr.Set$EXP_GPA <- factor(
    lr.Set$EXP_GPA,
    levels = c(1, 2, 3, 4, 5),
    labels = c("<2.00", "2.00-2.49", "2.50-2.99", "3.00-3.49", "above 3.49")
)
# Convert EXP_GPA into a binary outcome: 1 vs. 0
lr.Set$EXP_GPA_binary <- ifelse(lr.Set$EXP_GPA == "3.00-3.49", 1, 0)
unique(lr.Set$EXP_GPA_binary)
lr.Set$EXP_GPA

# Fit logistic regression model
model.LogReg <- glm(EXP_GPA_binary ~ LISTENS + NOTES + SCHOLARSHIP + ATTEND_DEPT, 
                    family = binomial(link = "logit"), 
                    data = lr.Set,
                    control = glm.control(maxit = 1000))

# Predict probabilities
pred_probs <- predict(model.LogReg, newdata = lr.Set, type = "response")
pred_probs

# Create binary outcome based on probability threshold (for example, 0.5)
predicted_class <- ifelse(pred_probs > 0.5, "3.00-3.49", "not 3.00-3.49")
predicted_class

# Create a confusion matrix
confusion_matrix <- table(predicted_class, lr.Set$EXP_GPA_binary)
print(confusion_matrix)

# Compute ROC curve
library(pROC)
roc_curve <- roc(as.numeric(lr.Set$EXP_GPA_binary == 1), pred_probs)
plot(roc_curve, main = "ROC Curve", col = "blue")



# Naive Bayesian 
library(caret)
library(e1071)
library(AUC)
library(pROC)

# Load the data
nb.Set <- student.Data %>% 
    select(EXP_GPA, LISTENS, NOTES, SCHOLARSHIP, ATTEND_DEPT)

# check for missing values
sum(is.na(nb.Set))

# check for zero variance prediction
nearZeroVar(nb.Set)

# Train model 
naive.Model <- naiveBayes(EXP_GPA ~ ., data = nb.Set)

# Confusion Matrix
pc <- predict(naive.Model, nb.Set, type = "class")
pc
summary(pc)
xtab <- table(pc, nb.Set$EXP_GPA)
caret::confusionMatrix(xtab, positive = "1")

# train model for lift chart
nb_probs <- predict(naive.Model, nb.Set, type = "raw")

# Check for NaN values
if (any(is.na(nb_probs))) {
    stop("Predicted probabilities contain NaN values. Check your data and model.")
}

# Extract the probabilities for the positive class as value 4 (3.00-3.49)
positive_class_prob <- nb_probs[, "4"]

# Create a data frame with actual and predicted values
lift_data_nb <- data.frame(
    actual = as.numeric(nb.Set$EXP_GPA == "4"),
    predicted = positive_class_prob
)

# Sort the data frame by predicted probability (descending order)
lift_data_nb <- lift_data_nb[order(-positive_class_prob), ]

# Calculate cumulative gains
lift_data_nb$cumulative_actuals <- cumsum(lift_data_nb$actual)
lift_data_nb$cumulative_predictions <- cumsum(lift_data_nb$predicted)

# Calculate lift
lift_data_nb$lift <- lift_data_nb$cumulative_predictions / lift_data_nb$cumulative_actuals

# Plot the lift chart
plot(1:nrow(lift_data_nb), lift_data_nb$lift, type = "l", col = "blue", lwd = 2, xlab = "Percentage of data", ylab = "Lift", main = "Naive Bayes - Lift Chart")


# K Nearest Neighbor
library(caret)
library(AUC)
library(KKN)

# Load the data
kkn.Set <- student.Data %>% 
    select(EXP_GPA, LISTENS, NOTES, SCHOLARSHIP, ATTEND_DEPT)

# Install required packages if not already installed
install.packages(c("kknn", "pcaL1", "pROC"))

# Load required libraries
library(kknn)
library(pcaL1)
library(pROC)

# Create a function to split the data into training and testing sets
split_data <- function(data, split_ratio = 0.8) {
    set.seed(123)  # Set seed for reproducibility
    indices <- sample(1:nrow(data), size = round(split_ratio * nrow(data)))
    training_data <- data[indices, ]
    testing_data <- data[-indices, ]
    return(list(training_data = training_data, testing_data = testing_data))
}

# Split the data into training and testing sets
data_splits <- split_data(kkn.Set)

# Training set
train_set <- data_splits$training_data
# Testing set
test_set <- data_splits$testing_data

# Define the predictor variables (features) and the target variable
X_train <- train_set[, c("LISTENS", "NOTES", "SCHOLARSHIP", "ATTEND_DEPT")]
Y_train <- train_set$EXP_GPA
X_test <- test_set[, c("LISTENS", "NOTES", "SCHOLARSHIP", "ATTEND_DEPT")]
Y_test <- test_set$EXP_GPA

# Train the KNN model
k_value <- 5  # Set the value of k
knn_model <- kknn::train.kknn(EXP_GPA ~ ., data = train_set, scale = TRUE, kmax = k_value)
knn_predictions <- predict(knn_model, X_test)
knn_predictions


# Combine actual and predicted values into a data frame
lift_data <- data.frame(actual = Y_test, predicted = knn_predictions)

# Order the data by predicted probabilities in descending order
lift_data <- lift_data[order(-as.numeric(lift_data$predicted)), ]

# Calculate the cumulative number of actual positives
lift_data$cumulative_actuals <- cumsum(as.numeric(lift_data$actual == "3.00-3.49"))

# Calculate the cumulative number of predicted positives
lift_data$cumulative_predictions <- cumsum(as.numeric(lift_data$predicted == "3.00-3.49"))

# Calculate the lift at each decile
lift_data$lift <- lift_data$cumulative_predictions / lift_data$cumulative_actuals

# Print the first few rows of the lift data
print(head(lift_data))

# Confusion matrix at a specific decile (e.g., the first decile)
decile_conf_matrix <- table(lift_data$predicted[1:(nrow(lift_data)/10)], lift_data$actual[1:(nrow(lift_data)/10)])
print("Confusion Matrix at First Decile:")
print(decile_conf_matrix)


# Lift Chart
lift_values <- gains(Y_test, as.numeric(knn_predictions == "3.00-3.49"), groups = 10)
print("Lift Chart:")
print(lift_values)

# Create ROC curves for each class
roc_curves <- lapply(seq_along(levels(Y_test)), function(i) {
    roc(Y_test == levels(Y_test)[i], as.numeric(knn_predictions == levels(Y_test)[i]))
})

# Plot the ROC curves
print("ROC Curves:")
par(mfrow = c(1, 1))
plot(roc_curves[[1]], col = 1, main = "KNN - ROC Curves", lwd = 2)
sapply(2:length(roc_curves), function(i) {
    plot(roc_curves[[i]], col = i, add = TRUE, lwd = 2)
})
legend("right", legend = levels(Y_test), col = 1:length(roc_curves), lwd = 2)





# K Nearest Neighbor
library(caret)
library(kknn)
library(pROC)

# Load the data
knn.Set <- student.Data %>% 
    select(EXP_GPA, LISTENS, NOTES, SCHOLARSHIP, ATTEND_DEPT)

# Split the data into training and testing sets
set.seed(123)
indices <- sample(1:nrow(knn.Set), size = round(0.8 * nrow(knn.Set)))
train_set <- knn.Set[indices, ]
test_set <- knn.Set[-indices, ]

# Define the predictor variables (features) and the target variable
X_train <- train_set[, c("LISTENS", "NOTES", "SCHOLARSHIP", "ATTEND_DEPT")]
Y_train <- train_set$EXP_GPA
X_test <- test_set[, c("LISTENS", "NOTES", "SCHOLARSHIP", "ATTEND_DEPT")]
Y_test <- test_set$EXP_GPA

# Train the KNN model
k_value <- 5  # Set the value of k
knn_model <- kknn::train.kknn(EXP_GPA ~ ., data = train_set, scale = TRUE, kmax = k_value)
knn_predictions <- predict(knn_model, X_test)

# Confusion Matrix
Y_test <- factor(Y_test, levels = levels(knn_predictions))
conf_matrix <- confusionMatrix(knn_predictions, Y_test, positive = "1")
print("Confusion Matrix:")
print(conf_matrix)


# Manual Lift Chart
# Combine actual and predicted values into a data frame
lift_data <- data.frame(actual = Y_test, predicted = as.numeric(knn_predictions == "3.00-3.49"))

# Order the data by predicted probabilities in descending order
lift_data <- lift_data[order(-as.numeric(lift_data$predicted)), ]

# Calculate the cumulative number of actual positives
lift_data$cumulative_actuals <- cumsum(lift_data$actual)

# Calculate the cumulative number of predicted positives
lift_data$cumulative_predictions <- cumsum(lift_data$predicted)

# Calculate the lift at each point
lift_data$lift <- lift_data$cumulative_predictions / lift_data$cumulative_actuals

# Plot the lift chart
plot(1:nrow(lift_data), lift_data$lift, type = "l", col = "blue", lwd = 2,
     xlab = "Percentage of data", ylab = "Lift", main = "KNN - Lift Chart")

# Create ROC curves for each class
roc_curves <- lapply(seq_along(levels(Y_test)), function(i) {
    roc(Y_test == levels(Y_test)[i], as.numeric(knn_predictions == levels(Y_test)[i]))
})

# Plot the ROC curves
print("ROC Curves:")
par(mfrow = c(1, 1))
plot(roc_curves[[1]], col = 1, main = "KNN - ROC Curves", lwd = 2)
sapply(2:length(roc_curves), function(i) {
    plot(roc_curves[[i]], col = i, add = TRUE, lwd = 2)
})
legend("right", legend = levels(Y_test), col = 1:length(roc_curves), lwd = 2)
