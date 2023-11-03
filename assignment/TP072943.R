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
    # EXP_GPA ~ ATTEND_DEPT,
    data = dt.Set,
    method = "class",
    minsplit = 2,
    minbucket = 1
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
install.packages("pheatmap")
library(pheatmap)
install.packages("cvms")
library(cvms)
install.packages("pROC")
library(pROC)

nb.Set <- student.Data %>% 
    select(EXP_GPA, LISTENS, NOTES, SCHOLARSHIP, ATTEND_DEPT)
head(nb.Set)

# Convert variables to factors if needed
nb.Set$ATTEND_DEPT <- factor(
    nb.Set$ATTEND_DEPT,
    levels = c(1, 2),
    labels = c("Yes", "No")
)
nb.Set$NOTES <- factor(
    nb.Set$NOTES,
    levels = c(1, 2, 3),
    labels = c("Never", "Sometimes", "Always")
)
nb.Set$LISTENS <- factor(
    nb.Set$LISTENS,
    levels = c(1, 2, 3),
    labels = c("Never", "Sometimes", "Always")
)
nb.Set$SCHOLARSHIP <- factor(
    nb.Set$SCHOLARSHIP,
    levels = c(1, 2, 3, 4, 5),
    labels = c("None", "25%", "50%", "75%", "Full")
)
nb.Set$EXP_GPA <- factor(
    nb.Set$EXP_GPA,
    levels = c(1, 2, 3, 4),
    labels = c("<2.00", "2.00-2.49", "2.50-2.99", "3.00-3.49")
)
head(nb.Set)

# Train the Naive Bayes model
nb_model <- naiveBayes(EXP_GPA ~ LISTENS + NOTES + SCHOLARSHIP + ATTEND_DEPT, data = nb.Set)
nb_model
sum(is.na(nb.Set$EXP_GPA))

# Get confusion matrix
confusion_matrix <- table(predict(nb_model, nb.Set), nb.Set$EXP_GPA)
print(confusion_matrix)

# Compute confusion matrix
confusion_matrix <- confusionMatrix(data = predict(nb_model, nb.Set), reference = nb.Set$EXP_GPA)

# Create confusion matrix heatmap
heatmap(confusion_matrix$table, 
        Colv = NA, 
        Rowv = NA, 
        col = colorRampPalette(c("white", "blue"))(20),
        main = "Confusion Matrix Heatmap",
        xlab = "Predicted",
        ylab = "Actual")

############################################
# Plot the confusion matrix
conf_matrix_plot <- ggplot(data = as.data.frame(confusion_matrix$table), aes(x = Reference, y = Prediction, fill = log(Freq))) +
    geom_tile() +
    scale_fill_gradient(low = "white", high = "blue") +
    theme_minimal() +
    labs(title = "Confusion Matrix Heatmap",
         x = "Actual",
         y = "Predicted")

# Print the plot
print(conf_matrix_plot)


############################################
# Get predicted probabilities for each class
pred_probs <- predict(nb_model, nb.Set, type = "raw")
pred_probs

# Convert EXP_GPA to binary for ROC curve calculation
binary_EXP_GPA <- ifelse(as.numeric(nb.Set$EXP_GPA) > 2, 1, 0)
binary_EXP_GPA

unique(pred_probs[, 4])
# Calculate ROC curve
roc_curve <- roc(binary_EXP_GPA, as.numeric(pred_probs[, 4]) > 0.3)  # Assuming you want ROC for class "3.00-3.49"

# Plot ROC curve
plot(roc_curve, main = "ROC Curve", col = "blue", lwd = 2)




############################################
# Create Precision-Recall curve for each class
pr_curves <- multiclass.pr.curve(response = nb.Set$EXP_GPA, predictor = as.numeric(pred_probs), plot = TRUE)

# Function to calculate precision and recall
calculate_precision_recall <- function(predictions, true_labels, positive_class) {
    true_positives <- sum(predictions == positive_class & true_labels == positive_class)
    predicted_positives <- sum(predictions == positive_class)
    actual_positives <- sum(true_labels == positive_class)
    
    precision <- true_positives / predicted_positives
    recall <- true_positives / actual_positives
    
    return(c(precision = precision, recall = recall))
}

# Convert EXP_GPA to binary for precision-recall calculation
binary_EXP_GPA <- ifelse(as.numeric(nb.Set$EXP_GPA) > 2, 1, 0)

# Calculate precision and recall for "3.00-3.49" class
precision_recall <- calculate_precision_recall(as.numeric(pred_probs[, 4] > 0.5), binary_EXP_GPA, positive_class = 1)  # Assuming "3.00-3.49" is the fourth column in pred_probs

# Print precision and recall
print(paste("Precision for 3.00-3.49:", precision_recall[1]))
print(paste("Recall for 3.00-3.49:", precision_recall[2]))
