getwd()

# set working directory
setwd("C:/Users/GIGABYTE/Documents/project/r/assignment")

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

library(caret)
library(caTools)
library(class)
library(pROC)

student.Data <- read.csv("student_prediction.csv")

# Load the data
knn.Set <- student.Data %>% 
    select(EXP_GPA, LISTENS, NOTES, SCHOLARSHIP, ATTEND_DEPT)

# split test and training data for kNN model
set.seed(255)
split = sample.split(knn.Set$EXP_GPA, SplitRatio = 0.75)
train = subset(knn.Set, split == TRUE)
test = subset(knn.Set, split == FALSE)
head(train)
head(test)

train_scaled = scale(train)
test_scaled = scale(test)
head(train_scaled)
head(test_scaled)

# Train the kNN model
k <- 10
knn_model <- knn(
    train = train_scaled,
    test = test_scaled,
    cl = train$EXP_GPA, 
    k = k
)

# confusion matrix
fac.EGPA <- test$EXP_GPA
xtable <- table(knn_model, factor(fac.EGPA))
conf.Matrix <- confusionMatrix(xtable, positive = "1")
conf.Matrix

# lift chart
# Calculate the cumulative response and lift
lift_data <- data.frame(
    Actual = as.numeric(fac.EGPA == "1"),
    Predicted = as.numeric(knn_model == "1")
)

lift_data <- lift_data[order(-lift_data$Predicted), ]

lift_data$CumulativeActual <- cumsum(lift_data$Actual)
lift_data$CumulativePercentage <- seq(1, length.out = nrow(lift_data)) / nrow(lift_data) * 100
lift_data$Lift <- lift_data$CumulativeActual / (lift_data$CumulativePercentage / 100)

# Plot the lift chart using ggplot2
ggplot(lift_data, aes(x = CumulativePercentage, y = Lift)) +
    geom_line(color = "blue", size = 2) +
    geom_segment(aes(x = 0, y = 1, xend = 100, yend = 1), linetype = "dashed", color = "red") +
    labs(title = "Lift Chart", x = "Cumulative Percentage", y = "Lift") +
    theme_minimal()

# Plot the lift chart using xyplot
xyplot(Lift ~ CumulativePercentage, data = lift_data, type = "l",
       main = "Lift Chart",
       xlab = "Cumulative Percentage",
       ylab = "Lift",
       col.line = "blue",
       lwd = 2,
       scales = list(y = list(relation = "free")),
       panel = function(x, y, ...) {
           panel.abline(h = 1, lty = 2, col = "red", ...)
           panel.xyplot(x, y, ...)
       },
       args.legend = list(title = "Legend")
)

# ROC Curve
# Create an ROC curve
roc_curve <- roc(response = as.numeric(fac.EGPA == "1"), predictor = as.numeric(knn_model == "1"))

# Plot the ROC curve
plot(roc_curve, main = "ROC Curve", col = "blue", lwd = 2, xlim = c(1, 0))

# Calculate the AUC
auc_value <- auc(roc_curve)
cat("AUC:", auc_value, "\n")

