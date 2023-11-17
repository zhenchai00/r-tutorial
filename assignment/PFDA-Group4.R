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


# Research Question 1: Does listening attentively in class increase the likelihood 
# of a student achieving a higher expected CGPA upon graduation?

# Analysis 1-1: uni-variation of student listening in classes
data.Copy <- student.Data
data.Copy$LISTENS = as.factor(data.Copy$LISTENS)
head(data.Copy)
ggplot(data = data.Copy) + 
  geom_bar(
    mapping = aes(x = as.factor(LISTENS), fill = LISTENS)
  ) +
  geom_text(
    aes(x = as.factor(LISTENS), y = after_stat(count), label = after_stat(count)),
    stat = "count",
    position = position_dodge(width = 0.9),
    vjust = -0.5,
    size = 3
  ) +
  labs(
    title = "Variation of Student Listening in Classes",
    x = "Listens",
    y = "Count"
  ) +
  scale_x_discrete(
    labels = c("1" = "Never", "2" = "Sometimes", "3" = "Always"),
    limits = c("1","2","3")
  ) +
  scale_fill_manual(
    values = c("1" = "red","2" = "yellow","3" = "green"),
    breaks = c("1","2","3"),
    labels = c("Never","Sometimes","Always")
  )

# Analysis 1-2: bi-variation between EXP_GPA and LISTENS
ggplot(data = data.Copy, aes(x = as.factor(EXP_GPA), fill = as.factor(LISTENS))) + 
  geom_bar(
    position = "dodge"
  ) +
  geom_text(
    aes(x = EXP_GPA, y = after_stat(count), label = after_stat(count)),
    stat = "count",
    position = position_dodge(width = 0.9),
    vjust = -0.5,
    size = 3
  ) +
  labs(
    title = "Covariation between Expected CGPA in Graduation and Listening in Classes",
    x = "Expected CGPA",
    y = "Count"
  ) +
  scale_x_discrete(
    labels = c("1" = "< 2.00", "2" = "2.00 - 2.49", "3" = "2.50 - 2.99", "4" = "3.00 - 3.49", "5" = "Above 3.49")
  ) +
  scale_fill_discrete(
    name = "Listening in Classes",
    labels = c("1"="Never", "2"="Sometimes", "3"="Always")
  )

ggplot(data = data.Copy) + 
  geom_bar(
    mapping = aes(x = as.factor(EXP_GPA), fill = as.factor(LISTENS))
  ) +
  geom_text(
    aes(x = EXP_GPA, y = after_stat(count), label = after_stat(count)),
    stat = "count",
    position = position_dodge(width = 0.9),
    vjust = -0.5,
    size = 3
  ) +
  labs(
    title = "Covariation between Expected CGPA in Graduation and Listening in Classes",
    x = "Expected CGPA",
    y = "Count"
  ) +
  scale_x_discrete(
    labels = c("1" = "< 2.00", "2" = "2.00 - 2.49", "3" = "2.50 - 2.99", "4" = "3.00 - 3.49", "5" = "Above 3.49")
  ) +
  scale_fill_discrete(
    name = "Listening in Classes",
    labels = c("1"="Never", "2"="Sometimes", "3"="Always")
  )

# Analysis 1-3: multi-variation between EXP_GPA, LISTENS and Partner
dat.Set <- student.Data %>%
    select(EXP_GPA, LISTENS, PARTNER)

# Convert variable to factors
dat.Set$EXP_GPA <- as.factor(dat.Set$EXP_GPA)
dat.Set$LISTENS <- as.factor(dat.Set$LISTENS)
dat.Set$PARTNER<-as.factor(dat.Set$PARTNER)

dat.Set$LISTENS <- factor(
    dat.Set$LISTENS,
    levels = c(1, 2, 3),
    labels = c("Never", "Sometimes", "Always")
)
dat.Set$EXP_GPA <- factor(
    dat.Set$EXP_GPA,
    levels = c(1, 2, 3, 4, 5),
    labels = c("<2.00", "2.00-2.49", "2.50-2.99", "3.00-3.49", "above 3.49")
)
dat.Set$PARTNER <- factor(
    dat.Set$PARTNER,
    levels = c(1, 2),
    labels = c("Yes", "No")
)

# Plot bar graph
ggplot(dat.Set, aes (x = EXP_GPA, fill = LISTENS)) + 
    geom_bar(position = "dodge") +
    labs(
        title = "Multi-Variant between Expected CGPA in Graduation, Listen in classess and have partner", 
        x = "Expected CGPA", 
        y = "Number of Students"
    ) +
    geom_text(
        stat = 'count',
        aes(label = stat(count)),
        position = position_dodge(width = 1),
        vjust = 1.5,
        size = 3
    ) +
    facet_wrap(~PARTNER, scales = "free") +
    theme_classic() +
    scale_fill_discrete(name = "Listens In Classes")

# Analysis 1-4: Chi-Square Test between EXP_GPA and LISTENS
tbl = table(data.Copy$EXP_GPA, data.Copy$LISTENS)
chisq.test(tbl)

# Analysis 1-5: Polychoric between EXP_GPA and LISTENS
polychor(data.Copy$EXP_GPA, data.Copy$LISTENS)


# Research Question 2: Does taking notes in class increase the likelihood of 
# a student achieving a higher expected CGPA upon graduation?

# Analysis 2-1: Uni-variation of student who take notes in class 
student.Data$NOTES <- as.factor(student.Data$NOTES)
ggplot(data = student.Data) +
    geom_bar(
      mapping = aes(x = as.factor(NOTES), fill = NOTES)
    ) +
    geom_text(
      aes(x = as.factor(NOTES), y = ..count.., label = after_stat(count)),
      stat = "count",
      position = position_dodge(width = 0.9),
      vjust = -0.5,
      size = 3
    ) +
    labs(
      title = "Covariation of Student Taking Notes in Classes",
      x = "Taking Notes",
      y = "Count"
    ) +
    scale_x_discrete(
      labels = c("1" = "Never", "2" = "Sometimes", "3" = "Always"),
      limits = c("1", "2", "3")
    ) +
    scale_fill_manual(values = c("1" = "red2", "2" = "blue", "3" = "darkgreen"),
                      labels=c("1" = "Never", "2" = "Sometimes", "3" = "Always")
    )

# Analysis 2-2: bi-variant for Covariation between EXP_GPA and NOTES
ggplot(data = student.Data, aes(x = as.factor(EXP_GPA), fill = as.factor(NOTES)))+
    geom_bar(
      position = "dodge"
      ) +
    geom_text(
      aes(x = EXP_GPA, y = ..count.., label = ..count..),
      stat = "count",
      position = position_dodge(width = 0.9),
      vjust = -0.5,
      size = 3
    ) +
    labs(
      title = "Covariation between Expected CGPA in Graduation and Taking Notes in Classes",
      x = "Expected CGPA",
      y = "Count"
    ) +
    scale_x_discrete(
      labels = c("1" = "< 2.00", "2" = "2.00 - 2.49", "3" = "2.50 - 2.99", "4" = "3.00 - 3.49")
    ) +
    scale_fill_discrete(
      name = "Taking Notes in Classes",
      labels = c("1" = "Never", "2" = "Sometimes","3"="Always")
    )
  
# Analysis 2-2: Stacked Bar Chart Plotting for Covariation between EXP_GPA and NOTES
ggplot(data = student.Data, aes(x = as.factor(EXP_GPA), fill = as.factor(NOTES))) +
    geom_bar(position = "fill") +
    labs(
      title = "Covariation between Expected CGPA in Graduation and Taking Notes in Classes",
      x = "Expected CGPA",
      y = "Count"
    ) +
    scale_x_discrete(
      labels = c("1" = "< 2.00", "2" = "2.00 - 2.49", "3" = "2.50 - 2.99", "4" = "3.00 - 3.49")
    ) +
    scale_fill_discrete(
      name = "Taking Notes",
      labels = c("1" = "Never", "2" = "Sometimes","3"="Always")
    )  
  
# Analysis 2-3: multi-variant for Covariation between EXP_GPA, NOTES and transport
dat.Set <- student.Data %>%
    select(EXP_GPA, NOTES, TRANSPORT)

# Convert variable to factors
dat.Set$EXP_GPA <- as.factor(dat.Set$EXP_GPA)
dat.Set$NOTES <- as.factor(dat.Set$NOTES)
dat.Set$TRANSPORT<-as.factor(dat.Set$TRANSPORT)

dat.Set$NOTES <- factor(
    dat.Set$NOTES,
    levels = c(1, 2, 3),
    labels = c("Never", "Sometimes", "Always")
)
dat.Set$EXP_GPA <- factor(
    dat.Set$EXP_GPA,
    levels = c(1, 2, 3, 4, 5),
    labels = c("<2.00", "2.00-2.49", "2.50-2.99", "3.00-3.49", "above 3.49")
)
dat.Set$TRANSPORT <- factor(
    dat.Set$TRANSPORT,
    levels = c(1, 2, 3, 4),
    labels = c("Bus", "Private Car/Taxi", "Bicyle", "Other")
)

# Plot bar graph
ggplot(dat.Set, aes (x = EXP_GPA, fill = NOTES)) + 
    geom_bar(position = "dodge") +
    labs(
        title = "Multi-Variant between Expected CGPA in Graduation, Take notes in classess and type of transportation", 
        x = "Expected CGPA", 
        y = "Number of Students"
    ) +
    geom_text(
        stat = 'count',
        aes(label = stat(count)),
        position = position_dodge(width = 1),
        vjust = 1.5,
        size = 3
    ) +
    facet_wrap(~TRANSPORT, scales = "free") +
    theme_classic() +
    scale_fill_discrete(name = "Take Notes In Classes")

# Analysis 2-4: Chi Square Test
chi_square_data<- table(student.Data$EXP_GPA, student.Data$NOTES)
chisq.test(chi_square_data)

# Analysis 2-5: Polychoric Correlation Analysis
exp.GPA.Value <- student.Data$EXP_GPA
Notes.Value<-student.Data$NOTES
polychor(exp.GPA.Value,Notes.Value)


# Research Question 3: Does having scholarships increase the 
# likelihood of a student achieving a higher expected CGPA upon graduation?

# Analysis 3-1: Uni-variant Analysis of Student with Scholarships
bar.Data <- student.Data
bar.Data$SCHOLARSHIP = as.factor(bar.Data$SCHOLARSHIP)
head(bar.Data)
ggplot(data = bar.Data) +
  geom_bar(
    mapping = aes(x = as.factor(SCHOLARSHIP))
  ) +
  geom_text(
    aes(x = as.factor(SCHOLARSHIP), y = ..count.., label = ..count..),
    stat = "count",
    position = position_dodge(width = 0.9),
    vjust = -0.5,
    size = 3
  ) +
  labs(
    title = "Variant of Students With Scholarships",
    x = "Scholarship Type",
    y = "Student Count"
  ) +
  scale_x_discrete(
    labels = c("1" = "None", "2" = "25%", "3" = "50%", "4" = "70%", "5" = "Full")
  ) 
  

# Analysis 3-2: Bi-variant Analysis For Expected GPA And Students With Scholarships
# Bar Chart Plotting
ggplot(data = student.Data, aes(x = as.factor(EXP_GPA), fill = as.factor(SCHOLARSHIP)))+
  geom_bar(
    position = "dodge"
  ) +
  geom_text(
    aes(x = EXP_GPA, y = ..count.., label = ..count..),
    stat = "count",
    position = position_dodge(width = 0.9),
    vjust = -0.5,
    size = 3
  ) +
  labs(
    title = "Covariation between Expected CGPA in Graduation and Students with Scholarships",
    x = "Expected CGPA",
    y = "Student Count"
  ) +
  scale_x_discrete(
    labels = c("1" = "< 2.00", "2" = "2.00 - 2.49", "3" = "2.50 - 2.99", "4" = "3.00 - 3.49")
  ) +
  scale_fill_discrete(
    name = "Scholarship Type",
    labels = c("1" = "None", "2" = "25%", "3" = "50%", "4" = "70%", "5" = "Full")
  )

# Stacked Bar Chart Plotting
ggplot(data = student.Data, aes(x = as.factor(EXP_GPA), fill = as.factor(SCHOLARSHIP)))+
  geom_bar(
      position = "fill"
    ) +
  labs(
    title = "Covariation between Expected CGPA in Graduation and Students with Scholarships",
    x = "Expected CGPA",
    y = "Student Count"
  ) +
  scale_x_discrete(
    labels = c("1" = "< 2.00", "2" = "2.00 - 2.49", "3" = "2.50 - 2.99", "4" = "3.00 - 3.49")
  ) +
  scale_fill_discrete(
    name = "Scholarship Type",
    labels = c("1" = "None", "2" = "25%", "3" = "50%", "4" = "70%", "5" = "Full")
  )

# Analysis 3-3: Multi-variant Analysis For Expected GPA And Students With Scholarships and Attendance to classes
dat.Set <- student.Data %>%
    select(EXP_GPA, SCHOLARSHIP, ATTEND)

# Convert variable to factors
dat.Set$EXP_GPA <- as.factor(dat.Set$EXP_GPA)
dat.Set$SCHOLARSHIP <- as.factor(dat.Set$SCHOLARSHIP)
dat.Set$ATTEND<-as.factor(dat.Set$ATTEND)

dat.Set$SCHOLARSHIP <- factor(
    dat.Set$SCHOLARSHIP,
    levels = c(1, 2, 3, 4, 5),
    labels = c("None", "25%", "50%", "75%", "Full")
)
dat.Set$EXP_GPA <- factor(
    dat.Set$EXP_GPA,
    levels = c(1, 2, 3, 4, 5),
    labels = c("<2.00", "2.00-2.49", "2.50-2.99", "3.00-3.49", "above 3.49")
)
dat.Set$ATTEND <- factor(
    dat.Set$ATTEND,
    levels = c(1, 2, 3),
    labels = c("Never", "Sometimes", "Always")
)

# Plot bar graph
ggplot(dat.Set, aes (x = EXP_GPA, fill = SCHOLARSHIP)) + 
    geom_bar(position = "dodge") +
    labs(
        title = "Multi-Variant between Expected CGPA in Graduation, Scholarship and attending to classes", 
        x = "Expected CGPA", 
        y = "Number of Students"
    ) +
    geom_text(
        stat = 'count',
        aes(label = stat(count)),
        position = position_dodge(width = 1),
        vjust = 1.5,
        size = 3
    ) +
    facet_wrap(~ATTEND, scales = "free") +
    theme_classic() +
    scale_fill_discrete(name = "Scholarship")

# Analysis 3-4:  Chi Square Test
tbl <- table(student.Data$SCHOLARSHIP, student.Data$SCHOLARSHIP)
chisq.test(tbl)


# Analysis 3-5:  Polychoric Correlation Analysis
# to get any relevant data 
exp.GPA.Value <- student.Data$SCHOLARSHIP
scholarship.Value <- student.Data$SCHOLARSHIP

# check the correlation with Polychoric method
polychor(exp.GPA.Value, scholarship.Value)


# Research Question 4: Does attending seminars or conferences related to the department 
# increase the likelihood of a student achieving a higher expected CGPA upon graduation?

# Analysis 4-1: Uni-variation of student attend to seminars or conference related to the department
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


# Analysis 4-2: Bi-variant Covariation between EXP_GPA and ATTEND_DEPT
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

# Stacked Bar Chart Plotting for Covariation between EXP_GPA and ATTEND_DEPT
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

# Analysis 4-3: Multi-variant Analysis for covariation betweent EXP_GPA, ATTEND_DEPT and WORK
# Select relevant columns
dat.Set <- student.Data %>%
    select(EXP_GPA, ATTEND_DEPT, WORK)

# Convert variable to factors
dat.Set$EXP_GPA <- as.factor(dat.Set$EXP_GPA)
dat.Set$ATTEND_DEPT <- as.factor(dat.Set$ATTEND_DEPT)
dat.Set$WORK<-as.factor(dat.Set$WORK)

dat.Set$ATTEND_DEPT <- factor(
    dat.Set$ATTEND_DEPT,
    levels = c(1, 2),
    labels = c("Yes", "No")
)
dat.Set$EXP_GPA <- factor(
    dat.Set$EXP_GPA,
    levels = c(1, 2, 3, 4, 5),
    labels = c("<2.00", "2.00-2.49", "2.50-2.99", "3.00-3.49", "above 3.49")
)
dat.Set$WORK <- factor(
    dat.Set$WORK,
    levels = c(1, 2),
    labels = c("Yes", "No")
)

# Plot bar graph
ggplot(dat.Set, aes (x = EXP_GPA, fill = ATTEND_DEPT)) + 
    geom_bar(position = "dodge") +
    labs(
        title = "Multi-Variant between Expected CGPA in Graduation, Attending Seminar/Conferences related to Department and Working", 
        x = "Expected CGPA", 
        y = "Number of Students"
    ) +
    geom_text(
        stat = 'count',
        aes(label = stat(count)),
        position = position_dodge(width = 1),
        vjust = 1.5,
        size = 3
    ) +
    facet_wrap(~WORK, scales = "free") +
    theme_classic() +
    scale_fill_discrete(
        name = "Attend Seminar/Conference",
        labels = c("1"="Yes", "2"="No")
    )

# Analysis 4-4:  Polychoric Correlation
# get the relevant data 
exp.GPA.Value <- student.Data$EXP_GPA
attendDept.Value <- student.Data$ATTEND_DEPT

# check the correlation with Polychoric method
polychor(exp.GPA.Value, attendDept.Value)

# Analysis 4-5:  Chi Square Test
tbl <- table(student.Data$EXP_GPA, student.Data$ATTEND_DEPT)
chisq.test(tbl)


# Analysis 5: Decision Tree
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


# Analysis 6: Logistic Regression
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


# Analysis 7: Naive Bayesian 
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


# Analysis 8: K Nearest Neighbors 
library(caTools)
library(class)
library(pROC)
# to copy relevant columns into a new data frames
knn.Data <- student.Data %>%
    select(EXP_GPA, LISTENS, NOTES, SCHOLARSHIP, ATTEND_DEPT)
knn.Test <- student.Data %>%
    select(EXP_GPA, LISTENS, NOTES, SCHOLARSHIP, ATTEND_DEPT)

# to convert data in every column's data type 
knn.Data$LISTENS <- factor(
    knn.Data$LISTENS,
    levels = c(1, 2, 3),
)
knn.Data$SCHOLARSHIP <- factor(
    knn.Data$SCHOLARSHIP,
    levels = c(1, 2, 3, 4, 5),
)
knn.Data$NOTES <- factor(
    knn.Data$NOTES,
    levels = c(1, 2, 3),
)
knn.Data$ATTEND_DEPT <- factor(
    knn.Data$ATTEND_DEPT,
    levels = c(1, 2),
)
knn.Data$EXP_GPA <- factor(
    knn.Data$EXP_GPA,
    levels = c(1, 2, 3, 4, 5),
)

knn.Test$LISTENS <- factor(
    knn.Test$LISTENS,
    levels = c(1, 2, 3),
)
knn.Test$SCHOLARSHIP <- factor(
    knn.Test$SCHOLARSHIP,
    levels = c(1, 2, 3, 4, 5),
)
knn.Test$NOTES <- factor(
    knn.Test$NOTES,
    levels = c(1, 2, 3),
)
knn.Test$ATTEND_DEPT <- factor(
    knn.Test$ATTEND_DEPT,
    levels = c(1, 2),
)
knn.Test$EXP_GPA <- factor(
    knn.Test$EXP_GPA,
    levels = c(1, 2, 3, 4, 5),
)

str(knn.Data)
str(knn.Test)

# Hence separate the response variable
train.Data = knn.Data[,-1]
# Separate the response variable from the test data
test.Data = knn.Test[,-1]

str(train.Data)
str(test.Data)

## Number of categories/classes
cls<-factor(knn.Data[,1])
cls


## To calculate the test accuracy of the model, we need to know the class of the test data
##Separating test data categories/class
actual<-factor(knn.Test[,1])
actual

## Creating K-NN Model using knn() function
knnmod<-knn(
    train=train.Data,
    test=test.Data,
    cl=cls,
    k=3,
    prob=FALSE
)

##Predict the new test data
predicted<-knnmod

## Calculate the classification error
check.Error = function(actual, predicted) {
    mean(actual != predicted)
}

check.Error(actual,predicted)

# Caclulate accuracy and other parameters using caret functions
report<-confusionMatrix(actual, predicted)
report

#Try different values of k
ks <- 1:10 ##Try k values from 1 to 10
##Storing errors in an array
store.Error <- rep(x=0, times = length(ks))

for (i in seq_along(ks)) {
    predicted = knn(train = train.Data,
                    test = test.Data,
                    cl = cls,
                    k = ks[i])
    store.Error[i] = check.Error(actual, predicted)
}

##Plot Error Vs K-values graph
plot(store.Error, 
     type='b', 
     col = "blue", 
     cex = 1, pch = 20,
     xlab = "K - values",
     ylab = "classification error",
     main = "Error rate vs K-Values") 