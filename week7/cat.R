# https://www.aljazeera.com/news/longform/2023/11/1/know-their-names-palestinians-killed-in-israeli-attacks-on-gaza


# To have R Studio “soft wrap” long sentences, Tools> Global Options > Code > Editing, check the box for "Soft-wrap R source files" 


# For categorical data analysis, the following are the minimum tests: 1) Univariate 2) Bivariate 3) Multivariate 4) Chi-square test 5) Correlation between categorical variables


# 4) Chi-square Test
# http://saedsayad.com/datasets/R5.txt


# 5) Correlation between categorical variables
# https://www.statology.org/correlation-between-categorical-variables/
# 1. Tetrachoric Correlation: Used to calculate the correlation between binary categorical variables.
# 
# 2. Polychoric Correlation: Used to calculate the correlation between ordinal categorical variables.
# 
# 3. Cramer’s V: Used to calculate the correlation between nominal categorical variables.


# Encoding with factor
# To show Attribute Information instead of encoding values, use FACTOR() FUNCTION

# FACTOR() FUNCTION
# Factors are used to represent categorical data and can be unordered or ordered.
# In the factor() function output, there are levels. 

# Levels are the unique values for that vector. The first value in the levels is the reference value for which all other values are compared. The order of levels is important for statistical tests or modeling to define the reference level to which all other levels are compared. It is also important in plotting where the reference (first) level will be plotted as the first bar in bar or column plots for categorical variables. R, by default, sorts the levels alphabetically, or if they are numbers, they are sorted in increasing order as shown in the examples above.

# Another important argument is the labels() which can be used to give labels to your levels.

# Another argument is ordered argument. set ordered = TRUE if you want ordered factor. 

PATH="C:\\kt\\apu\\CT127-3-2\\Tutorials\\student_prediction.csv"
dfstu <- read.csv(PATH, sep = ",")
dfstue <- dfstu
str(dfstue) #before

#nominal
dfstue$GENDER <- factor(dfstue$GENDER,levels = c(1,2), labels =c("female","male"))
dfstue$HS_TYPE <- factor(dfstue$HS_TYPE,levels = c(1,2,3), labels =c("private","state", "other"))

#ordinal with ordered = TRUE
dfstue$GRADE <- factor(dfstue$GRADE,levels = c(0,1,2,3,4,5,6,7), labels =c("Fail","DD", "DC","CC","CB", "BB","BA", "AA"), ordered = TRUE)

str(dfstue) #after
head(dfstue)

# OUTPUT Grade (0: Fail, 1: DD, 2: DC, 3: CC, 4: CB, 5: BB, 6: BA, 7: AA)

ggplot(dfstue, aes(GENDER)) +
  geom_bar() +
  labs(title="Barchart for Gender", x="Gender", y="Count")

ggplot(dfstue, aes(HS_TYPE)) +
  geom_bar() +
  labs(title="Barchart for graduated high-school type", x="Graduated high-school type", y="Count")

ggplot(dfstue, aes(GRADE)) +
  geom_bar() +
  labs(title="Barchart for Output Grade", x="Output Grade", y="Count")



?diamonds

dim(diamonds)
str(diamonds)
head(diamonds)
tail(diamonds)
summary(diamonds)


# Position Adjustments
# There’s one more piece of magic associated with bar charts. You can color a bar chart using either the color aesthetic, or more usefully, fill:

ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut, color = cut))

ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut, fill = cut))

ggplot(data = diamonds,aes(x = cut, fill =cut)) +
  geom_bar() +
  labs(title="Barchart for quality of the cut", x="Quality of the cut", y="Count")

# Note what happens if you map the fill aesthetic to another variable, like clarity: the bars are automatically stacked. Each colored rectangle represents a combination of cut and clarity:

ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut, fill = clarity))

# The stacking is performed automatically by the position adjustment specified by the position argument. If you don’t want a stacked bar chart, you can use one of three other options: "identity", "dodge" or "fill":
#   • position = "identity" will place each object exactly where it falls in the context of the graph. 

ggplot(data = diamonds) +
  geom_bar(
    mapping = aes(x = cut, fill = clarity),
    position = "dodge"
  )

ggplot(data = diamonds) +
  geom_bar(
    mapping = aes(x = cut, fill = clarity),
    position = "fill"
  )

ggplot(data = diamonds) +
  geom_bar(
    mapping = aes(x = cut, fill = clarity),
    position = "identity"
  )

# To see that overlapping we either need to make the bars slightly transparent by setting alpha to a small value, or completely transparent by setting fill = NA:
  ggplot(
    data = diamonds,
    mapping = aes(x = cut, fill = clarity)
  ) +
  geom_bar(alpha = 1/5, position = "identity")

  ggplot(
  data = diamonds,
  mapping = aes(x = cut, color = clarity)
) +
  geom_bar(fill = NA, position = "identity")




  
  # Univariate Graphs
# The first step in any comprehensive data analysis is to explore each import variable in turn. Univariate graphs plot the distribution of data from a single variable. The variable can be categorical (e.g., race, sex, political affiliation) or quantitative (e.g., age, weight, income).
# 
# The dataset Marriage contains the marriage records of 98 individuals in Mobile County, Alabama (see Appendix A.5). We’ll explore the distribution of three variables from this dataset - the age and race of the wedding participants, and the occupation of the wedding officials.

# Categorical
# 
# The race of the participants and the occupation of the officials are both categorical variables.The distribution of a single categorical variable is typically plotted with a bar chart, a pie chart, or (less commonly) a tree map or waffle chart.

# Bar chart
# 
# In Figure 4.1, a bar chart is used to display the distribution of wedding participants by race.


library(ggplot2)

data(Marriage, package = "mosaicData")
?Marriage

# Simple barchart

# The majority of participants are white, followed by black, with very few Hispanics or American Indians.

# plot the distribution of race
ggplot(Marriage, aes(x = race)) + 
  geom_bar()

# Barchart with modified colors, labels, and title

# You can modify the bar fill and border colors, plot labels, and title by adding options to the geom_bar function. In ggplot2, the fill parameter is used to specify the color of areas such as bars, rectangles, and polygons. The color parameter specifies the color objects that technically do not have an area, such as points, lines, and borders.

# plot the distribution of race with modified colors and labels
ggplot(Marriage, aes(x=race)) + 
  geom_bar(fill = "cornflowerblue", 
           color="black") +
  labs(x = "Race", 
       y = "Frequency", 
       title = "Participants by race")

# Barchart with percentages
# Bars can represent percents rather than counts. For bar charts, the code aes(x=race) is actually a shortcut for aes(x = race, y = after_stat(count)), where count is a special variable representing the frequency within each category. You can use this to calculate percentages, by specifying y variable explicitly.

# plot the distribution as percentages
ggplot(Marriage, 
       aes(x = race, y = after_stat(count/sum(count)))) + 
  geom_bar() +
  labs(x = "Race", 
       y = "Percent", 
       title  = "Participants by race") +
  scale_y_continuous(labels = scales::percent)

# In the code above, the scales package is used to add % symbols to the y-axis labels.

# Sorting categories
# It is often helpful to sort the bars by frequency. In the code below, the frequencies are calculated explicitly. Then the reorder function is used to sort the categories by the frequency. The option stat="identity" tells the plotting function not to calculate counts, because they are supplied directly.

# calculate number of participants in each race category
library(dplyr)
plotdata <- Marriage %>%
  count(race)
plotdata

# The resulting dataset is give below.
# Table 4.1: plotdata race 	n
# American Indian 	1
# Black 	22
# Hispanic 	1
# White 	74

# This new dataset is then used to create the graph.

# Sorted bar chart
# plot the bars in ascending order

ggplot(plotdata, 
       aes(x = reorder(race, n), y = n)) + 
  geom_bar(stat="identity") +
  labs(x = "Race", 
       y = "Frequency", 
       title  = "Participants by race")

# The graph bars are sorted in ascending order. Use reorder(race, -n) to sort in descending order.

# Labeling bars
# Finally, you may want to label each bar with its numerical value.

# plot the bars with numeric labels
ggplot(plotdata, 
       aes(x = race, y = n)) + 
  geom_bar(stat="identity") +
  geom_text(aes(label = n), vjust=-0.5) +
  labs(x = "Race", 
       y = "Frequency", 
       title  = "Participants by race")

# Here geom_text adds the labels, and vjust controls vertical justification. See Annotations (Section 11.7) for more details.


# Sorted bar chart with percent labels

# Putting these ideas together, you can create a graph like the one below. The minus sign in reorder(race, -pct) is used to order the bars in descending order.

library(dplyr)
library(scales)
plotdata <- Marriage %>%
  count(race) %>%
  mutate(pct = n / sum(n),
         pctlabel = paste0(round(pct*100), "%"))


# plot the bars as percentages, 
# in decending order with bar labels
ggplot(plotdata, 
       aes(x = reorder(race, -pct), y = pct)) + 
  geom_bar(stat="identity", fill="indianred3", color="black") +
  geom_text(aes(label = pctlabel), vjust=-0.25) +
  scale_y_continuous(labels = percent) +
  labs(x = "Race", 
       y = "Percent", 
       title  = "Participants by race")


# Barchart with problematic labels
# Overlapping labels

# Category labels may overlap if (1) there are many categories or (2) the labels are long. Consider the distribution of marriage officials.

# basic bar chart with overlapping labels
ggplot(Marriage, aes(x=officialTitle)) + 
  geom_bar() +
  labs(x = "Officiate",
       y = "Frequency",
       title = "Marriages by officiate")

# In this case, you can flip the x and y axes with the coord_flip function.

# horizontal bar chart
ggplot(Marriage, aes(x = officialTitle)) + 
  geom_bar() +
  labs(x = "",
       y = "Frequency",
       title = "Marriages by officiate") +
  coord_flip()

# Alternatively, you can rotate the axis labels.

# bar chart with rotated labels
ggplot(Marriage, aes(x=officialTitle)) + 
  geom_bar() +
  labs(x = "",
       y = "Frequency",
       title = "Marriages by officiate") +
  theme(axis.text.x = element_text(angle = 45, 
                                   hjust = 1))


# Finally, you can try staggering the labels. The trick is to add a newline \n to every other label.

# bar chart with staggered labels
(lbls <- paste0(c("","\n"), levels(Marriage$officialTitle)))
ggplot(Marriage, 
       aes(x=factor(officialTitle, 
                    labels = lbls))) + 
  geom_bar() +
  labs(x = "",
       y = "Frequency",
       title = "Marriages by officiate")


# Pie chart

# Pie charts are controversial in statistics. If your goal is to compare the frequency of categories, you are better off with bar charts (humans are better at judging the length of bars than the volume of pie slices). If your goal is compare each category with the the whole (e.g., what portion of participants are Hispanic compared to all participants), and the number of categories is small, then pie charts may work for you.

#create data frame
data <- data.frame("category" = c('A', 'B', 'C', 'D'),
                   "amount" = c(25, 40, 27, 8))

#create pie chart
ggplot(data, aes(x="", y=amount, fill=category)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) 


ggplot(data, aes(x="", y=amount, fill=category)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  theme_void()

ggplot(data, aes(x="", y=amount, fill=category)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  geom_text(aes(label = paste0(amount, "%")), position = position_stack(vjust=0.5)) +
  labs(x = NULL, y = NULL, fill = NULL)

ggplot(data, aes(x="", y=amount, fill=category)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  geom_text(aes(label = paste0(amount, "%")), position = position_stack(vjust=0.5)) +
  labs(x = NULL, y = NULL, fill = NULL) +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  scale_fill_manual(values=c("#FF5733", "#75FF33", "#33DBFF", "#BD33FF"))




# Bivariate Graphs
# 
# One of the most fundamental questions in research is “What is the relationship between A and B?”. Bivariate graphs display the relationship between two variables. The type of graph will depend on the measurement level of each variable (categorical or quantitative).

# Categorical vs. Categorical
# 
# When plotting the relationship between two categorical variables, stacked, grouped, or segmented bar charts are typically used. A less common approach is the mosaic chart.
# 
# In this section, we will look at automobile characteristics contained in mpg dataset that comes with the ggplot2 package. It provides fuel efficiency data for 38 popular car models in 1998 and 2008 (see Appendix A.6).

# Stacked bar chart
# 
# Let’s examine the relationship between automobile class and drive type (front-wheel, rear-wheel, or 4-wheel drive) for the automobiles in the mpg dataset.

# stacked bar chart
ggplot(mpg, aes(x = class, fill = drv)) + 
  geom_bar(position = "stack")


# We can see for example, that the most common vehicle is the SUV. All 2seater cars are rear wheel drive, while most, but not all SUVs are 4-wheel drive.

# Stacked is the default, so the last line could have also been written as geom_bar().

# Grouped bar chart
# Grouped bar charts place bars for the second categorical variable side-by-side. To create a grouped bar plot use the position = "dodge" option.

# grouped bar plot
# Side-by-side bar chart

ggplot(mpg, aes(x = class, fill = drv)) + 
  geom_bar(position = "dodge")

# Notice that all Minivans are front-wheel drive. By default, zero count bars are dropped and the remaining bars are made wider. This may not be the behavior you want. You can modify this using the position = position_dodge(preserve = "single")" option.

# grouped bar plot preserving zero count bars
ggplot(mpg, aes(x = class, fill = drv)) + 
  geom_bar(position = position_dodge(preserve = "single"))


# Segmented bar chart
# A segmented bar plot is a stacked bar plot where each bar represents 100 percent. You can create a segmented bar chart using the position = "filled" option.

# Segmented bar chart
# bar plot, with each bar representing 100%
ggplot(mpg, aes(x = class, fill = drv)) + 
  geom_bar(position = "fill") +
  labs(y = "Proportion")


# This type of plot is particularly useful if the goal is to compare the percentage of a category in one variable across each level of another variable. For example, the proportion of front-wheel drive cars go up as you move from compact, to midsize, to minivan.

# Improving the color and labeling
# You can use additional options to improve color and labeling. In the graph below

    # factor modifies the order of the categories for the class variable and both the order and the labels for the drive variable
    # scale_y_continuous modifies the y-axis tick mark labels
    # labs provides a title and changed the labels for the x and y axes and the legend
    # scale_fill_brewer changes the fill color scheme
    # theme_minimal removes the grey background and changed the grid color

# Segmented bar chart with improved labeling and color
# bar plot, with each bar representing 100%, 
# reordered bars, and better labels and colors
library(scales)
ggplot(mpg, 
       aes(x = factor(class,
                      levels = c("2seater", "subcompact", 
                                "compact", "midsize", 
                                "minivan", "suv", "pickup")),
           fill = factor(drv, 
                         levels = c("f", "r", "4"),
                         labels = c("front-wheel", 
                                    "rear-wheel", 
                                    "4-wheel")))) + 
  geom_bar(position = "fill") +
  scale_y_continuous(breaks = seq(0, 1, .2), 
                     label = percent) +
  scale_fill_brewer(palette = "Set2") +
  labs(y = "Percent", 
       fill="Drive Train",
       x = "Class",
       title = "Automobile Drive by Class") +
  theme_minimal()



# Segmented bar chart with value labeling

# Next, let’s add percent labels to each segment. First, we’ll create a summary dataset that has the necessary labels.

# create a summary dataset
library(dplyr)
library(scales)

plotdata <- mpg %>%
  group_by(class, drv) %>%
  summarize(n = n()) %>% 
  mutate(pct = n/sum(n),
         lbl = scales::percent(pct))
plotdata

# Next, we’ll use this dataset and the geom_text function to add labels to each bar segment.

# create segmented bar chart
# adding labels to each segment

ggplot(plotdata, 
       aes(x = factor(class,
                      levels = c("2seater", "subcompact", 
                                 "compact", "midsize", 
                                 "minivan", "suv", "pickup")),
           y = pct,
          fill = factor(drv, 
                        levels = c("f", "r", "4"),
                        labels = c("front-wheel", 
                                   "rear-wheel", 
                                   "4-wheel")))) + 
  geom_bar(stat = "identity",
           position = "fill") +
  scale_y_continuous(breaks = seq(0, 1, .2), 
                     label = percent) +
  geom_text(aes(label = lbl), 
            size = 3, 
            position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Set2") +
  labs(y = "Percent", 
       fill="Drive Train",
       x = "Class",
       title = "Automobile Drive by Class") +
  theme_minimal()



# https://rkabacoff.github.io/datavis/Univariate.html






