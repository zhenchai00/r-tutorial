# ggplot2
# https://ggplot2.tidyverse.org/

# “Tidyverse” packages include among others:
# 1. dplyr: a suite of functions for working with data frames.
# 2. tidyr: for tidying data.
# 3. magrittr: defines the %>% operator for chaining functions together in a series of operations on data.
# 4. ggplot2: for data visualization.

# Installation:
# The easiest way to get ggplot2 is to install the whole tidyverse:
install.packages("tidyverse")

# Alternatively, install just ggplot2:
install.packages("ggplot2")

# Loading required package:
library(tidyverse)
library(ggplot2)

# Search for data
data(package = .packages(all.available = TRUE))
# Loading data
data(diamonds,package="ggplot2")
data("economics")

dim(mpg)
str(mpg)
head(mpg)
tail(mpg)
summary(mpg)

# Think first:
# 1. How many variable(s)? Univariate, Bivariate and Multivariate
# 2. Data type?
# 3. How to convey your intention and messages effectively?

# Univariate
# dispersion or spread of data (range, minimum, maximum, quartiles, variance and standard deviation) 
# frequency distribution tables, histograms, pie charts, frequency polygon and bar charts. 

# Bivariate 
# Bivariate data analysis involves comparisons, relationships, causes and explanations. 
# The variables are typically plotted on the X and Y axis on a graph for better data understanding, 
# with one being independent and the other dependent.
# Check for Correlation

# Multivariate
# When the data involves three or more variables,
# Some of the techniques are regression analysis, path analysis, factor analysis and multivariate analysis of variance (MANOVA).


# Every ggplot2 plot has three key components:
# 1. data
# 2. A set of aesthetic mappings between variables in the data and visual properties, and
# 3. At least one layer which describes how to render each observation.

# ggplot(data = <DATA>, mapping = aes(<MAPPINGS>)) +
#   <GEOM_FUNCTION>()

# common mistake
ggplot(mpg, aes(x = displ, y = hwy))
+ geom_point()

ggplot(mpg, aes(x = displ, y = hwy)) + 
  geom_point()

# Not recommended
# ggplot(data = <DATA>) +
#   <GEOM_FUNCTION>(mapping = aes(<MAPPINGS>))

# 1. data
# ggplot2 starts a plot with ggplot(), 
# which creates a coordinate system for layer addition, 
# and initializes the dataset, creating an empty graph.
ggplot(data = mpg)

# 2. A set of aesthetic mappings between variables
# The mapping argument is always paired with aes(), and the x and y arguments
# of aes() specify which variables to map to the x- and y-axes.
ggplot(data = mpg, aes(x = displ, y = hwy))

# 3. Geom functions add the graphical elements of the plot
ggplot(mpg, aes(x = displ, y = hwy)) + geom_point()

# You can add a third variable, like class, to a two-dimensional scatterplot 
# by mapping it to an aesthetic. An aesthetic is a visual prop‐ erty of the objects in your plot.
# Aesthetics include things like the size, the shape, or the color of your points. 

# Common plot aesthetics you might want to specify include:
# 1. x: Position on x-axis
# 2. y: Position on y-axis
# 3. shape: Shape
# 4. color: Color of border of elements
# 5. fill: Color of inside of elements
# 6. size: Size
# 7. alpha: Transparency (1: opaque; 0: transparent)
# 8. linetype: Type of line (e.g., solid, dashed)

ggplot(data = mpg, mapping = aes(x = displ, y = hwy, color = class)) + 
  geom_point()

ggplot(data = mpg, mapping = aes(x = displ, y = hwy, size = class)) + 
  geom_point()

ggplot(data = mpg, mapping = aes(x = displ, y = hwy, alpha = class)) + 
  geom_point()

ggplot(data = mpg, mapping = aes(x = displ, y = hwy, shape = class)) + 
  geom_point()

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point(color = "blue")


ggplot(data = mpg,mapping = aes(x = displ, y = hwy)) +
  geom_point() +  facet_wrap(~ class, nrow = 2)

ggplot(data = mpg,mapping = aes(x = displ, y = hwy)) +
  geom_point() +  facet_grid(drv ~ cyl)



# 3. Geom functions add the graphical elements of the plot
# line plots use geom_line()
# path plots use geom_path()
# bar charts use geom_bar() 
# histogram use geom_histogram() 
# frequency polygons use geom_freqpoly() 
# boxplots use geom_boxplot()
# Scatterplots use geom_point()

# line plots use geom_line()
ggplot(economics, aes(x=date, y=pop)) + 
  geom_line()

# path plots use geom_path()
ggplot(economics, aes(unemploy / pop, uempmed, colour = date)) +
  geom_path()

# bar charts use geom_bar() 
ggplot(diamonds, aes(x = cut)) +  
  geom_bar()

# histogram use geom_histogram() 
ggplot(diamonds, aes(x=carat)) + 
  geom_histogram()

# frequency polygons use geom_freqpoly() 
ggplot(diamonds, aes(x=carat)) +
  geom_freqpoly()

# boxplots use geom_boxplot()
ggplot(diamonds, aes(y=carat, x=1)) + 
  geom_boxplot()

# Scatterplots use geom_point()
ggplot(diamonds, aes(x=carat, y=price)) + 
  geom_point()

# Two categorical variables: can be explored by counting the number of observations for each combination.
ggplot(diamonds, aes(x = cut, y = color)) + 
  geom_count() +
  labs(title="The co-variation between diamond's cut quality and color", x="cut", y="color")

# A categorical and continuous variables: can be explored using boxplots.
ggplot(diamonds, aes(x = color, y = price)) + 
  geom_boxplot() +
  labs(title="The co-variation between diamond's color and price", x="color", y="price")
