# https://statisticsglobe.com/r-read-excel-file-xlsx-xls

install.packages("xlsx")
library("xlsx")

data("iris")
head(iris)

data1 = xlsx::read.xlsx("Online-Retail.xlsx", sheetIndex = 1)
data1



# use the readxl package by install it 
install.packages("readxl")
library("readxl")

# read the xlsx file with readxl::read_excel
data = readxl::read_excel("C:\\Users\\GIGABYTE\\Documents\\project\\r\\week4\\Online-Retail.xlsx")
data

# modify column names
colnames(data)
colnames(data) = paste0("col", 1:ncol(data))
data

str(data)
dim(data)
tail(data, 13)
summary(data)
view(data)

# data cleaning 
# https://statisticsglobe.com/data-cleaning-r

# format the missing values
# data[data == ""] = NA
data$col1
data$col1[data$col1 == null] = NA
data$col1

# remove empty row or column
data = data[rowSums(is.na(data)) != ncol(data), ]
data = data[ , colSums(is.na(data)) != nrow(data)]
data

# remove rows with missing value
data = na.omit(data)
data

# remove duplicates
data = unique(data)
data

# modify classes of columns
sapply(data, class)
data = type.convert(data, as.is = TRUE)
sapply(data, class)


# detect & remove outliers
data$col4[data$col4 %in% boxplot.stats(data$col4)$out]
data = data[! data$col4 %in% boxplot.stats(data$col4)$out, ]
data

# remove spaces in character string
data$col1 = gsub(" ", "", data$col1)
data

# combine categories
data$col3[data$col3 %in% c("b", "c")] = "a"
data





# Q1 How many unique products in the store
length(unique(data$StockCode))
# 4070


# Q2 Find the five most sold products
colnames(data)
summary(data)
