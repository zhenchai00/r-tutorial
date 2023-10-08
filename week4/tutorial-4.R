# https://statisticsglobe.com/r-read-excel-file-xlsx-xls
install.packages("xlsx")
library("xlsx")
data1 = xlsx::read.xlsx("Online-Retail.xlsx", sheetIndex = 1)
data1

# use the readxl package by install it 
install.packages("readxl")
library("readxl")
library(dplyr)

# read the xlsx file with readxl::read_excel
data = readxl::read_excel("C:\\Users\\GIGABYTE\\Documents\\project\\r\\week4\\Online-Retail.xlsx")
data

# modify column names
# colnames(data)
# colnames(data) = paste0("col", 1:ncol(data))
# data

# Remove missing value
# cleanData = na.omit(data)

# Filling missing value in a numeric column with mean
# data$UnitPrice[is.na(data$UnitPrice)] = mean(data$UnitPrice, na.rm = TRUE)
# data

# To remove the empty rows 
data = data[rowSums(is.na(data)) != ncol(data), ]
data

# To remove the empty column
data = data[, colSums(is.na(data)) != nrow(data)]
data

# To remove any of the NA value in any row or columns
# data = na.omit(data)

# Check classes of columns 
sapply(data, class) # check the column class data type
data = type.convert(data, as.is = TRUE) # to convert the column class data type 
sapply(data, class) # check the column class data type

# Detect Outliers
data$UnitPrice[data$UnitPrice %in% boxplot.stats(data$UnitPrice)$out]

#####################################################

library("readxl")
library("dplyr")
# read the xlsx file with readxl::read_excel
data = readxl::read_excel("C:\\Users\\GIGABYTE\\Documents\\project\\r\\week4\\Online-Retail.xlsx")

str(data)
dim(data)
head(data, 13)
tail(data, 13)
summary(data)
View(data)
anyNA(data)

cat("Rows:", nrow(data), "\tColumns:", ncol(data))


# Check for missing value in each column
colSums(is.na(data))

# summary statistics, include missing value
summary(data)

# replace the empty data with other value
data$CustomerID[is.na(data$CustomerID)] = 1 # replace the empty to 1 
colSums(is.na(data))

# replace the description value based on matching stockcode and unitprice
duplicates = duplicated(data$StockCode) & duplicated(data$StockCode, fromLast = TRUE) &
    duplicated(data$UnitPrice) & duplicated(data$UnitPrice, fromLast = TRUE)

data2 = data

for (i in 1:nrow(data2)) {
    if (is.na(data2$Description[i]) && duplicates[i]) {
        stockPriceMatch = data2$StockCode == data2$StockCode[i] &
                            data2$UnitPrice == data2$UnitPrice[i] &
                            !is.na(data2$Description)
        if (any(stockPriceMatch)) {
            data2$Description[i] = data2$Description[stockPriceMatch][1]
        }
    }
}

colSums(is.na(data))
colSums(is.na(data2))

for (i in 1:nrow(data2)) {
    if (is.na(data2$Description[i])) {
        print(data2$Description[i])
    }
}

data2$Description[is.na(data2$Description)] = "N/A" # replace the empty to 1 
colSums(is.na(data2))



# Q1 How many unique products in the store
# basic R unique
length(unique(data2$StockCode))

# used dplyr package
library("dplyr")
n_distinct(data2$StockCode)
# 4070



# Q2 Find the five most sold products
library("dplyr")
# group the product and sum with Quantity by assign new variable
productSales = data2 %>% 
    group_by(StockCode) %>%
    summarise(quantityOfSales = sum(Quantity))
productSales

# sort the updated data set with descending
topSellProduct = productSales %>%
    arrange(desc(quantityOfSales))

# list top five record only
head(topSellProduct, 5)

# StockCode quantityOfSales
# <chr>               <dbl>
#     1 22197               56450
# 2 84077               53847
# 3 85099B              47363
# 4 85123A              38830
# 5 84879               36221



# Q3 Find the products that have different descriptions. 
library("dplyr")
# group the product with filter unique Description by assign new variable
productDescription = data2 %>%
    group_by(StockCode) %>%
    summarise(uniqueDescription = n_distinct(Description))
productDescription

# filter out the record more than 2
productWithDiffDescription = productDescription %>% 
    filter(uniqueDescription > 1)

print(productWithDiffDescription$StockCode)



# Q4 Find the highest spending customer
library(dplyr)
# Convert Quantity and UnitPrice datatype to numeric
dataQ4 = data2 %>% 
    mutate(
        Quantity = as.numeric(Quantity),
        UnitPrice = as.numeric(UnitPrice)
    )

# Filter guest customer as assigned as 1 
dataQ4 = dataQ4 %>% 
    filter(CustomerID > 1)

# calculate each customer total spending
customerSpending = dataQ4 %>%
    mutate(
        totalSpending = UnitPrice * Quantity
    ) %>%
    group_by(CustomerID) %>%
    summarise(totalSpending = sum(totalSpending, na.rm = TRUE))

# filter show only the highest spending amount
mostSpendCustomer = customerSpending %>%
    filter(totalSpending == max(totalSpending))
mostSpendCustomer



# Q5 Find the invoice with maximum number of unique products
library(dplyr)
# group by invoice and remove duplicated stockcode
invoiceProductCount = data2 %>%
    group_by(InvoiceNo) %>%
    summarise(uniqueProductPerCount = n_distinct(StockCode))

# calculate and filter the total stockcode in invoice
invoiceMaxUniqueProduct = invoiceProductCount %>%
    filter(uniqueProductPerCount == max(uniqueProductPerCount))
invoiceMaxUniqueProduct



# Q6 Find the most popular product for each country

# to check got any un related country name
country = unique(data2$Country)
country

# filter out the Unspecified value from country
dataQ6 = data2 %>%
    filter (Country != "Unspecified")

# group country and stockcode and count the total product
productCountryBuyCount = dataQ6 %>%
    group_by(Country, StockCode) %>%
    summarise(ProductCount = n())

# sort by desc product quantity and remove the duplicated value
mostPopularProduct <- productCountryBuyCount %>%
    arrange(desc(ProductCount)) %>%
    group_by(Country) %>%
    slice(1)  # Select the first row (most popular product) within each country group
mostPopularProduct



# Q7 Which are the two products that have customers purchased together often
library(dplyr)

# Group by InvoiceNo and summarize products in each invoice
product_combinations <- data2 %>%
    group_by(InvoiceNo) %>%
    summarise(products = list(StockCode)) %>%
    filter(length(products) > 1) %>%
    .$products

# Create a function to generate pairs of products from a list of products
generate_pairs <- function(products) {
    if(length(products) >= 2) {
        sort(combn(products, 2, paste, collapse = "-"))
    } else {
        character(0) # return an empty character vector if there are less than 2 products
    }
}

# Apply the function to product_combinations and count the occurrences of each pair
pair_counts <- unlist(lapply(product_combinations, generate_pairs)) %>%
    table()

# Convert the table to a data frame and arrange by frequency
pair_df <- as.data.frame(pair_counts, stringsAsFactors = FALSE)
names(pair_df) <- c("ProductPair", "Frequency")
pair_df <- pair_df %>%
    arrange(desc(Frequency))

# Extract the most frequent product pair
most_frequent_pair <- pair_df[1, ]
print(pair_df)