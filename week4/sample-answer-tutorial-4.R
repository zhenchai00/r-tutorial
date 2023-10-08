library(readxl)
dataSet <- readxl :: read_excel("C:\\Users\\GIGABYTE\\Documents\\project\\r\\week4\\Online-Retail.xlsx")

str(dataSet)
dim(dataSet)
head(dataSet,13)
tail(dataSet,13)

colnames(dataSet)

dataSet$Description[dataSet$Description == "NA"] <- NA
dataSet
dataSet$Description[is.na(dataSet$Description)] <- 0
dataSet[is.na(dataSet)] <- 0


#question1

library(dplyr)

unique_products <- dataSet %>% 
    distinct(StockCode)

num_unique_products <- n_distinct(unique_products$StockCode)

cat("Number of unique products in the store:", num_unique_products, "\n")


#question 2

library(dplyr)

# Group the data by product and sum the quantities sold
product_sales <- dataSet %>%
    group_by(StockCode) %>%
    summarise(TotalQuantitySold = sum(Quantity))

# Arrange the products by total quantity sold in descending order
top_selling_products <- product_sales %>%
    arrange(desc(TotalQuantitySold))

# Select the top five most sold products
top_five_products <- head(top_selling_products, 5)

# Print the result
print(top_five_products)


#question3

library(dplyr)

# Group the data by product and get unique descriptions for each product
product_descriptions <- dataSet %>%
    group_by(StockCode) %>%
    summarise(UniqueDescriptions = n_distinct(Description))

# Filter for products with more than one unique description
products_with_different_descriptions <- product_descriptions %>%
    filter(UniqueDescriptions > 1)

# Print the products with different descriptions
print(products_with_different_descriptions)


#question4

library(dplyr)

# Convert "Quantity" and "UnitPrice" columns to numeric
dataSet <- dataSet %>%
    mutate(
        Quantity = as.numeric(Quantity),
        UnitPrice = as.numeric(UnitPrice)
    )

# Filter out rows with CustomerID equal to 0 
dataSet <- dataSet %>%
    filter(CustomerID > 0)

# Calculate total spending for each customer
customer_spending <- dataSet %>%
    mutate(Total_Spending = Quantity * UnitPrice) %>%
    group_by(CustomerID) %>%
    summarise(Total_Spending = sum(Total_Spending, na.rm = TRUE))

# Find the customer with the highest total spending
highest_spending_customer <- customer_spending %>%
    filter(Total_Spending == max(Total_Spending))

# Print the highest spending customer
print(highest_spending_customer)


#question5

library(dplyr)

# Group the data by invoice and count the number of unique products
invoice_product_count <- dataSet %>%
    group_by(InvoiceNo) %>%
    summarise(UniqueProductCount = n_distinct(StockCode))

# Find the invoice with the maximum unique product count
invoice_with_max_unique_products <- invoice_product_count %>%
    filter(UniqueProductCount == max(UniqueProductCount))

# Print the invoice with the maximum unique product count
print(invoice_with_max_unique_products)


#question6

library(dplyr)

# Filter out "Unspecified" rows
filtered_data <- dataSet %>%
    filter(Country != "Unspecified")

# Group the filtered data by country and product, and count occurrences
country_product_counts <- filtered_data %>%
    group_by(Country, StockCode) %>%
    summarise(ProductCount = n())

# Find the most popular product for each country
most_popular_products <- country_product_counts %>%
    arrange(desc(ProductCount)) %>%
    group_by(Country) %>%
    slice(1)  # Select the first row (most popular product) within each country group

# Print the most popular product for each country
print(most_popular_products, n=36)