df = readxl::read_excel("C:\\Users\\GIGABYTE\\Documents\\project\\r\\week4\\Online-Retail.xlsx")

sum(is.na(df))

anyColNA = sapply(df, anyNA)
anyColNA[1:8]

dfc = df

# assume empty customerid is guest customer will replace to 1 value
dfc$CustomerID[is.na(dfc$CustomerID)] = 1 
sum(is.na(dfc$CustomerID))

anyColNA = sapply(dfc, anyNA)
anyColNA[1:8]

dfc[is.na(dfc$Description), ]
dfc[dfc$StockCode == 22139, ]
dfc[!is.na(dfc$Description) & dfc$StockCode == 22139, ]

# this is to check the description with empty value and unitprice are 0
# this is because that uniprice are 0 
dfc[is.na(dfc$Description) & dfc$UnitPrice != 0, ]

# drop these rows using: 
dfc = na.omit(dfc)
# or
# dfc = dfc[!is.na(df$Description), ]
# dim(dfc)

# Q1 How many unique products in the store
length(unique(dfc$StockCode))



# Q2 Find the five most sold products
sortedProducts = dfc[order(-dfc$Quantity), ]
top5Products = head(sortedProducts, 5)
print(top5Products)



# Q3 Find the products that have different descriptions. 
dfdup = dfc[c("StockCode", "Description")]
# https://statisticsglobe.com/duplicated-function-r
duplicatedProducts = dfdup[duplicated(dfdup$Description), ]
print(duplicatedProducts)



# Q4 Find the highest spending customer
# To get the column we need 
salesData = dfc[c("CustomerID", "UnitPrice", "Quantity")]

# calculate total spend 
salesData$TotalSpend = salesData$UnitPrice * salesData$Quantity

# calculate the total spending for each customer
totalSpending = aggregate(TotalSpend ~ CustomerID, data = salesData, FUN = sum)

# find the highest spending customer
highestSpendingCustomer = totalSpending[which.max(totalSpending$TotalSpend), ]
print(highestSpendingCustomer)



# Q5 Find the invoice with maximum number of unique products
# to get the column we need
invoice = dfc[c("InvoiceNo", "StockCode")]

# calculate the number of unique stockcode for each invoice
dfq4 = tapply(invoice$StockCode, invoice$InvoiceNo, function(x) length(unique(x)))
head(dfq4, 13)

# find the invoiceid with the maximum number of unique stockcode
stockCodeInvoice = which.max(dfq4)
print(stockCodeInvoice)

invoice[invoice$InvoiceNo == 573585, ]



# Q6 Find the most popular product for each country
salesData2 = dfc[c("Country", "StockCode", "Quantity")]
sumSalesData = aggregate(Quantity ~ Country + StockCode, salesData2, sum)

# select top n values by group in r
# https://statisticsglobe.com/select-top-n-highest-values-by-group-in-r
mostPopularProducts = sumSalesData[order(sumSalesData$Quantity, decreasing = TRUE), ]
mostPopularProducts = Reduce(rbind, by(
    mostPopularProducts,
    mostPopularProducts["Country"],
    head,
    n = 1
))
print(mostPopularProducts)