#Data cleaning
#lntegers have one special value: NA, while doubles have four: NA, NaN, Inf
#AII three special values NaN. Inf and -Inf can arise during division:
#Any mathematical operation involving NA Will return NA also.

0/0
Inf/Inf
NaN*3


typeof(NA)
typeof(NaN)
typeof(NULL)

is.na(NA)
is.nan(NaN)
is.null(NULL)

# The first element in the variable z in R is coerced from (numeric) to "I" (character) since the elements must have the same type
# vector only accept one type is homogeneous 
z = c(1, "hello")
typeof(z)

# There is one interesting fact that we can't add a NULL to a vector in R
(x=c (1,NA,NaN,NULL))
typeof(x)


# sum(is. na(x)) counts NA and NaN collectively, while counts NaN only.
#Search for NA
anyNA(x)
is.na(x)
sum(is.na(x))
# search for NaN
is.nan(x)
sum(is.nan(x))


library("readxl")
library("dplyr")
# read the xlsx file with readxl::read_excel
df = readxl::read_excel("C:\\Users\\GIGABYTE\\Documents\\project\\r\\week4\\Online-Retail.xlsx")


# first thing of data cleaning
sum(is.na(df))
(myFirstNA = anyNA(df.[,1]))
(mySecondNA = anyNA(df.[,2]))
(myEightNA = anyNA(df.[,8]))

# use sapply to simplify the work
# need to study about the sapply
anycolna = sapply(df, anyNA)
anycolna[1:8]
# Sample out put of the sapply 
# InvoiceNo   StockCode Description    Quantity 
# FALSE       FALSE        TRUE       FALSE 
# InvoiceDate   UnitPrice  CustomerID     Country 
# FALSE       FALSE        TRUE       FALSE

(vna = c(StockCOdeNA = sum(is.na(df$StockCode)),
         DescriptionNA = sum(is.na(df$Description)),
         CustomerIDNA = sum(is.na(df$CustomerID))))
sapply(df, function(x) sum(is.na(x)))


dfc = df
dfc$CustomerID[is.na(dfc$CustomerID)] <- 0
sum(is.na(dfc$CustomerID))
dim(dfc)
(vna = c(StockCOdeNA = sum(is.na(dfc$StockCode)),
         DescriptionNA = sum(is.na(dfc$Description)),
         CustomerIDNA = sum(is.na(dfc$CustomerID))))
sapply(dfc, function(x) sum(is.na(x)))

dfc[dfc$CustomerID == 0, ]
dfc[is.na(dfc$Description), ]
dfc[dfc$StockCode == 22139, ]
