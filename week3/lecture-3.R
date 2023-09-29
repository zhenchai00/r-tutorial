# vector factor
# This is to convert values stored in data vector from Arabic to Latin numeral.

data <- c(1,2,3,3,2,1,2,1,2,3,2,2,2,2,2)
fdata <- factor(data)
fdata

rdata = factor(data, labels = c("I", "II", "III"))
rdata

mx = matrix()
mx