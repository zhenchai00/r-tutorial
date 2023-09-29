# "Welcome to the R world of data analytics!"
# 43 + 40
# plot(1:10)
# 
# name <- "Sponge Bob"
# print(name) # print the value of the name variable
# 
# text1 <- "R is"
# text2 <- "awesome"
# paste(text1, text2)
# 
# num1 <- 5
# num2 <- 10
# num1 + num2
# 
# num <- 5
# text <- "Some text"
# num + text 

# library(tidyverse)
# library(dplyr)
# library(ggplot2)
# "Hello World!"
# 5+5
# plot(1:15)

# name <- "john"
# age <- 40
# 
# paste(name, "atest")
# 
# age + 6

# var1 <- var2 <- var3 <- "test"
# var1
# var2
# var3
# 
# myvar <- "test"
# my_var <- "test"
# myVar <- "test"
# MYVAR <- "test"
# myvar2 <- "test"
# .myvar <- "test"
# 
# varNumeric <- 12.4
# varInteger <- 44L
# varComplex <- 9 + 3i
# varChararter <- "test"
# varLogical <- TRUE
# 
# # check datatype 
# class(varNumeric)
# class(varInteger)
# class(varComplex)
# class(varChararter)
# class(varLogical)


# # Type conversion
# x <- 1L
# y <- 2
# 
# a <- as.numeric(x)
# b <- as.integer(y)
# 
# x
# y
# 
# class(a)
# class(b)


# # Build in math
# max(5, 13, 3)
# min(5, 13, 3)
# sqrt(16)
# abs(-4.5)
# ceiling(1.4)
# floor(1.4)
# 
# str <- "Lorem ipsum dolor sit amet,
# consectetur adipiscing elit,
# sed do eiusmod tempor incididunt
# ut labore et dolore magna aliqua."
# 
# str # print the value of str 
# cat(str)
# 
# str <- 'helowrold'
# nchar(str)
# grepl("h", str)
# grepl("wrold", str)
# grepl("x", str)
# 
# 
# 10 > 9
# 10 == 9
# 10 < 9
# 
# my_var <- 3
# my_var <<- 3
# 3 -> my_var
# 3 ->> my_var
# my_var # print my_var 

# a <- 200
# b <- 33
# c <- 500
# 
# if (a > b & c > a) {
#     print("Both conditions are true")
# } 
# 
# a <- 200
# b <- 33
# c <- 500
# 
# if (a > b | a > c) {
#     print("At least one of the conditions is true")
# } 
# 
# i <- 1
# while (i < 6) {
#     print(i)
#     i <- i + 1
#     if (i == 4) {
#         break
#     }
# }
# 
# 
# i <- 1
# while (i < 6) {
#     i <- i + 1
#     if (i == 3) {
#         next
#     }
#     print(i)
# }
# 
# 
# dice <- 1
# while (dice <= 6) {
#     if (dice < 6) {
#         print("No Yahtzee")
#     } else {
#         print("Yahtzee!")
#     }
#     dice <- dice + 1
# } 
# 
# for (x in 1:10) {
#     print(x)
# }
# 
# fruits <- list("apple", "banana", "cherry")
# for (x in fruits) {
#     if (x == "cherry"){
#         break
#     }
#     print(x)
# } 
# 
# dice <- c(1, 2, 3, 4, 5, 6)
# for (x in dice) {
#     print(x)
# }
# 
# 
# adj <- list("red", "big", "tasty")
# fruits <- list("apple", "banana", "cherry")
# for (x in adj) {
#     for (y in fruits) {
#         print(paste(x, y))
#     }
# }

# my_function <- function () {
#     print("funcion here")
# }
# my_function()


# Nested_function <- function (x, y) {
#     a <- x + y
#     return (a)
# }
# Nested_function(Nested_function(2,2), Nested_function(3,3))


# Recursion function
# tri_recursion <- function (k) {
#     if (k > 0) {
#         result <- k + tri_recursion(k - 1)
#         print(result)
#     } else {
#         result = 0
#         return (result)
#     }
# }
# tri_recursion(6)


txt <- "global variable"
my_function <- function() {
    txt = "fantastic"
    paste("R is", txt)
}
my_function()
txt # print txt 


my_function <- function() {
    txt <<- "fantastic"
    paste("R is", txt)
}
my_function()
print(txt) 

plot(1:7)

install.packages(c('R6', 'jsonlite'))