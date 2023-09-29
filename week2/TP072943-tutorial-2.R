# typeof(NA)
# getClass(TRUE)
# 
# a <- c("test", "hello")
# ls(a)
# 
# v1 <- seq(1, 10, length.out())

# question 1
a1 <- 5 + 17
a1

a2 <- 10 -7
a2

a3 <- 4 * 3 * 5
a3

a4 <- 18 / 9
a4

a5 <- 9 %% 4
a5

a6 <- (5 * 8) + (12 - 7)
a6


# question 2 
weeks <- 4
print(weeks)
hoursPerWeek = 40
print(hoursPerWeek)
assign('hourlyRate', 50)
print(hourlyRate)
x = 100
x
rm(x) # this rm() is to remove variable value
x # here will be error with not found


# question 3 
c1 = "hui"   # character
c2 = 123     # numeric
c3 = TRUE    # logical
c4 = NA      # logical
c5 = 1L      # integer
c6 = 3i      # complex
c7 = NULL    # special class = class = NULL

class(c1)
class(c2)
class(c3)
class(c4)
class(c5)
class(c6)
class(c7)

is.numeric(c1)
is.numeric(c2)
is.numeric(c3)
is.numeric(c4)
is.numeric(c5)
is.numeric(c6)
is.numeric(c7)

is.integer(c1)
is.integer(c2)
is.integer(c3)
is.integer(c4)
is.integer(c5)
is.integer(c6)
is.integer(c7)

is.character(c1)
is.character(c2)
is.character(c3)
is.character(c4)
is.character(c5)
is.character(c6)
is.character(c7)

is.logical(c1)
is.logical(c2)
is.logical(c3)
is.logical(c4)
is.logical(c5)
is.logical(c6)
is.logical(c7)


# question 4
test_Marks = c(4, 22, 12, 30, 29)
final_Marks = c(0, 70, 50, 40, 1)
total_Marks = test_Marks + final_Marks
print(test_Marks)
print(final_Marks)
print(total_Marks)


# question 5
pass = total_Marks >= 50
print(pass)


# question 6 
min(total_Marks)
max(total_Marks)
mean(total_Marks)
median(total_Marks)


# Take home exercise 1 
names(total_Marks) = c("a", "b", "c", "d", "e")
print(total_Marks)


# Take home exercise 2
round(547.8) 
length(total_Marks) 
sqrt(9) 
substr("abcd", 2,4)
strsplit("hello world", " ") 
paste("welcome to", "PFDA") 
nchar("hello") 