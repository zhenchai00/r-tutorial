"Hello World!"

5
10
25

5 + 5 

print("hellow world")

# This is a comment
"Hello World!"

"Hello World!" # This is a comment 

# "Good morning!"
"Good night!"

# This is a comment
# written in
# more than just one line
"Hello World!"

name <- "John"
age <- 40

name   # output "John"
age    # output 40 

for (x in 1:10) {
    print(x)
}

text <- "awesome"
paste("R is", text) 

text1 <- "R is"
text2 <- "awesome"
paste(text1, text2)

num1 <- 5
num2 <- 10
num1 + num2 

# num <- 5
# text <- "Some text"
# num + text

# Assign the same value to multiple variables in one line
var1 <- var2 <- var3 <- "Orange"
# Print variable values
var1
var2
var3 

# Legal variable names:
myvar <- "John"
my_var <- "John"
myVar <- "John"
MYVAR <- "John"
myvar2 <- "John"
.myvar <- "John"

# Illegal variable names:
# 2myvar <- "John"
# my-var <- "John"
# my var <- "John"
# _my_var <- "John"
# my_v@ar <- "John"
# TRUE <- "John"

# my_var <- 30 # my_var is type of numeric
# my_var <- "Sally" # my_var is now of type character (aka string)


# numeric
x <- 10.5
class(x)

# integer
x <- 1000L
class(x)

# complex
x <- 9i + 3
class(x)

# character/string
x <- "R is exciting"
class(x)

# logical/boolean
x <- TRUE
class(x) 


x <- 10.5   # numeric
y <- 10L    # integer
z <- 1i     # complex 

x <- 10.5
y <- 55

# Print values of x and y
x
y

# Print the class name of x and y
class(x)
class(y) 


x <- 1000L
y <- 55L

# Print values of x and y
x
y

# Print the class name of x and y
class(x)
class(y)


x <- 3+5i
y <- 5i

# Print values of x and y
x
y

# Print the class name of x and y
class(x)
class(y)


x <- 1L # integer
y <- 2 # numeric

# convert from integer to numeric:
a <- as.numeric(x)

# convert from numeric to integer:
b <- as.integer(y)

# print values of x and y
x
y

# print the class name of a and b
class(a)
class(b) 


10 + 5
10 - 5

max(5, 10, 15)
min(5, 10, 15) 

sqrt(16)

abs(-4.7)

ceiling(1.4)
floor(1.4) 

"hello"
'hello'

str <- "Hello"
str # print the value of str 


str <- "Lorem ipsum dolor sit amet,
consectetur adipiscing elit,
sed do eiusmod tempor incididunt
ut labore et dolore magna aliqua."

str # print the value of str


str <- "Lorem ipsum dolor sit amet,
consectetur adipiscing elit,
sed do eiusmod tempor incididunt
ut labore et dolore magna aliqua."

cat(str) 


str <- "Hello World!"
nchar(str) 


str <- "Hello World!"
grepl("H", str)
grepl("Hello", str)
grepl("X", str) 


str1 <- "Hello"
str2 <- "World"
paste(str1, str2) 


10 > 9    # TRUE because 10 is greater than 9
10 == 9   # FALSE because 10 is not equal to 9
10 < 9    # FALSE because 10 is greater than 9 

a <- 10
b <- 9
a > b 


a <- 200
b <- 33
if (b > a) {
    print ("b is greater than a")
} else {
    print("b is not greater than a")
} 


my_var <- 3
my_var <<- 3
3 -> my_var
3 ->> my_var
my_var # print my_var 


i <- 1
while (i < 6) {
    print(i)
    i <- i + 1
}

i <- 1
while (i < 6) {
    print(i)
    i <- i + 1
    if (i == 4) {
        break
    }
} 

i <- 0
while (i < 6) {
    i <- i + 1
    if (i == 3) {
        next
    }
    print(i)
} 

dice <- 1
while (dice <= 6) {
    if (dice < 6) {
        print("No Yahtzee")
    } else {
        print("Yahtzee!")
    }
    dice <- dice + 1
} 

