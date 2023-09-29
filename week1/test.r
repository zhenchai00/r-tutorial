x <- 5
y <-2

multiple <- function (y) y * x

func <- function () {
    x <-3 
    multiple(y)
}

func()


# vector
repeat_each = rep(c(1, 2, 3), each = 3)
repeat_each


repeat_time = rep(c(1, 2, 3), time = 3)
repeat_time


repeat_indepent = rep(c(1, 2, 3), time = c(5, 2, 1))
repeat_indepent

numbers = seq(from = 0, to = 100, by = 20)
numbers



# lists
thislist = list("apple", "banana", "cherry")
thislist
thislist[1]
thislist[1] = "blackcurrant"
thislist

length(thislist)

thislist = list("apple", "banana", "cherry")
"apple" %in% thislist
append(thislist, "orange", after = 2)

thislist = list("apple", "banana", "cherry")
newlist = thislist[-1]
newlist

thislist <- list("apple", "banana", "cherry", "orange", "kiwi", "melon", "mango")
(thislist)[2:5] 


thislist = list("apple", "banana", "cherry")
for (x in thislist) {
    print(x)
}


list1 = list("a", "b", "c")
list2 = list(1,2,3)
list3 = c(list1, list2)
list3


thisMatrix = matrix(c(1,2,3,4,5,6), nrow = 3, ncol = 2)
thisMatrix

thisMatrix = matrix(c("apple", "banana", "cherry", "orange"), nrow=2, ncol=2)
thisMatrix
thisMatrix[2, 1]
thisMatrix[2,]
thisMatrix[,2]


thismatrix <- matrix(c("apple", "banana", "cherry", "orange","grape", "pineapple", "pear", "melon", "fig"), nrow = 3, ncol = 3)
thismatrix[c(1,2),] 

thismatrix <- matrix(c("apple", "banana", "cherry", "orange","grape", "pineapple", "pear", "melon", "fig"), nrow = 3, ncol = 3)
thismatrix[, c(1,2)] 


thismatrix <- matrix(c("apple", "banana", "cherry", "orange","grape", "pineapple", "pear", "melon", "fig"), nrow = 3, ncol = 3)
newmatrix <- cbind(thismatrix, c("strawberry", "blueberry", "raspberry"))
newmatrix


thismatrix <- matrix(c("apple", "banana", "cherry", "orange","grape", "pineapple", "pear", "melon", "fig"), nrow = 3, ncol = 3)
newmatrix <- rbind(thismatrix, c("strawberry", "blueberry", "raspberry"))
newmatrix 


thismatrix <- matrix(c("apple", "banana", "cherry", "orange", "mango", "pineapple"), nrow = 3, ncol =2)
thismatrix <- thismatrix[-c(1), -c(1)]
thismatrix 


thismatrix <- matrix(c("apple", "banana", "cherry", "orange"), nrow = 2, ncol = 2)
"apple" %in% thismatrix 


thismatrix <- matrix(c("apple", "banana", "cherry", "orange"), nrow = 2, ncol = 2)
dim(thismatrix)
length(thismatrix)

for (rows in 1:nrow(thismatrix)) {
    for (columns in 1:ncol(thismatrix)) {
        print(thismatrix[rows, columns])
    }
}


Matrix1 <- matrix(c("apple", "banana", "cherry", "grape"), nrow = 2, ncol = 2)
Matrix2 <- matrix(c("orange", "mango", "pineapple", "watermelon"), nrow = 2, ncol = 2)
Matrix_Combined = rbind(Matrix1, Matrix2)
Matrix_Combined

Matrix_Combined = cbind(Matrix1, Matrix2)
Matrix_Combined


thisarray = c(1:24)
thisarray

multiarray = array(thisarray, dim = c(4,3,2))
multiarray
multiarray[2,3,2]


thisarray <- c(1:24)
multiarray <- array(thisarray, dim = c(4, 3, 2))
multiarray[c(1),,1]
multiarray <- array(thisarray, dim = c(4, 3, 2))
multiarray[,c(1),1] 


thisarray <- c(1:24)
multiarray <- array(thisarray, dim = c(4, 3, 2))
2 %in% multiarray 


thisarray <- c(1:24)
multiarray <- array(thisarray, dim = c(4, 3, 2))
for(x in multiarray){
    print(x)
} 