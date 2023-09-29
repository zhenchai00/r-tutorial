vec <- c(1,2,3)
vec

assign("vec2", c(4,5,6))
vec2


test_list <- list(1, "hello", c(2,3,1), FALSE, 3+4i, 6L)
test_list


test_list2<-list(list(1,"a",TRUE), list("b",45L,"c"), list(1,2))
str(test_list2)


test_matrix1 <- matrix(c(1:9), ncol = 3)
test_matrix1


rownames <- c("row1", "row2", "row3")
colnames <- c("col1", "col2", "col3")
test_matrix2 <- matrix(c(1:9), ncol = 3, dimnames = list(rownames, colnames))
test_matrix2

