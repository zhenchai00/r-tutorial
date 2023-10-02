# Q1
# data = c(1,2,2,0,1,2,0,1,2)
# dataValue = ""
# convertedValue = c();
# for (value in data) {
#     if (value == 0) {
#         dataValue = "Low"
#     } else if (value == 1) {
#         dataValue = "Medium"
#     } else if (value == 2) {
#         dataValue = "High"
#     }
#     convertedValue = append(convertedValue, dataValue)
# }
# print(convertedValue)
# 
# data = c(1,2,2,0,1,2,0,1,2)
# converted_data = c("Low", "Medium", "High")[data + 1]
# print(converted_data)


data = c(1,2,2,0,1,2,0,1,2)
levels = c(0,1,2)
labels = c("Low", "Medium", "High")
converted_data = factor(data, levels = levels, labels = labels)
print(converted_data)


x = c(1,2,3,4,4,3,2,5,5)
x
x = factor(x, levels = c(1,2,3,4,5), labels = c("a","b","c","d","e"))
x


# Q2
vectors = c(1,2,3,4,5)
matrixValue = matrix(vectors, nrow = 3, ncol = 2)
matrixValue


# Q3
vectors = c("a", "b", "c", "d")
matrixValue = matrix(vectors, nrow = 2, ncol = 2)
colnames(matrixValue) = c("col1", "col2")
rownames(matrixValue) = c("row1", "row2")
matrixValue


vectors = c("a", "b", "c", "d")
matrixValue = matrix(vectors, nrow = 2, ncol = 2)
dimnames(matrixValue) = list(c("row1", "row2"), c("col1", "col2"))
matrixValue



# Q4
vectors1 = c(1,2,3,42,5,26)
vectors2 = c(68,5,41,3,29,1)
matrixValue1 = matrix(vectors1, nrow = 2, ncol = 3)
matrixValue2 = matrix(vectors2, nrow = 2, ncol = 3)
print(matrixValue1 + matrixValue2)
print(matrixValue1 - matrixValue2)
print(matrixValue1 * matrixValue2)
print(matrixValue1 / matrixValue2)


# Q5
vectors = c(1,2,3,4,5,5,6,7,8)
matrixValue = matrix(vectors, nrow = 3, ncol = 4)
matrixValue
matrixValue[2,3]
matrixValue[3,]
matrixValue[,4]


# Q6
vectors = c(1,2,3,4,5,6)
matrixValue = matrix(vectors, nrow = 2, ncol = 3)
matrixValue
result = matrixValue[2, 3] * matrixValue[1,2]
result


# Q7
vectors1 = c(1,2,3,4,5,6)
matrixValue1 = matrix(vectors1, nrow = 2, ncol = 3)
matrixValue1
vectors2 = c(1,2,3)
matrixValue2 = matrix(vectors2, nrow = 1, ncol = 3)
matrixValue2

matrixValue = rbind(matrixValue1,matrixValue2)
matrixValue



# Q8
vectors1 = c(1,2,3,4,5,6)
matrixValue1 = matrix(vectors1, nrow = 2, ncol = 3)
matrixValue1
vectors2 = c(1,2,3,2)
matrixValue2 = matrix(vectors2, nrow = 2, ncol = 2)
matrixValue2

matrixValue = cbind(matrixValue1,matrixValue2)
matrixValue



# Q9
vectors = c(1,2,3,4,5,6)
matrixValue = matrix(vectors, nrow = 2, ncol = 3)
matrixValue
min(matrixValue)
max(matrixValue)


# Q10
vectors = c(1,2,3,4,5,6,7,8,9)
matrixValue = matrix(vectors, nrow = 3, ncol = 3)
matrixValue
max = which(matrixValue == max(matrixValue), arr.ind = TRUE)
matrixValue[max]
min = which(matrixValue == min(matrixValue), arr.ind = TRUE)
matrixValue[min]


# Q11
multiArray = array(c(1:18), dim = c(3,3,2))
# dim = c(4,3,2) the first and second number 
# specifies the amount of rows and columns
multiArray


# Q12
multiArray = array(c(1:18), dim = c(3,3,2))
multiArray[2,,2]
multiArray[3,3,1]


# Q13
multiArray = array(c(1:24), dim = c(3,4,2))
multiArray


# Q14
dataFrame = data.frame(
    vec1 = c(1:4),
    vec2 = c(1:4),
    vec3 = c(1:4),
    vec4 = c(1:4)
)
dataFrame


# Q15
dataFrame = data.frame(
    vec1 = c(1:4),
    vec2 = c(5:8),
    vec3 = c(9:12),
    vec4 = c(13:16)
)
dataFrame
dataFrame[["vec3"]]


# Q16
dataFrame = data.frame(
    vec1 = c(1:4),
    vec2 = c(5:8),
    vec3 = c(9:12),
    vec4 = c(13:16)
)
dataFrame
# Access directly
dataFrame[1,]
dataFrame[2,]
# Access by name
dataFrame1 = dataFrame[c(1:4), c("vec1","vec2")]
dataFrame1


# Q17
dataFrame = data.frame(
    vec1 = c(1:4),
    vec2 = c(5:8),
    vec3 = c(9:12),
    vec4 = c(13:16)
)
dataFrame

newCol = cbind(dataFrame, vec5 = c(17:20))
newCol


# Q18
dataFrame = data.frame(
    vec1 = c(1:4),
    vec2 = c(5:8),
    vec3 = c(9:12),
    vec4 = c(13:16)
)
dataFrame

newCol = cbind(dataFrame, vec5 = c(17:20))
newCol
newRow = rbind(newCol, c(1:5))
newRow


# Q19
dataFrame = data.frame(
    vec1 = c(1:4),
    vec2 = c(5:8),
    vec3 = c(9:12),
    vec4 = c(13:16)
)
dataFrame
columnsToDrop = c("vec1", "vec3")
dataFrame = dataFrame[, !names(dataFrame) %in% columnsToDrop]
dataFrame


library("dplyr")
dataFrame = data.frame(
    vec1 = c(1:4),
    vec2 = c(5:8),
    vec3 = c(9:12),
    vec4 = c(13:16)
)
dataFrame
dataFrame = select(dataFrame, -c("vec3", "vec2"))
dataFrame


# Q20
charVar = "hello guys"
numVar = 12
vector1 = c(1,2,3,4,5)
vector2 = c("a","b","c","d","e")
dataFrame = data.frame(
    num = c(1,2,3,4,5),
    char = c("a","b","c","d","e")
)
list1 = list("abc", "def", "ghi")
list2 = list(charVar, numVar, vector1, vector2, dataFrame, list1)
list2


# Q21
vec = c(1,2,3,4,5)
mat = matrix(c("a","b","c","d"), nrow = 2, ncol = 2)
list1 = list("abc", "def", "ghi")
listContain = list(vec = vec, mat = mat, list1 = list1)
listContain
names(listContain) = c("ele1","ele2","ele3")
listContain
listContain[[1]]
listContain[[2]]