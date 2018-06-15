library("rprojroot")
library("data.table")
library("checkmate")
library(mlr)
requireNamespace("withr")
root = find_root(is_git_root)
set.seed(1)

# read data
source(file.path(root,"data", "readData2.R"))
data = readData(version = 2) # version = 3 还未完成
train = data[data$split == "train",]
test = data[data$split == "test",]


plot(data$ELEVATORS_AVG, data$ELEVATORS_MEDI)

# data version 1
data = readData(version = 1)
data$TARGET = as.double(data$TARGET)
data$TARGET[data$split == "test"] = 0
train = data[data$split == "train",]
test = data[data$split == "test",]
table(train$TARGET)/dim(train)[1]

test[,TARGET := 0.08072882]
naive = test[,c("SK_ID_CURR","TARGET")]
write.csv(naive, file.path(root,"predict", "1406","submisssions1406_1.csv"), row.names = FALSE)










