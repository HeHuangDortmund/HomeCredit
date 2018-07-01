library("rprojroot")
library("data.table")
library("checkmate")
requireNamespace("withr")
root = find_root(is_git_root)
set.seed(1)

# TODO
#0. ISNA_indicator 是否高度相关
#1. category变量与TARGET的关联, 然后re-categorize
#2. 手动删除NA太多的变量
#3. 手动删除意义上重复变量, 如MEDI,AVG
#4. 特征选择: randomforest和mlr
#5. 建模: logistic, NN(与ROC等价的loss function), xgboost

# read data
source(file.path(root,"data", "readData2.R"))
data = readData(version = 2) # version = 3 还未完成
train = data[data$split == "train",]
test = data[data$split == "test",]

names(data)
dim(data)
# missing value
missValue = sapply(data, function(x) sum(is.na(x))) # too many missing value
missValue = missValue[missValue >1100]/dim(data)[1]
barplot(missValue,las = 2,cex.lab=0.2)


# add feature SUM_OVERDUE
source(file.path(root, "preproc", "add_feature_SUM_OVERDUE.R"))
data = preproc(data)

