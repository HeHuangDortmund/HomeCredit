library("rprojroot")
library("data.table")
library("checkmate")
requireNamespace("withr")
root = find_root(is_git_root)
set.seed(1)

# read data
source(file.path(root,"data", "readData2.R"))
data = readData(version = 2) # version = 3 还未完成
train = data[data$split == "train",]
test = data[data$split == "test",]

names(data)
dim(data)
# missing value
missValue = sapply(data, function(x) sum(is.na(x))) # too many missing value
barplot(missValue)

# add feature SUM_OVERDUE
source(file.path(root, "preproc", "add_feature_SUM_OVERDUE.R"))
data = preproc(data)

