library("rprojroot")
library("data.table")
library("checkmate")
requireNamespace("withr")
root = find_root(is_git_root)
set.seed(1)

# read data
source(file.path(root,"data", "readData.R"))
data = readData()
train = data[data$split == "train",]
test = data[data$split == "test",]

dim(data)

# missing value
sapply(data, function(x) sum(is.na(x))) # too many missing value


# add feature SUM_OVERDUE
source(file.path(root, "preproc", "add_feature_SUM_OVERDUE.R"))
data = preproc(data)
names(data)
