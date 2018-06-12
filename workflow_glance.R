library("rprojroot")
library("data.table")
library("checkmate")
requireNamespace("withr")
root = find_root(is_git_root)
set.seed(1)

# read data
source(file.path(root,"data", "readData.R"))
data = readData()


