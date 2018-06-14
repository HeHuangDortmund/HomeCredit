library("rprojroot")
library("data.table")
library("checkmate")
library(mlr)
requireNamespace("withr")
root = find_root(is_git_root)
set.seed(1)


# 把所有的NA删除，做一次预测
data = readData(version = 1)
data$TARGET = as.double(data$TARGET)
data$TARGET[data$split == "test"] = 0
missValue = sapply(data, function(x) sum(is.na(x))) # too many missing value
barplot(missValue)

indNA = (missValue == 0)
sum(indNA)
data = data[,indNA, with = FALSE]
names(data)
data = createDummyFeatures(as.data.frame(data), target = "TARGET", cols = names(data)[unlist(lapply(data, function(x) is.factor(x)))])

str(data)

train = data[data$split == "train",]
test = data[data$split == "test",]
train$split = NULL
test$split = NULL

test$TARGET = NULL
train$SK_ID_CURR = NULL
SK_ID_CURR = test$SK_ID_CURR
test$SK_ID_CURR = NULL



regr.task = makeRegrTask(id = "reg", data = as.data.frame(train), target = "TARGET")
regr.lrn = makeLearner("regr.ctree")

mod = train(regr.lrn, regr.task)
task.pred = predict(mod, newdata = test)
pred = task.pred$data$response
length(pred)

pred_ctree = data.frame(SK_ID_CURR = SK_ID_CURR, TARGET = pred)
write.csv(pred_ctree, file.path(root,"predict", "1506","submisssions1506_1.csv"), row.names = FALSE)

