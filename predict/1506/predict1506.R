library("rprojroot")
library("data.table")
library("checkmate")
library(mlr)
requireNamespace("withr")
library(corrplot)
root = find_root(is_git_root)
set.seed(1)


source(file.path(root,"data", "readData2.R"))
data = readData(version = 1)
data$TARGET = as.double(data$TARGET)
data$TARGET[data$split == "test"] = 0

names(data)
data = createDummyFeatures(as.data.frame(data), target = "TARGET", 
                           cols = names(data)[unlist(lapply(data, function(x) is.factor(x)))],
                           method = "reference")


train = data[data$split == "train",]
test = data[data$split == "test",]
train$split = NULL
test$split = NULL

test$TARGET = NULL
train$SK_ID_CURR = NULL
SK_ID_CURR = test$SK_ID_CURR
test$SK_ID_CURR = NULL

# 一下数据在训练集中为常量 ，删除
const = unlist(lapply(train, function(x) sd(x) == 0))
constan = names(train)[const]
train[constan]= NULL
test[constan] = NULL



regr.task = makeRegrTask(id = "reg", data = as.data.frame(train), target = "TARGET")
train2 = train
train2$TARGET = as.factor(train$TARGET)
kassif.tast = makeClassifTask(id = "classif", data = as.data.frame(train2), target = "TARGET")


lrn.logis = makeLearner("classif.logreg",predict.type = "prob")
lrn.ctree = makeLearner("regr.ctree")

mod.ctree = train(lrn.ctree, regr.task)
mod.log = train(lrn.logis, kassif.tast)

task.pred.ctree = predict(mod.log, newdata = test)
task.pred.log = predict(mod.log, newdata = test)

pred.ctree = task.pred.ctree$data$response
pred.log = task.pred.log$data$prob.1
length(pred.log)


pred_ctree = data.frame(SK_ID_CURR = SK_ID_CURR, TARGET = pred.ctree)
pred_log = data.frame(SK_ID_CURR = SK_ID_CURR, TARGET = pred.log)

write.csv(pred_ctree, file.path(root,"predict", "1506","submisssions1506_2.csv"), row.names = FALSE)
write.csv(pred_log, file.path(root,"predict", "1506","submisssions1506_3.csv"), row.names = FALSE)



