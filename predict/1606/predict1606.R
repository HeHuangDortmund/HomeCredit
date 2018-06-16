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

dim(data)
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

# 以下数据在训练集中为常量 
const = unlist(lapply(train, function(x) sd(x) == 0))
constan = names(train)[const]
train[constan]= NULL
test[constan] = NULL



## train and validation
dim(train)



#nr = c()
#for (i in 1:267) {
#  nr[i] = sum(corr[,i] == 1)
#  cat(i," : ",nr[i],"\n")
#}




regr.task = makeRegrTask(id = "reg", data = as.data.frame(train), target = "TARGET")
train2 = train
train2$TARGET = as.factor(train$TARGET)
kassif.tast = makeClassifTask(id = "classif", data = as.data.frame(train2), target = "TARGET")


lrn.logis = makeLearner("classif.logreg",predict.type = "prob")
lrn.ctree = makeLearner("regr.ctree")

mod.ctree = train(lrn.ctree, regr.task)
mod.log = train(lrn.logis, kassif.tast)

task.pred.ctree = predict(mod.ctree, newdata = test)
task.pred.log = predict(mod.log, newdata = test)

pred.ctree = task.pred.ctree$data$response
pred.log = task.pred.log$data$prob.1
length(pred.log)


pred_ctree = data.frame(SK_ID_CURR = SK_ID_CURR, TARGET = pred.ctree)
pred_log = data.frame(SK_ID_CURR = SK_ID_CURR, TARGET = pred.log)

#write.csv(pred_ctree, file.path(root,"predict", "1506","submisssions1506_4.csv"), row.names = FALSE)
#write.csv(pred_log, file.path(root,"predict", "1506","submisssions1506_3.csv"), row.names = FALSE)


# keras 
library(tensorflow)
require(keras)
library(dplyr)

shape = dim(train)[2] - 1
inp = layer_input(shape = c(shape), name = 'inp')

combined_model <- inp %>%
  layer_dense(units=256, activation = "tanh") %>%
  layer_dropout(rate = 0.1) %>%
  layer_dense(units=128, activation = "tanh") %>%
  layer_dropout(rate = 0.1) %>%
  layer_dense(units=64, activation = "tanh") %>%
  layer_dropout(rate = 0.1) %>%
  layer_dense(units=1,activation = "sigmoid")

## 4 model build
model <- keras::keras_model(inputs = inp,
                            outputs = combined_model)


#sgd = optimizer_sgd(lr=1e-9, decay=1e-12, momentum=0.9, nesterov=FALSE)
model %>% compile(loss = "mse", optimizer = "sgd", metric="mean_absolute_error")
summary(model)


#modelFit = fit(model, x = as.matrix(train[,-105]),
#               y= as.matrix(train[,105]) , epochs = 100, batch_size = 2^13)


set.seed(2)
ind <- sample(2, nrow(train), replace = TRUE, prob=c(0.7, 0.3))
trainset <- train[ind == 1,]
valiset <- train[ind == 2,]
model %>% fit(x = as.matrix(trainset[,-105]),
              y= as.matrix(trainset[,105]),
              epochs = 100,
              batch_size = 2**14,
              #callbacks = callbacks_list,
              validation_data = list(as.matrix(valiset[,-105]), as.matrix(valiset[,105])))




y.hat = predict(model, as.matrix(test))
pred_nn = data.frame(SK_ID_CURR = SK_ID_CURR, TARGET = y.hat)
write.csv(pred_nn, file.path(root,"predict", "1606","submisssions1606_1.csv"), row.names = FALSE)
