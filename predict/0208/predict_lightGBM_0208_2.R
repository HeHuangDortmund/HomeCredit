rm(list=ls())
pkgs <- c("rprojroot","data.table","mlr","ggplot2","lightgbm")
lapply(pkgs,require,character.only = TRUE)
requireNamespace("withr")
root = find_root(is_git_root)
set.seed(1)

# read data
source(file.path(root,"data", "readData2.R"))
data = readData(version = 3) # version = 3 还未完成

# add feature
source(file.path(root, "preproc", "add_feature.R"))
preproc(data)

nameFactor = names(data)[unlist(lapply(data, function(x) is.factor(x)))]
data = createDummyFeatures(as.data.frame(data), target = "TARGET",
                           cols = nameFactor,
                           method = "reference")

train = data[data$split == "train",]
test = data[data$split == "test",]

train$split = NULL
test$split = NULL

SK_ID_CURR = test$SK_ID_CURR
train$SK_ID_CURR = NULL
test$SK_ID_CURR = NULL
test$TARGET = NULL

const = c("CODE_GENDER.miss",
          "NAME_INCOME_TYPE.miss",
          "NAME_EDUCATION_TYPE.miss",
          "NAME_FAMILY_STATUS.miss",
          "NAME_HOUSING_TYPE.miss",
          "REGION_RATING_CLIENT.miss",
          "REGION_RATING_CLIENT_W_CITY.miss",
          "WEEKDAY_APPR_PROCESS_START.miss",
          "ORGANIZATION_TYPE.miss")
train[,const]= NULL
test[,const] = NULL

index = sample(c(1,0), nrow(train), replace = TRUE, prob = c(0.9,0.1))
train$TARGET = as.numeric(train$TARGET)
label_tr = train$TARGET[index == 1]
label_va = train$TARGET[index == 0]
train$TARGET = NULL
tr = train[index == 1,]
va = train[index == 0,]

lgb.tr = lgb.Dataset(data.matrix(tr), label = label_tr)
lgb.va = lgb.Dataset(data.matrix(va), label = label_va)

lgb.params = list(
  objective = "binary",
  metric = "auc",
  min_data_in_leaf = 1,
  min_sum_hessian_in_leaf = 100,
  feature_fraction = 0.9,
  bagging_fraction = 0.9,
  max_depth = 8
)

lgb.model = lgb.train(
  params = lgb.params,
  data = lgb.tr,
  valids = list(val = lgb.va),
  learning_rate = 0.02,
  num_leaves = 64,
  num_threads = 4, # number of real CPU cores
  nrounds = 3500,
  early_stopping_rounds = 1000,
  eval_freq = 50
)

imp = lgb.importance(lgb.model, percentage = TRUE)
lgb.plot.importance(imp, top_n = 100)
pred = predict(lgb.model, data = data.matrix(test), n = lgb.model$best_iter)
options(scipen = 3)
prediction = data.frame(SK_ID_CURR = SK_ID_CURR, TARGET = pred)
write.csv(prediction, file.path(root,"predict", "submisssions0208_2.csv"), row.names = FALSE)

tr_subset = tr[,names(train) %in% imp$Feature]
va_subset = va[,names(train) %in% imp$Feature]

lgb.tr = lgb.Dataset(data.matrix(tr_subset), label = label_tr)
lgb.va = lgb.Dataset(data.matrix(va_subset), label = label_va)

lgb.params = list(
  objective = "binary",
  metric = "auc",
  min_data_in_leaf = 1,
  min_sum_hessian_in_leaf = 100,
  feature_fraction = 0.9,
  bagging_fraction = 0.9,
  max_depth = 8
)

lgb.model = lgb.train(
  params = lgb.params,
  data = lgb.tr,
  valids = list(val = lgb.va),
  learning_rate = 0.02,
  num_leaves = 64,
  num_threads = 4, # number of real CPU cores
  nrounds = 3500,
  early_stopping_rounds = 1000,
  eval_freq = 50
)

imp_subset = lgb.importance(lgb.model, percentage = TRUE)
lgb.plot.importance(imp_subset, top_n = 100)
test_subset = test[,names(test) %in% imp$Feature]
pred = predict(lgb.model, data = data.matrix(test_subset), n = lgb.model$best_iter)
options(scipen = 3)
prediction = data.frame(SK_ID_CURR = SK_ID_CURR, TARGET = pred)
write.csv(prediction, file.path(root,"predict", "submisssions0208_3.csv"), row.names = FALSE)
