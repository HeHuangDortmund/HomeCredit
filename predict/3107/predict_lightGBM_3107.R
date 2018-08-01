rm(list=ls())
memory.limit(size = 56000)
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
# ##########################################################################################
# fwrite(data,"full_data.csv") # 此处save data
# data = fread("../data/full_data.csv")
# ##########################################################################################

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

set.seed(1)
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
  learning_rate = 0.05,
  num_leaves = 64,
  max_bin = 63,
  max_depth = 8,
  min_data_in_leaf = 1,
  min_sum_hessian_in_leaf = 100,
  feature_fraction = 0.9,
  bagging_fraction = 0.9,
  bagging_freq = 100,
  drop_rate = 0.5,
  lambda_l1 = 0.001,
  lambda_l2 = 0.002
)

lgb.model = lgb.train(
  params = lgb.params,
  data = lgb.tr,
  valids = list(val = lgb.va),
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
write.csv(prediction, file.path(root,"predict", "submisssions3107_4.csv"), row.names = FALSE)

# train$TARGET = as.numeric(train$TARGET)
# label = train$TARGET
# train$TARGET = NULL
# 
# grid_search = expand.grid(weight = 1:15)
# perf_weight <- numeric(length = nrow(grid_search))
# for (i in 1:nrow(grid_search)){
#   lgb_weight = (label * i + 1) / sum(label * i + 1)
#   lgb_train = lgb.Dataset(data = data.matrix(train),
#                           label = label,
#                           free_raw_data = FALSE,
#                           weight = lgb_weight)
#   params = list(objective = "binary",
#                 metric = "auc")
#   lgb.model.cv = lgb.cv(params = params,
#                         data = lgb_train,
#                         learning_rate = 0.1,
#                         num_threads = 4 ,
#                         nrounds = 300,
#                         stratified = TRUE,
#                         nfold = 10,
#                         early_stopping_rounds = 10,
#                         eval_freq = 50)
#   perf_weight[i] <- unlist(lgb.model.cv$record_evals$valid$auc$eval)[length(unlist(lgb.model.cv$record_evals$valid$auc$eval))]
#   rm(lgb_train)
#   gc()
# }