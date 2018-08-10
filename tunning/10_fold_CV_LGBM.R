rm(list=ls())
pkgs <- c("rprojroot","data.table","mlr","ggplot2","lightgbm","caret","dplyr")
lapply(pkgs,require,character.only = TRUE)
requireNamespace("withr")
root = find_root(is_git_root)
set.seed(1)

# read data
source(file.path(root,"data", "readData2.R"))
data = readData(version = 3) # version = 3 还未完成

varDrop_ISNA = c("NA_YEARS_BEGINEXPLUATATION",
                 "NA_FLOORSMAX",
                 "NA_LIVINGAREA",
                 "NA_ENTRANCES",
                 "NA_APARTMENT", 
                 "NA_ELEVATORS",
                 "NA_FLOORSMIN",
                 "NA_LIVINGAPARTMENTS",
                 "NA_NONLIVINGAPARTMENTS",
                 "NA_COMMONAREA",
                 "NA_BUREAU",
                 "NA_DAYS_CREDIT_ENDDATE",
                 "NA_AMT_ANNUITY_MEAN",
                 "NA_INSTALLMENT_PAYMENT")
# 去除意义上重复的变量
varDrop_Replicated = c("YEARS_BEGINEXPLUATATION_MODE",
                       "FLOORSMAX_MODE",
                       "LIVINGAREA_MODE",
                       "YEARS_BEGINEXPLUATATION_MEDI",
                       "FLOORSMAX_MEDI",
                       "LIVINGAREA_MEDI")
data = data[,c(varDrop_ISNA,varDrop_Replicated) := NULL]

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

set.seed(1)
folds = caret::createFolds(factor(train$TARGET), k = 10) # stratified partitioning
train$TARGET = as.numeric(train$TARGET)

cv_results = lapply(folds, function(x) {
label_tr = train$TARGET[-x]
label_va = train$TARGET[x]
tr = train[-x,]
va = train[x,]
tr$TARGET = NULL
va$TARGET = NULL

lgb.tr = lgb.Dataset(data.matrix(tr), label = label_tr)
lgb.va = lgb.Dataset(data.matrix(va), label = label_va)

lgb.params = list(
  objective = "binary",
  metric = "auc",
  num_leaves = 64,
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
  num_threads = 4, # number of real CPU cores
  learning_rate = 0.02,
  nrounds = 3500,
  early_stopping_rounds = 1000
)
gc()
return(lgb.model$best_score)
})

imp = lgb.importance(lgb.model, percentage = TRUE)
lgb.plot.importance(imp, top_n = 100)