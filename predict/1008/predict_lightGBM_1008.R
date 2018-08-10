rm(list=ls())
pkgs <- c("rprojroot","data.table","mlr","ggplot2","lightgbm")
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

feature_subset = fread(file.path(root,"predict", "1008","feature_subset.txt"))
target_tr = train$TARGET
train = train[,.SD,.SDcols = names(train) %in% feature_subset$x]
test = test[,.SD,.SDcols = names(test) %in% feature_subset$x]

set.seed(1)
index = sample(c(1,0), nrow(train), replace = TRUE, prob = c(0.9,0.1))
label_tr = target_tr[index == 1]
label_va = target_tr[index == 0]
tr = train[index == 1,]
va = train[index == 0,]

lgb.tr = lgb.Dataset(data.matrix(tr), label = label_tr)
lgb.va = lgb.Dataset(data.matrix(va), label = label_va)

lgb.params = list(
  objective = "binary",
  metric = "auc",
  max_bin = 128,
  num_leaves = 50,
  # min_data_in_leaf = 1,
  min_sum_hessian = 0.002,
  # min_sum_hessian_in_leaf = 100, # min_child_weight
  feature_fraction = 0.95, #colsample_bytree
  bagging_fraction = 0.9, # subsample
  max_depth = 8,
  min_split_gain = 0.02,
  lambda_l1 = 0, # reg_alpha
  lambda_l2 = 0, # reg_lambda
  drop_rate = 0.9,
  max_drop = 7
)


lgb.model = lgb.train(
  params = lgb.params,
  data = lgb.tr,
  valids = list(val = lgb.va),
  num_threads = 4, # number of real CPU cores
  learning_rate = 0.02,
  nrounds = 10000,
  early_stopping_rounds = 200,
  eval_freq = 100
)

tree_dt <- lgb.model.dt.tree(lgb.model)
# Extract elements
tree_imp <- tree_dt %>%
  magrittr::extract(.,
                    i = ! is.na(split_index),
                    j = .(Gain = sum(split_gain), Cover = sum(internal_count), Frequency = .N),
                    by = "split_feature") %T>%
  data.table::setnames(., old = "split_feature", new = "Feature") %>%
  magrittr::extract(., i = order(Gain, decreasing = TRUE))
lgb.plot.importance(tree_imp, top_n = 135)

pred = predict(lgb.model, data = data.matrix(test), n = lgb.model$best_iter)
options(scipen = 3)
prediction = data.frame(SK_ID_CURR = SK_ID_CURR, TARGET = pred)
write.csv(prediction, file.path(root,"predict", "submisssions1008_1.csv"), row.names = FALSE)
