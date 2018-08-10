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

## calculate actual feature importance
label_tr = train$TARGET
train$TARGET = NULL
lgb.tr = lgb.Dataset(data.matrix(train), label = label_tr, free_raw_data = FALSE)
lgb.params = list(
    objective = "binary",
    metric = "auc",
    boosting = "rf",
    num_leaves = 64,
    # min_data_in_leaf = 1,
    # min_sum_hessian_in_leaf = 40,
    feature_fraction = 0.9,
    bagging_fraction = 0.9,
    bagging_freq = 1,
    max_depth = 8
)
lgb.model = lgb.train(
    params = lgb.params,
    data = lgb.tr,
    num_threads = 4, # number of real CPU cores
    learning_rate = 0.02,
    nrounds = 200,
    eval_freq = 20
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
tr_score = mltools::auc_roc(predict(lgb.model, data = data.matrix(train)),label_tr)

## null importance distribution
nb_runs = 80
nb_imp = c()
for (i in 1:nb_runs){
  print(i)
  set.seed(i)
  label_tr_shuffled = label_tr[sample.int(length(label_tr))]
  lgb.tr = lgb.Dataset(data.matrix(train), label = label_tr_shuffled, free_raw_data = FALSE)
  lgb.model = lgb.train(
    params = lgb.params,
    data = lgb.tr,
    num_threads = 4, # number of real CPU cores
    learning_rate = 0.02,
    nrounds = 200,
    eval_freq = 20
  )
  tree_dt_shuffled <- lgb.model.dt.tree(lgb.model)
  # Extract elements
  tree_imp_shuffled <- tree_dt_shuffled %>%
    magrittr::extract(.,
                      i = ! is.na(split_index),
                      j = .(Gain = sum(split_gain), Cover = sum(internal_count), Frequency = .N),
                      by = "split_feature") %T>%
    data.table::setnames(., old = "split_feature", new = "Feature") %>%
    magrittr::extract(., i = order(Gain, decreasing = TRUE))
  tree_imp_shuffled$run = i
  nb_imp = rbind(nb_imp, tree_imp_shuffled)
  gc()
}

# calculate new gain scores
tree_imp_gain = c()
null_imp_gain = c()
gain_score = c()
for (i in 1:nrow(tree_imp)){
  tree_imp_gain[i] = tree_imp$Gain[i]
  null_imp_gain[i] = quantile(nb_imp$Gain[nb_imp$Feature == tree_imp$Feature[i]], 0.75)
  gain_score[i] = log(1e-10 + tree_imp_gain[i] / (1 + null_imp_gain[i]))
}
feature_score = as.data.table(cbind(tree_imp$Feature, gain_score))
feature_subset = feature_score$V1[feature_score$gain_score>0]