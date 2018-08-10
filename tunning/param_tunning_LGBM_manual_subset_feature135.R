rm(list=ls())
options(Java.parameters = "-Xmx8g")
memory.limit(size = 56000)
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

##############################################################################################################################
# weight (imbalance binary classification, might need over/under sample)
set.seed(1)
grid_search = expand.grid(weight = seq(1,15,2))
perf_weight <- numeric(length = nrow(grid_search))
for (i in 1:nrow(grid_search)){
  lgb_weight = (target_tr * i + 1) / sum(target_tr * i + 1)
  lgb_train = lgb.Dataset(data = data.matrix(train),
                          label = target_tr,
                          free_raw_data = FALSE,
                          weight = lgb_weight)
  params = list(objective = "binary",
                metric = "auc")
  lgb.model.cv = lgb.cv(params = params,
                        data = lgb_train,
                        learning_rate = 0.1,
                        num_threads = 4 ,
                        nrounds = 100,
                        stratified = TRUE,
                        nfold = 5,
                        early_stopping_rounds = 10)
  perf_weight[i] <- unlist(lgb.model.cv$record_evals$valid$auc$eval)[length(unlist(lgb.model.cv$record_evals$valid$auc$eval))]
  rm(lgb_train)
  gc()
}

# make plot
grid_search$perf <- perf_weight
ggplot(grid_search,aes(x = weight, y = perf)) + geom_point() + geom_smooth() # optimal weight = 5
write.table(grid_search,"grid_search_feature_subset_weight.txt")
##############################################################################################################################
# learning_rate
grid_search = expand.grid(learning_rate = 2^(-(8:1)))
perf_learning <- numeric(length = nrow(grid_search))
set.seed(1)
for (i in 1:nrow(grid_search)){
  lgb_weight = (target_tr * 5 + 1) / sum(target_tr * 5 + 1)
  lgb_train = lgb.Dataset(data = data.matrix(train),
                          label = target_tr,
                          free_raw_data = FALSE,
                          weight = lgb_weight)
  params = list(objective = "binary",
                metric = "auc",
                learning_rate = grid_search[i,'learning_rate'])
  lgb.model.cv = lgb.cv(params = params,
                        data = lgb_train,
                        num_threads = 4 ,
                        nrounds = 100,
                        stratified = TRUE,
                        nfold = 5,
                        early_stopping_rounds = 10)
  perf_learning[i] <- unlist(lgb.model.cv$record_evals$valid$auc$eval)[length(unlist(lgb.model.cv$record_evals$valid$auc$eval))]
  rm(lgb_train)
  gc()
}

grid_search$perf <- perf_learning
ggplot(grid_search,aes(x = learning_rate, y = perf)) + geom_point() + geom_smooth() # optimal learning_rate  = 2^-6(0.125)
write.table(grid_search,"grid_search_feature_subset_learning_rate.txt")
##############################################################################################################################
# num_leaves
grid_search = expand.grid(num_leaves = seq(50,300,50),
                          learning_rate = 0.125)
perf_num_leaves = numeric(length = nrow(grid_search))
set.seed(1)
for (i in 1:nrow(grid_search)){
  lgb_weight = (target_tr * 5 + 1) / sum(target_tr * 5 + 1)
  lgb_train = lgb.Dataset(data = data.matrix(train),
                          label = target_tr,
                          free_raw_data = FALSE,
                          weight = lgb_weight)
  params = list(objective = "binary",
                metric = "auc",
                learning_rate = grid_search[i,'learning_rate'],
                num_leaves = grid_search[i,'num_leaves'])
  lgb.model.cv = lgb.cv(params = params,
                        data = lgb_train,
                        num_threads = 4,
                        nrounds = 100,
                        stratified = TRUE,
                        nfold = 5,
                        early_stopping_rounds = 10)
  perf_num_leaves[i] <- unlist(lgb.model.cv$record_evals$valid$auc$eval)[length(unlist(lgb.model.cv$record_evals$valid$auc$eval))]
  rm(lgb_train,lgb.model.cv)
  gc()
}
grid_search$perf = perf_num_leaves
ggplot(grid_search,aes(x = num_leaves, y = perf)) + geom_point() + geom_smooth() # optimal num_leaves 50
write.table(grid_search,"grid_search_feature_subset_num_leaves.txt")
##############################################################################################################################
# min_data_in_leaf
grid_search = expand.grid(learning_rate = 0.125,
                          num_leaves = 50,
                          min_data_in_leaf = 2^(1:7)
                          )
perf_min_data_in_leaf <- numeric(length = nrow(grid_search))
set.seed(1)
for (i in 1:nrow(grid_search)){
  lgb_weight = (target_tr * 5 + 1) / sum(target_tr * 5 + 1)
  lgb_train = lgb.Dataset(data = data.matrix(train),
                          label = target_tr,
                          free_raw_data = FALSE,
                          weight = lgb_weight)
  params = list(objective = "binary",
                metric = "auc",
                learning_rate = grid_search[i,'learning_rate'],
                num_leaves = grid_search[i,'num_leaves'],
                min_data_in_leaf = grid_search[i, 'min_data_in_leaf'])
  lgb.model.cv = lgb.cv(params = params,
                        data = lgb_train,
                        num_threads = 4,
                        nrounds = 100,
                        stratified = TRUE,
                        nfold = 5,
                        early_stopping_rounds = 10)
  perf_min_data_in_leaf[i] <- unlist(lgb.model.cv$record_evals$valid$auc$eval)[length(unlist(lgb.model.cv$record_evals$valid$auc$eval))]
  rm(lgb_train,lgb.model.cv)
  gc()
}
grid_search$perf = perf_min_data_in_leaf
ggplot(grid_search,aes(x = min_data_in_leaf, y = perf)) + geom_point() + geom_smooth() # not sensitive to min_data_in_leaf
write.table(grid_search,"grid_search_feature_subset_num_leaves.txt")

##############################################################################################################################
# max_bin
grid_search = expand.grid(learning_rate = 0.125,
                          num_leaves = 50,
                          max_bin = 2^(5:10)
)
perf_max_bin <- numeric(length = nrow(grid_search))
set.seed(1)
for (i in 1:nrow(grid_search)){
  lgb_weight = (target_tr * 5 + 1) / sum(target_tr * 5 + 1)
  lgb_train = lgb.Dataset(data = data.matrix(train),
                          label = target_tr,
                          free_raw_data = FALSE,
                          weight = lgb_weight)
  params = list(objective = "binary",
                metric = "auc",
                learning_rate = grid_search[i,'learning_rate'],
                num_leaves = grid_search[i,'num_leaves'],
                max_bin = grid_search[i, 'max_bin'])
  lgb.model.cv = lgb.cv(params = params,
                        data = lgb_train,
                        num_threads = 4,
                        nrounds = 100,
                        stratified = TRUE,
                        nfold = 5,
                        early_stopping_rounds = 10)
  perf_max_bin[i] <- unlist(lgb.model.cv$record_evals$valid$auc$eval)[length(unlist(lgb.model.cv$record_evals$valid$auc$eval))]
  rm(lgb_train,lgb.model.cv)
  gc()
}
grid_search$perf = perf_max_bin
ggplot(grid_search,aes(x = max_bin, y = perf)) + geom_point() + geom_smooth() # optimal max_bin = 128
write.table(grid_search,"grid_search_feature_subset_max_bin.txt")
##############################################################################################################################
# feature_fraction
grid_search = expand.grid(learning_rate = 0.125,
                          num_leaves = 50,
                          max_bin = 128,
                          feature_fraction = seq(.5, 1, .05)
)
perf_feature_fraction <- numeric(length = nrow(grid_search))
set.seed(1)
for (i in 1:nrow(grid_search)){
  lgb_weight = (target_tr * 5 + 1) / sum(target_tr * 5 + 1)
  lgb_train = lgb.Dataset(data = data.matrix(train),
                          label = target_tr,
                          free_raw_data = FALSE,
                          weight = lgb_weight)
  params = list(objective = "binary",
                metric = "auc",
                learning_rate = grid_search[i,'learning_rate'],
                num_leaves = grid_search[i,'num_leaves'],
                max_bin = grid_search[i, 'max_bin'],
                feature_fraction = grid_search[i, 'feature_fraction'])
  lgb.model.cv = lgb.cv(params = params,
                        data = lgb_train,
                        num_threads = 4,
                        nrounds = 100,
                        stratified = TRUE,
                        nfold = 5,
                        early_stopping_rounds = 10)
  perf_feature_fraction[i] <- unlist(lgb.model.cv$record_evals$valid$auc$eval)[length(unlist(lgb.model.cv$record_evals$valid$auc$eval))]
  rm(lgb_train,lgb.model.cv)
  gc()
}
grid_search$perf = perf_feature_fraction
ggplot(grid_search,aes(x = feature_fraction, y = perf)) + geom_point() + geom_smooth() # optimal 0.95
write.table(grid_search,"grid_search_feature_subset_feature_fraction.txt")
##############################################################################################################################
# min_sum_hessian
grid_search = expand.grid(learning_rate = 0.125,
                          num_leaves = 50,
                          max_bin = 128,
                          feature_fraction = 0.95,
                          min_sum_hessian = seq(0, .02, .002)
)
perf_min_sum_hessian <- numeric(length = nrow(grid_search))
set.seed(1)
for (i in 1:nrow(grid_search)){
  lgb_weight = (target_tr * 5 + 1) / sum(target_tr * 5 + 1)
  lgb_train = lgb.Dataset(data = data.matrix(train),
                          label = target_tr,
                          free_raw_data = FALSE,
                          weight = lgb_weight)
  params = list(objective = "binary",
                metric = "auc",
                learning_rate = grid_search[i,'learning_rate'],
                num_leaves = grid_search[i,'num_leaves'],
                max_bin = grid_search[i, 'max_bin'],
                feature_fraction = grid_search[i, 'feature_fraction'],
                min_sum_hessian = grid_search[i, 'min_sum_hessian'])
  lgb.model.cv = lgb.cv(params = params,
                        data = lgb_train,
                        num_threads = 4,
                        nrounds = 100,
                        stratified = TRUE,
                        nfold = 5,
                        early_stopping_rounds = 10)
  perf_min_sum_hessian[i] <- unlist(lgb.model.cv$record_evals$valid$auc$eval)[length(unlist(lgb.model.cv$record_evals$valid$auc$eval))]
  rm(lgb_train,lgb.model.cv)
  gc()
}
grid_search$perf = perf_min_sum_hessian
ggplot(grid_search,aes(x = min_sum_hessian, y = perf)) + geom_point() + geom_smooth() # optimal 0.002
write.table(grid_search,"grid_search_feature_subset_min_sum_hessian.txt")
##############################################################################################################################
# lambda
grid_search = expand.grid(learning_rate = 0.125,
                          num_leaves = 50,
                          max_bin = 128,
                          feature_fraction = 0.95,
                          min_sum_hessian = 0.002,
                          lambda_l1 = seq(0, .01, .002),
                          lambda_l2 = seq(0, .01, .002)
)
perf_lambda <- numeric(length = nrow(grid_search))
set.seed(1)
for (i in 1:nrow(grid_search)){
  lgb_weight = (target_tr * 5 + 1) / sum(target_tr * 5 + 1)
  lgb_train = lgb.Dataset(data = data.matrix(train),
                          label = target_tr,
                          free_raw_data = FALSE,
                          weight = lgb_weight)
  params = list(objective = "binary",
                metric = "auc",
                learning_rate = grid_search[i,'learning_rate'],
                num_leaves = grid_search[i,'num_leaves'],
                max_bin = grid_search[i, 'max_bin'],
                feature_fraction = grid_search[i, 'feature_fraction'],
                min_sum_hessian = grid_search[i, 'min_sum_hessian'],
                lambda_l1 = grid_search[i, 'lambda_l1'],
                lambda_l2 = grid_search[i, 'lambda_l2'])
  lgb.model.cv = lgb.cv(params = params,
                        data = lgb_train,
                        num_threads = 4,
                        nrounds = 100,
                        stratified = TRUE,
                        nfold = 5,
                        early_stopping_rounds = 10)
  perf_lambda[i] <- unlist(lgb.model.cv$record_evals$valid$auc$eval)[length(unlist(lgb.model.cv$record_evals$valid$auc$eval))]
  rm(lgb_train,lgb.model.cv)
  gc()
}
grid_search$perf = perf_lambda
ggplot(grid_search,aes(x = lambda_l1, y = perf)) + geom_point() + facet_wrap(~lambda_l2, nrow = 5) # optimal lambda_l1 =0, lambda_l2 =0
write.table(grid_search,"grid_search_feature_subset_lambda.txt")
##############################################################################################################################
# drop_rate
grid_search = expand.grid(learning_rate = 0.125,
                          num_leaves = 50,
                          max_bin = 128,
                          feature_fraction = 0.95,
                          min_sum_hessian = 0.002,
                          lambda_l1 = 0,
                          lambda_l2 = 0,
                          drop_rate = seq(0, 1, .1)
)
perf_drop_rate <- numeric(length = nrow(grid_search))
set.seed(1)
for (i in 1:nrow(grid_search)){
  lgb_weight = (target_tr * 5 + 1) / sum(target_tr * 5 + 1)
  lgb_train = lgb.Dataset(data = data.matrix(train),
                          label = target_tr,
                          free_raw_data = FALSE,
                          weight = lgb_weight)
  params = list(objective = "binary",
                metric = "auc",
                learning_rate = grid_search[i,'learning_rate'],
                num_leaves = grid_search[i,'num_leaves'],
                max_bin = grid_search[i, 'max_bin'],
                feature_fraction = grid_search[i, 'feature_fraction'],
                min_sum_hessian = grid_search[i, 'min_sum_hessian'],
                lambda_l1 = grid_search[i, 'lambda_l1'],
                lambda_l2 = grid_search[i, 'lambda_l2'],
                drop_rate = grid_search[i, 'drop_rate'])
  lgb.model.cv = lgb.cv(params = params,
                        data = lgb_train,
                        num_threads = 4,
                        nrounds = 100,
                        stratified = TRUE,
                        nfold = 5,
                        early_stopping_rounds = 10)
  perf_drop_rate[i] <- unlist(lgb.model.cv$record_evals$valid$auc$eval)[length(unlist(lgb.model.cv$record_evals$valid$auc$eval))]
  rm(lgb_train,lgb.model.cv)
  gc()
}
grid_search$perf = perf_drop_rate
ggplot(grid_search,aes(x = drop_rate, y = perf)) + geom_point() + geom_smooth() # optimal drop_rate = 0.9
write.table(grid_search,"grid_search_feature_subset_drop_rate.txt")
##############################################################################################################################
# max_drop
grid_search = expand.grid(learning_rate = 0.125,
                          num_leaves = 50,
                          max_bin = 128,
                          feature_fraction = 0.95,
                          min_sum_hessian = 0.002,
                          lambda_l1 = 0,
                          lambda_l2 = 0,
                          drop_rate = 0.9,
                          max_drop = seq(1, 10, 2)
)
perf_max_drop <- numeric(length = nrow(grid_search))
set.seed(1)
for (i in 1:nrow(grid_search)){
  lgb_weight = (target_tr * 5 + 1) / sum(target_tr * 5 + 1)
  lgb_train = lgb.Dataset(data = data.matrix(train),
                          label = target_tr,
                          free_raw_data = FALSE,
                          weight = lgb_weight)
  params = list(objective = "binary",
                metric = "auc",
                learning_rate = grid_search[i,'learning_rate'],
                num_leaves = grid_search[i,'num_leaves'],
                max_bin = grid_search[i, 'max_bin'],
                feature_fraction = grid_search[i, 'feature_fraction'],
                min_sum_hessian = grid_search[i, 'min_sum_hessian'],
                lambda_l1 = grid_search[i, 'lambda_l1'],
                lambda_l2 = grid_search[i, 'lambda_l2'],
                drop_rate = grid_search[i, 'drop_rate'],
                max_drop = grid_search[i, 'max_drop'])
  lgb.model.cv = lgb.cv(params = params,
                        data = lgb_train,
                        num_threads = 4,
                        nrounds = 100,
                        stratified = TRUE,
                        nfold = 5,
                        early_stopping_rounds = 10)
  perf_max_drop[i] <- unlist(lgb.model.cv$record_evals$valid$auc$eval)[length(unlist(lgb.model.cv$record_evals$valid$auc$eval))]
  rm(lgb_train,lgb.model.cv)
  gc()
}
grid_search$perf = perf_max_drop
ggplot(grid_search,aes(x = max_drop, y = perf)) + geom_point() + geom_smooth() # optimal max_drop = 7
write.table(grid_search,"grid_search_feature_subset_max_drop.txt")
