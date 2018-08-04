rm(list=ls())
options(Java.parameters = "-Xmx8g")
memory.limit(size = 56000)
pkgs <- c("rprojroot","data.table","mlr","ggplot2","lightgbm")
lapply(pkgs,require,character.only = TRUE)
requireNamespace("withr")
root = find_root(is_git_root)
set.seed(1)

# read data
data = fread("../data/full_data.csv")
data$Add_RATIO_MEAN_PAYMENT_PREV[is.na(data$Add_RATIO_MEAN_PAYMENT_PREV)] = mean(data$Add_RATIO_MEAN_PAYMENT_PREV, na.rm = TRUE)

train = data[data$split == "train",]
test = data[data$split == "test",]
train$split = NULL
test$split = NULL

SK_ID_CURR = test$SK_ID_CURR
train$SK_ID_CURR = NULL
test$SK_ID_CURR = NULL
test$TARGET = NULL

label = train$TARGET
train$TARGET = NULL

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

# # feature selection
# lgb.task = makeClassifTask(data = train, target = 'TARGET')
# lgb.task.smote = oversample(lgb.task, rate = 10)
# fv = generateFilterValuesData(lgb.task, method = c('chi.squared')) # OutofMemory Error, Java heap space

##############################################################################################################################
# weight (imbalance binary classification, might need over/under sample)
set.seed(1)
grid_search = expand.grid(weight = 1:15)
perf_weight <- numeric(length = nrow(grid_search))
for (i in 1:nrow(grid_search)){
  lgb_weight = (label * i + 1) / sum(label * i + 1)
  lgb_train = lgb.Dataset(data = data.matrix(train),
                          label = label,
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
ggplot(grid_search,aes(x = weight, y = perf)) + geom_point() + geom_smooth() # optimal weight = 6

##############################################################################################################################
# learning_rate
grid_search = expand.grid(learning_rate = 2^(-(8:1)))
perf_learning <- numeric(length = nrow(grid_search))
set.seed(1)
for (i in 1:nrow(grid_search)){
  lgb_weight = (label * 6 + 1) / sum(label * 6 + 1)
  lgb_train = lgb.Dataset(data = data.matrix(train),
                          label = label,
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
write.table(grid_search,"grid_search_learning_rate.txt")

##############################################################################################################################
# num_leaves
grid_search = expand.grid(num_leaves = seq(50,300,50),
                          learning_rate = 0.125)
perf_num_leaves = numeric(length = nrow(grid_search))
set.seed(1)
for (i in 1:nrow(grid_search)){
  lgb_weight = (label * 6 + 1) / sum(label * 6 + 1)
  lgb_train = lgb.Dataset(data = data.matrix(train),
                          label = label,
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
grid_seach$perf = perf_num_leaves
ggplot(grid_search,aes(x = num_leaves, y = perf)) + geom_point() + geom_smooth()
write.table(grid_search,"grid_search_num_leaves.txt")