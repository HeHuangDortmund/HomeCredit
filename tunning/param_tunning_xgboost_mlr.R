rm(list=ls())
pkgs <- c("rprojroot","data.table","checkmate","mlr","ggplot2","corrplot","randomForest","parallel","parallelMap")
lapply(pkgs,require,character.only = TRUE)
requireNamespace("withr")
root = find_root(is_git_root)

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

train.task = makeClassifTask(id = "class", 
                             data = as.data.frame(train), 
                             target = "TARGET")
set.seed(1)
xgb_learner = makeLearner("classif.xgboost", 
                          predict.type = "prob",
                          par.vals = list(
                            objective = "binary:logistic",
                            eval_metric = "auc",
                            nrounds = 100L,
                            eta = 0.1,
                            verbose = 1
                          ))
xgb_params = makeParamSet(makeIntegerParam("max_depth", lower = 5L, upper = 10L),
                          makeIntegerParam("min_child_weight", lower = 1L, upper = 10L),
                          makeNumericParam("subsample", lower = 0.5, upper = 1),
                          makeNumericParam("colsample_bytree", lower = 0.5, upper = 1))
resample_desc = makeResampleDesc("CV", iters = 5, stratify = TRUE)
control = makeTuneControlRandom(maxit = 1L)

parallelStartSocket(cpus = detectCores())
tuned_params = tuneParams(learner = xgb_learner,
                          task = train.task,
                          resampling = resample_desc,
                          par.set = xgb_params,
                          control = control,
                          show.info = TRUE)
parallelStop()

xgb_tuned_learner = setHyperPars(learner = xgb_learner, par.vals = tuned_params$x)
xgb_model = train(xgb_tuned_learner, train.task)
result = predict(xgb_model, newdata = test)
pred.xgb = result$data$prob.1
options(scipen = 3)
pred = data.frame(SK_ID_CURR = SK_ID_CURR, TARGET = pred.xgb)