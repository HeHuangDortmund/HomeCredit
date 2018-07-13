pkgs <- c("rprojroot","data.table","checkmate","mlr","psych", "xgboost","corrplot","randomForest")
lapply(pkgs,require,character.only = TRUE)
requireNamespace("withr")
root = find_root(is_git_root)
set.seed(1)

source(file.path(root,"data", "readData2.R"))
data = readData(version = 3)

# nameNumeric = names(data)[unlist(lapply(data, function(x){length(unique(x))})) != 2 & 
#                           unlist(lapply(data, function(x) !is.factor(x)))]
# nameNumeric = nameNumeric[-c(1, length(nameNumeric))]
nameFactor = names(data)[unlist(lapply(data, function(x) is.factor(x)))]

#data = normalizeFeatures(as.data.frame(data), target = "TARGET", cols = nameNumeric, method = "center")
#data = normalizeFeatures(as.data.frame(data), target = "TARGET", cols = nameNumeric, method = "scale")

varNADrop = c("NA_YEARS_BEGINEXPLUATATION",
              "NA_FLOORSMAX",
              "NA_TOTALAREA_MODE",
              "NA_BUREAU",
              "NA_DAYS_CREDIT_ENDDATE",
              "NA_INSTALLMENT_PAYMENT")
varREDrop = c("YEARS_BEGINEXPLUATATION_MODE",
              "FLOORSMAX_MODE",
              "LIVINGAREA_MODE",
              "YEARS_BEGINEXPLUATATION_MEDI",
              "FLOORSMAX_MEDI",
              "LIVINGAREA_MEDI")

data = data[,c(varNADrop,varREDrop) := NULL]

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

const = unlist(lapply(train, function(x) sd(x) == 0))
constan = names(train)[const]
train[constan]= NULL
test[constan] = NULL
# temp = removeConstantFeatures(train)
train.task = makeClassifTask(id = "class", data = as.data.frame(train), target = "TARGET")

fv = generateFilterValuesData(train.task, method = "randomForest.importance")

xgb_learner = makeLearner("classif.xgboost", 
                          predict.type = "prob",
                          par.vals = list(
                            objective = "binary:logistic",
                            eval_metric = "auc",
                            nrounds = 100,
                            verbose = 1
                          ))

# xgb_params = makeParamSet(makeIntegerParam("max_depth", lower = 5, upper = 10),
#                           makeNumericParam("eta",lower = 0.1, upper = 0.5))
# resample_desc = makeResampleDesc("CV", iters = 5)
# control = makeTuneControlRandom(maxit = 10)
# tuned_params = tuneParams(learner = xgb_learner,
#                           task = train.task,
#                           resampling = resample_desc,
#                           par.set = xgb_params,
#                           control = control)
# 
# xgb_tuned_learner = setHyperPars(learner = xgb_learner, par.vals = tuned_params$x)
# xgb_model = train(xgb_tuned_learner, train.task)

xgb_model = train(xgb_learner, task = train.task)
result = predict(xgb_model, newdata = test)
pred.xgb = result$data$prob.1
options(scipen = 3)
pred = data.frame(SK_ID_CURR = SK_ID_CURR, TARGET = pred.xgb)
write.csv(pred, file.path(root,"predict", "submisssions0607_1.csv"), row.names = FALSE)
