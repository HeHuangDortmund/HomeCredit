pkgs <- c("rprojroot","data.table","checkmate","mlr","ggplot2","corrplot","randomForest")
lapply(pkgs,require,character.only = TRUE)
requireNamespace("withr")
root = find_root(is_git_root)
set.seed(1)

# read data
source(file.path(root,"data", "readData2.R"))
data = readData(version = 3) # version = 3 还未完成

# drop掉相关性太高的ISNA变量
varDrop_ISNA = c("NA_YEARS_BEGINEXPLUATATION",
                 "NA_FLOORSMAX",
                 "NA_LIVINGAREA",
                 "NA_ENTRANCES",
                 "NA_APARTMENT", 
                 "NA_ELEVATORS",
                 "NA_BUREAU",
                 "NA_DAYS_CREDIT_ENDDATE",
                 "NA_INSTALLMENT_PAYMENT")
# 去除意义上重复的变量
varDrop_Replicated = c("YEARS_BEGINEXPLUATATION_MODE",
                       "FLOORSMAX_MODE",
                       "LIVINGAREA_MODE",
                       "YEARS_BEGINEXPLUATATION_MEDI",
                       "FLOORSMAX_MEDI",
                       "LIVINGAREA_MEDI")

data = data[,c(varDrop_ISNA,varDrop_Replicated) := NULL]

nameFactor = names(data)[unlist(lapply(data, function(x) is.factor(x)))]
data = createDummyFeatures(as.data.frame(data), target = "TARGET", 
                           cols = nameFactor,
                           method = "reference")

var.list = read.table("var_list.txt", header = TRUE, stringsAsFactors = FALSE)
var.list = unlist(var.list)
data = data[,c("TARGET","split","SK_ID_CURR",var.list)]

train = data[data$split == "train",]
test = data[data$split == "test",]

train$split = NULL
test$split = NULL

SK_ID_CURR = test$SK_ID_CURR
train$SK_ID_CURR = NULL
test$SK_ID_CURR = NULL
test$TARGET = NULL

train.task = makeClassifTask(id = "class", data = as.data.frame(train), target = "TARGET")

xgb_learner = makeLearner("classif.xgboost", 
                          predict.type = "prob",
                          par.vals = list(
                            objective = "binary:logistic",
                            eval_metric = "auc",
                            nrounds = 100,
                            verbose = 1
                          ))

xgb_model = train(xgb_learner, task = train.task)
result = predict(xgb_model, newdata = test)
pred.xgb = result$data$prob.1
options(scipen = 3)
pred = data.frame(SK_ID_CURR = SK_ID_CURR, TARGET = pred.xgb)
write.csv(pred, file.path(root,"predict", "submisssions2107_1.csv"), row.names = FALSE)
