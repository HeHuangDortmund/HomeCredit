rm(list=ls())
options(Java.parameters = "-Xmx8g")
memory.limit(size = 56000)
pkgs <- c("rprojroot","data.table","mlr","ggplot2","lightgbm")
lapply(pkgs,require,character.only = TRUE)
requireNamespace("withr")
root = find_root(is_git_root)
set.seed(1)

##############################################################################################################################
makeRLearner.classif.lgb = function() {
  makeRLearnerClassif(
    cl = "classif.lgb",
    package = "lightgbm",
    par.set = makeParamSet(
      makeIntegerLearnerParam(id = "num_iterations", default = 100, lower=1), 
      makeIntegerLearnerParam(id = "nrounds", default = 10, lower = 1, upper = 1e4), 
      makeIntegerLearnerParam(id = "verbose", default = 1),
      makeDiscreteLearnerParam(id = "boosting", default = "gbdt", values = c("gbdt", "rf", "dart", "goss")), 
      makeNumericLearnerParam(id = "learning_rate", default = 0.1, lower = 0),
      makeIntegerLearnerParam(id = "max_depth", default = -1, lower = -1),  
      makeIntegerLearnerParam(id = "min_data_in_leaf", default = 20, lower = 0), 
      makeIntegerLearnerParam(id = "num_leaves", default = 31, lower = 1),
      makeNumericLearnerParam(id = "feature_fraction", default = 1, lower = 0, upper = 1), 
      makeNumericLearnerParam(id = "bagging_fraction", default = 1, lower = 0, upper = 1),
      makeIntegerLearnerParam(id = "bagging_freq", default = 0, lower = 0), 
      makeNumericLearnerParam(id = "min_gain_to_split", default = 0, lower = 0),
      makeNumericLearnerParam(id = "min_sum_hessian", default = 10),
      makeIntegerLearnerParam(id = "early_stopping_round", default = 0, lower = 0),
      makeNumericLearnerParam(id = "lambda_l1", default = 0, lower = 0),
      makeNumericLearnerParam(id = "lambda_l2", default = 0, lower = 0), 
      makeNumericLearnerParam(id = "drop_rate", default = 0.1, lower = 0, upper = 1, requires = quote(boosting == "dart")),
      makeIntegerLearnerParam(id = "max_drop", default = 50, requires = quote(boosting == "dart")),
      makeLogicalLearnerParam(id = "uniform_drop", default = FALSE, requires = quote(boosting == "dart")),
      makeLogicalLearnerParam(id = "xgboost_dart_mode", default = FALSE, requires = quote(boosting == "dart")),
      makeNumericLearnerParam(id = "top_rate", default = 0.2, lower = 0.1, upper = 1, requires = quote(boosting == "goss")),
      makeNumericLearnerParam(id = "other_rate", default = 0.1, lower = 0, upper = 1, requires = quote(boosting == "goss")),
      makeIntegerLearnerParam(id = "max_cat_group", default = 64),
      makeIntegerLearnerParam(id = "max_cat_threshold", default = 256),
      makeNumericLearnerParam(id = "min_cat_smooth", default = 5),
      makeNumericLearnerParam(id = "max_cat_smooth", default = 100),
      makeNumericLearnerParam(id = "cat_smooth_ratio", default = 0.01),
      ## 用于分类变量，可以减小分类变量中噪音的影响，特别是类别中数据较少的分别变量
      makeIntegerLearnerParam(id = "max_bin", default = 255),
      ## 数据行分组数，少的分组数会降低训练精度（训练速度快），但是可以提升泛化能力
      makeIntegerLearnerParam(id = "min_data_in_bin", default = 5),
      ## 分组内最小用户数
      makeLogicalLearnerParam(id = "header", default = FALSE),
      ## 设置为TRUE表示数据有字段名
      makeLogicalLearnerParam(id="use_missing", default = TRUE,tunable = FALSE),
      ## 设置为FALSE将不能对缺失值进行特殊操作 
      makeLogicalLearnerParam(id = "zero_as_missing", default = FALSE),
      ## 是否用0代替缺失值与未知值
      makeIntegerLearnerParam(id = "num_class", default = 1, lower = 1, upper = 1e6),
      ## 声明多分类中的类数
      makeDiscreteLearnerParam(id = "metric", default = "auc", values = c("auc", "multi_error")),
      makeLogicalLearnerParam(id = "is_training_metric", default = TRUE),
      ## set this to TRUE if need to output metric result of training
      makeDiscreteLearnerParam(id = "objective", default = "binary", values = c("multiclass", "binary")),
      makeDiscreteLearnerParam(id = "predict.method", values = c("plug-in", "predictive", "debiased"), default = "plug-in", when = "predict")
    ),
    par.vals = list(objective = "binary"),## 此处请按个人需求修改
    properties = c("twoclass","multiclass", "numerics", "factors", "prob", "weights"), 
    name = "LightGBM",
    short.name = "lgb",
    note = "Learner param 'predict.method' maps to 'method' in predict.lgb."
  )
}

trainLearner.classif.lgb = function(.learner, .task, .subset, .weights = NULL,  ...) {
  data = getTaskData(.task, .subset, target.extra = TRUE)
  lgb.data1 = lgb.Dataset(
    data = as.matrix(data$data[1:dim(data_train)[1], ]), ## 请将此处data_train修改为实际训练数据集的名称,见第3步
    label = as.integer(data$target[1:dim(data_train)[1]]) - 1L ## 请将此处data_train修改为实际训练数据集的名称
  )
  lgb.data2 = lgb.Dataset.create.valid(
    dataset = lgb.data1,
    data = as.matrix(data$data[(dim(data_train)[1] + 1):dim(data_total)[1], ]), 
    ## 请将此处data_train修改为实际训练数据集的名称，data_total是指训练数据+验证数据集的名称，即所有数据,见第3步
    label = as.integer(data$target[(dim(data_train)[1] + 1):dim(data_total)[1]]) - 1L
    ## 请将此处data_train修改为实际训练数据集的名称，data_total是指训练数据+验证数据集的名称，即所有数据,见第3步
  )
  valids = list(test = lgb.data2)
  
  lightgbm::lgb.train(
    list(), 
    data = lgb.data1,
    valids = valids,
    num_threads = 4, ## 请按实际需要的电脑线程数进行修改，默认值为CPU核数，非线程数（譬如本人电脑为两核四线程，则默认值为2）
    verbose = 1, ## verbose=1打印训练信息
    record = TRUE,
    #early_stopping_rounds = 10,
    nrounds = 1, ## cross-validation,不进行交叉验证
    ...
  )
}

predictLearner.classif.lgb = function(.learner, .model, .newdata, ...) {
  p = predict(.model$learner.model, data.matrix(.newdata), reshape = TRUE, ...)
  y = matrix(0, ncol = 2, nrow = nrow(data.matrix(.newdata)))
  colnames(y) = .model$task.desc$class.levels
  y[, 1L] = 1 - p
  y[, 2L] = p
  if (.learner$predict.type == "prob") {
    return(y)
  } else {
    p = colnames(y)[max.col(y)]
    names(p) = NULL
    p = factor(p, levels = colnames(y))
    return(p)
  }
}

registerS3method("makeRLearner", "classif.lgb", makeRLearner.classif.lgb)
registerS3method("trainLearner", "classif.lgb", trainLearner.classif.lgb)
registerS3method("predictLearner", "classif.lgb", predictLearner.classif.lgb)
##############################################################################################################################

# read data
data = fread("../data/full_data.csv")
data$Add_RATIO_MEAN_PAYMENT_PREV[is.na(data$Add_RATIO_MEAN_PAYMENT_PREV)] = mean(data$Add_RATIO_MEAN_PAYMENT_PREV, na.rm = TRUE)
set.seed(1)

train = data[data$split == "train",]
train$split = NULL
SK_ID_CURR = train$SK_ID_CURR
train$SK_ID_CURR = NULL
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

train_subset = sample(dim(train)[1], round(dim(train)[1]*0.9,0))
data_train = train[train_subset,]
data_validation = train[-train_subset,]
rm(train,data)
gc()
data_total = rbind(data_train, data_validation)
data_total$TARGET = as.factor(data_total$TARGET)

lgb.task <- makeClassifTask(data = as.data.frame(data_total), target = "TARGET")
lgb.ps <- makeParamSet(
  makeNumericParam("learning_rate", lower = .1, upper = 1),
  makeDiscreteParam("num_iterations", values = 100), ##默认值为100
  makeIntegerParam("num_leaves", lower = 5, upper = 50)
)
lgb.ctrl <- makeTuneControlGrid(resolution = 10L) ## 默认值为10
lgb.rdesc <- makeResampleDesc(method = "Bootstrap", iters = 10, stratify = TRUE) 

lrn.ps <- list(
  objective = "binary",
  metric = "auc",
  num_class = 1, ##请根据实际情况进行调整
  header = TRUE
)
lgb.lrn = makeLearner(
  'classif.lgb', 
  predict.type = 'prob', ## 输出的为概率，不是分类结果，用来计算logloss
  par.vals = lrn.ps
)

res_time <- system.time(
  lgb.res <- tuneParams(
    learner = lgb.lrn,
    task = lgb.task,
    resampling = lgb.rdesc, 
    par.set = lgb.ps,
    control = lgb.ctrl,
    show.info = TRUE,
    measures = list(auc)
  )
)