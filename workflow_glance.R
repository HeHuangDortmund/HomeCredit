rm(list=ls())
pkgs <- c("rprojroot","data.table","checkmate","mlr","ggplot2","corrplot","randomForest")
lapply(pkgs,require,character.only = TRUE)
requireNamespace("withr")
root = find_root(is_git_root)
set.seed(1)

# TODO
#0. 检查ISNA_indicator是否高度相关(done, see below)
#1. 尝试找出category变量中特定level与TARGET的关联, 避免在合并categories时损失过多信息 (解决办法:作图来看不明显, 对于类型太多的变量使用hash trick)
#2. 手动删除NA太多的变量(partially done, see readData2.R)
#3. 手动删除意义上重复变量, 如MOD,AVG,MEDI (partially done)
#4. 特征选择: randomforest和mlr (partially done)
#5. 建模: logistic, NN(与ROC等价的loss function), xgboost (partially done lightGBM)
#6. 处理month balance data， time-dependent trend， 利用var， diff或者ts model...
#7. 注意aggregation和add feature的顺序

# read data
source(file.path(root,"data", "readData2.R"))
data = readData(version = 3) # version = 3 还未完成

##ISNA_indicator是否高度相关###########################################################
matrix.cor = cor(data[,.SD,.SDcols = names(data)[grep("NA_",names(data))]])
pdf(file.path(root,"plots", "corr_ISNA_variable.pdf"),width = 14, height = 14)
corrplot.mixed(matrix.cor, tl.col = "black", tl.pos = "lt")
dev.off()
## 以下几类变量相互corr较高 (>=0.9), 手动删除NA太多变量之后
# 1. NA_TOTALAREA_MODE, NA_YEARS_BEGINEXPLUATATION, NA_FLOORSMAX, NA_LIVINGAREA, NA_ENTRANCES, NA_APARTMENT, NA_ELEVATORS
# 2. NA_AMT_REQ, NA_BUREAU, NA_DAYS_CREDIT_ENDDATE
# 3. NA_POS_CASH, NA_INSTALLMENT_PAYMENT, NA_PREV_APPLICATION

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
## 以下几类变量相互corr较高 (>=0.9), 未手动删除NA太多变量
# 1. NA_YEARS_BEGINEXPLUATATION, NA_FLOORSMAX, NA_ENTRANCES, NA_APARTMENT, NA_ELEVATORS, NA_NONLIVINGAREA, NA_TOTALAREA_MODE
# 2. NA_LANDAREA, NA_LIVINGAREA
# 3. NA_YEARS_BUILD, NA_FLOORSMIN, NA_LIVINGAPARTMENTS, NA_NONLIVINGAPARTMENTS, NA_COMMONAREA
# 4. NA_DAYS_CREDIT_ENDDATE, NA_BUREAU, NA_AMT_REQ
# 5. NA_INSTALLMENT_PAYMENT, NA_POS_CASH, NA_PREV_APPLICATION, NA_DAYS_FIRST_DRAWING_MEAN, NA_AMT_DRAWINGS_MEAN

# add feature
source(file.path(root, "preproc", "add_feature.R"))
preproc(data)
nameFactor = names(data)[unlist(lapply(data, function(x) is.factor(x)))]
data = createDummyFeatures(as.data.frame(data), target = "TARGET", 
                           cols = nameFactor,
                           method = "reference")
# fwrite(data,"full_data.csv")

train = data[data$split == "train",]
test = data[data$split == "test",]

train$split = NULL
test$split = NULL

SK_ID_CURR = test$SK_ID_CURR
train$SK_ID_CURR = NULL
test$SK_ID_CURR = NULL
test$TARGET = NULL

const = unlist(lapply(train, function(x) sd(x) == 0))
const = names(train)[const]
train[const]= NULL
test[const] = NULL


#### feature importance with Random Forest
# train.task = makeClassifTask(id = "class", data = as.data.frame(train), target = "TARGET")
# fv = generateFilterValuesData(train.task, method = "randomForest.importance")
start_time = Sys.time()
train$TARGET = factor(train$TARGET) # factor(TARGET), then classification in RF
quick_RF = randomForest(train[,-88],train[,88], 
                        ntree = 30, 
                        importance = TRUE, 
                        nodesize = 3500, # default for classif is 1, too small
                        proximity = FALSE,
                        do.trace = 1) # runtime 54min
end_time = Sys.time()
imp_RF <- importance(quick_RF)
imp_DF <- data.frame(Variables = row.names(imp_RF), Gini = imp_RF[,4])
imp_DF <- imp_DF[order(imp_DF$Gini, decreasing = TRUE),]

ggplot(imp_DF[1:50,], aes(x=reorder(Variables, Gini), y=Gini, fill=Gini)) + 
geom_bar(stat = 'identity') + 
labs(x = 'Variables', y= 'Mean decrease in Gini Index') + 
coord_flip() + 
theme(legend.position="none")
ggsave(file.path(root, "plots","feature_importance_randomForest.pdf"))

# var.list = as.vector(imp_DF$Variables[1:300])
# write.table(var.list, "var_list.txt")

# add feature SUM_OVERDUE
source(file.path(root, "preproc", "add_feature_SUM_OVERDUE.R"))
data = preproc(data)