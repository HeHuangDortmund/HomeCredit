library("rprojroot")
library("data.table")
library("checkmate")
library("ggplot2")
library("corrplot")
requireNamespace("withr")
root = find_root(is_git_root)
set.seed(1)

# TODO
#0. 检查ISNA_indicator是否高度相关(see below)
#1. 尝试找出category变量中特定level与TARGET的关联, 避免在合并categories时损失过多信息 (解决办法:作图来看不明显, 对于类型太多的变量使用hash trick)
#2. 手动删除NA太多的变量(done, see readData2.R)
#3. 手动删除意义上重复变量, 如MOD,AVG,MEDI (未做)
#4. 特征选择: randomforest和mlr (未做)
#5. 建模: logistic, NN(与ROC等价的loss function), xgboost (未做)

# read data
source(file.path(root,"data", "readData2.R"))
data = readData(version = 3) # version = 3 还未完成
train = data[data$split == "train",]
test = data[data$split == "test",]

##ISNA_indicator是否高度相关###########################################################
matrix.cor = cor(data[,.SD,.SDcols = names(data)[grep("NA_",names(data))]])
pdf("corr_ISNA_variable.pdf",width = 14, height = 14)
corrplot.mixed(matrix.cor, tl.col = "black", tl.pos = "lt")
dev.off()
## 以下几类变量相互corr较高 (>=0.9), 手动删除NA太多变量之后
# 1. NA_YEARS_BEGINEXPLUATATION, NA_FLOORSMAX, NA_LIVINGAREA, NA_TOTALAREA_MODE
# 2. NA_AMT_REQ, NA_BUREAU, NA_DAYS_CREDIT_ENDDATE
# 3. NA_POS_CASH, NA_INSTALLMENT_PAYMENT, NA_PREV_APPLICATION

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

## 以下几类变量相互corr较高 (>=0.9), 未手动删除NA太多变量
# 1. NA_YEARS_BEGINEXPLUATATION, NA_FLOORSMAX, NA_ENTRANCES, NA_APARTMENT, NA_ELEVATORS, NA_NONLIVINGAREA, NA_TOTALAREA_MODE
# 2. NA_LANDAREA, NA_LIVINGAREA
# 3. NA_YEARS_BUILD, NA_FLOORSMIN, NA_LIVINGAPARTMENTS, NA_NONLIVINGAPARTMENTS, NA_COMMONAREA
# 4. NA_DAYS_CREDIT_ENDDATE, NA_BUREAU, NA_AMT_REQ
# 5. NA_INSTALLMENT_PAYMENT, NA_POS_CASH, NA_PREV_APPLICATION, NA_DAYS_FIRST_DRAWING_MEAN, NA_AMT_DRAWINGS_MEAN

##plot categorical variables###############################################################
source(file.path(root,"data", "readData2.R"))
application = readData(version = 1)

# application
varCategory = names(application)[sapply(application, function(x) is.factor(x))]
train = application[split == "train"]

makeplot_category <- function(var){
  eval(parse(text = file.path("ggplot(data=train, aes(x=",
                              var,
                              "))+geom_bar(stat=\"count\", position = \"fill\", aes(fill = as.factor(TARGET))) + labs(x = \"\", fill = \"TARGET\") + coord_flip()",fsep = "")))
  ggsave(file.path("stacked_percentage_barplot_application_",var,".pdf",fsep= ""))
}
sapply(varCategory,makeplot_category)

# previous_application
previous_application = fread("../data/previous_application.csv", na.strings = "")
varCategory = names(previous_application)[sapply(previous_application, function(x) is.character(x))]
previous_application = previous_application[,.SD,.SDcols = varCategory,by = list(SK_ID_CURR,SK_ID_PREV)]
# Replace XNA,XAP
previous_application[previous_application == "XNA"] = "miss"
previous_application[previous_application == "XAP"] = "miss"
previous_application[is.na(previous_application)] = "miss"

application_subset = application[,.SD,.SDcols = c("SK_ID_CURR","TARGET","split")]
merged = merge(previous_application, application_subset, all.x = TRUE, by = "SK_ID_CURR")
merged = merged[split == "train"]

makeplot_category <- function(var){
  eval(parse(text = file.path("ggplot(data=merged, aes(x=",
                              var,
                              "))+geom_bar(stat=\"count\", position = \"fill\", aes(fill = as.factor(TARGET))) + labs(x = \"\", fill = \"TARGET\") + coord_flip()",fsep = "")))
  ggsave(file.path("stacked_percentage_barplot_previous_application_",var,".pdf",fsep= ""))
}
sapply(varCategory,makeplot_category)

# credit_card_balance
credit_card_balance = fread("../data/credit_card_balance.csv", na.strings = "")
merged = merge(credit_card_balance, application_subset, all.x = TRUE, by = "SK_ID_CURR")
merged = merged[split == "train"]
ggplot(data=merged, aes(x=NAME_CONTRACT_STATUS))+
  geom_bar(stat="count", position = "fill", aes(fill = as.factor(TARGET))) + 
  labs(x = "", fill = "TARGET") + 
  coord_flip()
ggsave("stacked_percentage_barplot_credit_card_balance_NAME_CONTRACT_STATUS.pdf")

# POS_CASH_balance
POS_CASH_balance = fread("../data/POS_CASH_balance.csv", na.strings = "")
merged = merge(POS_CASH_balance, application_subset, all.x = TRUE, by = "SK_ID_CURR")
merged = merged[split == "train"]
ggplot(data=merged, aes(x=NAME_CONTRACT_STATUS))+
  geom_bar(stat="count", position = "fill", aes(fill = as.factor(TARGET))) + 
  labs(x = "", fill = "TARGET") + 
  coord_flip()
ggsave("stacked_percentage_barplot_POS_CASH_balance_NAME_CONTRACT_STATUS.pdf")

# add feature SUM_OVERDUE
source(file.path(root, "preproc", "add_feature_SUM_OVERDUE.R"))
data = preproc(data)