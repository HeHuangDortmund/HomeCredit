#' @description Function to read the Daten and merge them
#' @return data.table
#' @author He Huang

# 通过步骤I把 application_train 和 application_test 汇总得到 application
# 通过步骤II把 bureau 和 bureau_balance 汇总得到 bureauMerged
# 通过步骤III 把 application 和 bureauMerged 汇总得到 新的application


readData = function() {
  library(rprojroot)
  library(data.table)
  requireNamespace("lubridate")
  
  root = find_root(is_git_root)
  setwd(root)
  
  ##############################################################################
  #################    I. application_train and application_test     ###########
  ##############################################################################
  ## 0. read data application_train and application_test
  application_train = fread("../data/application_train.csv", na.strings = "")
  application_test = fread("../data/application_test.csv", na.strings = "")
  

  #credit_card_balance = fread("../data/credit_card_balance.csv", na.strings = "")
  #installments_payments = fread("../data/installments_payments.csv", na.strings = "")
  #POS_CASH_balance = fread("../data/POS_CASH_balance.csv", na.strings = "")
  #previous_application = fread("../data/previous_application.csv", na.strings = "")
  

  ## 1. merge train and test data 
  application_train[, split := "train"]
  Target = application_train$TARGET
  application_train$TARGET = NULL
  application_train[, TARGET := Target]
  application_test[, split := "test"]
  application_test[, TARGET := NA]
  application = rbind(application_train, application_test)
  
  ## 2. Category Variablen as Factor or Interger
  numberOfValues = unlist(lapply(application, function(x){length(unique(x))}))
  # 0 < numberOfValues < 20 is Category Variablen, as Factor
  IndexOfCategory = (numberOfValues < 20 | names(application) == "ORGANIZATION_TYPE") & 
    !(names(application) %in% c("CNT_CHILDREN", 
                              "CNT_FAM_MEMBERS", 
                              "EF_30_CNT_SOCIAL_CIRCL", 
                              "DEF_60_CNT_SOCIAL_CIRCLE",
                              "AMT_REQ_CREDIT_BUREAU_HOUR",
                              "AMT_REQ_CREDIT_BUREAU_DAY",
                              "AMT_REQ_CREDIT_BUREAU_WEEK",
                              "AMT_REQ_CREDIT_BUREAU_QRT "))
  
  columsCategory = names(application)[IndexOfCategory]
  columsCategory = columsCategory[-length(columsCategory)]
  columsCategory = columsCategory[-length(columsCategory)]
  
  application[, (columsCategory) := lapply(.SD, function(x) as.factor(x)),
        .SDcols = columsCategory]
  
  IndexBinar = (numberOfValues == 2)
  columsBinar = names(application)[IndexBinar]
  columsBinar = columsBinar[-length(columsBinar)]
  application[, (columsBinar) := lapply(.SD, function(x) as.integer(x)-1),
              .SDcols = columsBinar] 
  
  
  ##############################################################################
  ################ II. bureau and bureau_balance   #############################
  ##############################################################################
  ## 0. read table bureau and bureau_balance, and join by SK_BUREAU_ID
  bureau = fread("../data/bureau.csv", na.strings = "")
  bureau_balance = fread("../data/bureau_balance.csv", na.strings = "")
  ## 1. bureau_balance : from long data to wide data
  tmp = bureau_balance[,.N, by = list(SK_ID_BUREAU,STATUS)]
  
  wideData = dcast(tmp, SK_ID_BUREAU ~ STATUS)
  ## 2. merge bureau and wide data
  bureauMerged = merge(bureau, wideData, all.x = TRUE, by = "SK_ID_BUREAU")
  
  
  ##############################################################################
  ################ III. application and bureauMerged   #########################
  ##############################################################################
  application = merge(application, bureauMerged, all.x = TRUE, by = "SK_ID_CURR")
  
 
  return(application)
}
