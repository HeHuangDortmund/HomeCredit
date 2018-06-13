#' @description Function to read the Daten and merge them
#' @return data.table
#' @author He Huang



readData = function(version) {
  library(rprojroot)
  library(data.table)
  requireNamespace("lubridate")
  root = find_root(is_git_root)
  setwd(root)
  
  ##############################################################################
  #################    I. application_train and application_test     ###########
  ##############################################################################
  if(version >= 1){
    ## 0. read data application_train and application_test
    application_train = fread("../data/application_train.csv", na.strings = "")
    application_test = fread("../data/application_test.csv", na.strings = "")

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
  }
  
  ##############################################################################
  ################ II. bureau and bureau_balance   #############################
  ##############################################################################
  if(version >= 2){
    source(file.path(root, "data", "readdata_bureau.R"))
    bureau = readdata()
    source(file.path(root, "data", "readdata_bureau_balance.R"))
    bureau_balance = readdata()
    bureauMerged = merge(bureau, bureau_balance, all.x = TRUE, by = "SK_ID_BUREAU")
    application = merge(application, bureauMerged, all.x = TRUE, by = "SK_ID_CURR")
  }

  
  ##############################################################################
  #III. credit_card_balance installments_payments  POS_CASH_balance  previous_application
  ##############################################################################
  if(version >= 3){
    source(file.path(root, "data", "readdata_credit_card_balance.R"))
    credit_card_balance = readdata()
    source(file.path(root, "data", "readdata_installments_payments.R"))
    installments_payments = readdata()
    merged_1 = merge(credit_card_balance, installments_payments, all.x = TRUE, by = "SK_ID_PREV")
    
    
    source(file.path(root, "data", "readdata_POS_CASH_balance.R"))
    POS_CASH_balance = readdata()
    merged_2 = merge(POS_CASH_balance, merged_1, all.x = TRUE, by = "SK_ID_PREV")
    
    source(file.path(root, "data", "readdata_previous_application.R"))
    previous_application = readdata()
    
    merged_3 = merge(previous_application, merged_2, all.x = TRUE, by = "SK_ID_PREV")
    
    application = merge(application, merged_3, all.x = TRUE, by = "SK_ID_CURR")
  }

  return(application)
}
