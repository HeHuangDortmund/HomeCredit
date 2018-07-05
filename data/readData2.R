#' @description Function to read the Daten and merge them
#' @return data.table
#' @author He Huang



readData = function(version) {
  library(rprojroot)
  library(data.table)
  library(mlr)
  root = find_root(is_git_root)
  setwd(root)
  
  ##############################################################################
  #################    I. application_train and application_test     ###########
  ##############################################################################
  if(version >= 1){
    source(file.path(root,"data","readdata_application.R"))
    application = readData()
    source(file.path(root,"data","imputeNAs_application.R"))
    application = imputeNA(application, naDrop = TRUE) # 删除了NA过多的变量
  }
  
  ##############################################################################
  ################ II. bureau and bureau_balance   #############################
  ##############################################################################
  if(version >= 2){
    source(file.path(root, "data", "readdata_bureau.R"))
    bureau = readData()
    application = merge(application, bureau, all.x = TRUE, by = "SK_ID_CURR")
    source(file.path(root, "data", "imputeNAs_bureau.R"))
    application = imputeNA(application,method = "mixed")
  }

  ##############################################################################
  #III. credit_card_balance installments_payments  POS_CASH_balance  previous_application
  ##############################################################################
  if(version >= 3){
    source(file.path(root, "data", "readdata_credit_card_balance.R"))
    credit_card_balance = readData()
    application = merge(application, credit_card_balance, all.x = TRUE, by = "SK_ID_CURR")
    source(file.path(root, "data", "imputeNAs_credit_card_balance.R"))
    application = imputeNA(application, naDrop = TRUE)# 删除了NA过多的变量
    
    source(file.path(root, "data", "readdata_installments_payments.R"))
    installments_payments = readData()
    application = merge(application, installments_payments, all.x = TRUE, by = "SK_ID_CURR")
    source(file.path(root, "data", "imputeNAs_installments_payments.R"))
    application = imputeNA(application)
    
    source(file.path(root, "data", "readdata_POS_CASH_balance.R"))
    POS_CASH_balance = readData()
    application = merge(application, POS_CASH_balance, all.x = TRUE, by = "SK_ID_CURR")
    source(file.path(root, "data", "imputeNAs_POS_CASH_balance.R"))
    application = imputeNA(application)
    
    source(file.path(root, "data", "readdata_previous_application.R"))
    previous_application = readData()
    application = merge(application, previous_application, all.x = TRUE, by = "SK_ID_CURR")
    source(file.path(root, "data", "imputeNAs_previous_application.R"))
    application = imputeNA(application, naDrop = TRUE)# 删除了NA过多的变量
  }
  
  return(application)
}