readData = function(version = 1){
  library(rprojroot)
  library(data.table)
  root = find_root(is_git_root)
  setwd(root)
  ####
  # Problem: XNA
  ####
  POS_CASH_balance = fread("../data/POS_CASH_balance.csv", na.strings = "")
  if (version == 2){
    POS_CASH_balance$CNT_INSTALMENT[is.na(POS_CASH_balance$CNT_INSTALMENT)] = mean(POS_CASH_balance$CNT_INSTALMENT, na.rm = TRUE)
    POS_CASH_balance$CNT_INSTALMENT_FUTURE[is.na(POS_CASH_balance$CNT_INSTALMENT_FUTURE)] = mean(POS_CASH_balance$CNT_INSTALMENT_FUTURE, na.rm = TRUE)
  }
  
  if(version == 99){
    temp = POS_CASH_balance[, .(MONTHS_BALANCE_MEAN = mean(MONTHS_BALANCE, na.rm = TRUE),
                                CNT_INSTALMENT_MEAN = mean(CNT_INSTALMENT, na.rm = TRUE),
                                CNT_INSTALMENT_FUTURE_MEAN = mean(CNT_INSTALMENT_FUTURE, na.rm = TRUE),
                                SK_DPD_MEAN = mean(SK_DPD, na.rm = TRUE),
                                SK_DPD_DEF_MEAN = mean(SK_DPD_DEF, na.rm = TRUE)) , by = list(SK_ID_CURR, SK_ID_PREV)]
    
    POS_CASH_balance$NAME_CONTRACT_STATUS[POS_CASH_balance$NAME_CONTRACT_STATUS == "Canceled" | 
                                            POS_CASH_balance$NAME_CONTRACT_STATUS == "XNA"] = "Other"
    temp2 = POS_CASH_balance[,.N,by = list(SK_ID_CURR, SK_ID_PREV, NAME_CONTRACT_STATUS)]
    temp2wide = dcast(temp2, SK_ID_CURR + SK_ID_PREV ~NAME_CONTRACT_STATUS, fill = 0)
    names(temp2wide) = c("SK_ID_CURR", 
                         "SK_ID_PREV",
                         "NAME_CONTRACT_STATUS_POSCASH_MONTH_Active",
                         "NAME_CONTRACT_STATUS_POSCASH_MONTH_Amortized",
                         "NAME_CONTRACT_STATUS_POSCASH_MONTH_Approved",
                         "NAME_CONTRACT_STATUS_POSCASH_MONTH_Completed",
                         "NAME_CONTRACT_STATUS_POSCASH_MONTH_Demand",
                         "NAME_CONTRACT_STATUS_POSCASH_MONTH_Other",
                         "NAME_CONTRACT_STATUS_POSCASH_MONTH_Returned",
                         "NAME_CONTRACT_STATUS_POSCASH_MONTH_Signed")
    POS_CASH_balance = merge(temp,temp2wide,all = TRUE, by = c("SK_ID_CURR","SK_ID_PREV"))
  }
  
  
  if (version <= 2){
    temp = POS_CASH_balance[, .(MONTHS_BALANCE_MEAN = mean(MONTHS_BALANCE, na.rm = TRUE),
                                CNT_INSTALMENT_MEAN = mean(CNT_INSTALMENT, na.rm = TRUE),
                                CNT_INSTALMENT_FUTURE_MEAN = mean(CNT_INSTALMENT_FUTURE, na.rm = TRUE),
                                SK_DPD_MEAN = mean(SK_DPD, na.rm = TRUE),
                                SK_DPD_DEF_MEAN = mean(SK_DPD_DEF, na.rm = TRUE)) , by = SK_ID_CURR]
    
    POS_CASH_balance$NAME_CONTRACT_STATUS[POS_CASH_balance$NAME_CONTRACT_STATUS == "Canceled" | 
                                            POS_CASH_balance$NAME_CONTRACT_STATUS == "XNA"] = "Other"
    temp2 = POS_CASH_balance[,.N,by = list(SK_ID_CURR, NAME_CONTRACT_STATUS)]
    temp2wide = dcast(temp2, SK_ID_CURR~NAME_CONTRACT_STATUS, fill = 0)
    names(temp2wide) = c("SK_ID_CURR", 
                         "NAME_CONTRACT_STATUS_POSCASH_MONTH_Active",
                         "NAME_CONTRACT_STATUS_POSCASH_MONTH_Amortized",
                         "NAME_CONTRACT_STATUS_POSCASH_MONTH_Approved",
                         "NAME_CONTRACT_STATUS_POSCASH_MONTH_Completed",
                         "NAME_CONTRACT_STATUS_POSCASH_MONTH_Demand",
                         "NAME_CONTRACT_STATUS_POSCASH_MONTH_Other",
                         "NAME_CONTRACT_STATUS_POSCASH_MONTH_Returned",
                         "NAME_CONTRACT_STATUS_POSCASH_MONTH_Signed")
    POS_CASH_balance = merge(temp,temp2wide,all = TRUE, by = "SK_ID_CURR")
  }
  return(POS_CASH_balance)
}