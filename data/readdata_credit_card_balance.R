readData = function(exploratory = 0,
                    version = 1){
  library(rprojroot)
  library(data.table)
  root = find_root(is_git_root)
  setwd(root)
  ########################################################################################
  # Problem: fill NAs, e.g., CNT_INSTALMENT_MATURE_CUM, AMT_INST_MIN_REGULARITY, AMT_PAYMENT_CURRENT
  ########################################################################################
  makeplot <- function(var, data, type){
    pdf(file.path(data,"_",var,".pdf",fsep = ""), 
        width = 14, 
        height = 12)
    par(cex.lab=1.5,cex.axis=1.5,mar=c(8,5,2,2) + 0.1,lwd=2)
    if (type == "barplot"){
      eval(parse(text = file.path("barplot(",data,"[,.N,by =", 
                                  var,
                                  "]$N,names.arg  = ",data,"[,.N,by =",
                                  var,
                                  "]$",
                                  var,
                                  ",las = 2)",
                                  fsep = "")))
    } else if (type == "density") {
      eval(parse(text = file.path("plot(density(na.omit(",data,"$", 
                                  var,
                                  ")))",
                                  fsep = "")))
    }
    dev.off()
  }
  
  credit_card_balance = fread("../data/credit_card_balance.csv", na.strings = "")
  if (exploratory == 1){
    varCategory = names(credit_card_balance)[sapply(credit_card_balance, function(x) is.character(x))]
    varNumeric = setdiff(names(credit_card_balance),varCategory)
    sapply(varCategory, makeplot, "credit_card_balance", "barplot")
    sapply(varNumeric, makeplot, "credit_card_balance", "density")
  }
  
  if (version == 2){ # aggregation之前先用0填补一部分NA, version==1 直接汇总
    credit_card_balance$AMT_DRAWINGS_ATM_CURRENT[is.na(credit_card_balance$AMT_DRAWINGS_ATM_CURRENT)] = 0
    credit_card_balance$AMT_DRAWINGS_OTHER_CURRENT[is.na(credit_card_balance$AMT_DRAWINGS_OTHER_CURRENT)] = 0
    credit_card_balance$AMT_DRAWINGS_POS_CURRENT[is.na(credit_card_balance$AMT_DRAWINGS_POS_CURRENT)] = 0
    credit_card_balance$CNT_DRAWINGS_ATM_CURRENT[is.na(credit_card_balance$CNT_DRAWINGS_ATM_CURRENT)] = 0
    credit_card_balance$CNT_DRAWINGS_OTHER_CURRENT[is.na(credit_card_balance$CNT_DRAWINGS_OTHER_CURRENT)] = 0
    credit_card_balance$CNT_DRAWINGS_POS_CURRENT[is.na(credit_card_balance$CNT_DRAWINGS_POS_CURRENT)] = 0
    credit_card_balance$AMT_INST_MIN_REGULARITY[is.na(credit_card_balance$AMT_INST_MIN_REGULARITY)] = 0
    credit_card_balance$AMT_PAYMENT_CURRENT[is.na(credit_card_balance$AMT_PAYMENT_CURRENT)] = 0
    credit_card_balance$CNT_INSTALMENT_MATURE_CUM[is.na(credit_card_balance$CNT_INSTALMENT_MATURE_CUM)] = 0 # 暂时补0,需merge installments 信息
  }

  temp = credit_card_balance[,.(N_MONTH = .N,
                                AMT_BALANCE_MEAN = mean(AMT_BALANCE, na.rm = TRUE),
                                AMT_CREDIT_LIMIT_ACTUAL_MAX = max(AMT_CREDIT_LIMIT_ACTUAL, na.rm = TRUE),
                                AMT_CREDIT_LIMIT_ACTUAL_MIN = min(AMT_CREDIT_LIMIT_ACTUAL, na.rm = TRUE),
                                AMT_DRAWINGS_ATM_CURRENT_MEAN = mean(AMT_DRAWINGS_ATM_CURRENT, na.rm = TRUE),
                                AMT_DRAWINGS_CURRENT_MEAN = mean(AMT_DRAWINGS_CURRENT, na.rm = TRUE),
                                AMT_DRAWINGS_OTHER_CURRENT_MEAN = mean(AMT_DRAWINGS_OTHER_CURRENT,na.rm = TRUE),
                                AMT_DRAWINGS_POS_CURRENT_MEAN = mean(AMT_DRAWINGS_POS_CURRENT,na.rm=TRUE),
                                AMT_INST_MIN_REGULARITY_MEAN = mean(AMT_INST_MIN_REGULARITY,na.rm=TRUE),
                                AMT_PAYMENT_CURRENT_MEAN = mean(AMT_PAYMENT_CURRENT, na.rm = TRUE),
                                AMT_PAYMENT_TOTAL_CURRENT_MEAN = mean(AMT_PAYMENT_TOTAL_CURRENT ,na.rm=TRUE),
                                AMT_RECEIVABLE_PRINCIPAL_MEAN = mean(AMT_RECEIVABLE_PRINCIPAL,na.rm = TRUE),
                                AMT_RECIVABLE_MEAN = mean(AMT_RECIVABLE, na.rm = TRUE),
                                AMT_TOTAL_RECEIVABLE_MEAN = mean(AMT_TOTAL_RECEIVABLE,na.rm=TRUE),
                                CNT_DRAWINGS_ATM_CURRENT_MEAN = mean(CNT_DRAWINGS_ATM_CURRENT, na.rm=TRUE),
                                CNT_DRAWINGS_CURRENT_MEAN = mean(CNT_DRAWINGS_CURRENT,na.rm=TRUE),
                                CNT_DRAWINGS_OTHER_CURRENT_MEAN = mean(CNT_DRAWINGS_OTHER_CURRENT,na.rm=TRUE),
                                CNT_DRAWINGS_POS_CURRENT_MEAN = mean(CNT_DRAWINGS_POS_CURRENT,na.rm=TRUE),
                                CNT_INSTALMENT_MATURE_CUM_MEAN = mean(CNT_INSTALMENT_MATURE_CUM,na.rm=TRUE),
                                SK_DPD_SUM = sum(SK_DPD),
                                SK_DPD_DEF_SUM = sum(SK_DPD_DEF)),by = list(SK_ID_CURR,SK_ID_PREV)]
  
  credit_card_balance$NAME_CONTRACT_STATUS[credit_card_balance$NAME_CONTRACT_STATUS != "Active" & 
                                           credit_card_balance$NAME_CONTRACT_STATUS != "Completed" & 
                                           credit_card_balance$NAME_CONTRACT_STATUS != "Signed"] = "Other"
  tempCONTRACT = credit_card_balance[,.N,by = list(SK_ID_CURR, NAME_CONTRACT_STATUS)]
  tempCONTRACTwide = dcast(tempCONTRACT, SK_ID_CURR ~ NAME_CONTRACT_STATUS, fill = 0, value.var = "N")
  names(tempCONTRACTwide) = c("SK_ID_CURR","NAME_CONTRACT_STATUS_Active", 
                              "NAME_CONTRACT_STATUS_Completed",
                              "NAME_CONTRACT_STATUS_Other",
                              "NAME_CONTRACT_STATUS_Signed")
  temp = merge(temp, tempCONTRACTwide, all = TRUE, by = "SK_ID_CURR")
  
  temp2 = temp[,.(MONTH_MEAN = mean(N_MONTH),
                  AMT_BALANCE_MEAN = mean(AMT_BALANCE_MEAN),
                  AMT_CREDIT_LIMIT_ACTUAL_MAX = max(AMT_CREDIT_LIMIT_ACTUAL_MAX),
                  AMT_CREDIT_LIMIT_ACTUAL_MIN = min(AMT_CREDIT_LIMIT_ACTUAL_MIN),
                  AMT_DRAWINGS_ATM_CURRENT_MEAN = mean(AMT_DRAWINGS_ATM_CURRENT_MEAN),
                  AMT_DRAWINGS_CURRENT_MEAN = mean(AMT_DRAWINGS_CURRENT_MEAN),
                  AMT_DRAWINGS_OTHER_CURRENT_MEAN = mean(AMT_DRAWINGS_OTHER_CURRENT_MEAN),
                  AMT_DRAWINGS_POS_CURRENT_MEAN = mean(AMT_DRAWINGS_POS_CURRENT_MEAN),
                  AMT_INST_MIN_REGULARITY_MEAN = mean(AMT_INST_MIN_REGULARITY_MEAN),
                  AMT_PAYMENT_CURRENT_MEAN = mean(AMT_PAYMENT_CURRENT_MEAN),
                  AMT_PAYMENT_TOTAL_CURRENT_MEAN = mean(AMT_PAYMENT_TOTAL_CURRENT_MEAN),
                  AMT_RECEIVABLE_PRINCIPAL_MEAN = mean(AMT_RECEIVABLE_PRINCIPAL_MEAN),
                  AMT_RECIVABLE_MEAN = mean(AMT_RECIVABLE_MEAN),
                  AMT_TOTAL_RECEIVABLE_MEAN = mean(AMT_TOTAL_RECEIVABLE_MEAN),
                  CNT_DRAWINGS_ATM_CURRENT_MEAN = mean(CNT_DRAWINGS_ATM_CURRENT_MEAN),
                  CNT_DRAWINGS_CURRENT_MEAN = mean(CNT_DRAWINGS_CURRENT_MEAN),
                  CNT_DRAWINGS_OTHER_CURRENT_MEAN = mean(CNT_DRAWINGS_OTHER_CURRENT_MEAN),
                  CNT_DRAWINGS_POS_CURRENT_MEAN = mean(CNT_DRAWINGS_POS_CURRENT_MEAN),
                  CNT_INSTALMENT_MATURE_CUM_MEAN = mean(CNT_INSTALMENT_MATURE_CUM_MEAN),
                  SK_DPD_SUM = sum(SK_DPD_SUM),
                  SK_DPD_DEF_SUM = sum(SK_DPD_DEF_SUM),
                  NAME_CONTRACT_STATUS_PREV_CREDIT_Active = sum(NAME_CONTRACT_STATUS_Active),
                  NAME_CONTRACT_STATUS_PREV_CREDIT_Completed = sum(NAME_CONTRACT_STATUS_Completed),
                  NAME_CONTRACT_STATUS_PREV_CREDIT_Other = sum(NAME_CONTRACT_STATUS_Other),
                  NAME_CONTRACT_STATUS_PREV_CREDIT_Signed = sum(NAME_CONTRACT_STATUS_Signed)),by = SK_ID_CURR]
  
  credit_card_balance = temp2
  
  return(credit_card_balance)
}