readData = function(version = 1){
  library(rprojroot)
  library(data.table)
  root = find_root(is_git_root)
  setwd(root)
  ########################################################################################
  # Problem: fill NAs, e.g., CNT_INSTALMENT_MATURE_CUM, AMT_INST_MIN_REGULARITY, AMT_PAYMENT_CURRENT
  ########################################################################################
  credit_card_balance = fread("../data/credit_card_balance.csv", na.strings = "")

  # aggregation之前先用0填NA, 以下6个变量为NA时AMT_DRAWINGS_ATM_CURRENT为0， AMT_DRAWINGS_CURRENT = AMT_DRAWINGS_ATM_CURRENT + AMT_DRAWINGS_OTHER_CURRENT + AMT_DRAWINGS_POS_CURRENT
  credit_card_balance$AMT_DRAWINGS_ATM_CURRENT[is.na(credit_card_balance$AMT_DRAWINGS_ATM_CURRENT)] = 0
  credit_card_balance$AMT_DRAWINGS_OTHER_CURRENT[is.na(credit_card_balance$AMT_DRAWINGS_OTHER_CURRENT)] = 0
  credit_card_balance$AMT_DRAWINGS_POS_CURRENT[is.na(credit_card_balance$AMT_DRAWINGS_POS_CURRENT)] = 0
  credit_card_balance$CNT_DRAWINGS_ATM_CURRENT[is.na(credit_card_balance$CNT_DRAWINGS_ATM_CURRENT)] = 0
  credit_card_balance$CNT_DRAWINGS_OTHER_CURRENT[is.na(credit_card_balance$CNT_DRAWINGS_OTHER_CURRENT)] = 0
  credit_card_balance$CNT_DRAWINGS_POS_CURRENT[is.na(credit_card_balance$CNT_DRAWINGS_POS_CURRENT)] = 0
  # AMT_PAYMENT_CURRENT = NA 时 所有AMT_PAYMENT_TOTAL_CURRENT = 0， 因此填0
  credit_card_balance$AMT_PAYMENT_CURRENT[is.na(credit_card_balance$AMT_PAYMENT_CURRENT) & 
                                          credit_card_balance$AMT_PAYMENT_TOTAL_CURRENT == 0] = 0
  
  # AMT_INST_MIN_REGULARITY和CNT_INSTALMENT_MATURE_CUM两个变量缺失时，大部分未发生transaction（即AMT_RECIVABLE,AMT_PAYMENT_CURRENT,AMT_DRAWINGS_CURRENT均为0），这部分用0填补，填补后剩余33822 NA
  credit_card_balance$AMT_INST_MIN_REGULARITY[is.na(credit_card_balance$AMT_INST_MIN_REGULARITY) & 
                                              credit_card_balance$AMT_RECIVABLE == 0 & 
                                              credit_card_balance$AMT_PAYMENT_CURRENT == 0 &
                                              credit_card_balance$AMT_DRAWINGS_CURRENT == 0] = 0
  credit_card_balance$CNT_INSTALMENT_MATURE_CUM[is.na(credit_card_balance$CNT_INSTALMENT_MATURE_CUM) & 
                                                credit_card_balance$AMT_RECIVABLE == 0 & 
                                                credit_card_balance$AMT_PAYMENT_CURRENT == 0 &
                                                credit_card_balance$AMT_DRAWINGS_CURRENT == 0] = 0
  
  # add hand maded features
  credit_card_balance[,`:=`(Add_DIFF_BALANCE_LIMIT_CARD = AMT_BALANCE - AMT_CREDIT_LIMIT_ACTUAL,
                            Add_DIFF_DRAWINGS_LIMIT_CARD = AMT_DRAWINGS_CURRENT - AMT_CREDIT_LIMIT_ACTUAL,
                            Add_DIFF_PAYMENT_INST_MIN_CARD =  AMT_PAYMENT_CURRENT - AMT_INST_MIN_REGULARITY,
                            Add_DIFF_PAYMENT_DRAWINGS_CARD = AMT_PAYMENT_CURRENT - AMT_DRAWINGS_CURRENT,
                            Add_DIFF_PAYMENT_RECEIVABLE_CARD = AMT_PAYMENT_CURRENT - AMT_RECIVABLE,
                            Add_DIFF_PAYMENT_RECEIVABLE_PRINCIPAL_CARD = AMT_PAYMENT_CURRENT - AMT_RECEIVABLE_PRINCIPAL,
                            Add_DIFF_RECEIVABLE_BALANCE_CARD = AMT_RECIVABLE - AMT_BALANCE,
                            Add_DIFF_RECEIVABLE_PRINCIPAL_BALANCE_CARD = AMT_RECEIVABLE_PRINCIPAL - AMT_BALANCE,
                            # Add_CARD_DRAWINGS_CURRENT_per_CNT = AMT_DRAWINGS_CURRENT / CNT_DRAWINGS_CURRENT, # Problematic cases: all AMT&CNT (CURRENT,ATM,POS,OTHER) =0 but AMT_CURRENT !=0
                            Add_CARD_DRAWINGS_ATM_per_CNT = AMT_DRAWINGS_ATM_CURRENT / CNT_DRAWINGS_ATM_CURRENT,
                            Add_CARD_DRAWINGS_OTHER_per_CNT = AMT_DRAWINGS_OTHER_CURRENT / CNT_DRAWINGS_OTHER_CURRENT,
                            Add_CARD_DRAWINGS_POS_per_CNT = AMT_DRAWINGS_POS_CURRENT / CNT_DRAWINGS_POS_CURRENT
  ), by = list(SK_ID_CURR,SK_ID_PREV)]
  credit_card_balance[credit_card_balance == "NaN"] = 0 # 填补后3个feature中CNT或者AMT是0时造成的NaN，i.e. 0/0
  
  # 对每个变量通过求MAX,SUM和MEAN进行汇总, 以SK_ID_PREV
  varMEAN = setdiff(names(credit_card_balance),c("SK_ID_CURR","SK_ID_PREV","NAME_CONTRACT_STATUS","MONTHS_BALANCE"))
  varSUMMAX = c("SK_DPD", "SK_DPD_DEF","AMT_CREDIT_LIMIT_ACTUAL","CNT_INSTALMENT_MATURE_CUM",names(credit_card_balance)[grep("Add",names(credit_card_balance))])
  temp_mean = credit_card_balance[,lapply(.SD, mean, na.rm = TRUE), .SDcols = varMEAN, by = list(SK_ID_CURR,SK_ID_PREV)]
  names(temp_mean)[-c(1,2)] = paste(names(temp_mean)[-c(1,2)],"MEAN",sep = "_")
  temp_summax = credit_card_balance[,c(lapply(.SD, sum, na.rm = TRUE),
                                 lapply(.SD, max, na.rm = TRUE)), .SDcols = varSUMMAX, by = list(SK_ID_CURR,SK_ID_PREV)]
  names(temp_summax)[-c(1,2)] = paste(names(temp_summax)[-c(1,2)], rep(c("SUM","MAX"), each = length(varSUMMAX)), sep = "_")
  # 对于每一个SK_ID_PREV求MONTH_BALANCE的数量, 然后求MEAN进行汇总
  temp_month = credit_card_balance[, .(Nr_Card_MONTH = .N), by = list(SK_ID_CURR,SK_ID_PREV)]
  # # 对于每一个SK_ID_CURR求相对应的SK_ID_PREV的数量(一个CURR基本只有一个PREV)
  # temp_ID = credit_card_balance[,.(Nr_CREDIT_CARD = length(unique(SK_ID_PREV))), by = "SK_ID_CURR"]
  
  # 处理NAME_CONTRACT_STATUS, 合并level
  credit_card_balance$NAME_CONTRACT_STATUS[credit_card_balance$NAME_CONTRACT_STATUS != "Active" & 
                                           credit_card_balance$NAME_CONTRACT_STATUS != "Completed" & 
                                           credit_card_balance$NAME_CONTRACT_STATUS != "Signed"] = "Other"
  temp_cat = credit_card_balance[,.N,by = list(SK_ID_CURR, SK_ID_PREV,NAME_CONTRACT_STATUS)]
  temp_cat_wide = dcast(temp_cat, SK_ID_CURR + SK_ID_PREV ~ NAME_CONTRACT_STATUS, fill = 0, value.var = "N")
  names(temp_cat_wide)[-c(1,2)] = paste("NAME_CONTRACT_STATUS_CC",names(temp_cat_wide)[-c(1,2)],sep = "_")

  # 汇总变量, 对每一个SK_ID_CURR
  temp_month = temp_month[, lapply(.SD, mean, na.rm = TRUE), .SDcols = names(temp_month)[-c(1,2)], by = SK_ID_CURR]
  names(temp_month)[-1] = "Nr_Card_MONTH_MEAN"
  temp_cat_wide = temp_cat_wide[, lapply(.SD, sum, na.rm = TRUE), .SDcols = names(temp_cat_wide)[-c(1,2)], by = SK_ID_CURR]
  names(temp_cat_wide)[-1] = paste(names(temp_cat_wide)[-1], "SUM", sep = "_")
  temp_mean = temp_mean[,lapply(.SD, mean, na.rm = TRUE), .SDcols = names(temp_mean)[-c(1,2)], by = SK_ID_CURR]
  temp_sum = temp_summax[, lapply(.SD, sum, na.rm = TRUE), .SDcols = c("SK_DPD_SUM","SK_DPD_DEF_SUM","AMT_CREDIT_LIMIT_ACTUAL_SUM","CNT_INSTALMENT_MATURE_CUM_SUM"), by = SK_ID_CURR]
  temp_max = temp_summax[, lapply(.SD, max, na.rm = TRUE), .SDcols = c("SK_DPD_MAX","SK_DPD_DEF_MAX","AMT_CREDIT_LIMIT_ACTUAL_MAX","CNT_INSTALMENT_MATURE_CUM_MAX",
                                                                       paste(names(credit_card_balance)[grep("Add",names(credit_card_balance))], "MAX", sep="_")), by = SK_ID_CURR]
  
  # merge
  temp_all = merge(temp_month, temp_mean,all = TRUE, by = c("SK_ID_CURR"))
  temp_all = merge(temp_all, temp_sum,all = TRUE, by = c("SK_ID_CURR"))
  temp_all = merge(temp_all, temp_max,all = TRUE, by = c("SK_ID_CURR"))
  temp_all = merge(temp_all, temp_cat_wide, all = TRUE, by = c("SK_ID_CURR"))
  
  names(temp_all)[grep("SK_DPD",names(temp_all))] = paste(names(temp_all)[grep("SK_DPD",names(temp_all))],"CC",sep="_")
  names(temp_all)[grep("MONTHS_BALANCE",names(temp_all))] = paste(names(temp_all)[grep("MONTHS_BALANCE",names(temp_all))],"CC",sep="_")

  credit_card_balance = temp_all
  return(credit_card_balance)
}