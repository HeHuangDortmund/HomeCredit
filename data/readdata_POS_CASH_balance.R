readData = function(version = 1){
  library(rprojroot)
  library(data.table)
  root = find_root(is_git_root)
  setwd(root)

  POS_CASH_balance = fread("../data/POS_CASH_balance.csv", na.strings = "")
  if (version == 2){ # 在汇总前先填一部分NA
    POS_CASH_balance$CNT_INSTALMENT[is.na(POS_CASH_balance$CNT_INSTALMENT)] = mean(POS_CASH_balance$CNT_INSTALMENT, na.rm = TRUE)
    POS_CASH_balance$CNT_INSTALMENT_FUTURE[is.na(POS_CASH_balance$CNT_INSTALMENT_FUTURE)] = mean(POS_CASH_balance$CNT_INSTALMENT_FUTURE, na.rm = TRUE)
  }
  POS_CASH_balance[, Add_RATIO_INSTALMENT_LEFT := CNT_INSTALMENT_FUTURE / CNT_INSTALMENT]
  
  ############################## calculate the trend and intercept variables ###################################################################
  # POS_CASH_balance = POS_CASH_balance[order(SK_ID_CURR, SK_ID_PREV, MONTHS_BALANCE)]
  # temp_data = POS_CASH_balance[!is.na(Add_RATIO_INSTALMENT_LEFT)]
  # temp_trend1 = temp_data[, lapply(.SD, function(x) {lm(x~(seq(1,length(MONTHS_BALANCE),by=1)))$coefficients[1]}),
  #                                .SDcols = c("Add_RATIO_INSTALMENT_LEFT"),
  #                                by = SK_ID_PREV] # 14min
  # temp_trend2 = temp_data[, lapply(.SD, function(x) {lm(x~(seq(1,length(MONTHS_BALANCE),by=1)))$coefficients[2]}),
  #                         .SDcols = c("Add_RATIO_INSTALMENT_LEFT"),
  #                         by = SK_ID_PREV]
  # temp_trend = merge(temp_trend1, temp_trend2, all = TRUE, by = c("SK_ID_PREV"))
  ##############################################################################################################################################
  
  temp_trend = fread("trend_pos_cash.txt", drop = "V1")
  
  # 对每个变量通过求MEAN和MAX进行汇总
  temp_name = setdiff(names(POS_CASH_balance),c("SK_ID_PREV","SK_ID_CURR","NAME_CONTRACT_STATUS"))
  temp = POS_CASH_balance[, c(lapply(.SD, mean, na.rm = TRUE),
                          lapply(.SD, max, na.rm = TRUE)), .SDcols = temp_name, by = SK_ID_CURR]
  setnames(temp, 2:ncol(temp),paste(temp_name, rep(c('MEAN','MAX'),each = length(temp_name)), sep = "_"))
  temp = temp[,c("CNT_INSTALMENT_MAX","CNT_INSTALMENT_FUTURE_MAX") := NULL] # drop 2 variables
  temp[temp == Inf | temp == -Inf] = NA # 对于Inf/-Inf用NA替换
  
  # 对于每一个SK_ID_PREV求MONTH_BALANCE的数量, 然后求MEAN进行汇总
  temp2 = POS_CASH_balance[,.(Nr_POSCASH_MONTH = .N),by = list(SK_ID_CURR,SK_ID_PREV)]
  ## merge temp_trend
  temp2 = merge(temp2, temp_trend, all = TRUE, by = "SK_ID_PREV")
  temp2 = temp2[, lapply(.SD, mean, na.rm = TRUE), .SDcols = names(temp2)[-c(1,2)],by = SK_ID_CURR]
  names(temp2)[-1] = paste(names(temp2)[-1],"MEAN",sep = "_")
  
  # 对于每一个SK_ID_CURR求相对应的SK_ID_PREV的数量
  temp3 = POS_CASH_balance[,.(Nr_POS_CASH = length(unique(SK_ID_PREV))), by = "SK_ID_CURR"]
  
  # merge
  temp = merge(temp2, temp, all = TRUE, by = "SK_ID_CURR")
  temp = merge(temp3, temp, all = TRUE, by = "SK_ID_CURR")
  rm(temp2,temp3)

  # 处理唯一的一个categorical变量
  # XNA = NA
  POS_CASH_balance$NAME_CONTRACT_STATUS[POS_CASH_balance$NAME_CONTRACT_STATUS == "XNA"] = NA
  # 此处或先不把Canceled和NA两个类别合并, 之后尝试使用mergeSmallFactorLevels合并?
  POS_CASH_balance$NAME_CONTRACT_STATUS[POS_CASH_balance$NAME_CONTRACT_STATUS == "Canceled" | is.na(POS_CASH_balance$NAME_CONTRACT_STATUS)] = "Other"
  temp_cat = POS_CASH_balance[,.N,by = list(SK_ID_CURR, NAME_CONTRACT_STATUS)]
  temp_cat_wide = dcast(temp_cat, SK_ID_CURR~NAME_CONTRACT_STATUS, fill = 0, value.var = "N")
  names(temp_cat_wide)[-1] = paste("NAME_CONTRACT_STATUS_POSCASH", names(temp_cat_wide)[-1],sep = "_")
  # merge
  POS_CASH_balance = merge(temp,temp_cat_wide,all = TRUE, by = "SK_ID_CURR")
  rm(temp,temp_cat,temp_cat_wide)
  
  POS_CASH_balance[,c("Add_RATIO_INSTALMENT_LEFT_MAX")] = NULL
  names(POS_CASH_balance)[grep("SK_DPD",names(POS_CASH_balance))] = paste(names(POS_CASH_balance)[grep("SK_DPD",names(POS_CASH_balance))],"POS",sep="_")
  names(POS_CASH_balance)[grep("MONTHS_BALANCE",names(POS_CASH_balance))] = paste(names(POS_CASH_balance)[grep("MONTHS_BALANCE",names(POS_CASH_balance))],"POS",sep="_")
  
  return(POS_CASH_balance)
}