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

  # 对每个变量通过求MAX,SUM和MEAN进行汇总, 以SK_ID_PREV
  varMEAN = setdiff(names(credit_card_balance),c("SK_ID_CURR","SK_ID_PREV","NAME_CONTRACT_STATUS"))
  varSUMMAX = c("SK_DPD", "SK_DPD_DEF","AMT_CREDIT_LIMIT_ACTUAL")
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
  temp_sum = temp_summax[, lapply(.SD, sum, na.rm = TRUE), .SDcols = c("SK_DPD_SUM","SK_DPD_DEF_SUM","AMT_CREDIT_LIMIT_ACTUAL_SUM"), by = SK_ID_CURR]
  temp_max = temp_summax[, lapply(.SD, max, na.rm = TRUE), .SDcols = c("SK_DPD_MAX","SK_DPD_DEF_MAX","AMT_CREDIT_LIMIT_ACTUAL_MAX"), by = SK_ID_CURR]
  
  # merge
  temp_all = merge(temp_month, temp_mean,all = TRUE, by = c("SK_ID_CURR"))
  temp_all = merge(temp_all, temp_sum,all = TRUE, by = c("SK_ID_CURR"))
  temp_all = merge(temp_all, temp_max,all = TRUE, by = c("SK_ID_CURR"))
  temp_all = merge(temp_all, temp_cat_wide, all = TRUE, by = c("SK_ID_CURR"))

  credit_card_balance = temp_all
  return(credit_card_balance)
}