
readData = function(version = 1){
  library(rprojroot)
  library(data.table)
  root = find_root(is_git_root)
  setwd(root)
  
  bureau = fread("../data/bureau.csv", na.strings = "")
  bureau_balance = fread("../data/bureau_balance.csv", na.strings = "")
  
  tmp = bureau_balance[,.N, by = list(SK_ID_BUREAU,STATUS)]
  tmp = tmp[, percentage := N/sum(N), by = SK_ID_BUREAU]
  wideData = dcast(tmp, SK_ID_BUREAU ~ STATUS, value.var = "percentage", fill = 0)
  names(wideData) = c("SK_ID_BUREAU", 
                      "BUREAU_STATUS_0", 
                      "BUREAU_STATUS_1", 
                      "BUREAU_STATUS_2", 
                      "BUREAU_STATUS_3",
                      "BUREAU_STATUS_4",
                      "BUREAU_STATUS_5",
                      "BUREAU_STATUS_C",
                      "BUREAU_STATUS_X")
  bureau_balance = wideData
  
  cat("number of different SK_ID_BUREAU in Table bureau : ",length(unique(bureau$SK_ID_BUREAU)), "\n")
  cat("number of different SK_ID_BUREAU in Table bureau_balance : ",length(unique(bureau_balance$SK_ID_BUREAU)), "\n")
  cat("that means, not every SK_ID_BUREAU has balance", "\n")
  cat("Are all SK_ID_BUREAU in Table bureau_balance also in Table bureau? :",all(unique(bureau_balance$SK_ID_BUREAU) %in% unique(bureau$SK_ID_BUREAU)), "\n")
  
  
  bureauMerged = merge(bureau, bureau_balance, all.x = TRUE, by = "SK_ID_BUREAU")
  
  #把每个变量按照SK_ID_CURR汇总， 这样才好与 application 合并：
  # 0. SK_ID_BUREAU
  tmp0 = bureauMerged[,.N, by = SK_ID_CURR]
  names(tmp0) = c("SK_ID_CURR", "Nr_BUREAU")
  
  # 1. CREDIT_DAY_OVERDUE
  tmp1 = bureauMerged[,.(TOTAL_CREDIT_DAY_OVERDUE = sum(CREDIT_DAY_OVERDUE, na.rm = TRUE)), 
                      by = SK_ID_CURR]
  # 2. CREDIT_ACTIVE
  tmp2 = bureauMerged[,.N, 
                      by = list(SK_ID_CURR,CREDIT_ACTIVE)]
  tmp2_wide = dcast(tmp2, SK_ID_CURR ~ CREDIT_ACTIVE, fill = 0, value.var = "N")
  names(tmp2_wide) = c("SK_ID_CURR",
                       "CREDIT_ACTIVE_Active",
                       "CREDIT_ACTIVE_badDebt",
                       "CREDIT_ACTIVE_Closed", 
                       "CREDIT_ACTIVE_Sold")
  
  # 3. CREDIT_CURRENCY
  tmp3 = bureauMerged[,.N, 
                      by = list(SK_ID_CURR,CREDIT_CURRENCY)]
  tmp3_wide = dcast(tmp3, SK_ID_CURR ~ CREDIT_CURRENCY, fill = 0, value.var = "N")
  names(tmp3_wide) = c("SK_ID_CURR", 
                       "CREDIT_CURRENCY_1",
                       "CREDIT_CURRENCY_2",
                       "CREDIT_CURRENCY_3",
                       "CREDIT_CURRENCY_4")
  
  # 4. DAYS_CREDIT
  tmp4 = bureauMerged[,.(DAYS_CREDIT_MAX = max(DAYS_CREDIT, na.rm = TRUE), 
                         DAYS_CREDIT_MIN = min(DAYS_CREDIT, na.rm = TRUE)), 
                      by = SK_ID_CURR] 
  
  # 5. DAYS_CREDIT_ENDDATE
  if (version == 2){
    # fill NA with mean (with respect to each CREDIT_ACTIVE status)
    bureauMerged$DAYS_CREDIT_ENDDATE[is.na(bureauMerged$DAYS_CREDIT_ENDDATE) & bureauMerged$CREDIT_ACTIVE == "Active"] = bureauMerged[CREDIT_ACTIVE == "Active", .(mu = mean(DAYS_CREDIT_ENDDATE, na.rm = TRUE))]$mu
    bureauMerged$DAYS_CREDIT_ENDDATE[is.na(bureauMerged$DAYS_CREDIT_ENDDATE) & bureauMerged$CREDIT_ACTIVE == "Bad debt"] = bureauMerged[CREDIT_ACTIVE == "Bad debt", .(mu = mean(DAYS_CREDIT_ENDDATE, na.rm = TRUE))]$mu
    bureauMerged$DAYS_CREDIT_ENDDATE[is.na(bureauMerged$DAYS_CREDIT_ENDDATE) & bureauMerged$CREDIT_ACTIVE == "Closed"] = bureauMerged[CREDIT_ACTIVE == "Closed", .(mu = mean(DAYS_CREDIT_ENDDATE, na.rm = TRUE))]$mu
    bureauMerged$DAYS_CREDIT_ENDDATE[is.na(bureauMerged$DAYS_CREDIT_ENDDATE) & bureauMerged$CREDIT_ACTIVE == "Sold"] = bureauMerged[CREDIT_ACTIVE == "Sold", .(mu = mean(DAYS_CREDIT_ENDDATE, na.rm = TRUE))]$mu
  }
  tmp5 = bureauMerged[,.(DAYS_CREDIT_ENDDATE_MAX = max(DAYS_CREDIT_ENDDATE, na.rm = TRUE), 
                         DAYS_CREDIT_ENDDATE_MIN = min(DAYS_CREDIT_ENDDATE, na.rm = TRUE)), 
                      by = SK_ID_CURR] 
  tmp5$DAYS_CREDIT_ENDDATE_MAX[is.infinite(tmp5$DAYS_CREDIT_ENDDATE_MAX)] = NA
  tmp5$DAYS_CREDIT_ENDDATE_MIN[is.infinite(tmp5$DAYS_CREDIT_ENDDATE_MIN)] = NA
  
  # 6. DAYS_ENDDATE_FACT  # only for closed
  ## Problem: DAYS_ENDDATE_FACT only for Closed Credit, but found 3627 entries with DAYS_ENDDATE_FACT and credit is not closed
  # bureauMerged[CREDIT_ACTIVE != "Closed" & !is.na(DAYS_ENDDATE_FACT),.N]
  if (version == 2){
    #fill NA with mean of DAYS_ENDDATE_FACT (closed entries) 
    bureauMerged$DAYS_ENDDATE_FACT[is.na(bureauMerged$DAYS_ENDDATE_FACT) & bureauMerged$CREDIT_ACTIVE == "Closed"] = bureauMerged[CREDIT_ACTIVE == "Closed", .(mu = mean(DAYS_ENDDATE_FACT, na.rm = TRUE))]$mu
  }
  tmp6 = bureauMerged[,.(DAYS_ENDDATE_FACT_MAX = max(DAYS_ENDDATE_FACT, na.rm = TRUE), 
                         DAYS_ENDDATE_FACT_MIN = min(DAYS_ENDDATE_FACT, na.rm = TRUE)), 
                      by = SK_ID_CURR] 
  # non-closed credit does not have DAYS_ENDDATE_FACT by construction
  tmp6$DAYS_ENDDATE_FACT_MAX[is.infinite(tmp6$DAYS_ENDDATE_FACT_MAX)] = NA
  tmp6$DAYS_ENDDATE_FACT_MIN[is.infinite(tmp6$DAYS_ENDDATE_FACT_MIN)] = NA
  
  # 7. AMT_CREDIT_MAX_OVERDUE    或者：可以把NA填补为0 (同意)
  if (version == 2){
    bureauMerged$AMT_CREDIT_MAX_OVERDUE[is.na(bureauMerged$AMT_CREDIT_MAX_OVERDUE)] = 0
  }
  tmp7 = bureauMerged[,.(AMT_CREDIT_MAX_OVERDUE_MAX = max(AMT_CREDIT_MAX_OVERDUE, na.rm = TRUE), 
                         AMT_CREDIT_MAX_OVERDUE_MIN = min(AMT_CREDIT_MAX_OVERDUE, na.rm = TRUE)), 
                      by = SK_ID_CURR] 
  tmp7$AMT_CREDIT_MAX_OVERDUE_MAX[is.infinite(tmp7$AMT_CREDIT_MAX_OVERDUE_MAX)] = NA
  tmp7$AMT_CREDIT_MAX_OVERDUE_MIN[is.infinite(tmp7$AMT_CREDIT_MAX_OVERDUE_MIN)] = NA
  
  # 8. CNT_CREDIT_PROLONG
  tmp8 = bureauMerged[,.(CNT_CREDIT_PROLONG_MAX = max(CNT_CREDIT_PROLONG, na.rm = TRUE), 
                         CNT_CREDIT_PROLONG_MIN = min(CNT_CREDIT_PROLONG, na.rm = TRUE)), 
                      by = SK_ID_CURR] 
  
  # 9. AMT_CREDIT_SUM # 个人认为这个求和就可以 (个人认为用mean或者max)
  tmp9 = bureauMerged[,.(AMT_CREDIT_SUM = sum(AMT_CREDIT_SUM, na.rm = TRUE)), by = SK_ID_CURR]
  
  # 10. AMT_CREDIT_SUM_DEBT (个人认为用mean或者max)
  tmp10 = bureauMerged[,.(AMT_CREDIT_SUM_DEBT = sum(AMT_CREDIT_SUM_DEBT, na.rm = TRUE)), by = SK_ID_CURR]
  
  # 11. AMT_CREDIT_SUM_LIMIT # 或者用min代替max? (个人认为此处为credit card limit, 个人认为用max)
  tmp11 = bureauMerged[,.(AMT_CREDIT_SUM_LIMIT = max(AMT_CREDIT_SUM_LIMIT, na.rm = TRUE)), by = SK_ID_CURR]
  tmp11$AMT_CREDIT_SUM_LIMIT[is.infinite(tmp11$AMT_CREDIT_SUM_LIMIT)] = NA
  if (version == 2){
    tmp11$AMT_CREDIT_SUM_LIMIT[is.infinite(tmp11$AMT_CREDIT_SUM_LIMIT)] = 0
  }
  # 12. AMT_CREDIT_SUM_OVERDUE
  tmp12 = bureauMerged[,.(AMT_CREDIT_SUM_OVERDUE = sum(AMT_CREDIT_SUM_OVERDUE, na.rm = TRUE)), by = SK_ID_CURR]
  
  # 13. CREDIT_TYPE
  tmp13 = bureauMerged[,.N, 
                       by = list(SK_ID_CURR,CREDIT_TYPE)]
  tmp13_wide = dcast(tmp13, SK_ID_CURR ~ CREDIT_TYPE, fill = 0, value.var = "N")
  names(tmp13_wide) = c(
    "SK_ID_CURR",
    "Another_type_loan_Nr" ,                       
    "Car_loan_Nr" ,                                    
    "Cash_loan_Nr" ,                  
    "Consumer_credit_Nr"  ,                            
    "Credit_card_Nr",                                 
    "Interbank_credit_Nr",                             
    "Loan_business_development_Nr" ,              
    "Loan_purchase_shares_Nr" ,
    "Loan_purchase_equipment_Nr" ,         
    "Loan_workingcapital_replenishment_Nr",       
    "Microloan_Nr"        ,                           
    "Mobile_operator_loan_Nr" ,                        
    "Mortgage_Nr"    ,                                
    "Real_estate_loan_Nr" ,                            
    "Unknown_type_loan_Nr"
  )
  
  # 14. DAYS_CREDIT_UPDATE # 取max， 因为是负数， 那么就是最新的日期
  tmp14 = bureauMerged[,.(DAYS_CREDIT_UPDATE = max(DAYS_CREDIT_UPDATE, na.rm = TRUE)), by = SK_ID_CURR]
  
  # 15. AMT_ANNUITY #不知道年金是什么，暂时求和吧
  tmp15 = bureauMerged[,.(AMT_ANNUITY_BUREAU = sum(AMT_ANNUITY, na.rm = TRUE)), by = SK_ID_CURR] # application总表有变量名为AMT_ANNUITY
  
  # 16. bureau_balance #都求和吧
  tmp16 = bureauMerged[,.(BUREAU_STATUS_0 = sum(BUREAU_STATUS_0, na.rm = TRUE),
                          BUREAU_STATUS_1 = sum(BUREAU_STATUS_1, na.rm = TRUE),
                          BUREAU_STATUS_2 = sum(BUREAU_STATUS_2, na.rm = TRUE),
                          BUREAU_STATUS_3 = sum(BUREAU_STATUS_3, na.rm = TRUE),
                          BUREAU_STATUS_4 = sum(BUREAU_STATUS_5, na.rm = TRUE),
                          BUREAU_STATUS_5 = sum(BUREAU_STATUS_5, na.rm = TRUE),
                          BUREAU_STATUS_C = sum(BUREAU_STATUS_C, na.rm = TRUE),
                          BUREAU_STATUS_X = sum(BUREAU_STATUS_X, na.rm = TRUE)), by = SK_ID_CURR]
  
  
  # 然后把tmp0 - tmp16 合并到一个表
  TMP = merge(tmp0, tmp1, all = TRUE, by =  "SK_ID_CURR")
  TMP = merge(TMP, tmp2_wide, all = TRUE, by =  "SK_ID_CURR")
  TMP = merge(TMP, tmp3_wide, all = TRUE, by =  "SK_ID_CURR")
  TMP = merge(TMP, tmp4, all = TRUE, by =  "SK_ID_CURR")
  TMP = merge(TMP, tmp5, all = TRUE, by =  "SK_ID_CURR")
  TMP = merge(TMP, tmp6, all = TRUE, by =  "SK_ID_CURR")
  TMP = merge(TMP, tmp7, all = TRUE, by =  "SK_ID_CURR")
  TMP = merge(TMP, tmp8, all = TRUE, by =  "SK_ID_CURR")
  TMP = merge(TMP, tmp9, all = TRUE, by =  "SK_ID_CURR")
  TMP = merge(TMP, tmp10, all = TRUE, by =  "SK_ID_CURR")
  TMP = merge(TMP, tmp11, all = TRUE, by =  "SK_ID_CURR")
  TMP = merge(TMP, tmp12, all = TRUE, by =  "SK_ID_CURR")
  TMP = merge(TMP, tmp13_wide, all = TRUE, by =  "SK_ID_CURR")
  TMP = merge(TMP, tmp14, all = TRUE, by =  "SK_ID_CURR")
  TMP = merge(TMP, tmp15, all = TRUE, by =  "SK_ID_CURR")
  TMP = merge(TMP, tmp16, all = TRUE, by =  "SK_ID_CURR")
  
  # TMP 这个表基本汇总和涵盖了 bureau 和 bureau_balance 这两个表的所有信息
  
  return(TMP)
}