#' @description Function to read the Daten and merge them
#' @return data.table
#' @author He Huang



readData = function(version) {
  library(rprojroot)
  library(data.table)
  library(mlr)
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
    IndexOfCategory = (numberOfValues < 20 | names(application) == "ORGANIZATION_TYPE" ) & (names(application) != "AMT_REQ_CREDIT_BUREAU_QRT")&
      (!(names(application) %in% c("CNT_CHILDREN", 
                                  "CNT_FAM_MEMBERS", 
                                  "EF_30_CNT_SOCIAL_CIRCL", 
                                  "DEF_60_CNT_SOCIAL_CIRCLE",
                                  "AMT_REQ_CREDIT_BUREAU_HOUR",
                                  "AMT_REQ_CREDIT_BUREAU_DAY",
                                  "AMT_REQ_CREDIT_BUREAU_WEEK",
                                  "AMT_REQ_CREDIT_BUREAU_QRT ")))
    
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
    
    
    # factor(category) 同意用常量 “miss” 填补
    application = impute(as.data.frame(application), classes = list(factor = imputeConstant("miss")))$data
    
    FAC = sapply(application, function(x) is.factor(x))
    names(application)[FAC]
    
    # 接下来把缺失值也填补了， 少数几个变量手动填补，其余的批量填补
    # numeric 用均值填补，然后生成心变量标注NA， factor直接填补为常量
    
    # AMT_ANNUITY 只有36个缺失值， 直接用均值填补，

    application$AMT_ANNUITY[is.na(application$AMT_ANNUITY)] = mean(application$AMT_ANNUITY, na.rm = TRUE)
    
    # AMT_GOODS_PRICE 只有278个确实值 ， 用均值填补
    application$AMT_GOODS_PRICE[is.na(application$AMT_GOODS_PRICE)] = mean(application$AMT_GOODS_PRICE, na.rm = TRUE)
    
    # CNT_FAM_MEMBERS 2
    application$CNT_FAM_MEMBERS[is.na(application$CNT_FAM_MEMBERS)] = mean(application$CNT_FAM_MEMBERS, na.rm = TRUE)
    
    # EXT_SOURCE_2  668 缺失值少 ， 均值填补
    application$EXT_SOURCE_2[is.na(application$EXT_SOURCE_2)] = mean(application$EXT_SOURCE_2, na.rm = TRUE)
    
    # DAYS_LAST_PHONE_CHANGE  1个缺失值， 均值填补
    application$DAYS_LAST_PHONE_CHANGE[is.na(application$DAYS_LAST_PHONE_CHANGE)] = mean(application$DAYS_LAST_PHONE_CHANGE, na.rm = TRUE)
    

    # numeric 标注NA, 然后用均值填补， 
    missValue = sapply(application, function(x) sum(is.na(x))) # too many missing value
    missValue = missValue[missValue >0]
    missValue = sort(missValue)
    
    #pdf(file.path(root,"plots", "missValueNumeric.pdf"))
    #barplot(missValue, las = 2)
    #dev.off()
    missVar = names(missValue)
    # barplot(missValue,las = 2,cex.lab=0.2)
    
    # OBS_30_CNT_SOCIAL_CIRCLE,OBS_60_CNT_SOCIAL_CIRCLE,DEF_60_CNT_SOCIAL_CIRCLE,
    # AMT_REQ_CREDIT_BUREAU_HOUR,AMT_REQ_CREDIT_BUREAU_DAY,AMT_REQ_CREDIT_BUREAU_WEEK,AMT_REQ_CREDIT_BUREAU_MON,AMT_REQ_CREDIT_BUREAU_YEAR,
    # YEARS_BEGINEXPLUATATION_AVG, YEARS_BEGINEXPLUATATION_MODE, YEARS_BEGINEXPLUATATION_MEDI,
    # FLOORSMAX_AVG ,FLOORSMAX_MODE,FLOORSMAX_MEDI,
    # LIVINGAREA_AVG,LIVINGAREA_MODE,LIVINGAREA_MEDI,
    # ENTRANCES_AVG,ENTRANCES_MODE,ENTRANCES_MEDI,
    # ...
    
    
    application$NA_CIRCLE = as.integer(is.na(application$OBS_30_CNT_SOCIAL_CIRCLE))
    application$NA_AMT_REQ = as.integer(is.na(application$AMT_REQ_CREDIT_BUREAU_DAY))
    application$NA_YEARS_BEGINEXPLUATATION = as.integer(is.na(application$YEARS_BEGINEXPLUATATION_AVG))
    application$NA_FLOORSMAX = as.integer(is.na(application$FLOORSMAX_AVG))
    application$NA_LIVINGAREA = as.integer(is.na(application$LANDAREA_AVG))
    application$NA_ENTRANCES = as.integer(is.na(application$ENTRANCES_AVG))
    application$NA_APARTMENT = as.integer(is.na(application$APARTMENTS_AVG))
    application$NA_ELEVATORS = as.integer(is.na(application$ELEVATORS_AVG))
    application$NA_NONLIVINGAREA = as.integer(is.na(application$NONLIVINGAREA_AVG))
    application$NA_BASEMENTAREA = as.integer(is.na(application$BASEMENTAREA_AVG))
    application$NA_LANDAREA = as.integer(is.na(application$LANDAREA_AVG))
    application$NA_YEARS_BUILD = as.integer(is.na(application$YEARS_BUILD_AVG))
    application$NA_FLOORSMIN = as.integer(is.na(application$FLOORSMIN_AVG))
    application$NA_LIVINGAPARTMENTS = as.integer(is.na(application$LIVINGAPARTMENTS_AVG))
    application$NA_NONLIVINGAPARTMENTS = as.integer(is.na(application$NONLIVINGAPARTMENTS_AVG))
    application$NA_COMMONAREA = as.integer(is.na(application$COMMONAREA_AVG))
    
    # EXT_SOURCE_3 TOTALAREA_MODE  EXT_SOURCE_1 OWN_CAR_AGE
    application$NA_EXT_SOURCE_3 = as.integer(is.na(application$EXT_SOURCE_3))
    application$NA_EXT_SOURCE_1 = as.integer(is.na(application$EXT_SOURCE_1))
    application$NA_TOTALAREA_MODE = as.integer(is.na(application$TOTALAREA_MODE))
    application$NA_OWN_CAR_AGE = as.integer(is.na(application$OWN_CAR_AGE))
    
    application = impute(application, classes = list(numeric = imputeMean()))$data
    
    application = as.data.table(application)
  }
  
  ##############################################################################
  ################ II. bureau and bureau_balance   #############################
  ##############################################################################
  if(version >= 2){
    source(file.path(root, "data", "readdata_bureau.R"))
    bureau = readData()
    source(file.path(root, "data", "readdata_bureau_balance.R"))
    bureau_balance = readData()
    
    
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
    tmp2_wide = dcast(tmp2, SK_ID_CURR ~ CREDIT_ACTIVE, fill = 0)
    names(tmp2_wide) = c("SK_ID_CURR",
                         "CREDIT_ACTIVE_Active",
                         "CREDIT_ACTIVE_badDebt",
                         "CREDIT_ACTIVE_Closed", 
                         "CREDIT_ACTIVE_Sold")
    
    # 3. CREDIT_CURRENCY
    tmp3 = bureauMerged[,.N, 
                        by = list(SK_ID_CURR,CREDIT_CURRENCY)]
    tmp3_wide = dcast(tmp3, SK_ID_CURR ~ CREDIT_CURRENCY, fill = 0)
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
    tmp5 = bureauMerged[,.(DAYS_CREDIT_ENDDATE_MAX = max(DAYS_CREDIT_ENDDATE, na.rm = TRUE), 
                           DAYS_CREDIT_ENDDATE_MIN = min(DAYS_CREDIT_ENDDATE, na.rm = TRUE)), 
                        by = SK_ID_CURR] 
    tmp5$DAYS_CREDIT_ENDDATE_MAX[is.infinite(tmp5$DAYS_CREDIT_ENDDATE_MAX)] = NA
    tmp5$DAYS_CREDIT_ENDDATE_MAX[is.infinite(tmp5$DAYS_CREDIT_ENDDATE_MIN)] = NA
    
    # 6. DAYS_ENDDATE_FACT  # only for closed
    tmp6 = bureauMerged[,.(DAYS_ENDDATE_FACT_MAX = max(DAYS_ENDDATE_FACT, na.rm = TRUE), 
                           DAYS_ENDDATE_FACT_MIN = min(DAYS_ENDDATE_FACT, na.rm = TRUE)), 
                        by = SK_ID_CURR] 
    tmp6$DAYS_ENDDATE_FACT_MAX[is.infinite(tmp6$DAYS_ENDDATE_FACT_MAX)] = NA
    tmp6$DAYS_ENDDATE_FACT_MIN[is.infinite(tmp6$DAYS_ENDDATE_FACT_MIN)] = NA
    
    # 7. AMT_CREDIT_MAX_OVERDUE    或者：可以把NA填补为0
    tmp7 = bureauMerged[,.(AMT_CREDIT_MAX_OVERDUE_MAX = max(AMT_CREDIT_MAX_OVERDUE, na.rm = TRUE), 
                           AMT_CREDIT_MAX_OVERDUE_MIN = min(AMT_CREDIT_MAX_OVERDUE, na.rm = TRUE)), 
                        by = SK_ID_CURR] 
    tmp7$AMT_CREDIT_MAX_OVERDUE_MAX[is.infinite(tmp7$AMT_CREDIT_MAX_OVERDUE_MAX)] = NA
    tmp7$AMT_CREDIT_MAX_OVERDUE_MIN[is.infinite(tmp7$AMT_CREDIT_MAX_OVERDUE_MIN)] = NA
    
    # 8. CNT_CREDIT_PROLONG
    tmp8 = bureauMerged[,.(CNT_CREDIT_PROLONG_MAX = max(CNT_CREDIT_PROLONG, na.rm = TRUE), 
                           CNT_CREDIT_PROLONG_MIN = min(CNT_CREDIT_PROLONG, na.rm = TRUE)), 
                        by = SK_ID_CURR] 

    # 9. AMT_CREDIT_SUM # 个人认为这个求和就可以
    tmp9 = bureauMerged[,.(AMT_CREDIT_SUM = sum(AMT_CREDIT_SUM, na.rm = TRUE)), by = SK_ID_CURR]
    
    # 10. AMT_CREDIT_SUM_DEBT
    tmp10 = bureauMerged[,.(AMT_CREDIT_SUM_DEBT = sum(AMT_CREDIT_SUM_DEBT, na.rm = TRUE)), by = SK_ID_CURR]
    
    # 11. AMT_CREDIT_SUM_LIMIT # 或者用min代替max?
    tmp11 = bureauMerged[,.(AMT_CREDIT_SUM_LIMIT = max(AMT_CREDIT_SUM_LIMIT, na.rm = TRUE)), by = SK_ID_CURR]
    tmp11$AMT_CREDIT_SUM_LIMIT[is.infinite(tmp11$AMT_CREDIT_SUM_LIMIT)] = NA
    
    # 12. AMT_CREDIT_SUM_OVERDUE
    tmp12 = bureauMerged[,.(AMT_CREDIT_SUM_OVERDUE = sum(AMT_CREDIT_SUM_OVERDUE, na.rm = TRUE)), by = SK_ID_CURR]
    
    # 13. CREDIT_TYPE
    tmp13 = bureauMerged[,.N, 
                        by = list(SK_ID_CURR,CREDIT_TYPE)]
    tmp13_wide = dcast(tmp13, SK_ID_CURR ~ CREDIT_TYPE, fill = 0)
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
    tmp15 = bureauMerged[,.(AMT_ANNUITY = sum(AMT_ANNUITY, na.rm = TRUE)), by = SK_ID_CURR]
    
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
    
    
    application = merge(application, TMP, all.x = TRUE, by = "SK_ID_CURR")
  }

  
  ##############################################################################
  #III. credit_card_balance installments_payments  POS_CASH_balance  previous_application
  ##############################################################################
  if(version >= 3){
    source(file.path(root, "data", "readdata_credit_card_balance.R"))
    credit_card_balance = readData()
    source(file.path(root, "data", "readdata_installments_payments.R"))
    installments_payments = readData()
    merged_1 = merge(credit_card_balance, installments_payments, all.x = TRUE, by = "SK_ID_PREV")
    
    
    source(file.path(root, "data", "readdata_POS_CASH_balance.R"))
    POS_CASH_balance = readData()
    merged_2 = merge(POS_CASH_balance, merged_1, all.x = TRUE, by = "SK_ID_PREV")
    
    source(file.path(root, "data", "readdata_previous_application.R"))
    previous_application = readData()
    
    merged_3 = merge(previous_application, merged_2, all.x = TRUE, by = "SK_ID_PREV")
    
    application = merge(application, merged_3, all.x = TRUE, by = "SK_ID_CURR")
  }
  
  return(application)
}
