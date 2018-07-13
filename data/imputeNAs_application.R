imputeNA <- function(application,
                     exploratory = 0, # 1 produces exploratory plot
                     naDrop = FALSE){ # naDrop=TRUE drops variables with too many NAs
  
  # factor(category) 同意用常量 “miss” 填补
  application = impute(as.data.frame(application), classes = list(factor = imputeConstant("miss")))$data
  
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
  if (exploratory == 1){
    missValue = sapply(application, function(x) sum(is.na(x))) # too many missing value
    missValue = missValue[missValue >0]
    missValue = sort(missValue)
    missVar = names(missValue)
    
    pdf(file.path(root,"plots", "missValueNumeric.pdf"))
    barplot(missValue, las = 2, cex.lab=0.2)
    dev.off()
  }
  
  if (naDrop == FALSE){
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
    
    # EXT_SOURCE_3 TOTALAREA_MODE  EXT_SOURCE_1 OWN_CAR_AGE DAYS_EMPLOYED
    application$NA_EXT_SOURCE_3 = as.integer(is.na(application$EXT_SOURCE_3))
    application$NA_EXT_SOURCE_1 = as.integer(is.na(application$EXT_SOURCE_1))
    application$NA_TOTALAREA_MODE = as.integer(is.na(application$TOTALAREA_MODE))
    application$NA_OWN_CAR_AGE = as.integer(is.na(application$OWN_CAR_AGE))
    application$NA_DAYS_EMPLOYED = as.integer(is.na(application$DAYS_EMPLOYED))
    
    application = impute(application, classes = list(numeric = imputeMean()))$data
    application = impute(application, cols = list(DAYS_EMPLOYED = imputeMean()))$data
  } else { # drops variables with too many NAs (percentage > 50%)
    missValue = sapply(application, function(x) sum(is.na(x))) 
    missValue = missValue[missValue > 0]/dim(application)[1]
    # barplot(missValue,las = 2,cex.lab=0.2)
    varDrop = names(missValue)[missValue > 2/3] # EXT_SOURCE_1, OWN_CAR_AGE are kept
    setDT(application)
    application = application[,c(varDrop) := NULL]
    
    application$NA_CIRCLE = as.integer(is.na(application$OBS_30_CNT_SOCIAL_CIRCLE))
    application$NA_AMT_REQ = as.integer(is.na(application$AMT_REQ_CREDIT_BUREAU_DAY))
    application$NA_DAYS_EMPLOYED = as.integer(is.na(application$DAYS_EMPLOYED))
    application$NA_EXT_SOURCE_3 = as.integer(is.na(application$EXT_SOURCE_3))
    application$NA_TOTALAREA_MODE = as.integer(is.na(application$TOTALAREA_MODE))
    application$NA_YEARS_BEGINEXPLUATATION = as.integer(is.na(application$YEARS_BEGINEXPLUATATION_AVG))
    application$NA_FLOORSMAX = as.integer(is.na(application$FLOORSMAX_AVG))
    application$NA_LIVINGAREA = as.integer(is.na(application$LIVINGAREA_AVG))
    application$NA_ENTRANCES = as.integer(is.na(application$ENTRANCES_AVG))
    application$NA_APARTMENT = as.integer(is.na(application$APARTMENTS_AVG))
    application$NA_ELEVATORS = as.integer(is.na(application$ELEVATORS_AVG))
    application$NA_EXT_SOURCE_1 = as.integer(is.na(application$EXT_SOURCE_1))
    application$NA_NONLIVINGAREA = as.integer(is.na(application$NONLIVINGAREA_AVG))
    application$NA_BASEMENTAREA = as.integer(is.na(application$BASEMENTAREA_AVG))
    application$NA_LANDAREA = as.integer(is.na(application$LANDAREA_AVG))
    application$NA_YEARS_BUILD = as.integer(is.na(application$YEARS_BUILD_AVG))
 
    application = impute(as.data.frame(application), classes = list(numeric = imputeMean()))$data
    application = impute(application, cols = list(DAYS_EMPLOYED = imputeMean()))$data
  }
  
  application = as.data.table(application)
  return(application)
}