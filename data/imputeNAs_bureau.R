imputeNA <- function(application,method = "mean"){
  application$NA_BUREAU = as.integer(is.na(application$Nr_BUREAU))
  application$NA_DAYS_CREDIT_ENDDATE = as.integer(is.na(application$DAYS_CREDIT_ENDDATE_MAX))
  application$NA_AMT_CREDIT_SUM_LIMIT = as.integer(is.na(application$AMT_CREDIT_SUM_LIMIT_MAX))
  application$NA_DAYS_ENDDATE_FACT = as.integer(is.na(application$DAYS_ENDDATE_FACT_MAX))
  application$NA_AMT_CREDIT_MAX_OVERDUE = as.integer(is.na(application$AMT_CREDIT_MAX_OVERDUE_MAX))
  application$NA_BUREAU_STATUS_MEAN = as.integer(is.na(application$BUREAU_STATUS_0_MEAN))
  application$NA_AMT_ANNUITY_MEAN = as.integer(is.na(application$AMT_ANNUITY_MEAN))
  if (method == "mixed"){
    application = impute(as.data.frame(application), 
                         cols = list(CREDIT_DAY_OVERDUE_MEAN = 0,
                                     CREDIT_DAY_OVERDUE_MAX = 0,
                                     AMT_CREDIT_MAX_OVERDUE_MAX = 0,
                                     AMT_CREDIT_MAX_OVERDUE_MIN = 0,
                                     AMT_CREDIT_MAX_OVERDUE_MEAN = 0,
                                     CNT_CREDIT_PROLONG_MAX = 0,
                                     CNT_CREDIT_PROLONG_MIN = 0,
                                     AMT_CREDIT_SUM_OVERDUE_SUM = 0,
                                     AMT_CREDIT_SUM_OVERDUE_MEAN = 0))$data
  }
  application = impute(application, 
                       target = "TARGET", 
                       classes = list(numeric = imputeMean(),
                                      integer = imputeMean()))$data
  application = as.data.table(application)
  return(application)
}