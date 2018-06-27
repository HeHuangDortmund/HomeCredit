imputeNA <- function(application,method = "mean"){
  application$NA_BUREAU = as.integer(is.na(application$Nr_BUREAU))
  application$NA_DAYS_CREDIT_ENDDATE = as.integer(is.na(application$DAYS_CREDIT_ENDDATE_MAX))
  application$NA_AMT_CREDIT_SUM_LIMIT = as.integer(is.na(application$AMT_CREDIT_SUM_LIMIT))
  application$NA_DAYS_ENDDATE_FACT = as.integer(is.na(application$DAYS_ENDDATE_FACT_MAX))
  application$NA_AMT_CREDIT_MAX_OVERDUE = as.integer(is.na(application$AMT_CREDIT_MAX_OVERDUE_MAX))
  if (method == "mixed"){
    application = impute(as.data.frame(application), 
                         cols = list(TOTAL_CREDIT_DAY_OVERDUE = 0,
                                     AMT_CREDIT_MAX_OVERDUE_MAX = 0,
                                     AMT_CREDIT_MAX_OVERDUE_MIN = 0,
                                     CNT_CREDIT_PROLONG_MAX = 0,
                                     CNT_CREDIT_PROLONG_MIN = 0,
                                     AMT_CREDIT_SUM_OVERDUE = 0))$data
  }
  application = impute(application, 
                       target = "TARGET", 
                       classes = list(numeric = imputeMean(),
                                      integer = imputeMean()))$data
  application = as.data.table(application)
  return(application)
}