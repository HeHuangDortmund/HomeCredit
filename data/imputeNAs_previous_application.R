imputeNA <- function(application){
  application$NA_PREV_APPLICATION  = as.integer(is.na(application$CNT_PREV))
  application$NA_AMT_DOWN_PAYMENT_MEAN = as.integer(is.na(application$AMT_DOWN_PAYMENT_MEAN))
  application$NA_DAYS_TERMINATION_MEAN = as.integer(is.na(application$DAYS_TERMINATION_MEAN))
  application$NA_DAYS_FIRST_DRAWING_MEAN = as.integer(is.na(application$DAYS_FIRST_DRAWING_MEAN))
  application$NA_RATE_INTEREST_MEAN = as.integer(is.na(application$RATE_INTEREST_PRIMARY_MEAN))
  application = impute(as.data.frame(application), 
                       target = "TARGET", 
                       classes = list(numeric = imputeMean(),
                                      integer = imputeMean()))$data
  application = as.data.table(application)
  return(application)
}