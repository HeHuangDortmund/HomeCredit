imputeNA <- function(application,
                     naDrop = FALSE){
  if (naDrop == FALSE){
    application$NA_PREV_APPLICATION  = as.integer(is.na(application$Nr_PREV_APPL_CNT))
    application$NA_AMT_DOWN_PAYMENT_MEAN = as.integer(is.na(application$AMT_DOWN_PAYMENT_MEAN))
    application$NA_DAYS_TERMINATION_MEAN = as.integer(is.na(application$DAYS_TERMINATION_MEAN))
    application$NA_DAYS_FIRST_DRAWING_MEAN = as.integer(is.na(application$DAYS_FIRST_DRAWING_MEAN))
    # application$NA_RATE_INTEREST_MEAN = as.integer(is.na(application$RATE_INTEREST_PRIMARY_MEAN))
    application = impute(as.data.frame(application), 
                         target = "TARGET", 
                         classes = list(numeric = imputeMean(),
                                        integer = imputeMean()))$data
  } else {
    missValue = sapply(application, function(x) sum(is.na(x))) 
    missValue = missValue[missValue > 0]/dim(application)[1]
    # barplot(missValue,las = 2,cex.lab=0.2)
    varDrop = names(missValue)[missValue > 2/3]
    setDT(application)
    application = application[,c(varDrop) := NULL]
    
    application$NA_PREV_APPLICATION  = as.integer(is.na(application$Nr_PREV_APPL_CNT))
    application$NA_AMT_DOWN_PAYMENT_MEAN = as.integer(is.na(application$AMT_DOWN_PAYMENT_MEAN))
    application$NA_DAYS_TERMINATION_MEAN = as.integer(is.na(application$DAYS_TERMINATION_MEAN))
    application = impute(as.data.frame(application), 
                         target = "TARGET", 
                         classes = list(numeric = imputeMean(),
                                        integer = imputeMean()))$data
  }
  application = as.data.table(application)
  return(application)
}