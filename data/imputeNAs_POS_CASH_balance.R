imputeNA <- function(application){
  application$NA_POS_CASH = as.integer(is.na(application$MONTHS_BALANCE_MEAN_POS))
  application = impute(as.data.frame(application), 
                       target = "TARGET", 
                       classes = list(numeric = imputeMean(),
                                      integer = imputeMean()))$data
  application = as.data.table(application)
  return(application)
}