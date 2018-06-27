imputeNA <- function(application){
  application$NA_AMT_DRAWINGS_CURRENT_MEAN = as.integer(is.na(application$AMT_DRAWINGS_CURRENT_MEAN))
  application$NA_AMT_DRAWINGS_ATM_CURRENT_MEAN = as.integer(is.na(application$AMT_DRAWINGS_ATM_CURRENT_MEAN))
  application = impute(as.data.frame(application), 
                       target = "TARGET", 
                       classes = list(numeric = imputeMean(),
                                      integer = imputeMean()))$data
  application = as.data.table(application)
  return(application)
}