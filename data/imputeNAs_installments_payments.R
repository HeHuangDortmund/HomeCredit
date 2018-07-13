imputeNA <- function(application){
  application$NA_INSTALLMENT_PAYMENT = as.integer(is.na(application$AMT_INSTALMENT_MEAN))
  application = impute(as.data.frame(application), 
                       target = "TARGET", 
                       classes = list(numeric = imputeMean()))$data
  application = as.data.table(application)
  return(application)
}