imputeNA <- function(application){
  application$NA_INSTALLMENT_PAYMENT = as.integer(is.na(application$NUM_INSTALMENT_VERSION_MEAN))
  application = impute(as.data.frame(application), 
                       target = "TARGET", 
                       classes = list(numeric = imputeMean()))$data
  application = as.data.table(application)
  return(application)
}