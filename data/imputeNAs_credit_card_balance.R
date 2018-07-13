imputeNA <- function(application,
                     naDrop = FALSE){
  if (naDrop == FALSE){
    application$NA_AMT_MEAN = as.integer(is.na(application$AMT_BALANCE_MEAN))
    application$NA_AMT_DRAWINGS_MEAN = as.integer(is.na(application$AMT_DRAWINGS_ATM_CURRENT_MEAN))
    application = impute(as.data.frame(application), 
                         target = "TARGET", 
                         classes = list(numeric = imputeMean(),
                                        integer = imputeMean()))$data
  } else {
    missValue = sapply(application, function(x) sum(is.na(x))) 
    missValue = missValue[missValue > 0]/dim(application)[1]
    varDrop = names(missValue)[missValue > 2/3]
    setDT(application)
    application = application[,c(varDrop) := NULL]
  }  
  application = as.data.table(application)
  return(application)
}