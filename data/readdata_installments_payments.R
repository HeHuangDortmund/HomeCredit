
readData = function(){
  library(rprojroot)
  library(data.table)
  requireNamespace("lubridate")
  root = find_root(is_git_root)
  setwd(root)
  
  installments_payments = fread("../data/installments_payments.csv", na.strings = "") 
  
  return(installments_payments)
}


