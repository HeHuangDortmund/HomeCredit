
readData = function(){
  library(rprojroot)
  library(data.table)
  requireNamespace("lubridate")
  root = find_root(is_git_root)
  setwd(root)
  
  credit_card_balance = fread("../data/credit_card_balance.csv", na.strings = "")
  return(credit_card_balance)
}

