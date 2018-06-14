
readData = function(){
  library(rprojroot)
  library(data.table)
  requireNamespace("lubridate")
  root = find_root(is_git_root)
  setwd(root)
  
  bureau_balance = fread("../data/bureau_balance.csv", na.strings = "")
  tmp = bureau_balance[,.N, by = list(SK_ID_BUREAU,STATUS)]
  wideData = dcast(tmp, SK_ID_BUREAU ~ STATUS, fill = 0)
  names(wideData) = c("SK_ID_BUREAU", 
                      "BUREAU_STATUS_0", 
                      "BUREAU_STATUS_1", 
                      "BUREAU_STATUS_2", 
                      "BUREAU_STATUS_3",
                      "BUREAU_STATUS_4",
                      "BUREAU_STATUS_5",
                      "BUREAU_STATUS_C",
                      "BUREAU_STATUS_X")

  return(wideData)
}