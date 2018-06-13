
readData = function(){
  library(rprojroot)
  library(data.table)
  requireNamespace("lubridate")
  root = find_root(is_git_root)
  setwd(root)
  
  bureau_balance = fread("../data/bureau_balance.csv", na.strings = "")
  tmp = bureau_balance[,.N, by = list(SK_ID_BUREAU,STATUS)]
  wideData = dcast(tmp, SK_ID_BUREAU ~ STATUS)

  return(wideData)
}