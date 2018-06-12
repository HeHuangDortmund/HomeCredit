# Add feature: SUM_OVERDUE according to the dataset bureau
# He

preproc = function(data) {
  
  ibrary(rprojroot)
  library(data.table)
  requireNamespace("lubridate")
  root = find_root(is_git_root)
  setwd(root)
  
  bureau = fread("../data/bureau.csv", na.strings = "")
  tmp = bureau[,.(SUM_OVERDUE = sum(AMT_CREDIT_SUM_OVERDUE)), by = SK_ID_CURR]
  
  data = merge(data, tmp, all.x = TRUE, by = "SK_ID_CURR")
  return(data)
}


