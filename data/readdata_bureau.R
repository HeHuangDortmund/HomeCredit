
readData = function(){
  library(rprojroot)
  library(data.table)
  requireNamespace("lubridate")
  root = find_root(is_git_root)
  setwd(root)
  
  bureau = fread("../data/bureau.csv", na.strings = "")
  return(bureau)
}