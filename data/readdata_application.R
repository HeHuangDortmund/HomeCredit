readData = function(version = 1){
  library(rprojroot)
  library(data.table)
  root = find_root(is_git_root)
  setwd(root)
  
  ## 0. read data application_train and application_test
  application_train = fread("../data/application_train.csv", na.strings = "")
  application_test = fread("../data/application_test.csv", na.strings = "")
  
  ## 1. merge train and test data 
  application_train[, split := "train"]
  Target = application_train$TARGET
  application_train$TARGET = NULL
  application_train[, TARGET := Target]
  application_test[, split := "test"]
  application_test[, TARGET := NA]
  application = rbind(application_train, application_test)
  
  # 365243 in `DAYS` variable means NA
  application$DAYS_EMPLOYED[application$DAYS_EMPLOYED == 365243] = NA 
  
  ## 2. Category Variablen as Factor or Interger
  numberOfValues = unlist(lapply(application, function(x){length(unique(x))}))
  
  # 0 < numberOfValues < 20 is Category Variablen, as Factor
  IndexOfCategory = (numberOfValues < 20 | names(application) == "ORGANIZATION_TYPE" ) &
    (!(names(application) %in% c("CNT_CHILDREN", 
                                 "CNT_FAM_MEMBERS", 
                                 "DEF_30_CNT_SOCIAL_CIRCLE", 
                                 "DEF_60_CNT_SOCIAL_CIRCLE",
                                 "AMT_REQ_CREDIT_BUREAU_HOUR",
                                 "AMT_REQ_CREDIT_BUREAU_DAY",
                                 "AMT_REQ_CREDIT_BUREAU_WEEK",
                                 "AMT_REQ_CREDIT_BUREAU_QRT")))
  
  columsCategory = names(application)[IndexOfCategory]
  columsCategory = columsCategory[-length(columsCategory)]
  columsCategory = columsCategory[-length(columsCategory)]
  
  application[, (columsCategory) := lapply(.SD, function(x) as.factor(x)),
              .SDcols = columsCategory]
  
  IndexBinar = (numberOfValues == 2)
  columsBinar = names(application)[IndexBinar]
  columsBinar = columsBinar[-length(columsBinar)]
  application[, (columsBinar) := lapply(.SD, function(x) as.integer(x)-1),
              .SDcols = columsBinar] 
  
  return(application)
}