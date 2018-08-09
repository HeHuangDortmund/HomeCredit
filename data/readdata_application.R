readData = function(){
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
  rm(application_train, application_test)
  
  # 365243 in `DAYS` variable means NA
  application$DAYS_EMPLOYED[application$DAYS_EMPLOYED == 365243] = NA 
  application$DAYS_EMPLOYED = as.numeric(application$DAYS_EMPLOYED)
  
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
  
  # add new features
  application[,`:=`(EXT_SOURCE_MEAN = apply(.SD,1,mean,na.rm=TRUE)),.SDcols = c("EXT_SOURCE_1","EXT_SOURCE_2","EXT_SOURCE_3")]
  application[,`:=`(EXT_SOURCE_MIN = pmin(EXT_SOURCE_1,EXT_SOURCE_2,EXT_SOURCE_3, na.rm = TRUE),
                    EXT_SOURCE_MAX = pmax(EXT_SOURCE_1,EXT_SOURCE_2,EXT_SOURCE_3, na.rm = TRUE),
                    Add_RATIO_ANNUITY_INCOME = AMT_ANNUITY / AMT_INCOME_TOTAL,
                    Add_RATIO_CREDIT_INCOME = AMT_CREDIT / AMT_INCOME_TOTAL,
                    Add_RATIO_CREDIT_GOODS = AMT_CREDIT / AMT_GOODS_PRICE, 
                    Add_RATIO_PAYMENT = AMT_ANNUITY / AMT_CREDIT, 
                    Add_RATIO_EMPLOYED_BIRTH = DAYS_EMPLOYED / DAYS_BIRTH,
                    Add_RATIO_CAR_BIRTH = OWN_CAR_AGE / DAYS_BIRTH,
                    Add_RATIO_CAR_EMPLOYED = OWN_CAR_AGE / DAYS_EMPLOYED, 
                    Add_RATIO_PHONE_BIRTH = DAYS_LAST_PHONE_CHANGE / DAYS_BIRTH,
                    Add_RATIO_PHONE_EMPLOYED = DAYS_LAST_PHONE_CHANGE / DAYS_EMPLOYED,
                    Add_RATIO_CHILD = CNT_CHILDREN / CNT_FAM_MEMBERS,
                    Add_INCOME_per_FAM = AMT_INCOME_TOTAL / CNT_FAM_MEMBERS,
                    Add_INCOME_per_ADULT = AMT_INCOME_TOTAL / (CNT_FAM_MEMBERS - CNT_CHILDREN),
                    Add_CREDIT_per_FAM = AMT_CREDIT / CNT_FAM_MEMBERS,
                    Add_CREDIT_per_ADULT = AMT_CREDIT / (CNT_FAM_MEMBERS - CNT_CHILDREN))]
  application$Add_RATIO_PHONE_EMPLOYED[is.infinite(application$Add_RATIO_PHONE_EMPLOYED)] = NA # DAYS_EMPLOYED = 0 (self-employed) generates -Inf
  return(application)
}