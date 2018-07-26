readData = function(version = 1,
                    category = "FeatureHashing"){
  library(rprojroot)
  library(data.table)
  root = find_root(is_git_root)
  setwd(root)
  ########################################################################################
  # TODO
  # 1. outlier以及NA的处理: DAYS_FIRST_DRAWING, DAYS_FIRST_DUE, DAYS_LAST_DUE_1ST_VERSION, DAYS_LAST_DUE, DAYS_TERMINATION, NFLAG_INSURED_ON_APPROVAL
  ########################################################################################
  #### Define some functions
  fillNA <- function(var, method = "mean"){ # a generic function to fill NAs with mean or other method
    eval(parse(text = file.path("previous_application$",
                                var,
                                "[is.na(previous_application$",
                                var,
                                ")] = ",
                                method,
                                "(previous_application$",
                                var,
                                ",na.rm = TRUE)",fsep = "")))
    return(previous_application)
  }
  
  mergeCategory <- function(var,temp){ # encode and merge categorical variables
    eval(parse(text = file.path("temp2 = previous_application[,.N,by = list(SK_ID_CURR, ",
                                var,
                                ")]",fsep = "")))
    eval(parse(text = file.path("temp2wide = dcast(temp2, SK_ID_CURR ~ ",
                                var,
                                ", fill = 0, value.var = \"N\")", fsep = "")))
    names(temp2wide) = gsub(" ","_", names(temp2wide))
    names(temp2wide) = c("SK_ID_CURR",paste(var,names(temp2wide),sep = "_")[-1])
    temp = merge(temp, temp2wide, all = TRUE, by = "SK_ID_CURR")
    return(temp)
  }
  
  previous_application = fread("../data/previous_application.csv", na.strings = "")
  
  # Replace XNA,XAP,365243
  previous_application[previous_application == "XNA"] = "miss"
  previous_application[previous_application == "XAP"] = "miss"
  previous_application$DAYS_TERMINATION[previous_application$DAYS_TERMINATION == 365243] = NA
  previous_application$DAYS_LAST_DUE[previous_application$DAYS_LAST_DUE == 365243] = NA
  previous_application$DAYS_LAST_DUE_1ST_VERSION[previous_application$DAYS_LAST_DUE_1ST_VERSION == 365243] = NA
  previous_application$DAYS_FIRST_DUE[previous_application$DAYS_FIRST_DUE == 365243] = NA
  previous_application$DAYS_FIRST_DRAWING[previous_application$DAYS_FIRST_DRAWING == 365243] = NA
  # PRODUCT_COMBINATION (346 missing values filled with "miss")
  previous_application$PRODUCT_COMBINATION[is.na(previous_application$PRODUCT_COMBINATION)] = "miss"
  # NAME_TYPE_SUITE (820405 missing values)
  previous_application$NAME_TYPE_SUITE[is.na(previous_application$NAME_TYPE_SUITE)] = "miss"
  
  numLevels = sapply(previous_application, function(x){length(unique(x))})
  IndexCategory = (numLevels < 30 | names(previous_application) == "SELLERPLACE_AREA") & (names(previous_application) != "RATE_INTEREST_PRIVILEGED")
  ColumnsCat = names(previous_application)[IndexCategory]
  previous_application[, (ColumnsCat) := lapply(.SD, function(x) as.factor(x)), .SDcols = ColumnsCat]
  ColumnsBinary = names(previous_application)[numLevels == 2]
  previous_application[, (ColumnsBinary) := lapply(.SD, function(x) as.integer(x) - 1), .SDcols = ColumnsBinary]
  
  if (version == 2){
    ## first fill NAs
    # 1.AMT_ANNUITY(与AMT_APPLICATION成一定比例, 并基于NAME_CONTRACT_STATUS状态构成一定相异性)
    previous_application$AMT_ANNUITY[is.na(previous_application$AMT_ANNUITY) & previous_application$NAME_CONTRACT_STATUS == "Approved"] = 
      mean(previous_application$AMT_ANNUITY[previous_application$NAME_CONTRACT_STATUS == "Approved" & !is.na(previous_application$AMT_ANNUITY)], na.rm = TRUE) # approved类的求mean
    previous_application$AMT_ANNUITY[is.na(previous_application$AMT_ANNUITY) &
                                       previous_application$NAME_CONTRACT_STATUS == "Canceled" & 
                                       previous_application$AMT_APPLICATION == 0 & 
                                       previous_application$AMT_CREDIT == 0] = 0 # canceled类, 如果AMT_APPLICATION ==0, 则补0
    previous_application$AMT_ANNUITY[is.na(previous_application$AMT_ANNUITY) &
                                       previous_application$NAME_CONTRACT_STATUS == "Canceled" & 
                                       previous_application$AMT_APPLICATION != 0 & 
                                       previous_application$AMT_CREDIT != 0] = mean(previous_application$AMT_ANNUITY[previous_application$NAME_CONTRACT_STATUS == "Canceled" & !is.na(previous_application$AMT_ANNUITY)], na.rm = TRUE) # canceled类, 如果AMT_APPLICATION !=0, 则求mean
    previous_application$AMT_ANNUITY[is.na(previous_application$AMT_ANNUITY) &
                                       previous_application$NAME_CONTRACT_STATUS == "Refused" & 
                                       previous_application$AMT_APPLICATION == 0 | 
                                       previous_application$AMT_CREDIT == 0] = 0 # refused类, 如果AMT_APPLICATION ==0 或者 AMT_CREDIT == 0, 则补0
    previous_application$AMT_ANNUITY[is.na(previous_application$AMT_ANNUITY) &
                                       previous_application$NAME_CONTRACT_STATUS == "Refused" & 
                                       previous_application$AMT_APPLICATION != 0 & 
                                       previous_application$AMT_CREDIT != 0] = mean(previous_application$AMT_ANNUITY[previous_application$NAME_CONTRACT_STATUS == "Refused" & !is.na(previous_application$AMT_ANNUITY)], na.rm = TRUE)
    previous_application$AMT_ANNUITY[is.na(previous_application$AMT_ANNUITY) &
                                       previous_application$NAME_CONTRACT_STATUS == "Unused offer" & 
                                       previous_application$AMT_APPLICATION == 0 & 
                                       previous_application$AMT_CREDIT == 0] = 0
    previous_application$AMT_ANNUITY[is.na(previous_application$AMT_ANNUITY) &
                                       previous_application$NAME_CONTRACT_STATUS == "Unused offer" & 
                                       previous_application$AMT_APPLICATION != 0 & 
                                       previous_application$AMT_CREDIT != 0] = mean(previous_application$AMT_ANNUITY[previous_application$NAME_CONTRACT_STATUS == "Unused offer" & !is.na(previous_application$AMT_ANNUITY)], na.rm = TRUE)
    # 2. CNT_PAYMENT (fill with mean)
    previous_application = fillNA("CNT_PAYMENT")
    
    # 3. AMT_GOODS_PRICE(fill with mean)
    previous_application = fillNA("AMT_GOODS_PRICE")

  }
  # fill NA in AMT_CREDIT (only one NA, fill it with 0, since its corresponding AMT_APPLICATION is 0, AMT_CREDIT与AMT_APPLICATION高度相关)
  previous_application$AMT_CREDIT[is.na(previous_application$AMT_CREDIT)] = 0
  
  # add hand crafted features
  previous_application[, `:=`(Add_DIFF_CREDIT_RECEIVED_ASKED_PREV = AMT_CREDIT - AMT_APPLICATION,
                              Add_DIFF_ASKED_GOODS_PRICE_PREV = AMT_APPLICATION - AMT_GOODS_PRICE,
                              Add_DIFF_RECEIVED_GOODS_PRICE_PREV = AMT_CREDIT - AMT_GOODS_PRICE,
                              Add_RATIO_PAYMENT_PREV = AMT_ANNUITY/AMT_CREDIT,
                              Add_DIFF_DAYS_LAST_DUE_PREV = DAYS_LAST_DUE_1ST_VERSION - DAYS_LAST_DUE
  ), by = list(SK_ID_CURR,SK_ID_PREV)]
  
  ## NUMERIC aggregation
  # 以SK_ID_CURR用mean，min，max汇总numeric
  varMEAN = setdiff(names(previous_application), ColumnsCat)[-c(1,2)]
  temp = previous_application[, c(lapply(.SD, mean, na.rm = TRUE),
                                  lapply(.SD, min, na.rm = TRUE),
                                  lapply(.SD, max, na.rm = TRUE)), .SDcols = varMEAN, by = SK_ID_CURR]
  names(temp)[-1] = paste(names(temp)[-1], rep(c("MEAN","MIN","MAX"), each = length(varMEAN)), sep = "_")
  # 替换Inf/-Inf/NaN
  temp[temp == Inf | temp == -Inf] = NA
  temp[temp == "NaN"] = NA
  # 与每一个SK_ID_CURR对应的PREV ID的数量
  temp_prev = previous_application[,.N, by = SK_ID_CURR]
  names(temp_prev) = c("SK_ID_CURR", "Nr_PREV_APPL_CNT")
  temp = merge(temp_prev, temp, all = TRUE, by = "SK_ID_CURR")

  ## CATEGORY aggregation
  if (category == "redefine"){ # 针对category数量较多(>20), 如NAME_CASH_LOAN_PURPOSE, NAME_GOODS_CATEGORY, SELLERPLACE_AREA
    # 方法1: redefine categories 
    # NAME_CASH_LOAN_PURPOSE and NAME_GOODS_CATEGORY (数量<1000 并入 Other)
    previous_application$NAME_CASH_LOAN_PURPOSE[previous_application$NAME_CASH_LOAN_PURPOSE == "Refusal to name the goal" |
                                                  previous_application$NAME_CASH_LOAN_PURPOSE == "Money for a third person" |
                                                  previous_application$NAME_CASH_LOAN_PURPOSE == "Hobby" |
                                                  previous_application$NAME_CASH_LOAN_PURPOSE == "Buying a garage" |
                                                  previous_application$NAME_CASH_LOAN_PURPOSE == "Gasification / water supply" |
                                                  previous_application$NAME_CASH_LOAN_PURPOSE == "Business development" |
                                                  previous_application$NAME_CASH_LOAN_PURPOSE == "Buying a holiday home / land" |
                                                  previous_application$NAME_CASH_LOAN_PURPOSE == "Furniture" |
                                                  previous_application$NAME_CASH_LOAN_PURPOSE == "Car repairs" |
                                                  previous_application$NAME_CASH_LOAN_PURPOSE == "Buying a home" |
                                                  previous_application$NAME_CASH_LOAN_PURPOSE == "Wedding / gift / holiday"] = "Other"
    previous_application$NAME_GOODS_CATEGORY[previous_application$NAME_GOODS_CATEGORY == "Animals" |
                                               previous_application$NAME_GOODS_CATEGORY == "House Construction" |
                                               previous_application$NAME_GOODS_CATEGORY == "Insurance" |
                                               previous_application$NAME_GOODS_CATEGORY == "Weapon" |
                                               previous_application$NAME_GOODS_CATEGORY == "Education" |
                                               previous_application$NAME_GOODS_CATEGORY == "Additional Service" |
                                               previous_application$NAME_GOODS_CATEGORY == "Fitness" |
                                               previous_application$NAME_GOODS_CATEGORY == "Direct Sales"] = "Other"
    # SELLERPLACE_AREA (level共2097)
    setSELLER = setdiff(unique(previous_application$SELLERPLACE_AREA),c("-1","0","50")) # the three most frequently shown area-code
    previous_application$SELLERPLACE_AREA[previous_application$SELLERPLACE_AREA %in% setSELLER] = "Other"
  
  } else if (category == "FeatureHashing"){# 方法2: hashing function, original values as keys and are mapped to a smaller set of artificial hash values
    NAME_CASH_LOAN_PURPOSE_old = data.frame(unique(previous_application$NAME_CASH_LOAN_PURPOSE))
    hash.obj = hashed.model.matrix(~.-1, 
                                   NAME_CASH_LOAN_PURPOSE_old, 
                                   hash.size = 2^4, 
                                   create.mapping = TRUE)
    mapping = as.vector(unlist(as.list(attr(hash.obj, "mapping"))))
    NAME_CASH_LOAN_PURPOSE_old <- as.vector(unique(previous_application$NAME_CASH_LOAN_PURPOSE))
    previous_application$NAME_CASH_LOAN_PURPOSE = mapvalues(previous_application$NAME_CASH_LOAN_PURPOSE, NAME_CASH_LOAN_PURPOSE_old, mapping)
    
    NAME_GOODS_CATEGORY_old = data.frame(unique(previous_application$NAME_GOODS_CATEGORY))
    hash.obj = hashed.model.matrix(~.-1, 
                                   NAME_GOODS_CATEGORY_old, 
                                   hash.size = 2^4, 
                                   create.mapping = TRUE)
    mapping = as.vector(unlist(as.list(attr(hash.obj, "mapping"))))
    NAME_GOODS_CATEGORY_old <- as.vector(unique(previous_application$NAME_GOODS_CATEGORY))
    previous_application$NAME_GOODS_CATEGORY = mapvalues(previous_application$NAME_GOODS_CATEGORY, NAME_GOODS_CATEGORY_old, mapping)
    
    SELLERPLACE_AREA_old = data.frame(unique(previous_application$SELLERPLACE_AREA))
    hash.obj = hashed.model.matrix(~.-1, 
                                   SELLERPLACE_AREA_old, 
                                   hash.size = 2^6, 
                                   create.mapping = TRUE)
    mapping = as.vector(unlist(as.list(attr(hash.obj, "mapping"))))
    SELLERPLACE_AREA_old <- as.vector(unique(previous_application$SELLERPLACE_AREA))
    previous_application$SELLERPLACE_AREA = mapvalues(previous_application$SELLERPLACE_AREA, SELLERPLACE_AREA_old, mapping)
    
  } else if (category == "dummy"){ # 方法3: create dummy variables for categorical variables, and zero-variance/near zero-variance dummy predictors dropped
    print("This method is not available at the moment")
  }

  # 讲category变量merge进入temp
  for (i in 1:length(ColumnsCat)){
    temp = mergeCategory(ColumnsCat[i],temp)
  }
  
  varDrop = c(names(temp)[grep("RATE_INTEREST_PRIMARY",names(temp))],names(temp)[grep("RATE_INTEREST_PRIVILEGED",names(temp))])
  temp[,c(varDrop) := NULL]
  
  if (version == 2){
    ## 以下variable(分别)大量且同步缺失
    # 1. DAYS_FIRST_DRAWING, DAYS_FIRST_DUE, DAYS_LAST_DUE_1ST_VERSION, DAYS_LAST_DUE, DAYS_TERMINATION
    # 此6个变量与previous application相关, 缺失值多由于prev application撤销或被拒
    temp$NA_DAYS_FIRST_LAST_MEAN = as.integer(is.na(temp$DAYS_FIRST_DRAWING_MEAN))
    temp$DAYS_FIRST_DRAWING_MEAN[is.na(temp$DAYS_FIRST_DRAWING_MEAN)] = mean(temp$DAYS_FIRST_DRAWING_MEAN, na.rm =TRUE)
    temp$DAYS_FIRST_DUE_MEAN[is.na(temp$DAYS_FIRST_DUE_MEAN)] = mean(temp$DAYS_FIRST_DUE_MEAN, na.rm =TRUE)
    temp$DAYS_LAST_DUE_1ST_VERSION_MEAN[is.na(temp$DAYS_LAST_DUE_1ST_VERSION_MEAN)] = mean(temp$DAYS_LAST_DUE_1ST_VERSION_MEAN, na.rm =TRUE)
    temp$DAYS_LAST_DUE_MEAN[is.na(temp$DAYS_LAST_DUE_MEAN)] = mean(temp$DAYS_LAST_DUE_MEAN, na.rm =TRUE)
    temp$DAYS_TERMINATION_MEAN[is.na(temp$DAYS_TERMINATION_MEAN)] = mean(temp$DAYS_TERMINATION_MEAN, na.rm =TRUE)
    
    # 2.AMT_DOWN_PAYMENT, RATE_DOWN_PAYMENT (缺失多为Cash loans 或者 Revolving loans)
    temp$NA_DOWN_PAYMENT_MEAN = as.integer(is.na(temp$AMT_DOWN_PAYMENT_MEAN))
    temp$AMT_DOWN_PAYMENT_MEAN[is.na(temp$AMT_DOWN_PAYMENT_MEAN)] = mean(temp$AMT_DOWN_PAYMENT_MEAN, na.rm = TRUE)
    temp$RATE_DOWN_PAYMENT_MEAN[is.na(temp$RATE_DOWN_PAYMENT_MEAN)] = mean(temp$RATE_DOWN_PAYMENT_MEAN, na.rm = TRUE)
    
    # 3. RATE_INTEREST_PRIMARY, RATE_INTEREST_PRIVILEGED
    temp$NA_RATE_INTEREST_MEAN = as.integer(is.na(temp$RATE_INTEREST_PRIMARY_MEAN))
    temp$RATE_INTEREST_PRIMARY_MEAN[is.na(temp$RATE_INTEREST_PRIMARY_MEAN)] = mean(temp$RATE_INTEREST_PRIMARY_MEAN, na.rm = TRUE)
    temp$RATE_INTEREST_PRIVILEGED_MEAN[is.na(temp$RATE_INTEREST_PRIVILEGED_MEAN)] = mean(temp$RATE_INTEREST_PRIVILEGED_MEAN, na.rm = TRUE)
  }
  names(temp) = make.names(names(temp))
  names(temp)[3] = "AMT_ANNUITY_PREV_MEAN" # 与BUREAU中的AMT_ANNUITY_MEAN重名
  previous_application = temp
  return(previous_application)
}
