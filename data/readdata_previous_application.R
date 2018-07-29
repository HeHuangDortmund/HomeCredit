readData = function(category = "FeatureHashing"){
  library(rprojroot)
  library(data.table)
  root = find_root(is_git_root)
  setwd(root)
  ########################################################################################
  # TODO
  # 0. 比较有把握地处理NA: AMT_ANNUITY, AMT_CREDIT, CNT_PAYMENT, AMT_GOODS_PRICE
  # 1. 未处理NA: DAYS_FIRST_DUE, DAYS_LAST_DUE_1ST_VERSION, DAYS_LAST_DUE, DAYS_TERMINATION, DAYS_FIRST_DRAWING(缺失值多由于prev application撤销或被拒)
  # 2. 未处理NA: AMT_DOWN_PAYMENT, RATE_DOWN_PAYMENT (缺失多为Cash loans 或者 Revolving loans)
  # 3. 目前直接去掉 RATE_INTEREST_PRIMARY, RATE_INTEREST_PRIVILEGED
  ########################################################################################
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
  
  ###########填NA#########################################################################
  # AMT_ANNUITY与AMT_APPLICATION(或AMT_CREDIT)构成一定比例, 比如5%,10%,20%, 即每月还款比例
  # 猜测可能与一些categorical变量有关, 比如NAME_CONTRACT_STATUS, NAME_CLIENT_TYPE, NAME_YIELD_GROUP
  # 因此填NA需考虑mean可能不是最优选择, 不过目前先简单地使用mean
  # 以下comment部分是做boxplot图来看AMT_ANNUITY与AMT_CREDIT比例是否在不同group有显著差异
  # temp = previous_application[!is.na(AMT_ANNUITY)]
  # temp[,ratio_annuity_credit := AMT_ANNUITY / AMT_CREDIT]
  # temp = temp[ratio_annuity_credit != "NaN"]
  # makeplot_category <- function(var){
  #   eval(parse(text = file.path("ggplot(data=temp, aes(x=",
  #                               var,
  #                               ",y = ratio_annuity_credit, fill = ",var,"))+geom_boxplot()",fsep = "")))
  #   ggsave(file.path("boxplot_previous_application_",var,".pdf",fsep= ""))
  # }
  # sapply(ColumnsCat,makeplot_category)
  ########################################################################################
  #### 目前对于NA的做法: 如果对应的AMT_CREDIT和AMT_APPLICATION同时为0, 则填0, 否则用mean
  # 对于NA的另一种填补可能: (1)如果对应的AMT_CREDIT和AMT_APPLICATION同时不为0, 则用均值填补
  # (2)如果AMT_CREDIT和AMT_APPLICATION同时为0, 多为canceled/rejected application, 保留NA(包括ANNUITY, 几个DAYS_变量和AMT_DOWN_PAYMENT, RATE_DOWN_PAYMENT本应是NA, 不应用0填补)
  ## 1. fill NA in AMT_ANNUITY
  previous_application$AMT_ANNUITY[is.na(previous_application$AMT_ANNUITY) &
                                   previous_application$AMT_APPLICATION != 0 & 
                                   previous_application$AMT_CREDIT != 0] = mean(previous_application$AMT_ANNUITY, na.rm = TRUE)
  previous_application$AMT_ANNUITY[is.na(previous_application$AMT_ANNUITY) &
                                   previous_application$AMT_APPLICATION == 0 &
                                   previous_application$AMT_CREDIT == 0] = 0
  # one NA left in AMT_ANNUITY, fill it with 0, since AMT_APPLICTION = 0
  previous_application$AMT_ANNUITY[is.na(previous_application$AMT_ANNUITY)] = 0
  ## 2. fill NA in AMT_CREDIT (only one NA, fill it with 0, since its corresponding AMT_APPLICATION is 0, AMT_CREDIT与AMT_APPLICATION高度相关)
  previous_application$AMT_CREDIT[is.na(previous_application$AMT_CREDIT)] = 0
  ## 3. CNT_PAYMENT
  previous_application$CNT_PAYMENT[is.na(previous_application$CNT_PAYMENT) & 
                                   previous_application$AMT_APPLICATION != 0 & 
                                   previous_application$AMT_CREDIT != 0] = mean(previous_application$CNT_PAYMENT, na.rm = TRUE)
  previous_application$CNT_PAYMENT[is.na(previous_application$CNT_PAYMENT) &
                                   previous_application$AMT_APPLICATION == 0 &
                                   previous_application$AMT_CREDIT == 0] = 0
  ## 4. AMT_GOODS_PRICE
  previous_application$AMT_GOODS_PRICE[is.na(previous_application$AMT_GOODS_PRICE) & 
                                     previous_application$AMT_APPLICATION != 0 & 
                                     previous_application$AMT_CREDIT != 0] = mean(previous_application$AMT_GOODS_PRICE, na.rm = TRUE)
  previous_application$AMT_GOODS_PRICE[is.na(previous_application$AMT_GOODS_PRICE) &
                                     previous_application$AMT_APPLICATION == 0] = 0
  ## 5. drop RATE_INTEREST_PRIMARY, RATE_INTEREST_PRIVILEGED
  varDrop = c("RATE_INTEREST_PRIMARY", "RATE_INTEREST_PRIVILEGED")
  previous_application[,c(varDrop) := NULL]
  
  ###########加新feature##################################################################
  # add hand crafted features
  previous_application[, `:=`(Add_DIFF_CREDIT_RECEIVED_ASKED_PREV = AMT_CREDIT - AMT_APPLICATION,
                              Add_DIFF_ASKED_GOODS_PRICE_PREV = AMT_APPLICATION - AMT_GOODS_PRICE,
                              Add_DIFF_RECEIVED_GOODS_PRICE_PREV = AMT_CREDIT - AMT_GOODS_PRICE,
                              Add_RATIO_PAYMENT_PREV = AMT_ANNUITY/AMT_CREDIT, # 此处产生NaN(由于0/0, canceled or rejected application),但在汇总前未做任何处理
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
  # 注意对于Add_RATIO_PAYMENT_PREV(MEAN,MIN,MAX)汇总后,出现NaN,Inf/-Inf并非由于组内全是NA, 而是组内全是由于0/0 division造成NaN
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
  # Define a function
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
  for (i in 1:length(ColumnsCat)){
    temp = mergeCategory(ColumnsCat[i],temp)
  }

  # renames
  names(temp) = make.names(names(temp))
  names(temp)[3] = "AMT_ANNUITY_PREV_MEAN" # 与BUREAU中的AMT_ANNUITY_MEAN重名
  
  previous_application = temp
  rm(temp)
  gc()
  return(previous_application)
}