readData = function(exploratory = 0){
  library(rprojroot)
  library(data.table)
  root = find_root(is_git_root)
  setwd(root)
  ########################################################################################
  # THINGS TO DO:
  # 1. outlier以及NA的处理: DAYS_FIRST_DRAWING, DAYS_FIRST_DUE, DAYS_LAST_DUE_1ST_VERSION, DAYS_LAST_DUE, DAYS_TERMINATION, NFLAG_INSURED_ON_APPROVAL
  # 2. 变量 SELLERPLACE_AREA 的处理 
  ########################################################################################
  #### Define some functions
  makeplot <- function(var, type){
    pdf(file.path("previous_application_",var,".pdf",fsep = ""), 
        width = 14, 
        height = 12)
    par(cex.lab=1.5,cex.axis=1.5,mar=c(8,5,2,2) + 0.1,lwd=2)
    if (type == "barplot"){
      eval(parse(text = file.path("barplot(previous_application[,.N,by =", 
                                  var,
                                  "]$N,names.arg  = previous_application[,.N,by =",
                                  var,
                                  "]$",
                                  var,
                                  ",las = 2)",
                                  fsep = "")))
    } else if (type == "density") {
      eval(parse(text = file.path("plot(density(na.omit(previous_application$", 
                                  var,
                                  ")))",
                                  fsep = "")))
    }
    dev.off()
  }
  
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
                                ", fill = 0)", fsep = "")))
    name_temp = c()
    for (i in 2:length(names(temp2wide))){
      tmp = gsub(" ","_", names(temp2wide)[i])
      name_temp[i-1] = file.path(var,"_",tmp, fsep = "")
    }
    names(temp2wide) = c("SK_ID_CURR",name_temp)
    temp = merge(temp, temp2wide, all = TRUE, by = "SK_ID_CURR")
    return(temp)
  }
  
  previous_application = fread("../data/previous_application.csv", na.strings = "")
  previous_application$FLAG_LAST_APPL_PER_CONTRACT = as.integer(as.factor(previous_application$FLAG_LAST_APPL_PER_CONTRACT))-1
  # barplot(sort(sapply(previous_application, function(x) sum(is.na(x)))),las = 2)
  varCategory = names(previous_application)[sapply(previous_application, function(x) is.character(x))]
  varNumeric = setdiff(names(previous_application),varCategory)
  if (exploratory == 1){
    ## Exploratory Plots 
    sapply(varCategory, makeplot, "barplot")
    sapply(varNumeric, makeplot, "density")
  }
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
  
  # 3. AMT_CREDIT (only one NA, fill it with 0, since its corresponding AMT_APPLICATION is 0, AMT_CREDIT与AMT_APPLICATION高度相关)
  previous_application$AMT_CREDIT[is.na(previous_application$AMT_CREDIT)] = 0
  
  # 4. AMT_GOODS_PRICE(fill with mean)
  previous_application = fillNA("AMT_GOODS_PRICE")
  
  ## NUMERIC aggregation
  temp = previous_application[, .(CNT_PREV = .N,
                                  AMT_ANNUITY_MEAN = mean(AMT_ANNUITY, na.rm = TRUE),
                                  AMT_APPLICATION_MEAN = mean(AMT_APPLICATION, na.rm = TRUE),
                                  AMT_CREDIT_MEAN = mean(AMT_CREDIT, na.rm = TRUE),
                                  AMT_DOWN_PAYMENT_MEAN = mean(AMT_DOWN_PAYMENT, na.rm = TRUE),
                                  AMT_GOODS_PRICE_MEAN = mean(AMT_GOODS_PRICE,na.rm = TRUE),
                                  HOUR_APPR_PROCESS_START_MEAN = mean(HOUR_APPR_PROCESS_START, na.rm = TRUE),
                                  FLAG_LAST_APPL_PER_CONTRACT_MIN = min(FLAG_LAST_APPL_PER_CONTRACT, na.rm = TRUE),
                                  NFLAG_LAST_APPL_IN_DAY_MIN = min(NFLAG_LAST_APPL_IN_DAY,na.rm = TRUE),
                                  RATE_DOWN_PAYMENT_MEAN = mean(RATE_DOWN_PAYMENT, na.rm = TRUE),
                                  RATE_INTEREST_PRIMARY_MEAN = mean(RATE_INTEREST_PRIMARY, na.rm = TRUE),
                                  RATE_INTEREST_PRIVILEGED_MEAN = mean(RATE_INTEREST_PRIVILEGED, na.rm = TRUE),
                                  DAYS_DECISION_MEAN= mean(DAYS_DECISION, na.rm = TRUE),
                                  CNT_PAYMENT = mean(CNT_PAYMENT, na.rm = TRUE),
                                  DAYS_FIRST_DRAWING_MEAN = mean(DAYS_FIRST_DRAWING, na.rm = TRUE),
                                  DAYS_FIRST_DUE_MEAN = mean(DAYS_FIRST_DUE, na.rm = TRUE),
                                  DAYS_LAST_DUE_1ST_VERSION_MEAN = mean(DAYS_LAST_DUE_1ST_VERSION, na.rm = TRUE),
                                  DAYS_LAST_DUE_MEAN = mean(DAYS_LAST_DUE, na.rm = TRUE),
                                  DAYS_TERMINATION_MEAN = mean(DAYS_TERMINATION, na.rm = TRUE)), by = SK_ID_CURR]  
  ## CATEGORY aggregation
  # fill NAs in PRODUCT_COMBINATION (346 missing values filled with "miss")
  previous_application$PRODUCT_COMBINATION[is.na(previous_application$PRODUCT_COMBINATION)] = "miss"
  # fill NAs in NAME_TYPE_SUITE
  previous_application$NAME_TYPE_SUITE[is.na(previous_application$NAME_TYPE_SUITE)] = "miss"
  # re-categorize NAME_CASH_LOAN_PURPOSE and NAME_GOODS_CATEGORY
  previous_application$NAME_CASH_LOAN_PURPOSE[previous_application$NAME_CASH_LOAN_PURPOSE != "XAP" & previous_application$NAME_CASH_LOAN_PURPOSE != "XNA"] = "Other"
  previous_application$NAME_GOODS_CATEGORY[previous_application$NAME_GOODS_CATEGORY != "XNA" &
                                             previous_application$NAME_GOODS_CATEGORY != "Mobile" &
                                             previous_application$NAME_GOODS_CATEGORY != "Consumer Electronics" &
                                             previous_application$NAME_GOODS_CATEGORY != "Audio/Video" & 
                                             previous_application$NAME_GOODS_CATEGORY != "Furniture" &
                                             previous_application$NAME_GOODS_CATEGORY != "Computers"] = "Other"
  for (i in 1:length(varCategory)){
    temp = mergeCategory(varCategory[i],temp)
  }
  temp = mergeCategory("NFLAG_INSURED_ON_APPROVAL",temp)
  
  setSELLER = setdiff(unique(previous_application$SELLERPLACE_AREA),c(-1,0,50)) # the three most frequently shown area-code
  previous_application$SELLERPLACE_AREA[previous_application$SELLERPLACE_AREA %in% setSELLER] = "other"
  tempSELLER = previous_application[,.N,by = list(SK_ID_CURR, SELLERPLACE_AREA)]
  tempSELLERwide = dcast(tempSELLER, SK_ID_CURR ~ SELLERPLACE_AREA, fill = 0)
  names(tempSELLERwide) = c("SK_ID_CURR","SELLERPLACE_AREA_minus1", "SELLERPLACE_AREA_0","SELLERPLACE_AREA_50", "SELLERPLACE_AREA_other")
  temp = merge(temp, tempSELLERwide, all = TRUE, by = "SK_ID_CURR")
  rm(tempSELLER,tempSELLERwide)
  
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
  
  previous_application = temp
  return(previous_application)
}