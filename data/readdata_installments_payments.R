readData = function(){
  library(rprojroot)
  library(data.table)
  root = find_root(is_git_root)
  setwd(root)
  ########################################################################################
  # Problem: 1. NUM_INSTALMENT_NUMBER是否需要转成categorical? NUM_INSTALMENT_VERSION已转
  # 2. NAs in DAYS_ENTRY_PAYMENT, AMT_PAYMENT
  ########################################################################################
  # read data
  installments_payments = fread("../data/installments_payments.csv", na.strings = "") 
  
  # add two features
  installments_payments[, INSTALMENTS_DPD := DAYS_ENTRY_PAYMENT - DAYS_INSTALMENT]# 逾期天数 >0 代表逾期
  installments_payments[, INSTALMENTS_LESS := AMT_PAYMENT - AMT_INSTALMENT]        # 少还金额 <0 代表少还了
  
  ############################## calculate the trend and intercept variables ###################################################################
  # installments_payments = installments_payments[order(SK_ID_CURR, SK_ID_PREV, NUM_INSTALMENT_NUMBER)] # take order and then analysis the trend
  # temp1 = installments_payments[!is.na(DAYS_ENTRY_PAYMENT)]
  # temp_intercept = temp1[, lapply(.SD, function(x) {lm(x~(seq(1,length(NUM_INSTALMENT_NUMBER),by=1)))$coefficients[1]}),
  #                                .SDcols = c("INSTALMENTS_DPD","INSTALMENTS_LESS"),
  #                                by = SK_ID_PREV] # this takes around 30mins, so save and load txt
  # temp_trend = temp1[, lapply(.SD, function(x) {lm(x~(seq(1,length(NUM_INSTALMENT_NUMBER),by=1)))$coefficients[2]}),
  #              .SDcols = c("INSTALMENTS_DPD","INSTALMENTS_LESS"),
  #              by = SK_ID_PREV] # this takes around 30mins, so save and load txt
  # names(temp_intercept)[-1] = c("INSTALMENTS_DPD_INTERCEPT","INSTALMENTS_LESS_INTERCEPT")
  # names(temp_trend)[-1] = c("INSTALMENTS_DPD_TREND","INSTALMENTS_LESS_TREND")
  # temp_trend = merge(temp_intercept,temp_trend, all = TRUE, by = "SK_ID_PREV")
  ##############################################################################################################################################
  temp_trend = fread("trend_installments.txt", drop = "V1")
  
  # 对变量进行汇总(mean和max)
  temp_name = setdiff(names(installments_payments), c("SK_ID_PREV","SK_ID_CURR","NUM_INSTALMENT_VERSION"))
  temp = installments_payments[, c(lapply(.SD, mean, na.rm = TRUE),
  #                                  lapply(.SD, max, na.rm = TRUE)), .SDcols = temp_name, by = "SK_ID_CURR"]
  # names(temp)[-1] = paste(names(temp)[-1], rep(c("MEAN","MAX"),each = length(temp_name)), sep = "_")
                                   lapply(.SD, max, na.rm = TRUE)), .SDcols = temp_name, by = list(SK_ID_CURR,SK_ID_PREV)]
  names(temp)[-c(1,2)] = paste(names(temp)[-c(1,2)], rep(c("MEAN","MAX"),each = length(temp_name)), sep = "_")
  
  temp = merge(temp, temp_trend, all.x = TRUE, by = c("SK_ID_PREV"))
  temp_agg_max = temp[, lapply(.SD, max, na.rm = TRUE), .SDcols = names(temp)[grep("MAX",names(temp))], by = SK_ID_CURR]
  temp_agg_mean = temp[, lapply(.SD, mean, na.rm = TRUE), .SDcols = setdiff(names(temp),c(names(temp)[grep("MAX",names(temp))],"SK_ID_CURR","SK_ID_PREV")), by = SK_ID_CURR]
  temp_agg = merge(temp_agg_max, temp_agg_mean, all = TRUE, by = c("SK_ID_CURR"))
  
  # 对于每一个SK_ID_PREV求INSTALLMENTS的数量, 然后求MEAN进行汇总
  temp1 = installments_payments[,.(Nr_INSTALLMENTS = .N),by = list(SK_ID_CURR,SK_ID_PREV)]
  temp1 = temp1[,.(Nr_INSTALLMENTS_MEAN = mean(Nr_INSTALLMENTS, na.rm = TRUE)), by = "SK_ID_CURR"] 
  
  # 对于每一个SK_ID_CURR求相对应的SK_ID_PREV的数量
  temp2 = installments_payments[,.(Nr_INSTALLMENTS_PREV = length(unique(SK_ID_PREV))), by = "SK_ID_CURR"]
  
  # 处理NUM_INSTALMENT_VERSION(65 level), 将其转为categorical variable, 后用hash trick降维
  installments_payments$NUM_INSTALMENT_VERSION = as.character(installments_payments$NUM_INSTALMENT_VERSION)
  NUM_INSTALMENT_VERSION_old = data.frame(unique(installments_payments$NUM_INSTALMENT_VERSION))
  hash.obj = hashed.model.matrix(~.-1, 
                                 NUM_INSTALMENT_VERSION_old, 
                                 hash.size = 2^4, 
                                 create.mapping = TRUE)
  mapping = as.vector(unlist(as.list(attr(hash.obj, "mapping"))))
  NUM_INSTALMENT_VERSION_old <- as.vector(unique(installments_payments$NUM_INSTALMENT_VERSION))
  installments_payments$NUM_INSTALMENT_VERSION = mapvalues(installments_payments$NUM_INSTALMENT_VERSION, NUM_INSTALMENT_VERSION_old, mapping)
  
  temp_cat = installments_payments[,.N, by = list(SK_ID_CURR,NUM_INSTALMENT_VERSION)]
  temp_cat_wide = dcast(temp_cat, SK_ID_CURR ~ NUM_INSTALMENT_VERSION, fill = 0, value.var = "N")
  names(temp_cat_wide)[-1] = paste("NUM_INSTALMENT_VERSION", names(temp_cat_wide)[-1], sep = "_")
  
  # merge
  temp_all = merge(temp1, temp2, all = TRUE, by = "SK_ID_CURR")
  temp_all = merge(temp_all, temp_agg, all = TRUE, by = "SK_ID_CURR")
  temp_all = merge(temp_all, temp_cat_wide, all = TRUE, by = "SK_ID_CURR")
  
  temp_all$DAYS_ENTRY_PAYMENT_MEAN[is.nan(temp_all$DAYS_ENTRY_PAYMENT_MEAN)] = NA
  temp_all$AMT_PAYMENT_MEAN[is.nan(temp_all$AMT_PAYMENT_MEAN)] = NA
  temp_all$INSTALMENTS_DPD_MEAN[is.nan(temp_all$INSTALMENTS_DPD_MEAN)] = NA
  temp_all$INSTALMENTS_LESS_MEAN[is.nan(temp_all$INSTALMENTS_LESS_MEAN)] = NA
  
  temp_all$DAYS_ENTRY_PAYMENT_MAX[is.infinite(temp_all$DAYS_ENTRY_PAYMENT_MAX)] = NA
  temp_all$AMT_PAYMENT_MAX[is.infinite(temp_all$AMT_PAYMENT_MAX)] = NA
  temp_all$INSTALMENTS_DPD_MAX[is.infinite(temp_all$INSTALMENTS_DPD_MAX)] = NA
  temp_all$INSTALMENTS_LESS_MAX[is.infinite(temp_all$INSTALMENTS_LESS_MAX)] = NA
  
  installments_payments = temp_all
  gc()
  return(installments_payments)
}