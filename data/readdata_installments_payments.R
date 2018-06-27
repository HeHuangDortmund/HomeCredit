readData = function(exploratory = 0){
  library(rprojroot)
  library(data.table)
  root = find_root(is_git_root)
  setwd(root)
  ########################################################################################
  # Problem: 1. NUM_INSTALMENT_VERSION的处理
  # 2. NAs in DAYS_ENTRY_PAYMENT, AMT_PAYMENT
  ########################################################################################
  makeplot <- function(var, data, type){
    pdf(file.path(data,"_",var,".pdf",fsep = ""), 
        width = 14, 
        height = 12)
    par(cex.lab=1.5,cex.axis=1.5,mar=c(8,5,2,2) + 0.1,lwd=2)
    if (type == "barplot"){
      eval(parse(text = file.path("barplot(",data,"[,.N,by =", 
                                  var,
                                  "]$N,names.arg  = ",data,"[,.N,by =",
                                  var,
                                  "]$",
                                  var,
                                  ",las = 2)",
                                  fsep = "")))
    } else if (type == "density") {
      eval(parse(text = file.path("plot(density(na.omit(",data,"$", 
                                  var,
                                  ")))",
                                  fsep = "")))
    }
    dev.off()
  }
  
  installments_payments = fread("../data/installments_payments.csv", na.strings = "") 
  sort(sapply(installments_payments,function(x) sum(is.na(x))))
  
  installments_payments[, INSTALMENTS_DPD := DAYS_ENTRY_PAYMENT - DAYS_INSTALMENT] # 逾期天数 >0 代表逾期
  installments_payments[, INSTALMENTS_LESS := AMT_PAYMENT - AMT_INSTALMENT]        # 少还金额 <0 代表少还了
  
  temp  = installments_payments[,.(NUM_INSTALMENT_VERSION_MEAN = mean(NUM_INSTALMENT_VERSION),
                                   NUM_INSTALMENT_NUMBER_MEAN = mean(NUM_INSTALMENT_NUMBER),
                                   DAYS_INSTALMENT_MEAN = mean(DAYS_INSTALMENT),
                                   DAYS_ENTRY_PAYMENT_MEAN = mean(DAYS_ENTRY_PAYMENT, na.rm = TRUE),
                                   AMT_INSTALMENT_MEAN = mean(AMT_INSTALMENT),
                                   AMT_PAYMENT_MEAN = mean(AMT_PAYMENT, na.rm = TRUE),
                                   INSTALMENTS_DPD_MEAN = mean(INSTALMENTS_DPD, na.rm = TRUE),
                                   INSTALMENTS_LESS_MEAN = mean(INSTALMENTS_LESS, na.rm=TRUE)), by = SK_ID_CURR]
  temp$DAYS_ENTRY_PAYMENT_MEAN[is.nan(temp$DAYS_ENTRY_PAYMENT_MEAN)] = NA
  temp$AMT_PAYMENT_MEAN[is.nan(temp$AMT_PAYMENT_MEAN)] = NA
  temp$INSTALMENTS_DPD_MEAN[is.nan(temp$INSTALMENTS_DPD_MEAN)] = NA
  temp$INSTALMENTS_LESS_MEAN[is.nan(temp$INSTALMENTS_LESS_MEAN)] = NA
  
  if (exploratory == 1){
    sapply(names(installments_payments), makeplot, "installments_payments", "density")
  }
  
  installments_payments = temp
  return(installments_payments)
}


