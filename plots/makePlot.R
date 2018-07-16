rm(list=ls())
pkgs <- c("rprojroot","data.table","checkmate","ggplot2","corrplot")
lapply(pkgs,require,character.only = TRUE)
requireNamespace("withr")
root = find_root(is_git_root)
set.seed(1)

################################PLOTS#####################################################
###############APPLICATION MAIN TABLE#####################################################
source(file.path(root,"data","readdata_application.R"))
application = readData()

# exploratory plot
missValue = sapply(application, function(x) sum(is.na(x))) # too many missing value
missValue = missValue[missValue >0]
missValue = sort(missValue)
missVar = names(missValue)

pdf(file.path(root,"plots", "missValueNumeric.pdf"))
barplot(missValue, las = 2, cex.lab=0.2)
dev.off()

# categorical plot
varCategory = names(application)[sapply(application, function(x) is.factor(x))]
train = application[split == "train"]
makeplot_category <- function(var){
  eval(parse(text = file.path("ggplot(data=train, aes(x=",
                              var,
                              "))+geom_bar(stat=\"count\", position = \"fill\", aes(fill = as.factor(TARGET))) + labs(x = \"\", fill = \"TARGET\") + coord_flip()",fsep = "")))
  ggsave(file.path("stacked_percentage_barplot_application_",var,".pdf",fsep= ""))
}
sapply(varCategory,makeplot_category)

############################PREVIOUS APPLICATION##########################################
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
previous_application = fread("../data/previous_application.csv", na.strings = "")
varCategory = names(previous_application)[sapply(previous_application, function(x) is.character(x))]
## Exploratory Plots 
sapply(varCategory, makeplot, "previous_application","barplot")
sapply(setdiff(names(previous_application),varCategory)[-c(1,2)], makeplot, "previous_application", "density")

previous_application = previous_application[,.SD,.SDcols = varCategory,by = list(SK_ID_CURR,SK_ID_PREV)]
# Replace XNA,XAP
previous_application[previous_application == "XNA"] = "miss"
previous_application[previous_application == "XAP"] = "miss"
previous_application[is.na(previous_application)] = "miss"

# categorical 
application_subset = application[,.SD,.SDcols = c("SK_ID_CURR","TARGET","split")]
merged = merge(previous_application, application_subset, all.x = TRUE, by = "SK_ID_CURR")
merged = merged[split == "train"]

makeplot_category <- function(var){
  eval(parse(text = file.path("ggplot(data=merged, aes(x=",
                              var,
                              "))+geom_bar(stat=\"count\", position = \"fill\", aes(fill = as.factor(TARGET))) + labs(x = \"\", fill = \"TARGET\") + coord_flip()",fsep = "")))
  ggsave(file.path("stacked_percentage_barplot_previous_application_",var,".pdf",fsep= ""))
}
sapply(varCategory,makeplot_category)

##########################CREDIT CARD BALANCE#############################################
credit_card_balance = fread("../data/credit_card_balance.csv", na.strings = "")
varCategory = names(credit_card_balance)[sapply(credit_card_balance, function(x) is.character(x))]
varNumeric = setdiff(names(credit_card_balance),varCategory)
# exploratory
sapply(varCategory, makeplot, "credit_card_balance", "barplot")
sapply(varNumeric, makeplot, "credit_card_balance", "density")

# categorical
merged = merge(credit_card_balance, application_subset, all.x = TRUE, by = "SK_ID_CURR")
merged = merged[split == "train"]
ggplot(data=merged, aes(x=NAME_CONTRACT_STATUS))+
  geom_bar(stat="count", position = "fill", aes(fill = as.factor(TARGET))) + 
  labs(x = "", fill = "TARGET") + 
  coord_flip()
ggsave("stacked_percentage_barplot_credit_card_balance_NAME_CONTRACT_STATUS.pdf")

#####################POS_CASH_balance#####################################################
POS_CASH_balance = fread("../data/POS_CASH_balance.csv", na.strings = "")
# categorical variables
merged = merge(POS_CASH_balance, application_subset, all.x = TRUE, by = "SK_ID_CURR")
merged = merged[split == "train"]
ggplot(data=merged, aes(x=NAME_CONTRACT_STATUS))+
  geom_bar(stat="count", position = "fill", aes(fill = as.factor(TARGET))) + 
  labs(x = "", fill = "TARGET") + 
  coord_flip()
ggsave("stacked_percentage_barplot_POS_CASH_balance_NAME_CONTRACT_STATUS.pdf")

##################INSTALLMENT PAYMENT#####################################################
installments_payments = fread("../data/installments_payments.csv", na.strings = "") 
# exploratory
sapply(names(installments_payments), makeplot, "installments_payments", "density")