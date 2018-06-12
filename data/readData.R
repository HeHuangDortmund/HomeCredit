#' @description Function to the Daten and zusammenfuegen der Datensaetze
#' @return List
#' @author He Huang


readData = function() {
  library(rprojroot)
  library(data.table)
  requireNamespace("lubridate")
  
  root = find_root(is_git_root)
  setwd(root)
  # Einlesen der Datensaetze
  application_train = fread("../data/application_train.csv", na.strings = "")
  application_test = fread("../data/application_test.csv", na.strings = "")
  bureau = fread("../data/bureau.csv", na.strings = "")
  bureau_balance = fread("../data/bureau_balance.csv", na.strings = "")
  credit_card_balance = fread("../data/credit_card_balance.csv", na.strings = "")
  installments_payments = fread("../data/installments_payments.csv", na.strings = "")
  POS_CASH_balance = fread("../data/POS_CASH_balance.csv", na.strings = "")
  previous_application = fread("../data/previous_application.csv", na.strings = "")
  
  return(list(application_train = application_train, 
              application_test = application_test,
              bureau = bureau, 
              bureau_balance = bureau_balance,
              credit_card_balance,
              installments_payments,
              POS_CASH_balance,
              previous_application))
}
