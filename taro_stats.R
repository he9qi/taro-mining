# taro_stats.R
InstallCandidates <- c('plyr', 'lubridate')
toInstall <- InstallCandidates[!InstallCandidates %in% library()$results[,1]]
if(length(toInstall)!=0)
{install.packages(toInstall, repos='http://cran.r-project.org')}
lapply(InstallCandidates, library, character.only = TRUE)


### schema:
#  product  quantity amount last_purchase_at   last_purchase_amount
#  CA 4113      116  19295       2013-12-31              68
Taro.Stats.productly <- function(data, start=NULL, end=NULL){
  result <- Taro.Helper.basic_group_sales(data, 'product', 'quantity', start, end)
  return(result)
}


### schema
# compute the frequency count
# daysBetween quantity
#       1      706
Taro.Stats.purchase_frequency_count <- function(f){
  purchaseFreq <- ddply(f, .(cust), summarize,  daysBetween = as.numeric(diff(date)))
  result <- Taro.Helper.bin_count(purchaseFreq, "daysBetween")
  
  return(result)
}


# schema:
# amount   quantity customer_count amount_per_customer quantity_per_customer trans_new_customer trans_new_cust_interval customer_buy_count_2
# 1125048    10844     3941            285.4727              2.751586           2.672246                      52                 2453
# total quantity and amount
Taro.Stats.total <- function(f) {
  amount   = sum(f$sales)
  quantity = length(f$sales)
  customer_count = length(unique(f$cust))
  amount_per_customer = amount / customer_count
  quantity_per_customer = quantity / customer_count
  
  result <- data.frame(amount=amount, quantity=quantity, 
             customer_count=customer_count,
             amount_per_customer=amount_per_customer,
             quantity_per_customer=quantity_per_customer)
  
  return(result)
}
