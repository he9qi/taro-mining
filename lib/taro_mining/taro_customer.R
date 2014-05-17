# taro_customer.R

### schema:
# cust   quantity amount last_purchase_at    last_purchase_amount
# jack       82   9905       2013-12-19                  122
Taro.Customer.mergeTransactions <- function(data, start=NULL, end=NULL) {
  result <- Taro.Helper.basic_group_sales(data, 'cust', 'quantity', start, end)
  return(result)
}


# compute user purchase freq(avg)
# cust        avg_purchase_freq
# ALLAN          28.20000
Taro.Customer.avg_purchase_freq <- function(f){
  purchaseFreq <- ddply(f, .(cust), summarize,  daysBetween = as.numeric(diff(date)))
  result <- ddply(purchaseFreq, 'cust', function(x) c(avg_purchase_freq=mean(x$daysBetween)))
  return(result)
}

# compute user value and est value
Taro.Customer.computeValues <- function(f, sales.users){
  amount   = sum(f$sales)
  quantity = sum(f$count)
  customer_count = length(unique(f$cust))
  amount_per_customer = amount / customer_count
  quantity_per_customer = quantity / customer_count
  
  computeValues <- function(user, base_value=0.0001) {
    value      <- exp(-(amount_per_customer / (user$ltv+base_value)))
    est_value  <- exp(-(quantity_per_customer / (user$trans52+base_value)))
    
    user$value     <- round(as.numeric(value), 8)
    user$est_value <- round(as.numeric(est_value), 8)
    user
  }
  
  result <- ddply(sales.users, 'cust', computeValues )
  return(result)
}


# Merge the user average purchase frequency talbe *sales.user_purchase_freq* 
# into *sales.users*
# cust        avg_purchase_freq
#  1           28.2
Taro.Customer.mergeAvgPurchaseFreq <- function(xf, sales.users){
  sales.user_purchase_freq = Taro.Customer.avg_purchase_freq(xf)
  
  appendAvgPurchaseFreq <- function(user) {
    apf  <- sales.user_purchase_freq[sales.user_purchase_freq$cust == as.character(user$cust),]$avg_purchase_freq
    apf  <- as.numeric(paste(apf, collapse=''))
    user$avg_purchase_freq <- apf
    user
  }
  
  result <- ddply(sales.users, 'cust', appendAvgPurchaseFreq )
  return(result)
}


# Merge user transactions by user and month, each user row will have
# an array or string separated by "|" of quantity and/or amount
# cust  monthly_transactions
#  1     2014-01-01 1 3.4
Taro.Customer.mergeTransactionsByMonth <- function(sales.userMonthly, sales.users){
  appendMontlyTransactions <- function(user) {
    monthlyTransactions <- sales.userMonthly[sales.userMonthly$cust == as.character(user$cust),]
    date     <- monthlyTransactions$date
    quantity <- monthlyTransactions$quantity
    amount   <- monthlyTransactions$amount
    
    user$monthly_transactions <- paste(date, quantity, amount, collapse="|")
    user
  }
  
  result <- ddply(sales.users, 'cust', appendMontlyTransactions)
}


# Merge user recommendations, each user row will have
# an array or string separated by "|" of recoms
# cust  recom
#  1    apple|banana
Taro.Customer.mergeRecommendations <- function(recoms, sales.users){
  appendRecoms <- function(user) {
    recom      <- recoms[recoms$cust == as.character(user$cust),]$recom
    user$recom <- paste(recom, collapse="|")
    user
  }
  
  result <- ddply(sales.users, 'cust', appendRecoms)
}
