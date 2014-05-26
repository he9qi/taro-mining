# taro_stats.R
InstallCandidates <- c('plyr', 'lubridate')
toInstall <- InstallCandidates[!InstallCandidates %in% library()$results[,1]]
if(length(toInstall)!=0)
{install.packages(toInstall, repos='http://cran.r-project.org')}
lapply(InstallCandidates, library, character.only = TRUE)


# build data for stats
# columns: 
#    id,time,product,cust,sales,count*
Taro.Stats.prepareData <- function(rdata, column_vector, format="%Y/%m/%d") {
  utrans <- rdata[,column_vector]
  if(length(column_vector) <= 5){
    utrans$count  <- 1
  }
  
  names(utrans) <- c("trans_id","date","product", 'cust', 'sales', 'count') 
  utrans$sales  <- as.numeric(as.character(utrans$sales))
  utrans$count  <- as.numeric(as.character(utrans$count))
  utrans        <- subset(utrans, utrans$sales > 0)
  utrans$date   <- Taro.Helper.toDate(as.character(utrans$date), format=format)#%Y%m%d %m/%d/%Y
  
  return(unique(utrans))
}


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


# merge last month stats from *sales.monthly* and *sales.total*
# note: for computation benefit, we don't compute sales.monthly and 
# sales.total here, we just use them directly.
Taro.Stats.mergeLastMonthStats <- function(sales.monthly, sales.total) {
  last <- tail(sales.monthly, n=1)
  sales.total$last_month_stats <- paste(last$amount, last$quantity, last$delta, last$qdelta)
  return(sales.total)
}


# schema:
# amount   quantity customer_count amount_per_customer quantity_per_customer trans_new_customer trans_new_cust_interval customer_buy_count_2
# 1125048    10844     3941            285.4727              2.751586           2.672246                      52                 2453
# total quantity and amount
Taro.Stats.total <- function(f) {
  amount   = sum(f$sales)
  quantity = sum(f$count)
  customer_count = length(unique(f$cust))
  amount_per_customer = amount / customer_count
  quantity_per_customer = quantity / customer_count
  
  result <- data.frame(amount=amount, quantity=quantity, 
             customer_count=customer_count,
             amount_per_customer=amount_per_customer,
             quantity_per_customer=quantity_per_customer)
  
  return(result)
}
