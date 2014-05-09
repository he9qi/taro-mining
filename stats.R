# stats.R

################  HELPER FUNCTIONS ################  

# count the column row
stats.bin_count <- function(f, column_to_count){
  result <- ddply(f, column_to_count, function(x) c(quantity=nrow(x)))
  
  return(result)
}


# returns data within dates and dates are sorted too
stats.select_dates <- function(data, date_s=FALSE, date_e=FALSE) {
  if(!date_e){ date_e = max(data$date) }
  if(!date_s){ date_s = min(data$date) }
  result <- subset(data, data$date >= date_s & data$date <= date_e)
  
  return(result[order(result$date),])
}


# returns data with customer who purchase more than *purchase_count* times
stats.select_customer_purchases <- function(data, cust_column, purchase_count) {
  result <- ddply(f, cust_column, function(x) c(quantity=nrow(x), amount=sum(x$sales)))
  result <- subset(result, result$quantity > purchase_count)
  
  return(result)
}


# group the transactions by *group_by* (in our case, usually 'date/week/month') and then compute the following 
# properties of each group:
#     quantities
#     total amount
#     total customers
#     quantity per customer
#     amount per customer
#     change in amount between each concatenated group
#     change in quantity between each concatenated group
stats.group_sales <- function(f, group_by='date'){
  result <- ddply(f, group_by, function(x) c(quantity=nrow(x), amount=sum(x$sales), tnc=length(unique(x$cust)) ))
  result$qpc <- as.numeric(result$quantity) / as.numeric(result$tnc)
  result$apc <- as.numeric(result$amount) / as.numeric(result$tnc)
  result$delta <- c(0, diff(as.numeric(result$amount)))
  result$qdelta <- c(0, diff(as.numeric(result$quantity)))
  return(result)
}


# group transaction by *group_by* (usually 'NONE-date' in our case) and order them by *order_by* and 
# compute the following properties for each group:
#     quantity
#     amount
#     last purchased date
#     last purchased amount
stats.basic_group_sales <- function(data, group_by, order_by, start=FALSE, end=FALSE) {
  f <- stats.select_dates(data, start, end)
  result <- ddply(f, group_by, function(x) c(quantity=nrow(x), amount=sum(x$sales), last_purchase_at=as.character(tail(x$date, n=1)), last_purchase_amount=as.numeric(tail(x$sales, n=1))) )
  result$quantity   <- as.numeric(result$quantity)
  result$amount     <- as.numeric(result$amount)
  result[,group_by] <- as.character(result[,group_by])
  result            <- result[order(-result[,order_by]),]
  return(result)
}


# group the data by month and add a field 'month' in data note that 
# there's no way to preserve actually date of each transactions
# as each transactions are grouped into month/year/weeks. Since we 
# use group_sales here, the result is in same format of what returns
# from group_sales plus a month field.
stats.helper.groupMonth <- function(data){
  mo <- strftime(data$date, "%m")
  yr <- strftime(data$date, "%Y")
  data$date  <- paste(yr,mo,'01',sep='-')
  data <- stats.group_sales(data, 'date')
  data$month <- as.numeric(strftime(data$date, "%m"))
  return(data)
}


# double group the transactions by *group_by* and in each group group the transaction by function
# provided as FUC, in this case for example, the function is group by month, so transactions are
# grouped by 'user'/'product' and each user has transactions group by month
stats.groupTimely <- function(data, group_by, FUN=stats.helper.groupMonth, start=FALSE, end=FALSE){
  f <- stats.select_dates(data, start, end)
  l <- unique(f[,group_by])
  
  first = TRUE
  for (pname in l) {
    ptrans = subset(f, f[,group_by] == as.character(pname))
    ptransg <- FUN(ptrans)
    ptransg[,group_by] = as.character(pname)
    ptransg$delta <- c(0, diff(as.numeric(ptransg$amount)))
    if (first){
      ptransg2 <- ptransg
      first = FALSE
    }
    else{
      ptransg2 <- rbind(ptransg, ptransg2)
    }
  }
  
  return(ptransg2)
}


### 
# Merge the user average purchase frequency talbe *sales.user_purchase_freq* 
# into *sales.users*
stats.mergeAvgPurchaseFreq <- function(sales.users, sales.user_purchase_freq){
  first = TRUE
  for(muser in sales.users$cust){
    kusers = subset(sales.users, sales.users$cust == muser)
    if( muser %in% sales.user_purchase_freq$cust){
      pusers = subset(sales.user_purchase_freq, sales.user_purchase_freq$cust == muser)
      kusers$avg_purchase_freq <- pusers$avg_purchase_freq
    }else{
      kusers$avg_purchase_freq <- 0
    }
    if(first){ 
      sales.musers <- kusers
      first = FALSE
    }
    else { sales.musers <- rbind( sales.musers, kusers ) }
  }
  return(sales.musers)
}


###################### STATS FUNCTIONS ######################


### schema:
#  day    quantity amount
# monday    110      11
stats.days <- function(data, start=FALSE, end=FALSE) {
  
  day.name = c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday')
  
  f     <- stats.select_dates(data, start, end)
  f$day <- weekdays(f$date)
  result = ddply(f, 'day', function(x) c(quantity=nrow(x), amount=sum(x$sales)))
  result = result[order(factor(result$day, levels = day.name)),]
  
  return(result)
}


### schema:
# month    quantity amount
# Jan.      1102    113
stats.months <- function(data, start=FALSE, end=FALSE) {
  
  f <- stats.select_dates(data, start, end)
  
  f$month <- months(f$date)
  result = ddply(f, 'month', function(x) c(quantity=nrow(x), amount=sum(x$sales)))
  result = result[order(factor(result$month, levels = month.name)),]
  
  return(result)
}


### schema:
# cust   quantity amount last_purchase_at    last_purchase_amount
# jack       82   9905       2013-12-19                  122
stats.personly <- function(data, start=FALSE, end=FALSE) {
  result <- stats.basic_group_sales(data, 'cust', 'quantity', start, end)
  return(result)
}


### schema:
#  product  quantity amount last_purchase_at   last_purchase_amount
#  CA 4113      116  19295       2013-12-31              68
stats.productly <- function(data, start=FALSE, end=FALSE){
  result <- stats.basic_group_sales(data, 'product', 'quantity', start, end)
  return(result)
}


### schema:
# date         quantity amount tnc qpc       apc    delta qdelta
# 2013-01-01       17    2548  17   1 1   49.88235     0      0
stats.daily <- function(data, start=FALSE, end=FALSE) {
  f <- stats.select_dates(data, start, end)
  da <- strftime(f$date, "%d")
  mo <- strftime(f$date, "%m")
  yr <- strftime(f$date, "%Y")
  
  f$date <- paste(yr,mo,da,sep='-')
  result <- stats.group_sales(f, 'date')

  return(result)
}


### schema:
# date        quantity amount  tnc    qpc       apc   delta qdelta
# 2013-01-07      161  15020   140    1.15 107.28571      0      0
stats.weekly <- function(data, start=FALSE, end=FALSE) {
  f <- stats.select_dates(data, start, end)
  we <- strftime(f$date, "%W")
  yr <- strftime(f$date, "%Y")
  
  f$date <- paste(yr,we,'1',sep='-')
  result = stats.group_sales(f, 'date')
  
  result$date = unlist(lapply(result$date, function(x) as.character(as.POSIXlt(x, format = "%Y-%U-%u"))))
  
  return(result)
}


### schema
# monthly stats
# date        quantity amount tnc     qpc      apc      delta qdelta
# 2013-01-01     1102 113432  655 1.682443  173.1786      0      0
stats.monthly <- function(data, start=FALSE, end=FALSE) {
  f <- stats.select_dates(data, start, end)
  mo <- strftime(f$date, "%m")
  yr <- strftime(f$date, "%Y")
  
  f$date  <- paste(yr,mo,'01',sep='-')
  result = stats.group_sales(f, 'date')
  
  return(result)
}


### schema
# yearly stats
# date        quantity  amount  tnc      qpc      apc     delta qdelta
# 2013-01-01    10844  1125048 3941   2.751586 285.4727     0      0
stats.yearly <- function(data, start=FALSE, end=FALSE) {
  f <- stats.select_dates(data, start, end)
  yr <- strftime(f$date, "%Y")
  
  f$date <- paste(yr,'01','01',sep='-')
  result = stats.group_sales(f, 'date')
  
  return(result)
}


### schema
# compute user purchase freq(avg)
# cust        avg_purchase_freq
# ALLAN          28.20000
stats.user_purchase_freq <- function(f){
  purchaseFreq <- ddply(f, .(cust), summarize,  daysBetween = as.numeric(diff(date)))
  result <- ddply(purchaseFreq, 'cust', function(x) c(avg_purchase_freq=mean(x$daysBetween)))
  return(result)
}


### schema
# compute the frequency count
# daysBetween quantity
#       1      706
stats.purchase_frequency <- function(f){
  purchaseFreq <- ddply(f, .(cust), summarize,  daysBetween = as.numeric(diff(date)))
  result <- stats.bin_count(purchaseFreq, "daysBetween")
  
  return(result)
}


# schema:
# amount   quantity customer_count amount_per_customer quantity_per_customer trans_new_customer trans_new_cust_interval customer_buy_count_2
# 1125048    10844     3941            285.4727              2.751586           2.672246                      52                 2453
# total quantity and amount
stats.total <- function(f) {
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

# for recommender
stats.productPersonly <- function(data, start=FALSE, end=FALSE){
  f <- stats.select_dates(data, start, end)
  l <- unique(f$product)
  u <- unique(f$cust)
  
  first = TRUE
  for (user in u) {
    productOfUser = subset(f, f$cust == as.character(user))
    
    # check to see if every product is bought by a user
    bi = lapply(l, function(x) x %in% productOfUser$product)
    
    if (first){
      row <- bi
      first = FALSE
    }
    else{
      row <- rbind(row, bi)
    }
  }
  return(row)
}
