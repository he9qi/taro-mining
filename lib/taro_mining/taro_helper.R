# taro_helper.R
InstallCandidates <- c('plyr', 'lubridate')
toInstall <- InstallCandidates[!InstallCandidates %in% library()$results[,1]]
if(length(toInstall)!=0)
{install.packages(toInstall, repos='http://cran.r-project.org')}
lapply(InstallCandidates, library, character.only = TRUE)

################  HELPER FUNCTIONS ################  

# count the column row
Taro.Helper.bin_count <- function(f, column_to_count){
  result <- ddply(f, column_to_count, function(x) c(quantity=nrow(x)))
  
  return(result)
}

# convert to date
Taro.Helper.toDate <- function(dates, format){
  mydate <- as.Date(dates, format=format)
  if( is.na(mydate[1])){    
    mydate <- as.Date(dates) # try without format
  }
  return(mydate)
}


# returns data within dates and dates are sorted too
Taro.Helper.select_dates <- function(data, date_s=NULL, date_e=NULL) {
  if(is.null(date_e)){ date_e = max(data$date) }
  if(is.null(date_s)){ date_s = min(data$date) }
  result <- subset(data, data$date >= date_s & data$date <= date_e)
  
  return(result[order(result$date),])
}


# returns data with customer who purchase more than *purchase_count* times
Taro.Helper.select_customer_purchases <- function(data, cust_column, purchase_count) {
  result <- ddply(data, cust_column, function(x) c(quantity=nrow(x), amount=sum(x$sales)))
  result <- subset(result, result$quantity >= purchase_count)
  
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
# note that *count* *sales* and *cust* columns have to be there
Taro.Helper.group_sales <- function(f, group_by='date'){
  result <- ddply(f, group_by, function(x) c(quantity=sum(x$count), amount=sum(x$sales), tnc=length(unique(x$cust)) ))
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
# NOTE: order_by has to be one of *quantity* *amount* *last_purchase_at* *last_purchase_amount*
Taro.Helper.basic_group_sales <- function(data, group_by, order_by, start=NULL, end=NULL) {
  f <- Taro.Helper.select_dates(data, start, end)
  result <- ddply(f, group_by, function(x) c(quantity=sum(x$count), amount=sum(x$sales), last_purchase_at=as.character(tail(x$date, n=1)), last_purchase_amount=as.numeric(tail(x$sales, n=1))) )
  result$quantity   <- as.numeric(result$quantity)
  result$amount     <- as.numeric(result$amount)
  result[,group_by] <- as.character(result[,group_by])
  result            <- result[order(-result[,order_by]),]
  result$last_purchase_amount     <- as.numeric(result$last_purchase_amount)
  return(result)
}


# group the data by month and add a field 'month' in data note that 
# there's no way to preserve actually date of each transactions
# as each transactions are grouped into month/year/weeks. Since we 
# use group_sales here, the result is in same format of what returns
# from group_sales plus a month field.
Taro.Helper.groupMonth <- function(data){
  mo <- strftime(data$date, "%m")
  yr <- strftime(data$date, "%Y")
  data$date  <- paste(yr,mo,'01',sep='-')
  data <- Taro.Helper.group_sales(data, 'date')
  data$month <- as.numeric(strftime(data$date, "%m"))
  return(data)
}


# double group the transactions by *group_by* and in each group group the transaction by function
# provided as FUC, in this case for example, the function is group by month, so transactions are
# grouped by 'user'/'product' and each user has transactions group by month
Taro.Helper.groupTimely <- function(data, group_by, FUN=Taro.Helper.groupMonth, start=NULL, end=NULL){
  f <- Taro.Helper.select_dates(data, start, end)
  l <- unique(f[,group_by])
  
  ptransg2 = NULL
  for (pname in l) {
    ptrans = subset(f, f[,group_by] == as.character(pname))
    ptransg <- FUN(ptrans)
    ptransg[,group_by] = as.character(pname)
    ptransg$delta <- c(0, diff(as.numeric(ptransg$amount)))
    ptransg2 <- rbind(ptransg, ptransg2)
  }
  
  return(ptransg2)
}

