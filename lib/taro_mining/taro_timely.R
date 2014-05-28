# taro_timely.R
InstallCandidates <- c('plyr', 'lubridate')
toInstall <- InstallCandidates[!InstallCandidates %in% library()$results[,1]]
if(length(toInstall)!=0)
{install.packages(toInstall, repos='http://cran.r-project.org')}
lapply(InstallCandidates, library, character.only = TRUE)

### schema:
#  day    quantity amount
# monday    110      11
Taro.Timely.days <- function(data, start=NULL, end=NULL) {
  
  day.name = c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday')
  
  f     <- Taro.Helper.select_dates(data, start, end)
  f$day <- weekdays(f$date)
  result = ddply(f, 'day', function(x) c(quantity=sum(x$count), amount=sum(x$sales)))
  result = result[order(factor(result$day, levels = day.name)),]
  
  return(result)
}


### schema:
# month    quantity amount
# Jan.      1102    113
Taro.Timely.months <- function(data, start=NULL, end=NULL) {
  
  f <- Taro.Helper.select_dates(data, start, end)
  
  f$month <- months(f$date)
  result = ddply(f, 'month', function(x) c(quantity=sum(x$count), amount=sum(x$sales)))
  result = result[order(factor(result$month, levels = month.name)),]
  
  return(result)
}


### schema:
# date         quantity amount tnc qpc       apc    delta qdelta
# 2013-01-01       17    2548  17   1 1   49.88235     0      0
Taro.Timely.daily <- function(data, start=NULL, end=NULL) {
  f <- Taro.Helper.select_dates(data, start, end)
  f <- Taro.Timely.toDateString(f)
  result <- Taro.Helper.group_sales(f, 'date')
  
  return(result)
}


### schema:
# date        quantity amount  tnc    qpc       apc   delta qdelta
# 2013-01-07      161  15020   140    1.15 107.28571      0      0
Taro.Timely.weekly <- function(data, start=NULL, end=NULL) {
  f <- Taro.Helper.select_dates(data, start, end)
  f <- Taro.Timely.toWeekString(f)
  result = Taro.Helper.group_sales(f, 'date')
  
  result$date = unlist(lapply(result$date, function(x) as.character(as.POSIXlt(x, format = "%Y-%U-%u"))))
  result <- subset(result, !is.na(result$date))
  
  return(result)
}


### schema
# monthly stats
# date        quantity amount tnc     qpc      apc      delta qdelta
# 2013-01-01     1102 113432  655 1.682443  173.1786      0      0
Taro.Timely.monthly <- function(data, start=NULL, end=NULL) {
  f <- Taro.Helper.select_dates(data, start, end)
  f <- Taro.Timely.toMonthString(f)
  Taro.Helper.group_sales(f, 'date')
}


### schema
# yearly stats
# date        quantity  amount  tnc      qpc      apc     delta qdelta
# 2013-01-01    10844  1125048 3941   2.751586 285.4727     0      0
Taro.Timely.yearly <- function(data, start=NULL, end=NULL) {
  f <- Taro.Helper.select_dates(data, start, end)
  f <- Taro.Timely.toYearString(f)
  Taro.Helper.group_sales(f, 'date')
}


# private

Taro.Timely.toDateString <- function(f){
  da <- strftime(f$date, "%d")
  mo <- strftime(f$date, "%m")
  yr <- strftime(f$date, "%Y")
  
  f$date <- paste(yr,mo,da,sep='-')
  return(f)
}

Taro.Timely.toWeekString <- function(f){
  we <- strftime(f$date, "%W")
  yr <- strftime(f$date, "%Y")
  
  f$date <- paste(yr,we,'1',sep='-')
  return(f)
}

Taro.Timely.toMonthString <- function(f){
  mo <- strftime(f$date, "%m")
  yr <- strftime(f$date, "%Y")
  
  f$date  <- paste(yr,mo,'01',sep='-')
  return(f)
}

Taro.Timely.toYearString <- function(f){
  yr <- strftime(f$date, "%Y")
  
  f$date <- paste(yr,'01','01',sep='-')
  return(f)
}
