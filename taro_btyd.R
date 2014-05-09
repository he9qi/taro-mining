# taro_btyd.R
#   predict customer behavior

# build data for predict customer behavior, normally, only 3 columns needed
# for it:  customer_id, purchase_date, and purchase_amount
Taro.BTYD.prepareData <- function(rdata, col_cust, col_date, col_sales) {
  f        <- rdata[,c(col_cust,col_date,col_sales)]
  names(f) <- c("cust","date","sales") 
  f[,3]    <- as.numeric(as.character(f[,3]))                  # format price
  f        <- subset(f, f$sales > 0)                           # eliminate negative price
  f$date   <- as.Date(as.character(f$date), format="%Y/%m/%d") # format date #%Y%m%d %m/%d/%Y 
  xf       <- dc.MergeTransactionsOnSameDate(f)                # merge same day transactions
  return(xf)
}


# return customer live probability
Taro.BTYD.predictCustomerLiveProbability <- function(params2, cal2.cbs) {
  p.alives <- pnbd.PAlive(params2, cal2.cbs[,"x"], cal2.cbs[,"t.x"], cal2.cbs[,"T.cal"])
  return(p.alives)
}

# return predicted customer transaction in weeks
Taro.BTYD.predictNewCustomerTransactionsInWeeks <- function(params2, t=52) {
  return(pnbd.Expectation(params2, t=t))
}

# estimate transactions in a T.star-long duration for that cust
Taro.BTYD.predictCustomerTransactionsInWeeks <- function(params2, cal2.cbs, t.star=52) {
  result <- pnbd.ConditionalExpectedTransactions(params2, T.star = t.star, cal2.cbs[,"x"], cal2.cbs[,"t.x"], cal2.cbs[,"T.cal"])  # T.star => weeks
  return(result)
}

# convert data to CBT and CBS so we can estimate later on
Taro.BTYD.toCbsCbt <- function(f) {
  
  # divide data into train and test
  # below is calibration 
  # min(f$date) + as.numeric((max(f$date)- min(f$date))/2)
  (end.of.cal.period <- max(f$date)) #REMEMBER TO USE LAST DATE,,, (IN EXPERIMENT, USE MIDDLE DATE)
  
  
  # split data into train(calibration) and test (holdout) and make matrices
  # Uses an event log to return calibration period CBT and CBS, holdout 
  # period CBT and CBS, and summary data for each customer (including times of first and last transactions).
  data <- dc.ElogToCbsCbt(f, per="week", 
                          T.cal=end.of.cal.period,
                          merge.same.date=TRUE, # already did this
                          statistic = "freq") # which CBT to return
  str(data)
  
  return(data)
}

# get CBS for predication
Taro.BTYD.dataCBS <- function(data){
  # cbs is short for "customer-by-sufficient-statistic” matrix, with the sufficient stats being: 
  #    frequency
  #    recency (time of last transaction) and
  #    total time observed
  # cal2.cbs <- data$cal$cbs #same thing as below????
  cal2.cbs  <- as.matrix(data[[1]][[1]]) #first item in list, first item in it!
  
  return(cal2.cbs)
}

# Main model params are :

# r          gamma parameter for NBD transaction 
# alpha      gamma parameter for NBD transaction 
# s          gamma parameter for Pareto (exponential gamma) dropout process
# beta       gammma parameter for Pareto (exponential gamma) dropout process

Taro.BTYD.estimateParametersByCBS <- function(cal2.cbs, iteration_count=20) {
  
  # Estimate parameters for model
  # Purchase shape and scale params: r and α
  # Dropout shape and scale params: β and s
  
  # initial estimate
  (params2 <- pnbd.EstimateParameters(cal2.cbs))
  
  # look at log likelihood
  (LL <- pnbd.cbs.LL(params2, cal2.cbs))
  
  # make a series of estimates, see if they converge
  p.matrix <- c(params2, LL)
  for (i in 1:iteration_count) {
    params2 <- pnbd.EstimateParameters(cal2.cbs, params2)
    LL <- pnbd.cbs.LL(params2, cal2.cbs)
    p.matrix.row <- c(params2, LL)
    p.matrix <- rbind(p.matrix, p.matrix.row)
  }
  
  # use final set of values
  (params2 <- p.matrix[dim(p.matrix)[1],1:4])
  
  return(params2)
}
  