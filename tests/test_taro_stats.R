# test_taro_timely.R
file_dir = getwd(); if( grepl('tests', getwd()) ){ wd <- getwd(); setwd(".."); file_dir <- getwd(); setwd(wd) }

source(paste(file_dir,"/tests/test_helper.R",sep=""))
source(paste(file_dir,"/taro_stats.R",sep=""))

context("Taro Stats")
context("  total stats")

# Taro.Stats.total
test_that("  total stats", {
  cust  <- c('jack','jack','daniel','park','park','jack')
  sales <- c(2,3,1,3,1,2)    
  count <- c(2,2,1,1,1,1)
  date <- as.Date(c("2014-01-02","2014-02-02","2014-01-02","2014-04-02","2014-05-09","2014-06-02"))
  test_data <- data.frame(cust, date, sales, count)
  
  total <- Taro.Stats.total(test_data)
  
  expect_equivalent(total$quantity, 8) 
  expect_equivalent(total$amount, 12) 
  expect_equivalent(total$customer_count, 3) 
  expect_equivalent(total$amount_per_customer, 4) 
  expect_equivalent(total$quantity_per_customer, 8/3) 
  
})

context("  merge total stats with last month stats")

# Taro.Stats.mergeLastMonthStats
test_that("  merge total stats with last month stats", {
  cust  <- c('jack','jack','daniel','park','park','jack')
  sales <- c(2,3,1,3,1,2)  
  count <- c(2,2,1,1,1,1)
  date  <- as.Date(c("2014-01-02","2014-02-02","2014-01-02","2014-04-02","2014-05-09","2014-06-02"))
  test_data <- data.frame(cust, date, sales, count)
  
  sales <- function(){
    sales.monthly <- Taro.Timely.monthly(test_data)
    sales.total   <- Taro.Stats.total(test_data) 
    Taro.Stats.mergeLastMonthStats(sales.monthly, sales.total)
  }
  
  total <- sales()
  expect_equivalent(total$quantity, 8) 
  expect_equivalent(total$amount, 12) 
  expect_equivalent(total$customer_count, 3) 
  expect_equivalent(total$amount_per_customer, 4) 
  expect_equivalent(total$quantity_per_customer, 8/3) 
  expect_equivalent(total$last_month_stats, "2 1 1 0") 
})

context("  purchase frequency count")

# Taro.Stats.purchase_frequency_count
test_that("  purchase frequency count", {
  cust  <- c('jack','jack','daniel','park','park','jack')
  sales <- c(2,3,1,3,1,2)  
  date <- as.Date(c("2014-01-02","2014-02-02","2014-01-02","2014-04-02","2014-05-09","2014-06-02"))
  test_data <- data.frame(cust, date, sales)
  
  pfcOf <- function(num){
    pfc <- Taro.Stats.purchase_frequency_count(test_data)
    pfc[pfc$daysBetween == num,]
  }
  
  p1 = pfcOf(31)
  p2 = pfcOf(1)
  
  expect_equivalent(p1$quantity, 1) 
  expect_equivalent(nrow(p2), 0)
})


context("  group by products and order by quantity")

# Taro.Stats.productly
test_that("  group by products and order by quantity", {
  cust     <- c('jack','jack','daniel','park','park','jack')
  product  <- c('banana','apple','peach','apple','apple','banana')
  sales    <- c(2,3,1,3,3,2)  
  count    <- c(2,2,1,1,1,1)
  date     <- as.Date(c("2014-01-02","2014-02-02","2014-01-02","2014-04-02","2014-05-09","2014-06-02"))
  test_data <- data.frame(cust, date, sales, product, count)
  
  productly <- Taro.Stats.productly(test_data)
  
  p1 = head(productly, n=1)
  
  expect_equivalent(p1$product, "apple") 
  expect_equivalent(p1$quantity, 4)
  expect_equivalent(p1$amount, 9)
  expect_equivalent(p1$last_purchase_at, "2014-05-09")
  expect_equivalent(p1$last_purchase_amount, 3)
})


context("  prepare data w/o count column")

# Taro.Stats.purchase_frequency_count
test_that("  prepare data w/o count column", {
  x1 <- c(2,3,1,3,1,2)  
  x2 <- c('jack','jack','daniel','park','park','jack')
  x3 <- as.Date(c("2014-01-02","2014-01-02","2014-01-02","2014-04-02","2014-05-09","2014-06-02"))
  x4 <- c(2,2,2,3,2,2)  
  x5 <- c(7,7,7,7,7,7)  
  x6 <- c('apple','banana','apple','peach','watermelon','banana')  
  test_data <- data.frame(x1, x2, x3, x4, x5, x6)
  
  # "trans_id","date","product", 'cust', 'sales', 'count'
  data <- Taro.Stats.prepareData(test_data, c(5,3,6,2,1,4),"%Y-%m-%d")
  d1 <- data[data$cust=='jack',]
  d2 <- head(d1, n=1)
  expect_equivalent(names(data), c("trans_id","date","product", 'cust', 'sales', 'count')) 
  expect_equivalent(nrow(d1), 3)
  expect_equivalent(d2$count, 2)
  
  data <- Taro.Stats.prepareData(test_data, c(5,3,6,2,1),"%Y-%m-%d")
  d1 <- data[data$cust=='jack',]
  d2 <- head(d1, n=1)
  expect_equivalent(names(data), c("trans_id","date","product", 'cust', 'sales', 'count')) 
  expect_equivalent(nrow(d1), 3)
  expect_equivalent(d2$count, 1)
 
})

