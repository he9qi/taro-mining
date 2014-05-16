# test_taro_customer.R
file_dir = getwd(); if( grepl('tests', getwd()) ){ wd <- getwd(); setwd(".."); file_dir <- getwd(); setwd(wd) }

source(paste(file_dir,"/tests/test_helper.R",sep=""))
source(paste(file_dir,"/lib/taro_mining/taro_customer.R",sep=""))

context("Taro Customer")
context("  merge transactions by customer")

# Taro.Customer.mergeTransactions
test_that("  merge transactions by customer", {
  cust  <- c('jack','jack','daniel','park','park','jack')
  sales <- c(2,3,1,3,1,2)  
  count <- c(2,1,1,1,1,1)  
  date <- as.Date(c("2014-01-02","2014-02-02","2014-01-02","2014-04-02","2014-05-09","2014-06-02"))
  test_data <- data.frame(cust, date, sales, count)
  
  personly <- Taro.Customer.mergeTransactions(test_data)
  
  p1 = personly[personly$cust == 'jack',]

  expect_equivalent(p1$cust, "jack") 
  expect_equivalent(p1$quantity, 4)
  expect_equivalent(p1$amount, 7)
  expect_equivalent(p1$last_purchase_at, "2014-06-02")
  expect_equivalent(p1$last_purchase_amount, 2)
})


context("  user average purchase frequency count")

# Taro.Customer.avg_purchase_freq
test_that("  user average purchase frequency count", {
  cust  <- c('jack','jack','daniel','park','park','jack')
  sales <- c(2,3,1,3,1,2)  
  date <- as.Date(c("2014-01-02","2014-02-02","2014-01-02","2014-04-02","2014-05-09","2014-06-02"))
  test_data <- data.frame(cust, date, sales)
  
  pfcOf <- function(name){
    pfc <- Taro.Customer.avg_purchase_freq(test_data)
    pfc[pfc$cust == name,]
  }
  
  p1 = pfcOf('jack')
  p2 = pfcOf('daniel')
  
  expect_equivalent(p1$avg_purchase_freq, 75.5) 
  expect_equivalent(nrow(p2), 0)
})

context("  merge avg_purchase_freq to users")

# Taro.Customer.mergeAvgPurchaseFreq
test_that("  merge average purchase frequency of a user \n", {
  
  cust  <- c('jack','jack','daniel','park','park','jack')
  sales <- c(2,3,1,3,1,2)  
  date <- as.Date(c("2014-01-02","2014-02-02","2014-01-02","2014-04-02","2014-05-09","2014-06-02"))
  test_data <- data.frame(cust, date, sales)
  
  cust  <- c('jack','daniel','park')
  sales <- c(2,3,1)  
  test_users <- data.frame(cust, sales)
  
  sales <- function() { 
    sales <- Taro.Customer.mergeAvgPurchaseFreq(test_data, test_users)
  }
  
  cs  = sales()
  cs1 = cs[cs$cust == 'jack',]
  cs2 = cs[cs$cust == 'park',]
  cs3 = cs[cs$cust == 'daniel',]
  
  expect_equivalent(nrow(cs), 3) 
  expect_equivalent(cs1$avg_purchase_freq, 75.5) 
  expect_equivalent(cs2$avg_purchase_freq, 37) 
  expect_equivalent(cs3$avg_purchase_freq, as.numeric(NA)) 
})


context("  merge by individual users and months")

# Taro.Customer.mergeTransactionsByMonth
test_that("  merge by individual users and months", {
  
  cust  <- c('jack','jack','daniel','park','park','jack')
  sales <- c(2,3,1,3,1,2)  
  count <- c(1,1,1,2,2,2)  
  date <- as.Date(c("2014-01-02","2014-02-02","2014-01-02","2014-04-02","2014-04-09","2014-06-02"))
  test_data <- data.frame(cust, date, sales, count)
  
  cust  <- c('jack','daniel','park')
  sales <- c(2,3,1)  
  test_users <- data.frame(cust, sales)
  
  sales <- function() { 
    sales.userMonthly <- Taro.Helper.groupTimely(test_data, 'cust')
    Taro.Customer.mergeTransactionsByMonth(sales.userMonthly, test_users)
  }
  
  cs  = sales()
  cs1 = cs[cs$cust == 'jack',]
  cs2 = cs[cs$cust == 'park',]
  
  expect_equivalent(nrow(cs), 3) 
  expect_equivalent(cs1$monthly_transactions, "2014-01-01 1 2|2014-02-01 1 3|2014-06-01 2 2") 
  expect_equivalent(cs2$monthly_transactions, "2014-04-01 4 4") 
})

context("  merge recommendations")

# Taro.Customer.mergeRecommendations
test_that("  merge recommendations", {
  
  cust  <- c('jack','jack','daniel','park','park','park')
  recom <- c('peach', 'avacado', 'banana', 'apple', 'avacado', 'banana')
  recom_data <- data.frame(cust, recom)
  
  cust  <- c('jack','daniel','park')
  sales <- c(2,3,1)  
  test_users <- data.frame(cust, sales)
  
  sales <- function() { 
    Taro.Customer.mergeRecommendations(recom_data, test_users)
  }
  
  cs  = sales()
  
  cs1 = cs[cs$cust == 'jack',]
  cs2 = cs[cs$cust == 'park',]
  
  expect_equivalent(nrow(cs), 3) 
  expect_equivalent(cs1$recom, "peach|avacado") 
  expect_equivalent(cs2$recom, "apple|avacado|banana") 
})

