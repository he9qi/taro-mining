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
  date <- as.Date(c("2014-01-02","2014-02-02","2014-01-02","2014-04-02","2014-05-09","2014-06-02"))
  test_data <- data.frame(cust, date, sales)
  
  total <- Taro.Stats.total(test_data)
  
  expect_equivalent(total$quantity, 6) 
  expect_equivalent(total$amount, 12) 
  expect_equivalent(total$customer_count, 3) 
  expect_equivalent(total$amount_per_customer, 4) 
  expect_equivalent(total$quantity_per_customer, 2) 
  
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

context("  user average purchase frequency count")

# Taro.Stats.user_avg_purchase_freq
test_that("  user average purchase frequency count", {
  cust  <- c('jack','jack','daniel','park','park','jack')
  sales <- c(2,3,1,3,1,2)  
  date <- as.Date(c("2014-01-02","2014-02-02","2014-01-02","2014-04-02","2014-05-09","2014-06-02"))
  test_data <- data.frame(cust, date, sales)
  
  pfcOf <- function(name){
    pfc <- Taro.Stats.user_avg_purchase_freq(test_data)
    pfc[pfc$cust == name,]
  }
  
  p1 = pfcOf('jack')
  p2 = pfcOf('daniel')
  
  expect_equivalent(p1$avg_purchase_freq, 75.5) 
  expect_equivalent(nrow(p2), 0)
})

context("  group by users and order by quantity")

# Taro.Stats.personly
test_that("  group by users and order by quantity", {
  cust  <- c('jack','jack','daniel','park','park','jack')
  sales <- c(2,3,1,3,1,2)  
  date <- as.Date(c("2014-01-02","2014-02-02","2014-01-02","2014-04-02","2014-05-09","2014-06-02"))
  test_data <- data.frame(cust, date, sales)
  
  personly <- Taro.Stats.personly(test_data)
  
  p1 = head(personly, n=1)
  
  expect_equivalent(p1$cust, "jack") 
  expect_equivalent(p1$quantity, 3)
  expect_equivalent(p1$amount, 7)
  expect_equivalent(p1$last_purchase_at, "2014-06-02")
  expect_equivalent(p1$last_purchase_amount, 2)
})

context("  group by products and order by quantity")

# Taro.Stats.productly
test_that("  group by products and order by quantity", {
  cust     <- c('jack','jack','daniel','park','park','jack')
  product  <- c('banana','apple','peach','apple','apple','banana')
  sales    <- c(2,3,1,3,3,2)  
  date     <- as.Date(c("2014-01-02","2014-02-02","2014-01-02","2014-04-02","2014-05-09","2014-06-02"))
  test_data <- data.frame(cust, date, sales, product)
  
  productly <- Taro.Stats.productly(test_data)
  
  p1 = head(productly, n=1)
  
  expect_equivalent(p1$product, "apple") 
  expect_equivalent(p1$quantity, 3)
  expect_equivalent(p1$amount, 9)
  expect_equivalent(p1$last_purchase_at, "2014-05-09")
  expect_equivalent(p1$last_purchase_amount, 3)
})



