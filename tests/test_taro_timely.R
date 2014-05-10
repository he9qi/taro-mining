# test_taro_timely.R
file_dir = getwd(); if( grepl('tests', getwd()) ){ wd <- getwd(); setwd(".."); file_dir <- getwd(); setwd(wd) }

source(paste(file_dir,"/tests/test_helper.R",sep=""))
source(paste(file_dir,"/taro_timely.R",sep=""))

context("Taro Timely")
context("  in week days")

# Taro.Timely.days
test_that("  sales of each week day", {
  cust  <- c('jack','jack','daniel','park','park','jack')
  sales <- c(2,3,1,3,1,2)  
  date <- as.Date(c("2014-01-02","2014-02-02","2014-01-02","2014-04-02","2014-05-09","2014-06-02"))
  test_data <- data.frame(cust, date, sales)
  
  salesOfDay <- function(day) { 
    sales <- Taro.Timely.days(test_data)
    sales[sales$day == day,]
  }
  
  friday   = salesOfDay("Friday")
  thursday = salesOfDay("Thursday")
  
  expect_equivalent(friday$quantity, 1) 
  expect_equivalent(friday$amount, 1) 
  
  expect_equivalent(thursday$quantity, 2) 
  expect_equivalent(thursday$amount, 3) 

})

context("  in months")

# Taro.Timely.months
test_that("  sales of each month", {
  cust  <- c('jack','jack','daniel','park','park','jack')
  sales <- c(2,3,1,3,1,2)  
  date <- as.Date(c("2014-01-02","2014-01-03","2014-01-04","2014-04-02","2014-05-09","2014-06-02"))
  test_data <- data.frame(cust, date, sales)
  
  salesOfMonth <- function(month) { 
    sales <- Taro.Timely.months(test_data)
    sales[sales$month == month,]
  }
  
  jan = salesOfMonth("January")
  jun = salesOfMonth("June")
  
  expect_equivalent(jan$quantity, 3) 
  expect_equivalent(jan$amount, 6) 
  
  expect_equivalent(jun$quantity, 1) 
  expect_equivalent(jun$amount, 2) 
  
})

context("  daily")

# Taro.Timely.daily
test_that("sale daily", {
  cust  <- c('jack','jack','daniel','park','park','jack')
  sales <- c(2,3,1,3,1,2)  
  date <- as.Date(c("2014-01-02","2014-01-02","2014-01-04","2014-04-02","2014-05-09","2014-06-02"))
  test_data <- data.frame(cust, date, sales)
  
  daily <- Taro.Timely.daily(test_data)
  s1 <- daily[daily$date == "2014-01-02",]
  
  expect_equivalent(nrow(daily), 5) 
  expect_equivalent(s1$amount, 5) 
  expect_equivalent(s1$quantity, 2) 
  expect_equivalent(s1$date, "2014-01-02") 
  
})

context("  weekly")

# Taro.Timely.weekly
test_that("sale weekly", {
  cust  <- c('jack','jack','daniel','park','park','jack')
  sales <- c(2,3,1,3,1,2)  
  date <- as.Date(c("2014-01-02","2014-01-03","2014-01-04","2014-04-02","2014-05-09","2014-06-02"))
  test_data <- data.frame(cust, date, sales)
  
  weekly <- Taro.Timely.weekly(test_data)
  s1 <- weekly[weekly$date == "2014-01-06",]

  expect_equivalent(nrow(weekly), 4) 
  expect_equivalent(s1$amount, 6) 
  expect_equivalent(s1$quantity, 3) 
  expect_equivalent(s1$date, "2014-01-06") 
  
})

context("  monthly")

# Taro.Timely.monthly
test_that("sale monthly", {
  cust  <- c('jack','jack','daniel','park','park','jack')
  sales <- c(2,3,1,3,1,2)  
  date <- as.Date(c("2014-01-02","2014-01-03","2014-01-04","2014-04-02","2014-04-09","2014-06-02"))
  test_data <- data.frame(cust, date, sales)
  
  monthly <- Taro.Timely.monthly(test_data)
  s1 <- monthly[monthly$date == "2014-04-01",]
  
  expect_equivalent(nrow(monthly), 3) 
  expect_equivalent(s1$amount, 4)
  expect_equivalent(s1$quantity, 2) 
  expect_equivalent(s1$date, "2014-04-01") 
  
})

context("  yearly")

# Taro.Timely.yearly
test_that("sale yearly", {
  cust  <- c('jack','jack','daniel','park','park','jack')
  sales <- c(2,3,1,3,1,2)  
  date <- as.Date(c("2013-01-02","2013-01-03","2014-01-04","2014-04-02","2014-04-09","2014-06-02"))
  test_data <- data.frame(cust, date, sales)
  
  yearly <- Taro.Timely.yearly(test_data)
  s1 <- yearly[yearly$date == "2014-01-01",]
  
  expect_equivalent(nrow(yearly), 2) 
  expect_equivalent(s1$amount, 7)
  expect_equivalent(s1$quantity, 4) 
  expect_equivalent(s1$date, "2014-01-01") 
  
})