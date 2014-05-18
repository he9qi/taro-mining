# test_taro_helper.R
file_dir = getwd(); if( grepl('tests', getwd()) ){ wd <- getwd(); setwd(".."); file_dir <- getwd(); setwd(wd) }

source(paste(file_dir,"/tests/test_helper.R",sep=""))
source(paste(file_dir,"/lib/taro_mining/taro_helper.R",sep=""))

context("Taro Helper")
context("  conver to date by format")

# Taro.Helper.toDate
test_that("conver to date by format", {
  col1 <- c('a','b','c','a','c')
  col2 <- c(1,2,3,4,5)
  date <- c('2014-1-1','2014-1-2','2014-1-3','2014-1-4','2014-1-5')
  test_data <- data.frame(col1, col2, date)
  
  dates <- Taro.Helper.toDate(test_data$date, "%Y-%k-%m")
  
  expect_equivalent(dates[1], as.Date("2014-01-01"))
})


context("  bin_count")

# Taro.Helper.bin_count
test_that("bin_count count number of unique rows by a column", {
  col1 <- c('a','b','c','a','c','c')
  col2 <- c(1,2,3,4,5,6)
  test_data <- data.frame(col1, col2)
  
  bin_count <- function(x) {
    count_data <- Taro.Helper.bin_count(test_data, 'col1')
    count_data[count_data$col1 == x, ]
  }
  
  count_a <- bin_count("a")
  count_b <- bin_count("b")
  count_c <- bin_count("c")
  
  expect_equivalent(count_a$quantity, 2)
  expect_equivalent(count_b$quantity, 1)
  expect_equivalent(count_c$quantity, 3)
})

context("  select_dates")

# Taro.Helper.select_dates
test_that("select dates from data", {
  name <- c('a','b','c','d','e','f')
  date <- as.Date(c("2013-01-02","2013-02-02","2013-03-02","2013-04-02","2013-05-02","2013-06-02"))
  test_data <- data.frame(name, date)
  
  date_select <- function(x=NULL, y=NULL){
    if(is.null(x)) { Taro.Helper.select_dates(test_data)  }
    else           { Taro.Helper.select_dates(test_data, as.Date(x), as.Date(y)) }
  }
  
  ds1 <- date_select("2013-02-01", "2013-04-03")
  ds2 <- date_select("2013-02-03", "2013-04-01")
  ds3 <- date_select()
  
  expect_equivalent(ds1$date, as.Date(c("2013-02-02","2013-03-02","2013-04-02"))) 
  expect_equivalent(ds2$date, as.Date(c("2013-03-02")))
  expect_equivalent(ds3$date, date) 
})

context("  select_customer_purchases")

# Taro.Helper.select_customer_purchases (data, col, count)
test_that("select customers by their purchase count", {
  name  <- c('park','jack','daniel','park','park','daniel')
  sales <- c(2,3,1,3,1,2)  
  count <- c(2,1,1,1,1,1)  
  test_data <- data.frame(name, sales, count)
  
  customer_select <- function(count){
    Taro.Helper.select_customer_purchases(test_data, 'name', count)
  }
  
  cs1 <- customer_select(1)
  cs2 <- customer_select(2)
  cs3 <- customer_select(3)
  
  expect_equivalent(cs1$name, as.factor(c("daniel","jack","park"))) 
  expect_equivalent(cs2$name, as.factor(c("daniel","park"))) 
  expect_equivalent(cs3$name, as.factor(c("park"))) 
})

context("  group_sales")

# Taro.Helper.group_sales (data, group_by)
test_that("group customer by mostly time", {
  cust  <- c('park','jack','daniel','park','park','jack')
  sales <- c(2,3,1,3,1,2)  
  count <- c(2,3,1,1,1,1)  
  date <- as.Date(c("2013-02-02","2013-02-02","2013-01-02","2013-04-02","2013-05-02","2013-02-02"))
  test_data <- data.frame(cust, date, sales, count)
  
  sales_at <- function(x) { 
    sales <- Taro.Helper.group_sales(test_data, 'date')
    sales[sales$date == x, ] 
  }

  sa1 = sales_at(as.Date("2013-02-02"))
  
  expect_equivalent(sa1$quantity, 6) 
  expect_equivalent(sa1$amount, 7) 
  expect_equivalent(sa1$tnc, 2) 
  expect_equivalent(sa1$qpc, 3) 
  expect_equivalent(sa1$apc, 3.5) 
  expect_equivalent(sa1$delta, 6) 
  expect_equivalent(sa1$qdelta, 5) 
})

context("  basic_group_sales")

# Taro.Helper.basic_group_sales <- function(data, group_by, order_by, ..)
test_that("group customer by mostly NONE-time and order by", {
  cust  <- c('park','jack','daniel','park','jack','jack')
  sales <- c(2,3,1,3,1,2)  
  count <- c(1,1,1,1,1,2)  
  date <- as.Date(c("2013-02-02","2013-03-02","2013-01-02","2013-04-02","2013-04-02","2013-02-02"))
  test_data <- data.frame(cust, date, sales, count)
  
  salesOf <- function(name) { 
    sales <- Taro.Helper.basic_group_sales(test_data, 'cust', 'quantity')
    p1 = sales[sales$cust == name,]
  }
  
  cs = salesOf('jack')
  
  expect_equivalent(cs$quantity, 4) 
  expect_equivalent(cs$cust, 'jack') 
  expect_equivalent(cs$last_purchase_at, "2013-04-02") 
  expect_equivalent(cs$last_purchase_amount, 1) 
})

context("  group by month")

# Taro.Helper.groupMonth
test_that("  group by month", {
  cust  <- c('park','jack','daniel','park','park','jack')
  sales <- c(2,3,1,3,1,2)  
  count <- c(2,1,2,1,1,2)  
  date <- as.Date(c("2013-02-02","2013-02-02","2013-01-02","2013-04-02","2013-05-02","2013-02-02"))
  test_data <- data.frame(cust, date, sales, count)
  
  sales <- function() { 
    sales <- Taro.Helper.groupMonth(test_data)
  }
  
  cs  = sales()
  cs1 = head(cs, 1)
  expect_equivalent(nrow(cs), 4) 
  expect_equivalent(cs1$month, 1) 
  expect_equivalent(cs1$quantity, 2) 
})

context("  group data by two factors")

# Taro.Helper.groupTimely
test_that("  group by customer and month", {
  cust  <- c('jack','jack','daniel','park','park','jack')
  sales <- c(2,3,1,3,1,2)  
  count <- c(2,1,1,3,1,2)  
  date <- as.Date(c("2013-02-02","2013-02-02","2013-01-02","2013-04-02","2013-05-02","2013-06-02"))
  test_data <- data.frame(cust, date, sales, count)
  
  sales <- function() { 
    sales <- Taro.Helper.groupTimely(test_data, 'cust')
  }
  
  cs  = sales()
  cs1 = head(cs, 1)
  
  expect_equivalent(nrow(cs), 5) 
  expect_equivalent(cs1$date, '2013-04-01') 
  expect_equivalent(cs1$cust, 'park') 
  expect_equivalent(cs1$quantity, 3) 
})

