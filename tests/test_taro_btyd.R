# test_taro_btyd.R
file_dir = getwd(); if( grepl('tests', getwd()) ){ wd <- getwd(); setwd(".."); file_dir <- getwd(); setwd(wd) }

source(paste(file_dir,"/tests/test_helper.R",sep=""))
source(paste(file_dir,"/lib/taro_mining/taro_btyd.R",sep=""))

context("Taro BTYD")
context("  prepare data without count column")

# Taro.Stats.purchase_frequency_count
test_that("  prepare data without count column", {
  x1 <- c(2,3,1,3,1,2)  
  x2 <- c('jack','jack','daniel','park','park','jack')
  x3 <- c("2014-01-02","2014-01-02","2014-01-02","2014-04-02","2014-05-09","2014-06-02")
  test_data <- data.frame(x1, x2, x3)
  
  # 'cust',"date",'sales'
  data <- Taro.BTYD.prepareData(test_data, c(2,3,1),"%Y-%m-%d")
  d1 <- data[data$cust=='jack',]
  d2 <- head(d1, n=1)
  expect_equivalent(names(data), c("cust","date","sales","count")) 
  expect_equivalent(nrow(d1), 2)
  expect_equivalent(d2$count, 2)
})

context("  prepare data with count column")

# Taro.Stats.purchase_frequency_count
test_that("  prepare data with count column", {
  x1 <- c(2,3,1,3,1,2)  
  x2 <- c('jack','jack','daniel','park','park','jack')
  x3 <- as.Date(c("2014-01-02","2014-01-02","2014-01-02","2014-04-02","2014-05-09","2014-06-02"))
  x4 <- c(2,2,2,3,2,2)  
  test_data <- data.frame(x1, x2, x3, x4)
  
  # 'cust',"date",'sales', 'count'
  data <- Taro.BTYD.prepareData(test_data, c(2,3,1,4),"%Y-%m-%d")
  d1 <- data[data$cust=='jack',]
  d2 <- head(d1, n=1)
  expect_equivalent(names(data), c("cust","date","sales","count")) 
  expect_equivalent(nrow(d1), 2)
  expect_equivalent(d2$count, 4)
 
})