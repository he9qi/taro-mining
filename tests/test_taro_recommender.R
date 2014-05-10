# test_taro_recommender.R
file_dir = getwd(); if( grepl('tests', getwd()) ){ wd <- getwd(); setwd(".."); file_dir <- getwd(); setwd(wd) }

source(paste(file_dir,"/tests/test_helper.R",sep=""))
source(paste(file_dir,"/taro_recommender.R",sep=""))

context("Taro Recommender")
context("  prepare data for recommender by product x person")

# Taro.Recommender.productByPerson
test_that("  group by products and order by quantity", {
  cust     <- c('jack','jack','daniel','park','park','jack','adam','jessica','jordan')
  product  <- c('banana','apple','peach','apple','apple','banana','peach','avacado','banana')
  sales    <- c(2,3,1,3,3,2,1,2,3)  
  date     <- as.Date(c("2014-01-02","2014-02-02","2014-01-02","2014-04-02","2014-05-09","2014-06-02","2014-04-02","2014-05-09","2014-06-02"))
  test_data <- data.frame(cust, date, sales, product)
  
  mat <- Taro.Recommender.productByPerson(test_data)

  expect_equivalent(dimnames(mat)$user, c("jack", "daniel", "park", "adam", "jessica", "jordan") )
  expect_equivalent(dimnames(mat)$item, c("banana", "peach", "apple", 'avacado') )
  
  expect_equivalent(as.numeric(mat[1,1]), 1) # jack bought banana?
  expect_equivalent(as.numeric(mat[1,2]), 0) # jack bought peach?
  
  expect_equivalent(as.numeric(mat[2,3]), 0) # daniel bought apple?
  expect_equivalent(as.numeric(mat[2,2]), 1) # daniel bought peach?
  
  ### prediction
  cust_recom <- Taro.Recommender.build(mat)
  recomFor   <- function(person) {
    cust_recom[cust_recom$cust == person,]
  }
  recoms <- recomFor('jack')
  expect_equivalent(as.character(recoms$recom[1]), 'peach')
  
})