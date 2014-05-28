# test_taro_data.R
# find files from a directory and read the file data

file_dir = getwd(); if( grepl('tests', getwd()) ){ wd <- getwd(); setwd(".."); file_dir <- getwd(); setwd(wd) }
source(paste(file_dir,"/tests/test_helper.R",sep=""))
source(paste(file_dir,"/lib/taro_mining/taro_data.R",sep=""))

test.data_path <- paste(file_dir, "/tests/fixtures" ,sep="")

context("Taro Data")
context("  read data from directory that doesn't have the files")

# TaroData.readDataFromDir
test_that("  read data from directory that doesn't have the files", {
  data <- TaroData.readDataFromDir(test.data_path, pattern=".r")
  # should return NULL
  expect_equivalent(data, NULL) 
})

context("  read data from singel file")

# TaroData.readDataFromDir
test_that("  read data from singel file", {
  data <- TaroData.readDataFromDir(paste(test.data_path, "/2013-01-02.csv", sep='/'), pattern=".csv")
  # should return NULL
  expect_equivalent(nrow(data), 3) 
  
  first = head(data, n=1)
  expect_equivalent(first[,3], 7842361474778)
})


context("  read data from multiples files")

# TaroData.readDataFromDir
test_that("  read data from directory that doesn't have the files", {
  data <- TaroData.readDataFromDir(test.data_path, pattern=".csv")
  # should return NULL
  expect_equivalent(nrow(data), 5) 
  
  first = head(data, n=1)
  expect_equivalent(first[,3], 9992194378756)
})
