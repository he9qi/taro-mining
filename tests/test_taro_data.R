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
  data <- TaroData.readDataFromDir(test.data_path, pattern="unkown")
  # should return NULL
  expect_equivalent(data, NULL) 
})

context("  read data from single file")

# TaroData.readDataFromDir
test_that("  read data from singel file", {
  data <- TaroData.readDataFromDir(paste(test.data_path, "2013/1/02.csv", sep='/'), pattern=".csv")
  # should return NULL
  expect_equivalent(nrow(data), 23) 
  
  first = head(data, n=1)
  expect_equivalent(first[,1], 8762650364684)
})


context("  read data from multiples files")

# TaroData.readDataFromDir
test_that("  read data from directory that doesn't have the files", {
  data <- TaroData.readDataFromDir(test.data_path, pattern=".csv")
  # should return NULL
  expect_equivalent(nrow(data), 2131) 
  
  first = head(data, n=1)
  expect_equivalent(first[,1], 9992194378756)
})

test.data_path <- paste(test.data_path , "/2013" ,sep="")

context("  list directories from a directory")

test_that("  list directories from a directory", {
  dirs <- TaroData.listDirs(test.data_path)
  expect_equivalent(length(dirs), 58)
})
