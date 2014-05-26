file_dir = getwd(); if( grepl('tests', getwd()) ){ wd <- getwd(); setwd(".."); file_dir <- getwd(); setwd(wd) }
source(paste(file_dir,"/lib/taro_mining/taro_helper.R",sep=""))