InstallCandidates <- c("testthat")
toInstall <- InstallCandidates[!InstallCandidates %in% library()$results[,1]]
if(length(toInstall)!=0)
{install.packages(toInstall, repos='http://cran.r-project.org')}
lapply(InstallCandidates, library, character.only = TRUE)

file_dir = getwd(); if( grepl('tests', getwd()) ){ wd <- getwd(); setwd(".."); file_dir <- getwd(); setwd(wd) }
source(paste(file_dir,"/lib/taro_mining/taro_helper.R",sep=""))