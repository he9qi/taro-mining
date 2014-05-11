### taro_mining.R
### install packages if necessory
###   plyr:           to break a big problem down into manageable pieces, operate on each pieces 
###   rmongodb:       mongodb for R
###   BTYD:           event log behavior prediction
###   lubridate:      work with dates and times
###   recommenderlab: recommender algorithm for R
###   gdata:          file read xls
###   RCurl:          read file from URL

InstallCandidates <- c('rmongodb', "BTYD", 'plyr', 'lubridate', 'recommenderlab', 'gdata', 'RCurl')
toInstall <- InstallCandidates[!InstallCandidates %in% library()$results[,1]]
if(length(toInstall)!=0)
{install.packages(toInstall, repos='http://cran.r-project.org')}
lapply(InstallCandidates, library, character.only = TRUE)

######  include sources  ########
source("taro_helper.R")         # always include this first
source("taro_btyd.R")           
source("taro_mongo.R")
source("taro_stats.R")
source("taro_timely.R")
source("taro_recommender.R")
source("taro_customer.R")