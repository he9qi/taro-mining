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
source(paste(getwd(),"/lib/taro_mining/taro_helper.R",sep=""))         # always include this first
source(paste(getwd(),"/lib/taro_mining/taro_data.R",sep=""))
source(paste(getwd(),"/lib/taro_mining/taro_btyd.R",sep=""))           
source(paste(getwd(),"/lib/taro_mining/taro_mongo.R",sep=""))
source(paste(getwd(),"/lib/taro_mining/taro_stats.R",sep=""))
source(paste(getwd(),"/lib/taro_mining/taro_timely.R",sep=""))
source(paste(getwd(),"/lib/taro_mining/taro_recommender.R",sep=""))
source(paste(getwd(),"/lib/taro_mining/taro_customer.R",sep=""))

debugMode <- TRUE
debugger <- function(message) {
  if(debugMode){
    message(paste("TM:", message, collapse=" "))
  }
}

stop_with_message <- function( message ){
  stop(paste("TM:", message, collapse=" "))
}