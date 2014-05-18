# app installer
### install packages if necessory
###   plyr:           to break a big problem down into manageable pieces, operate on each pieces 
###   rmongodb:       mongodb for R
###   BTYD:           event log behavior prediction
###   lubridate:      work with dates and times
###   recommenderlab: recommender algorithm for R
###   gdata:          file read xls
###   RCurl:          read file from URL

update.packages(checkBuilt = TRUE, ask = FALSE)
InstallCandidates <- c('rmongodb', "BTYD", 'plyr', 'lubridate', 'recommenderlab', 'gdata', 'RCurl')
toInstall <- InstallCandidates[!InstallCandidates %in% library()$results[,1]]
if(length(toInstall)!=0)
{install.packages(toInstall, repos='http://cran.r-project.org')}
lapply(InstallCandidates, library, character.only = TRUE)