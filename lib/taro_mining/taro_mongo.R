# taro_mongo.R
# save lists to mongo db

InstallCandidates <- c("rmongodb")
toInstall <- InstallCandidates[!InstallCandidates %in% library()$results[,1]]
if(length(toInstall)!=0)
{install.packages(toInstall, repos='http://cran.r-project.org')}
library(rmongodb)

Taro.Mongo.is.connected <- function(Taro.Mongo.client) {
  mongo.is.connected(Taro.Mongo.client)
}

Taro.Mongo.init <- function(host, port=27017, username="", password="") {
  Taro.Mongo.client <- mongo.create(host=host, username=username, password=password)
  return(Taro.Mongo.client)
}

Taro.Mongo.drop <- function(Taro.Mongo.client, Taro.Mongo.db, collection) {
  ns  <- paste(Taro.Mongo.db, collection, sep=".")
  mongo.drop(Taro.Mongo.client, ns)
}

Taro.Mongo.insert <- function(Taro.Mongo.client, Taro.Mongo.db, collection, data) {
  ns  <- paste(Taro.Mongo.db, collection, sep=".")
  mongo.insert(Taro.Mongo.client, ns, data)
}

Taro.Mongo.save <- function(Taro.Mongo.client, Taro.Mongo.db, collection, data){
  Taro.Mongo.drop(Taro.Mongo.client, Taro.Mongo.db, collection)
  for(i in 1:nrow(data)) { 
    row <- data[i,]
    li  <- as.list(row)
    Taro.Mongo.insert(Taro.Mongo.client, Taro.Mongo.db, collection, li)
  }
}

Taro.Mongo.read <- function(Taro.Mongo.client, Taro.Mongo.db, collection){
  ns  <- paste(Taro.Mongo.db, collection, sep=".")
  
  items  <- data.frame()
  cursor <- mongo.find(Taro.Mongo.client, ns)
  
  # iterate and grab the next record 
  while (mongo.cursor.next(cursor)) {
    tmp    <- mongo.bson.to.list(mongo.cursor.value(cursor))
    tmp.df <- as.data.frame(t(unlist(tmp)))
    items  <- rbind.fill(items, tmp.df)
  }
  return(items)
}


