# taro_recommender.R

InstallCandidates <- c("recommenderlab")
toInstall <- InstallCandidates[!InstallCandidates %in% library()$results[,1]]
if(length(toInstall)!=0)
{install.packages(toInstall, repos='http://cran.r-project.org')}
lapply(InstallCandidates, library, character.only = TRUE)

# prepare data for recommender
Taro.Recommender.productByPerson <- function(data, start=NULL, end=NULL){
  f <- Taro.Helper.select_dates(data, start, end)
  l <- unique(f$product)
  u <- unique(f$cust)
  
  row <- NULL
  for (p in l) {
    
    # check to see if every user has bought the product
    usersOfProduct = subset(f, f$product == as.character(p))
    bi = lapply(u, function(x) (if(x %in% usersOfProduct$cust){1}else{0}) )
    
    # add row
    if (is.null(row)){ row <- bi }
    else{ row <- c(row,bi) }
  }
  
  # convert to matrix
  matrix(row, ncol=length(l), dimnames=list(user=u, item=l))
}


# build recommendation data for user x product
Taro.Recommender.build <- function(mat, partitions=4){
  users <- dimnames(mat)$user
  len   <- length(users)
  b     <- as(mat, "binaryRatingMatrix")
  if( partitions > 10 ) { partitions = 4 } # partitions can not be too big or it is too computensive
  
  # first split partitions
  parts <- split(c(1:len), ceiling(seq_along(c(1:len))/(len/partitions)))
  
  # convert to vector so we can exclude later
  partsVec <- NULL
  for(p in parts){ partsVec <- c(partsVec,(as.vector(p))) }
  
  # loop through parts and do recommendation
  cust_recom <- NULL
  for( part in parts){
    trainParts <- partsVec[-part]
    
    r <- Recommender(b[trainParts], method="POPULAR")
    recom <- predict(r, b[part], n=3)
    
    for( j in c(1:length(part)) ){
      u <- users[part][j]
      l <- getList(recom)[j]
      df <- data.frame(cust=u, recom=unlist(getList(recom)[1]))
      cust_recom <- rbind(cust_recom, df)
    }
  }
  return(cust_recom)
}