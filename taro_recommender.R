# taro_recommender.R

InstallCandidates <- c("recommenderlab")
toInstall <- InstallCandidates[!InstallCandidates %in% library()$results[,1]]
if(length(toInstall)!=0)
{install.packages(toInstall, repos='http://cran.r-project.org')}
library(rmongodb)

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

Taro.Recommender.build <- function(mat, partitions=4){
  users <- dimnames(mat)$user
  b     <- as(mat, "binaryRatingMatrix")
  if( partitions > 10 ) { partitions = 4 } # partitions can not be too big or it is too computensive
  
  len  <- length(users)
  
  # first split partitions
  parts <- split(c(1:len), ceiling(seq_along(c(1:len))/(len/partitions)))
  
  for( part in parts){
    print('---part--')
    print(as.vector(part))
    trainParts <- parts[-part]

    print(trainParts)
    y <- NULL
    for(p in trainParts){ y <- c(y,(as.vector(p))) }
    
    print('---trainParts--')
    print(y)
  }
  
#   for(i in 1:partitions){
#     trainLen = len * i / partitions
#     
#   }
  
}
# 
# b <- as(mat, "binaryRatingMatrix")
# r <- Recommender(b[1:2], method="POPULAR")
# recom <- predict(r, b[3:3], n=1)
# print(getList(recom))
