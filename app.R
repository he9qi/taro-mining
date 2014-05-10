### app.R
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
source("stats.R")
source("taro_mongo.R")
source("taro_btyd.R")



###### connect to database ######
host     <- "33.33.33.100"
username <- "heyook"
password <- "heyook123"
db       <- "testdatabase"

Taro.Mongo.client   <- Taro.Mongo.init(host, username, password)
Taro.Mongo.db       <- db
Taro.Mongo.saveData <- function(collection, data) { Taro.Mongo.save(Taro.Mongo.client, Taro.Mongo.db, collection, data) }



##### import files to database #####
# use file path:  file_path <- paste(getwd(),"/data/", file_name,sep="") 
# use url:        url <- "http://dl.dropboxusercontent.com/u/123/1332.xls"

file_name  <- "airline.xls"
file_path  <- paste(getwd(),"/data/", file_name,sep="") 
excel.data <- read.xls(file_path)


######################## preprocess data #########################
rdata = excel.data[,c(1:28)]
rdata = subset(rdata, rdata$出票日期 != "出票日期")
rdata = subset(rdata, rdata$票证类型 == "正常票")


####################### prepare data for stats ##################
utrans = rdata[,c(2,1,8,9,18,28,12)]#id,time,product,cust,sales,domestic,klass
names(utrans) <- c("trans_id","date","product", 'cust', 'sales', 'domestic', 'klass') 
utrans$sales  <- as.numeric(as.character(utrans$sales))
utrans = subset(utrans, utrans$sales > 0)
utrans$date   <- as.Date(as.character(utrans$date), format="%Y/%m/%d")#%Y%m%d %m/%d/%Y
utrans$airline<- substr(as.character(utrans$product),1,2)
categories    <- c('klass', 'domestic', 'airline') #input by user

######### preprocess for purchase history analysis ###############
xf <- Taro.BTYD.prepareData(rdata, 9, 1, 18)

###################### END PRE-PROCESSING DATA ###################




############################ DO STATS ############################

# in days and months
sales.days   = stats.days(xf)
sales.months = stats.months(xf)

# timely 
sales.daily   = stats.daily(xf)
sales.weekly  = stats.weekly(xf)
sales.monthly = stats.monthly(xf)
sales.yearly  = stats.yearly(xf)

##### user purchase information
sales.personly     = stats.personly(xf)           # group transactions by users
sales.purchaseFreq = stats.purchase_frequency_count(xf) # compute frequency purchase bin 
f2f <- stats.select_customer_purchases(xf, 'cust', 2)     # get customer with at least 2 purchase

### user purchase frequency
sales.user_purchase_freq = stats.user_avg_purchase_freq(xf)

### Compute Total Amount/Quantity and average
sales.total = stats.total(xf)                    

########## Start estimate for users and total ############
data     <- Taro.BTYD.toCbsCbt(xf)                      # prepare data for estimation
cal2.cbs <- Taro.BTYD.dataCBS(data)                     # get "customer sufficient matrix"
params2  <- Taro.BTYD.estimateParametersByCBS(cal2.cbs) # estimate parameters

# predict # of transactions for a new customer in the next 52 weeks
estimateInterval = 52
transactionsNewCustomer <- Taro.BTYD.predictNewCustomerTransactionsInWeeks(params2, estimateInterval)
customerTransactions    <- Taro.BTYD.predictCustomerTransactionsInWeeks(params2, cal2.cbs, estimateInterval)
customerLiveProbability <- Taro.BTYD.predictCustomerLiveProbability(params2, cal2.cbs)

########## build total info ###############
sales.total$trans_new_cust_interval <- estimateInterval      # estimated interval used for new customer
sales.total$trans_new_customer <- transactionsNewCustomer
sales.total$customer_buy_count_2 <- length(f2f$cust)         # returned customer


########## build user info ###########
# alive_prob, per_trans_in_future, purchasefreq, ltv
# or we can use # merge two data frames by ID : total <- merge(data frameA,data frameB,by="ID")
sales.users <- sales.personly[c(1,2,3,4,5)]
sales.users$trans52  <- customerTransactions[sales.users$cust]
sales.users$liveness <- customerLiveProbability[sales.users$cust]
sales.users$ltv <- sales.users$amount
sales.users$name <- as.character(sales.users$cust)
sales.users <- sales.users[order(-sales.users[,'trans52']),]

#merge user purchase freq
sales.users <- stats.mergeAvgPurchaseFreq(sales.users, sales.user_purchase_freq)


######## USING utrans to compute mainly timely transaction data ##########

# f2ftrans <- subset(utrans, utrans$cust %in% f2f$cust)
sales.userTimely = stats.groupTimely(utrans, 'cust')

##### productly
sales.productly = stats.productly(utrans)          # group transactions by products
sales.productTimely = stats.groupTimely(utrans, 'product')  # group same month transactions by products



###########  save stats to DB! ###########

##### calculate category
for( cat in categories ) {  
  col_name = paste('stats', cat, sep='_')
  sales.catly = stats.basic_group_sales(utrans, cat, 'quantity')   
  Taro.Mongo.saveData(col_name, sales.catly)
}


### sales time series
Taro.Mongo.saveData('stats_days', sales.days)
Taro.Mongo.saveData('stats_months', sales.months)
Taro.Mongo.saveData('stats_daily', sales.daily)
Taro.Mongo.saveData('stats_weekly', sales.weekly)
Taro.Mongo.saveData('stats_monthly', sales.monthly)
Taro.Mongo.saveData('stats_yearly', sales.yearly)

### user purchase information
Taro.Mongo.saveData('stats_personly', sales.personly)
Taro.Mongo.saveData('stats_purchase_freq', sales.purchaseFreq)
Taro.Mongo.saveData('stats_userTimely', sales.userTimely)

### product purchase information
Taro.Mongo.saveData('stats_productly', sales.productly)
Taro.Mongo.saveData('stats_productTimely', sales.productTimely)

### save total and users
Taro.Mongo.saveData('stats_users', sales.users)
Taro.Mongo.saveData('stats_total', sales.total)
