# taro_data.R
# find files from a directory and read the file data

TaroData.readDataFromDir <- function(dir_path, pattern=NULL){
  data <- NULL
  files <- dir(dir_path, pattern = pattern, full.names = TRUE, ignore.case = TRUE)
  for( file in files ){
    file.data <- read.csv(file)#, encoding="UTF-8", stringsAsFactors=FALSE
    data <- rbind(data, file.data)
  }
  return(data)
}

