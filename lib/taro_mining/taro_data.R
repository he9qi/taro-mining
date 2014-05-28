# taro_data.R
# find files from a directory and read the file data

TaroData.readDataFromDir <- function(dir_path, pattern=NULL){
  data <- NULL
  
  isDir <- ( length(list.dirs(dir_path)) != 0L )
  
  if(isDir){
    files <- dir(dir_path, pattern = pattern, full.names = TRUE, ignore.case = TRUE)
    for( file in files ){
      file.data <- read.csv(file)#, encoding="UTF-8", stringsAsFactors=FALSE
      data <- rbind(data, file.data)
    }
  }else{
    data <- read.csv(dir_path)
  }
  return(data)
}

