complete <- function(directory, id = 1:332) {
  result <- data.frame(id = numeric(length(id)), nobs = numeric(length(id)), stringsAsFactors = FALSE)
  for (i in id){
    file_name=paste(directory,"/",formatC(i, width=3, flag="0"),".csv",sep="")
    data<-read.csv(file_name) 
    result[match(i,id),]<-c(i,sum(complete.cases(data)))
    data<-NULL;
  }
  return(result)
}