source("complete.R")

corr <- function(directory, threshold = 0) {
  complete_sizes <- complete(directory)
  complete_enough <- subset(complete_sizes,complete_sizes$nobs>threshold)
  if(nrow(complete_enough)==0)
    return(numeric())
  result<-cbind(complete_enough,correlation=0)
  apply(result,1,function(x) {
    file_name=paste(directory,"/",formatC(x["id"], width=3, flag="0"),".csv",sep="")
    data <- read.csv(file_name)
    cleaned<- data[complete.cases(data),]       
    x["correlation"] = cor(cleaned$sulfate,cleaned$nitrate)
  })
}