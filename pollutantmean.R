pollutantmean <- function(directory, pollutant, id = 1:332) {
  all_data<-NULL;
  for (i in id){
    file_name=paste(directory,"/",formatC(i, width=3, flag="0"),".csv",sep="")
    print(file_name)
    if (exists("all_data"))
      all_data <-rbind(all_data,read.csv(file_name))  
    else
      all_data <- read.csv(file_name)
  }
  cleaned<- subset(all_data,!is.na(all_data[,pollutant]))
  return(mean(cleaned[,pollutant]))
}