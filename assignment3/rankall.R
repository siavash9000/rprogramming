rankall <- function(outcome, num = "best") {
  outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  outcome_data[, 11] <- as.numeric(outcome_data[, 11])
  outcome_data[, 17] <- as.numeric(outcome_data[, 17])
  outcome_data[, 23] <- as.numeric(outcome_data[, 23])
  allowed_outcomes <- c("heart attack","heart failure","pneumonia")
  names(outcome_data)[11] <- "heart attack"
  names(outcome_data)[17] <- "heart failure"
  names(outcome_data)[23] <- "pneumonia"
  if(!(outcome %in% allowed_outcomes)) stop("invalid outcome")
  outcome_data$State = factor(outcome_data$State)
  outcome_data = outcome_data[!is.na(outcome_data[,outcome]),]
  outcome_data = outcome_data[with(outcome_data,order(outcome_data[,outcome],outcome_data$Hospital.Name)),]  
  if(num=='best') num=1  
  library(plyr)
  state_labels = levels(outcome_data[,'State'])
  rank = ddply(outcome_data, .(State),function(x)
                  {
                    if(num=='worst') num=nrow(x)
                    cbind(hospital=x[num,'Hospital.Name'])
                  }
          )
  colnames(rank)[1]<-"state"
  rank
}