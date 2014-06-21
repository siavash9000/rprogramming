best <- function(state, outcome) {
  outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  outcome_data[, 11] <- as.numeric(outcome_data[, 11])
  outcome_data[, 17] <- as.numeric(outcome_data[, 17])
  outcome_data[, 23] <- as.numeric(outcome_data[, 23])
  allowed_outcomes <- c("heart attack","heart failure","pneumonia")
  names(outcome_data)[11] <- "heart attack"
  names(outcome_data)[17] <- "heart failure"
  names(outcome_data)[23] <- "pneumonia"
  if(!(state %in% outcome_data[,'State'])) stop("invalid state")
  if(!(outcome %in% allowed_outcomes)) stop("invalid outcome")
  outcome_data = outcome_data[with(outcome_data,order(outcome_data$Hospital.Name)),]  
  state_data = outcome_data[outcome_data$State==state&!is.na(outcome_data[,outcome]),]
  result = state_data[which.min(state_data[,outcome]),]
  result$Hospital.Name
}