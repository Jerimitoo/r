outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
head(outcome)
outcome[, 11] <- as.numeric(outcome[, 11])
## You may get a warning about NAs being introduced; that is okay
hist(outcome[, 11])

best <- function(state, outcome) {

    ## Read outcome data
  outcome1 <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

    ## Check that state and outcome are valid
  if(!any(state == outcome1$State)){
    stop("invalid state")}
  else if((outcome %in% c("heart attack", "heart failure","pneumonia")) == FALSE) {
    stop(print("invalid outcome"))
  }
  
  ## Return hospital name in that state with lowest 30-day death
  ## rate
    outcome2 <- subset(outcome1, State == state)
  if (outcome == "heart attack") {
    colnum <- 11
  }
  else if (outcome == "heart failure") {
    colnum <- 17
  }
  else {
    colnum <- 23
  }
  min_row <- which(as.numeric(outcome2[ ,colnum]) == 
             min(as.numeric(outcome2[ ,colnum]), na.rm = TRUE))
  hospitals <- outcome2[min_row,2]
  hospitals <- sort(hospitals)
  return(hospitals[1])
}