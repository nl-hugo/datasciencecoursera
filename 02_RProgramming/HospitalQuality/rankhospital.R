rankhospital <- function(state, outcome, num = "best") {
  ## This function reads the outcome-of-care-measures.csv file and returns a 
  ## character vector with the name of the hospital that has the ranking 
  ## specified by the num argument. This function takes three arguments: the 
  ## 2-character abbreviated name of a state (state), an outcome (outcome), and 
  ## the ranking of a hospital in that state for that outcome (num). The num 
  ## argument can take values “best”, “worst”, or an integer indicating the 
  ## ranking (smaller numbers are better). If the number given by num is larger
  ## than the number of hospitals in that state, then the function returns NA.
  ## Hospitals that do not have data on a particular outcome are excluded
  ## from the set of hospitals when deciding the rankings.
  
  ## Read data.
  d <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Get the column index for outcome, or throw an error when an invalid outcome
  ## is specified.
  ## 11 - heart attack
  ## 17 - heart failure
  ## 23 - pneumonia
  i <- if (outcome == "heart attack") {
    11
  } else if (outcome == "heart failure") {
    17
  } else if (outcome == "pneumonia") {
    23
  } else {
    stop("invalid outcome")
  }
  
  ## Coerce to numeric. This introduces NA values, but don't worry about that
  ## now.
  d[,i] <- suppressWarnings(as.numeric(d[,i]))
  
  ## Returns hospital names and outcome in the specified state. NA values are
  ## filtered here as well.
  h <- subset(d[complete.cases(d),], State==state, select=c(Hospital.Name, i))
  
  ## Assume an invalid state when no results are returned.
  if (nrow(h) == 0) {
    stop("invalid state")
  }
  
  ## Order the subset based on the lowest outcome rate first, and hospital name
  ## next.
  r <- h[order(h[,2], h[,1]), ]
  
  ## Read the specified rank for the hospital.
  n <- if (num == "best") {
    1
  } else if (num == "worst") {
    nrow(r)
  } else if (class(num) == "numeric") {
    num
  } else {
    stop("invalid num")
  }
  
  ## Return the name of the hospital, or NA when 'num' is greater than the
  ## number of hospitals in the list.
  r[n,1]
}

## Test:
## > source("rankhospital.R")
## > rankhospital("TX", "heart failure", 4)
## [1] "DETAR HOSPITAL NAVARRO"

## > rankhospital("MD", "heart attack", "worst")
## [1] "HARFORD MEMORIAL HOSPITAL"

## > rankhospital("MN", "heart attack", 5000)
## [1] NA
