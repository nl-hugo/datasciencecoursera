best <- function(state, outcome) {
  ## This function reads the outcome-of-care-measures.csv file and returns a 
  ## character vector with the name of the hospital that has the best (i.e. 
  ## lowest) 30-day mortality for the specified outcome in that state. This 
  ## function takes two arguments: the 2-character abbreviated name of a state 
  ## and an outcome name. Hospitals that do not have data on a particular 
  ## outcome are excluded from the set of hospitals when deciding the rankings.
    
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
  
  ## Return the name of the best hospital.
  r[1,1]
}

## Test:
## > source("best.R")
## > best("TX", "heart attack")
## [1] "CYPRESS FAIRBANKS MEDICAL CENTER"

## > best("TX", "heart failure")
## [1] "FORT DUNCAN MEDICAL CENTER"

## > best("MD", "heart attack")
## [1] "JOHNS HOPKINS HOSPITAL, THE"

## > best("MD", "pneumonia")
## [1] "GREATER BALTIMORE MEDICAL CENTER"

## > best("BB", "heart attack")
## Error in best("BB", "heart attack") : invalid state

## > best("NY", "hert attack")
## Error in best("NY", "hert attack") : invalid outcome
