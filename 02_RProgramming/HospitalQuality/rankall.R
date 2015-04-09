rankall <- function(outcome, num = "best") {
  ## This function reads the outcome-of-care-measures.csv file and returns a 
  ## 2-column data frame containing the hospital in each state that has the 
  ## ranking specified in num. The function takes two arguments: an outcome name
  ## (outcome) and a hospital ranking (num). The function returns a value for
  ## every state (some may be NA). The first column in the data frame is named
  ## hospital, which contains the hospital name, and the second column is named
  ## state, which contains the 2-character abbreviation for the state name.
  ## Hospitals that do not have data on a particular outcome are excluded from
  ## the set of hospitals when deciding the rankings.
    
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
  
  ## Returns hospital names, state and outcome for all states. NA values are 
  ## filtered here as well.
  h <- subset(d[complete.cases(d),], select=c(Hospital.Name, State, i))
  
  t <- lapply(split(h, h[,2]), function(x){
    ## Order the subset based on the lowest outcome rate first, and hospital name
    ## next.
    r <- x[order(x[,3], x[,1]), ]    
    
    ## Read the specified rank for the hospital
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
    r[n,1:2]
  })
  
  ## Reconstruct the dataframe with properly named columns.
  s <- as.data.frame(do.call(rbind,t))
  colnames(s) <- c("hospital","state")
  
  ## Hack to fill missing states with index value.
  s[!complete.cases(s),2] <- rownames(s)[is.na(s$state)]
  
  ## Return the list
  s  
}

## Test:
## > source("rankall.R")
## > head(rankall("heart attack", 20), 10)
## hospital state
## AK <NA> AK
## AL D W MCMILLAN MEMORIAL HOSPITAL AL
## AR ARKANSAS METHODIST MEDICAL CENTER AR
## AZ JOHN C LINCOLN DEER VALLEY HOSPITAL AZ
## CA SHERMAN OAKS HOSPITAL CA
## CO SKY RIDGE MEDICAL CENTER CO
## CT MIDSTATE MEDICAL CENTER CT
## DC <NA> DC
## DE <NA> DE
## FL SOUTH FLORIDA BAPTIST HOSPITAL FL

## > tail(rankall("pneumonia", "worst"), 3)
## hospital state
## WI MAYO CLINIC HEALTH SYSTEM - NORTHLAND, INC WI
## WV PLATEAU MEDICAL CENTER WV
## WY NORTH BIG HORN HOSPITAL DISTRICT WY

## > tail(rankall("heart failure"), 10)
## hospital state
## TN WELLMONT HAWKINS COUNTY MEMORIAL HOSPITAL TN
## TX FORT DUNCAN MEDICAL CENTER TX
## UT VA SALT LAKE CITY HEALTHCARE - GEORGE E. WAHLEN VA MEDICAL CENTER UT
## VA SENTARA POTOMAC HOSPITAL VA
## VI GOV JUAN F LUIS HOSPITAL & MEDICAL CTR VI
## VT SPRINGFIELD HOSPITAL VT
## WA HARBORVIEW MEDICAL CENTER WA
## WI AURORA ST LUKES MEDICAL CENTER WI
## WV FAIRMONT GENERAL HOSPITAL WV
## WY CHEYENNE VA MEDICAL CENTER WY
