source("complete.R")
corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0

  ## Return a numeric vector of correlations
  
  out <- c()
  ids <- subset(complete(directory), nobs>threshold, id)[["id"]]
  f <- list.files(directory, full.names = TRUE)[ids]
  
  for (i in f) {
    d <- read.csv(i)
    out <- c(out, cor(d[["sulfate"]],d[["nitrate"]], use="complete.obs"))
  }
  return(out)
}