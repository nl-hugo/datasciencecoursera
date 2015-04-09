pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files

  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".

  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## read the data
  d <- read_data(directory, id)
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  mean(d[[pollutant]], na.rm=TRUE)
}

read_data <- function(directory, id = 1:332) {
  f <- list.files(directory, full.names = TRUE)[id]
  d <- lapply(f, read.csv)
  do.call(rbind, d)
}