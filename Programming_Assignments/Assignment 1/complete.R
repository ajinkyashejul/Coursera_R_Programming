complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  setwd(directory);
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  nobs <- rep(0,times=length(id))
  for(j in 1:length(id))
  {
    i = id[j]
    if(i<=9)
    {
      name <- paste("00",as.character(i),".csv",sep="")
    }
    if(i>9 & i<100)
    {
      name <- paste("0",as.character(i),".csv",sep="")
    }
    if(i>99)
    {
      name <- paste(as.character(i),".csv",sep="")
    }
    temp <- read.csv(name)
    nobs[j] <- sum(complete.cases(temp))
  }
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  d <- data.frame(id,nobs)
  d
}