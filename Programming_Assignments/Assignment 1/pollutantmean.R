pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  setwd(directory);
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  data <- data.frame();
  
  for(i in id)
  {
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
    data <- rbind(data,temp)
  }
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  ## NOTE: Do not round the result!
  if(pollutant == "sulfate")
  {
    d <- data$sulfate
    d <- d[!is.na(d)]
  }
  if(pollutant == "nitrate")
  {
    d <- data$nitrate
    d <- d[!is.na(d)]
  }
  mean(d)
}