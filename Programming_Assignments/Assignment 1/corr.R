corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  #setwd("specdata")
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  id = 1:332
  data <- data.frame()
  corr <- numeric()
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
  #  if(sum(complete.cases(temp)) > threshold)
   # {
  #    data <- rbind(data,temp)
   # }
    if(sum(complete.cases(temp)) > threshold)
    {
      corr <- c(corr,cor(temp$sulfate,temp$nitrate,use="pairwise.complete.obs"))
    }
  }
  corr
  ## Return a numeric vector of correlations
  ## NOTE: Do not round the result!
}