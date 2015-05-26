rankall <- function(outcome, num = "best")
{
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  if(num == "best")
  {
    num1 = 1;
  }
  else
  {
    num1 = as.numeric(num)
  }
  
  s <- unique(data[,7])
  s <- s[order(s)]
  name <- character()
  
  data[,11] <- as.numeric(data[,11])
  data[,17] <- as.numeric(data[,17])
  data[,23] <- as.numeric(data[,23])
  
  for(i in 1:length(s))
  {
    data1 <- subset(data,data[,7]==s[i])    
    
    if(outcome == "heart attack")
    {
      data2 <- data1[order(data1[,11],data1[,2],na.last = NA),]
      if(num == "worst")
      {
        data2 <- subset(data2, data2[,11] == max(data2[,11],na.rm = TRUE))
        name <- c(name,data2$Hospital.Name[1])
      }
      else
      {
        name <- c(name,data2$Hospital.Name[num1])
      }
    }
    else if(outcome == "heart failure")
    {
      data2 <- data1[order(data1[,17],data1[,2],na.last = NA),]
      if(num == "worst")
      {
        data2 <- subset(data2, data2[,17] == max(data2[,17],na.rm = TRUE))
        name <- c(name,data2$Hospital.Name[1])
      }
      else
      {
        name <- c(name,data2$Hospital.Name[num1])
      }
    }
    else if(outcome == "pneumonia")
    {
      data2 <- data1[order(data1[,23],data1[,2],na.last = NA),]
      if(num == "worst")
      {
        data2 <- subset(data2, data2[,23] == max(data2[,23],na.rm = TRUE))
        name <- c(name,data2$Hospital.Name[1])
      }
      else
      {
        name <- c(name,data2$Hospital.Name[num1])
      }
    }
    else
    {
      stop("invalid outcome")  
    }
  }
  d <- data.frame(name,s)
  colnames(d) <- c("hospital","state")
  d
}