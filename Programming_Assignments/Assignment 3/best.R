best <- function(state,outcome)
{
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  if(any(data$State == state) == TRUE)
  {
    data1 <- subset(data,State==state)
    data1[,11] <- as.numeric(data1[,11])
    data1[,17] <- as.numeric(data1[,17])
    data1[,23] <- as.numeric(data1[,23])
    if(outcome == "heart attack")
    {
      data2 <- subset(data1, data1[,11] == min(data1[,11],na.rm = TRUE))
      data2 <- data2[order(data2$Hospital.Name),]
      print(data2$Hospital.Name[1])
    }
    else if(outcome == "heart failure")
    {
      data2 <- subset(data1, data1[,17] == min(data1[,17],na.rm = TRUE))
      data2 <- data2[order(data2$Hospital.Name),]
      print(data2$Hospital.Name[1])
    }
    else if(outcome == "pneumonia")
    {
      data2 <- subset(data1, data1[,23] == min(data1[,23],na.rm = TRUE))
      data2 <- data2[order(data2$Hospital.Name),]
      print(data2$Hospital.Name[1])
    }
    else
    {
      stop("invalid outcome")  
    }
  }
  else
  {
    stop("invalid state")
  }
}