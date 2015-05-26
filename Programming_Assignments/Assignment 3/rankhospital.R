rankhospital <- function(state, outcome, num = "best")
{
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  if(num == "best")
  {
    num1 = 1;
  }
  else if(num == "worst")
  {
    num1 = nrow(data);
  }
  else
  {
    num1 = as.numeric(num)
  }
  if(any(data$State == state) == TRUE)
  {
    data1 <- subset(data,State==state)
    data1[,11] <- as.numeric(data1[,11])
    data1[,17] <- as.numeric(data1[,17])
    data1[,23] <- as.numeric(data1[,23])
    if(outcome == "heart attack")
    {
      data1 <- data1[order(data1[,11],data1[,2]),]
      if(num == "worst")
      {
        data2 <- subset(data1, data1[,11] == max(data1[,11],na.rm = TRUE))
        data2 <- data2[order(data2$Hospital.Name),]
        print(data2$Hospital.Name[1])
      }
      else
      {
        print(data1$Hospital.Name[num1])
      }      
    }
    else if(outcome == "heart failure")
    {
      data1 <- data1[order(data1[,17],data1[,2]),]
      if(num == "worst")
      {
        data2 <- subset(data1, data1[,17] == max(data1[,17],na.rm = TRUE))
        data2 <- data2[order(data2$Hospital.Name),]
        print(data2$Hospital.Name[1])
      }
      else
      {
        print(data1$Hospital.Name[num1])
      }  
    }
    else if(outcome == "pneumonia")
    {
      data1 <- data1[order(data1[,23],data1[,2]),]
      if(num == "worst")
      {
        data2 <- subset(data1, data1[,23] == max(data1[,23],na.rm = TRUE))
        data2 <- data2[order(data2$Hospital.Name),]
        print(data2$Hospital.Name[1])
      }
      else
      {
        print(data1$Hospital.Name[num1])
      }  
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