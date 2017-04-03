best<- function(state,outcome){
  
  # read data # data is still with NAs
  data<- read.csv("outcome-of-care-measures.csv",colClasses = "character")
  sdata<-data[,c(2,7,11,17,23)]
  names(sdata)<- c("h.name","State","heart attack","heart failure","pneumonia")
  
  # validating Argument outcome
  if (outcome== "heart attack")
    sdata$"heart attack"<-as.numeric(sdata$"heart attack")
  else 
    if (outcome== "heart failure")
      sdata$"heart failure"<-as.numeric(sdata$"heart failure")
  else 
    if (outcome== "pneumonia")
      sdata$"pneumonia"<-as.numeric(sdata$"pneumonia")
  else
    stop("invalid Outcome")
  
  # validating Argument State
  y<- (sdata$State == state)
  if (sum(y) == 0) 
  stop("invalid State")
  
  ## Take only those rows with have the required state value	
  sdata <- sdata[sdata$State==state & sdata[outcome] != 'Not Available', ]
  vals <- sdata[, outcome]
  rowNum <- which.min(vals)
  ## Return hospital name in that state with lowest 30-day death rate
  sdata[rowNum,1]
}