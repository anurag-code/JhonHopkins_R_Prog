rankall<- function(outcome,num="best"){
  
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
  
  
  # Total nos of Unique States
  t<-unique(sdata$State)
  t<-sort(t)
  len<-length(t)
  
  # Loop for all states
  # initialize a data frame
  rank_vec<-data.frame()
  
 
  
  for (i in 1:len) {
    
  
  ## Take only those rows wcich have the required state value	
  d1 <- sdata[sdata$State==t[i],]
  d1<-d1[c("h.name",outcome)]
  
  
  
  
  ## Sorting
  
  sort_data<- d1[order(d1[outcome],d1["h.name"]),]
  sort_data<- na.omit(sort_data)
  leng<-nrow(sort_data)
  
  # condition for best and worst
  if(num=="worst")
    numb=leng
  
  else
    
    if(num=="best")
      numb=1
  else 
    numb=num
  
 
 ## Data frame input
  rank_vec[i,1]<- t[i]
  rank_vec[i,2]<- sort_data[numb,1]
  }
  rank_vec
}