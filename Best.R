best<-function(State,outcome)
{
 f8<-read.csv("outcome-of-care-measures.csv",colClasses="character")
 f8[,11]<-as.numeric(f8[,11])
f8[,17]<-as.numeric(f8[,17])
f8[,23]<-as.numeric(f8[,23])
 f9<-f8[,c(2,7,11,17,23)]

if ((outcome != "Heart Failure") && (outcome != "Heart Attack") && (outcome != "Pneumonia"))
{
  message("Invalid outcome")
  return
}
 if (nrow(f8[f8$State==State,])==0)
{
  message("Invalid State")
  return
}
if (outcome == "Heart Failure")
{
 f5<-f9[!is.na(f9[,3])&f9$State==State,]
 f5<-f5[order(f5[,3]),]
 x<-head(f5,1)
 x<-x[,c(1,3)]
return(x) 
} 
if (outcome == "Heart Attack")
{
 f5<-f9[!is.na(f9[,4])&f9$State==State,]
 f5<-f5[order(f5[,4]),]
 x<-head(f5,1)
 x<-x[,c(1,4)]
return(x) 
} 
if (outcome == "Pneumonia")
{
f5<-f9[!is.na(f9[,5])&f9$State==State,]
 f5<-f5[order(f5[,5]),]
 x<-head(f5,1)
 x<-x[,c(1,5)]
return(x) 
} 

}