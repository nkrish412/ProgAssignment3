RankHospital<-function(State,outcome,Rank)
{
 f8<-read.csv("outcome-of-care-measures.csv",colClasses="character")
 

if (class(Rank) != "numeric")
{
  if (Rank != "Best" && Rank != "Worst")
  {
#    message("NA")
    return("NA")
  }
  Rank1 <- 1
}
else
{
  Rank1 <- Rank
}

if ((outcome != "Heart Failure") && (outcome != "Heart Attack") && (outcome != "Pneumonia"))
{
  message("Invalid outcome")
  return()
}
 if (nrow(f8[f8$State==State,])==0)
{
  return("Invalid State")
}

  Revorderby = FALSE
 if (Rank == "Worst" )
{
  Revorderby = TRUE 
}

f8[,11]<-suppressWarnings(as.numeric(f8[,11]))
f8[,17]<-suppressWarnings(as.numeric(f8[,17]))
f8[,23]<-suppressWarnings(as.numeric(f8[,23]))
 f9<-f8[,c(2,7,11,17,23)]

if (outcome == "Heart Failure")
{
 f5<-f9[!is.na(f9[,3])&f9$State==State,]
 f5<-f5[order(f5[,3],decreasing = Revorderby),]
 if (Rank1 > NROW(f5)) return("NA")
 x<-f5[Rank1,]
 x<-x[,c(1,3)]
} 
if (outcome == "Heart Attack")
{
 f5<-f9[!is.na(f9[,4])&f9$State==State,]
 f5<-f5[order(f5[,4],decreasing = Revorderby),]
 if (Rank1 > NROW(f5)) return("NA")
 ;x<-head(f5,Rank1)
x<-f5[Rank1,] 
x<-x[,c(1,4)]
} 
if (outcome == "Pneumonia")
{
f5<-f9[!is.na(f9[,5])&f9$State==State,]
 f5<-f5[order(f5[,5],decreasing = Revorderby),]
 if (Rank1 > NROW(f5)) return("NA")
 ;x<-head(f5,Rank1)
x<-f5[Rank1,] 
x<-x[,c(1,5)]
} 
return(x)
}
