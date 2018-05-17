while(!require('gdata'))
  install.packages('gdata')

DataCorrect <- as.data.frame(fread("CorrectScores.csv", header=TRUE))
DataGuess <- as.data.frame(fread("FileName.csv", header=TRUE)) #GOEDE FILENAAM HIER
DataGuessDict<-c()
search_IDs <- unique(DataCorrect[,1])
now=Sys.time()
SCORE<- 0
NDCG<-c()
n<-0
for(ID in search_IDs)
{
  n<-n+1
  CorrectDataByID<- DataCorrect[DataCorrect[,1]==ID,]
  GuessDataByID<- DataGuess[DataGuess[,1]==ID,]
  DataGuess<- DataGuess[-1:-length(GuessDataByID),]
  counter <- 1
  booking<-length(CorrectDataByID[CorrectDataByID[,4]==1,])
  clicks<-length(CorrectDataByID[CorrectDataByID[,3]==1,])-booking
  counter2 <- 1
  if(booking>0)
  {
    counter2<-2
    BestScore<-5
  }
  else
    counter2<-1
  for(i in 1:clicks)
  {
    BestScore<-BestScore+1/log2(1+counter2)
    counter2<-counter2+1
  }
  
  for(dataItem in DataGuess)
  {
    if(dataItem[4]==1)
      SCORE<- SCORE+5/log2(1+counter)
    else if(dataItem[3]==1)
      SCORE<- SCORE+1/counter
    counter<-counter+1
  }
  NDCG[n]<-SCORE/BestScore
}
SCORE<-SCORE/length(search_IDs)
print(Sys.time()-now)
print(SCORE)