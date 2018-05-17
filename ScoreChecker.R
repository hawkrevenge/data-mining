while(!require('gdata'))
  install.packages('gdata')

DataCorrect <- as.data.frame(fread("CorrectScores.csv", header=TRUE))
DataGuess <- as.data.frame(fread("FileName.csv", header=TRUE)) #GOEDE FILENAAM HIER
DataGuessDict<-c()
search_IDs <- unique(DataCorrect[,1])
now=Sys.time()
SCORE<- 0
for(ID in search_IDs)
{
  CorrectDataByID<- DataCorrect[DataCorrect[,1]==ID,]
  GuessDataByID<- DataGuess[DataGuess[,1]==ID,]
  DataGuess<- DataGuess[-1:-length(CorrectDataByID),]
  counter <- 1
  for(dataItem in DataGuess)
  {
    if(dataItem[4]==1)
      SCORE<- SCORE+5/log2(1+counter)
    else if(dataItem[3]==1)
      SCORE<- SCORE+1/counter
    counter<-counter+1/log2(1+counter)
  }
}
print(Sys.time()-now)
print(SCORE)