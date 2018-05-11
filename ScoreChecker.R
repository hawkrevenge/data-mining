while(!require('gdata'))
  install.packages('gdata')

DataCorrect <- as.data.frame(fread("CorrectScores.csv", header=TRUE))
DataGuess <- as.data.frame(fread("FileName.csv", header=TRUE)) #GOEDE FILENAAM HIER

search_IDs <- unique(DataCorrect[,1])
SCORE<-0
for(ID in search_IDs)
{
  CorrectDataByID<- DataCorrect[DataCorrect[,1]==ID,]
  GuessDataByID<- DataGuess[DataGuess[,1]==ID,]
  counter <- 1
  while(TRUE)
  {
    if(length(CorrectDataByID[CorrectDataByID[,4]==1,2])==0)
      break
    if(!GuessDataByID[counter,2] %in% CorrectDataByID[CorrectDataByID[,4]==1,2])
      break
    counter<-counter+1
    CorrectDataByID<-CorrectDataByID[-1,]
    SCORE<- SCORE+5
  }
  while(TRUE)
  {
    if(length(CorrectDataByID[CorrectDataByID[,3]==1,2])==0)
      break
    if(!GuessDataByID[counter,2] %in% CorrectDataByID[CorrectDataByID[,3]==1,2])
      break
    counter<-counter+1
    SCORE<- SCORE+1
  }
}
print(SCORE)