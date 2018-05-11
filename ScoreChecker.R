while(!require('gdata'))
  install.packages('gdata')

DataCorrect <- as.data.frame(fread("CorrectScores.csv", header=TRUE))
DataGuess <- as.data.frame(fread("FileName.csv", header=TRUE)) #GOEDE FILENAAM HIER
DataGuessDict<-c()
search_IDs <- unique(DataCorrect[,1])
names(DataGuessDict)<-names(search_IDs)
print(DataGuessDict)
SCORE<-0
now=Sys.time()
for(ID in search_IDs[1:100000])
{
  break
  #print(ID)
  CorrectDataByID<- DataCorrect[DataCorrect[,1]==ID,]
  GuessDataByID<- DataGuess[DataGuess[,1]==ID,]
  
  #timecheck van 100000 items:
                                                                        #Zonder is 3.212 min
  #DataGuess<- DataGuess[!DataGuess[,1]==ID,]                           #met is 1.79
  DataGuess<- DataGuess[-1:-length(CorrectDataByID),]                  #met is 1.698
  #DataGuess<- DataGuess[length(CorrectDataByID)+1:length(DataGuess),]   #met is 3.147
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
    if(!GuessDataByID[counter,2] %in% CorrectDataByID[,2])
      break
    counter<-counter+1
    SCORE<- SCORE+1
  }
}
print(Sys.time()-now)
print(SCORE)