################ FUNCTIONS ###################
#return a vector with the correct part of the day
AssignPartOfDay<-function(vctr) 
{
  returnvctr<-c()
  chosenTime = ""
  for(i in 1:length(vctr)){
    if(as.integer(vctr[i])<6){
      chosenTime = "Night"
    }
    else if(as.integer(vctr[i])<12){
      chosenTime = "Morning"
    }
    else if(as.integer(vctr[i])<18){
      chosenTime = "Afternoon"
    }
    else {
      chosenTime = "Evening"
    }
    returnvctr[i]<-chosenTime
  }
  return(returnvctr)
}


#RMSE FUNCTION
RMSE <- function(error)
{
  sqrt(mean(error^2))
}

#sorts the list by a column of ones choice
Sort<-function(unsorted, sortBy, level=NULL){
  if(!is.null(level))
    {sortBy<- factor(sortBy, levels=level)}
  return(unsorted[order(sortBy), ])
}

#function for the first plots
ReconPlot<-function(mydata, plot){
  if(plot){
    print("Begin Plotting")
    #plot of the weekdays:
    mydata$weekday <- factor(mydata$weekday, levels=DaysOfTheWeek)
    mydata<-mydata[order(mydata$weekday), ]
    boxplot(value~weekday,data=mydata[mydata$variable=="mood", ], main="",
            xlab="Day of the Week", ylab="Mood", col="deepskyBlue")
    #extra violinPlot
    print(ggplot(mydata[mydata$variable=="mood", ], aes(weekday, value)) +
            geom_violin(aes(fill = weekday)))
    
    #plot of the time of day
    mydata$partOfDay <- factor(mydata$partOfDay, levels=PartsOfTheDay)
    mydata<-mydata[order(mydata$partOfDay), ]
    boxplot(value~partOfDay,data=mydata[mydata$variable=="mood", ], main="",
            xlab="Part of the Day", ylab="Mood",col="darkorchid")
    #extra violinPlot
    print(ggplot(mydata[mydata$variable=="mood", ], aes(partOfDay, value)) +
            geom_violin(aes(fill = partOfDay)))
  }
}

ChooseData<-function(mydata){#preprocesses for the learning algorithm
  uniqueUsers<-unique(mydata$id)
  mydata<-mydata[order(mydata$time),] #order by time just to be sure
  day<-as.POSIXct(Sys.Date())
  daySpot<-c()
  daysBackMood<-c()
  daysBackActual<-c()
  actualDaySpot<-c()
  id<-c()
  moodList <- mydata[mydata$variable=="mood",]
  counter<-0
  testCounter<-0
  for(user in uniqueUsers){
    tempDays <- unique(moodList[moodList$id==user,]$day)
    tempAllDays<- as.character(unique(mydata[mydata$id==user,]$day))
    for(i in 5:length(tempDays)){
      skip<-FALSE
      counter<- counter+1
      for(j in 4:5){
        if(difftime(tempDays[i],tempDays[i-j], units="days")<=5)
        {
          daysBackMood[counter]<-j
          daysBackActual[counter]<-grep(as.character(tempDays[i]),tempAllDays)-grep(as.character(tempDays[i-j]),tempAllDays)
        }
        else{
          if(j==4)
            skip<-TRUE
          break
        }
        if(i-j==1)
          break
      }
      if(skip){
        counter<- counter-1
        next
      }
      else{
        testCounter<-testCounter+1
      }
      day[counter]<-tempDays[i]
      daySpot[counter]<-i #place of the day in the tempDays(!!MUST BE ORDERED TO WORK!!)
      id[counter]<-user
      actualDaySpot[counter]<-grep(as.character(tempDays[i]),tempAllDays)
    }
  }
  return(data.frame(id,day,daySpot,daysBackMood,daysBackActual,actualDaySpot))
}


#Benchmark algorithms ##
BenchmarkPreprocess<-function(mydata, chosenDays){ #only get the mean mood of the previous recorded day
  
  uniqueUsers<-unique(chosenDays$id)
  
  mydata<-mydata[order(mydata$time),] #order by time just to be sure
  chosenDays<-chosenDays[order(chosenDays$day),]
  prevMood<-c()#colnames(list())<-c("prevMood","todayMood")
  todayMood<-c()
  difference<-c()
  moodList <- mydata[mydata$variable=="mood",]
  counter<-0
  for(user in uniqueUsers){
    dataForUser<-moodList[moodList$id==user,]
    tempDays <- unique(dataForUser$day)
    chosenDaysUser<-chosenDays[chosenDays$id==user,]
    for(i in 2:length(tempDays)){
      counter<-counter+1
      prevMood[counter]<-mean(dataForUser[dataForUser$day== tempDays[i-1],]$value)
      todayMood[counter]<-mean(dataForUser[dataForUser$day== tempDays[i],]$value)
      difference[counter]<-abs(prevMood[counter]-todayMood[counter])
    }
  }
  return(data.frame(prevMood,todayMood,difference))
}

Benchmark<-function(mydata, benchSwitch, chosenDays){
  if(benchSwitch)
  {
    benchData<-BenchmarkPreprocess(mydata,chosenDays)
    print("BENCHMARK")  
    
    print("RMSE")
    print(RMSE(benchData$difference))
    #print(summary(benchData$difference))
    #print(stat.desc(benchData$difference))
    #print(benchData)
  }
}

#Multinom algorithms ##
MultinomPreprocess<-function(mydata, chosenDays){
  print("MULTINOMPREPROCESS")
  uniqueUsers<-unique(chosenDays$id)
  
  mydata<-mydata[order(mydata$time),] #order by time just to be sure
  #chosenDays<-chosenDays[order(chosenDays$day),]
  timenow<-Sys.time()
  #all used variables
  prevMood<-c()
  prevMood1<-c()
  prevMood2<-c()
  prevMood3<-c()
  prevMood4<-c()
  prevMood5<-c()
  todayMood<-c()
  
  prevVal<-c()
  todVal<-c()
  prevAr<-c()
  todAr<-c()
  prevAc<-c()
  todAc<-c()
  prevEntGame<-c()
  todEntGame<-c()
  meanVal<-mean(mydata[mydata$variable=="circumplex.valence",]$value)
  count<-c()
  prevValence<-c()
  todayValence<-c()
  meanMood<-c()
  id<-c()
  moodList <- mydata[mydata$variable=="mood",]
  counter<-0
  testcounter<-0
  for(user in uniqueUsers){
    chosenDaysUser<-chosenDays[chosenDays$id==user,]
    dataForUserMood<-moodList[moodList$id==user,]
    dataForUser<-mydata[mydata$id==user,]
    tempDaysMood <- unique(dataForUserMood$day)
    tempDays<-unique(dataForUser$day)
    for(i in 1:length(chosenDaysUser$day)){
        counter<- counter+1
        count<-append(count,counter)
        id<-append(id,user)
        tempMood<-c()
        now<-chosenDaysUser$daySpot[i]
        nowActual<-chosenDaysUser$actualDaySpot[i]
        #Interpoleren van de mood als een dag mist, It ain't pretty but who cares
        for( j in 1:chosenDaysUser$daysBackMood[i]){
          tempMood<-append(tempMood,mean(dataForUserMood[dataForUserMood$day==tempDaysMood[now-j],]$value))
        }
        if(length(dataForUserMood[dataForUserMood$day==tempDays[nowActual-1],]$value)>0)
          prevMood1<-append(prevMood1,mean(dataForUserMood[dataForUserMood$day==tempDays[nowActual-1],]$value))
        else
          prevMood1<-append(prevMood1,mean(dataForUserMood[dataForUserMood$day==tempDays[nowActual-2],]$value))
        if(length(dataForUserMood[dataForUserMood$day==tempDays[nowActual-2],]$value)>0)
          prevMood2<-append(prevMood2,mean(dataForUserMood[dataForUserMood$day==tempDays[nowActual-2],]$value))
        else
          prevMood2<-append(prevMood2,(mean(dataForUserMood[dataForUserMood$day==tempDays[nowActual-1],]$value)+
                                     mean(dataForUserMood[dataForUserMood$day==tempDays[nowActual-3],]$value))/2)
        if(length(dataForUserMood[dataForUserMood$day==tempDays[nowActual-3],]$value)>0)
          prevMood3<-append(prevMood3,mean(dataForUserMood[dataForUserMood$day==tempDays[nowActual-3],]$value))
        else
          prevMood3<-append(prevMood3,mean(c(mean(dataForUserMood[dataForUserMood$day==tempDays[nowActual-2],]$value),
                                     mean(dataForUserMood[dataForUserMood$day==tempDays[nowActual-4],]$value))))
        if(length(dataForUserMood[dataForUserMood$day==tempDays[nowActual-4],]$value)>0)
          prevMood4<-append(prevMood4,mean(dataForUserMood[dataForUserMood$day==tempDays[nowActual-4],]$value))
        else
          prevMood4<-append(prevMood4,mean(c(mean(dataForUserMood[dataForUserMood$day==tempDays[nowActual-3],]$value),
                                     mean(dataForUserMood[dataForUserMood$day==tempDays[nowActual],]$value))))
        if(length(dataForUserMood[dataForUserMood$day==tempDays[nowActual-5],]$value)>0)
          prevMood5<-append(prevMood5,mean(dataForUserMood[dataForUserMood$day==tempDays[nowActual-5],]$value))
        else
          prevMood5<-append(prevMood5,mean(dataForUserMood[dataForUserMood$day==tempDays[nowActual-4],]$value))
        prevMood[counter]<-mean(tempMood)
        todayMood[counter]<-mean(dataForUserMood[dataForUserMood$day== tempDaysMood[now],]$value)
        meanMood[counter]<-mean(dataForUserMood[dataForUserMood$day<tempDaysMood[now],]$value)
        
        tempValence<-c()
        valenceData<-dataForUser[dataForUser$variable=="circumplex.valence"&complete.cases(dataForUser$value),]
        counterj<-0
        for( j in 1:chosenDaysUser$daysBackActual[i]){
          counterj<-1+counterj
          tempVal<-valenceData[valenceData$day==tempDays[nowActual-j],]$value
            if(length(tempVal)>0)
              tempValence[counterj]<-mean(tempVal)
            else
              counterj<-counterj-1
        }
        if(length(tempValence)>0)
        {
          prevValence[counter]<-mean(tempValence)
        }
        else
          prevValence[counter]<-0
        if(length(valenceData[valenceData$day==tempDays[nowActual],]$value)>0)
          todayValence[counter]<-mean(valenceData[valenceData$day==tempDays[nowActual],]$value)
        else{
          testcounter<-testcounter+1
          todayValence[counter]<-0
        }
        
        tempAr<-c()
        arData<-dataForUser[dataForUser$variable=="circumplex.arousal"&complete.cases(dataForUser$value),]
        counterj<-0
        for( j in 1:chosenDaysUser$daysBackActual[i]){
          counterj<-1+counterj
          tempVal<-arData[arData$day==tempDays[nowActual-j],]$value
          if(length(tempVal)>0)
            tempAr[counterj]<-mean(tempVal)
          else
            counterj<-counterj-1
        }
        if(length(tempAr)>0)
        {
          prevAr[counter]<-mean(tempAr)
        }
        else
          prevAr[counter]<-0
        if(length(arData[arData$day==tempDays[nowActual],]$value)>0)
          todAr[counter]<-mean(arData[arData$day==tempDays[nowActual],]$value)
        else{
          todAr[counter]<-0
        }
        
        tempAc<-c()
        acData<-dataForUser[dataForUser$variable=="activity"&complete.cases(dataForUser$value),]
        counterj<-0
        for( j in 1:chosenDaysUser$daysBackActual[i]){
          counterj<-1+counterj
          tempVal<-acData[acData$day==tempDays[nowActual-j],]$value
          if(length(tempVal)>0)
            tempAc[counterj]<-mean(tempVal)
          else
            counterj<-counterj-1
        }
        if(length(tempAc)>0)
        {
          prevAc[counter]<-mean(tempAc)
        }
        else
          prevAc[counter]<-0
        if(length(acData[acData$day==tempDays[nowActual],]$value)>0)
          todAc[counter]<-mean(acData[acData$day==tempDays[nowActual],]$value)
        else{
          todAc[counter]<-0
        }
        
        tempEntGame<-c()
        entGameData<-dataForUser[(dataForUser$variable=="appCat.entertainment"|dataForUser$variable=="appCat.game")
                                 &dataForUser$value>0&dataForUser$value<5000,]
        counterj<-0
        for( j in 1:chosenDaysUser$daysBackActual[i]){
          counterj<-1+counterj
          tempVal<-entGameData[entGameData$day==tempDays[nowActual-j],]$value
          if(length(tempVal)>0)
            tempEntGame[counterj]<-sum(tempVal)
          else
            counterj<-counterj-1
        }
        if(length(tempEntGame)>0)
        {
          prevEntGame[counter]<-mean(tempEntGame)
        }
        else
          prevEntGame[counter]<-0
        if(length(entGameData[entGameData$day==tempDays[nowActual],]$value)>0)
          todEntGame[counter]<-sum(entGameData[entGameData$day==tempDays[nowActual],]$value)
        else
          todEntGame[counter]<-0
    }
  }
  print(Sys.time()-timenow)
  print("Making frame")
  return(data.frame(id,prevMood1,prevMood2,prevMood3,prevMood4,prevMood5,prevMood,todayMood,prevValence,todEntGame,todAr,todAc, meanMood,todayValence,count))
}

MultinomLearning<-function(mydata, chosenDays, MultinomSwitch){
  if(MultinomSwitch)
  {
    multinomData<-MultinomPreprocess(mydata,chosenDays)
    print("SAMPLING")
    differenceLm1<-c()
    differenceLm2<-c()
    uniqueUsers<-unique(multinomData$id)
    #Lm function
    #for(user in uniqueUsers)
    #{
    #  userData<-multinomData[multinomData$id==user,]
    #  funclm1<- lm(todayMood~ prevMood1+prevMood2+prevMood3+prevMood4+prevMood5, data=userData)
    #  predlm1 <- predict(funclm1, userData)
    #  differenceLm1<-c(differenceLm1,(abs(predlm1-multinomData$todayMood)))
    #  
    #  funclm2<- lm(todayMood~ prevMood1+prevMood2+prevMood3+prevMood4+prevMood5+prevValence+todayValence+meanMood, data=userData)
    #  predlm2 <- predict(funclm2, userData)
    #  differenceLm2<-c(differenceLm2,(abs(predlm2-multinomData$todayMood)))
    #}
    
    totalMeanListlm1<-c()
    totalMeanListlm2<-c()
    totalMeanListpolr<-c()
    totalMeanListmulti<-c()
    for(n in 1:5000){
      folds<-createFolds(multinomData$count)
      meanlm1<-c()
      meanlm2<-c()
      meanpolr<-c()
      meanMulti<-c()
      if(n%%50==0)
      print(n)
      for(i in 1:10)
      {
        valList<-multinomData[folds[[i]],]
        testList<-multinomData[!multinomData$count%in%folds[[i]],]
        
        #funclm1<- lm(todayMood~ prevMood1+prevMood2+prevMood3+prevMood4+prevMood5, data=testList)
        #predlm1 <- predict(funclm1, valList)
        #differencelm1<-abs(predlm1-valList$todayMood)
        #meanlm1<-append(meanlm1,RMSE(differencelm1))
        
        funclm2<- lm(todayMood~ prevMood1+prevMood2+prevMood3+prevMood4+prevMood5+prevValence+todayValence+meanMood+todEntGame+todAr+todAc, data=testList)
        predlm2 <- predict(funclm2, valList)
        differencelm2<-abs(predlm2-valList$todayMood)
        meanlm2<-append(meanlm2,RMSE(differencelm2))
        
        #polr function
        #funcpolr <- polr(as.factor(todayMood)~ prevMood1+prevMood2+prevMood3+prevMood4+prevMood5+prevValence+todayValence+meanMood, data=testList)
        #predpolr <- predict(funcpolr, valList)
        #meanpolr<-append(meanpolr,RMSE((abs(as.numeric(levels(predpolr))[predpolr]-multinomData$todayMood))))
        
        #multinom function
        #funcmult <- multinom(as.factor(todayMood)~ prevMood1+prevMood2+prevMood3+prevMood4+prevMood5+prevValence+todayValence+meanMood, data=testList, trace=FALSE)
        #predmult <- predict(funcmult, valList)
        #meanMulti<-append(meanMulti,RMSE((abs(as.numeric(levels(predmult))[predmult]-multinomData$todayMood))))
      }
      #totalMeanListlm1<-append(totalMeanListlm1,mean(meanlm1))
      totalMeanListlm2<-append(totalMeanListlm2,mean(meanlm2))
      
      #totalMeanListpolr<-append(totalMeanListpolr,mean(meanpolr))
      #totalMeanListmulti<-append(totalMeanListmulti,mean(meanMulti))
    }
    print("LM")
    #print(totalMeanListlm2)
    #rounded<-round(totalMeanListlm2,3)
    hist(totalMeanListlm2, 
         main="RMSE of the linear regression", 
         xlab="RMSE", 
         border="black", 
         col="red",
         xlim=c(0.388,0.398),
         las=1, 
         breaks=40)
    print(min(totalMeanListlm2))
    print(max(totalMeanListlm2))
    print(stat.desc(totalMeanListlm2))
    #print(totalMeanListpolr)
    #print(totalMeanListmulti)
    #print(RMSE(differenceLm1))
    #print(RMSE(differencelm1))
    #print(RMSE(differenceLm2))
    #print(RMSE(differencelm2))
    #print(stat.desc(differencelm1))
    #print(stat.desc(differencelm2))
    #print(summary(funclm1))
    #print(summary(funclm2))
    
    #print("POLR")
    #print(stat.desc(differencepolr))
    #print("MULTINOM")  
    #print(stat.desc(differencemult))
    #print(summary(funcmult))
  }
}


################### MAIN #######################
Main<-function(){
  #Required packages
  while(!require('ggplot2'))
    install.packages('ggplot2')
  while(!require('forecast'))
    install.packages('forecast')
  while(!require('tseries'))
    install.packages('tseries') 
  while(!require('pastecs'))
    install.packages('pastecs')
  while(!require('nnet'))
    install.packages('nnet')
  while(!require('MASS'))
    install.packages('MASS')
  while(!require('caret'))
    install.packages('caret')  
  
  #constants (often for sorting purposes)

  #descision variables
  plot <- FALSE #whether it should plot
  init <- FALSE #whether we should still initialize
  reload <- FALSE # reloads everything
  benchSwitch <- TRUE #whether the benchmark should be runned
  MultinomSwitch<- TRUE #whether the normal should be runned
  
  
    
  #Check if the Data has already been read otherwise it will load
  if(!exists("mydata")||is.null(mydata)||reload){
    print("Load")
    init=TRUE
    mydata <- read.csv("dataset_mood_smartphone.csv")
    
  }
  #preprocesses all data 
  if(init){
    print("Init")
    mydata$time<-strptime(mydata$time, "%Y-%m-%d %H:%M:%OS")
    mydata$day<-strptime(mydata[,3], "%Y-%m-%d")
    
    mydata$hourOfDay<-format(mydata$time, format="%H")
    mydata$timeOfDay<-format(mydata$time, format="%H:%M:%S")
    
    mydata$weekday<-weekdays(mydata$day)
    mydata$partOfDay <- AssignPartOfDay(mydata$hourOfDay)
  }
  
  
  #Begin the sorting and plotting
  ReconPlot(mydata, plot)
  
  chosenDays<-ChooseData(mydata)# variable to select the days we want to use per users (in case of too big differences)
  
  #begin the prediction algorithms
  Benchmark(mydata, benchSwitch, chosenDays)
  
  #print(chosenDays)
  MultinomLearning(mydata, chosenDays, MultinomSwitch)
  
  print("FINISHED")
}

DaysOfTheWeek<<-c( "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
DaysOfTheWeekend<<-c("Saturday", "Sunday")
PartsOfTheDay<<-c("Morning","Afternoon","Evening","Night")
Main()
