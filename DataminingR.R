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
            xlab="Part of the Day", ylab="Mood", col="deepskyBlue")
    #extra violinPlot
    #print(ggplot(mydata[mydata$variable=="mood", ], aes(weekday, value)) +
     #       geom_violin(aes(fill = weekday)))
    
    #plot of the time of day
    #mydata$PartOfDay <- factor(mydata$partOfDay, levels=PartsOfTheDay)
    #mydata<-mydata[order(mydata$partOfDay), ]
    #boxplot(value~partOfDay,data=mydata[mydata$variable=="mood", ], main="",
    #        xlab="Part of the Day", ylab="Mood",col="darkorchid")
    #extra violinPlot
    #print(ggplot(mydata[mydata$variable=="mood", ], aes(partOfDay, value)) +
     #       geom_violin(aes(fill = partOfDay)))
  }
}

ChooseData<-function(mydata){#preprocesses for the learning algorithm
  uniqueUsers<-unique(mydata$id)
  mydata<-mydata[order(mydata$time),] #order by time just to be sure
  day<-as.POSIXct(Sys.Date())
  daySpot<-c()
  daysBackMood<-c()
  daysBackActual<-c()
  id<-c()
  moodList <- mydata[mydata$variable=="mood",]
  counter<-0
  for(user in uniqueUsers){
    tempDays <- unique(moodList[moodList$id==user,]$day)
    tempAllDays<- as.character(unique(mydata[mydata$id==user,]$day))
    for(i in 6:length(tempDays)){
      skip<-FALSE
      counter<- counter+1
      for(j in 3:5){
        if(difftime(tempDays[i],tempDays[i-j], units="days")<=10)
        {
          daysBackMood[counter]<-j
          daysBackActual[counter]<-grep(as.character(tempDays[i]),tempAllDays)-grep(as.character(tempDays[i-j]),tempAllDays)
        }
        else{
          if(j==3)
            skip<-TRUE
          break
        }
      }
      if(skip){
        next
        counter<- counter-1
      }
      day[counter]<-tempDays[i]
      daySpot[counter]<-i #place of the day in the tempDays(!!MUST BE ORDERED TO WORK!!)
      id[counter]<-user
    }
  }
  return(data.frame(id,day,daySpot,daysBackMood,daysBackActual))
}


#Benchmark algorithms ##
BenchmarkPreprocess<-function(mydata, chosenDays){ #only get the mean mood of the previous recorded day
  
  uniqueUsers<-unique(mydata$id)
  
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
    for(i in 1:length(chosenDaysUser)){
      counter<- counter+1
      now<-chosenDaysUser$daySpot[i]
      prevMood[counter]<-mean(dataForUser[dataForUser$day== tempDays[now-1],]$value)
      todayMood[counter]<-mean(dataForUser[dataForUser$day== tempDays[now],]$value)
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
    
    print(summary(benchData$difference))
    print(stat.desc(benchData$difference))
    #print(benchData)
  }
}

#Multinom algorithms ##
MultinomPreprocess<-function(mydata, chosenDays){
  uniqueUsers<-unique(chosenDays$id)
  
  mydata<-mydata[order(mydata$time),] #order by time just to be sure
  chosenDays<-chosenDays[order(chosenDays$day),]
  
  #all used variables
  prevMood<-c()
  prevMood1<-c()
  prevMood2<-c()
  prevMood3<-c()
  todayMood<-c()
  
  moodList <- mydata[mydata$variable=="mood",]
  counter<-0
  for(user in uniqueUsers){
    chosenDaysUser<-chosenDays[chosenDays$id==user,]
    dataForUserMood<-moodList[moodList$id==user,]
    tempDaysMood <- unique(dataForUserMood$day)
    for(i in 1:length(chosenDaysUser)){
        counter<- counter+1
        tempMood<-c()
        now<-chosenDaysUser$daySpot[i]
        for( j in 1:chosenDaysUser$daysBackMood[i]){
          tempMood[j]<-mean(dataForUserMood[dataForUserMood$day==tempDaysMood[now-j],]$value)
        }
        prevMood1[counter]<-mean(dataForUserMood[dataForUserMood$day==tempDaysMood[now-1],]$value)
        prevMood2[counter]<-mean(dataForUserMood[dataForUserMood$day==tempDaysMood[now-2],]$value)
        prevMood3[counter]<-mean(dataForUserMood[dataForUserMood$day==tempDaysMood[now-3],]$value)
        prevMood[counter]<-mean(tempMood)
        todayMood[counter]<-mean(dataForUserMood[dataForUserMood$day== tempDaysMood[now],]$value)
    }
  }
  return(data.frame(prevMood1,prevMood2,prevMood3,prevMood,todayMood))
}

MultinomLearning<-function(mydata, chosenDays, MultinomSwitch){
  if(MultinomSwitch)
  {
    multinomData<-MultinomPreprocess(mydata,chosenDays)
    
    #Lm function
    funclm<- lm(todayMood~ prevMood1+prevMood2+prevMood3, data=multinomData)
    predlm <- predict(funclm, multinomData)
    differencelm<-(abs(predlm-multinomData$todayMood))
    
    #polr function
    funcpolr <- polr(as.factor(todayMood)~ prevMood1+prevMood2+prevMood3, data=multinomData)
    predpolr <- predict(funcpolr, multinomData)
    differencepolr<-(abs(as.numeric(levels(predpolr))[predpolr]-multinomData$todayMood))
    
    #multinom function
    funcmult <- multinom(as.factor(todayMood)~ prevMood1+prevMood2+prevMood3, data=multinomData)
    predmult <- predict(funcmult, multinomData)
    differencemult<-(abs(as.numeric(levels(predmult))[predmult]-multinomData$todayMood))
    
    print("LM")
    print(stat.desc(differencelm))
    print(summary(funclm))
    print("POLR")
    print(stat.desc(differencepolr))
    print("MULTINOM")  
    print(stat.desc(differencemult))
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
  
  
  #constants (often for sorting purposes)
  DaysOfTheWeek<<-c( "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
  DaysOfTheWeekend<<-c("Saturday", "Sunday")
  PartsOfTheDay<<-c("Morning","Afternoon","Evening","Night")
  
  #descision variables
  plot <- TRUE #whether it should plot
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
Main()
