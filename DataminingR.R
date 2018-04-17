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
    print(ggplot(mydata[mydata$variable=="mood", ], aes(weekday, value)) +
            geom_violin(aes(fill = weekday)))
    
    #plot of the time of day
    mydata$PartOfDay <- factor(mydata$partOfDay, levels=PartsOfTheDay)
    mydata<-mydata[order(mydata$partOfDay), ]
    boxplot(value~partOfDay,data=mydata[mydata$variable=="mood", ], main="",
            xlab="Part of the Day", ylab="Mood",col="darkorchid")
    #extra violinPlot
    print(ggplot(mydata[mydata$variable=="mood", ], aes(partOfDay, value)) +
            geom_violin(aes(fill = partOfDay)))
  }
}

PreprocessData<-function(mydata){#preprocesses for the learning algorithm
  uniqueUsers<-unique(mydata$id)
  mydata<-mydata[order(mydata$time),] #order by time just to be sure
  day<-as.POSIXct(Sys.Date())
  id<-c()
  moodList <- mydata[mydata$variable=="mood",]
  #notMoodList <- mydata[mydata$variable!="mood",]
  counter<-0
  for(user in uniqueUsers){
    tempDays <- unique(moodList[moodList$id==user,]$day)
    #notTempDays<-as.character(unique(notMoodList[notMoodList$id==user,]$day))
    #print(notTempDays)
    for(i in 6:length(tempDays)){
      #if(! (as.character(tempDays[i]) %in% notTempDays)){print("only mood")}
      counter<- counter+1
      day[counter]<-tempDays[i]
      id[counter]<-user
    }
  }
  return(data.frame(id,day))
}


#Benchmark algorithms ##
BenchmarkPreprocess<-function(mydata){ #only get the mean mood of the previous recorded day
  
  uniqueUsers<-unique(mydata$id)
  
  mydata<-mydata[order(mydata$time),] #order by time just to be sure
  prevMood<-c()#colnames(list())<-c("prevMood","todayMood")
  todayMood<-c()
  difference<-c()
  moodList <- mydata[mydata$variable=="mood",]
  counter<-0
  for(user in uniqueUsers){
    dataForUser<-moodList[moodList$id==user,]
    tempDays <- unique(dataForUser$day)
    for(i in 2:length(tempDays)){
      #chance to skip days here when too much space between (might be prepreprocess though)
      if(TRUE){
        counter<- counter+1
        prevMood[counter]<-mean(dataForUser[dataForUser$day== tempDays[i-1],]$value)
        todayMood[counter]<-mean(dataForUser[dataForUser$day== tempDays[i],]$value)
        difference[counter]<-abs(prevMood[counter]-todayMood[counter])
      }
    }
  }
  return(data.frame(prevMood,todayMood,difference))
}

Benchmark<-function(mydata, benchSwitch){
  if(benchSwitch)
  {
    benchData<-BenchmarkPreprocess(mydata)
    print(summary(benchData$difference))
    print(stat.desc(benchData$difference))
    #print(benchData)
  }
}

#Normal algorithms ##
MultinomPreprocess<-function(mydata, chosenDays){
  uniqueUsers<-unique(mydata$id)
  
  mydata<-mydata[order(mydata$time),] #order by time just to be sure
  prevMood<-c()#colnames(list())<-c("prevMood","todayMood")
  todayMood<-c()
  difference<-c()
  moodList <- mydata[mydata$variable=="mood",]
  counter<-0
  for(user in uniqueUsers){
    dataForUser<-moodList[moodList$id==user,]
    tempDays <- unique(dataForUser$day)
    for(i in length(tempDays)){
        counter<- counter+1
        prevMood[counter]<-mean(dataForUser[dataForUser$day== tempDays[i-1],]$value)
        todayMood[counter]<-mean(dataForUser[dataForUser$day== tempDays[i],]$value)
        difference[counter]<-abs(prevMood[counter]-todayMood[counter])
    }
  }
}

MultinomLearning<-function(mydata, chosenDays, MultinomSwitch){
  if(normalSwitch)
  {
    MultinomData<-MultinomPreprocess(mydata,chosenDays)
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

  
  #constants (often for sorting purposes)
  DaysOfTheWeek<<-c( "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
  DaysOfTheWeekend<<-c("Saturday", "Sunday")
  PartsOfTheDay<<-c("Morning","Afternoon","Evening","Night")
  
  #descision variables
  plot <- FALSE #whether it should plot
  init <- FALSE #whether we should still initialize
  reload <- FALSE # reloads everything
  benchSwitch <- FALSE #whether the benchmark should be runned
  MultinomSwitch<- TRUE #whether the normal should be runned
  
  
    
  #Check if the Data has already been read otherwise it will load
  if(!exists("mydata")||is.null(mydata)||reload){
    init=TRUE
    mydata <- read.csv("dataset_mood_smartphone.csv")
  }
  #preprocesses all data 
  if(init){
    mydata$time<-strptime(mydata$time, "%Y-%m-%d %H:%M:%OS")
    mydata$day<-strptime(mydata[,3], "%Y-%m-%d")
    
    mydata$hourOfDay<-format(mydata$time, format="%H")
    mydata$timeOfDay<-format(mydata$time, format="%H:%M:%S")
    
    mydata$weekday<-weekdays(mydata$day)
    mydata$partOfDay <- AssignPartOfDay(mydata$hourOfDay)
  }

  
  #Begin the sorting and plotting
  ReconPlot(mydata, plot)
  
  #begin the prediction algorithms
  Benchmark(mydata, benchSwitch)

  chosenDays<-PreprocessData(mydata)# variable to select the days we want to use per users (in case of too big differences)
  #print(chosenDays)
  MultinomLearning(mydata,chosenDays, MultinomSwitch)
  
  print("FINISHED")
}

Main()
