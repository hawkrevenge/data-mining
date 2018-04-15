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
  
}

BenchmarkPreprocess<-function(mydata){ #only get the mean mood of the previous recorded day
  
  uniqueUsers<-unique(mydata$id)
  
  mydata[order(mydata$time),] #order by time just to be sure
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
    print(attributes(benchData$difference))
    print(stat.desc(benchData$difference))
    #print(benchData)
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
  init <- TRUE #whether we should still initialize
  reload <- TRUE # reloads everything
  benchSwitch <- TRUE #whether the benchmark should be runned
  
  
    
  #Check if the Data has already been read otherwise it will load
  if(!exists("mydata")||is.null(mydata)||reload){
    init=TRUE
    mydata <- read.csv("dataset_mood_smartphone.csv")
  }
  #preprocesses all data 
  if(init){
    mydata$day<-strptime(mydata[,3], "%Y-%m-%d")
    
    mydata$timeOfDay<-strptime(mydata$time, "%Y-%m-%d %H:%M:%OS")
    mydata$hourOfDay<-format(mydata$timeOfDay, format="%H")
    mydata$timeOfDay<-format(mydata$timeOfDay, format="%H:%M:%S")
    
    mydata$weekday<-weekdays(mydata$day)
    mydata$partOfDay <- AssignPartOfDay(mydata$hourOfDay)
  }
  

  
  #Begin the sorting and plotting
  ReconPlot(mydata, plot)
  
  #begin the prediction algorithms
  Benchmark(mydata, benchSwitch)
  
  
  print("FINISHED")
}

Main()
