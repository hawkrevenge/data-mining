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

################### MAIN #######################
Main<-function(){
  
  #Required packages
  while(!require('ggplot2'))
    install.packages('ggplot2')
  while(!require('forecast'))
    install.packages('forecast')
  while(!require('tseries'))
    install.packages('tseries')  
  
  #constants (often for sorting purposes)
  DaysOfTheWeek<<-c( "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
  DaysOfTheWeekend<<-c("Saturday", "Sunday")
  PartsOfTheDay<<-c("Morning","Afternoon","Evening","Night")
  
  #descision variables
  plot <- TRUE #whether it should plot
  init <- TRUE #whether we should still initialize
  reload <- TRUE # reloads everything
  

    
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
}

Main()
