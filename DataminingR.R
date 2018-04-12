while(!require('ggplot2'))
  install.packages('ggplot2')
while(!require('forecast'))
  install.packages('forecast')
while(!require('tseries'))
  install.packages('tseries')


if(!exists("mydata")||is.null(mydata)){
  mydata <- read.csv("dataset_mood_smartphone.csv")
}
mydata$day<-strptime(mydata[,3], "%Y-%m-%d")

mydata$timeOfDay<-strptime(mydata$time, "%Y-%m-%d %H:%M:%OS")
mydata$timeOfDay<-format(mydata$timeOfDay, format="%H:%M:%S")

mydata$weekday<-weekdays(mydata$day)
DaysOfTheWeek<-c( "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
DaysOfTheWeekend<-c("Saturday", "Sunday")


mydata$weekday <- factor(mydata$weekday, levels= DaysOfTheWeek)
mydata<-mydata[order(mydata$weekday), ]


#boxplot(value~weekday,data=mydata[mydata$variable=="mood", ], main="Mood", xlab="Day of the Week", ylab="Mood")

#print(DaysOfTheWeek)
