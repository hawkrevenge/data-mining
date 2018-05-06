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

FindAttribute<-function(mydata, attributefind){
  if(attributefind){
    # merge databse
    install.packages("ggpubr")
    library("ggpubr")
    #arousal
      mydata_mood <- mydata[mydata$variable=="mood",]
      mydata_arousal <- mydata[mydata$variable=="circumplex.arousal",]
      total_arousal <- merge(mydata_mood, mydata_arousal, by=c("id","time"))
      boxplot(value.x~value.y, data=total_arousal,  main="",
              xlab="Arousal", ylab="Mood", col="deepskyBlue",cex.axis=0.8)
      res <- cor.test(total_arousal$value.x,total_arousal$value.y, method = "pearson")
      res
      #valence
      mydata_valence <- mydata[mydata$variable=="circumplex.valence",]
      total_valence <- merge(mydata_mood, mydata_valence, by=c("id","time"))
      means <- aggregate(value.x~value.y, total_valence, mean)
      boxplot(value.x~value.y, data=total_valence, main="",
              xlab="Valence", ylab="Mood", col="deepskyBlue", cex.axis=0.8)
      points(1:5, means$value.x, col = "red")
      
      res <- cor.test(total_valence$value.x,total_valence$value.y, method = "pearson")
      res
      # activity score  (signi)
      mydata_activity <- mydata[mydata$variable == "activity",]
      mydata_activity <- merge(mydata_mood, mydata_activity, by=c("id","time"))
      
      plot(value.x~value.y, data=mydata_activity, main="",
              xlab="Activity", ylab="Mood", col="deepskyBlue", cex.axis=0.8)
      res <- cor.test(mydata_activity$value.x,mydata_activity$value.y, method = "pearson")
      res
      
      # ratio finance+office / social app (niet signi)
      moodList <- mydata[mydata$variable=="mood",]
      moodList_aggregate<-aggregate(x=moodList[,5],by=list(id=moodList[,2] , day=as.character(moodList[,6])), FUN=mean)
      financeList <- mydata[mydata$variable=="appCat.finance",]
      financeList_aggregate <- aggregate(x=financeList[,5],by=list(id=financeList[,2] , day=as.character(financeList[,6])), FUN=mean)
      moodfinance <- merge(moodList_aggregate, financeList_aggregate, by=c("id","day"))
      officeList <-mydata[mydata$variable=="appCat.office",]
      colnames(moodfinance)[colnames(moodfinance)=="x.x"] <- "mood"
      colnames(moodfinance)[colnames(moodfinance)=="x.y"] <- "finance"
      officeList_aggregate <- aggregate(x=officeList[,5],by=list(id=officeList[,2] , day=as.character(officeList[,6])), FUN=mean)
      moodfinance <- merge(moodfinance, officeList_aggregate, by=c("id","day"))
      colnames(moodfinance)[colnames(moodfinance)=="x"] <- "office"
      moodfinance$sum <- moodfinance$finance+moodfinance$office
      moodfinance <- moodfinance[moodfinance$sum<1500,]
      socialList <-mydata[mydata$variable=="appCat.social",]
      socialList_aggregate <- aggregate(x=socialList[,5],by=list(id=socialList[,2] , day=as.character(socialList[,6])), FUN=mean)
      moodfinance <- merge(moodfinance, socialList_aggregate, by=c("id","day"))
      colnames(moodfinance)[colnames(moodfinance)=="x"] <- "social"
      moodfinance$ratio <- moodfinance$sum/moodfinance$social
      plot(ratio~mood, data=moodfinance, main="",
           xlab="Activity", ylab="Mood", col="deepskyBlue", cex.axis=0.8)
      res <- cor.test(moodfinance$ratio,moodfinance$mood, method = "pearson")
      res
      
      
      # call + sms made (both)
      callList <- mydata[mydata$variable=="call",]
      callList_aggregate <- aggregate(x=callList[,5],by=list(id=callList[,2] , day=as.character(callList[,6])), FUN=mean)
      moodcall <- merge(moodList_aggregate, callList_aggregate, by=c("id","day"))
      smsList<- mydata[mydata$variable=="sms",]
      smsList_aggregate <- aggregate(x=smsList[,5],by=list(id=smsList[,2] , day=as.character(smsList[,6])), FUN=mean)
      moodcall <- merge(moodcall, smsList_aggregate, by=c("id","day"))
      
      # duration game app (niet signi)
      gameList <- mydata[mydata$variable=="appCat.game",]
      gameList_aggregate <- aggregate(x=gameList[,5],by=list(id=gameList[,2] , day=as.character(gameList[,6])), FUN=mean)
      moodgame <- merge(moodList_aggregate, gameList_aggregate, by=c("id","day"))
      moodgame <-moodgame[moodgame$x.y<1500,]
      plot(x.y~x.x,data=moodgame,main="")
      res <- cor.test(moodgame$x.y,moodgame$x.x, method = "pearson")
      res
      
      # game / social (niet signi)
      moodgame <- merge(moodgame, socialList_aggregate, by=c("id","day"))
      moodgame$ratiogamesocial <- moodgame$x.y/moodgame$x
      moodgame<- moodgame[moodgame$ratiogamesocial<40,]
      plot(ratiogamesocial~x.x,data=moodgame,main="")
      res <- cor.test(moodgame$ratiogamesocial,moodgame$x.x, method = "pearson")
      res
          
      # entertainment + game (signi)
      entertainmentList <- mydata[mydata$variable=="appCat.entertainment",]
      entertainmentList_aggregate <- aggregate(x=entertainmentList[,5],by=list(id=entertainmentList[,2] , day=as.character(entertainmentList[,6])), FUN=mean)
      moodfun <-  merge(moodList_aggregate, gameList_aggregate, by=c("id","day"))
      moodfun <- merge(moodfun, entertainmentList_aggregate, by=c("id","day"))
      moodfun$sum <- moodfun$x.y + moodfun$x
      res <- cor.test(moodfun$sum,moodfun$x.x, method = "pearson")
      res
      
      # finance + office / screen (niet signi)
      moodList <- mydata[mydata$variable=="mood",]
      moodList_aggregate<-aggregate(x=moodList[,5],by=list(id=moodList[,2] , day=as.character(moodList[,6])), FUN=mean)
      financeList <- mydata[mydata$variable=="appCat.finance",]
      financeList_aggregate <- aggregate(x=financeList[,5],by=list(id=financeList[,2] , day=as.character(financeList[,6])), FUN=mean)
      moodfinance <- merge(moodList_aggregate, financeList_aggregate, by=c("id","day"))
      officeList <-mydata[mydata$variable=="appCat.office",]
      colnames(moodfinance)[colnames(moodfinance)=="x.x"] <- "mood"
      colnames(moodfinance)[colnames(moodfinance)=="x.y"] <- "finance"
      officeList_aggregate <- aggregate(x=officeList[,5],by=list(id=officeList[,2] , day=as.character(officeList[,6])), FUN=mean)
      moodfinance <- merge(moodfinance, officeList_aggregate, by=c("id","day"))
      colnames(moodfinance)[colnames(moodfinance)=="x"] <- "office"
      moodfinance$sum <- moodfinance$finance+moodfinance$office
      moodfinance <- moodfinance[moodfinance$sum<1500,]
      screenList <- mydata[mydata$variable=="screen",]
      screenList_aggregate <- aggregate(x=screenList[,5],by=list(id=screenList[,2] , day=as.character(screenList[,6])), FUN=mean)
      moodfinance <- merge(moodfinance, screenList_aggregate, by=c("id","day"))
      colnames(moodfinance)[colnames(moodfinance)=="x"] <- "screen"
      moodfinance$ratioscreen <- moodfinance$sum/moodfinance$screen
      res <- cor.test(moodfinance$ratioscreen,moodfinance$mood, method = "pearson")
      res
       # travel + social + game + entertainmant /  screen (niet signi)
      entertainmentList <- mydata[mydata$variable=="appCat.entertainment",]
      entertainmentList_aggregate <- aggregate(x=entertainmentList[,5],by=list(id=entertainmentList[,2] , day=as.character(entertainmentList[,6])), FUN=mean)
      moodfun <-  merge(moodList_aggregate, gameList_aggregate, by=c("id","day"))
      moodfun <- merge(moodfun, entertainmentList_aggregate, by=c("id","day"))
      moodfun$sum <- moodfun$x.y + moodfun$x
      colnames(moodfun)[colnames(moodfun)=="x.x"] <- "mood"
      colnames(moodfun)[colnames(moodfun)=="x.y"] <- "game"
      colnames(moodfun)[colnames(moodfun)=="x"] <- "entertainment"
      moodfun <- merge(moodfun, screenList_aggregate, by=c("id","day"))
      moodfun$ratio <- moodfun$sum/moodfun$x  
      res <- cor.test(moodfun$ratio,moodfun$x, method = "pearson")
      res
    
  }
}



#function for the first plots
ReconPlot<-function(mydata, plot){
  if(plot){

     
    print("Begin Plotting")
    #plot of the weekdays:
    #mydata$weekday <- factor(mydata$weekday, levels=DaysOfTheWeek)
    #mydata<-mydata[order(mydata$weekday), ]
    boxplot(value~weekday,data=mydata[mydata$variable=="mood", ], main="",
            xlab="Part of the Day", ylab="Mood", col="deepskyBlue", las=2, cex.axis=0.5)
    #extra violinPlot
    print(ggplot(mydata[mydata$variable=="mood", ], aes(weekday, value)) +
            geom_violin(aes(fill = weekday)))
    
    #plot of the time of day
    #mydata$PartOfDay <- factor(mydata$partOfDay, levels=PartsOfTheDay)
    #mydata<-mydata[order(mydata$partOfDay), ]
    boxplot(value~partOfDay,data=mydata[mydata$variable=="mood", ], main="",
            xlab="Part of the Day", ylab="Mood",col="darkorchid", las=2, cex.axis=0.5)
    #extra violinPlot
    print(ggplot(mydata[mydata$variable=="mood", ], aes(partOfDay, value)) +
            geom_violin(aes(fill = partOfDay)))
    
    # weekenddays
    boxplot(value~weekend,data=mydata[mydata$variable=="mood", ], main="",
            xlab="Weekend", ylab="Mood",col="darkorchid", las=2, cex.axis=0.5)
    means <- tapply(newdata1$value,newdata1$weekend,mean)
    points(means,col="red",pch=18)
    #extra violinPlot
    print(ggplot(mydata[mydata$variable=="mood", ], aes(weekend, value)) +
            geom_violin(aes(fill = weekend)) + labs(x="Weekend", y = "Mood")+stat_summary(fun.y=mean, colour="darkred", geom="point", 
                                                         shape=18, size=3,show_guide = FALSE))
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
  testCounter<-0
  for(user in uniqueUsers){
    tempDays <- unique(moodList[moodList$id==user,]$day)
    tempAllDays<- as.character(unique(mydata[mydata$id==user,]$day))
    for(i in 5:length(tempDays)){
      skip<-FALSE
      counter<- counter+1
      for(j in 4:5){
        (print)
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
    
    print("RMSE")
    print(RMSE(benchData$difference))
    print(summary(benchData$difference))
    print(stat.desc(benchData$difference))
    #print(benchData)
  }
}

#Multinom algorithms ##
MultinomPreprocess<-function(mydata, chosenDays){
  uniqueUsers<-unique(chosenDays$id)
  
  mydata<-mydata[order(mydata$time),] #order by time just to be sure
  #chosenDays<-chosenDays[order(chosenDays$day),]
  
  #all used variables
  prevMood<-c()
  prevMood1<-c()
  prevMood2<-c()
  prevMood3<-c()
  todayMood<-c()
  prevValence<-c()
  todayValence<-c()

  moodList <- mydata[mydata$variable=="mood",]
  counter<-0
  for(user in uniqueUsers){
    #print(user)
    chosenDaysUser<-chosenDays[chosenDays$id==user,]
    dataForUserMood<-moodList[moodList$id==user,]
    dataForUser<-mydata[mydata$id==user,]
    tempDaysMood <- unique(dataForUserMood$day)
    tempDays<-unique(dataForUser$day)
    for(i in 1:length(chosenDaysUser$day)){
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
        userid[counter]<- chosenDays$id
          
        tempValence<-c()
        valenceData<-dataForUser[dataForUser$variable=="circumplex.valence"&complete.cases(dataForUser$value),]
        counterj<-0
        for( j in 1:chosenDaysUser$daysBackActual[i]){
          counterj<-1+counterj
          tempVal<-valenceData[valenceData$day==tempDays[now-j],]$value
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
          prevValence[counter]<-(0)
        if(length(valenceData[valenceData$day==tempDays[now],]$value)>0)
          todayValence[counter]<-mean(valenceData[valenceData$day==tempDays[now],]$value)
        else
          todayValence[counter]<-0
    }
  }
  return(data.frame(prevMood1,prevMood2,prevMood3,prevMood,todayMood,prevValence,todayValence))
}

MultinomLearning<-function(mydata, chosenDays, MultinomSwitch){
  if(MultinomSwitch)
  {
    multinomData<-MultinomPreprocess(mydata,chosenDays)
    print(dim(multinomData))
    #Lm function
    funclm1<- lm(todayMood~ prevMood1+prevMood2+prevMood3, data=multinomData)
    predlm1 <- predict(funclm1, multinomData)
    differencelm1<-(abs(predlm1-multinomData$todayMood))
    
    funclm2<- lm(todayMood~ prevMood1+prevMood2+prevMood3+prevValence+todayValence, , data=multinomData)
    predlm2 <- predict(funclm2, multinomData)
    differencelm2<-(abs(predlm2-multinomData$todayMood))
    
    #polr function
    funcpolr <- polr(as.factor(todayMood)~ prevMood1+prevMood2+prevMood3+prevValence+todayValence, data=multinomData)
    predpolr <- predict(funcpolr, multinomData)
    differencepolr<-(abs(as.numeric(levels(predpolr))[predpolr]-multinomData$todayMood))
    
    #multinom function
    funcmult <- multinom(as.factor(todayMood)~ prevMood1+prevMood2+prevMood3+prevValence+todayValence, data=multinomData)
    predmult <- predict(funcmult, multinomData)
    differencemult<-(abs(as.numeric(levels(predmult))[predmult]-multinomData$todayMood))
    
    print("LM")
    print(stat.desc(differencelm1))
    print(stat.desc(differencelm2))
    print(summary(funclm1))
    print(summary(funclm2))
    
    #print("POLR")
    #print(stat.desc(differencepolr))
    #print("MULTINOM")  
    #print(stat.desc(differencemult))
    #print(summary(funcmult))
  }
}

arimafunction<-function(mydata, chosenDays, ArimaSwitch){
  if(ArimaSwitch)
  {
    # install.packages("urca")
    # library("urca")
    # install.packages("dynlm")
    # library("dynlm")
    users <- c("AS14.01", "AS14.17", "AS14.08", "AS14.13", "AS14.16", "AS14.05", "AS14.15", "AS14.12", "AS14.02", "AS14.07", "AS14.14", "AS14.20", "AS14.30", "AS14.03","AS14.09", "AS14.19", "AS14.23", "AS14.26", "AS14.06", "AS14.28", "AS14.32", "AS14.29", "AS14.27", "AS14.25", "AS14.24", "AS14.33", "AS14.31")
    p <- c()
    scatterresult <- c()
    stationary <- c()
    for (user in users){
    moodList_aggregate<-aggregate(x=moodList[,5],by=list(id=moodList[,2] , day=as.character(moodList[,6])), FUN=mean)
    moodList_aggregate_user<- moodList_aggregate[moodList_aggregate$id== toString(user),]
    moodList_aggregate_user$day <- as.Date(moodList_aggregate_user$day)
    print(paste("the user is", user))
    print(ggplot(moodList_aggregate_user,aes(x=day, y=x,group = 1))+geom_line() + expand_limits(y = 0)+
          geom_point() + labs(x="Day", y = "Mood")+scale_x_date(date_labels = "%b-%Y",  date_breaks="1 month"))
    
    
    fulldata <- data.frame(seq(min(moodList_aggregate_user$day), max(moodList_aggregate_user$day), by="1 day"))
    colnames(fulldata) <- "day"
    moodList_aggregate_user <- merge(fulldata, moodList_aggregate_user, by=c("day"), all.x=TRUE)
    moodList_aggregate_user$weekday<-weekdays(moodList_aggregate_user$day)
    moodList_aggregate_user$weekend <- ifelse(weekdays(moodList_aggregate_user$day) %in% c("zaterdag", "zondag"), "weekend", "weekday")
    moodList_aggregate_user$id <- user
    
    for(i in 1:nrow(moodList_aggregate_user)) 
    {
      if (is.na(moodList_aggregate_user$x[i]))
      {
        if(!is.na(moodList_aggregate_user$x[i-1]) & !is.na(moodList_aggregate_user$x[i+1])) {
          moodList_aggregate_user$x[i] <- (moodList_aggregate_user$x[i-1] + moodList_aggregate_user$x[i+1])/2
          # print(paste("insert",i))
        }
        if(is.na(moodList_aggregate_user$x[i-1]) | is.na(moodList_aggregate_user$x[i+1]) ){
          # print(paste("split komt eraan ",i))
        if(!is.na(moodList_aggregate_user$x[i+1]) & i!=41){
          moodList_aggregate_user <- moodList_aggregate_user[-c(1:i), ]
          print("split")
        #   print(paste("split komt eraan ",i))
        # }
        #   else{
        #     if(i<nrow(moodList_aggregate_user)/2){
        #       moodList_aggregate_user <- moodList_aggregate_user[-c(1:i), ]
        #     }
        #     else{
        #       moodList_aggregate_user <- moodList_aggregate_user[-c(:nrow(moodList_aggregate_user)), ]
        #     }
        #   }
        }
          if(!is.na(moodList_aggregate_user$x[i+1]) & i==41){
            moodList_aggregate_user <- moodList_aggregate_user[-c(40:nrow(moodList_aggregate_user)), ]
            print("split")
          }
        }
        
      }
    }
    for(i in 1:nrow(moodList_aggregate_user)) 
    {
      if (is.na(moodList_aggregate_user$x[i]))
      {
        moodList_aggregate_user$x[i] <- (moodList_aggregate_user$x[i-1] + moodList_aggregate_user$x[i+1])/2
      }
      }
    
    
    station <- adf.test(moodList_aggregate_user$x, alternative = "stationary", k=0)
    print(paste("pvalue,",station$p.value))
    stationary[length(stationary)+1] <- station$p.value
    #x=ur.df(moodList_aggregate_user$x,type="drift",selectlags = "AIC")
    #print(summary(x))
    count_d1 = diff(moodList_aggregate_user$x, differences = 1)
    #plot(count_d1)
    #station1 <-adf.test(count_d1, alternative = "stationary")
    #stationary[length(stationary)+1] <- station1$p.value

 
   Acf(count_d1, main='ACF for Differenced Series')
    Pacf(count_d1, main='PACF for Differenced Series')

    model <- auto.arima(moodList_aggregate_user$x,d = 1)
    print(model)
    require(forecast)
    serie <- c()
    serie <- moodList_aggregate_user$x
    series <- list()
    series[[1]] <- ts(serie, frequency=7)
    holdout <- round(length(moodList_aggregate_user$x)*0.10)
    forecasts <- lapply(series,function(foo) { 
      subseries <- ts(head(foo,length(foo)-holdout),start=start(foo),frequency=frequency(foo)) 
      forecast(auto.arima(subseries,d=0),h=holdout)
    })
    result <- mapply(FUN=accuracy,f=forecasts,x=series,SIMPLIFY=FALSE)
    print(sapply(result,"[","Test set","RMSE"))
    scatterresult[length(scatterresult)+1]=sapply(result,"[","Test set","RMSE")
    
    
    
  }
    # continue user AS14.31 auto.arima(subseries)
    
    
  }
  hist(scatterresult, 
       main="RMSE of the ARIMA regression", 
       xlab="RMSE", 
       border="black", 
       col="red",
       xlim=c(0.0,1.4),
       las=1, 
       breaks=9)
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
  ArimaSwitch <- TRUE #whether arima calculations should be runned
  attributefind <- TRUE # whether want to find new attributes
  
    
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
    mydata$weekend <- ifelse(weekdays(mydata$day) %in% c("zaterdag", "zondag"), "weekend", "weekday")
    
    # delete negative time variables
    mydata_new <- subset(mydata,(variable=="appCat.game" |variable=="appCat.finance" | variable=="appCat.entertainment" | variable=="appCat.communication" | variable=="appCat.builtin" | variable=="appCat.other" | variable=="appCat.social" | variable=="appCat.travel" | variable=="appCat.unknown" | variable=="appCat.utilities" | variable=="appCat.weather"   ) & (value<0 | value > 20000 ))
  }
  
  # data aggregate
  
  
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
