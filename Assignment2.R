############################################
## Datamining VU - Expedia 
############################################

# Libraries
Main<-function(){
  while(!require('data.table'))
    install.packages('data.table')
  while(!require('ggplot2'))
    install.packages('ggplot2')
  while(!require('dplyr'))
    install.packages('dplyr')
  while(!require('emil'))
    install.packages('emil')
  while(!require('magrittr'))
    install.packages('magrittr')
  while(!require('purrrlyr'))
    install.packages('purrrlyr')
  while(!require('MASS'))
    install.packages('MASS')
  while(!require('sjlabelled'))
    install.packages('sjlabelled')
  while(!require('tidyr'))
    install.packages('tidyr')
  while(!require('xgboost'))
    install.packages('xgboost')
  while(!require('caret'))
    install.packages('caret')
  while(!require('gbm'))
    install.packages('gbm')

  
  # Set switches 
  loadingSwitch <- FALSE
  exploreSwitch <- FALSE
  preprocSwitch <- TRUE
  
  loading(loadingSwitch)
  exploring(exploreSwitch)

# preproc function requires the switch, the dataframe you wish to process and the name of dataframe you wish to store the processed dataframe in
# input_test is the dataframe you whish to preproces
# name_new_dataframe is the name of the new dataframe
  now<-Sys.time()
preproc(preprocSwitch, expedia.data, expedia.dataproc)
print(Sys.time()-now)
}

############################################
## Loading dataset for each user
############################################

loading<-function(loadingSwitch){
  if(loadingSwitch)
  {
datatraining.folder  <<- 
  if(grepl("Michael", getwd())) {
    "C:/Users/Michael"
  } else if(grepl("Roel", getwd())) {
    "training_set_VU_DM_2014.csv"
  } else if(grepl("Emma", getwd())) {
    "/Users/Emma/Downloads/Data Mining VU data/training_set_VU_DM_2014.csv"
  }


datatest.folder  <<- 
  if(grepl("Michael", getwd())) {
    "C:/Users/Michael"
  } else if(grepl("Roel", getwd())) {
    "test_set_VU_DM_2014.csv"
  } else if(grepl("Emma", getwd())) {
    "/Users/Emma/Downloads/Data Mining VU data/test_set_VU_DM_2014.csv"
  }

expedia.data <<- fread(datatraining.folder, header=TRUE, na.strings=c("","NULL","NA"))       # 31 seconde bij Emma, 6 bij Roel
expedia.test <<- fread(datatest.folder, header=TRUE, na.strings=c("","NA"))   # 28 seconde bij Emma, 11 bij Roel
input_data <<- expedia.data[sample(nrow(expedia.data), 100000), ] 
input_test <<- expedia.data[sample(nrow(expedia.data), 1000),]
  }
}


############################################
## Exploring dataset
############################################

exploring<-function(exploringSwitch){
  if(exploringSwitch)
  {
# number of unique properties/id in data
expedia.data %>%
  group_by(prop_id) %>%
  summarise(n_distinct(srch_id))

expedia.data %>%
  group_by(srch_id) %>%
  summarise(n_distinct(prop_id))

# number of unique properties/id in test
expedia.test %>%
  group_by(prop_id) %>%
  summarise(n_distinct(srch_id))

expedia.test %>%
  group_by(srch_id) %>%
  summarise(n_distinct(prop_id))

# length of property id's that are in data set but not in testset 
setdiff(expedia.data$prop_id, expedia.test$prop_id) %>%
  length()
# length of property id's that are in test set but not in trainingset 
setdiff(expedia.test$prop_id, expedia.data$prop_id) %>%
  length()

# count number of prop id per search id's 
prop_per_srch <- expedia.data %>%
  group_by(srch_id) %>%
  summarise(n_distinct(prop_id))

# number of property id's that has at least 1 booking
booking_numb <- aggregate(booking_bool ~ prop_id, data = expedia.data, FUN = sum)
nrow(booking_numb[booking_numb$booking_bool != 0,])
# number of property id's that has at least 1 click
click_numb <-   aggregate(click_bool ~ prop_id, data = expedia.data, FUN = sum)
nrow(click_numb[click_numb$click_bool != 0,])


# Some plots of variables
df <- aggregate( booking_bool ~ site_id, data = expedia.data, FUN = length)
ggplot(df, aes(x=site_id, y=booking_bool)) + geom_col() + theme(axis.text.x = element_text(angle = 90, hjust = 1))

df <- aggregate( booking_bool ~ visitor_location_country_id, data = expedia.data, FUN = length)
ggplot(df, aes(x=visitor_location_country_id, y=booking_bool)) + geom_col() + theme(axis.text.x = element_text(angle = 90, hjust = 1))

df <- aggregate( booking_bool ~ prop_country_id, data = expedia.data, FUN = length)
ggplot(df, aes(x=prop_country_id, y=booking_bool)) + geom_col() + theme(axis.text.x = element_text(angle = 90, hjust = 1))


# see into data
dim(expedia.data)
str(expedia.data)
summary(expedia.data)

# counting missing values each row and plotting it 
df <- sapply(expedia.data, function(x) sum(is.na(x)))
df1 <- data.frame(name = names(df), rank = df) 
df1 <- df1[order(df1$rank),]
df1$rank <- df1$rank / 4958347
df1$name <- factor(df1$name, levels = df1$name[order(df1$rank)])
ggplot(df1, aes(x=name, y=rank)) + geom_col() + theme(axis.text.x = element_text(angle = 90, hjust = 1))


df <- sapply(input_data, function(x) sum(is.na(x)))
df1 <- data.frame(name = names(df), rank = df) 
df1 <- df1[order(df1$rank),]
df1$rank <- df1$rank
df1$name <- factor(df1$name, levels = df1$name[order(df1$rank)])
ggplot(df1, aes(x=name, y=rank)) + geom_col(fill = "#FF6666") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + xlab("Attributes")+ylab("Missing values")

##################################################################################################
# prop_review_score, prop location_score2 , orig_destination_distance
##################################################################################################
# booking
histo <- expedia.data[, c("booking_bool","click_bool", "prop_review_score", "prop_location_score2", "orig_destination_distance")]
histo$review_na_bool <- as.integer(as.logical(is.na(histo$prop_review_score)))
histo$location_na_bool <- as.integer(as.logical(is.na(histo$prop_location_score2)))
histo$orig_na_bool <- as.integer(as.logical(is.na(histo$orig_destination_distance)))

message("Woking on histogram book percentage")
histo2 <- histo[, c("booking_bool", "review_na_bool")]
histo3 <- aggregate(histo2,by=list(histo2$review_na_bool),mean)
histo4 <- histo[, c("booking_bool", "location_na_bool")]
histo5 <- aggregate(histo4,by=list(histo4$location_na_bool),mean)
histo6 <- histo[, c("booking_bool", "orig_na_bool")]
histo7 <- aggregate(histo6,by=list(histo6$orig_na_bool),mean)

histo3$review_na_bool <- histo3$booking_bool
histo3$booking_bool <- histo3$Group.1
histo3$location_na_bool <- histo5$booking_bool
histo3$orig_na_bool <- histo7$booking_bool

histo4 <- histo3[,2:length(histo3)]
histo4.long<-melt(histo4,id.vars="booking_bool")

ggplot(histo4.long,aes(x=variable,y=value,fill=factor(booking_bool)))+
  geom_bar(stat="identity",position="dodge")+
  scale_fill_discrete(name="Booking bool",
                      breaks=c(0, 1),
                      labels=c("Data Available", "No data available"))+
  xlab("Attributes")+ylab("Percentage hotels being booked") +theme(axis.text.x = element_text(angle = 45, hjust = 1))



# clicks
message("Woking on histogram click percentage")
histo2 <- histo[, c("click_bool", "review_na_bool")]
histo3 <- aggregate(histo2,by=list(histo2$review_na_bool),mean)
histo4 <- histo[, c("click_bool", "location_na_bool")]
histo5 <- aggregate(histo4,by=list(histo4$location_na_bool),mean)
histo6 <- histo[, c("click_bool", "orig_na_bool")]
histo7 <- aggregate(histo6,by=list(histo6$orig_na_bool),mean)

histo3$review_na_bool <- histo3$click_bool
histo3$click_bool <- histo3$Group.1
histo3$location_na_bool <- histo5$click_bool
histo3$orig_na_bool <- histo7$click_bool

histo4 <- histo3[,2:length(histo3)]
histo4.long<-melt(histo4,id.vars="click_bool")

ggplot(histo4.long,aes(x=variable,y=value,fill=factor(click_bool)))+
  geom_bar(stat="identity",position="dodge")+
  scale_fill_discrete(name="Click bool",
                      breaks=c(0, 1),
                      labels=c("Data Available", "No data available"))+
  xlab("Attributes")+ylab("Percentage hotels being clicked") +theme(axis.text.x = element_text(angle = 45, hjust = 1))



#library(reshape2)
#ggplot(melt(expedia.data), aes(variable, value)) + geom_boxplot()

#ggplot(stack(expedia.data), aes(x = ind, y = values)) +
 # geom_boxplot()

# What days of the week are people booking?
#expedia.booked$DayOfWeek <- factor(weekdays(as.Date(expedia.booked$date_time)), levels= c("maandag", "dinsdag", "woensdag", "donderdag", "vrijdag", "zaterdag", "zondag"))
#ggplot(data = expedia.booked, aes(expedia.booked$DayOfWeek)) + geom_bar()
  }
}

expedia.data$new <- NULL



############################################
## Preproccess
############################################
preproc<-function(preprocSwitch, input_data, name_new_dataframe){
  if(preprocSwitch)
  {
    message("Working on competitor features...")
    # Count the number of NaN/-1/+1 values across compX_rate
    comp_rate_missing = input_data  %>%
      dplyr::select(ends_with("rate"))%>%
      rowwise()%>%(function(x)(mutate(input_data,
        comp_rate_na=sum(is.na(x)),
        comp_rate_positive=sum(x == 1, na.rm = T),
        comp_rate_negative=sum(x == -1, na.rm = T),
        comp_rate_neutral=sum(x == 0, na.rm = T)))) %>% 
      dplyr::select(starts_with("comp_rate"))
    
    # Count the number of NaN/0/+1 values across compX_inv
    comp_inv_missing = input_data %>% 
      rowwise() %>%
      dplyr::select(ends_with("inv")) %>% 
      (function(x)mutate(input_data,
        comp_inv_na = sum(is.na(x)),
        comp_inv_positive = sum(x == 1, na.rm=T),
        comp_inv_neutral = sum(x == 0, na.rm = T)))%>% 
      dplyr::select(starts_with("comp_inv"))
    
    # Count the number of NaN/mean(abs) values across compX_rate_percent
    comp_rate_percent_missing = input_data %>% rowwise()%>%
      dplyr::select(ends_with("rate_percent_diff")) %>%
      (function(x)mutate(input_data,
                         comp_rate_percent_diff_na = sum(is.na(x))))%>% 
      dplyr::select(starts_with("comp_rate_percent"))
    
    # Cbind the existing data
    input_data = cbind(input_data, comp_rate_missing, comp_inv_missing, comp_rate_percent_missing)
    
    
    #######################################################################################
    # Lower price available 
    #######################################################################################
    
    # make bool if lower price available
    input_data$comp_rate_bool <- as.integer(pmin(1, input_data$comp_rate_positive))
    input_data$comp_inv_bool <- as.integer(pmin(1, input_data$comp_inv_neutral))
    
    histo <- input_data[, c("booking_bool","click_bool", "comp_rate_bool", "comp_inv_bool")]
    histo2 <- histo[, c("booking_bool", "comp_rate_bool")]
    histo3 <- aggregate(histo2,by=list(histo2$comp_rate_bool),mean)
    histo4 <- histo[, c("booking_bool", "comp_inv_bool")]
    histo5 <- aggregate(histo4,by=list(histo4$comp_inv_bool),mean)
    
    histo3$comp_rate_bool <- histo3$booking_bool
    histo3$booking_bool <- histo3$Group.1
    histo3$comp_inv_bool <- histo5$booking_bool
    histo4 <- histo3[,2:length(histo3)]
    histo4.long <- melt(histo4,id.vars="booking_bool")
    
    ggplot(histo4.long,aes(x=variable,y=value,fill=factor(booking_bool)))+
      geom_bar(stat="identity",position="dodge")+
      scale_fill_discrete(name="Availability",
                          breaks=c(0, 1),
                          labels=c("Yes", "No"))+
      xlab("Attributes")+ylab("Percentage hotels being booked") +theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    
    histo2 <- histo[, c("click_bool", "comp_rate_bool")]
    histo3 <- aggregate(histo2,by=list(histo2$comp_rate_bool),mean)
    histo4 <- histo[, c("click_bool", "comp_inv_bool")]
    histo5 <- aggregate(histo4,by=list(histo4$comp_inv_bool),mean)
    
    histo3$comp_rate_bool <- histo3$click_bool
    histo3$click_bool <- histo3$Group.1
    histo3$comp_inv_bool <- histo5$click_bool
    histo4 <- histo3[,2:length(histo3)]
    histo4.long <- melt(histo4,id.vars="click_bool")
    
    ggplot(histo4.long,aes(x=variable,y=value,fill=factor(click_bool)))+
      geom_bar(stat="identity",position="dodge")+
      scale_fill_discrete(name="Availability",
                          breaks=c(0, 1),
                          labels=c("Yes", "No"))+
      xlab("Attributes")+ylab("Percentage hotels being clicked") +theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    
  
    # Message
    message("Finished working on competitor features...")
    
    # - - - - - - - - - - - - - - - 
    # Fix prop review score
    # - - - - - - - - - - - - - - -
    # Set the prop_review_score to the minimum value across all scores
    message("Working on prop_review_score feature...")
    input_data = input_data %>% 
      group_by(srch_destination_id) %>% 
      replace_na(list(prop_review_score = min(.$prop_review_score, na.rm = T))) %>% 
      ungroup()
    
    # - - - - - - - - - - - - - - - 
    # Fix visitor history
    # - - - - - - - - - - - - - - -
    # Replace NA values with the mean starrating across the origin
    message("Working on visior history...")
    input_data = input_data %>% 
      replace_na(list(visitor_hist_starrating = mean(.$visitor_hist_starrating, na.rm = T)))
    input_data$starr_dif <- abs((input_data$visitor_hist_starrating - input_data$prop_starrating))
      
    # Replace NA values with the mean price across the origin
    message("Working on visior history...")
    input_data = input_data %>% 
      replace_na(list(visitor_hist_adr_usd = mean(.$visitor_hist_adr_usd, na.rm = T)))
    input_data$price_dif <- abs((input_data$visitor_hist_adr_usd - input_data$price_usd))

    #### make histogram for booking bool star diff
    histo <- input_data[, c("booking_bool","click_bool", "starr_dif", "price_dif")]
    histo$group_starr_dif <- as.integer(ifelse(histo$starr_dif < 1, "1", ifelse(histo$starr_dif < 2, "2", ifelse(histo$starr_dif < 3, "3", ifelse(histo$starr_dif < 4, "4", ifelse(histo$starr_dif < 5, "5", "6" ) ) ) ) ) ) 
    # histo$group <- as.integer(ifelse(histo$price_quality < 0.10, "0.0-0.1", ifelse(histo$price_quality < 0.2, "0.1-0.2", ifelse(histo$price_quality < 0.3, "0.2-0.3", ifelse(histo$price_quality < 0.4, "0.3-0.4", ifelse(histo$price_quality < 0.5, "0.4-0.5", ifelse(histo$price_quality < 0.6, "0.5-1.0", ifelse(histo$price_quality < 0.7, "0.5-1.0", ifelse(histo$price_quality < 0.8, "0.5-1.0", ifelse(histo$price_quality < 0.9, "0.5-1.0", ifelse(histo$price_quality < 1, "0.5-1.0", "Rank=0" ) ) ) ) ) ) ) ) ) ) )
    histo2 <- histo[, c("booking_bool", "group_starr_dif")]
    histo3 <- aggregate(histo2,by=list(histo2$group_starr_dif),mean)
 
    ggplot(data=histo3, aes(x=group_starr_dif, y=booking_bool)) +
      geom_bar(stat="identity") + scale_x_continuous("PriceQuality",breaks=c(1:5), labels=c("0-1","1-2","2,3","3-4","4-5")) + theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    #### make histogram for click bool star diff
    histo <- input_data[, c("booking_bool","click_bool", "starr_dif", "price_dif")]
    histo$group_starr_dif <- as.integer(ifelse(histo$starr_dif < 1, "1", ifelse(histo$starr_dif < 2, "2", ifelse(histo$starr_dif < 3, "3", ifelse(histo$starr_dif < 4, "4", ifelse(histo$starr_dif < 5, "5", "6" ) ) ) ) ) ) 
    # histo$group <- as.integer(ifelse(histo$price_quality < 0.10, "0.0-0.1", ifelse(histo$price_quality < 0.2, "0.1-0.2", ifelse(histo$price_quality < 0.3, "0.2-0.3", ifelse(histo$price_quality < 0.4, "0.3-0.4", ifelse(histo$price_quality < 0.5, "0.4-0.5", ifelse(histo$price_quality < 0.6, "0.5-1.0", ifelse(histo$price_quality < 0.7, "0.5-1.0", ifelse(histo$price_quality < 0.8, "0.5-1.0", ifelse(histo$price_quality < 0.9, "0.5-1.0", ifelse(histo$price_quality < 1, "0.5-1.0", "Rank=0" ) ) ) ) ) ) ) ) ) ) )
    histo2 <- histo[, c("click_bool", "group_starr_dif")]
    histo3 <- aggregate(histo2,by=list(histo2$group_starr_dif),mean)
    
    ggplot(data=histo3, aes(x=group_starr_dif, y=click_bool)) +
      geom_bar(stat="identity") + scale_x_continuous("PriceQuality",breaks=c(1:5), labels=c("0-1","1-2","2,3","3-4","4-5")) + theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    
    # - - - - - - - - - - - - - - - 
    # Fix srch_query_affinity_score
    # - - - - - - - - - - - - - - -
    # Replace NA values with the mean distances across the origin
    message("Working on srch_query_affinity_score...")
    input_data = input_data %>% 
      replace_na(list(srch_query_affinity_score = 0))
    # - - - - - - - - - - - - - - - 
    # Fix location score #2 values
    # - - - - - - - - - - - - - - -
    # Set the location score #2 to the minimum value across all scores
    message("Working on prop_location_score feature...")
    input_data = input_data %>% 
      group_by(srch_destination_id) %>% 
      replace_na(list(prop_location_score2 = min(.$prop_location_score2, na.rm = T))) %>% 
      ungroup()
    
    # - - - - - - - - - - - - - - - 
    # Fix origin distance values
    # - - - - - - - - - - - - - - -
    # Replace NA values with the mean distances across the origin
    message("Working on origin distance feature...")
    input_data = input_data %>% 
      replace_na(list(orig_destination_distance = mean(.$orig_destination_distance, na.rm = T)))
    
    # - - - - - - - - - - - - - - - 
    # Fix gross_bookings_usd
    # - - - - - - - - - - - - - - -
    # Replace NA values with 
    message("Working on origin distance feature...")
    input_data = input_data %>% 
      replace_na(list(gross_bookings_usd = -1 ))
    # count missing values  
    na_count <-sapply(input_data, function(y) sum(length(which(is.na(y)))))
    
    # deleting competitor original data
    input_data = input_data[,!grepl("comp1",names(input_data))]
    input_data = input_data[,!grepl("comp2",names(input_data))]
    input_data = input_data[,!grepl("comp3",names(input_data))]
    input_data = input_data[,!grepl("comp4",names(input_data))]
    input_data = input_data[,!grepl("comp5",names(input_data))]
    input_data = input_data[,!grepl("comp6",names(input_data))]
    input_data = input_data[,!grepl("comp7",names(input_data))]
    input_data = input_data[,!grepl("comp8",names(input_data))]
    
    ##############################################################################
    # Feature making 
    ##############################################################################
    
    # making feature normal price, normal locationscore, price_quality
    input_data$normal_price <- (input_data$price_usd / mean(input_data$price_usd))
    #input_data$normal_location <- (input_data$prop_location_score1 / mean(input_data$prop_location_score1)
    input_data$price_quality <- (input_data$normal_price/ pmax(0.1,input_data$prop_starrating))
    
    histo <- input_data[, c("booking_bool","click_bool", "price_quality")]
    histo$group <- as.integer(ifelse(histo$price_quality < 0.10, "1", ifelse(histo$price_quality < 0.2, "2", ifelse(histo$price_quality < 0.3, "3", ifelse(histo$price_quality < 0.4, "4", ifelse(histo$price_quality < 0.5, "5", ifelse(histo$price_quality < 0.6, "6", ifelse(histo$price_quality < 0.7, "6", ifelse(histo$price_quality < 0.8, "6", ifelse(histo$price_quality < 0.9, "6", ifelse(histo$price_quality < 1, "6", "7" ) ) ) ) ) ) ) ) ) ) )
    # histo$group <- as.integer(ifelse(histo$price_quality < 0.10, "0.0-0.1", ifelse(histo$price_quality < 0.2, "0.1-0.2", ifelse(histo$price_quality < 0.3, "0.2-0.3", ifelse(histo$price_quality < 0.4, "0.3-0.4", ifelse(histo$price_quality < 0.5, "0.4-0.5", ifelse(histo$price_quality < 0.6, "0.5-1.0", ifelse(histo$price_quality < 0.7, "0.5-1.0", ifelse(histo$price_quality < 0.8, "0.5-1.0", ifelse(histo$price_quality < 0.9, "0.5-1.0", ifelse(histo$price_quality < 1, "0.5-1.0", "Rank=0" ) ) ) ) ) ) ) ) ) ) )
    histo2 <- histo[, c("booking_bool", "group")]
    histo3 <- aggregate(histo2,by=list(histo2$group),mean)
    labels2 <- c("0.0-0.1", "0.1-0.2","0.2-0.3", "0.3-0.4", "0.4-0.5", "0.5-1", "Rank=0")
    
    ggplot(data=histo3, aes(x=group, y=booking_bool)) +
      geom_bar(stat="identity") + scale_x_continuous("PriceQuality",breaks=c(1:7), labels=labels2) + theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    histo2 <- histo[, c("click_bool", "group")]
    histo3 <- aggregate(histo2,by=list(histo2$group),mean)

    ggplot(data=histo3, aes(x=group, y=click_bool)) +
      geom_bar(stat="identity") + scale_x_continuous("PriceQuality",breaks=c(1:7), labels=labels2) + theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    #########################################################################################
    # making feature for search window
    #########################################################################################
    histo <- aggregate(expedia.data$booking_bool, list(length = expedia.data$srch_booking_window), FUN = length)
    histo2 <- aggregate(expedia.data$booking_bool, list(length = expedia.data$srch_booking_window), FUN = sum)
    histo$group <- as.integer(ifelse(histo$length < 365, "0", "1"))
    histo2$group <- as.integer(ifelse(histo2$length < 365, "0", "1"))
    histo3 <- histo[, c("x", "group")]
    histo4 <- histo2[,c("x", "group")]
    histo3$sum <- histo2$x
    histo3$mean <- histo3$sum/histo3$x
    histo3$weigthedmean <- histo3$mean * histo3$x
    histo4 <- aggregate(histo3,by=list(histo2$group),mean)
    
    eval(parse(text = paste(substitute(name_new_dataframe), "<<- input_data")))
    
    
  }
}

preprocess_data = function(input_data, subsample = 0.10){
  
  
  # - - - - - - - - - - - - - - - 
  # Fix competition flags
  # - - - - - - - - - - - - - - -
  message("Working on competition values features...")
  # Flags for lowest price and only with availability
  
  comp_indices <- which(na_features %in% features[29:52])
  comp_features <- names(na_df[, comp_indices])
  comp_df <- na_df[, comp_indices]
  
  rate_i <- which(comp_features %in% comp_features[seq(1, length(comp_features), 3)])
  inv_i <- which(comp_features %in% comp_features[seq(2, length(comp_features), 3)])
  rpd_i <- which(comp_features %in% comp_features[seq(3, length(comp_features), 3)])
  
  n <- nrow(comp_df)
  lowest_price <- logical(n)
  only_avail <- logical(n)
  
  lp_func <- function(x) (min(x, na.rm=TRUE) > -1 && any(!is.na(x)))
  oa_func <- function(x) (min(x, na.rm=TRUE) == 1 && any(!is.na(x)))
  lowest_price <- apply(comp_df[, rate_i], 1, lp_func)
  only_avail <- apply(comp_df[, inv_i], 1, oa_func)
  
  add_comp_df <- data.frame(lowest_price, only_avail)
  
  # - - - - - - - - - - - - - - - 
  # Fix review score
  # - - - - - - - - - - - - - - -
  message("Working on review score features...")
  # Imputing NA review score by value corresponding to 10% quantile
  input_data$prop_review_score_n <- input_data$prop_review_score
  
  property_indices <- which(na_features %in% features[9:15])
  property_features <- names(na_df[, property_indices])
  
  low_review_score <- quantile(na_df$prop_review_score, probs = c(0.1), na.rm=TRUE)
  
  
  #imputing in alterative way the prop_score review
  #summary by hotels
  summary.hotels <- input_data %>%
    group_by(prop_country_id,prop_id) %>%
    summarise(count=n(), bool_counts_by_prop  = sum(booking_bool),relative_bool= sum(booking_bool)/n(),  clicks_counts_by_prop = sum(click_bool), relative_click= sum(click_bool)/n())
  
  
  input_data <- left_join(input_data,summary.hotels,by=c('prop_country_id','prop_id'))
  
  input_data <- input_data %>% group_by(prop_review_score) %>% mutate(normalised_trick3=(clicks_counts_by_prop*bool_counts_by_prop)-mean(clicks_counts_by_prop*bool_counts_by_prop)/sd( clicks_counts_by_prop*bool_counts_by_prop))
  
  summary_for_prop_score <- input_data %>%
    group_by(prop_review_score_n)%>%
    summarise(number=n(),mean_trick=mean(clicks_counts_by_prop*bool_counts_by_prop),
              sd_trick=sd(clicks_counts_by_prop*bool_counts_by_prop),
              mean_trick=mean(clicks_counts_by_prop*bool_counts_by_prop),
              count_book=sum(booking_bool),count_click=sum(click_bool),
              book_freq =sum(booking_bool)/number ,click_freq=sum(click_bool)/number,mean_byrscore_group=mean(normalised_trick3,na.rm=T))
  print(summary_for_prop_score)
  combinations <- (combn(seq(1,5,0.5),2))
  
  
  # handle missing values of prop_review_score
  dista <- function(x){
    min_dist = abs(x^2-groups_centers[1,2]^2)
    winner=1
    
    for (i in seq(2,dim(groups_centers)[1])){
      if (abs(x-groups_centers[i,2]) < min_dist){
        min_dist = abs(x^2-groups_centers[i,2]^2)
        winner=i
      }
    }
    return(as.integer(groups_centers[winner,1]))
  }
  
  groups_centers <- summary_for_prop_score[1:10,c("prop_review_score_n","mean_byrscore_group")]
  to_substitute <-  apply(input_data[is.na(input_data$prop_review_score_n),"normalised_trick3"],1,FUN =dista)
  
  input_data[is.na(input_data$prop_review_score_n),"prop_review_score_n"] <- to_substitute
  
  
  # - - - - - - - - - - - - - - - 
  # Fix competition flags
  # - - - - - - - - - - - - - - -
  message("Working on affinity feature")
  # Temporary method for imputing affinity, imputing with min
  input_data$srch_query_affinity_score_2 <- input_data$srch_query_affinity_score
  
  
  affinity <- input_data$srch_query_affinity_score
  input_data$srch_query_affinity_score[
    is.na(affinity)] <- min(affinity, na.rm=TRUE)
  
  #alternative to impute NA affinity value
  message("Compute new features to impute NA affinity via fitting hyperbole...")
  
  input_data <- input_data %>%
    mutate(responses = case_when(click_bool == 0 & booking_bool == 0 ~ 0,
                                 click_bool == 1 & booking_bool == 0 ~ 0.5,
                                 click_bool == 1 & booking_bool == 1 ~ 1)) %>%
    mutate(group_size = srch_adults_count + srch_children_count,
           srch_room_count= , 
           group_size_normalized_by_room = (srch_adults_count+srch_children_count)/srch_room_count)
  
  input_data <- input_data %>% 
    mutate(trick = ((case_when(promotion_flag == '0' ~ 1, promotion_flag == '1' ~ 2)*case_when(srch_saturday_night_bool == '0' ~ 1, srch_saturday_night_bool == '1' ~ 2))*(srch_length_of_stay*srch_booking_window*group_size))) %>%
    mutate(affinity = relative_bool * relative_click, affinity_log =  ((relative_bool * relative_click) + count)) %>%
    mutate(trick2 = ((srch_length_of_stay*srch_booking_window*group_size)+count))
  
  fun.1 <- function(x){
    return ((-350*(1/(sqrt(x))) -12) +rnorm(1))
  }
  
  input_data <- input_data %>%
    mutate( srch_query_affinity_estimate = fun.1(trick2))
  
  input_data$srch_query_affinity_score_2[is.na(input_data$srch_query_affinity_score_2)] <- input_data$srch_query_affinity_estimate[is.na(input_data$srch_query_affinity_score_2)]
  
  # - - - - - - - - - - - - - - - 
  # Altering original data, adding new columns
  # - - - - - - - - - - - - - - -
  
  input_data$prop_review_score[is.na(input_data$prop_review_score)] <- low_review_score
  input_data$visitor_hist_starrating <- visitor_stars_complete
  input_data$visitor_hist_adr_usd <- visitor_usd_complete
  input_data <- cbind(input_data, add_visitor_df, add_comp_df)
  
  # - - - - - - - - - - - - - - - - - -
  # Remove gross_bookings_usd
  # - - - - - - - - - - - - - - - - - -
  # Feature is not included in the testing set
  input_data = dplyr::select(input_data, -gross_bookings_usd)
  
  
  # - - - - - - - - - - - - - - - - - -
  # Fix NA values for competitors
  # - - - - - - - - - - - - - - - - - -
  # Setting every NA to 2
  
  imputed_comp_df <- comp_df
  imputed_comp_df[is.na(comp_df)] <- 2
  input_data[, seq(29,52)] <- imputed_comp_df
  
  
  # - - - - - - - - - - - -
  # Subsample the dataframe
  # - - - - - - - - - - - -
  message("Subsampling stratified the data...")
  # Get the number of search queries
  #subsample_size = length(levels(as.factor(input_data$srch_id))) * subsample
  
  # Gather X% of training queries
  #subsample_idx = sample(levels(as.factor(input_data$srch_id)), size = ceiling(subsample_size))
  
  # Keep only sampled queries
  #input_data_subsampled = input_data %>% 
  #  filter(srch_id %in% subsample_idx)
  
  # Return dataframe with imputed values
  
  message("Subsampling stratified the data...")
  
  input_data.click <- input_data[input_data$click_bool==1 & input_data$booking_bool == 0,]
  input_data.book <- input_data[input_data$click_bool==1 & input_data$booking_bool == 1,] 
  input_data.notbook <- input_data[input_data$click_bool==0 & input_data$booking_bool == 0,]
  N <- dim(input_data.click)[1]
  stratified <- rbind(sample_n(input_data.click,size =N,replace=T),sample_n(input_data.book,size=N,replace=T),sample_n(input_data.notbook,size=N,replace=T))
  
  return(list(input_data = input_data, 
              stratified = stratified))
  #return(input_data_subsampled)
} 
############################################
## LambdaMART
############################################
LambdaMART<-function(Data)
{
  amount1<-30000
  amount2<-50000
  Data<-as.data.frame(Data)
  now<-Sys.time()
  search_IDs <- sample(unique(Data[,1]),amount1+amount2)
  Data_IDs<-sample(search_IDs,amount1)
  Data2_IDs<-search_IDs[!search_IDs%in% Data_IDs]
  Data2<- Data[Data$srch_id%in% Data2_IDs,]
  Data<- Data[Data$srch_id%in% Data_IDs,]
  bookClickScore =  4 * Data[,"booking_bool"] + Data[,"click_bool"]
  Data <- data.frame(Data, bookClickScore)
  fit<-gbm(
    bookClickScore~ visitor_hist_starrating + prop_starrating + prop_review_score + prop_brand_bool+prop_location_score1 + prop_location_score2+
      prop_log_historical_price + price_usd+promotion_flag + srch_length_of_stay + srch_booking_window + srch_adult_count + srch_children_count+
      srch_room_count + srch_saturday_night_bool + srch_query_affinity_score + orig_destination_distance + 
      comp_rate_na + comp_rate_positive + comp_rate_negative + comp_rate_neutral+comp_inv_na + comp_inv_positive + 
      comp_inv_neutral + comp_rate_percent_diff_na + comp_rate_bool+ comp_inv_bool + starr_dif + price_dif+normal_price + price_quality 
    , data=Data, shrinkage =0.1, n.trees = 500,
           distribution=list(name='pairwise',metric="conc",group='srch_id'), bag.fraction = 0.5,  n.minobsinnode = 50)
  best.iter <- gbm.perf(fit,method="OOB")
  Prediction <-  predict(fit,Data2,best.iter)
  print(Sys.time()-now)
  submit <- data.frame(Data2, prediction = Prediction,index=0)[,-53][,-9:-51][,-2:-7]
  submit<- submit[order(submit[,1],-submit[,6]),]
  CheckScore(submit)
}



############################################
## Benchmark
############################################
Benchmark<-function(Data)
{
  Data<-as.data.frame(Data)
  search_IDs <- unique(Data[,1])
  samples<-sample(search_IDs,100000)
  #now<-Sys.time()
  BenchmarkData<-Data[sample(nrow(Data)),]
  #print(Sys.time()-now)
  BenchmarkData<-BenchmarkData[,-53][,-9:-51][,-2:-7]

  #test<-as.data.frame(test,index=0)
  
  return(CheckScore(BenchmarkData[BenchmarkData[,1]%in%samples,]))
  
  #print(head(BenchmarkData))
}


############################################
## Scorechecker
############################################
GiveDCG<-function(click,book,pos)
{
  if(book)
    return(5/log2(1+pos))
  else if(click)
    return(1/log2(1+pos))
  else
    return(0)
}


CheckScore<-function(DataGuess)
{
  DataGuess$index=0
  now=Sys.time()
  DataCorrect<-DataGuess[order(DataGuess$srch_id,-DataGuess$booking_bool,-DataGuess$click_bool),]
  print(Sys.time()-now)
  search_IDs <- unique(DataGuess[,1])
  now=Sys.time()
  DataCorrect<-DataCorrect[DataCorrect[,3]==1|DataCorrect[,4]==1,]
  DataCorrect <- DataCorrect %>%
    group_by(srch_id) %>%
    mutate(index = row_number()) %>%
    ungroup() %>% as.data.frame()
  DataGuess <- DataGuess %>%
    group_by(srch_id) %>%
    mutate(index = row_number()) %>%
    ungroup() %>% as.data.frame()
  DataGuess<-DataGuess[DataGuess[,3]==1|DataGuess[,4]==1,]
  
  print(Sys.time()-now)
  #print(DataGuess[1:100,])

  NDCG<-c()
  n<-0
  now<-Sys.time()
  DCGGuess <- DataGuess %>%rowwise()%>%
    mutate(DCG = GiveDCG(click_bool,booking_bool,index))%>%group_by(srch_id)%>%
    summarize(DCGtotalGuess=sum(DCG)) %>% as.data.frame()
  DCGCorrect <- DataCorrect %>%rowwise()%>%
    mutate(DCG = GiveDCG(click_bool,booking_bool,index))%>%group_by(srch_id)%>%
    summarize(DCGtotalCorrect=sum(DCG)) %>% as.data.frame()

  DCGFrame<-cbind(DCGGuess,DCGCorrect[-1])
  NDCG<- DCGFrame%>%rowwise() %>% mutate(NDCG=DCGtotalGuess/DCGtotalCorrect)%>%  as.data.frame()
  print(Sys.time()-now)
  #print(NDCG[1:100,])
  print(mean(NDCG[,4]))
}
#LambdaMART(expedia.data)
#Benchmark(expedia.data)
Main()