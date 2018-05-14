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

# Set switches 
exploreSwitch <- TRUE
loadingSwitch <- FALSE

loading(loadingSwitch)
exploring(exploreSwitch)

  
}

############################################
## Loading dataset for each user
############################################

loading<-function(loadingSwitch){
  if(loadingSwitch)
  {
datatraining.folder  <- 
  if(grepl("Michael", getwd())) {
    "C:/Users/Michael"
  } else if(grepl("Roel", getwd())) {
    "training_set_VU_DM_2014.csv"
  } else if(grepl("Emma", getwd())) {
    "/Users/Emma/Downloads/Data Mining VU data/training_set_VU_DM_2014.csv"
  }


datatest.folder  <- 
  if(grepl("Michael", getwd())) {
    "C:/Users/Michael"
  } else if(grepl("Roel", getwd())) {
    "test_set_VU_DM_2014.csv"
  } else if(grepl("Emma", getwd())) {
    "/Users/Emma/Downloads/Data Mining VU data/test_set_VU_DM_2014.csv"
  }

expedia.data <- fread(datatraining.folder, header=TRUE, na.strings=c("","NULL","NA"))       # 31 seconde bij Emma, 6 bij Roel
expedia.test <- fread(datatest.folder, header=TRUE, na.strings=c("","NA"))   # 28 seconde bij Emma, 11 bij Roel
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

# What days of the week are people booking?
expedia.booked$DayOfWeek <- factor(weekdays(as.Date(expedia.booked$date_time)), levels= c("maandag", "dinsdag", "woensdag", "donderdag", "vrijdag", "zaterdag", "zondag"))
ggplot(data = expedia.booked, aes(expedia.booked$DayOfWeek)) + geom_bar()
  }
}


