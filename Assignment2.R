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

  


# Set switches 
loadingSwitch <- TRUE
exploreSwitch <- TRUE
preprocSwitch <- TRUE

loading(loadingSwitch)
exploring(exploreSwitch)

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
input_data <<- expedia.data[sample(nrow(expedia.data), 1000), ] 
test_data <<- expedia.data[sample(nrow(expedia.data), 1000),]
    

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
ggplot(df1, aes(x=name, y=rank)) + geom_col() + theme(axis.text.x = element_text(angle = 90, hjust = 1))

#library(reshape2)
#ggplot(melt(expedia.data), aes(variable, value)) + geom_boxplot()

#ggplot(stack(expedia.data), aes(x = ind, y = values)) +
 # geom_boxplot()

# What days of the week are people booking?
expedia.booked$DayOfWeek <- factor(weekdays(as.Date(expedia.booked$date_time)), levels= c("maandag", "dinsdag", "woensdag", "donderdag", "vrijdag", "zaterdag", "zondag"))
ggplot(data = expedia.booked, aes(expedia.booked$DayOfWeek)) + geom_bar()
  }
}

expedia.data$new <- NULL

preproc<-function(preprocSwitch, input_data, subsample = 0.10){
  if(preprocSwitch)
  {
    message("Working on competitor features...")
    # Count the number of NaN/-1/+1 values across compX_rate
    comp_rate_missing = input_data %>% 
      dplyr::select(ends_with("rate")) %>% 
      by_row(
        ..f = function(x) {
          sum(is.na(x[1:8]))
        },
        .to = "comp_rate_na",
        .collate = "cols"
      ) %>% 
      by_row(
        ..f = function(x) {
          sum(x[1:8] == 1, na.rm = T)
        },
        .to = "comp_rate_positive",
        .collate = "cols"
      ) %>% 
      by_row(
        ..f = function(x) {
          sum(x[1:8] == -1, na.rm = T)
        },
        .to = "comp_rate_negative",
        .collate = "cols"
      ) %>% 
      by_row(
        ..f = function(x) {
          sum(x[1:8] == 0, na.rm = T)
        },
        .to = "comp_rate_neutral",
        .collate = "cols"
      ) %>% 
      dplyr::select(starts_with("comp_rate"))
    
    # Count the number of NaN/0/+1 values across compX_inv
    comp_inv_missing = input_data %>% 
      dplyr::select(ends_with("inv")) %>% 
      by_row(
        ..f = function(x) {
          sum(is.na(x[1:8]))
        },
        .to = "comp_inv_na",
        .collate = "cols"
      ) %>% 
      by_row(
        ..f = function(x) {
          sum(x[1:8] == 1, na.rm = T)
        },
        .to = "comp_inv_positive",
        .collate = "cols"
      ) %>% 
      by_row(
        ..f = function(x) {
          sum(x[1:8] == 0, na.rm = T)
        },
        .to = "comp_inv_neutral",
        .collate = "cols"
      ) %>% 
      dplyr::select(starts_with("comp_inv"))
    
    # Count the number of NaN/mean(abs) values across compX_rate_percent
    comp_rate_percent_missing = input_data %>% 
      dplyr::select(ends_with("rate_percent_diff")) %>% 
      by_row(
        ..f = function(x) {
          sum(is.na(x[1:8]))
        },
        .to = "comp_rate_percent_diff_na",
        .collate = "cols"
      ) %>% 
      dplyr::select(starts_with("comp_rate_percent"))
    
    # Cbind the existing data
    input_data = input_data %>% 
      cbind(comp_rate_missing) %>% 
      cbind(comp_inv_missing) %>% 
      cbind(comp_rate_percent_missing)
    
    #Boxplot bool
    ggplot(aggregate( booking_bool ~ comp_rate_negative, data = input_data, FUN = length) , aes(comp_rate_negative, booking_bool))  + geom_col()
    
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
    input_data$starr_dif <- (input_data$visitor_hist_starrating - input_data$prop_starrating)
      

    # Replace NA values with the mean price across the origin
    message("Working on visior history...")
    input_data = input_data %>% 
      replace_na(list(visitor_hist_adr_usd = mean(.$visitor_hist_adr_usd, na.rm = T)))
    input_data$price_dif <- (input_data$visitor_hist_adr_usd - input_data$price_usd)
    
    
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
    
    # making feature normal price, normal locationscore, price_quality
    input_data$normal_price <- (input_data$price_usd / mean(input_data$price_usd))
    #input_data$normal_location <- (input_data$prop_location_score1 / mean(input_data$prop_location_score1)
    input_data$price_quality <- (input_data$normal_price/ input_data$prop_starrating)
    
  }
}

# Read the data

train.clean <- read.csv('./data/clean_training_data.csv', header = TRUE, stringsAsFactors = FALSE)
test.sample <- read.csv('./data/clean_testing_data.csv', header = TRUE, stringsAsFactors = FALSE)

train.clean <- input_data
test.sample <- test_data
# Generate random samples for a training and a testing sets
set.seed(0)
individual_users <- unique(train.clean$srch_id)

# Select X% of the total population at random
X = 0.1
sample_users_pool <- sample(individual_users, (length(individual_users) * X))

sample.train <- unique(sort(sample_users_pool[1:length(sample_users_pool)]))

train.sample <- train.clean[train.clean$srch_id %in% sample.train,]

rm(train.clean, individual_users, sample_users_pool, sample.train)

## Set up the initial model for click_bool
# Get the initial variables from the text file
variable.names <- as.vector(names(input_data))

## Model each prediction------------------------
## Model the click boolean
xgboost.click_bool <- xgboost(data = sapply(train.sample[variable.names], as.numeric), 
                              label = train.sample$click_bool,
                              params = list(objective = "binary:logistic",
                                            eta = 0.01,
                                            max.depth = 40,
                                            nthread = 6),
                              nrounds = 3,
                              verbose = 0)
# Predict click_bool
xgboost.click_bool.pred <- predict(object = xgboost.click_bool, 
                                   newdata = sapply(test.sample[variable.names], as.numeric))
# Add to dataset
test.sample$click_bool.pred <- ifelse(xgboost.click_bool.pred > 0.489, 1, 0)

# Remove
rm(xgboost.click_bool, xgboost.click_bool.pred)


## Model the booking boolean
xgboost.booking_bool <- xgboost(data = sapply(train.sample[c(variable.names,'click_bool')], as.numeric), 
                                label = train.sample$booking_bool,
                                params = list(objective = "binary:logistic",
                                              eta = 0.01,
                                              max.depth = 40,
                                              nthread = 6),
                                nrounds = 3,
                                verbose = 0)
# predict booking_bool
xgboost.booking_bool.pred <- predict(object = xgboost.booking_bool, 
                                     newdata = sapply(test.sample[c(variable.names,'click_bool')], as.numeric))
# Add to dataset
test.sample$booking_bool.pred <- ifelse(xgboost.booking_bool.pred > 0.50, 1, 0)

# Remove
rm(xgboost.booking_bool, xgboost.booking_bool.pred)

## Model the ranking
xgboost.position <- xgboost(data = sapply(train.sample[c(variable.names,
                                                         'click_bool', 
                                                         'booking_bool')], 
                                          as.numeric), 
                            label = train.sample$position,
                            params = list(objective = "rank:pairwise",
                                          eta = 0.01,
                                          max.depth = 40,
                                          nthread = 6,
                                          eval_metric = 'ndcg'),
                            nrounds = 3,
                            verbose = 0)
# Predict the position in the ranking
xgboost.position.pred <- predict(object = xgboost.position, 
                                 newdata = sapply(test.sample[c(variable.names,
                                                                'click_bool', 
                                                                'booking_bool')], 
                                                  as.numeric))
test.sample$position.pred <- xgboost.position.pred
test.sample <- test.sample[order(test.sample$srch_id, test.sample$position.pred),]
test.sample$position.order <- ave(test.sample$position.pred, test.sample$srch_id, FUN = seq_along)
test.sample <- test.sample[order(test.sample$srch_id, test.sample$position.order),]

# Make the final output file
final_output <- cbind(test.sample$srch_id, test.sample$prop_id)
write.csv(final_output, 'FINAL_Output (080).csv')