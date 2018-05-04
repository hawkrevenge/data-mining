############################################
## Datamining VU - Expedia 
############################################

# Libraries
library(data.table)  
library(ggplot2)    

############################################
## Loading dataset for each user
############################################
datatraining.folder  <- 
  if(grepl("Michael", getwd())) {
    "C:/Users/Michael"
  } else if(grepl("Roel", getwd())) {
    "/Users/Roel"
  } else if(grepl("Emma", getwd())) {
    "/Users/Emma/Downloads/Data Mining VU data/training_set_VU_DM_2014.csv"
  }


datatest.folder  <- 
  if(grepl("Michael", getwd())) {
    "C:/Users/Michael"
  } else if(grepl("Roel", getwd())) {
    "/Users/Roel"
  } else if(grepl("Emma", getwd())) {
    "/Users/Emma/Downloads/Data Mining VU data/test_set_VU_DM_2014.csv"
  }

expedia.data <- fread(datatraining.folder, header=TRUE)       # 31 seconde bij Emma
expedia.test <- fread(datatest.folder, header=TRUE)   # 28 seconde bij Emma

############################################
## Exploring dataset
############################################

dim(expedia.data)
dim(expedia.test)
str(expedia.data)
summary(expedia.test)

