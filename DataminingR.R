
checkpackages <- function()
{
  library('ggplot2')
  library('forecast')
  library('tseries')
}

installPackages <- function(){
  install.packages('ggplot2')
  install.packages('forecast')
  install.packages('tseries')
  
  checkpackages()
}

tryCatch( checkpackages(), error=installPackages())


mydata = read.csv("dataset_mood_smartphone.csv")
