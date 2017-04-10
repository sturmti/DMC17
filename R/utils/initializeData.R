source("utils/loadData.R")
source("preprocessing/featureEngineering.R")

#' Initialize data.train
#' @description Initializes data.train by importing the data and adding the engineered features.
#' @return A data.table containing the train.csv data with the additional engineered features.
initializeDataTrain <- function(){
  data.train <- getTrainData()
  data.train <- createEngineeredFeaturesForDataTrain(data.train)
  data.train
}