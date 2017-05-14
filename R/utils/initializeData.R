source("utils/loadData.R")
source("preprocessing/featureEngineering.R")
source("preprocessing/dataCleaning.R")
library(dplyr)

#' Initialize data.train
#' @description Initializes data.train by importing the data and adding the engineered features.
#' @return A data.table containing the train.csv data with the additional engineered features.
initializeDataTrain <- function(dropFirst19Days = FALSE, combine.With.class.data=FALSE){
  data.train <- getTrainData(dropFirst19Days)
  data.train <- createEngineeredFeaturesForDataTrain(data.train, combine.With.class.data)
  data.train
}

#' Initialize data.items
#' @description Initializes data.items by importing the data and adding the engineered features.
#' @return A data.table containing the items.csv data with the additional engineered features.
initializeDataItems <- function(){
  data.items <- getItemData()
  data.items <- cleanDataItems(data.items)
  data.items <- createEngineeredFeaturesForDataItems(data.items)
  data.items
}

#' Initialize data.all
#' @description Initializes data.items by importing the data and adding the engineered features.
#' @return A data.table containing the items.csv data with the additional engineered features.
initializeJoinedData <- function(dropFirst19Days = FALSE, combine.With.class.data=FALSE){
  data.train <- initializeDataTrain(dropFirst19Days, combine.With.class.data)
  data.items <- initializeDataItems()
  data.all <- joinData(data.train, data.items)
  data.all <- createEngineeredFeaturesForDataTrainItems(data.all)
  data.all
}

#' Initializes and samples the train data
#' @description Initializes the train data and samples it according to the given fraction.
#' @return A fraction based sample of the train data.
getInitializedSampleTrainData <- function(size = 0.2){
  sample <- initializeDataTrain()
  sample <- sample_frac(tbl = sample, size = size)
  sample
}

#' Creates a sample of the given data
#' @description Samples the given data according to the given fraction.
#' @return A fraction based sample of the train data.
sampleData <- function(data, size = 0.2){
  sample <- sample_frac(tbl = data, size = size)
  sample
}

#' Join two given data sets
#' @description Makes a left join between data1 and data2 based on the "pid" feature.
#' @param data1 data.table containing data for the left side of the join.
#' @param data2 data.table containing data for the right side of the join.
#' @return A data table containing the joined data.
joinData <- function(data1, data2){
  data.all <- data.table(left_join(x = data1, y = data2, by = c("pid" = "pid")))
  data.all
}