if (!require(data.table)) install.packages("data.table")

###### HELPER FUNCTIONS #####
#' [Helper Function] Return file path
#' @description Returns the file path for a given file.
#' @param file.name file name as string.
#' @return A path as string
getFilePath <- function(file.name){
  path <- "C:\\Users\\i852496\\Documents\\Data Mining Cup 2017\\"
  paste0(path, file.name)
}


###### MAIN FUNCTIONS #####
#' Get item.csv data
#' @description Imports the item.csv data, factors certain columns and returns a corresponding data.table.
#' @return A data.table containing the item.csv data.
getItemData <- function(){
  # Import Data
  data.items.unfactored <- data.table(read.csv(file = getFilePath("items.csv"), header = T, sep = "|"))
  # factorize the polynominal columns:
  #   - factored: all columns, except: rrp (= 11th column)
  data.items <- data.items.unfactored[, lapply(.SD[, -11], factor)]
  data.items$rrp <- data.items.unfactored$rrp
  data.items
} 

#' Get the train.csv data
#' @description Imports the train.csv data, factors certain columns  and returns a corresponding data.table.
#' @return A data.table containing the train.csv data.
getTrainData <- function(dropFirst19Days = FALSE){
  # Import Data
  data.train <- data.table(read.csv(file = getFilePath("train.csv"), header = T, sep = "|"))
  if(dropFirst19Days == TRUE){
    data.train <- data.train[day > 22]
  }
  # factorize the polynominal columns: 
  #    - ordered: lineID, pid, availability
  #data.train$lineID <- ordered(data.train$lineID)
  data.train$pid <- ordered(data.train$pid)
  data.train$availability <- ordered(data.train$availability, levels = c(1, 2, 3, 4))
  data.train
}

#' Get the class.csv data
#' @description Imports the class.csv data, factors certain columns  and returns a corresponding data.table.
#' @return A data.table containing the class.csv data.
getClassData <- function(){
  # Import Data
  data.class <- data.table(read.csv(file = getFilePath("class.csv"), header = T, sep = "|"))
  # factorize the polynominal columns: 
  #    - ordered: lineID, pid, availability
  data.class$lineID <- data.class$lineID + 2756003
  #data.class$lineID <- ordered(data.class$lineID)
  data.class$pid <- ordered(data.class$pid)
  data.class$availability <- ordered(data.class$availability, levels = c(1, 2, 3, 4))
  data.class
}

#' Get the daily price difference data
#' @description Imports the train_dailyPriceDifference.csv data, factors the pid column and returns a corresponding data.table.
#' @return A data.table containing the train_dailyPriceDifference.csv data.
getDailyPriceDifferenceData <- function(){
  data.dailyPriceDifference <- data.table(read.csv(file = getFilePath("trainClass_dailyPriceDifference_V1.1.csv"), header = T, sep = "|"))
  #data.dailyPriceDifference$lineID <- ordered(data.dailyPriceDifference$lineID)
  data.dailyPriceDifference <- data.dailyPriceDifference[order(lineID)][, c("lineID", "dailyPriceDifference")]
  data.dailyPriceDifference
}

#' Get the daily competitorPrice difference data
#' @description Imports the train_dailyCompetitorPriceDifference.csv data, factors the pid column and returns a corresponding data.table.
#' @return A data.table containing the train_dailyCompetitorPriceDifference.csv data.
getDailyCompetitorPriceDifferenceData <- function(){
  data.dailyCompetitorPriceDifference <- data.table(read.csv(file = getFilePath("trainClass_dailyCompetitorPriceDifference_V1.1.csv"), header = T, sep = "|"))
  #data.dailyCompetitorPriceDifference$lineID <- ordered(data.dailyCompetitorPriceDifference$lineID)
  data.dailyCompetitorPriceDifference <- data.dailyCompetitorPriceDifference[order(lineID)][, c("lineID", "dailyCompetitorPriceDifference")]
  data.dailyCompetitorPriceDifference
}



getBasketData <- function(){
  data.basket <- data.table(read.csv(file = getFilePath("baskets.csv"), header = T, sep = ";"))
  data.basket
}


getAlexTrainData <- function(){
  data.alex <- data.table(read.csv(file = getFilePath("trainNewFeatures_10_5_2017_Alex.csv"), header = T, sep = ";"))
  #data.alex$lineID <- ordered(data.alex$lineID)
  data.alex <- data.alex[, !c("predict.Order.")]
  data.alex
}

getAlexClassData <- function(){
  data.alex <- data.table(read.csv(file = getFilePath("classBasketFeaturesAlex2.0.csv"), header = T, sep = ";"))
  #data.alex$lineID <- ordered(data.alex$lineID)
 # data.alex$lineID <- data.alex$lineID + 2756003
  data.alex
}

getDataTrainClass <- function(){
  data.train <- getTrainData()
  data.class <- getClassData()
  data.trainClass <- union_all(data.train, data.class)
  data.trainClass <- data.trainClass[order(lineID)]
  data.trainClass
}


###### Association Analysis ###### 
getAssociationSetOrderedItems <- function(){
  data.associationSetOrderedItems <- data.table(read.csv(file = getFilePath("Association_Analysis_Data\\orderedItemSets.csv"), header = T, sep = ";"))
  data.associationSetOrderedItems
}

getAssociationSetNotOrderedItems <- function(){
  data.associationSetNotOrderedItems <- data.table(read.csv(file = getFilePath("Association_Analysis_Data\\NotOrderedItemSets.csv"), header = T, sep = ";"))
  data.associationSetNotOrderedItems
}

