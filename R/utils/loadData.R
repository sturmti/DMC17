if (!require(data.table)) install.packages("data.table")

###### HELPER FUNCTIONS #####
#' [Helper Function] Return file path
#' @description Returns the file path for a given file.
#' @param file.name file name as string.
#' @return A path as string
getFilePath <- function(file.name){
  path <- "/Users/TimoSturm/Documents/Masterstudium/2. Semester/Data Mining II/Data Mining Cup 2017/DMC_2017_task/"
  paste0(path, file.name)
}


###### MAIN FUNCTIONS #####
#' Get item.csv data
#' @description Imports the item.csv data, factors certain columns and returns a corresponding data.table.
#' @return A data.table containing the item.csv data.
getItemData <- function(){
  # Import Data
  data.items <- data.table(read.csv(file = getFilePath("items.csv"), header = T, sep = "|"))
  # factorize the polynominal columns:
  #   - factored: all columns
  data.items <- data.items[, lapply(.SD, factor)]
} 

#' Get the train.csv data
#' @description Imports the train.csv data, factors certain columns  and returns a corresponding data.table.
#' @return A data.table containing the train.csv data.
getTrainData <- function(){
  # Import Data
  data.train <- data.table(read.csv(file = getFilePath("train.csv"), header = T, sep = "|"))
  # factorize the polynominal columns: 
  #    - factored: lineID, pi
  #    - ordered: availability
  data.train$lineID <- factor(data.train$lineID)
  data.train$pid <- factor(data.train$pid)
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
  #    - factored: lineID, pi
  #    - ordered: availability
  data.class$lineID <- factor(data.class$lineID)
  data.class$pid <- factor(data.class$pid)
  data.class$availability <- ordered(data.class$availability, levels = c(1, 2, 3, 4))
  data.class
}

#' Loads and initializes the Data
#' @description Imports all three files (item.csv, train.csv, class.csv), factorizes if needed and returns a vector containing corresponding data.tables. 
#' @return A vector containing the data of item.csv, train.csv, class.csv
initializeData <- function(){
  list(itemData = getItemData(), trainData = getTrainData(), classData = getClassData())
}