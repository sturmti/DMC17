#' Store data in a csv file
#' @description Stores the given data table in a correctly formatted csv file, named as the given fileName.
#' @param data data table containing the data.
#' @param file.name name for the csv file.
storeData <- function(data, file.name){
  path <- "C:\\Users\\i852496\\Documents\\Data Mining Cup 2017\\"
  write.table(x = data, file = paste0(path, file.name), sep = "|", quote = FALSE, row.names = FALSE)
}