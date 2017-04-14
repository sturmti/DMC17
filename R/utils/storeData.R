#' Store data in a csv file
#' @description Stores the given data table in a correctly formatted csv file, named as the given fileName.
#' @param data data table containing the data.
#' @param file.name name for the csv file.
storeData <- function(data, file.name){
  path <- "/Users/TimoSturm/Documents/Masterstudium/2. Semester/Data Mining II/Data Mining Cup 2017/DMC_2017_task/"
  write.table(x = data, file = paste0(path, file.name), sep = "|", quote = FALSE, row.names = FALSE)
}