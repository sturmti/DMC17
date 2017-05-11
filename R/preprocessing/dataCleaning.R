### Data Cleaning 
## Purpose: Handle missing values, wrong values, etc. to improve data quality

#' Handle missing values, wrong values, etc. of data.items to improve data quality of items.csv.
#' @description Transforms data.items to improve data quality.
#' @return A data.table containing the cleaned data.items.
cleanDataItems <- function(data.items){
  ## pharmForm
  data.items$pharmForm <- toupper(data.items$pharmForm)
  
  data.items
}


