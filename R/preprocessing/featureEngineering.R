### Feature Engineering
## Purpose: Try out some things (but don't delete it to document it! (if it's not trivial))
##          If something meaningful was found and should be available in the main train data, move it to featureGeneration.R.

#' Create additional engineered features for the train.csv
#' @description Computes and adds the engineered features to the data.train data set.
#' @return A data.table containing the enhanced data.train data set.
createEngineeredFeatures <- function(data.train){
  ## based on price, competitorPrice
  # diff and ratios of diff between price, competitorPrice
  data.train$diffPriceCompetitorPrice <- data.train$price - data.train$competitorPrice
  data.train$ratioDiffPriceCompetitorPriceToPrice <- (data.train$price - data.train$competitorPrice)/data.train$price
  data.train$ratioDiffPriceCompetitorPriceToCompetitorPrice <- (data.train$price - data.train$competitorPrice)/data.train$competitorPrice
  # normalization of price, competitorPrice
  data.train$priceNorm <- data.train$price/max(data.train$price)
  data.train$competitorPriceNorm <- data.train$competitorPrice/max(data.train$competitorPrice, na.rm = TRUE)
  # ordered factorized relation between price, customerPrice ("lower", "equal", "higher")
  data.train <- within(data.train, {
    factorizedRelationPriceCustomerPrice = ifelse(data.train$price > data.train$competitorPrice, "higher", ifelse(data.train$price < data.train$competitorPrice, "lower", "equal"))
  })
  data.train$factorizedRelationPriceCustomerPrice = ordered(data.train$factorizedRelationPriceCustomerPrice, c("lower", "equal", "higher"))
  
  data.train
}