#' Create additional engineered features for the train.csv
#' @description Computes and adds the engineered features to the data.train data set.
#' @return A data.table containing the enhanced data.train data set.
createEngineeredFeaturesForDataTrain <- function(data.train){
  ## based on click, basket, order
  # aggregates the features click, baset, order into one single feature
  data.train <- within(data.train, {
    actionType = ifelse(data.train$click == 1, "click", ifelse(data.train$basket == 1, "basket", "order"))
  })
  ## based on revenue, price
  # quantity of ordered products, NA if action is click or basket
  data.train <- within(data.train, {
    quantity = ifelse(data.train$order == 1, (data.train$revenue / data.train$price), NA)
  })
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
  # indicator if line has revenue (= order)
  data.train <- within(data.train, {
    hasRevenue = ifelse(data.train$order == 1, 1, 0)
  })
  
  data.train
}

#' Create additional engineered features for the items.csv
#' @description Computes and adds the engineered features to the data.items data set.
#' @return A data.table containing the enhanced data.items data set.
createEngineeredFeaturesForDataItems <- function(data.items){
  ## based on unit
  # binning of unit values: "ST" and "notST"
  data.items <- within(data.items, {
    unit_is_ST = ifelse(data.items$unit == "ST", 1, 0)
  })
  # binning of unit values: "ST", "ML" and "OTHER"
  data.items <- within(data.items, {
    unit_ST_ML_OTHER = ifelse(data.items$unit == "ST", "ST", ifelse(data.items$unit == "ML", "ML", "OTHER"))
  })
  ## based on content
  # number of packages of each product according to content
  data.items$numberOfPackages <- lapply(data.items$content, function(data){
    numberVector <- strsplit(as.character(data), "X")[[1]]
    if(length(numberVector) > 1)
      result = numberVector[1]
    else 
      result = 1
    result
  })
  # quantity in each package
  data.items$quantityByPackage <- lapply(data.items$content, function(data){
    numberVector <- strsplit(as.character(data), "X")[[1]]
    if(length(numberVector) > 1)
      result = numberVector[2]
    else
      result = numberVector[1]
    result  
  })
  
  data.items
}