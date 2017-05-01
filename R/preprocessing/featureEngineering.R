#' Create additional engineered features for the train.csv
#' @description Computes and adds the engineered features to the data.train data set.
#' @return A data.table containing the enhanced data.train data set.
createEngineeredFeaturesForDataTrain <- function(data.train){
  ## based on day
  # timestamp (format: mm/dd/YYYY) starts on 01.10.2016
  data.train$date <- as.Date("2016-10-01") + data.train$day - 1
  # indicator for weekday
  data.train$weekday_number <- lapply(data.train$day, function(day){
    modRes <- (day %% 7)
    ifelse(modRes == 0, 7, modRes)
  })
  data.train$weekday_number <- as.numeric(data.train$weekday_number)  # without this transformation the values will be saved as lists
  # indicator for weekday name
  data.train$weekday_name <- weekdays(data.train$date)
  # indicator for week
  #data.train$week <- as.numeric( format(data.train$date-2, "%W"))
  data.train$week <- ifelse(data.train$day %% 7 == 0, data.train$day %/% 7, data.train$day %/% 7 + 1)
  #indicator for month half
  data.train$monthHalf <- ifelse(as.numeric(format(data.train$date, "%d")) > 15, 2, 1)
  # indicator for month
  data.train$month <- as.numeric(format(data.train$date, "%m")) - 9 # the "-9" is necessary because we want 1,2,3 and not 10,11,12 
  # indicator for holidays
  data.train <- within(data.train, {
    holiday = ifelse(data.train$day %in% c(3, 31, 32, 86, 87), 1, 0)
  })
  #indicator for weekend
  data.train <- within(data.train, {
    weekend = ifelse(data.train$weekday_name %in% c("Saturday", "Sunday"), 1, 0)
  })
  
  ## based on click, basket, order
  # aggregates the features click, baset, order into one single feature
  data.train <- within(data.train, {
    actionType = ifelse(data.train$click == 1, "click", ifelse(data.train$basket == 1, "basket", "order"))
  })
  # indicator if product has already been sold on the same day
  check.if.contained <- function(current.pid, current.lineID, current.day){
    current.pid %in% subset(data.train[day == current.day], data.train$lineID < current.lineID)$pid
  }
 # data.train$alreadyBoughtOnSameDay <- lapply(data.train, function(data){
  #  ifelse(check.if.contained(data$pid, data$lineID, data$day), 1, 0)
  #})
  
  data.train <- within(data.train, {
    alreadyBoughtOnSameDay =  ifelse(check.if.contained(data.train$pid, data.train$lineID, data.train$day), 1, 0)
  })
  
  #data.train <- within(data.train, {
   # alreadyBoughtOnSameDay = ifelse(data.train$pid %in% subset(data.train[day == data.train$day], lineID < data.train$lineID)$pid, 1, 0)
  #})
  
  
  # counter for the amount of ordered amount per day
  
  
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
  # ordered factorized relation between price, competitorPrice ("lower", "equal", "higher")
  data.train <- within(data.train, {
    factorizedRelationPriceCompetitorPrice = ifelse(data.train$price > data.train$competitorPrice, "higher", ifelse(data.train$price < data.train$competitorPrice, "lower", ifelse(data.train$price == data.train$competitorPrice, "equal", NA)))
  })
  data.train$factorizedRelationPriceCompetitorPrice = ordered(data.train$factorizedRelationPriceCompetitorPrice, c("lower", "equal", "higher"))
  # mean price per product
  meanPricePerProduct <- setNames(aggregate(x = data.train$price, by = list(pid = data.train$pid), FUN = mean), c("pid", "meanPricePerProduct"))
  data.train <- data.table(left_join(data.train, data.table(meanPricePerProduct), by=c("pid" = "pid")))
  # number of transactions per product, number orders per product, order ratio
  countPerProduct <- setNames(aggregate(x = data.train$pid, by = list(pid = data.train$pid), FUN = length), c("pid", "productActionCounter"))
  countPerOrderProduct <- setNames(aggregate(x = data.train$order, by = list(pid = data.train$pid), FUN = sum), c("pid", "productOrderCounter"))
  data.temp <- data.table(left_join(data.table(countPerProduct), data.table(countPerOrderProduct), by=c("pid" = "pid")))
  data.temp$productOrderRatio <- (data.temp$productOrderCounter / data.temp$productActionCounter)
  data.train <- data.table(left_join(data.train, data.temp, by=c("pid" = "pid")))
  
  data.train
}

#' Create additional engineered features for the items.csv
#' @description Computes and adds the engineered features to the data.items data set.
#' @return A data.table containing the enhanced data.items data set.
createEngineeredFeaturesForDataItems <- function(data.items){
  ## based on rrp
  # normalized rrp
  data.items$rrpNorm <- data.items$rrp/max(data.items$rrp)
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
    if(length(numberVector) == 3){
      result = as.character(as.numeric(numberVector[1]) * as.numeric(numberVector[2]))
    }
    else{
      if(data == "PAK" || data == "L   125")
        result = NA
      else if(length(numberVector) > 1)
        result = numberVector[[1]]
      else 
        result = "1"
    }
    result
  })
  data.items$numberOfPackages <- as.numeric(data.items$numberOfPackages)  # without this transformation each value will be saved as list
  
  # quantity in each package
  data.items$quantityByPackage <- lapply(data.items$content, function(data){
    numberVector <- strsplit(as.character(data), "X")[[1]]
    if(length(numberVector) == 3)
      result = numberVector[3]
    else if(length(numberVector) == 2)
      result = numberVector[2]
    else
      result = numberVector[1]
    result
  })
  data.items$quantityByPackage <- as.numeric(data.items$quantityByPackage)  # without this transformation each value will be saved as list
  
  # total number of pieces
  data.items$totalNumberOfPieces <- data.items$numberOfPackages * data.items$quantityByPackage
  
  data.items
}

#' Create additional engineered features for the trainItems.csv
#' @description Computes and adds the engineered features to the data.train.items data set.
#' @return A data.table containing the enhanced data.train.items data set.
createEngineeredFeaturesForDataTrainItems <- function(data.train.items){
  ## based on rrp, price, competitorPrice
  # absolute difference between rrp and price (= rrp - price)
  data.train.items$diff_rrp_price <- data.train.items$rrp - data.train.items$price
  # relative difference between rrp and price (= (rrp - price)/rrp)
  data.train.items$ratio_diff_rrp_price_to_rrp <- data.train.items$diff_rrp_price / data.train.items$rrp
  # absolute difference between rrp and competitorPrice (= rrp - competitorPrice)
  data.train.items$diff_rrp_competitorPrice <- data.train.items$rrp - data.train.items$competitorPrice
  # relative difference between rrp and competitorPrice (= (rrp - competitorPrice)/rrp)
  data.train.items$ratio_diff_rrp_competitorPrice_to_rrp <- data.train.items$diff_rrp_competitorPrice / data.train.items$rrp
  
  ## based on totalNumberOfPieces
  # price per piece (price / total number of pieces)
  data.train.items$pricePerPiece <- data.train.items$price / data.train.items$totalNumberOfPieces
  # competitorPrice per piece (competitorPrice / total number of pieces)
  data.train.items$competitorPricePerPiece <- data.train.items$competitorPrice / data.train.items$totalNumberOfPieces
  # rrp per piece (rrp / total number of pieces)
  data.train.items$rrpPerPiece <- data.train.items$rrp / data.train.items$totalNumberOfPieces
  
  data.train.items
}
