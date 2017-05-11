#' Create additional engineered features for the train.csv
#' @description Computes and adds the engineered features to the data.train data set.
#' @return A data.table containing the enhanced data.train data set.
createEngineeredFeaturesForDataTrain <- function(data.train){
  # removing test set information (only necessary to use for order or/and revenue based features!)
  data.train <- data.train[day < 64]

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
    actionType = ifelse(data.train$click == 1, "click", ifelse(data.train$basket == 1, "basket", ifelse(data.train$order == 1, "order", NA)))
  })
  
  ## based on revenue, price
  # quantity of ordered products, NA if action is click or basket
  data.train <- within(data.train, {
    quantity = ifelse(data.train$order == 1, (data.train$revenue / data.train$price), 0)
  })
  ## based on price, competitorPrice
  # classifies the price into 3 categories based on the overall price quantiles
  data.train.priceQuantiles <- quantile(data.train$price)
  data.train$priceCategory <- vapply(data.train$price, function(current.price){
    if(current.price <= data.train.priceQuantiles[2]){
      "low_price"
    }
    else if(current.price <= data.train.priceQuantiles[3]){
      "middle_price"
    }
    else{
      "high_price"
    }
  }, FUN.VALUE = character(1))
  # diff and ratios of difference between price, competitorPrice
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
  # max price per pid
  data.train <- merge(data.train, setnames(aggregate(data.train$price, by=list(pid = data.train$pid), FUN=max, na.rm=TRUE), c("pid", "maxPrice")), all.x=TRUE, by=c("pid"))
  # min price per pid
  data.train <- merge(data.train, setnames(aggregate(data.train$price, by=list(pid = data.train$pid), FUN=min, na.rm=TRUE), c("pid", "minPrice")), all.x=TRUE, by=c("pid"))
  # absolute variance of maxPrice and minPrice per pid
  data.train$maxMinpriceVariance <- data.train$maxPrice - data.train$minPrice
  # price variance ratio of maxprice and minPrice to maxPrice per pid (= maxMinpriceVariance/maxPrice)
  data.train$maxMinpriceVarianceRatioToMaxPrice <- data.train$maxMinpriceVariance/data.train$maxPrice
  # absolute price variance of meanPrice and price per pid 
  data.train$priceMeanPriceVariance <- data.train$meanPricePerProduct - data.train$price
  # price variance ratio of meanPrice and price to meanPrice per pid
  data.train$meanPricePriceVarianceToMeanPrice <- data.train$priceMeanPriceVariance/data.train$meanPricePerProduct
  ##################### Start: UNTESTED!!! #####################
  
  ## features based on dailyPriceDifference
  data.train <- merge(data.train, getDailyPriceDifferenceData(), all.x=TRUE, by=c("lineID"))

  # daily price change 
  data.train$dailyPriceChange <- abs(data.train$dailyPriceDifference)
  
  # used for the following features
  dailyPriceDifferencePerDayAndPid <- setNames(data.table(aggregate(data.train$dailyPriceDifference, by=list(data.train$day, data.train$pid), FUN=mean, na.rm=TRUE)), c("day", "pid", "dailyPriceDifference"))
  dailyPriceChangePerDayAndPid <- setNames(data.table(aggregate(data.train$dailyPriceChange, by=list(data.train$day, data.train$pid), FUN=mean, na.rm=TRUE)), c("day", "pid", "dailyPriceChange"))
  
  # mean daily price change
  data.train <- merge(data.train, setnames(data.table(aggregate(dailyPriceChangePerDayAndPid$dailyPriceChange, list(dailyPriceChangePerDayAndPid$pid), FUN=mean, na.rm=TRUE)), c("pid", "meanDailyPriceChange")), all.x=TRUE, by=c("pid")) 
  # mean daily price difference
  data.train <- merge(data.train, setnames(data.table(aggregate(dailyPriceDifferencePerDayAndPid$dailyPriceDifference, list(dailyPriceDifferencePerDayAndPid$pid), FUN=mean, na.rm=TRUE)), c("pid", "meanDailyPriceDifference")), all.x=TRUE, by=c("pid"))
  # max daily price difference per pid
  data.train <- merge(data.train, setnames(data.table(aggregate(dailyPriceDifferencePerDayAndPid$dailyPriceDifference, list(dailyPriceDifferencePerDayAndPid$pid), FUN=max, na.rm=TRUE)), c("pid", "maxDailyPriceDifference")), all.x=TRUE, by=c("pid"))
  # min daily price difference per pid
  data.train <- merge(data.train, setnames(data.table(aggregate(dailyPriceDifferencePerDayAndPid$dailyPriceDifference, list(dailyPriceDifferencePerDayAndPid$pid), FUN=min, na.rm=TRUE)), c("pid", "minDailyPriceDifference")), all.x=TRUE, by=c("pid"))
  # max daily price change
  data.train <- merge(data.train, setnames(data.table(aggregate(dailyPriceChangePerDayAndPid$dailyPriceChange, list(dailyPriceChangePerDayAndPid$pid), FUN=max, na.rm=TRUE)), c("pid", "maxDailyPriceChange")), all.x=TRUE, by=c("pid"))
  # min daily price change
  data.train <- merge(data.train, setnames(data.table(aggregate(dailyPriceChangePerDayAndPid$dailyPriceChange, list(dailyPriceChangePerDayAndPid$pid), FUN=min, na.rm=TRUE)), c("pid", "minDailyPriceChange")), all.x=TRUE, by=c("pid"))
  # range of daily price difference
  data.train$dailyPriceDifferenceRange <- data.train$maxDailyPriceDifference - data.train$minDailyPriceDifference
  # range of daily price change
  data.train$dailyPriceChangeRange <- data.train$maxDailyPriceChange - data.train$minDailyPriceChange
  # total number of negative daily price differences (= when the price decreased)
  data.train <- merge(data.train, setNames(data.table(aggregate(dailyPriceDifferencePerDayAndPid[!is.na(dailyPriceDifference) & dailyPriceDifference < 0]$dailyPriceDifference, by=list(dailyPriceDifferencePerDayAndPid[!is.na(dailyPriceDifference) & dailyPriceDifference < 0]$pid), FUN=length)), c("pid", "totalNumberOfNegativePriceDifferencesOverTime")), all.x=TRUE, by=("pid"))
  # counter of positive daily price differencs (= when the price increased)
  data.train <- merge(data.train, setNames(data.table(aggregate(dailyPriceDifferencePerDayAndPid[!is.na(dailyPriceDifference) & dailyPriceDifference > 0]$dailyPriceDifference, by=list(dailyPriceDifferencePerDayAndPid[!is.na(dailyPriceDifference) & dailyPriceDifference > 0]$pid), FUN=length)), c("pid", "totalNumberOfPositivePriceDifferencesOverTime")), all.x=TRUE, by=("pid"))
  # counter of daily price differences = 0 (= when the price didn't change)
  data.train <- merge(data.train, setNames(data.table(aggregate(dailyPriceDifferencePerDayAndPid[!is.na(dailyPriceDifference) & dailyPriceDifference == 0]$dailyPriceDifference, by=list(dailyPriceDifferencePerDayAndPid[!is.na(dailyPriceDifference) & dailyPriceDifference == 0]$pid), FUN=length)), c("pid", "totalNumberOfConstantPriceDifferencesOverTime")), all.x=TRUE, by=("pid"))
  # counter of daily prices != 0 (how often did it change)
  data.train <- merge(data.train, setNames(data.table(aggregate(dailyPriceDifferencePerDayAndPid[!is.na(dailyPriceDifference) & dailyPriceDifference != 0]$dailyPriceDifference, by=list(dailyPriceDifferencePerDayAndPid[!is.na(dailyPriceDifference) & dailyPriceDifference != 0]$pid), FUN=length)), c("pid", "totalNumberOfNonConstantPriceDifferencesOverTime")), all.x=TRUE, by=("pid"))
  # flexibility of the daily prices
  data.train$dailyPriceFlexibility <- data.train$totalNumberOfNonConstantPriceDifferencesOverTime / (data.train$totalNumberOfConstantPriceDifferencesOverTime + data.train$totalNumberOfNonConstantPricesOverTime)
  # ratio of daily price difference to price
  data.train$dailyPriceDifferenceRatioToPrice <- data.train$dailyPriceDifference / data.train$price
  # ratio of daily price difference to mean price
  data.train$dailyPriceDifferenceRatioToMeanPrice <- data.train$dailyPriceDifference / data.train$meanPricePerProduct
  # ratio of daily price difference to max price
  data.train$dailyPriceDifferenceRatioToMaxPrice <- data.train$dailyPriceDifference / data.train$maxPrice
  # discretized daily price difference
  data.train$dailyPriceDifferenceDiscretized <- unlist(lapply(data.train$dailyPriceDifference, function(priceDifference){
    if(is.na(priceDifference)){
      NA
    }
    else if(priceDifference < 0){
      "decreased"
    }
    else if(priceDifference > 0){
      "increased"
    }
    else{
      "constant"
    }
  }))
  
  print("daily difference of competitorPrice")
  
  # daily difference of competitorPrice
  data.train <- merge(data.train, getDailyCompetitorPriceDifferenceData(), all.x=TRUE, by=c("lineID"))
  
  # daily change of competitorPrice
  data.train$dailyCompetitorPriceChange <- abs(data.train$dailyCompetitorPriceDifference) 
  
  print("ratios")
  
  # difference between dailyPriceDifference and dailyCompetitorPriceDifference
  data.train$differenceDailyPriceDifferenceCompetitorPriceDifference <- data.train$dailyPriceDifference - data.train$dailyCompetitorPriceDifference
  # ratio of differenceDailyPriceDifferenceCompetitorPriceDifference to dailyPriceDifference
  data.train$differenceDailyPriceDifferenceCompetitorPriceDifferenceRatioToDailyPriceDifference <- data.train$differenceDailyPriceDifferenceCompetitorPriceDifference / data.train$dailyPriceDifference
  # ratio of differenceDailyPriceDifferenceCompetitorPriceDifference to comPetitorPriceDifference
  data.train$differenceDailyPriceDifferenceCompetitorPriceDifferenceRatioToDailyCompetitorPriceDifference  <- data.train$differenceDailyPriceDifferenceCompetitorPriceDifference / data.train$dailyCompetitorPriceDifference
  
  print("Currently MISSING: justGotCheaperThanCompetitors")
  
#  dailyCompetitorPriceDifferencePerDayAndPid <- order(setNames(data.table(aggregate(data.train$dailyCompetitorPriceDifference, by=list(data.train$day, data.train$pid), FUN=mean, na.rm=TRUE)), c("day", "pid", "dailyCompetitorPriceDifference")), cols=c(day, pid))
#  dailyCompetitorPricePerDayAndPid <- order(setNames(data.table(aggregate(data.train$competitorPrice, by=list(data.train$day, data.train$pid), FUN=mean, na.rm=TRUE)), c("day", "pid", "competitorPrice")), cols=c(day, pid))
#  dailyPricePerDayAndPid <- order(setNames(data.table(aggregate(data.train$price, by=list(data.train$day, data.train$pid), FUN=mean, na.rm=TRUE)), c("day", "pid", "price")), cols=c(day, pid))
#  dailyCompetitorPriceCompetitorPriceDifferencePricePerDayAndPid <- order(merge(dailyCompetitorPriceDifferencePerDayAndPid, merge(dailyCompetitorPricePerDayAndPid, dailyPricePerDayAndPid, all.x=TRUE, by=c("day", "pid")), all.x=TRUE, by=c("day", "pid")), cols=c(day, pid))
  
#  dailyCompetitorPriceDifferencePerDayAndPid$justGotCheaperThanCompetitors <- vapply(1:nrow(dailyCompetitorPriceDifferencePerDayAndPid), function(i){
#    if(is.na(dailyCompetitorPriceCompetitorPriceDifferencePricePerDayAndPid[i]$competitorPrice)){
#      NA
#    }
#    else if(is.na(dailyCompetitorPriceCompetitorPriceDifferencePricePerDayAndPid[i]$dailyCompetitorPriceDifference)){
#      NA
#    }
#    else if((dailyCompetitorPriceCompetitorPriceDifferencePricePerDayAndPid[i]$competitorPrice - dailyCompetitorPriceCompetitorPriceDifferencePricePerDayAndPid[i]$dailyCompetitorPriceDifference > dailyCompetitorPriceCompetitorPriceDifferencePricePerDayAndPid[i]$price) & dailyCompetitorPriceCompetitorPriceDifferencePricePerDayAndPid[i]$price - dailyCompetitorPriceCompetitorPriceDifferencePricePerDayAndPid[i]$dailyPriceDifference > dailyCompetitorPriceCompetitorPriceDifferencePricePerDayAndPid[i]$competitorPrice - dailyCompetitorPriceCompetitorPriceDifferencePricePerDayAndPid[i]$dailyCompetitorPrice){
#      1
#    }
#    else{
#      0
#    }
#  }, FUN.VALUE = numeric(1))
  
#  data.train <- merge(data.train, dailyCompetitorPriceDifferencePerDayAndPid[, c("day", "pid", "justGotCheaperThanCompetitors")], all.x=TRUE, by=c("day", "pid")) 
  
  # INEFFICIENT:
  # indicates if the product just got cheaper than the competitors
#  data.train$justGotCheaperThanCompetitors <- vapply(1:nrow(data.train), function(i){
#    if(is.na(data.train[i]$competitorPrice)){
#      NA
#    }
#    else if(is.na(data.train[i]$dailyCompetitorPriceDifference)){
#      NA
#    }
#    else if((data.train[i]$competitorPrice + data.train[i]$dailyCompetitorPriceDifference) > data.train[i]$price){
#      1
#    }
#    else{
#      0
#    }
#  }, FUN.VALUE = numeric(1))
  
  print("Currently MISSING: justGotMoreExpensiveThanCompetitors")
  
#  dailyCompetitorPriceDifferencePerDayAndPid$justGotMoreExpensiveThanCompetitors <- vapply(1:nrow(dailyCompetitorPriceDifferencePerDayAndPid), function(i){
#    if(is.na(dailyCompetitorPricePerDayAndPid[i]$competitorPrice)){
#      NA
#    }
#    else if(is.na(dailyCompetitorPriceDifferencePerDayAndPid[i]$dailyCompetitorPriceDifference)){
#      NA
#    }
#    else if((dailyCompetitorPricePerDayAndPid[i]$competitorPrice + dailyCompetitorPriceDifferencePerDayAndPid[i]$dailyCompetitorPriceDifference) < dailyPricePerDayAndPid[i]$price){
#      1
#    }
#    else{
#      0
#    }
#  }, FUN.VALUE = numeric(1))
  
 # data.train <- merge(data.train, dailyCompetitorPriceDifferencePerDayAndPid[, c("day", "pid", "justGotMoreExpensiveThanCompetitors")], all.x=TRUE, by=c("day", "pid")) 
  
  
  
  
  
  # INEFFICIENT:
  # indicates if the product just got more expensive than the competitors
#  data.train$justGotMoreExpensiveThanCompetitors <- vapply(1:nrow(data.train), function(i){
#    if(is.na(data.train[i]$competitorPrice)){
#      NA
#    }
#    else if(is.na(data.train[i]$dailyCompetitorPriceDifference)){
#      NA
#    }
#    else if((data.train[i]$competitorPrice + data.train[i]$dailyCompetitorPriceDifference) < data.train[i]$price){
#      1
#    }
#    else{
#      0
#    }
#  }, FUN.VALUE = numeric(1))
  
  
  ##features based on: amountAlreadyBoughtOnSameDay
  # infdicates if amountAlreadyBoughtOnSameDay is at least 1 or not
#  data.train$alreadyBoughtOnSameDay <- lapply(data.train$amountAlreadyBoughtOnSameDay, function(i){
#    if(i > 0){
#      1
#    }
#    else{
#      0
#    }
#  })
  # mean daily order per pid
  #TODO
  # ratio of amountAlreadyBoughtOnSameDay to meanDailyOrder
#  data.train$amountAlreadyBoughtOnSameDayRatioToMeanDailyOrder <- data.train$amountAlreadyBoughtOnSameDay / data.train%meanDailyOrder
  # amountAlreadyBoughtOnSameDay including the current order (if action is an order action)
#  data.train$amountAlreadyBoughtOnSameDayIncludingCurrent <- data.train$amountAlreadyBoughtOnSameDay + data.train$order
  
  # quantity already bought on same day
  # TODO
  # mean of the daily quantity
  # TODo
  # ratio of quantityAlreadyBoughtOnSameDay to meanDailyQuantity
 # data.train$quantityAlreadyBoughtOnSameDayRatioToMeanDailyQuantity <- data.train$quantityAlreadyBoughtOnSameDay / data.train$meanDailyQuantity
  #quantityAlreadyBoughtOnSameDay including current
  #data.train$quantityAlreadyBoughtOnSameDayIncludingCurrent <- data.train$quantityAlreadyBoughtOnSameDay + data.train$quantitydata.train$quantityAlreadyBoughtOnSameDay
  
  
  ##################### End: UNTESTED!!! #####################
  
  ##############  Start: FEATURES WITH HIGH CREATION TIME (commented out due to this):   ##############
  
  
  # daily price difference for each pid
  #dailyPriceDiff <- {
  #  pids <- unique(data.train$pid)
  #  pricePerPidAndDay <- unique(data.train[, c("pid", "day")])
  #  pricePerPidAndDay$price <- data.table(aggregate(x = data.train$price, by = list(pid = data.train$pid, day = data.train$day), FUN= mean))[order(pid, day)]$x     #aggregate(x = data.all$quantity, by = list(numberOfPackages = data.all$numberOfPackages), FUN = mean, na.rm=TRUE)
  #  result <- c()
  #  c(result, sapply(1:length(pids), function(i){
  #    if(i %% 1000 == 0){
  #      print(paste0(round(i/21758 * 100, 2), "%"))
  #    }
  #    currentPidData <- pricePerPidAndDay[pid == pids[i]]
  #    priceDiffsForCurrentPidData <- vapply(1:nrow(currentPidData), function(j){
  #      if(j == 1){
  #        NA
  #      }
  #      else{
  #        currentPidData[j]$price - currentPidData[j-1]$price
  #      }
  #    }, FUN.VALUE = numeric(1))
  #    priceDiffsForCurrentPidData
  #  }))
  #}
  #pricePerPidAndDay$dailyPriceDifference <- unlist(dailyPriceDiff)
  #data.train <- merge(data.train, pricePerPidAndDay[, c("pid", "day", "dailyPriceDifference")], all.x=TRUE, by=c("pid", "day"))
  
  
  ##############  End: FEATURES WITH HIGH CREATION TIME (commented out due to this):   ##############
  
  
  
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
