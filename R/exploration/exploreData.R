source("utils/loadData.R")
source("preprocessing/featureEngineering.R")
source("preprocessing/dataCleaning.R")
source("utils/initializeData.R")
source("utils/storeData.R")
library(ggplot2)
install.packages("gdata")
library(gdata)
library(plyr)

####### Initialization ####### 
data.items <- getItemData()
data.train <- getTrainData()
data.class <- getClassData()
data.basket <- getBasketData()
data.alex <- getAlexData()
data.all.with.alex <- merge(data.alex, data.all, all.x=TRUE, by=c("lineID"))
data.dailyPriceDifference <- getDailyPriceDifferenceData()

data.train <- initializeDataTrain(TRUE)
data.items <- initializeDataItems()

data.all <- initializeJoinedData(TRUE)

####### CSV File Creation ####### 
storeData(data = data.items, file.name = "items_v1.5.csv")
storeData(data = data.train, file.name = "train_2.0.csv")
storeData(data = data.all, file.name = "trainItems_v1.9.csv")


#3480: record with content == "40X0.5"
data.items[content == "40X0.5"]
class(data.items$numberOfPackages)
class(data.items$quantityByPackage)

data.train.with.daily.price.difference[pid == 1]

nrow(data.train.with.daily.price.difference)
nrow(data.train2)

data.train$dailyPriceDifference <- data.dailyPriceDifference[order(lineID)]$dailyPriceDifference
data.train.with.daily.price.difference

abs(data.train2[pid == 1]$dailyPriceDifference)
data.train2[pid == 1]

data.train[, c("day", "pid", "price", "competitorPrice", "dailyPriceDifference", "dailyCompetitorPriceDifference", "justGotCheaperThanCompetitors", "justGotMoreExpensiveThanCompetitors")][pid == 2342]

class(as.character(unique(data.train$dailyPriceDifferenceDiscretized)))

data.all$dailyPriceDifferenceDiscretized <- unlist(data.all$dailyPriceDifferenceDiscretized)

      
abs(data.train2$dailyPriceDifference)

ggplot(data = data.train[pid==11575], aes(x = day, y = competitorPrice)) + geom_line()

ggplot(data.train[pid== 21086], aes(day)) + 
  geom_line(aes(y = competitorPrice, colour = "competitorPrice")) + 
  geom_line(aes(y = revenue, colour = "revenue")) +
  geom_line(aes(y = price, colour = "price"))

data.train

abs(c(NA, NA, 1, -1))
data.CompetitorPriceDifference$dailyCompetitorPriceDifference <- getDailyCompetitorPriceDifferenceData()
abs(data.CompetitorPriceDifference$dailyCompetitorPriceDifference)[2296179]

#################### START: Competitor == 0 Clean up ##########################

## FINAL:
competitorPriceCleanedZeros <- {
  recordsWithzero <- data.train[competitorPrice == 0]
  recordsWithzero$competitorPrice <- sapply(1:nrow(recordsWithzero), function(i){
    recordsWithCurrentPid <- data.train[pid == recordsWithzero[i]$pid]
    row.up <- recordsWithCurrentPid[lineID == recordsWithzero[i]$lineID, which = TRUE]
    row.down <- recordsWithCurrentPid[lineID == recordsWithzero[i]$lineID, which = TRUE]
    result.up <- NULL
    result.down <- NULL
    while(is.null(result.down)){
        print(paste0("row.down: ", row.down))
      if(row.down == 1){
        result.down <- NA
        break
      }
      else if(is.na(recordsWithCurrentPid[row.down]$competitorPrice)){
        row.down <- row.down -1
      }
      else if(recordsWithCurrentPid[row.down]$competitorPrice != 0){
        result.down <- row.down
        break
      }
      else{
        row.down <- row.down -1
      }
    }
    while(is.null(result.up)){
      if(row.up == nrow(recordsWithCurrentPid) +1){
        result.up <- NA
        break
      }
      else if(is.na(recordsWithCurrentPid[row.down]$competitorPrice)){
        row.up <- row.up +1
      }
      else if(recordsWithCurrentPid[row.up]$competitorPrice != 0){
        result.up <- row.up
        break
      }
      else{
        row.up <- row.up +1
      }
    }
    print(paste0("i: ", i))
    if(is.na(result.down)){
      if(is.na(result.up)){
       # print(NA)
        NA
      }
      else{
       # print(paste0("from result.up: ", recordsWithCurrentPid[result.up]$competitorPrice))
        recordsWithCurrentPid[result.up]$competitorPrice
      }
    }
    else{
      if(is.na(result.up)){
       # print(paste0("from result.down: ", recordsWithCurrentPid[result.down]$competitorPrice))
        recordsWithCurrentPid[result.down]$competitorPrice
      }
      else{
        #print(paste0("from calculation: ", as.numeric(ifelse(recordsWithzero[i]$day - recordsWithCurrentPid[result.down]$day <= recordsWithCurrentPid[result.up]$day - recordsWithzero[i]$day, recordsWithCurrentPid[result.down]$competitorPrice, recordsWithCurrentPid[result.up]$competitorPrice))))
        as.numeric(ifelse(recordsWithzero[i]$day - recordsWithCurrentPid[result.down]$day <= recordsWithCurrentPid[result.up]$day - recordsWithzero[i]$day, recordsWithCurrentPid[result.down]$competitorPrice, recordsWithCurrentPid[result.up]$competitorPrice))
      }
    }
  })
  print(recordsWithzero)

  data.train.cleanedCompetitorPrice <- funion(data.train[!(competitorPrice == 0 & !is.na(competitorPrice))], recordsWithzero)
  data.train.cleanedCompetitorPrice <- data.train.cleanedCompetitorPrice[order(lineID)]
  data.train.cleanedCompetitorPrice
}

#################### END: Competitor == 0 Clean up ##########################
?order
data.train

############### Start: Daily Price Difference ############### 

### FINAL: ###
dailyPriceDiff <- {
  pids <- sort(unique(data.train$pid))
  pricePerPidAndDay <- unique(data.train[, c("pid", "day")])[order(cols=pid, day)]
  pricePerPidAndDay$price <- data.table(aggregate(x = data.train$price, by = list(pid = data.train$pid, day = data.train$day), FUN= mean))[order(pid, day)]$x     #aggregate(x = data.all$quantity, by = list(numberOfPackages = data.all$numberOfPackages), FUN = mean, na.rm=TRUE)
  result <- c()
  c(result, sapply(1:length(pids), function(i){
    if(i %% 1000 == 0){
      print(paste0(round(i/21758 * 100, 2), "%"))
    }
    currentPidData <- pricePerPidAndDay[pid == pids[i]]
    priceDiffsForCurrentPidData <- vapply(1:nrow(currentPidData), function(j){
      if(j == 1){
        NA
      }
      else{
        currentPidData[j]$price - currentPidData[j-1]$price
      }
    }, FUN.VALUE = numeric(1))
    priceDiffsForCurrentPidData
  }))
}
pricePerPidAndDay$dailyPriceDifference <- unlist(dailyPriceDiff)
data.train.with.daily.price.difference <- merge(data.train, pricePerPidAndDay[, c("pid", "day", "dailyPriceDifference")], all.x=TRUE, by=c("pid", "day"))
data.train[day == 1][1:10, ]
data.train.with.daily.price.difference[pid == 1][, c("pid", "day", "price", "dailyPriceDifference")]
data.train[pid == 6570]
pricePerPidAndDay.ordered <-  order(pricePerPidAndDay, cols=c("pid"))
pricePerPidAndDay$dailyPriceDifference <- unlist(dailyPriceDiff)

?order
############### End: Daily Price Difference ############### 

############### Start: Daily competitorPrice Difference ############### 

### FINAL: ###
dailyCompetitorPriceDiff <- {
  pids <- sort(unique(data.train$pid))
  competitorPricePerPidAndDay <- unique(data.train[, c("pid", "day")])[order(cols=pid)]
  competitorPricePerPidAndDay$competitorPrice <- data.table(aggregate(x = data.train$competitorPrice, by = list(pid = data.train$pid, day = data.train$day), FUN= mean))[order(pid, day)]$x     #aggregate(x = data.all$quantity, by = list(numberOfPackages = data.all$numberOfPackages), FUN = mean, na.rm=TRUE)
  result <- c()
  c(result, sapply(1:length(pids), function(i){
    if(i %% 1000 == 0){
      print(paste0(round(i/21758 * 100, 2), "%"))
    }
    currentPidData <- competitorPricePerPidAndDay[pid == pids[i]]
    competitorPriceDiffsForCurrentPidData <- vapply(1:nrow(currentPidData), function(j){
      if(j == 1){
        NA
      }
      else if(is.na(currentPidData[j]$competitorPrice)){
        NA
      }
      else if(is.na(currentPidData[j-1]$competitorPrice)){
        NA
      }
      else{
        currentPidData[j]$competitorPrice - currentPidData[j-1]$competitorPrice
      }
    }, FUN.VALUE = numeric(1))
    competitorPriceDiffsForCurrentPidData
  }))
}
competitorPricePerPidAndDay$dailyCompetitorPriceDifference <- unlist(dailyCompetitorPriceDiff)
data.train.with.daily.competitorPrice.difference <- merge(data.train, competitorPricePerPidAndDay[, c("pid", "day", "dailyCompetitorPriceDifference")], all.x=TRUE, by=c("pid", "day"))
data.train.with.daily.price.difference[, c("pid", "day", "price", "dailyPriceDifference")]

############### End: Daily competitorPrice Difference ############### 
data.train.with.daily.competitorPrice.difference[pid == 2537][, c("day","pid","competitorPrice", "dailyCompetitorPriceDifference")]

round(unlist(dailyCompetitorPriceDiff),2)
data.train.ordered <- data.train[order(cols=pid)]
data.train.ordered$dailyCompetitorPriceDifference <- round(unlist(dailyCompetitorPriceDiff),2)
competitorPricePerPidAndDay$dailyCompetitorPriceDifference <- round(unlist(dailyCompetitorPriceDiff),2)



############### Start: amountAlreadyBoughtOnSameDay ############### 
 
##### FINAL: ##### Nur [1:100] muss entfernt werden!
data.train.with.amountAlreadyBoughtOnSameDay <- {
  result <- c()
  result <- c(result, sapply(23:max(data.train$day), function(i){
    #print(i)
    print(paste0(round(((i-22)/72)*100,2), "%"))
    all.actions.per.day <- data.train[day == i]
    bought.products.per.day <- data.train[order == 1 & day == i]
    already.bought.products.for.each.action.per.day <- vapply(1:nrow(all.actions.per.day), function(j){
      #print(paste0("j: ", j))
      nrow(bought.products.per.day[lineID < all.actions.per.day[j]$lineID & pid == all.actions.per.day[j]$pid])
    }, FUN.VALUE = numeric(1))
    already.bought.products.for.each.action.per.day
  }))
  resultTable <- data.train
  resultTable$amountAlreadyBoughtOnSameDay <- result
  resultTable
}

#data.train[day < 25][day == 24][1:100][, c("pid", "day", "order")]

#beschränkter Umfang (= mit [1:100]):
amountAlreadyBoughtOnSameDay <- {
  result <- c()
  result <- c(result, vapply(23:max(data.train[day < 25]$day), function(i){
    print(paste0(round(i/72*100,2), "%"))
    all.actions.per.day <- data.train[day < 25][day == i][1:100]
    bought.products.per.day <- data.train[day < 25][order == 1 & day == i][1:100]
    already.bought.products.for.each.action.per.day <- vapply(1:nrow(all.actions.per.day), function(j){
      print(paste0("j: ", j))
      nrow(bought.products.per.day[lineID < all.actions.per.day[j]$lineID & pid == all.actions.per.day[j]$pid])
    }, FUN.VALUE = numeric(1))
    already.bought.products.for.each.action.per.day
  }, FUN.VALUE = numeric(nrow(data.train[day == i][1:100]))))
}

############### End: amountBoughtOnSameDay ###############




data.train.testSet <- data.train


########## MIGHT WORK!!!! ########## 
dailyPriceChangeCategory <- {
  pids <- sort(unique(data.train.testSet$pid))
  unlist(lapply(pids, function(current.pid){
    print(current.pid)
    data.train.for.current.pid <- data.train.testSet[day < 64][pid == current.pid]
    current.pid.DailyPriceChange.quantiles <- quantile(data.train.for.current.pid[dailyPriceChange != 0]$dailyPriceChange, na.rm=TRUE)
    data.train.for.current.pid.unique <- unique(data.train.for.current.pid[, c("day", "dailyPriceChange")])
    data.train.for.current.pid.unique$dailyPriceChangeCategory <- vapply(1:nrow(data.train.for.current.pid.unique), function(i){
      if(is.na(data.train.for.current.pid.unique[i]$dailyPriceChange)){
        "NA"
      }
      else if(data.train.for.current.pid.unique[i]$dailyPriceChange == 0){
        "none"
      }
      else if(nrow(data.train.for.current.pid.unique) == 1){
        "middle"
      }
      else if(data.train.for.current.pid.unique[i]$dailyPriceChange <= current.pid.DailyPriceChange.quantiles[2]){
        "low"
      }
      else if(data.train.for.current.pid.unique[i]$dailyPriceChange <= current.pid.DailyPriceChange.quantiles[3]){
        "middle"
      }
      else{
        "high"
      }
    }, FUN.VALUE = character(1))
    data.train.for.current.pid <- merge(data.train.for.current.pid, data.train.for.current.pid.unique, all.x=TRUE, by=c("day", "dailyPriceChange"))
    #print(data.train.for.current.pid$dailyPriceChangeCategory)
    data.train.for.current.pid$dailyPriceChangeCategory
  }))
}


nrow(unique(data.train.testSet[, c("pid", "day", "dailyPriceChange")]))


unique(data.train.testSet[pid == pids[63]][, c("day", "dailyPriceChange")][1])

max(data.train$day)

data.train[pid == 185]

print(1:1)



nrow(data.train[order==1][pid==11563])/nrow(data.train2491)


nrow(data.train[pid == 2492][order==1])/nrow(data.train[order==1])




















###### NEW BASKET FEATURES ###################
# Indicator if the basket is an order or a non-order basket (depending on the majority of the contained ordered items)
data.all.with.alex$basket_type <- setnames(data.all.with.alex[,as.numeric(names(which.max(table(order)))),by=basket_id], c("basket_id", "basket_type"))$basket_type


# create basket_id_unique 
data.baskets.unique <- unique(data.basket[, !c("basket_id")])
data.baskets.unique$basket_id_unique <- seq.int(nrow(data.baskets.unique))
data.basket <- merge(data.basket, data.baskets.unique, all.x=TRUE, by=names(data.baskets.unique[, !c("basket_id_unique", "number.of.occurences.per.basket")]))

data.baskets.unique$number.of.occurences.per.basket <- number.of.occurences.per.basket

number.of.occurences.per.basket <- vapply(1:nrow(data.baskets.unique), function(i){
  if(i%%10000 == 0){
    print(i)
  }
 nrow(data.basket[basket_id_unique == data.baskets.unique[i]$basket_id_unique]) 
}, FUN.VALUE=numeric(1))


names(data.all.with.alex)
data.all.with.alex <- merge(data.all.with.alex, data.basket[, c("basket_id", "basket_id_unique", "number.of.occurences.per.basket")], all.x=TRUE, by=c("basket_id"))

data.basket[][number.of.occurences.per.basket == max(data.basket$number.of.occurences.per.basket)]


data.all.with.alex[basket_id_unique == 2][basket_type == 1]
