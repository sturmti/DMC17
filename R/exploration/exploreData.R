source("utils/loadData.R")
source("preprocessing/featureEngineering.R")
source("preprocessing/dataCleaning.R")
source("utils/initializeData.R")
source("utils/storeData.R")
library(ggplot2)
install.packages("gdata")
library(gdata)

####### Initialization ####### 
data.items <- getItemData()
data.train <- getTrainData(TRUE)
data.class <- getClassData()

data.train <- initializeDataTrain(TRUE)
data.items <- initializeDataItems()

data.all <- initializeJoinedData(TRUE)

####### CSV File Creation ####### 
storeData(data = data.items, file.name = "items_v1.5.csv")
storeData(data = data.train, file.name = "train_v1.6.csv")
storeData(data = data.all, file.name = "trainItems_v1.6.csv")


#3480: record with content == "40X0.5"
data.items[content == "40X0.5"]
class(data.items$numberOfPackages)
class(data.items$quantityByPackage)







ggplot(data = data.train[pid==11575], aes(x = day, y = competitorPrice)) + geom_line()

ggplot(data.train[pid== 21086], aes(day)) + 
  geom_line(aes(y = competitorPrice, colour = "competitorPrice")) + 
  geom_line(aes(y = revenue, colour = "revenue")) +
  geom_line(aes(y = price, colour = "price"))


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



############### Start: Daily Price Difference ############### 

### FINAL: ###
data.train.with.daily.pricedifference <- {
  pids <- unique(data.train$pid)
  pricePerPidAndDay <- unique(data.train[, c("pid", "day")])
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
  pricePerPidAndDay$dailyPriceDifference <- unlist(dailyPriceDiff)
  merge(data.train, pricePerPidAndDay[, c("pid", "day", "dailyPriceDifference")], all.x=TRUE, by=c("pid", "day"))
}

############### End: Daily Price Difference ############### 



############### Start: amountAlreadyBoughtOnSameDay ############### 
 
##### FINAL: ##### Nur [1:100] muss entfernt werden!
amountAlreadyBoughtOnSameDay <- {
  result <- c()
  result <- c(result, vapply(20:max(data.train[day < 22]$day), function(i){
    print(paste0("i: ", i))
    all.actions.per.day <- data.train[day < 22][day == i][1:100]
    bought.products.per.day <- data.train[day < 22][order == 1 & day == i][1:100]
    already.bought.products.for.each.action.per.day <- vapply(1:nrow(all.actions.per.day), function(j){
      print(paste0("j: ", j))
      nrow(bought.products.per.day[lineID < all.actions.per.day[j]$lineID & pid == all.actions.per.day[j]$pid])
    }, FUN.VALUE = numeric(1))
    already.bought.products.for.each.action.per.day
  }, FUN.VALUE = numeric(nrow(data.train[day == i][1:100]))))
}

############### End: amountBoughtOnSameDay ###############


