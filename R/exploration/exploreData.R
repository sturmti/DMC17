source("utils/loadData.R")
source("preprocessing/featureEngineering.R")
source("utils/initializeData.R")
source("utils/storeData.R")

####### Initialization ####### 
data.items <- getItemData()
data.train <- getTrainData()
data.class <- getClassData()

data.train <- initializeDataTrain()
data.items <- initializeDataItems()

data.all <- initializeJoinedData()

####### CSV File Creation ####### 
storeData(data = data.all, file.name = "trainItems_v1.2.csv")

#3480: record with content == "40X0.5"
data.items[content == "40X0.5"]
class(data.items$numberOfPackages)
class(data.items$quantityByPackage)



data.train[pid==10]

dailyPriceDifference <- c()
for(current.pid in unique(data.train$pid)){
  current.data.train <- data.train[pid == current.pid]
  preceding.price <- NULL
  for(current.line in as.numeric(data.train$lineID)){
    append(dailyPriceDifference, ifelse(is.null(preceding.price), 0, (preceding.price - data.train[lineID==current.line]$price)))
    preceding.price <- data.train[lineID==current.line]$price
  }
}
dailyPriceDifference



setNames(aggregate(x = data.train$price, by = list(pid = data.train$pid), FUN = mean), c("pid", "meanPricePerProduct"))
sum(data.train[pid==273]$price)/nrow(data.train[pid==273])

data.train[pid==273]$meanPricePerProduct
class(data.train)

data.items$quantityByPackage <- as.character(data.items$quantityByPackage)

