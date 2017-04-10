source("utils/loadData.R")
source("preprocessing/featureEngineering.R")
source("utils/initializeData.R")
library(ggplot2)

data.items <- getItemData()
data.train <- getTrainData()
data.class <- getClassData()


