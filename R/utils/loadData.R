if (!require(data.table)) install.packages("data.table")

### Import Data
###   - data.train: import of train.csv
###   - data.items: import of items.csv
###   - data.class: import class.csv
data.train <- data.table(read.csv(file = "/Users/TimoSturm/Documents/Masterstudium/2. Semester/Data Mining II/Data Mining Cup 2017/DMC_2017_task/train.csv", header = T, sep = "|"))
data.items <- data.table(read.csv(file = "/Users/TimoSturm/Documents/Masterstudium/2. Semester/Data Mining II/Data Mining Cup 2017/DMC_2017_task/items.csv", header = T, sep = "|"))
data.class <- data.table(read.csv(file = "/Users/TimoSturm/Documents/Masterstudium/2. Semester/Data Mining II/Data Mining Cup 2017/DMC_2017_task/class.csv", header = T, sep = "|"))

### factorize the polynominal columns
##    - data.items: factored: all columns
##    - data.train: factored: lineID, pid
##                  ordered: availability
##    - data.class: factored: lineID, pid
##                  ordered: availability
data.items <- data.items[, lapply(.SD, factor)]
data.train$lineID <- factor(data.train$lineID)
data.train$pid <- factor(data.train$pid)
data.train$availability <- ordered(data.train$availability, levels = c(1, 2, 3, 4))
data.class$lineID <- factor(data.class$lineID)
data.class$pid <- factor(data.class$pid)
data.class$availability <- ordered(data.class$availability, levels = c(1, 2, 3, 4))