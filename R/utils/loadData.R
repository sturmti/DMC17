if (!require(data.table)) install.packages("data.table")

### Import Data
###   - data.train: import of train.csv
###   - data.items: import of items.csv
###   - data.class: import class.csv
data.train <- data.table(read.csv(file = "/Users/TimoSturm/Documents/Masterstudium/2. Semester/Data Mining II/Data Mining Cup 2017/DMC_2017_task/train.csv", header = T, sep = "|"))
data.items <- data.table(read.csv(file = "/Users/TimoSturm/Documents/Masterstudium/2. Semester/Data Mining II/Data Mining Cup 2017/DMC_2017_task/items.csv", header = T, sep = "|"))
data.class <- data.table(read.csv(file = "/Users/TimoSturm/Documents/Masterstudium/2. Semester/Data Mining II/Data Mining Cup 2017/DMC_2017_task/class.csv", header = T, sep = "|"))

### factorize the polynominal columns
##    - data.items: all columns
##    - data.train:
##    - data.class:

data.items <- factor(data.items)

