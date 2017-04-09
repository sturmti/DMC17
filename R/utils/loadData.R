if (!require(data.table)) install.packages("data.table")

### Import Data
###   - dataTrain: import of train.csv
###   - dataItems: import of items.csv
###   - dataClass: import class.csv
dataTrain <- data.table(read.csv(file = "/Users/TimoSturm/Documents/Masterstudium/2. Semester/Data Mining II/Data Mining Cup 2017/DMC_2017_task/train.csv", header = T, sep = "|"))
dataItems <- data.table(read.csv(file = "/Users/TimoSturm/Documents/Masterstudium/2. Semester/Data Mining II/Data Mining Cup 2017/DMC_2017_task/items.csv", header = T, sep = "|"))
dataClass <- data.table(read.csv(file = "/Users/TimoSturm/Documents/Masterstudium/2. Semester/Data Mining II/Data Mining Cup 2017/DMC_2017_task/class.csv", header = T, sep = "|"))


dataTrain
