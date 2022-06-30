####5.3.2.R####
d <- read.csv(file = 'input/data-attendance-3.txt')
aggregate(Y ~ Weather, data = d, FUN = table)
