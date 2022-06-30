#ex.3.r#
setwd("C:/Users/toshi/OneDrive/R/stan_practice/matsuura2016/ch5")

d <- read.csv(file = 'input/data-attendance-3.txt')
head(d)
aggregate(Y ~ A, data = d, FUN = table)

#####answer####
d <- read.csv(file='../input/data-attendance-3.txt')
aggregate(Y ~ A, data=d, FUN=table)

#   A Y.0 Y.1
# 1 0 288 994
# 2 1 386 728