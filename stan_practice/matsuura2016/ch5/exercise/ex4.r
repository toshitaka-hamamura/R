#ex4.r#
rm(list = ls())
getwd()
library(rstan)
setwd("C:/Users/toshi/OneDrive/R/stan_practice/matsuura2016/ch5")
d <- read.csv(file = 'input/data-attendance-3.txt')
head(d)
is.factor(d$Weather)
as.numeric(d$Weather)

conv <- c(1, 2, 3)
names(conv) <- c('A', 'B', 'C')
(data <- list(I=nrow(d), A=d$A, Score=d$Score/200, W=conv[d$Weather], Y=d$Y))
(fit <- stan(file='exercise/ex4.stan', data=data, seed=1234))
# fit <- stan(file='model/model5-5b.stan', data=data, seed=1234)
save.image('exercise/ex4.RData')

####answer#### <- correct!

library(rstan)

d <- read.csv(file='../input/data-attendance-3.txt')
d_conv <- data.frame(X=c(1, 2, 3))
rownames(d_conv) <- c('A', 'B', 'C')
data <- list(I=nrow(d), A=d$A, Score=d$Score/200, WID=d_conv[d$Weather, ], Y=d$Y)

fit <- stan(file='ex4.stan', data=data, pars=c('b', 'bw2', 'bw3'), seed=1234)

