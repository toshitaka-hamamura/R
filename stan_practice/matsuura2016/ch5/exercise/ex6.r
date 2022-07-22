#ex.6.r#
rm(list = ls())
getwd()
setwd("C:/Users/toshi/OneDrive/R/stan_practice/matsuura2016/ch5/exercise")
library(rstan)
d <- read.csv(file = 'data3a.csv', header = TRUE)
head(d)

d$f <- as.numeric(d$f)
(data <- list(N = nrow(d), Y = d$y, X = d$x, F = d$f))
(fit <- stan(file = 'ex6.stan', data = data, seed = 321))

#ex_ans.6.r#
rm(list = ls())
library(rstan)
d <- read.csv(file='data3a.csv')
d_conv <- data.frame(X=c(0, 1))
rownames(d_conv) <- c('C', 'T')
(data <- list(N=nrow(d), Y=d$y, X=d$x, F=d_conv[d$f,]))
(fit <- stan(file='ex6_ans.stan', data=data, seed=1234))
