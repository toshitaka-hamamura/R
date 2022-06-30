######ch4ex5.R######
rm(list = ls())
library(rstan)
library(ggmcmc)
getwd()
source('matsuura2016/ch4/exercise/generate-data.R')
(data <- list(N1=N1, N2=N2, Y1=Y1, Y2=Y2))
(fit <- stan(file = 'ex5.stan', data = data, seed = 1111))

(ms <- rstan::extract(fit))
names(ms)
mean(ms$mu1 < ms$mu2)
sum(ms$mu1 < ms$mu2)/length(ms$mu1)

#####answer#####
library(rstan)
source('matsuura2016/ch4/exercise/generate-data.R')
(data <- list(N1=N1, N2=N2, Y1=Y1, Y2=Y2))
fit <- stan(file='matsuura2016/ch4/exercise/ex5.stan', data=data, seed=1234)
save.image(file = 'matsuura2016/ch4/exercise/ex5_ans.RData')
(ms <- rstan::extract(fit))
(prob <- mean(ms$mu1 < ms$mu2))  #=> 0.9457
