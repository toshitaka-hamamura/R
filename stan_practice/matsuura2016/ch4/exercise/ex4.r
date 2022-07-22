#####exercise 4#####
rm(list = ls())
library(rstan)
library(ggmcmc)
source('generate-data.R')
(fit <- stan(file='ex3_ans.stan', data=data, seed=1234))
save.image('result-ex3_ans.RData')
load('result-ex3_ans.RData')
ms <- rstan::extract(fit)
true <- ms$mu1 < ms$mu2
length(true[true==TRUE])/length(ms$mu1)

#####exercise 4: answer######
library(rstan)
load('result-ex3_ans.RData')
ms <- rstan::extract(fit)
(prob <- mean(ms$mu1 < ms$mu2))  #=> 0.9325
# N_mcmc <- length(ms$mu1)
# prob <- sum(ms$mu1 < ms$mu2)/N_mcmc  #=> 0.9325