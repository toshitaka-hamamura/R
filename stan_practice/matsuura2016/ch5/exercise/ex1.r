#ex1.r#
rm(list = ls())
library(rstan)
setwd("C:/Users/toshi/OneDrive/R/stan_practice/matsuura2016/ch5")
load(file = 'output/result-model5-3.RData')

ms <- rstan::extract(fit)
names(ms)

N_mcmc <- length(ms$lp__)

for (i in 1:N_mcmc) {
  ep <- ms$y_pred[i] - ms$mu[i]
}

####answer####
# after run-model5-3.R

load('output/result-model5-3.RData')

ms <- rstan::extract(fit)
noise <- t(-t(ms$mu) + d$Y)

