#####exercise 3#####
rm(list=ls())#remove all lists
setwd("C:/Users/toshi/OneDrive/R/stan_practice/matsuura2016/ch4/exercise")

library(rstan)
set.seed(123)
N1 <- 30
N2 <- 20
Y1 <- rnorm(n=N1, mean=0, sd=5)
Y2 <- rnorm(n=N2, mean=1, sd=4)

d1 <- data.frame(group=1, Y=Y1)
d2 <- data.frame(group=2, Y=Y2)
d <- rbind(d1, d2)
d$group <- as.factor(d$group)

data<-list(N=nrow(d),group=d$group,Y=d$Y)

fit<-stan(file='ex3.stan',data=data,seed=1234)
save.image(file = 'result-ex3.RData')

#####exercise 3:answer#####
library(rstan)
library(ggmcmc)

source('generate-data.R')

(data <- list(N1=N1, N2=N2, Y1=Y1, Y2=Y2))
(fit <- stan(file='ex3_ans.stan', data=data, seed=1234))

save.image('result-ex3_ans.RData')

#####follow up#####
write.table(data.frame(summary(fit)$summary),
            file="result-ex3_ans.RData", sep="\t", quote=FALSE, col.names=NA)
ggmcmc(ggs(fit),file="fit-ggmcmc_ex3.pdf")       
