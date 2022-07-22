#ex7.r#
rm(list = ls())
getwd()
setwd("C:/Users/toshi/OneDrive/R/stan_practice/matsuura2016/ch5/exercise")
library(rstan)
d <- read.csv(file = 'data4a.csv')
(d_conv <- data.frame(X = c(0,1)))
(rownames(d_conv) <- c('C', 'T'))

(data <- list(N = nrow(d), A = d$N, Y = d$y, X = d$x, F = d_conv[d$f,]))
(fit <- stan(file = 'ex7.stan', data = data, seed = 123))
save.image(file = 'ex7.RData')
#diagnotic#
library(ggmcmc)
load("ex7.RData")
write.table(data.frame(summary(fit)$summary),
            file="ex7.RData", sep=",", quote=FALSE, col.names=NA)
ggmcmc(ggs(fit, inc_warmup=TRUE,stan_include_auxiliar=TRUE),
       file="ex7_fit-traceplot.pdf",plot="traceplot")
ggmcmc(ggs(fit),file="ex7_fit-ggmcmc.pdf")       

(exp(-19.66)*exp(1.96*9.76)*exp(2.03*0))/(exp(-19.66)*exp(1.96*9.76)*exp(2.03*1))
(exp(-19.66)*exp(1.96*10.48)*exp(2.03*1))/(exp(-19.66)*exp(1.96*10.83)*exp(2.03*1))

head(d)

#####answer####
library(rstan)
setwd("C:/Users/toshi/OneDrive/R/stan_practice/matsuura2016/ch5/exercise")
d <- read.csv(file='data4a.csv')
d_conv <- data.frame(X=c(0, 1))
rownames(d_conv) <- c('C', 'T')
data <- list(I=nrow(d), N=d$N, Y=d$y, X=d$x, F=d_conv[d$f,])
fit <- stan(file='ex7_ans.stan', data=data, seed=1234)
