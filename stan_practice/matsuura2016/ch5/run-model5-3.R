#####run-model5-3.R#####
library(rstan)

d <- read.csv(file = 'input/data-attendance-1.txt')
(data <- list(N=nrow(d), A=d$A, Score=d$Score/200, Y=d$Y))
(fit <- stan(file = 'model/model5-3.stan', data=data, seed=1234))
save.image('output/result-model5-3.RData')
#####rstan-save-diagnostics#####
library(rstan)
library(ggmcmc)

write.table(data.frame(summary(fit)$summary),
            file="output/result-model4-5.RData", sep="\t", quote=FALSE, col.names=NA)

ggmcmc(ggs(fit, inc_warmup=TRUE,stan_include_auxiliar=TRUE),
       file="output/5-3_fit-traceplot.pdf",plot="traceplot")

ggmcmc(ggs(fit),file="output/5-3_fit-ggmcmc.pdf")    
