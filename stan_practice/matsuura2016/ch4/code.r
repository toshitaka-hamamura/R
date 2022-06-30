#####ch4 r codes#####
rm(list=ls())#remove all lists
setwd("C:/Users/toshi/OneDrive/R/stan_practice/matsuura2016/ch4")
#####lm.R#####
d <- read.csv(file='data-salary.txt')
res_lm<-lm(Y~X,data = d)
X_new<-data.frame(X=23:60)
conf_95<-predict(res_lm,X_new,interval="confidence",level=0.95)
pred_95<-predict(res_lm,X_new,interval="prediction",level=0.95)

#####run-model4-5.r#####
d<-read.csv('C:/Users/toshi/OneDrive/R/stan_practice/matsuura2016/data-salary.txt')
data<-list(N=nrow(d),X=d$X,Y=d$Y)
fit<-stan(file='C:/Users/toshi/OneDrive/R/stan_practice/matsuura2016/model4-5.stan',data=data,seed=1234)
save.image(file = 'C:/Users/toshi/OneDrive/R/stan_practice/matsuura2016/output/result-model4-5.RData')

#####rstan-save-diagnostics#####
library(rstan)
library(ggmcmc)

load("C:/Users/toshi/OneDrive/R/stan_practice/matsuura2016/output/result-model4-5.RData")

write.table(data.frame(summary(fit)$summary),
            file="C:/Users/toshi/OneDrive/R/stan_practice/matsuura2016/output/result-model4-5.RData", sep="\t", quote=FALSE, col.names=NA)

ggmcmc(ggs(fit, inc_warmup=TRUE,stan_include_auxiliar=TRUE),
       file="C:/Users/toshi/OneDrive/R/stan_practice/matsuura2016/output/fit-traceplot.pdf",plot="traceplot")

ggmcmc(ggs(fit),file="C:/Users/toshi/OneDrive/R/stan_practice/matsuura2016/output/fit-ggmcmc.pdf")       

#####rstan-modify-MCMCsettings.R#####
library(rstan)

d<-read.csv('C:/Users/toshi/OneDrive/R/stan_practice/matsuura2016/data-salary.txt')
data<-list(N=nrow(d),X=d$X,Y=d$Y)

stanmodel<-stan_model(file='C:/Users/toshi/OneDrive/R/stan_practice/matsuura2016/model4-5.stan')

fit<-sampling(
  stanmodel,
  data=data,
  pars=c("a","b","sigma"),
  init=function(){
    list(a=runif(1,-10,10),b=runif(1,0,10),sigma=10)
  },
  seed=123,
  chains=4,iter=1000,warmup=200,thin=2
)

#####rtan-extract-MCMCsamples.R#####
load("output/result-model4-5.RData")
ms<-rstan::extract(fit)
ms$b
quantile(ms$b,probs=c(0.025, 0.975))
d_mcmc<-data.frame(a=ms$a, b=ms$b, sigma=ms$sigma)
head(d_mcmc)
plot(d_mcmc$a,d_mcmc$b)
hist(d_mcmc$a,breaks = 100)
hist(d_mcmc$b,breaks = 100)

#####rstan-extract-MCMCsamples.R#####
N_mcmc <- length(ms$lp__)
y50_base <- ms$a + ms$b * 50
y50 <- rnorm(n=N_mcmc, mean=y50_base, sd=ms$sigma)
d_mcmc <- data.frame(a=ms$a, b=ms$b, sigma=ms$sigma, y50_base, y50)

#####run-model4-4.R#####
d<-read.csv(file = "input/data-salary.txt")
(X_new<-23:60)
(data<-list(N=nrow(d),X=d$X,Y=d$Y,N_new=length(X_new),X_new=X_new))
(fit<-stan(file="model/model4-4.stan",data=data,seed=1234))
(ms<-rstan::extract(fit))

data.frame.quantile.mcmc <- function(x, y_mcmc,
                                     probs = c(2.5, 25, 50, 75, 97.5)/100) {
  qua <- apply(y_mcmc, 2, quantile, probs=probs)
  d <- data.frame(X=x, t(qua))
  colnames(d) <- c("X", paste0("p", probs*100))
  return(d)
}

ggplot.5quantile <- function(data) {
  p <- ggplot(data=data, aes(x=X, y=p50))
  p <- p + theme_bw(base_size = 18)
  p <- p + geom_ribbon(aes(ymin = p2.5, ymax = p97.5), fill = "black", alpha = 1/6)
  p <- p + geom_ribbon(aes(ymin = p25, ymax = p75), fill = "black", alpha = 2/6)
  p <- p + geom_line(size = 1)
  return(p)
}

customize.ggplot.axis <- function(p) {
  p <- p + labs(x = "X", y = "Y")
  p <- p + scale_y_continuous(breaks = seq(from = 200, to = 1400, by = 400))
  p <- p + coord_cartesian(xlim = c(22, 61), ylim = c(200, 1400))
  return(p)
}  

(d_est <- data.frame.quantile.mcmc(x=X_new, y_mcmc = ms$y_base_new))
(p <- ggplot.5quantile(data = d_est))
(p <- p + geom_point(data = d, aes(x = X, y= Y), shape = 1, size = 3))
(p <- customize.ggplot.axis(p))
ggsave(file = "output/fig4-8-right-2.png", plot = p, dpi = 300, w = 4, h = 3)

(head(ms$y_new[,1:4]))

