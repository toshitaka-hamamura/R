rm(list=ls())#remove all lists
setwd("C:/Users/toshi/OneDrive/R/stan_practice/matsuura2016/ch7")
library(ggplot2)
#####fig.4.2#####
library(ggplot2)
d <- read.csv(file='data-salary.txt')
p <- ggplot(data=d, aes(x=X, y=Y))
p <- p + theme_bw(base_size=18)
p <- p + geom_point(shape=1, size=3)
ggsave(file='output/fig4-2.png', plot=p, dpi=300, w=4, h=3)
#####4.3#####
d <- read.csv(file='data-salary.txt')
res_lm <- lm(Y ~ X, data=d)
X_new <- data.frame(X=23:60)

conf_95 <- predict(res_lm, X_new, interval='confidence', level=0.95)
conf_95 <- data.frame(X_new, conf_95)
conf_50 <- predict(res_lm, X_new, interval='confidence', level=0.50)
conf_50 <- data.frame(X_new, conf_50)
pred_95 <- predict(res_lm, X_new, interval='prediction', level=0.95)
pred_95 <- data.frame(X_new, pred_95)
pred_50 <- predict(res_lm, X_new, interval='prediction', level=0.50)
pred_50 <- data.frame(X_new, pred_50)

p <- ggplot()
p <- p + theme_bw(base_size=18)
p <- p + geom_ribbon(data=conf_95, aes(x=X, ymin=lwr, ymax=upr), alpha=1/6)
p <- p + geom_ribbon(data=conf_50, aes(x=X, ymin=lwr, ymax=upr), alpha=2/6)
(p <- p + geom_line(data=conf_50, aes(x=X, y=fit), size=1))
(p <- p + geom_point(data=d, aes(x=X, y=Y), shape=1, size=3))
(p <- p + labs(x='X', y='Y') + coord_cartesian(xlim=c(22, 61), ylim=c(200, 1400)))
p <- p + scale_y_continuous(breaks=seq(from=200, to=1400, by=400))
ggsave(p, file='output/fig4-3-left.png', dpi=300, w=4, h=3)

p <- ggplot()
p <- p + theme_bw(base_size=18)
p <- p + geom_ribbon(data=pred_95, aes(x=X, ymin=lwr, ymax=upr), alpha=1/6)
p <- p + geom_ribbon(data=pred_50, aes(x=X, ymin=lwr, ymax=upr), alpha=2/6)
p <- p + geom_line(data=pred_50, aes(x=X, y=fit), size=1)
p <- p + geom_point(data=d, aes(x=X, y=Y), shape=1, size=3)
p <- p + labs(x='X', y='Y') + coord_cartesian(xlim=c(22, 61), ylim=c(200, 1400))
p <- p + scale_y_continuous(breaks=seq(from=200, to=1400, by=400))
ggsave(p, file='output/fig4-3-right.png', dpi=300, w=4, h=3)

#####4.4######
library(ggmcmc)

load('output/result-model4-5.RData')

p <- ggs_traceplot(ggs(fit, inc_warmup=TRUE, stan_include_auxiliar=TRUE))
p <- p + theme_bw(base_size=18)
p <- p + scale_colour_manual(values=c('#dcdcdc','#a9a9a9','#696969','#000000'))
p <- p + labs(color='Chain')
ggsave(p, file='output/fig4-4.png', dpi=300, w=7, h=10)

#####4.7: plot and distribution#####
library(ggplot2)
source('../common.R')

load('output/result-model4-5.RData')
ms <- rstan::extract(fit)
df_mcmc <- data.frame(a=ms$a, b=ms$b, sigma=ms$sigma)

p_xy <- ggplot(df_mcmc,aes(x=a,y=b)) +
  theme() +
  geom_point(alpha=1/4, size=2, shape=1) +
  scale_x_continuous(breaks=seq(-400, 200, 200), limits=c(-420, 210)) +
  scale_y_continuous(breaks=seq(15, 25, 5), limits=c(14.5, 29))

p_x <- ggplot(df_mcmc, aes(x=a)) +
  theme() +
  geom_histogram(aes(y=..density..), colour='black', fill='white') +
  geom_density(alpha=0.3, fill='gray20') +
  scale_x_continuous(breaks=seq(-400, 200, 200), limits=c(-420, 210)) +
  labs(x='', y='')

p_y <- ggplot(df_mcmc, aes(x=b)) +
  theme() +
  coord_flip() +
  geom_histogram(aes(y=..density..), colour='black', fill='white') +
  geom_density(alpha=0.3, fill='gray20') +
  scale_x_continuous(breaks=seq(15, 25, 5), limits=c(14.5, 29)) +
  labs(x='', y='')

p_emp <- ggplot(data.frame(0,0)) + 
  theme_bw() + 
  theme(rect=element_rect(fill='white'), panel.border=element_blank())

g_xy <- ggplotGrob(p_xy)
g_x <- ggplotGrob(p_x)
g_y <- ggplotGrob(p_y)
g_emp <- ggplotGrob(p_emp)

g1 <- cbind(g_x, g_emp, size='first')
g2 <- cbind(g_xy, g_y, size='first')
g <- rbind(g1, g2, size='first')
g$widths[1:3] <- grid::unit.pmax(g1$widths[1:3], g2$widths[1:3])
g$heights[8:13] <- g$widths[6:11] <- rep(unit(0.5,'mm'), 6)
g$heights[7] <- g$widths[14] <- unit(3,'cm')

png(file='output/fig4-7.png', res=300, w=1800, h=1800)
grid::grid.draw(g)
dev.off()

#####4.8' MCMC regression line#####
library(ggplot2)
source('../common.R')

load('output/result-model4-5.RData')
ms <- rstan::extract(fit)

X_new <- 23:60
N_X <- length(X_new)
N_mcmc <- length(ms$lp__)

set.seed(1234)
(y_base_mcmc <- as.data.frame(matrix(nrow=N_mcmc, ncol=N_X)))
(y_mcmc <- as.data.frame(matrix(nrow=N_mcmc, ncol=N_X)))
for (i in 1:N_X) {
  y_base_mcmc[,i] <- ms$a + ms$b * X_new[i]
  y_mcmc[,i] <- rnorm(n=N_mcmc, mean=y_base_mcmc[,i], sd=ms$sigma)
}

customize.ggplot.axis <- function(p) {
  p <- p + labs(x='X', y='Y')
  p <- p + scale_y_continuous(breaks=seq(from=200, to=1400, by=400))
  p <- p + coord_cartesian(xlim=c(22, 61), ylim=c(200, 1400))
  return(p)
}

(d_est <- data.frame.quantile.mcmc(x=X_new, y_mcmc=y_base_mcmc))
(p <- ggplot.5quantile(data=d_est))
(p <- p + geom_point(data=d, aes(x=X, y=Y), shape=1, size=3))
(p <- customize.ggplot.axis(p))
ggsave(file='output/fig4-8-left.png', plot=p, dpi=300, w=4, h=3)

d_est <- data.frame.quantile.mcmc(x=X_new, y_mcmc=y_mcmc)
p <- ggplot.5quantile(data=d_est)
p <- p + geom_point(data=d, aes(x=X, y=Y), shape=1, size=3)
p <- customize.ggplot.axis(p)
ggsave(file='output/fig4-8-right.png', plot=p, dpi=300, w=4, h=3)
