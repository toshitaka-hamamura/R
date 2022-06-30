#ex7_fig.r#

library(ggplot2)
setwd("C:/Users/toshi/OneDrive/R/stan_practice/matsuura2016/ch5/exercise")
load('ex7.RData')
ms <- rstan::extract(fit)

d_qua <- t(apply(ms$y_pred, 2, quantile, prob=c(0.1, 0.5, 0.9)))
colnames(d_qua) <- c('p10', 'p50', 'p90')
d_qua <- data.frame(d, d_qua)
d_qua$f <- as.factor(d_qua$f)

(p <- ggplot(data=d_qua, aes(x=y, y=p50, ymin=p10, ymax=p90, shape=f, fill=f)))
(p <- p + theme_bw(base_size=18) + theme(legend.key.height=grid::unit(2.5,'line')))
(p <- p + coord_fixed(ratio=1, xlim=c(0, 10), ylim=c(0, 10)))
(p <- p + geom_pointrange(size=0.8, color='grey5'))
(p <- p + geom_abline(aes(slope=1, intercept=0), color='black', alpha=3/5, linetype='31'))
(p <- p + scale_shape_manual(values=c(21, 24)))
p <- p + scale_fill_manual(values=c('white', 'grey70'))
(p <- p + labs(x='Observed', y='Predicted'))
(p <- p + scale_x_continuous(breaks=seq(from=0, to=70, by=20)))
(p <- p + scale_y_continuous(breaks=seq(from=0, to=70, by=20)))
ggsave(file='ex7_fig.png', plot=p, dpi=300, w=5, h=4)
