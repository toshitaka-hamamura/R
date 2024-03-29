####fig6-10.R####
library(ggplot2)

(ms <- data.frame(mu=c(0, 2), sigma=c(1, 0.5)))
(ms <- cbind(ms, gr=letters[1:nrow(ms)]))
(my_labs <- parse(text=sprintf('mu=="%.0f"~~sigma=="%.1f"', ms$mu, ms$sigma)))
(p <- ggplot(data.frame(X=c(-4, 4)), aes(x=X)))
(p <- p + theme_bw(base_size=18) + theme(legend.key.width=grid::unit(2.5,'line')))
(p <- p + mapply(
  function(mu, sigma, co) stat_function(fun=dnorm, args=list(mean=mu, sd=sigma), aes_q(linetype=co)),
  ms$mu, ms$sigma, ms$gr
))
(p <- p + scale_linetype_manual('parameter', values=c('solid', '52', '12'), labels=my_labs))
(p <- p + labs(x='y', y='density'))
ggsave(file='output/fig6-10.png', plot=p, dpi=300, w=6, h=3)
