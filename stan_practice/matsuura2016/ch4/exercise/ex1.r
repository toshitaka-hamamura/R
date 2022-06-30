#####exercise#####
set.seed(123)
(N1 <- 3000)
(N2 <- 2000)
(Y1 <- rnorm (n = N1, mean = 0, sd = 5))
(Y2 <- rnorm (n = N2, mean = 1, sd = 4))


p1 <- hist(rnorm (n = N1, mean = 0, sd = 5),breaks = 30)
p2 <- hist(rnorm (n = N1, mean = 1, sd = 4),breaks = 20)
plot(p1, col = rgb(0,0,1,1/4), xlim = c(-50,50))
plot(p2, col = rgb(1,0,0,1/4), xlim = c(-50,50), add=T)

(a1<- data.frame(length = rnorm(n = N1, mean = 0, sd = 5)))
(a2<- data.frame(length = rnorm(n = N2, mean = 1, sd = 4)))
(dens_Y1 <- density(Y1))
(dens_Y2 <- density(Y2))

(xlim <- range(dens_Y1$x, dens_Y2$x))
(ylim <- range(0,dens_Y1$y, dens_Y2$y))
plot(dens_Y1, xlim = xlim, ylim = ylim, xlab = 'Length',
     main = 'Distribution of two varibles',
     panel.first = grid())
(polygon(dens_Y1, density = -1))
(polygon(dens_Y2, density = -1))

#####exercise 1: answer######
library(ggplot2)
source('generate-data.R')

d1 <- data.frame(group=1, Y=Y1)
d2 <- data.frame(group=2, Y=Y2)
d <- rbind(d1, d2)
d$group <- as.factor(d$group)

(p <- ggplot(data=d, aes(x=group, y=Y, group=group, col=group)))
(p <- p + geom_boxplot(outlier.size=0))
(p <- p + geom_point(position=position_jitter(w=0.4, h=0), size=2))
(ggsave(file='fig-ex1.png', plot=p, dpi=300, w=4, h=3))
