#ex3.r#
rnorm(2000, mean = 50, sd = 20)
barplot(dnorm(-50:150, mean = 50, sd = 20))

rnorm(2000, mean = 20, sd = 15)
barplot(dnorm(-50:150, mean = 20, sd = 15))

rnorm(4000, mean = (50+20), sd = sqrt((20^2)+(15^2)))
barplot(dnorm(-50:150, mean = (50+20), sd = sqrt((20^2)+(15^2))))

#ex3_ans#
set.seed(123)

y1 <- rnorm(n=2000, mean=50, sd=20)
y2 <- rnorm(n=2000, mean=20, sd=15)
y <- y1 - y2

library(ggplot2)
(p <- ggplot(data.frame(x=y), aes(x)))
(p <- p + geom_density())
ggsave(file='fig-ex3.png', plot=p, dpi=300, w=4, h=3)

(py1 <- ggplot(data.frame(x=y1), aes(x)))
(py1 <- py1 + geom_density())
