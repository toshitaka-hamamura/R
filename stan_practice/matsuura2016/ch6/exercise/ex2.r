#ex.2.R#
####uniform distribution####
runif(100, min = 0, max = 1)
plot(runif(100, min = 0.5, max = 1), ylim=c(0, 1))
####bernoulli distribtuion####
install.packages('purrr')
library(purrr)
?rbernoulli
rbernoulli(100, 0.5)
plot(rbernoulli(100, 0.5))
barplot(table(sample.int(n=2, size=50, replace=TRUE, prob=c(0.8, 0.2)) - 1))
####binominal distribution####
?rbinom
rbinom(5, size = 100, prob = 0.1)
barplot(rbinom(5, size = 100, prob = 0.2))
####beta distribution####
?beta
?rnorm
?rbeta
rbeta(100, 1, 1, ncp = 0)
####categorical distribution####
?rcategorical
x <- sample(LETTERS[1:4], 100, replace = TRUE, prob = c(0.1, 0.2, 0.65, 0.05))
prop.table(table(x))
barplot(prop.table(table(x)))
####multinomial####
?rmultinom
rmultinom(4, size = 100, prob = c(0.1,0.2,0.7))
dmultinom(3, size = NULL, prob = c(0.1,0.2,0.7), log = FALSE)
dmultinom(x=c(7,2,3), prob = c(0.4,0.35,0.25))
####dirichlet####
set.seed(1234)
(N <- 200)
(alpha <- rep(1, 3))
(x <- rdirichlet(N, alpha))
####exponential####
?rexp
rexp(100, rate = 1)
###posisson####
?rpois
rpois(100, lambda = 4)
dpois(0:10, lambda = 2.5)
barplot(dpois(0:10, lambda = 2.5))
####gamma####
?rgamma
?dgamma
rgamma(100, shape = 1, rate = 1)
dgamma(100, shape = 1, rate = 1)
barplot(rgamma(100, shape = 1, rate = 1))
barplot(dgamma(0:10, shape = 1, rate = 1))
####normal####
?rnorm
rnorm(100, mean = 0, sd = 1)
dnorm(0:100, mean = 0, sd = 1)
barplot(dnorm(-5:5, mean = 0, sd = 1))
####lognormal####
?rlnorm
rlnorm(100, meanlog = 0, sdlog = 1)
dlnorm(0:10, meanlog = 0, sdlog = 1)
barplot(dlnorm(0:10, meanlog = 2, sdlog = 0.4))
####multinormal####
library(mvtnorm)
??mvtnorm
?mvtnorm::dmvnorm()
dmvnorm(x = c(0,0), mean = c(1,1))

(sigma <- matrix(c(4,2,2,3), ncol=2))
(x <- rmvnorm(n=500, mean=c(1,2), sigma=sigma))
colMeans(x)
var(x)

(x <- rmvnorm(n=500, mean=c(1,2), sigma=sigma, method="chol"))
colMeans(x)
var(x)

plot(x)
####cauchy####
?rcauchy
rcauchy(100, location = 0, scale = 1)
dcauchy(0:100, location = 0, scale = 1)
barplot(dcauchy(-10:10, location = 0, scale = 1))
####student t####
?TDist
rt(100,1)
dt(-50:50, 1)
barplot(dt(-10:10, 1))
####doubole exponential####
library(nimble)
?rdexp
rdexp(100, location = 0, scale = 1)
barplot(ddexp(-10:10, location = 0, scale = 1))
