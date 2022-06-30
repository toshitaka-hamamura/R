#ex4.r#
install.packages('gamlss.dist')
library(gamlss.dist)
?gamlss.dist
####the skew power exponential (SEP) distribution####
SEP(mu.link = "identity", sigma.link = "log", nu.link = "identity", 
    tau.link = "log")
dSEP(0:100, mu = 0, sigma = 1, nu = 0, tau = 2, log = FALSE)
pSEP(q, mu = 0, sigma = 1, nu = 0, tau = 2, lower.tail = TRUE, 
     log.p = FALSE)
qSEP(p, mu = 0, sigma = 1, nu = 0, tau = 2, lower.tail = TRUE, 
     log.p = FALSE, lower.limit = mu - 5 * sigma, 
     upper.limit = mu + 5 * sigma)
rSEP(100, mu = 0, sigma = 1, nu = 0, tau = 2)

barplot(dSEP(0:10, mu = 0, sigma = 1, nu = 0, tau = 2, log = FALSE))
