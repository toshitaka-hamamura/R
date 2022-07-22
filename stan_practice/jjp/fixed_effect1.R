#----stan_run----#
rm(list = ls())
load("C:/Users/toshi/OneDrive - 株式会社KDDI総合研究所／教育・医療ICTグループ/外部発表/jjp2021/analysis_jjp2021/RData/cleaning.RData")
library('rstan')
getwd()
str(d)

standata <- list(kddi = df$t201912_kddi, sex = df$t201912_scr_sex, age =df$t202012scr_age, N = nrow(df))
stanmodel <- stan(file = "C:/Users/toshi/OneDrive/R/stan_practice/jjp/fixed_effect2.stan", data = standata, iter = 1000, warmup = 300, chains = 3)

####--plot--####
traceplot(stanmodel, pars = c("b1", "b2", "b3", "sigma"))

save.image("fixed_effect2.RData")
