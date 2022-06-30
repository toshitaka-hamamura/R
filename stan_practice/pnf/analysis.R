#----data----#
rm(list = ls())
library(rstan)
getwd()
load(file = "/Users/toshitakahamamura/OneDrive/3.My Research/study5.2_yha/analysis_pnf_v2/cleaning_itt.RData")
str(df)
(data <- list(N=nrow(df), A=df$ae_positive, B=df$ae_negative, Y=df$ddq))
(fit <- stan(file = 'pnf/model1.stan', data=data, seed=1234))


