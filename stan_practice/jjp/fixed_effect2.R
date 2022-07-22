#----stan_run----#
rm(list = ls())
load("C:/Users/toshi/OneDrive - 株式会社KDDI総合研究所／教育・医療ICTグループ/外部発表/jjp2021/analysis_jjp2021/RData/cleaning.RData")
library('rstan')
getwd()
str(d)

standata <- list(N = nrow(df), 
                 kddi = df$t201912_kddi, 
                 sex = df$t201912_scr_sex, 
                 age =df$t202012scr_age, 
                 openness = df$t201912_bigfive_openness, 
                 consc = df$t201912_bigfive_conscientiousness, 
                 extrav = df$t201912_bigfive_extraversion, 
                 agree = df$t201912_bigfive_agreeableness, 
                 neur = df$t201912_bigfive_neuroticism
                 )
stanmodel <- stan(file = "C:/Users/toshi/OneDrive/R/stan_practice/jjp/fixed_effect2.stan", data = standata, iter = 1000, warmup = 300, chains = 3)

####--plot--####
traceplot(stanmodel)
print(stanmodel, probs = c(0.025, 0.5, 0.975))
save.image("fixed_effect2.RData")
