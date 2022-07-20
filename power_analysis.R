#cohen's d: .0.2=small; 0.5=medium; 0.8=large
library(pwr)
pwr.t.test(n=191,d=0.20,sig.level=.05,power=NULL,type = c("two.sample") )
pwr.t.test(n=NULL,d=0.5,sig.level = .05,power = 0.80,type = c("two.sample"))#sample size
pwr.t.test(n=NULL,d=0.2,sig.level = .05,power = 0.80,type = c("two.sample"))#sample size
pwr.r.test(n=191,r=0.16,sig.level = .10,power = NULL)#SelfRcordApp study

pwr.anova.test(k=2,n=NULL,f=0.20,sig.level = .05,power = .95)#CYD study

pwr.t.test(n=NULL,d=0.394,sig.level = .05,power = 0.80,type = c("two.sample"))#小林さん
