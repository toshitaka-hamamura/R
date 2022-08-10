rm(list = ls())
library(lme4)
library(simr)

#1) GET DATA
## create covariates
subj <- factor(1:10)
class_id <- letters[1:5]
time <- 0:2
group <- c("control", "intervention")
subj_full <- rep(subj, 15)
class_full <- rep(rep(class_id, each=10), 3)
time_full <- rep(time, each=50)
group_full <- rep(rep(group, each=5), 15)
covars <- data.frame(id=subj_full, class=class_full, treat=group_full, time=factor(time_full))
## covars
## Intercept and slopes for intervention, time1, time2, intervention:time1, intervention:time2
fixed <- c(5, 0, 0.1, 0.2, 1, 0.9)
## Random intercepts for participants clustered by class
rand <- list(0.5, 0.1)
## residual variance
res <- 2
#2) CREATE MODEL
model <- makeLmer(y ~ treat*time + (1|class/id), fixef=fixed, VarCorr=rand, sigma=res, data=covars)
model

#3) SIMULATE POWER
sim_treat <- powerSim(model, nsim=100, test = fcompare(y~time))
sim_treat
sim_time <- powerSim(model, nsim=100, test = fcompare(y~treat))
sim_time
#4) POWER CURVE
## changing effect size
model_large <- model
fixef(model_large)['treatintervention:time1'] <- 2
sim_treat_large <- powerSim(model_large, nsim=100, test = fcompare(y~time))
sim_treat_large
## changing number of classes
model_ext_class <- extend(model, along="class", n=20)
model_ext_class
sim_treat_class <- powerSim(model_ext_class, nsim=100, test = fcompare(y~time))
sim_treat_class
p_curve_treat <- powerCurve(model_ext_class, test=fcompare(y~time), along="class")
plot(p_curve_treat)
## changing number within classes
model_ext_subj <- extend(model, within="class+treat+time", n=20)
model_ext_subj
sim_treat_subj <- powerSim(model_ext_subj, nsim=100, test = fcompare(y~time))
sim_treat_subj
p_curve_treat <- powerCurve(model_ext_subj, test=fcompare(y~time), within="class+treat+time", breaks=c(5,10,15,20))
plot(p_curve_treat)
## changing both
model_final <- extend(model, along="class", n=8)
model_final <- extend(model_final, within="class+treat+time", n=10)
sim_final <- powerSim(model_final, nsim=100, test = fcompare(y~time))
sim_final
save.image("power_analysis/results.RData")
