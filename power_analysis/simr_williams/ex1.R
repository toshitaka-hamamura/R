rm(list = ls())
library(lme4)
library(simr)
# load data ----

# example 1 ----
#1) GET DATA
simdata2<-read.csv("power_analysis/SIMR_RW_example.csv") 
head(simdata2)
hist(simdata2$Pr_rate)
#2) CREATE MODEL
model1 <- glmer(Pr_rate ~ Day + (1|Doctor), family="poisson", data=simdata2)
summary(model1)
fixef(model1)["Day"] #view effect
fixef(model1)["Day"] <- -0.06 #change effect to about half
#3) SIMULATE POWER
powerSim(model1)
model2 <- extend(model1, along="Day", n=20)
powerSim(model2)
#4) POWER CURVE
pc2 <- powerCurve(model2)
print(pc2)
plot(pc2)
model3 <- extend(model1, along="Doctor", n=15)
pc3 <- powerCurve(model3, along="Doctor")
plot(pc3)
model4 <- extend(model1, within="Day+Doctor", n=5)
pc4 <- powerCurve(model4, within="Day+Doctor", breaks=1:5)
print(pc4)
plot(pc4)