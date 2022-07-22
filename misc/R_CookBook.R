#-----probabilities-----#

#-2019/12/30-#
data()
medians<-numeric(1000)
for(i in 1:1000){
  medians[i]<-median(sample(USArrests$Rape,replace = TRUE))
}
ci<-quantile(medians,c(0.025,0.975))
cat("95% confidence interval is (",ci,")\n")

sample(1:19)
dbinom(1,size=10,prob=0.5)
pbinom(10,size = 10, prob = 0.5)
pnorm(168,mean = 171,sd=5.6)
x<-seq(from=-3,to=+3,length.out = 100)
plot(x,dnorm(x))
x<-seq(from=0, to=6, length.out = 100)
plot(x,dunif(x,min = 2,max = 4),main = "Uniform",type="l",ylim = c(0,0.6))
plot(x,dnorm(x,mean = 3, sd = 1), main = "Normal",type="l",ylim = c(0,0.6))

x<-seq(from=-3,to=+3,length.out = 100)
y<-dnorm(x)
plot(x,y,main="Standard Normal Distribution",type="l",ylab = "Density", xlab = "Quantile")
abline(h=0)
region.x<-x[1 <= x & x <=2]
region.y<-y[1 <= x & x <=2]
region.x<-c(region.x[1],region.x,tail(region.x,1))
region.y<-c(          0,region.y,              0 )
polygon(region.x,region.y,density=10)

rm(list = ls())
x<-seq(from=0,to=6,length.out = 100)
plot(x,dnorm(x,mean = 3,sd=1),type = "l")
?plot
