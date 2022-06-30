#図9.1の描画
##################################################
alpha <- 0.05
xlabel <- "chi-squared"	
curve(dchisq(x,df=20,0),0, 100,ylab="確率密度",
xlab=xlabel,ylim=c(0,0.08))
par(new=T)	
curve(dchisq(x,df=20,30),0, 100,ylab="",
xlab=xlabel,ylim=c(0,0.08))

crit <- qchisq(0.95,20,0)
xvals <- seq(crit,100)	
br <- length(xvals)

dvals <- dchisq(xvals,df=20,30)  # 対応するグラフの高さ
polygon(c(xvals,rev(xvals)),c(rep(0,br),rev(dvals)),
yaxt="n",ylab="",col="grey") 
abline(h=0)
##################################################

#RMSEAの標本分布とカイ二乗分布の描画
#パッケージsemToolsをアクティブにする
library(semTools)

#RMESAは2条件を指定
epsilon <- c(0.00,0.08)
#標本数は200
nobs <- 200 
#モデル自由度は22
d <- 22 
#RMSEA=0.00の分布で95パーセントランクに垂直線を引く
p <- 0.95 
#図中の凡例を作成
label <- paste("RMSEA=",epsilon,sep="")

#カイ二乗分布を描画
plotRMSEAdist(rmsea=epsilon,n=nobs,
df=d,ptile=p,caption=label,
rmseaScale=FALSE)
##############################################

#カイ二乗分布の描画
##############################################
#RMESAは2条件を指定
epsilon <- c(0.00,0.05)
#標本数は100
nobs <- 100
#モデル自由度は300
d <- 30
#RMSEA=0.00の分布で99パーセントランクに垂直線を引く
p <- 0.99 
#図中の凡例を作成
label <- paste("RMSEA=",epsilon,sep="")

#カイ二乗分布を描画
plotRMSEAdist(rmsea=epsilon,n=nobs,
df=d,ptile=p,caption=label,
rmseaScale=FALSE)
##############################################

#exact fitにおける検定力の算出
##############################################
#帰無仮説のRMSEA
epsilon0 <- 0.00
#対立仮説のRMSEA
epsilonA <- 0.05
#標本数
nobs <- 100
#両モデルの自由度
d <- 30
#有意水準
a <- 0.05

#検定力の算出
findRMSEApower(rmsea0=epsilon0,rmseaA=epsilonA,
n=nobs,df=d,alpha=a)
###############################################

#様々な帰無仮説における検定力の算出
###############################################
#close fit
epsilon0 <- 0.05
epsilonA <- 0.10

#検定力の算出
findRMSEApower(rmsea0=epsilon0,
rmseaA=epsilonA, n=nobs,df=d, alpha=a)

#not-close fit
epsilon0 <- 0.05
epsilonA <- 0.01
#検定力の算出
findRMSEApower(rmsea0=epsilon0,
rmseaA=epsilonA, n=nobs,df=d, alpha=a)
###############################################

#標本数の算出
###############################################
epsilon0 <- 0.05
epsilonA <- 0.01
pow <- 0.8
a <- 0.05
d <- 30
findRMSEAsamplesize(rmsea0=epsilon0,
rmseaA=epsilonA,df=d,power=pow,alpha=a)
###############################################

#検定力関数の描画
###############################################
library(semTools)

#標本数の下限の指定
nl <-50
#標本数の上限の指定
nh <- 600

plotRMSEApower(rmsea0=epsilon0,
rmseaA=epsilonA, df=d, nlow=nl, 
nhigh=nh, alpha=a)
##############################################

#ネストしたモデルの検定力分析
##############################################
library(semTools)
#モデルAにおける帰無仮説
epsilon0A <- 0.00
#モデルBにおける帰無仮説
epsilon0B <- 0.00
#モデルAにおける対立仮説
epsilon1A <- 0.06
#モデルBにおける対立仮説
epsilon1B <- 0.02
#モデルAの自由度
dA <- 22
#モデルBの自由度
dB <- 18
#標本数
nobs <- 200
#有意水準
a <- 0.05

findRMSEApowernested(rmsea0A=epsilon0A,
rmsea0B=epsilon0B,rmsea1A=epsilon1A,
rmsea1B=epsilon1B,dfA=dA, dfB=dB, 
n=nobs,alpha=a)
############################################


#ネストモデルにおける標本数の算出
################################################
p <- 0.8

findRMSEAsamplesizenested(rmsea0A=epsilon0A,
rmsea0B=epsilon0B,rmsea1A=epsilon1A,
rmsea1B=epsilon1B,dfA=dA, dfB=dB, 
power=p,alpha=a)
################################################

#ネストモデルにおける検定力関数の描画
################################################
nl <- 50
nh <- 500

plotRMSEApowernested(rmsea0A=epsilon0A,
rmsea0B=epsilon0B,rmsea1A=epsilon1A,
rmsea1B=epsilon1B,dfA=dA, dfB=dB, 
nlow=nl, nhigh=nh, alpha=a)
################################################

#事後の分析
################################################
library(semTools)
#帰無仮説のRMSEA
epsilon0 <- 0.00
#対立仮説のRMSEA
epsilon1 <- 0.05
#標本数
nobs <- 200
#両モデルの自由度
d <- 22
#有意水準
a <- 0.05

findRMSEApower(rmsea0=epsilon0,rmseaA=epsilon1,
n=nobs,df=d,alpha=a)
################################################

p <- 0.8
findRMSEAsamplesize(rmsea0=epsilon0,
rmseaA=epsilon1,pow=p,df=d,alpha=a)
