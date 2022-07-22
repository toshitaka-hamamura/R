#第2章用スクリプト
getwd()                        #working directoryの確認
source('myfunc/myfunc.R')      #自作関数の読み込み
library(rstan)                 #パッケージrstanの呼び出し
rstan_options(auto_write=T)
options(mc.cores=parallel::detectCores())

#「牛丼データ」の入力
x<-c(76.5,83.9,87.9,70.8,84.6,85.1,79.6,79.8,79.7,78.0)

#正規分布に関する推測
out <-G1mean(x,prior=T,mL=0, mH=1000, sL=0, sH=100, fi=NA)       
traceplot(out$fit)
out$fit

#母数の事後分布
plot(density(out$mu))        #図2.2 平均の事後分布
plot(density(out$sigma))     #図2.3 sdの事後分布
#事後予測分布
plot(density(out$xaste))     #図2.4

#生成量の計算
bunsan <-out$sigma^2
hendou <-out$sigma/out$mu
delta85<-(out$mu-85)/out$sigma
delta80<-(out$mu-80)/out$sigma
quarter<-out$mu-0.675*out$sigma
yosok85<-pnorm(85,out$mu,out$sigma)
yosok80<-pnorm(80,out$mu,out$sigma)
hirit85<-out$xaste/85

#表2.4　生成量の推定結果(分布の要約) 
gqcal(bunsan )#分散
gqcal(hendou )#変動係数
gqcal(delta85)#デルタ85
gqcal(delta80)#デルタ80
gqcal(quarter)#第1四分位
gqcal(yosok85)#85g以下の確率
gqcal(yosok80)#80g以下の確率
gqcal(hirit85)#85gに対する比


#表2.5　研究仮説が正しい確率
phc01(c(85,80),out$mu,   0,cc="ltc", byoga="no"); #
phc01(c(85,80),out$xaste,0,cc="ltc", byoga="no"); #
phc01(-1      ,delta85,  0,cc="ltc", byoga="no"); #
phc01(0.0     ,delta80,  0,cc="ltc", byoga="no"); #
phc01(0.5     ,yosok85,  0,cc="ltc", byoga="no"); #
phc01(0.5     ,yosok80,  0,cc="ltc", byoga="no"); #

#phc曲線
phc01(seq(70,90,0.5),out$mu,   0,cc="ltc", byoga="yes"); #
gqcal(out$mu,probs=c(0.9, 0.95, 0.99))

#生成量のヒストグラム
plot(density(bunsan ),xlim=c(0,110))     #図2.5
plot(density(hendou ),xlim=c(0.02,0.14)) #図2.6
plot(density(delta85),xlim=c(-2,0.5))    #図2.7左図
plot(density(delta80),xlim=c(-1,1))      #図2.7右図
plot(density(quarter),xlim=c(65,85))     #図2.8
plot(density(yosok85),xlim=c(0.3,1.0))   #図2.9左図
plot(density(yosok80),xlim=c(0.1,0.9))   #図2.9右図
plot(density(hirit85),xlim=c(0.7,1.2))   #図2.10

### 第2章章末問題
##データ
x<-c(
36,38,51,40,41,52,43,31,35,37,49,43,43,41,36,53,43,26,45,37,
33,38,33,35,36,28,46,41,32,49,43,38,46,46,46,45,44,40,38,37,
35,39,31,55,48,32,37,37,45,39,42,40,40,50,38,51,29,44,41,42,
43,36,38,33,32,42,43,40,46,54,37,24,47,35,35,47,38,31,41,39,
40,43,37,45,38,42,48,43,38,48,47,44,42,36,50,36,55,51,38,33)

out <-G1mean(x,prior=T,mL=0, mH=1000, sL=0, sH=100, fi=out$fit)       

#RQ.1,2,3,4,5
print(out$fit,,probs=c(0.025, 0.05, 0.5, 0.95, 0.975))

#生成量の計算
bunsan <-out$sigma^2
hendou <-out$sigma/out$mu
delta45<-(out$mu-45)/out$sigma
delta35<-(out$mu-35)/out$sigma
perce20<-out$mu+qnorm(0.2,0,1)*out$sigma
yosok45<-pnorm(45,out$mu,out$sigma)
yosok35<-pnorm(35,out$mu,out$sigma)
hirit45<-out$xaste/45

#RQ.6
gqcal(bunsan )#分散
#RQ.7
gqcal(hendou )#変動係数
#RQ.8,9
gqcal(delta45)#デルタ45
gqcal(delta35)#デルタ35
#RQ.10
gqcal(perce20)#第1四分位
#RQ.11
gqcal(yosok45)#45点未満が観察される確率
gqcal(yosok35)#35点未満が観察される確率
#RQ.12
gqcal(hirit45)#45点に対する比

#RQ.13　
phc01(seq(37,43,0.5),out$mu,   0,cc="ltc", byoga="yes"); #
gqcal(out$mu,probs=c(0.9, 0.95, 0.99))
phc01(c(45,35),out$mu,   0,cc="ltc", byoga="no"); #


#RQ.14　
phc01(c(45,35),out$xaste,   0,cc="ltc", byoga="no"); #

#RQ.15 
phc01(-0.6,    delta45,   0,cc="ltc", byoga="no"); #
phc01( 0.6,    delta35,   0,cc="ltc", byoga="no"); #

#RQ.16
phc01( 0.8,    yosok45,   0,cc="ltc", byoga="no"); #
phc01( 0.8,    yosok35,   0,cc="ltc", byoga="no"); #
