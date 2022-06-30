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
out2<-print(out,degits=3,cr1=85,cr2a=85,
             cr3=85,cr4=85,cr5=-1, pr1=0.25,pr2=0.5)
out3<-print(out,degits=3,cr1=80,cr2a=80,
             cr3=80,cr4=80,cr5=0.0,pr1=0.25,pr2=0.5)

#母数の事後分布
plot(density(out$mu))        #図2.2 平均の事後分布
plot(density(out$sigma))     #図2.3 sdの事後分布

#事後予測分布
plot(density(out$xaste))     #図2.4

#生成量の事後分布
plot(density(out2$G[,1]),xlim=c(0,110)) #図2.5
plot(density(out2$G[,2]))       #図2.6
plot(density(out2$G[,3]))       #図2.7左図
plot(density(out3$G[,3]))       #図2.7右図
plot(density(out2$G[,4]))       #図2.8
plot(density(out2$G[,5]))       #図2.9左図
plot(density(out3$G[,5]))       #図2.9右図
plot(density(out2$G[,6]))       #図2.10

### 第2章章末問題
##データ
x<-c(
36,38,51,40,41,52,43,31,35,37,49,43,43,41,36,53,43,26,45,37,
33,38,33,35,36,28,46,41,32,49,43,38,46,46,46,45,44,40,38,37,
35,39,31,55,48,32,37,37,45,39,42,40,40,50,38,51,29,44,41,42,
43,36,38,33,32,42,43,40,46,54,37,24,47,35,35,47,38,31,41,39,
40,43,37,45,38,42,48,43,38,48,47,44,42,36,50,36,55,51,38,33)

out <-G1mean(x,prior=T,mL=0, mH=1000, sL=0, sH=100, fi=out$fit)       
out2<-print(out,degits=3,cr1=45,cr2a=45,
             cr3=45,cr4=45,cr5=-0.6, pr1=0.20,pr2=0.8)
out3<-print(out,degits=3,cr1=35,cr2a=35,
             cr3=35,cr4=35,cr5= 0.6, pr1=0.20,pr2=0.8)
