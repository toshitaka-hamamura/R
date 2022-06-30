#第3章用スクリプト
getwd()                        #working directoryの確認
source('myfunc/myfunc.R')      #自作関数の読み込み
library(rstan)                 #パッケージrstanの呼び出し
rstan_options(auto_write=T)
options(mc.cores=parallel::detectCores())

#表3.1のデータ、クラスA、クラスBの順に入力
x1<-c(49,66,69,55,54,72,51,76,40,62,66,51,59,68,66,57,53,66,58,57)
x2<-c(41,55,21,49,53,50,52,67,54,69,57,48,31,52,56,50,46,38,62,59)

#表3.2　要約統計量　
(n1<-length(x1));(n2<-length(x2))         #データの数
mean(x1);mean(x2)                         #平均値
van<-function(x){mean((x-mean(x))^2)}     #分散を計算する関数
van(x1);van(x2)                           #分散
sqrt(van(x1));sqrt(van(x2))               #標準偏差
sort(x1);sort(x2)                         #小さい順に並べる
median(x1);median(x2)                     #中央値
quantile(x1,type =2);quantile(x2,type =2) #％点
sort(x1);sort(x2)                         #表3.3　

#図3.1箱ひげ図
x<-c(x1,x2);y<-c(rep("クラスA",n1),rep("クラスB",n2))
boxplot(x~y,cex.axis=2.0)                 

#独立した2群の差の推測（標準偏差は共通）
outEQU<-G2Ind(x1,x2,EQU=1,prior=T,mL=0, mH=100, sL=0, sH=50, fi=NA)
#独立した2群の差の推測（標準偏差が異なる）
outDEF<-G2Ind(x1,x2,EQU=0,prior=T,mL=0, mH=100, sL=0, sH=50, fi=outEQU$fit)
#出力パート
outEQU2<-print(outEQU,3,cr1=5,cr2=3,cr3=0.3,pr1=0.60,pr2=0.80,pr3=0.80)
outDEF2<-print(outDEF,3,cr1=5,cr2=3,cr3=0.3,pr1=0.60,pr2=0.80,pr3=0.80)

plot(density(outEQU2$G[,1]))      #図3.3右図
plot(density(outEQU2$G[,2]))      #図3.4左図
plot(density(outEQU2$G[,4]))      #図3.4右図
plot(density(outEQU2$G[,6]))      #図3.6左図
plot(density(outEQU2$G[,7]))      #図3.6右図

### 第3章章末問題

#データ入力　罹患群を第1群，健常群を第2群とする
x1<-c(
56,55,55,62,54,63,47,58,56,56,57,52,53,50,50,57,57,55,60,65,53,43,60,51,52,
60,54,49,56,54,55,57,53,58,54,57,60,57,53,61,60,58,56,52,62,52,66,63,54,50)
x2<-c(
33,37,59,41,42,61,46,25,32,35,55,44,45,41,33,61,46,16,48,34,27,37,28,31,32,
20,50,42,26,55,45,36,51,51,50,48,47,39,36,35,32,38,25,66,54,27,35,34,49,39)
n1<-length(x1);n2<-length(x2);

## 1　標本平均，標本分散，標本sd，標本四分位点
x<-c(x1,x2)
y<-c(rep("クラスA",n1),rep("クラスB",n2))
mean(x1);mean(x2)
van<-function(x){mean((x-mean(x))^2)}
van(x1);van(x2)
sqrt(van(x1));sqrt(van(x2))
median(x1);median(x2)
quantile(x1,type =2);quantile(x2,type =2)
sort(x1);sort(x2)
boxplot(x~y,cex.axis=2.0)
outDEF<-G2Ind(x1,x2,EQU=0,prior=T,mL=0, mH=100, sL=0, sH=50, fi=outDEF$fit)
outDEF2<-print(outDEF,3,cr1=13,cr2=10,cr3=2.0,pr1=0.8,pr2=0.8,pr3=0.7)
outDEF2<-print(outDEF,3,cr3=1.2)  #RQ.6の効果量の1.2

