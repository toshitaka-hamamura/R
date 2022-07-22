#第4章用スクリプト
getwd()                        #working directoryの確認
source('myfunc/myfunc.R')      #自作関数の読み込み
library(rstan)                 #パッケージrstanの呼び出し
rstan_options(auto_write=T)
options(mc.cores=parallel::detectCores())

#表4.1の「ダイエット」データ入力
x1<-c(53.1,51.5,45.5,55.5,49.6,50.1,59.2,54.7,53.0,48.6,55.3,52.6,51.7,
      48.6,56.4,42.9,50.3,42.4,51.2,39.1)
x2<-c(48.3,45.2,46.6,56.6,41.2,44.6,51.9,55.5,45.4,47.6,50.6,54.5,49.0,
      43.9,53.8,40.1,52.8,35.3,55.6,38.0)
n<-nrow(x1)
x<-cbind(x1,x2)

#要約統計量　表4.2
h1<-mean(x1);round(h1,1)                #平均値
h2<-mean(x2);round(h2,1)
van<-function(x){mean((x-mean(x))^2)}   #分散を計算する関数
va1<-van(x1);round(va1,1)               #分散
va2<-van(x2);round(va2,1)
sd1<-sqrt(va1);round(sd1,2)             #標準偏差
sd2<-sqrt(va2);round(sd2,2)
(me1<-median(x1))                       #中央値
(me2<-median(x2))
round(quantile(x1,type =2),1)           #％点
round(quantile(x2,type =2),1)
plot(x1,x2);abline(0,1,lwd=1.5)         #図4.2

#共分散と相関係数
v1<-x1-h1;round(v1,1)                   #平均偏差データ　表4.3
v2<-x2-h2;round(v2,1)
Co<-mean(v1*v2);round(Co,1)             #共分散
z1<-v1/sd1;round(z1,2)                  #標準化  表4.4
z2<-v2/sd2;round(z2,2)
r<-mean(z1*z2);round(r,2)               #相関係数

#対応ある2群の差の推測（標準偏差は共通）
outEQU<-G2pair(x,EQU=1,prior=T,mL=0, mH=100, sL=0, sH=50, fi=NA)
EQU<-print(outEQU,onlydiff=F,3,cr1=2,cr2=1,cr3=0.3,cr4=3,
                 ra= 0.8,rb= 0.6,pr1=0.6,pr2=0.7,pr3=0.7,pr4=0.8)
EQU<-print(outEQU,onlydiff=F,3,cr2=2)        #差得点の閾上率2.0
EQU<-print(outEQU,onlydiff=F,3,cr2=3,pr3=0.5)#差得点の閾上率3.0
EQU<-print(outEQU,onlydiff=F,3,cr2=4)        #差得点の閾上率4.0
EQU<-print(outEQU,onlydiff=F,3,pr2=0.8)      #差得点の優越率が0.8より大
#対応ある2群の差の推測（標準偏差が異なる）
outDEF<-G2pair(x,EQU=0,prior=T,mL=0, mH=100, sL=0, sH=50, fi=outEQU$fit)
DEF<-print(outDEF,onlydiff=F,3,cr1=2,cr2=1,cr3=0.3,cr4=3,
                 ra= 0.8,rb= 0.6,pr1=0.6,pr2=0.7,pr3=0.7,pr4=0.8)
DEF<-print(outDEF,onlydiff=F,3,cr2=2)        #差得点の閾上率2.0
DEF<-print(outDEF,onlydiff=F,3,cr2=4)        #差得点の閾上率4.0

### 第4章章末問題

#データ
#援助後を第1群，援助前を第2群とする
x1<-c(
73,72,56,58,71,42,78,77,75,72,56,71,69,77,84,51,62,88,56,58,84,91,71,82,81,
77,65,78,79,60,66,70,65,57,64,61,56,67,75,64,68,67,80,55,48,85,56,62,65,79)
x2<-c(
62,54,19,54,47,22,35,77,64,60,27,41,41,44,57,16,42,89,40,67,69,46,74,62,60,
87,32,42,73,25,42,57,31,35,33,38,43,53,55,62,67,56,76,05,31,70,66,65,34,48)
n<-nrow(x1)
x<-cbind(x1,x2)

#標本平均，標本分散，標本sd，標本四分位点
h1<-mean(x1);round(h1,1)
h2<-mean(x2);round(h2,1)
van<-function(x){mean((x-mean(x))^2)}
va1<-van(x1);round(va1,1)
va2<-van(x2);round(va2,1)
sd1<-sqrt(va1);round(sd1,1)
sd2<-sqrt(va2);round(sd2,1)
(me1<-median(x1))
(me2<-median(x2))
round(quantile(x1,type =2),1);round(quantile(x2,type =2),1)

#標本相関係数
round(cor(x1,x2),2)

#対応ある2群の差の推測（標準偏差が異なる）
outDEF<-G2pair(x,EQU=0,prior=T,mL=0, mH=100, sL=0, sH=50, fi=outDEF$fit)
DEF<-print(outDEF,onlydiff=F,3,cr1=15,cr2=10,cr3=1.5,cr4=20,
                 ra= 0.7,rb= 0.5,pr1=0.8,pr2=0.7,pr3=0.5,pr4=0.6)
#5.0に対する差得点の閾上率、効果量2が1.0より大きい確率
#優越率が0.8より大きい確率、差得点の効果量が1.0より大きい確率
#差得点の優越率が0.8より大きい確率、
#5.0に対する差得点の閾上率が0.9より大きい確率
#以上の6つを計算するために以下を実行
DEF<-print(outDEF,onlydiff=F,3,cr2=5,cr3=1.0,pr2=0.8,pr3=0.9)
