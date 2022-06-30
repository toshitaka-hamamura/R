#第1章用スクリプト
getwd()                        #working directoryの確認
source('myfunc/myfunc.R')      #自作関数の読み込み

#「牛丼データ」の入力
x<-c(76.5,83.9,87.9,70.8,84.6,85.1,79.6,79.8,79.7,78.0)

#表1.1　度数分布表
(xs50<-seq(70,90,5))                #階級値
(xt<-table(cut(x,xs50)))            #度数
(xp<-xt/length(x))                  #確率
cumsum(xt)                          #累積度数
cumsum(xp)                          #累積確率

#図1.1　ヒストグラム
hist(x,xs50)
(xs25<-seq(70,90,2.5)); hist(x,xs25)

#要約統計量(積率系)　
mean(x)                               #平均値
van<-function(x){mean((x-mean(x))^2)} #分散を計算する関数
round(s2<-van(x),3)                   #分散
round(s<-sqrt(s2),3)                  #標準偏差

#要約統計量(分位系)　
sort(x)                               #小さい順に並べる
median(x)                             #中央値
quantile(x,0.3,type =1)               #30％点
quantile(x,0.7,type =1)               #70％点
rev(sort(table(round(x))))    #小数第1位で四捨五入したときの最頻値

#図1.7　ドアAに高級車がある事後確率
set.seed(1234)
nod<-10000000                #乱数の数
BgAc<-runif(nod)             #f(Bg|Ac)の確率分布 
hist(BgAc,breaks=50,freq=F)  #そのヒストグラム
AcBg<-BgAc/(BgAc+1)          #事後分布
mean(AcBg)                   #EAP推定値
median(AcBg)                 #MED推定値
hist(AcBg,breaks=50,freq=F)  #MAP推定値=0.5

### 第1章章末問題
##データ
x<-c(
36,38,51,40,41,52,43,31,35,37,49,43,43,41,36,53,43,26,45,37,
33,38,33,35,36,28,46,41,32,49,43,38,46,46,46,45,44,40,38,37,
35,39,31,55,48,32,37,37,45,39,42,40,40,50,38,51,29,44,41,42,
43,36,38,33,32,42,43,40,46,54,37,24,47,35,35,47,38,31,41,39,
40,43,37,45,38,42,48,43,38,48,47,44,42,36,50,36,55,51,38,33)

##1.度数分布表
(xs50<-seq(20,60,5))
(xt<-table(cut(x,xs50,right=F)))
(xp<-xt/length(x))
cumsum(xt)
cumsum(xp)

##2.ヒストグラム
hist(x,xs50,right=F)
(xs15<-seq(20,60,10)); hist(x,xs15,right=F)

##3.標本平均、標本分散、標本標準偏差
mean(x)
van<-function(x){mean((x-mean(x))^2)}
(s2<-van(x))
(s<-sqrt(s2))

##4.最大値・最小値
sort(x)
min(x)
max(x)

##5.中央値、最頻値
median(x)
rev(sort(table(round(x))))[1:5] 

##6(a)
round(dnorm(30,40.64, 6.40),4)
round(dnorm(40,40.64, 6.40),4)

##6(b)
1-round(pnorm(45,40.64, 6.40),4)

##6(c)
round(pnorm(40,40.64, 6.40)-pnorm(35,40.64, 6.40),4)

##6(d)
round(40.64- 1.96*6.40, 2)
round(40.64+ 1.96*6.40, 2)

##6(e)
round(qnorm(0.95,40.64,6.40),4)

##6(f)
round(qnorm(0.25,40.64, 6.40),2)
round(qnorm(0.5,40.64, 6.40),2)
round(qnorm(0.75,40.64, 6.40),2)
