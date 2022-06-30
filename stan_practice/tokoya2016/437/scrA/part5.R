#第5章用スクリプト
getwd()                        #working directoryの確認
source('myfunc/myfunc.R')      #自作関数の読み込み
library(rstan)                 #パッケージrstanの呼び出し
rstan_options(auto_write=T)
options(mc.cores=parallel::detectCores())

#表5.1の「亜硫酸ガスデータ」の入力
  A <- rep(1:4,each=6);a<-length(unique(A))
  y <- c(
    10, 10, 09, 11, 12, 11,   08, 10, 08, 10, 12, 09,
    08, 08, 11, 11, 14, 15,   14, 12, 11, 16, 13, 12)
AA<-c(rep("1春",6),rep("2夏",6),rep("3秋",6),rep("4冬",6))
boxplot(y~AA)

#1要因実験の推測
aaa<-E1Ind(y,A,prior=T,mL=0, mH=50, sL=0, sH=50, fi=NA)
print(aaa,3)

#図5.1 箱ひげ図
size<-dim(aaa$muA)[1];              #乱数数
aa<-aaa$aj[,1]; for (i in 2:a){aa<-c(aa,aaa$aj[,i])};
boxplot(aa~rep(1:a,each=size)); abline(h=0.0,lwd=1.0)

#説明率と効果量の事後分布
plot(density(aaa$eta2))       #図5.2左
plot(density(aaa$delta))      #図5.2右

#連言命題が正しい確率
printIJ(aaa$U2,3,IJ=rbind(c(4,3),c(3,1),c(1,2)))
printIJ(aaa$U2,3,IJ=rbind(c(4,3),c(4,1),c(3,2),c(1,2)))
printIJ(aaa$U2,3,IJ=rbind(c(4,1),c(4,2),c(4,3)))
printIJ(aaa$U2,3,IJ=rbind(c(4,1),c(4,2)))

#特に興味のある2水準間の比較
E1betw_level(aaa,3,4,2,cr1=2.0)

#図5.3(例として効果量と非重複度の事後分布を示す)
plot(density(aaa$muA[,4]-aaa$muA[,2]))
plot(density(pnorm(aaa$muA[,4],aaa$muA[,2],aaa$sigmaE)))

#表5.8の「サンフランシスコとロサンゼルスのホテルの料金」の入力
A <- rep(1:2,each=18)
B <- rep(1:2,each=9,times=2)
y <- c(
 079,107,103,092,180,165,240,265,300,
 075,060,060,094,119,100,102,125,165,
 095,099,070,116,170,145,205,200,210,
 153,078,075,092,115,155,250,340,380)

#2要因実験の推測
bbb<-E2Ind(y,A,B,prior=T,mL=0, mH=1000, sL=0, sH=500)
print(bbb,3)

#特に興味のある2セル間の比較
E2betw_level(bbb,degits=3,H="A",F=1,I=1,J=2,cr1=10)
E2betw_level(bbb,degits=3,H="A",F=2,I=2,J=1,cr1=10)

### 第5章章末問題

## 1
y<-c(05.02, 06.67, 08.17, 02.79, 08.13, 06.34, 06.32, 03.97,
     09.89, 09.58, 11.20, 09.05, 12.33, 09.39, 10.88, 09.37, 17.40,
     10.20, 07.29, 07.57, 03.42, 05.82, 10.92, 05.21, 13.47, 08.64, 06.05)
A<-c(rep(1,8),rep(2,9),rep(3,10))
a<-length(unique(A))
out<-E1Ind(y,A,prior=T,mL=0, mH=50, sL=0, sH=50, fi=aaa$fit)
print(out,3)
printIJ(out$U2,3,IJ=rbind(c(2,3),c(3,1)))
E1betw_level(out,3,2,1,cr1=5.0)

## 2
A<-c(rep(1,49),rep(2,49))
B<-c(rep(1,10),rep(2,8),rep(3,7),rep(4,9),rep(5,8),rep(6,7),
     rep(1,10),rep(2,8),rep(3,7),rep(4,9),rep(5,8),rep(6,7))
y<-c(140,146,149,136,147,147,143,143,143,141,
139,136,136,140,135,132,140,134,
123,127,131,130,138,128,129,
115,120,118,118,121,124,129,119,128,
128,124,123,121,122,126,131,122,
121,121,120,116,117,113,118,
143,141,142,145,149,145,143,141,142,155,
138,134,142,136,135,136,131,133,
131,128,128,128,127,130,130,
117,125,132,122,119,122,129,117,127,
117,120,124,122,122,122,118,122,
119,125,122,116,119,113,122)
a<-length(unique(A))
b<-length(unique(B))
out<-E2Ind(y,A,B,prior=T,mL=0, mH=1000, sL=0, sH=500, fi=bbb$fit)
print(out,3)
printIJ(out$U2B,3,IJ=rbind(c(1,3),c(1,2),c(3,5),c(3,4),c(3,6)))
printIJ(out$U2B,3,IJ=rbind(c(1,2),c(2,3),c(2,5),c(2,4),c(2,6)))
printIJ(out$U2B,3,IJ=rbind(c(1,2),c(2,3),c(3,5),c(3,4),c(3,6)))
E2betw_level(out,degits=3,H="A",F=2,I=4,J=6,cr1=10)
