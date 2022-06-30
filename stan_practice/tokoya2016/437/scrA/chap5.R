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

#表5.2 母数の推定結果
colnames(aaa$muA)    <-paste("muA",1:4,sep="")
gqcal(aaa$muA,   1)    ;
gqcal(aaa$sigmaE,1)    ;

#表5.3 生成量の推定結果
gqcal(aaa$mu,1)    ;
colnames(aaa$aj)    <-paste("aj",1:4,sep="")
gqcal(aaa$aj,1)    ;

#表5.4 水準の効果が0より大きい（小さい）確率
(Upphc<-round(apply(aaa$Ubig,2,mean),3))    ;
1-Upphc;

#表5.5 効果の大きさに関する生成量の推定結果
gqcal(aaa$sigmaA,1)    ;
gqcal(aaa$eta2,  3)    ;
gqcal(aaa$delta, 3)    ;

#表5.6 行の水準の効果が列の水準の効果より大きい確率
phc02(0,aaa$aj, cc="gtc")

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

#表5.7  特に興味のある2水準間の比較
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

#表5.9  母数の推定結果
gqcal(bbb$mu,        1)    ;
gqcal(bbb$muA[,1],   1)    ;
gqcal(bbb$muB[,1],   1)    ;
gqcal(bbb$muAB[,1,1],1)    ;
gqcal(bbb$sigmaE,    1)    ;

#表5.10 水準・交互作用が0より大きい(小さい)確率
mean(bbb$muA[,1]>0);mean(bbb$muB[,1]>0);mean(bbb$muAB[,1,1]>0 )
mean(bbb$muA[,1]<=0);mean(bbb$muB[,1]<=0);mean(bbb$muAB[,1,1]<=0)

#表5.11 効果の大きさに関する生成量の推定結果
gqcal(bbb$sigmaA, 1)    ;
gqcal(bbb$sigmaB, 1)    ;
gqcal(bbb$sigmaAB,1)    ;
gqcal(bbb$eta2A,  3)    ;
gqcal(bbb$eta2B,  3)    ;
gqcal(bbb$eta2AB, 3)    ;
gqcal(bbb$eta2T,  3)    ;
gqcal(bbb$deltaA, 3)    ;
gqcal(bbb$deltaB, 3)    ;
gqcal(bbb$deltaAB,3)    ;

#表5.12 セル平均の推定結果
cellmean  <-cbind(bbb$cellmea[,1,]  ,bbb$cellmean[,2,])
colnames(cellmean)<-as.vector(t(outer(1:2,1:2,paste,sep=",")))
gqcal(cellmean,1)  ;  

#特に興味のある2セル間の比較
#表5.13 サンフランシスコの推定結果
E2betw_level(bbb,degits=3,H="A",F=1,I=1,J=2,cr1=10)
#表5.14 ロサンゼルスの推定結果
E2betw_level(bbb,degits=3,H="A",F=2,I=2,J=1,cr1=10)

### 第5章章末問題

## 1
#表5.15 マウスの体重のデータ
y<-c(05.02, 06.67, 08.17, 02.79, 08.13, 06.34, 06.32, 03.97,
     09.89, 09.58, 11.20, 09.05, 12.33, 09.39, 10.88, 09.37, 17.40,
     10.20, 07.29, 07.57, 03.42, 05.82, 10.92, 05.21, 13.47, 08.64, 06.05)
A<-c(rep(1,8),rep(2,9),rep(3,10))
a<-length(unique(A))
aaa<-E1Ind(y,A,prior=T,mL=0, mH=50, sL=0, sH=50)

#表A.13 母数の事後分布の数値要約
colnames(aaa$muA)    <-paste("muA",1:3,sep="")
gqcal(aaa$muA,   3)    ;
gqcal(aaa$sigmaE,3)    ;

#表A.14生成量の事後分布の数値要約
gqcal(aaa$mu,3)    ;
colnames(aaa$aj)    <-paste("aj",1:3,sep="")
gqcal(aaa$aj,3)    ;

#表A.15水準の効果が0より大きい（小さい）確率
(Upphc<-round(apply(aaa$Ubig,2,mean),3))    ;
1-Upphc;

#表A.16効果の大きさに関する生成量の推定結果
gqcal(aaa$sigmaA,3)    ;
gqcal(aaa$eta2,  3)    ;
gqcal(aaa$delta, 3)    ;

#表A.17行の水準の効果が列の水準の効果より大きい確率
phc02(0,aaa$aj, cc="gtc")

#表A.18
printIJ(aaa$U2,3,IJ=rbind(c(2,3),c(3,1)))
E1betw_level(aaa,3,2,1,cr1=5.0)

## 2
#表5.16 走者の有無による選手Ｅの球種別の球速
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
bbb<-E2Ind(y,A,B,prior=T,mL=0, mH=1000, sL=0, sH=500)

#表A.19 一部の母数の事後分布の数値要約
gqcal(bbb$mu,        1)    ;
gqcal(bbb$muA[,1],   3)    ;
gqcal(bbb$sigmaE,    3)    ;

#表A.20 要因Ｂ（球種）の水準の効果が0より大きい（小さい）確率
(Upphc<-round(apply(bbb$UbigB,2,mean),3))    ;
1-Upphc;

#表A.21効果の大きさに関する生成量の推定結果
gqcal(bbb$sigmaA, 3)    ;
gqcal(bbb$sigmaB, 3)    ;
gqcal(bbb$sigmaAB,3)    ;
gqcal(bbb$eta2A,  3)    ;
gqcal(bbb$eta2B,  3)    ;
gqcal(bbb$eta2AB, 3)    ;
gqcal(bbb$eta2T,  3)    ;
gqcal(bbb$deltaA, 3)    ;
gqcal(bbb$deltaB, 3)    ;
gqcal(bbb$deltaAB,3)    ;

#表A.22 行の水準が列の水準より大きい確率
phc02(c=0.0 ,bbb$muB,cc="gtc") ;

#表A.23
E2betw_level(bbb,degits=3,H="A",F=2,I=4,J=6,cr1=10)

#連言命題が正しい確率
printIJ(bbb$U2B,3,IJ=rbind(c(1,3),c(1,2),c(3,5),c(3,4),c(3,6)))
printIJ(bbb$U2B,3,IJ=rbind(c(1,2),c(2,3),c(2,5),c(2,4),c(2,6)))
printIJ(bbb$U2B,3,IJ=rbind(c(1,2),c(2,3),c(3,5),c(3,4),c(3,6)))
