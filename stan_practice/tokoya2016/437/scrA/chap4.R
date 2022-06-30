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

#対応ある2群の差の推測（標準偏差は共通）表4.5
outEQU<-G2pair(x,EQU=1,prior=T,mL=0, mH=100, sL=0, sH=50, fi=NA)

#母数の数値要約　
outEQU$fit

#MCMC標本の名前が長いので取り出す。outEQU$sigma1をsigmaとする
mu1<-outEQU$mu1;mu2<-outEQU$mu2;rho<-outEQU$rho;xaste1<-outEQU$xaste1;
xaste2<-outEQU$xaste2;sigma<-outEQU$sigma1;log_lik<-outEQU$log_lik;

#標準偏差が同じモデルのWAIC  表4.10の左
(waic<- (-2)*(log(mean(exp(log_lik)))) + 2*(var(log_lik)))

#対応ある2群の差の推測（標準偏差が同じ）
mudef  <-mu1-mu2;                             #(3.11)
delta  <-mudef/sigma;                         #(3.15)
U3     <-pnorm(mu1,mu2,sigma);                #(3.20)
yuetsu <-pnorm(delta/sqrt(2),0,1);            #(3.24)
ikijyo1<-pnorm((mudef-1)/(sqrt(2)*sigma),0,1);#(3.28)

#分析　１　（群間の分析）
#生成量の数値要約　　表4.6
gqcal(mudef)
gqcal(delta  ); 
gqcal(1-U3   ); 
gqcal(yuetsu ); 
gqcal(ikijyo1); 

#研究仮説が正しい確率　表4.7
phc01(0.0,mudef,0,cc="gtc", byoga="no");                    #
phc01(2.0,mudef,0,cc="gtc", byoga="no");                    #
phc01(seq(0.0,5.0,0.25),mudef,    0,cc="gtc", byoga="yes"); #
phc01(0.3,delta,  0,cc="gtc", byoga="no");                  #
phc01(0.6,U3,     0,cc="gtc", byoga="no");                  #
phc01(0.7,yuetsu, 0,cc="gtc", byoga="no");                  #
phc01(0.7,ikijyo1,0,cc="gtc", byoga="no");                  #

#対応ある2群の差の推測（標準偏差が同じ）
sigmaD  <-sigma*sqrt(2*(1-rho));       #(4.23)
deltaD  <-mudef/sigmaD;                #(4.26)
yuetsuD <-pnorm(mudef/sigmaD,0,1);     #(4.29)
ikijyoD2<-pnorm((mudef-2)/sigmaD,0,1); #(4.32)
ikijyoD3<-pnorm((mudef-3)/sigmaD,0,1); #(4.32)
ikijyoD4<-pnorm((mudef-4)/sigmaD,0,1); #(4.32)
con     <- 0.5+(1/pi)*asin(rho);       #(4.36)

#分析　２　（相関を考慮した個人内変化の分析）
#生成量の数値要約　　表4.8
gqcal(sigmaD  ); 
gqcal(deltaD  ); 
gqcal(yuetsuD ); 
gqcal(ikijyoD2); 
gqcal(ikijyoD4); 
gqcal(rho)     ; 
gqcal(con)     ; 

#研究仮説が正しい確率　表4.9
phc01(3.0,sigmaD, 0,cc="ltc", byoga="no") ; #
phc01(0.3,deltaD, 0,cc="gtc", byoga="no") ; #
phc01(0.8,yuetsuD,0,cc="gtc", byoga="no") ; #
phc01(0.5,ikijyoD3,0,cc="gtc", byoga="no"); #
mean((0.6<rho)&(rho<0.8))                 ; #
phc01(0.8,con,0,cc="gtc", byoga="no")     ; #

#対応ある2群の差の推測（標準偏差が異なる）
outDEF<-G2pair(x,EQU=0,prior=T,mL=0, mH=100, sL=0, sH=50)

#母数のの数値要約
outDEF$fit

#MCMC標本の名前が長いので取り出す
mu1<-outDEF$mu1;mu2<-outDEF$mu2;rho<-outDEF$rho;xaste1<-outDEF$xaste1;
xaste2<-outDEF$xaste2;sigma1<-outDEF$sigma1;sigma2<-outDEF$sigma2;
log_lik<-outDEF$log_lik;

#標準偏差が異なるモデルのWAIC  表4.10の右
(waic<- (-2)*(log(mean(exp(log_lik)))) + 2*(var(log_lik)))

#生成量の計算  表4.11
mudef   <-mu1-mu2;                                     #(3.11)
delta1  <-mudef/sigma1;                                #(3.15)
delta2  <-mudef/sigma2;                                #(3.15)
U31     <-pnorm(mu1,mu2,sigma2);                       #(3.20)
U32     <-1-pnorm(mu2,mu1,sigma1);                     #(3.20)
yuetsu  <-pnorm(mudef/sqrt(sigma1^2+sigma2^2),0,1);    #(3.46)
ikijyo1 <-pnorm((mudef-1)/sqrt(sigma1^2+sigma2^2),0,1);#(3.49)

#生成量の計算  表4.12 （相関を考慮した個人内変化の生成量）
sigmaD  <-sqrt(sigma1^2+sigma2^2-2*rho*sigma1*sigma2); #(4.42)
deltaD  <-mudef/sigmaD;                                #(4.44)
yuetsuD <-pnorm(mudef/sigmaD,0,1);                     #(4.45)
ikijyoD2<-pnorm((mudef-2)/sigmaD,0,1);                 #(4.32)
ikijyoD4<-pnorm((mudef-4)/sigmaD,0,1);                 #(4.32)
con     <- 0.5+(1/pi)*asin(rho);                       #(4.36)

#分析　３　数値要約
gqcal(mudef   )
gqcal(delta1  )
gqcal(delta2  )
gqcal(U31     )
gqcal(U32     )
gqcal(yuetsu  )
gqcal(ikijyo1 )
gqcal(sigmaD  )
gqcal(deltaD  )
gqcal(yuetsuD )
gqcal(ikijyoD2)
gqcal(ikijyoD4)
gqcal(con     )

### 第4章章末問題

#データ 援助後を第1群，援助前を第2群とする
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
outDEF<-G2pair(x,EQU=0,prior=T,mL=0, mH=100, sL=0, sH=50)

#母数の分布の数値要約 表A10
outDEF$fit

#MCMC標本の名前が長いので取り出す
mu1<-outDEF$mu1;mu2<-outDEF$mu2;rho<-outDEF$rho;xaste1<-outDEF$xaste1;
xaste2<-outDEF$xaste2;sigma1<-outDEF$sigma1;sigma2<-outDEF$sigma2;

#生成量の計算  表A.11前半
mudef   <-mu1-mu2;                                      #(3.11)
delta1  <-mudef/sigma1;                                 #(3.15)
delta2  <-mudef/sigma2;                                 #(3.15)
U31     <-pnorm(mu1,mu2,sigma2);                        #(3.20)
U32     <-1-pnorm(mu2,mu1,sigma1);                      #(3.20)
yuetsu  <-pnorm(mudef/sqrt(sigma1^2+sigma2^2),0,1);     #(4.46)
ikijyo10<-pnorm((mudef-10)/sqrt(sigma1^2+sigma2^2),0,1);#(4.49)

#生成量の計算  表4.11後半 （相関を考慮した個人内変化の生成量）
sigmaD  <-sqrt(sigma1^2+sigma2^2-2*rho*sigma1*sigma2);  #(4.42)
deltaD  <-mudef/sigmaD;                                 #(4.44)
yuetsuD <-pnorm(mudef/sigmaD,0,1);                      #(4.45)
ikijyoD5<-pnorm((mudef-5)/sigmaD,0,1);                  #(4.32)
con     <- 0.5+(1/pi)*asin(rho);                        #(4.36)

#生成量の数値要約
gqcal(mudef   )
gqcal(delta1  )
gqcal(delta2  )
gqcal(U31     )
gqcal(U32     )
gqcal(yuetsu  )
gqcal(ikijyo10)
gqcal(sigmaD  )
gqcal(deltaD  )
gqcal(yuetsuD )
gqcal(ikijyoD5)
gqcal(con     )

#研究仮説が正しい確率
phc01(0.0,mudef   , 0,cc="gtc", byoga="no") ; #
phc01(15, mudef   , 0,cc="gtc", byoga="no") ; #
phc01(1.5,delta1  , 0,cc="gtc", byoga="no") ; #
phc01(1.0,delta2  , 0,cc="gtc", byoga="no") ; #
phc01(0.8,U31     , 0,cc="gtc", byoga="no") ; #
phc01(0.7,yuetsu  , 0,cc="gtc", byoga="no") ; #
phc01(0.8,yuetsu  , 0,cc="gtc", byoga="no") ; #
phc01(0.5,ikijyo10, 0,cc="gtc", byoga="no") ; #
phc01(20 ,sigmaD  , 0,cc="ltc", byoga="no") ; #
phc01(1.0,deltaD  , 0,cc="gtc", byoga="no") ; #
phc01(0.8,yuetsuD , 0,cc="gtc", byoga="no") ; #
phc01(0.9,ikijyoD5, 0,cc="gtc", byoga="no") ; #
phc01(0.6,con     , 0,cc="gtc", byoga="no") ; #
