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

#MCMC標本の名前が長いので取り出す。outEQU$sigma1をsigmaとする
mu1<-outEQU$mu1;mu2<-outEQU$mu2;sigma<-outEQU$sigma1;
xaste1<-outEQU$xaste1;xaste2<-outEQU$xaste2;log_lik<-outEQU$log_lik;

#標準偏差が同じモデルのWAIC  表3.13の左
(waic<- (-2)*(log(mean(exp(log_lik)))) + 2*(var(log_lik)))

#母数の数値要約　表3.5
gqcal(mu1    );
gqcal(mu2    );
gqcal(sigma  ); 
mudef  <-mu1-mu2;                             #(3.11)
gqcal(mudef); 

#研究仮説が正しい確率　表3.6
phc01(c(0,3,5,10),mudef,0,cc="gtc", byoga="no");   #

#デルタ   表3.7
delta  <-mudef/sigma;                         #(3.15)
gqcal(delta)
phc01(0.3,delta ,0,cc="gtc", byoga="no");            #

#非重複度 表3.8
U3     <-pnorm(mu1,mu2,sigma);                #(3.20)
gqcal(U3)
phc01(0.6,U3    ,0,cc="gtc", byoga="no");            #

#優越率と閾上率   表3.9
gqcal(xaste1)
gqcal(xaste2)

#優越率と閾上率   表3.10
yuetsu <-pnorm(delta/sqrt(2),0,1);            #(3.24)
ikijyo3<-pnorm((mudef-3)/(sqrt(2)*sigma),0,1);#(3.28)
gqcal(yuetsu)
gqcal(ikijyo3)
phc01(0.8,yuetsu, 0,cc="gtc", byoga="no");           #p.76文中
phc01(0.8,ikijyo3,0,cc="gtc", byoga="no");           #p.77文中

#密度関数
plot(density(mudef  ))      #図3.3右図
plot(density(delta  ))      #図3.4左図
plot(density(U3     ))      #図3.4右図
plot(density(yuetsu ))      #図3.6左図
plot(density(ikijyo3))      #図3.6右図


#独立した2群の差の推測（標準偏差が異なる）
outDEF<-G2Ind(x1,x2,EQU=0,prior=T,mL=0, mH=100, sL=0, sH=50)

#MCMC標本の名前が長いので取り出す。
mu1<-outDEF$mu1;mu2<-outDEF$mu2;sigma1<-outDEF$sigma1;sigma2<-outDEF$sigma2;
xaste1<-outDEF$xaste1;xaste2<-outDEF$xaste2;log_lik<-outDEF$log_lik;

#標準偏差が同じモデルのWAIC  表3.13の右
(waic<- (-2)*(log(mean(exp(log_lik)))) + 2*(var(log_lik)))

#生成量の計算  
mudef   <-mu1-mu2;                                     #(3.11)
delta1  <-mudef/sigma1;                                #(3.15)
delta2  <-mudef/sigma2;                                #(3.15)
U31     <-pnorm(mu1,mu2,sigma2);                       #(3.20)
U32     <-1-pnorm(mu2,mu1,sigma1);                     #(3.20)
yuetsu  <-pnorm(mudef/sqrt(sigma1^2+sigma2^2),0,1);    #(3.46)
ikijyo3 <-pnorm((mudef-3)/sqrt(sigma1^2+sigma2^2),0,1);#(3.49)

#数値要約  表3.11
gqcal(mu1     );#
gqcal(mu2     );#
gqcal(sigma1  );#
gqcal(sigma2  );#
gqcal(xaste1  );#
gqcal(xaste2  );#
gqcal(mudef   );#
gqcal(delta1  );#
gqcal(delta2  );#
gqcal(U31     );#
gqcal(U32     );#
gqcal(yuetsu  );#
gqcal(ikijyo3 );#

#研究仮説が正しい確率　表3.12
phc01(c(0,3,5,10),mudef,0,cc="gtc", byoga="no");     #
phc01(0.3,delta1 ,0,cc="gtc", byoga="no");           #
phc01(0.6,U31    ,0,cc="gtc", byoga="no");           #
phc01(0.8,yuetsu, 0,cc="gtc", byoga="no");           #
phc01(0.8,ikijyo3,0,cc="gtc", byoga="no");           #

### 第3章章末問題

#データ入力　罹患群を第1群，健常群を第2群とする
x1<-c(
56,55,55,62,54,63,47,58,56,56,57,52,53,50,50,57,57,55,60,65,53,43,60,51,52,
60,54,49,56,54,55,57,53,58,54,57,60,57,53,61,60,58,56,52,62,52,66,63,54,50)
x2<-c(
33,37,59,41,42,61,46,25,32,35,55,44,45,41,33,61,46,16,48,34,27,37,28,31,32,
20,50,42,26,55,45,36,51,51,50,48,47,39,36,35,32,38,25,66,54,27,35,34,49,39)
n1<-length(x1);n2<-length(x2);

## 1　標本平均，標本分散，標本sd，標本四分位点 表A.5
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
outDEF<-G2Ind(x1,x2,EQU=0,prior=T,mL=0, mH=100, sL=0, sH=50)

#MCMC標本の名前が長いので取り出す。
mu1<-outDEF$mu1;mu2<-outDEF$mu2;sigma1<-outDEF$sigma1;sigma2<-outDEF$sigma2;
xaste1<-outDEF$xaste1;xaste2<-outDEF$xaste2;log_lik<-outDEF$log_lik;

#生成量の計算  
mudef   <-mu1-mu2;                                      #(3.11)
delta1  <-mudef/sigma1;                                 #(3.15)
delta2  <-mudef/sigma2;                                 #(3.15)
U31     <-pnorm(mu1,mu2,sigma2);                        #(3.20)
U32     <-1-pnorm(mu2,mu1,sigma1);                      #(3.20)
yuetsu  <-pnorm(mudef/sqrt(sigma1^2+sigma2^2),0,1);     #(3.46)
ikijyo10<-pnorm((mudef-10)/sqrt(sigma1^2+sigma2^2),0,1);#(3.49)

#数値要約  表A.6
gqcal(mu1     );#
gqcal(mu2     );#
gqcal(sigma1  );#
gqcal(sigma2  );#
gqcal(xaste1  );#
gqcal(xaste2  );#

#数値要約  表A.7
gqcal(mudef   );#
gqcal(delta1  );#
gqcal(delta2  );#
gqcal(U31     );#
gqcal(U32     );#
gqcal(yuetsu  );#
gqcal(ikijyo10);#

#研究仮説が正しい確率　表A.8
phc01(c(0,13),mudef,0,cc="gtc", byoga="no");     #
phc01(1.2,delta2   ,0,cc="gtc", byoga="no");     #
phc01(2.0,delta1   ,0,cc="gtc", byoga="no");     #
phc01(0.8,U31      ,0,cc="gtc", byoga="no");     #
phc01(0.8,U32      ,0,cc="gtc", byoga="no");     #
phc01(0.8,yuetsu   ,0,cc="gtc", byoga="no");     #
phc01(0.7,ikijyo10 ,0,cc="gtc", byoga="no");     #


