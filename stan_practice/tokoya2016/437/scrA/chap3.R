#��3�͗p�X�N���v�g
getwd()                        #working directory�̊m�F
source('myfunc/myfunc.R')      #����֐��̓ǂݍ���
library(rstan)                 #�p�b�P�[�Wrstan�̌Ăяo��
rstan_options(auto_write=T)
options(mc.cores=parallel::detectCores())

#�\3.1�̃f�[�^�A�N���XA�A�N���XB�̏��ɓ���
x1<-c(49,66,69,55,54,72,51,76,40,62,66,51,59,68,66,57,53,66,58,57)
x2<-c(41,55,21,49,53,50,52,67,54,69,57,48,31,52,56,50,46,38,62,59)

#�\3.2�@�v�񓝌v�ʁ@
(n1<-length(x1));(n2<-length(x2))         #�f�[�^�̐�
mean(x1);mean(x2)                         #���ϒl
van<-function(x){mean((x-mean(x))^2)}     #���U���v�Z����֐�
van(x1);van(x2)                           #���U
sqrt(van(x1));sqrt(van(x2))               #�W���΍�
sort(x1);sort(x2)                         #���������ɕ��ׂ�
median(x1);median(x2)                     #�����l
quantile(x1,type =2);quantile(x2,type =2) #���_
sort(x1);sort(x2)                         #�\3.3�@

#�}3.1���Ђ��}
x<-c(x1,x2);y<-c(rep("�N���XA",n1),rep("�N���XB",n2))
boxplot(x~y,cex.axis=2.0)                 

#�Ɨ�����2�Q�̍��̐����i�W���΍��͋��ʁj
outEQU<-G2Ind(x1,x2,EQU=1,prior=T,mL=0, mH=100, sL=0, sH=50, fi=NA)

#MCMC�W�{�̖��O�������̂Ŏ��o���BoutEQU$sigma1��sigma�Ƃ���
mu1<-outEQU$mu1;mu2<-outEQU$mu2;sigma<-outEQU$sigma1;
xaste1<-outEQU$xaste1;xaste2<-outEQU$xaste2;log_lik<-outEQU$log_lik;

#�W���΍����������f����WAIC  �\3.13�̍�
(waic<- (-2)*(log(mean(exp(log_lik)))) + 2*(var(log_lik)))

#�ꐔ�̐��l�v��@�\3.5
gqcal(mu1    );
gqcal(mu2    );
gqcal(sigma  ); 
mudef  <-mu1-mu2;                             #(3.11)
gqcal(mudef); 

#�����������������m���@�\3.6
phc01(c(0,3,5,10),mudef,0,cc="gtc", byoga="no");   #

#�f���^   �\3.7
delta  <-mudef/sigma;                         #(3.15)
gqcal(delta)
phc01(0.3,delta ,0,cc="gtc", byoga="no");            #

#��d���x �\3.8
U3     <-pnorm(mu1,mu2,sigma);                #(3.20)
gqcal(U3)
phc01(0.6,U3    ,0,cc="gtc", byoga="no");            #

#�D�z����臏㗦   �\3.9
gqcal(xaste1)
gqcal(xaste2)

#�D�z����臏㗦   �\3.10
yuetsu <-pnorm(delta/sqrt(2),0,1);            #(3.24)
ikijyo3<-pnorm((mudef-3)/(sqrt(2)*sigma),0,1);#(3.28)
gqcal(yuetsu)
gqcal(ikijyo3)
phc01(0.8,yuetsu, 0,cc="gtc", byoga="no");           #p.76����
phc01(0.8,ikijyo3,0,cc="gtc", byoga="no");           #p.77����

#���x�֐�
plot(density(mudef  ))      #�}3.3�E�}
plot(density(delta  ))      #�}3.4���}
plot(density(U3     ))      #�}3.4�E�}
plot(density(yuetsu ))      #�}3.6���}
plot(density(ikijyo3))      #�}3.6�E�}


#�Ɨ�����2�Q�̍��̐����i�W���΍����قȂ�j
outDEF<-G2Ind(x1,x2,EQU=0,prior=T,mL=0, mH=100, sL=0, sH=50)

#MCMC�W�{�̖��O�������̂Ŏ��o���B
mu1<-outDEF$mu1;mu2<-outDEF$mu2;sigma1<-outDEF$sigma1;sigma2<-outDEF$sigma2;
xaste1<-outDEF$xaste1;xaste2<-outDEF$xaste2;log_lik<-outDEF$log_lik;

#�W���΍����������f����WAIC  �\3.13�̉E
(waic<- (-2)*(log(mean(exp(log_lik)))) + 2*(var(log_lik)))

#�����ʂ̌v�Z  
mudef   <-mu1-mu2;                                     #(3.11)
delta1  <-mudef/sigma1;                                #(3.15)
delta2  <-mudef/sigma2;                                #(3.15)
U31     <-pnorm(mu1,mu2,sigma2);                       #(3.20)
U32     <-1-pnorm(mu2,mu1,sigma1);                     #(3.20)
yuetsu  <-pnorm(mudef/sqrt(sigma1^2+sigma2^2),0,1);    #(3.46)
ikijyo3 <-pnorm((mudef-3)/sqrt(sigma1^2+sigma2^2),0,1);#(3.49)

#���l�v��  �\3.11
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

#�����������������m���@�\3.12
phc01(c(0,3,5,10),mudef,0,cc="gtc", byoga="no");     #
phc01(0.3,delta1 ,0,cc="gtc", byoga="no");           #
phc01(0.6,U31    ,0,cc="gtc", byoga="no");           #
phc01(0.8,yuetsu, 0,cc="gtc", byoga="no");           #
phc01(0.8,ikijyo3,0,cc="gtc", byoga="no");           #

### ��3�͏͖����

#�f�[�^���́@�늳�Q���1�Q�C����Q���2�Q�Ƃ���
x1<-c(
56,55,55,62,54,63,47,58,56,56,57,52,53,50,50,57,57,55,60,65,53,43,60,51,52,
60,54,49,56,54,55,57,53,58,54,57,60,57,53,61,60,58,56,52,62,52,66,63,54,50)
x2<-c(
33,37,59,41,42,61,46,25,32,35,55,44,45,41,33,61,46,16,48,34,27,37,28,31,32,
20,50,42,26,55,45,36,51,51,50,48,47,39,36,35,32,38,25,66,54,27,35,34,49,39)
n1<-length(x1);n2<-length(x2);

## 1�@�W�{���ρC�W�{���U�C�W�{sd�C�W�{�l���ʓ_ �\A.5
x<-c(x1,x2)
y<-c(rep("�N���XA",n1),rep("�N���XB",n2))
mean(x1);mean(x2)
van<-function(x){mean((x-mean(x))^2)}
van(x1);van(x2)
sqrt(van(x1));sqrt(van(x2))
median(x1);median(x2)
quantile(x1,type =2);quantile(x2,type =2)
sort(x1);sort(x2)
boxplot(x~y,cex.axis=2.0)
outDEF<-G2Ind(x1,x2,EQU=0,prior=T,mL=0, mH=100, sL=0, sH=50)

#MCMC�W�{�̖��O�������̂Ŏ��o���B
mu1<-outDEF$mu1;mu2<-outDEF$mu2;sigma1<-outDEF$sigma1;sigma2<-outDEF$sigma2;
xaste1<-outDEF$xaste1;xaste2<-outDEF$xaste2;log_lik<-outDEF$log_lik;

#�����ʂ̌v�Z  
mudef   <-mu1-mu2;                                      #(3.11)
delta1  <-mudef/sigma1;                                 #(3.15)
delta2  <-mudef/sigma2;                                 #(3.15)
U31     <-pnorm(mu1,mu2,sigma2);                        #(3.20)
U32     <-1-pnorm(mu2,mu1,sigma1);                      #(3.20)
yuetsu  <-pnorm(mudef/sqrt(sigma1^2+sigma2^2),0,1);     #(3.46)
ikijyo10<-pnorm((mudef-10)/sqrt(sigma1^2+sigma2^2),0,1);#(3.49)

#���l�v��  �\A.6
gqcal(mu1     );#
gqcal(mu2     );#
gqcal(sigma1  );#
gqcal(sigma2  );#
gqcal(xaste1  );#
gqcal(xaste2  );#

#���l�v��  �\A.7
gqcal(mudef   );#
gqcal(delta1  );#
gqcal(delta2  );#
gqcal(U31     );#
gqcal(U32     );#
gqcal(yuetsu  );#
gqcal(ikijyo10);#

#�����������������m���@�\A.8
phc01(c(0,13),mudef,0,cc="gtc", byoga="no");     #
phc01(1.2,delta2   ,0,cc="gtc", byoga="no");     #
phc01(2.0,delta1   ,0,cc="gtc", byoga="no");     #
phc01(0.8,U31      ,0,cc="gtc", byoga="no");     #
phc01(0.8,U32      ,0,cc="gtc", byoga="no");     #
phc01(0.8,yuetsu   ,0,cc="gtc", byoga="no");     #
phc01(0.7,ikijyo10 ,0,cc="gtc", byoga="no");     #

