#��2�͗p�X�N���v�g
getwd()                        #working directory�̊m�F
source('myfunc/myfunc.R')      #����֐��̓ǂݍ���
library(rstan)                 #�p�b�P�[�Wrstan�̌Ăяo��
rstan_options(auto_write=T)
options(mc.cores=parallel::detectCores())

#�u�����f�[�^�v�̓���
x<-c(76.5,83.9,87.9,70.8,84.6,85.1,79.6,79.8,79.7,78.0)

#���K���z�Ɋւ��鐄��
out <-G1mean(x,prior=T,mL=0, mH=1000, sL=0, sH=100, fi=NA)       
traceplot(out$fit)
out$fit

#�ꐔ�̎��㕪�z
plot(density(out$mu))        #�}2.2 ���ς̎��㕪�z
plot(density(out$sigma))     #�}2.3 sd�̎��㕪�z
#����\�����z
plot(density(out$xaste))     #�}2.4

#�����ʂ̌v�Z
bunsan <-out$sigma^2
hendou <-out$sigma/out$mu
delta85<-(out$mu-85)/out$sigma
delta80<-(out$mu-80)/out$sigma
quarter<-out$mu-0.675*out$sigma
yosok85<-pnorm(85,out$mu,out$sigma)
yosok80<-pnorm(80,out$mu,out$sigma)
hirit85<-out$xaste/85

#�\2.4�@�����ʂ̐��茋��(���z�̗v��) 
gqcal(bunsan )#���U
gqcal(hendou )#�ϓ��W��
gqcal(delta85)#�f���^85
gqcal(delta80)#�f���^80
gqcal(quarter)#��1�l����
gqcal(yosok85)#85g�ȉ��̊m��
gqcal(yosok80)#80g�ȉ��̊m��
gqcal(hirit85)#85g�ɑ΂����


#�\2.5�@�����������������m��
phc01(c(85,80),out$mu,   0,cc="ltc", byoga="no"); #
phc01(c(85,80),out$xaste,0,cc="ltc", byoga="no"); #
phc01(-1      ,delta85,  0,cc="ltc", byoga="no"); #
phc01(0.0     ,delta80,  0,cc="ltc", byoga="no"); #
phc01(0.5     ,yosok85,  0,cc="ltc", byoga="no"); #
phc01(0.5     ,yosok80,  0,cc="ltc", byoga="no"); #

#phc�Ȑ�
phc01(seq(70,90,0.5),out$mu,   0,cc="ltc", byoga="yes"); #
gqcal(out$mu,probs=c(0.9, 0.95, 0.99))

#�����ʂ̃q�X�g�O����
plot(density(bunsan ),xlim=c(0,110))     #�}2.5
plot(density(hendou ),xlim=c(0.02,0.14)) #�}2.6
plot(density(delta85),xlim=c(-2,0.5))    #�}2.7���}
plot(density(delta80),xlim=c(-1,1))      #�}2.7�E�}
plot(density(quarter),xlim=c(65,85))     #�}2.8
plot(density(yosok85),xlim=c(0.3,1.0))   #�}2.9���}
plot(density(yosok80),xlim=c(0.1,0.9))   #�}2.9�E�}
plot(density(hirit85),xlim=c(0.7,1.2))   #�}2.10

### ��2�͏͖����
##�f�[�^
x<-c(
36,38,51,40,41,52,43,31,35,37,49,43,43,41,36,53,43,26,45,37,
33,38,33,35,36,28,46,41,32,49,43,38,46,46,46,45,44,40,38,37,
35,39,31,55,48,32,37,37,45,39,42,40,40,50,38,51,29,44,41,42,
43,36,38,33,32,42,43,40,46,54,37,24,47,35,35,47,38,31,41,39,
40,43,37,45,38,42,48,43,38,48,47,44,42,36,50,36,55,51,38,33)

out <-G1mean(x,prior=T,mL=0, mH=1000, sL=0, sH=100, fi=out$fit)       

#RQ.1,2,3,4,5
print(out$fit,,probs=c(0.025, 0.05, 0.5, 0.95, 0.975))

#�����ʂ̌v�Z
bunsan <-out$sigma^2
hendou <-out$sigma/out$mu
delta45<-(out$mu-45)/out$sigma
delta35<-(out$mu-35)/out$sigma
perce20<-out$mu+qnorm(0.2,0,1)*out$sigma
yosok45<-pnorm(45,out$mu,out$sigma)
yosok35<-pnorm(35,out$mu,out$sigma)
hirit45<-out$xaste/45

#RQ.6
gqcal(bunsan )#���U
#RQ.7
gqcal(hendou )#�ϓ��W��
#RQ.8,9
gqcal(delta45)#�f���^45
gqcal(delta35)#�f���^35
#RQ.10
gqcal(perce20)#��1�l����
#RQ.11
gqcal(yosok45)#45�_�������ώ@�����m��
gqcal(yosok35)#35�_�������ώ@�����m��
#RQ.12
gqcal(hirit45)#45�_�ɑ΂����

#RQ.13�@
phc01(seq(37,43,0.5),out$mu,   0,cc="ltc", byoga="yes"); #
gqcal(out$mu,probs=c(0.9, 0.95, 0.99))
phc01(c(45,35),out$mu,   0,cc="ltc", byoga="no"); #


#RQ.14�@
phc01(c(45,35),out$xaste,   0,cc="ltc", byoga="no"); #

#RQ.15 
phc01(-0.6,    delta45,   0,cc="ltc", byoga="no"); #
phc01( 0.6,    delta35,   0,cc="ltc", byoga="no"); #

#RQ.16
phc01( 0.8,    yosok45,   0,cc="ltc", byoga="no"); #
phc01( 0.8,    yosok35,   0,cc="ltc", byoga="no"); #