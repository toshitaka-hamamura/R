#��5�͗p�X�N���v�g
getwd()                        #working directory�̊m�F
source('myfunc/myfunc.R')      #����֐��̓ǂݍ���
library(rstan)                 #�p�b�P�[�Wrstan�̌Ăяo��
rstan_options(auto_write=T)
options(mc.cores=parallel::detectCores())

#�\5.1�́u�����_�K�X�f�[�^�v�̓���
  A <- rep(1:4,each=6);a<-length(unique(A))
  y <- c(
    10, 10, 09, 11, 12, 11,   08, 10, 08, 10, 12, 09,
    08, 08, 11, 11, 14, 15,   14, 12, 11, 16, 13, 12)
AA<-c(rep("1�t",6),rep("2��",6),rep("3�H",6),rep("4�~",6))
boxplot(y~AA)

#1�v�������̐���
aaa<-E1Ind(y,A,prior=T,mL=0, mH=50, sL=0, sH=50, fi=NA)

#�\5.2 �ꐔ�̐��茋��
colnames(aaa$muA)    <-paste("muA",1:4,sep="")
gqcal(aaa$muA,   1)    ;
gqcal(aaa$sigmaE,1)    ;

#�\5.3 �����ʂ̐��茋��
gqcal(aaa$mu,1)    ;
colnames(aaa$aj)    <-paste("aj",1:4,sep="")
gqcal(aaa$aj,1)    ;

#�\5.4 �����̌��ʂ�0���傫���i�������j�m��
(Upphc<-round(apply(aaa$Ubig,2,mean),3))    ;
1-Upphc;

#�\5.5 ���ʂ̑傫���Ɋւ��鐶���ʂ̐��茋��
gqcal(aaa$sigmaA,1)    ;
gqcal(aaa$eta2,  3)    ;
gqcal(aaa$delta, 3)    ;

#�\5.6 �s�̐����̌��ʂ���̐����̌��ʂ��傫���m��
phc02(0,aaa$aj, cc="gtc")

#�}5.1 ���Ђ��}
size<-dim(aaa$muA)[1];              #������
aa<-aaa$aj[,1]; for (i in 2:a){aa<-c(aa,aaa$aj[,i])};
boxplot(aa~rep(1:a,each=size)); abline(h=0.0,lwd=1.0)

#�������ƌ��ʗʂ̎��㕪�z
plot(density(aaa$eta2))       #�}5.2��
plot(density(aaa$delta))      #�}5.2�E

#�A�����肪�������m��
printIJ(aaa$U2,3,IJ=rbind(c(4,3),c(3,1),c(1,2)))
printIJ(aaa$U2,3,IJ=rbind(c(4,3),c(4,1),c(3,2),c(1,2)))
printIJ(aaa$U2,3,IJ=rbind(c(4,1),c(4,2),c(4,3)))
printIJ(aaa$U2,3,IJ=rbind(c(4,1),c(4,2)))

#�\5.7  ���ɋ����̂���2�����Ԃ̔�r
E1betw_level(aaa,3,4,2,cr1=2.0)

#�}5.3(��Ƃ��Č��ʗʂƔ�d���x�̎��㕪�z������)
plot(density(aaa$muA[,4]-aaa$muA[,2]))
plot(density(pnorm(aaa$muA[,4],aaa$muA[,2],aaa$sigmaE)))

#�\5.8�́u�T���t�����V�X�R�ƃ��T���[���X�̃z�e���̗����v�̓���
A <- rep(1:2,each=18)
B <- rep(1:2,each=9,times=2)
y <- c(
 079,107,103,092,180,165,240,265,300,
 075,060,060,094,119,100,102,125,165,
 095,099,070,116,170,145,205,200,210,
 153,078,075,092,115,155,250,340,380)

#2�v�������̐���
bbb<-E2Ind(y,A,B,prior=T,mL=0, mH=1000, sL=0, sH=500)

#�\5.9  �ꐔ�̐��茋��
gqcal(bbb$mu,        1)    ;
gqcal(bbb$muA[,1],   1)    ;
gqcal(bbb$muB[,1],   1)    ;
gqcal(bbb$muAB[,1,1],1)    ;
gqcal(bbb$sigmaE,    1)    ;

#�\5.10 �����E���ݍ�p��0���傫��(������)�m��
mean(bbb$muA[,1]>0);mean(bbb$muB[,1]>0);mean(bbb$muAB[,1,1]>0 )
mean(bbb$muA[,1]<=0);mean(bbb$muB[,1]<=0);mean(bbb$muAB[,1,1]<=0)

#�\5.11 ���ʂ̑傫���Ɋւ��鐶���ʂ̐��茋��
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

#�\5.12 �Z�����ς̐��茋��
cellmean  <-cbind(bbb$cellmea[,1,]  ,bbb$cellmean[,2,])
colnames(cellmean)<-as.vector(t(outer(1:2,1:2,paste,sep=",")))
gqcal(cellmean,1)  ;  

#���ɋ����̂���2�Z���Ԃ̔�r
#�\5.13 �T���t�����V�X�R�̐��茋��
E2betw_level(bbb,degits=3,H="A",F=1,I=1,J=2,cr1=10)
#�\5.14 ���T���[���X�̐��茋��
E2betw_level(bbb,degits=3,H="A",F=2,I=2,J=1,cr1=10)

### ��5�͏͖����

## 1
#�\5.15 �}�E�X�̑̏d�̃f�[�^
y<-c(05.02, 06.67, 08.17, 02.79, 08.13, 06.34, 06.32, 03.97,
     09.89, 09.58, 11.20, 09.05, 12.33, 09.39, 10.88, 09.37, 17.40,
     10.20, 07.29, 07.57, 03.42, 05.82, 10.92, 05.21, 13.47, 08.64, 06.05)
A<-c(rep(1,8),rep(2,9),rep(3,10))
a<-length(unique(A))
aaa<-E1Ind(y,A,prior=T,mL=0, mH=50, sL=0, sH=50)

#�\A.13 �ꐔ�̎��㕪�z�̐��l�v��
colnames(aaa$muA)    <-paste("muA",1:3,sep="")
gqcal(aaa$muA,   3)    ;
gqcal(aaa$sigmaE,3)    ;

#�\A.14�����ʂ̎��㕪�z�̐��l�v��
gqcal(aaa$mu,3)    ;
colnames(aaa$aj)    <-paste("aj",1:3,sep="")
gqcal(aaa$aj,3)    ;

#�\A.15�����̌��ʂ�0���傫���i�������j�m��
(Upphc<-round(apply(aaa$Ubig,2,mean),3))    ;
1-Upphc;

#�\A.16���ʂ̑傫���Ɋւ��鐶���ʂ̐��茋��
gqcal(aaa$sigmaA,3)    ;
gqcal(aaa$eta2,  3)    ;
gqcal(aaa$delta, 3)    ;

#�\A.17�s�̐����̌��ʂ���̐����̌��ʂ��傫���m��
phc02(0,aaa$aj, cc="gtc")

#�\A.18
printIJ(aaa$U2,3,IJ=rbind(c(2,3),c(3,1)))
E1betw_level(aaa,3,2,1,cr1=5.0)

## 2
#�\5.16 ���҂̗L���ɂ��I��d�̋���ʂ̋���
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

#�\A.19 �ꕔ�̕ꐔ�̎��㕪�z�̐��l�v��
gqcal(bbb$mu,        1)    ;
gqcal(bbb$muA[,1],   3)    ;
gqcal(bbb$sigmaE,    3)    ;

#�\A.20 �v���a�i����j�̐����̌��ʂ�0���傫���i�������j�m��
(Upphc<-round(apply(bbb$UbigB,2,mean),3))    ;
1-Upphc;

#�\A.21���ʂ̑傫���Ɋւ��鐶���ʂ̐��茋��
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

#�\A.22 �s�̐�������̐������傫���m��
phc02(c=0.0 ,bbb$muB,cc="gtc") ;

#�\A.23
E2betw_level(bbb,degits=3,H="A",F=2,I=4,J=6,cr1=10)

#�A�����肪�������m��
printIJ(bbb$U2B,3,IJ=rbind(c(1,3),c(1,2),c(3,5),c(3,4),c(3,6)))
printIJ(bbb$U2B,3,IJ=rbind(c(1,2),c(2,3),c(2,5),c(2,4),c(2,6)))
printIJ(bbb$U2B,3,IJ=rbind(c(1,2),c(2,3),c(3,5),c(3,4),c(3,6)))