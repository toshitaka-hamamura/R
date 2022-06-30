//2�v���Ή��Ȃ� �A���o�����X�ł��v�Z�\
data { 
  int<lower=1> n;                              //�f�[�^�� 
  int<lower=2> a;                              //A������  
  int<lower=2> b;                              //B������  
  vector[n]    y;                              //�����l   
  int<lower=1> A[n];                           //A����   
  int<lower=1> B[n];                           //B����   
}
parameters {
  real                           mu;           //�S����
  vector                   [a-1] m1A;          //A����1���Ȃ�
  vector                   [b-1] m1B;          //B����1���Ȃ�
  matrix               [a-1,b-1] m1AB;         //���ݍ�p1���Ȃ�
  real<lower=0>             sigmaE;            //E�W���΍�
}
transformed parameters {
  vector                   [a] muA;            //A����
  vector                   [b] muB;            //B����
  matrix                   [a,b] muAB;         //���ݍ�p
  vector                   [a-1] m1a;          //�r���v�Z�p
  vector                   [b-1] m1b;          //�r���v�Z�p
  for(i in 1:(a-1)){ muA[i]  = m1A[i]; m1a[i] = 0.0 ;}   
  muA[a]  = -sum(m1A);
  for(j in 1:(b-1)){ muB[j]  = m1B[j]; m1b[j] = 0.0 ;}   
  muB[b]  = -sum(m1B); 
  for(i in 1:(a-1)){ for(j in 1:(b-1)){
      muAB[i,j] =m1AB[i,j]; m1a[i] =m1a[i]+m1AB[i,j];}  }
  for(j in 1:(b-1)){ for(i in 1:(a-1)){
                            m1b[j] =m1b[j]+m1AB[i,j];}  }
  for(i in 1:(a-1)){muAB[i,b] = (-1)*m1a[i]; }
  for(j in 1:(b-1)){muAB[a,j] = (-1)*m1b[j]; }
  muAB[a,b] = sum(m1a);                         //�����̓}�C�i�X�ł͂Ȃ�
}
model {
  for(i in 1:n){
     y[i] ~ normal(mu+muA[A[i]]+muB[B[i]]+muAB[A[i],B[i]],sigmaE);}
}
generated quantities{
  real<lower=sL,upper=sH>  sigmaA;              //A�W���΍�
  real<lower=sL,upper=sH>  sigmaB;              //B�W���΍�
  real<lower=sL,upper=sH>  sigmaAB;             //AB�W���΍�
  matrix             [a,b] cellmean;            //�Z�����ϒl
  real log_lik;                                 //�ΐ��ޓx
  real<lower=0,upper=1>      eta2A;             //A������
  real<lower=0,upper=1>      eta2B;             //B������
  real<lower=0,upper=1>      eta2AB;            //AB������
  real<lower=0,upper=1>      eta2T;             //�S������
  real<lower=0>               ABAB;             //���ԕϐ�A+B+AB���U
  real<lower=0>             deltaA;             //A���ʗ�
  real<lower=0>             deltaB;             //B���ʗ�
  real<lower=0>             deltaAB;            //AB���ʗ�
  real<lower=0,upper=1>   UbigA[a];             //A�_���l0�ȏ�m��
  real<lower=0,upper=1>   UsmaA[a];             //A�_���l0�ȉ��m��
  real<lower=0,upper=1>   UbigB[b];             //B�_���l0�ȏ�m��
  real<lower=0,upper=1>   UsmaB[b];             //B�_���l0�ȉ��m��
  real<lower=0,upper=1>   UbigAB[a,b];          //AB�_���l0�ȏ�m��
  real<lower=0,upper=1>   UsmaAB[a,b];          //AB�_���l0�ȉ��m��
  real<lower=0,upper=1>   U2A[a,a];             //A��2������r
  real<lower=0,upper=1>   U2B[b,b];             //B��2������r
  sigmaA   = sqrt(variance(muA)*(a-1)/a);       
  sigmaB   = sqrt(variance(muB)*(b-1)/b);       
  sigmaAB  = sqrt(variance(muAB)*(a*b-1)/(a*b));
  ABAB     = pow(sigmaA,2)+pow(sigmaB,2)+pow(sigmaAB,2);
  eta2A    = pow(sigmaA,2)/(ABAB+pow(sigmaE,2));
  eta2B    = pow(sigmaB,2)/(ABAB+pow(sigmaE,2));
  eta2AB   = pow(sigmaAB,2)/(ABAB+pow(sigmaE,2));
  eta2T    = ABAB/(ABAB+pow(sigmaE,2));
  deltaA   = sigmaA/sigmaE;
  deltaB   = sigmaB/sigmaE;
  deltaAB  = sigmaAB/sigmaE;
  for(i in 1:a){ for(j in 1:b){cellmean[i,j] = mu+muA[i]+muB[j]+muAB[i,j]; }}
  for (i in 1:a){UbigA[i] =muA[i]>0 ? 1 : 0;
                 UsmaA[i] =muA[i]<0 ? 1 : 0;
                 U2A[i,i] =0;                   }
  for (i in 1:b){UbigB[i] =muB[i]>0 ? 1 : 0;
                 UsmaB[i] =muB[i]<0 ? 1 : 0;
                 U2B[i,i] =0;                   }
  for (i in 1:a){for (j in 1:b){
                 UbigAB[i,j] =muAB[i,j]>0 ? 1 : 0;
                 UsmaAB[i,j] =muAB[i,j]<0 ? 1 : 0; }}
  for (i in 1:(a-1)){    for (j in (i+1):a){
      U2A[i,j] = muA[i]-muA[j]>0 ? 1 : 0;
      U2A[j,i] = !(U2A[i,j]);                           }}
  for (i in 1:(b-1)){    for (j in (i+1):b){
      U2B[i,j] = muB[i]-muB[j]>0 ? 1 : 0;
      U2B[j,i] = !(U2B[i,j]);                           }}
  log_lik  = 0.0;
  for(i in 1:n){ log_lik  = log_lik + 
    normal_lpdf(y[i] | mu+muA[A[i]]+muB[B[i]]+muAB[A[i],B[i]], sigmaE);}
}
