//1�v���Ή��Ȃ� �A���o�����X�ł��v�Z�\
data { 
  int<lower=0> n;                              //�f�[�^�� 
  int<lower=0> a;                              //A������   
  vector[n]    y;                              //�����l   
  int<lower=0> A[n];                           //A����   
  real mL;real mH;real sL;real sH;             //���O���z
}
parameters {
  vector<lower=mL,upper=mH>[a] muA;            //A����
  real<lower=sL,upper=sH>   sigmaE;            //�덷SD
}
transformed parameters {
}
model {
  for(i in 1:n){y[i]~normal(muA[A[i]],sigmaE);}//���K���z
}
generated quantities{
  real<lower=sL,upper=sH>  sigmaA;             //�v��ASD
  real<lower=0,upper=1>      eta2;             //������
  real<lower=0>             delta;             //���ʗ�
  real                         mu;             //�S����
  vector                   [a] aj;             //A����
  real<lower=0,upper=1>   Ubig[a];             //�_���l0�ȏ�
  real<lower=0,upper=1>   Usma[a];             //�_���l0�ȉ�
  real<lower=0,upper=1>   U2[a,a];             //2������r
  real log_lik;
  sigmaA  = sqrt(variance(muA)*(a-1)/a);       
  eta2  = pow(sigmaA,2)/(pow(sigmaA,2)+pow(sigmaE,2));
  delta  = sigmaA/sigmaE;
  mu  = mean(muA);
  for (i in 1:a){aj[i]  = muA[i]-mu;
                 Ubig[i] =aj[i]>0 ? 1 : 0;
                 Usma[i] =aj[i]<0 ? 1 : 0;
                 U2[i,i] =0;                   }
  for (i in 1:(a-1)){
    for (j in (i+1):a){
      U2[i,j] = aj[i]-aj[j]>0 ? 1 : 0;
      U2[j,i] = !(U2[i,j]);  }  }
  log_lik  = 0.0;
  for(i in 1:n){ log_lik  = log_lik + 
        normal_lpdf(y[i] | muA[A[i]], sigmaE); //�ΐ��ޓx
        }
}
