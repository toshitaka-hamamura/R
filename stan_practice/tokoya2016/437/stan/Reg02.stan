//�d��A���f��
data { 
  int<lower=0>  n;                     //�f�[�^�� 
  int<lower=0>  p;                     //�\���ϐ���   
  vector[n]     y;                     //��ϐ�   
  matrix[n,p]   X;                     //�\���ϐ��s��   
}
parameters {
  real        a;                       //�ؕ�
  vector[p]   b;                       //�Ή�A�W���x�N�g��  
  real<lower=0> sigma;                 //�덷�W���΍�
}
transformed parameters {
  vector[n]  yhat;                     //yhat  
  yhat = a + X*b;
}
model {
    y ~ normal(yhat, sigma);           //���K���z���f��
}
generated quantities{
  real log_lik;                        //�ΐ��ޓx
  real<lower=0> vyhat;                 //yhat�̕��U
  real<lower=0,upper=1>    r2;         //����W��
  vyhat =(variance(yhat)*(n-1))/n;
  r2 = vyhat/(vyhat+sigma^2);
  log_lik  =  normal_lpdf(y | a + X*b, sigma);    
}
