//�d��A���f��
data { 
  int<lower=0>  n;                         //�f�[�^�� 
  int<lower=0>  p;                         //�\���ϐ���   
  vector[n]     y;                         //��ϐ�   
  matrix[n,p+1] X;                         //�\���ϐ��s��   
}
parameters {
  vector[p+1]   bb;                        //�Ή�A�W���x�N�g��  
  real<lower=0> sigma;                     //�덷�W���΍�
}
transformed parameters {
  vector[n]  yhat;                         //yhat  
  yhat =X*bb;
}
model {
    y ~ normal(yhat, sigma);               //���K���z���f��
}
generated quantities{
  real              a;                     //�ؕ�
  vector[p]         b;                     //�Ή�A�W���i�ؕЂȂ��j   
  real log_lik;                            //�ΐ��ޓx
  real<lower=0> vyhat;                     //yhat�̕��U
  real<lower=0,upper=1>    r2;             //����W��
  real<lower=0,upper=1>    r;              //�d���֌W��
  a =bb[p+1];
  for (i in 1:p) {b[i] =bb[i];}
  vyhat =(variance(yhat)*(n-1))/n;
  r2 = vyhat/(vyhat+sigma^2);
  r =sqrt(r2);
  log_lik  =  0;
  for (i in 1:n) {
     log_lik  = log_lik + normal_lpdf(y[i] | yhat[i], sigma);}    
}
