data {
  int<lower=0> n1;     int<lower=0> n2;       //�f�[�^��
  real x1[n1];         real x2[n2];           //�f�[�^
  real EQU;                                   //�r�c�A1 - ���ʁA0 - �قȂ�
}
parameters {
  vector                    [2] mu;          //����(�͈͎w��)
  real<lower=0>           sigma1;            //�W���΍�(�͈͎w��)
  real<lower=0>           dummy;             //�_�~�[�̂r�c(�͈͎w��)
}
transformed parameters {
  real<lower=0>           sigma2;             //�W���΍�2
  sigma2 =EQU>0.5 ? sigma1 : dummy;
}
model {
  x1 ~ normal(mu[1],sigma1);                  //���K���z1
  x2 ~ normal(mu[2],sigma2);                  //���K���z2
}
generated quantities{
  real xaste[2];
  real log_lik;
  xaste[1] = normal_rng(mu[1],sigma1);        //�\�����z1
  xaste[2] = normal_rng(mu[2],sigma2);        //�\�����z2
  log_lik =normal_lpdf(x1 | mu[1],sigma1)+
           normal_lpdf(x2 | mu[2],sigma2);       //�ΐ��ޓx
}
