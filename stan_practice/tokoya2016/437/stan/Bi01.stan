//1�Q�̔䗦�Ɋւ��铝�v�I����
data { 
  int<lower=0> x;                           //�������� 
  int<lower=0> n;                           //�f�[�^�� 
}
parameters {
  real<lower=0,upper=1>   theta;            //��䗦
}
transformed parameters {
}
model {
  x ~ binomial(n,theta);
}
generated quantities{
  int<lower=0> xaste;
  real<lower=0> Odds;
  real log_lik;
xaste = binomial_rng(n,theta);              //�\�����z
Odds =theta/(1-theta);                      //�I�b�Y
log_lik =binomial_lpmf(x|n,theta);          //�ΐ��m��
}


