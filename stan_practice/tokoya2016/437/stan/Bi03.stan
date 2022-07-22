//�Ή��̂Ȃ�g�Q�̔䗦�Ɋւ��铝�v�I����
data { 
  int<lower=0> g;                              //�Q�̐� 
  int<lower=0> x[g];                           //�������� 
  int<lower=0> n[g];                           //�f�[�^�� 
}
parameters {
  real<lower=0,upper=1>   p[g];                //��䗦
}
transformed parameters {
}
model {
for (i in 1:g){
  x[i] ~ binomial(n[i],p[i]);}
}
generated quantities{
  int<lower=0> xaste[g];
  real log_lik;
  real<lower=0,upper=1>   U2[g,g];             //2������r
  for (i in 1:g){  U2[i,i] =0;}
  for (i in 1:(g-1)){
    for (j in (i+1):g){
      U2[i,j] = p[i]-p[j]>0 ? 1 : 0;
      U2[j,i] = !(U2[i,j]);  }  }
  log_lik  = 0.0;
  for (i in 1:g){
    xaste[i] = binomial_rng(n[i],p[i]);                  //�\�����z
    log_lik  = log_lik + binomial_lpmf(x[i]|n[i],p[i]);} //�ΐ��m��
}



