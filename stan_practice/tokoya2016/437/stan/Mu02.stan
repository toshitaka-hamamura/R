//�Ή��̂���a�~b�̃N���X�\�Ɋւ��铝�v�I����
data { 
  int<lower=0> a;                          //�s�� 
  int<lower=0> b;                          //�� 
  int<lower=0> x[a,b];                     //�������̍s��`�� 
}
transformed data{
  int<lower=0> N;                          //���v������ 
  int<lower=0> ab;                         //�Z�����v�� 
  int<lower=0> xv[a*b];                    //�������̃x�N�g���`��
  ab  = a*b;
  for (i in 1:a){
    for (j in 1:b){
      xv[(i-1)*b+j]  = x[i,j];}}
  N  = sum(xv);
}
parameters {
  simplex[ab]    pi;                        //�a��1�̕�䗦�̃x�N�g���`��
}
transformed parameters{
  real<lower=0,upper=1> pim[a,b];           //�a��1�̕�䗦�̍s��`��
  for (i in 1:a){
    for (j in 1:b){
      pim[i,j]  = pi[(i-1)*b+j];}}
}
model {
  xv ~ multinomial(pi);                     //�������z
}
generated quantities{
  int<lower=0> xastev[ab];                  //�\�����z�x�N�g���`��
  int<lower=0> xaste[a,b];                  //�\�����z�s��`��
  real log_lik;
  real pa[a]; real pb[b];  real V;  real res[a,b];
  real<lower=0,upper=1>   Up[a,b];          //�s�A�\���c�����{
  real<lower=0,upper=1>   Um[a,b];          //�s�A�\���c�����[
    xastev    = multinomial_rng(pi,N);      //�\�����z
    V  = 0;
    for (i in 1:a){pa[i] =0;}               //���ӊm��
    for (j in 1:b){pb[j] =0;}
    for (i in 1:a){
      for (j in 1:b){
        pa[i] =pa[i]+pim[i,j];
        pb[j] =pb[j]+pim[i,j];
    }}
    V  = 0;
    for (i in 1:a){
      for (j in 1:b){
        xaste[i,j]  = xastev[(i-1)*b+j];
        res[i,j]    =(pim[i,j]-pa[i]*pb[j])/sqrt(pa[i]*pb[j]);
        V  = V+ pow(res[i,j],2);
        Up[i,j] = res[i,j]>0 ? 1 : 0;
        Um[i,j] = !(Up[i,j]);
    }}
    V  = sqrt(V /(min(a,b)-1));
    log_lik  = multinomial_lpmf(xv | pi);      //�ΐ��m��

}
