//�J�e�S������k�̔䗦�̓��v�I����
data { 
  int<lower=0>    k;                       //�J�e�S���� 
  int<lower=0> x[k];                       //������ 
}
transformed data{
}
parameters {
  simplex[k]    pi;                        //��䗦
}
model {
  x ~ multinomial(pi);                     //�������z
}
generated quantities{
  real<lower=0,upper=1>   U2[k,k];         //2������r
  real log_lik;
  log_lik  = multinomial_lpmf(x | pi);
  for (i in 1:k){  U2[i,i] =0;}
  for (i in 1:(k-1)){
    for (j in (i+1):k){
      U2[i,j] = pi[i]-pi[j]>0 ? 1 : 0;
      U2[j,i] = !(U2[i,j]);  }  }
}
