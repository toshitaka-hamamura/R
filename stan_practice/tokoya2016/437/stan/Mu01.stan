//カテゴリ数がkの比率の統計的推測
data { 
  int<lower=0>    k;                       //カテゴリ数 
  int<lower=0> x[k];                       //反応数 
}
transformed data{
}
parameters {
  simplex[k]    pi;                        //母比率
}
model {
  x ~ multinomial(pi);                     //多項分布
}
generated quantities{
  real<lower=0,upper=1>   U2[k,k];         //2水準比較
  real log_lik;
  log_lik  = multinomial_lpmf(x | pi);
  for (i in 1:k){  U2[i,i] =0;}
  for (i in 1:(k-1)){
    for (j in (i+1):k){
      U2[i,j] = pi[i]-pi[j]>0 ? 1 : 0;
      U2[j,i] = !(U2[i,j]);  }  }
}
