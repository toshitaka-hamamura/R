//対応のないg群の比率に関する統計的推測
data { 
  int<lower=0> g;                              //群の数 
  int<lower=0> x[g];                           //正反応数 
  int<lower=0> n[g];                           //データ数 
}
parameters {
  real<lower=0,upper=1>   p[g];                //母比率
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
  real<lower=0,upper=1>   U2[g,g];             //2水準比較
  for (i in 1:g){  U2[i,i] =0;}
  for (i in 1:(g-1)){
    for (j in (i+1):g){
      U2[i,j] = p[i]-p[j]>0 ? 1 : 0;
      U2[j,i] = !(U2[i,j]);  }  }
  log_lik  = 0.0;
  for (i in 1:g){
    xaste[i] = binomial_rng(n[i],p[i]);                  //予測分布
    log_lik  = log_lik + binomial_lpmf(x[i]|n[i],p[i]);} //対数確率
}



