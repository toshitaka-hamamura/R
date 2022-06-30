data {
  int<lower=0> n1;     int<lower=0> n2;       //データ数
  real x1[n1];         real x2[n2];           //データ
  real EQU;                                   //ＳＤ、1 - 共通、0 - 異なる
}
parameters {
  vector                    [2] mu;          //平均(範囲指定)
  real<lower=0>           sigma1;            //標準偏差(範囲指定)
  real<lower=0>           dummy;             //ダミーのＳＤ(範囲指定)
}
transformed parameters {
  real<lower=0>           sigma2;             //標準偏差2
  sigma2 =EQU>0.5 ? sigma1 : dummy;
}
model {
  x1 ~ normal(mu[1],sigma1);                  //正規分布1
  x2 ~ normal(mu[2],sigma2);                  //正規分布2
}
generated quantities{
  real xaste[2];
  real log_lik;
  xaste[1] = normal_rng(mu[1],sigma1);        //予測分布1
  xaste[2] = normal_rng(mu[2],sigma2);        //予測分布2
  log_lik =normal_lpdf(x1 | mu[1],sigma1)+
           normal_lpdf(x2 | mu[2],sigma2);       //対数尤度
}
