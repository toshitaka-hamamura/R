//重回帰モデル
data { 
  int<lower=0>  n;                         //データ数 
  int<lower=0>  p;                         //予測変数数   
  vector[n]     y;                         //基準変数   
  matrix[n,p+1] X;                         //予測変数行列   
}
parameters {
  vector[p+1]   bb;                        //偏回帰係数ベクトル  
  real<lower=0> sigma;                     //誤差標準偏差
}
transformed parameters {
  vector[n]  yhat;                         //yhat  
  yhat =X*bb;
}
model {
    y ~ normal(yhat, sigma);               //正規分布モデル
}
generated quantities{
  real              a;                     //切片
  vector[p]         b;                     //偏回帰係数（切片なし）   
  real log_lik;                            //対数尤度
  real<lower=0> vyhat;                     //yhatの分散
  real<lower=0,upper=1>    r2;             //決定係数
  real<lower=0,upper=1>    r;              //重相関係数
  a =bb[p+1];
  for (i in 1:p) {b[i] =bb[i];}
  vyhat =(variance(yhat)*(n-1))/n;
  r2 = vyhat/(vyhat+sigma^2);
  r =sqrt(r2);
  log_lik  =  0;
  for (i in 1:n) {
     log_lik  = log_lik + normal_lpdf(y[i] | yhat[i], sigma);}    
}
