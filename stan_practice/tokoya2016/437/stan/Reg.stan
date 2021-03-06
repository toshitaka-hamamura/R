//dρAf
data { 
  int<lower=0>  n;                         //f[^ 
  int<lower=0>  p;                         //\ͺΟ   
  vector[n]     y;                         //ξΟ   
  matrix[n,p+1] X;                         //\ͺΟsρ   
}
parameters {
  vector[p+1]   bb;                        //ΞρAWxNg  
  real<lower=0> sigma;                     //λ·WΞ·
}
transformed parameters {
  vector[n]  yhat;                         //yhat  
  yhat =X*bb;
}
model {
    y ~ normal(yhat, sigma);               //³Kͺzf
}
generated quantities{
  real              a;                     //ΨΠ
  vector[p]         b;                     //ΞρAWiΨΠΘ΅j   
  real log_lik;                            //Ξήx
  real<lower=0> vyhat;                     //yhatΜͺU
  real<lower=0,upper=1>    r2;             //θW
  real<lower=0,upper=1>    r;              //dΦW
  a =bb[p+1];
  for (i in 1:p) {b[i] =bb[i];}
  vyhat =(variance(yhat)*(n-1))/n;
  r2 = vyhat/(vyhat+sigma^2);
  r =sqrt(r2);
  log_lik  =  0;
  for (i in 1:n) {
     log_lik  = log_lik + normal_lpdf(y[i] | yhat[i], sigma);}    
}
