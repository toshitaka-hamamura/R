#####model4-ex1.stan######
data{
  int N;
  real X1[N];
  real X2[N];
  real t[N];
}

parameters{
  real a;
  real b;
  real<lower=0> sigma;
}

model{
  for (n in 1:N) {
    t[n] ~ normal((X1[n]-X2[n])/(sigma * sqrt(2/n)), sigma);
  }
}