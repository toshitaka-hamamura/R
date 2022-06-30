data{
  int<lower = 1> N;
  real kddi[N];
  real<lower = 0, upper = 1> sex[N];
  real<lower = 0> age[N];
}

parameters{
  vector[3] beta;
  real<lower = 0> sigma_e;
}

model{
  real mu;
  for (i in 1:N){
    mu = beta[1] + beta[2] * sex[i] + beta[3] * age [i];
    kddi ~ normal(mu, sigma_e);
  }
}
