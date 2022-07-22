data {
  int<lower=1> N;
  real kddi[N];
  real<lower = 0, upper = 1> sex[N];
  real<lower = 0> age[N];
  real<lower = 12, upper=84> openness[N];
  real<lower = 12, upper=84> consc[N];
  real<lower = 12, upper=84> extrav[N];
  real<lower = 12, upper=84> agree[N];
  real<lower = 12, upper=84> neur[N];
}

parameters {
  real beta_intcpt;
  real beta_sex;
  real beta_age;
  real beta_openness;
  real beta_consc;
  real beta_extrav;
  real beta_agree;
  real beta_neur;
  real<lower=0> sigma;
}

transformed parameters {
  real mu[N];
  for (n in 1:N)
    mu[n] = beta_intcpt + beta_sex*sex[n] + beta_age*age[n] + beta_openness*openness[n] + beta_consc*consc[n] + beta_extrav*extrav[n] + beta_agree*agree[n] + beta_neur*neur[n];
}

model {
  for (n in 1:N)
    kddi[n] ~ normal(mu[n], sigma);
}

generated quantities {
  real y_pred[N];
  for (n in 1:N)
    y_pred[n] = normal_rng(mu[n], sigma);
}
