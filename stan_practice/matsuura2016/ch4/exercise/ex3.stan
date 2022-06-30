#####ex3.stan######
data{
  int N1;
  int N2;
  real group1[N1];
  real group2[N2];
  real Y1[N1];
  real Y2[N2];
}

parameters{
  real mu;
  real<lower=0> sigma;
}

model{
  for (i in 1:N1) {
    Y1[i] ~ normal(mu, sigma);
  }
  for (j in 1:N2) {
    Y2[j] ~ normal (mu, sigma);
  }
}