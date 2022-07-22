#ex6.stan#
data {
  int N;
  int<lower=0> F[N];
  int<lower=0> Y[N];
  real<lower=0> X[N];
}

parameters {
  real b[3];
}

transformed parameters {
  real lambda[N];
  for (n in 1:N) {
    lambda[n] = exp(b[1] + b[2]*X[n] + b[3]*F[n]);
  }
}

model {
  for (n in 1:N)
    Y[n] ~ poisson(lambda[n]);
}

generated quantities {
  int y_pred[N];
  for (n in 1:N)
    y_pred[n] = poisson_rng(lambda[n]);
}
