#ex7.stan#
data {
  int N;
  int<lower =0> A[N];
  int<lower = 0> Y[N];
  real<lower = 0> X[N];
  int F[N];
}

parameters {
  real b1;
  real b2;
  real b3;
}

transformed parameters {
  real q[N];
  for (n in 1:N)
    q[n] = inv_logit(b1 + b2*X[n] + b3*F[n]);
}

model {
  for (n in 1:N)
    Y[n] ~ binomial(A[n], q[n]);
}

generated quantities {
  real y_pred[N];
  for (n in 1:N)
    y_pred[n] = binomial_rng(A[n], q[n]);
}
