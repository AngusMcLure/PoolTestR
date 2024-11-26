data {
  int<lower=1> N;
  int<lower=1> D;
  array[N] vector[D] x;
  real intercept;
  real<lower=0> sigma;
  real<lower=0> lengthscale;
  real<lower=0> nugget;
}
transformed data {
  matrix[N, N] K = cov_exp_quad(x, sigma, lengthscale);
  vector[N] mu = rep_vector(intercept, N);
  for (n in 1:N) {
    K[n, n] = K[n, n] + nugget;
  }
}
parameters {
  vector[N] y;
}
model {
  y ~ multi_normal(mu, K);
}
