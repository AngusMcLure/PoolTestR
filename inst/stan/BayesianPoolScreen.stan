data {
  int<lower=1> N;
  int<lower=0, upper=1> Result[N];
  int<lower=0> PoolSize[N];
}
parameters {
  real<lower=0, upper=1> p;
}
transformed parameters{
  real<lower=0, upper=1> ps[N];
  for(n in 1:N){
    ps[n] = 1-(1-p)^PoolSize[n];
  }
}
model{
  target += beta_lpdf(p |1,1);
  target += bernoulli_lpmf(Result | ps);
}

