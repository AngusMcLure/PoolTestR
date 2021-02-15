data {
  int<lower=1> N;
  int<lower=0, upper=1> Result[N];
  vector<lower=0>[N] PoolSize;
  real<lower=0> PriorAlpha;
  real<lower=0> PriorBeta;
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
  p ~ beta(PriorAlpha,PriorBeta);
  Result ~ bernoulli(ps);
}

