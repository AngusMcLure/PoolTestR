data {
  int<lower=1> N;
  int<lower=0, upper=1> Result[N];
  vector<lower=0>[N] PoolSize;
}
parameters {
  real<lower=0, upper=1> p;
}
transformed parameters{
  real<lower=0, upper=1> ps[N];
  real q;
  real s;
  q = 1-p;
  s = 0;
  for(n in 1:N){
    real PS = PoolSize[n];
    ps[n] = 1 - q ^ PS;
    s += PS ^ 2.0 * q ^ (PS - 2) / (1 - q ^ PS);
  }
}
model{
  target += log(s);
  Result ~ bernoulli(ps);
}

