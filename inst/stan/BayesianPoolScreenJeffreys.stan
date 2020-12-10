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
  real q;
  real s;
  q = 1-p;
  s = 0;
  for(n in 1:N){
    int PS = PoolSize[n];
    ps[n] = 1 - q ^ PS;
    s += PS ^ 2 * q ^ (PS - 2) / (1 - q ^ PS);
  }
}
model{
  target += log(s);
  Result ~ bernoulli(ps);
}

