data {
  int<lower=1> N;
  array[N] int<lower=0, upper=1> Result;
  vector<lower=0>[N] PoolSize;
  real<lower=0> PriorAlpha;
  real<lower=0> PriorBeta;
  int<lower=0, upper=1> JeffreysPrior;
}
parameters {
  real<lower=0, upper=1> p;
}
transformed parameters{
  array[N] real<lower=0, upper=1> ps;
  real q;
  q = 1-p;
  for(n in 1:N){
    real PS = PoolSize[n];
    ps[n] = 1-q^PS;
  }
}
model{
  if(JeffreysPrior){
    real s;
    s = 0;
    for(n in 1:N){
        real PS = PoolSize[n];
        s += PS ^ 2.0 * q ^ (PS - 2) / (1 - q ^ PS);
    }
    target += log(s)/2;
  }else{
    p ~ beta(PriorAlpha,PriorBeta);
  }
  Result ~ bernoulli(ps);
}

