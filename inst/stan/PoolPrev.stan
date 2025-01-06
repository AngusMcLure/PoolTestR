data {
  int<lower=1> N;
  array[N] int<lower=0, upper=1> Result;
  vector<lower=0>[N] PoolSize;
  real<lower=0> PriorAlpha;
  real<lower=0> PriorBeta;
  int<lower=0, upper=1> JeffreysPrior;
}
transformed data{
  array[N] int<lower=0, upper=1> FlippedResult;
  for(n in 1:N){
    // We can avoid '1 - ' operation later if we predict negative pools rather than positive ones
    FlippedResult[n] = 1 - Result[n];
  }
}
parameters {
  real<lower=0, upper=1> p;
}
transformed parameters{
  vector<lower=0, upper=1>[N] qpool;
  real q;
  q = 1-p; //probability of negative individual
  //qpool = q ^ PoolSize; \\below is equivalent to this line
  qpool = exp(log1m(p) .* PoolSize); // probability of a negative pool
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
  FlippedResult ~ bernoulli(qpool);
}

