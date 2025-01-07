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
  //vector[N] LogPoolSize2;
  vector[N] PoolSizeLess2;
  vector[N] PoolSizeSq;


  for(n in 1:N){
    // We can avoid '1 - ' operation later if we predict negative pools rather than positive ones
    FlippedResult[n] = 1 - Result[n];
  }
  
  // LogPoolSize2 = log(PoolSize) * 2;
  PoolSizeLess2 = PoolSize - 2;
  PoolSizeSq = PoolSizeSq ^ 2.0;

  
}
parameters {
  real<lower=0, upper=1> p;
}
transformed parameters{
  vector<lower=0, upper=1>[N] qpool;
  //qpool = q ^ PoolSize; \\below is equivalent to this line
  qpool = exp(log1m(p) .* PoolSize); // probability of a negative pool
}
model{
  if(JeffreysPrior){
    // real logq; # log of 1-p
    vector[N] s;
    s = PoolSizeSq .* (1 - p) ^ PoolSizeLess2 ./ (1 - (1 - p) ^ PoolSize);
    target += log(sum(s))/2;
    // Below should be equivalent to the above commented lines and more stable,
    // but I have gotten undiagnosable errors so have reverted to the above
    // (which seems stable enough anyway) 
    // logq = log1m(p);
    // s = LogPoolSize2 + logq .* PoolSizeLess2 - log1m_exp(logq .* PoolSize); 
    // target += log_sum_exp(s)/2;
  }else{
    p ~ beta(PriorAlpha,PriorBeta);
  }
  FlippedResult ~ bernoulli(qpool);
}

