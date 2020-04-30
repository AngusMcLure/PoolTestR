data {
  int<lower=1> N; //Number of different pool sizes complete dataset
  int<lower=1> PoolSizes[N]; //The different pool sizes
  int<lower=1> G; //Number of groups
  int<lower=0> PosPools[N,G]; //Number of positive pools of size PoolSizes[1], PoolSizes[2], ..., PoolSizes[N]
  int<lower=0> NegPools[N,G]; //Number of negaitve pools of size PoolSizes[1], PoolSizes[2], ..., PoolSizes[N]
  int<lower=1> A; //Number of parameters for model matrix
  matrix [G,A] MM; //Model matrix
}
transformed data{
  int<lower=0> TotPools[N,G] ;
  for(n in 1:N){
    for(g in 1:G){
        TotPools[n,g] = NegPools[n,g] + PosPools[n,g];
    }
  }
}
parameters {
  vector[A] alpha;
}
transformed parameters {
  vector<lower=0, upper=1>[G] p = inv_logit(MM * alpha); //prevalence in each group
  matrix<lower=0, upper=1>[N,G] ps; //Probability of success for each poolsize and each group
  for(n in 1:N){
    for(g in 1:G){
      ps[n,g] = 1 - (1 - p[g])^PoolSizes[n] ;
    }
  }
}
model{
  for(n in 1:N){
    PosPools[n,] ~ binomial(TotPools[n,],ps[n,]);
  }
  alpha ~ normal(0,1);
}

