data{
  int<lower = 0> N; //number of sites
  //Number of different sizes of pools
  //i.e. if the sizes of the individual pools are 1,1,1,2,2,3,5,7,20,20,20
  //then M would be 6 and
  //PoolSizes would be 1,2,3,5,7,20
  int<lower =1> M;
  vector<lower = 1>[M] PoolSizes;
  int<lower = 0, upper = 1> Positive[N,M]; //Number of positive results from tests on pools of size PoolSizes[M] from site N
  int<lower = 0> PoolNumber[N,M]; //Number of pools of size PoolSizes[M] from site N
  //beta prior parameters for sensitivity, specificity, mozzie-level prevalence and site-level prevalence
  real<lower = 0, upper = 1> alpha_C;
  real<lower = 0, upper = 1> beta_C;
  real<lower = 0, upper = 1> alpha_S;
  real<lower = 0, upper = 1> beta_S;
  real<lower = 0, upper = 1> alpha_p;
  real<lower = 0, upper = 1> beta_p;
  real<lower = 0, upper = 1> alpha_q;
  real<lower = 0, upper = 1> beta_q;
}
parameters {
  real<lower = 0, upper = 1> p; //prevalence (per individual) assuming present at site
  real<lower = 0, upper = 1> q; //prevalence (per site) i.e. probability that a random site has prevalence > 0
  real<lower = 0, upper = 1> C; //test specificity (at the level of pools)
  real<lower = 0, upper = 1> S; //test sensitivity (at the level of pools)
}
transformed parameters{
  vector[M] ps; // probability that a pool of size n tests positive given individual-level prevalence, sensitivity, and specificity
  ps = exp(log1m(p) * PoolSizes) * (1-C-S) + S;
}
model{
  //priors
  C ~ beta(alpha_C,beta_C);
  S ~ beta(alpha_S,beta_S);
  p ~ beta(alpha_p,beta_p);
  q ~ beta(alpha_q,beta_q);
  //likelihood
  for(n in 1:N){
    target += log_mix(q, binomial_lpmf(Positive[n,]|PoolNumber[n,],ps), binomial_lpmf(Positive[n,]|PoolNumber[n,],1-C));
  }
}
