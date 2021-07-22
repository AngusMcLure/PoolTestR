data {
  int<lower=1> N; //Number of data points
  int<lower=1> L; // Number of levels or grouping variables
  int<lower=1> NumGroups[L]; //Number of groups for each level of grouping variable
  int<lower=L> TotalGroups; //Total number of groups across all levels
  int<lower=0, upper=1> Result[N];
  vector<lower=0>[N] PoolSize;
  //int<lower = 1> G[N,L]; //The group membership of each datapoint at each level
  matrix<lower = 0, upper = 1>[N,TotalGroups] Z; //Model matrix for group effects
  real<lower=0> PriorAlpha;
  real<lower=0> PriorBeta;
  real<lower=0> HyperpriorSD;
}
transformed data{
  int<lower=0, upper=1> FlippedResult[N];
  //Get a sparse version of the model matrix for group effects (Z)
  vector[L*N] Zw;
  int Zv[L*N];
  int Zu[N+1];
  Zw = csr_extract_w(Z);
  Zv = csr_extract_v(Z);
  Zu = csr_extract_u(Z);

  for(n in 1:N){
    FlippedResult[n] = 1 - Result[n];
  }
}
parameters {
  real<lower=0, upper=1> p;
  vector[TotalGroups] u; //standardised group effects
  vector<lower=0>[L] group_sd;
}
model{
  int k;
  vector[N] ps; //1 - prevalence at the pool level (adjusted for pool size)
  vector[TotalGroups] au; //actual group effects
  k = 1;
  for(l in 1:L){
    au[k:(k+NumGroups[l]-1)] = u[k:(k+NumGroups[l]-1)] * group_sd[l];
    k = k + NumGroups[l];
  }

  //vector[N] los; //log odds at the individual level at that site
  //los = logit(p) + Z * au;
  //ps = exp(-log1p_exp(logit(p) + Z * au) .* PoolSize);
  //ps = exp(log1m_inv_logit(logit(p) + Z * au) .* PoolSize);
  ps = exp(log1m_inv_logit(logit(p) + csr_matrix_times_vector(N,TotalGroups,Zw,Zv,Zu,au)) .* PoolSize);

  u ~ normal(0, 1);
  group_sd ~ cauchy(0,HyperpriorSD);
  p ~ beta(PriorAlpha,PriorBeta);
  FlippedResult ~ bernoulli(ps);
}
