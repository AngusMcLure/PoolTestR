data {
  int<lower=1> N; //Number of data points
  int<lower=1> L; // Number of levels or grouping variables
  array[L] int<lower=1> NumGroups; //Number of groups for each level of grouping variable
  int<lower=L> TotalGroups; //Total number of groups across all levels
  array[N] int<lower=0, upper=1> Result;
  vector<lower=0>[N] PoolSize;
  matrix<lower = 0, upper = 1>[N,TotalGroups] Z; //Model matrix for group effects
  // Parameters for t-distributed priors for:
  //     Intercept
  real<lower=0> InterceptNu;
  real<lower=0> InterceptMu;
  real<lower=0> InterceptSigma;
    //     standard deviations of group effects
  real<lower=0> GroupSDNu;
  real<lower=0> GroupSDMu;
  real<lower=0> GroupSDSigma;
}
transformed data{
  array[N] int<lower=0, upper=1> FlippedResult;
  //Get a sparse version of the model matrix for group effects (Z)
  vector[L*N] Zw;
  array[L*N] int Zv;
  array[N+1] int Zu;
  Zw = csr_extract_w(Z);
  Zv = csr_extract_v(Z);
  Zu = csr_extract_u(Z);

  // We can avoid '1 - ' operation later if we predict negative pools rather than positive ones
  for(n in 1:N){
    FlippedResult[n] = 1 - Result[n];
  }
}
parameters {
  real Intercept; //
  vector[TotalGroups] u; //standardised group effects
  real <lower = 0> total_group_sd; // sum-root-square of group effect sd
  simplex[L] var_frac; // fraction of total group variance at each level, this specification allows prior to be on total_group_sd
}
transformed parameters{
  vector<lower=0>[L] group_sd; //sd of group effects
  group_sd = sqrt(var_frac) * total_group_sd;
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

  //This code is equvailent way of calculating ps (though less efficient)

  //vector[N] los; //log odds at the individual level at that site
  //los = Intercept + Z * au;
  //ps = exp(-log1p_exp(los) .* PoolSize);

  //Also equivalent to
  //ps = exp(log1m_inv_logit(logit(p) + Z * au) .* PoolSize);
  ps = exp(log1m_inv_logit(Intercept + csr_matrix_times_vector(N,TotalGroups,Zw,Zv,Zu,au)) .* PoolSize);

  Intercept        ~ student_t(InterceptNu, InterceptMu, InterceptSigma);
  total_group_sd   ~ student_t(GroupSDNu, GroupSDMu, GroupSDSigma);
  u                ~ std_normal();
  FlippedResult    ~ bernoulli(ps);
}
