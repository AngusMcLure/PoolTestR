data {
  int<lower=1> N; //Number of pooled tests
  int<lower=1> A; //Number of parameters
  int<lower=0, upper=1> Result[N];
  int<lower=0> PoolSize[N];
  matrix[N,A] MM; //Model matrix
}
transformed data{
  matrix[N,A] RegMM; //Regularised Model matrix
  vector[A] R;
  row_vector[A] r;
  for(a in 1:A){
    if(max(MM[,a]) != min(MM[,a])){
      R[a] = 1/(max(MM[,a]) - min(MM[,a]));
      r[a] = min(MM[,a]);
    }else{
      R[a] = 1;
      r[a] = 0;
    }
  }
  RegMM = diag_post_multiply(MM - rep_matrix(r,N),R);
  print(RegMM)
}
parameters {
  vector[A] RegAlpha; //regularised regression parameters
}
transformed parameters{
  vector<lower=0, upper=1>[N] p = inv_logit(RegMM * RegAlpha); //prevalence
  vector<lower=0, upper=1>[N] ps; //probability of test success
  for(n in 1:N){
    ps[n] = 1-(1-p[n])^PoolSize[n];
  }
}
model{
  RegAlpha ~ normal(0,1);
  Result ~ bernoulli(ps);
}
generated quantities{
  vector[A] Alpha = R .* RegAlpha;
  Alpha[1] = Alpha[1] - r[2:A]*RegAlpha[2:A];
}

