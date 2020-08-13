# hierarchical model with proababilty of absence

Partially inspired by Suess EA, Gardner IA, Johnson WO, 2002. Hierarchical Bayesian model for prevalence inferences and determination of a country's status for an animal pathogen. *Prev. Vet. Med.* 55: 155-171.

Assumed study design is as follows. We are interested in understanding prevalence of a marker in a region. So we choose some random sites within that region. At each site we collect a number of samples. In the general case the number of samples collected is a random variable (as we collect as many samples as we can normally). We pool these samples into sets, so that there is typically at least one pool for each site, and then test each pool. The test has imperfect sensitivity and specificity. The pool sizes within each site, and the number of pools from each site may be different.  Again in the general cae where the number of samples collected is random, the number and/or size of pools will be random also.

Let $q$ be the probability that a randomly chosen site is one where the marker is present (prevalence greater than 0%). Let $p$ be the prevalence of the marker in the sites where the marker is present.

Let $N_{ij}$ and $y_{ij}$ respectively be the size and test result (1 if positive, 0 if negative) of pool $i$ from site $j$. 

Then the likelihood is

$P(y_{ij}|p,q,N_{ij}) = \prod_j P(y_{.j}|p,q,N_{.j})$

where

$P(y_{.j}|p,q,N_{.j}) = \cases{(1-q) + q(1-p)^{\sum_i N_{ij}}, \text{if } y_{.j} = 0 //q\prod_i y_{ij} + (-1)^{y_{ij}} (1-p)^{N_{ij}}, \text{otherwise}}$  

Equivalently  $E[Y_{ij}|N_{ij}] \sim B_{j} B_{ij}$ with i.i.d. $B_{j} \sim Bernoulli(q)$ and i.i.d. $B_{ij} \sim Bernoulli(1-(1-p)^{N_i})$ 

In the bayesian framework we place some priors on $p$ and $q$. A reasonably general and convenient choice may be of the form

$p \sim Beta(\alpha_p,\beta_p)$ and

$q \sim Beta(\alpha_q, \beta_q)$.

We may want to extend this to include covariates at the site or higher levels. Let $X_{jk}$ be the observation of variable $k$ at site $j$ . We model $p$ and $q$ at each site

$logit(p) = X \mathbf{\beta}$

$logit(q) = X \mathbf{\gamma}$

In the bayesian framework we would then replace the priors on $p$ and $q$ with some priors on $\mathbf{\beta}$ and $\mathbf{\gamma}$. A classic uninformative choice would be of the form

$\beta_k \sim Normal(0,\sigma_{\beta}^2)$

$\gamma_k \sim Normal(0,\sigma_{\gamma}^2)$

with sufficiently large $\sigma_{\gamma}^2$ and $\sigma_{\beta}^2$.



If the test is imperfect and has sensitivity $S$ and specificity $C$ then likelihood for a single site is

$P(y_{.j}|p,q,N_{.j},S,C) = (1-q) \prod_i (y_{ij} + (-1)^{y_{ij}} C) +  q \prod_i( y_{ij} + (-1)^{y_{ij}} [1 - S + (1-p)^{N_{ij}} (S+C-1)])$  

```{stan}
data{
	int<lower = 0> N; //number of data points
	int<lower = 0> NumSite; //number of sites
	int<lower = 1> PoolNum[NumSite]; // Number of pools from each site
	int<lower = 0, upper = 1> Result[N]; //Result of tests
	int<lower = 0> PoolSize[N]; //Size of pools 
	int<lower = 1, upper = NumSite> Site[N]; // The Site that each pool is taken from 
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
transformed data{
	// sort data to be ordered by site (i.e. first PoolNum[1] entries are for Site 1, the next PoolNum[2] entries are for site 2 etc.)
	int Order[N] = sort_indices_asc(Site);
	//Site = Site[Order];
	Result = Result[Order]; 
	PoolSize = PoolSize[Order];
	//int k = 0;
	//for(s in Site){
	//	if(s != k)
	//}
}
parameters {
	real<lower = 0, upper = 1> p; //prevalence (per sample) assuming present
	real<lower = 0, upper = 1> q; //prevalence (per site) i.e. probability that a random site has prevalence > 0
	real<lower = 0, upper = 1> C; //test specificity
	real<lower = 0, upper = 1> S; //test sensitivity
}
transformed parameters{
  real<lower=0, upper=1> ps[N];
  ps = exp(log1m(p) * PoolSize) // adjustment of * (1-C-S) + S has been moved to the model section
}
model{
	int pos; //for splitting the results up by site
	pos = 1;
	//priors
	C ~ beta(alpha_C,beta_C);
	S ~ beta(alpha_S,beta_S);
	p ~ beta(alpha_p,beta_p);
	q ~ beta(alpha_q,beta_q);
	//likelihood
	for(n in 1:NumSite){
		SR = segment(Result,pos,PoolNum[n]) //The results from one site
		Sps = segment(ps, pos, PoolNum[n]) 
		target += log_mix(q, bernoulli_lpmf(SR|Sps * (1-C-S) + S), bernoulli_lpmf(SR|1-C));
		pos = pos + PoolNum[n];
	}
}

```

The same thing refactored so that each line represents a single site (rather than a pool) and so the data on each site 

```{stan}
data{
	int<lower = 0> N; //number of sites
	int<lower = 1> M; //The Maximum pool size (assumes minimum size is 1)
	int<lower = 0, upper = 1> Positive[N,M]; //Number of positive results from tests on pools of size M from site N 
	int<lower = 0> PoolNumber[N,M]; //Number of pools of size M from site N  
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
  real<lower=0, upper=1> ps[N]; // probability that a pool of size n tests positive given prevalene, sensitivity, and specificity
  ps = exp(log1m(p) * (1:M)) * (1-C-S) + S 
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
```







