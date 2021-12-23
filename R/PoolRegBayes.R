#' Bayesian Mixed or Fixed Effect Logistic Regression with Presence/Absence
#' Tests on Pooled Samples
#'
#' It can be useful to do mixed effects logistic regression on the
#' presence/absence results from pooled samples, however one must adjust for the
#' size of each pool to correctly identify trends and associations.
#'
#' @export
#' @param data A \code{data.frame} with one row for each pooled sampled and
#'   columns for the size of the pool (i.e. the number of specimens / isolates /
#'   insects pooled to make that particular pool), the result of the test of the
#'   pool and any number of columns to be used as the dependent variables in the
#'   logistic regression.
#' @param poolSize The name of the column with number of specimens / isolates /
#'   insects in each pool.
#' @param formula A \code{formula} of the kind used to define models in
#'   \code{brms}, which are generalisation of the formulae used in \code{lm,
#'   glm} or \code{lme4}. The left-hand side of the formula should be the name
#'   of column in \code{data} with the result of the test on the pooled samples.
#'   The result must be stored with 1 indicating a positive test result and 0
#'   indicating a negative test result.
#' @param link Link function. There are three options `logit` (i.e logistic
#'   regression, the default), `cloglog` (complementary log-log), and
#'   `loglogit`. The final option blends a log link function and the logit
#'   function so that parameters are (log) prevalence/rate ratios as long as
#'   predicted prevalence is <0.8 (for details see Clark and Barr, Stat Methods
#'   Med Res (2018) <DOI:10.1177/0962280217698174>)
#' @param prior The priors to be used for the regression parameters. Defaults to
#'   a non-informative (normal(0,100)) prior on linear coefficients and a
#'   zero-truncated student-t prior on the group effect standard deviations.
#'   Custom priors must be \code{brmsprior} objects produced by
#'   \code{\link[brms:set_prior]{brms::set_prior}}
#' @param cores The number of CPU cores to be used. By default one core is used
#' @param ... Additional arguments to be passed to \code{brms::brms}.
#' @return An object of class \code{brms} with the regression outputs.
#'
#' @seealso
#'      \code{\link{PoolReg}},
#'      \code{\link{getPrevalence}}
#' @example examples/LogisticRegression.R
#' @references
#'   Clark RG, Barr M: A blended link approach to relative risk regression.
#'   Statistical Methods in Medical Research 2018, 27(11):3325-3339.
#'   <DOI:10.1177/0962280217698174>
#'
#'   Angus McLure, Ben O'Neill, Helen Mayfield, Colleen Lau, Brady McPherson (2021).
#'   PoolTestR: An R package for estimating prevalence and regression modelling
#'   for molecular xenomonitoring and other applications with pooled samples.
#'   Environmental Modelling & Software, 145:105158. <DOI:10.1016/j.envsoft.2021.105158>
#'
#'




PoolRegBayes <- function (formula, data, poolSize,
                          link = 'logit', prior = NULL, cores = NULL, ...){
  poolSize <- dplyr::enquo(poolSize)
  AllVars <- all.vars(formula)
  PoolSizeName <- dplyr::as_label(poolSize)

  # Ideally I would like to:
  # Set number of cores to use (use all the cores! BUT when checking R
  # packages they limit you to two cores)
  # However, there appear to be some issues where running in parallel is a
  # lot slower sometimes. So I am setting 1 core as default, but keeping this
  # code here so I change later if I iron out parallel issues

  if(is.null(cores)){
    chk <- Sys.getenv("_R_CHECK_LIMIT_CORES_", "")
    if (nzchar(chk) && chk == "TRUE") {
      # use 2 cores in CRAN/Travis/AppVeyor
      cores <- 1L
    } else {
      cores <- 1L
    }
  }
  #if(!is.integer(cores)){stop("Number of cores must be numeric")}

  if(!all(AllVars %in% colnames(data))){
    stop("formula contains variables that aren't in the data: ",
         paste(AllVars[!(AllVars %in% colnames(data))], collapse = ", "))
  }

  if(!(PoolSizeName %in% colnames(data))){
    stop("poolSize does not match any of the variable names in the the data provided.")
  }

  if("eta" %in% AllVars){
    stop("formula contains a variable called eta which is needed internally. ",
         "Please rename this variable and try again")
  }
  if(PoolSizeName %in% AllVars){
    warning("The size of the pools (",PoolSizeName,")",
            "is included as a variable in the regression formula. ",
            "Are you sure this is what you meant to do?")
  }

  #Set up formula for the format needed for brms

  f <- formula
  f[[2]] <- str2lang(paste0(formula[[2]], " | vint(",PoolSizeName,")" ))
  bform <- brms::bf(f)

  family <- switch(link,
                   logit = pool_bernoulli_logit,
                   log = pool_bernoulli_log,
                   loglogit = pool_bernoulli_loglogit,
                   cloglog = pool_bernoulli_cloglog,
                   stop('Invalid link function. Options are logit, log, loglogit, or cloglog'))

  stanvars <- brms::stanvar(scode = switch(link,
                                           logit = stancode_pool_bernoulli_logit,
                                           log = stancode_pool_bernoulli_log,
                                           loglogit = stancode_pool_bernoulli_loglogit,
                                           cloglog = stancode_pool_bernoulli_cloglog,
                                           stop('Invalid link function. Options are logit, log, loglogit, or cloglog')),
                            block = "functions")

  if(is.null(prior)){
    prior <- brms::set_prior("normal(0,100)", class = "b",
                             nlpar = "")
  }

  code <- brms::make_stancode(bform,
                              family = family,
                              data = data,
                              cores = cores,
                              prior = prior,
                              stanvars = stanvars,
                              ...)
  model <- brms::brm(bform,
                     family = family,
                     data = data,
                     cores = cores,
                     prior = prior,
                     stanvars = stanvars,
                     ...)
  model$link <- link
  model$PoolSizeName <- PoolSizeName
  return(model)
}

#Post-processing functions for logit link

pool_bernoulli_logit_lpmf <- function(y, alpha, N){
  if(y){
    log1p(-(exp(alpha) + 1)^-N)
  }else{
    log1p(exp(alpha)) * -N
  }
}

pool_bernoulli_logit_rng <- function(alpha, N){
  as.integer(stats::runif(length(alpha)) > exp(log1p(exp(alpha)) * -N))
}


log_lik_pool_bernoulli_logit <- function(i, prep) {
  alpha <- prep$dpars$mu[, i]
  N <- prep$data$vint1[i]
  y <- prep$data$Y[i]
  pool_bernoulli_logit_lpmf(y, alpha, N)
}

posterior_predict_pool_bernoulli_logit <- function(i, prep, ...) {
  alpha <- prep$dpars$mu[, i]
  N <- prep$data$vint1[i]
  y <- prep$data$Y[i]
  pool_bernoulli_logit_rng(alpha, N)
}

posterior_epred_pool_bernoulli_logit <- function(prep) {
  alpha <- prep$dpars$mu
  N <- prep$data$vint1
  N <- matrix(N, nrow = nrow(alpha), ncol = ncol(alpha), byrow = TRUE)
  -expm1(log1p(-stats::plogis(alpha)) * N)
}

#Post-processing functions for log link

pool_bernoulli_log_lpmf <- function(y, alpha, N){
  if(y){
    log1p(-(-expm1(pmin(-0.000000001,alpha)))^N)
  }else{
    log1p(-exp(pmin(-0.000000001,alpha))) * N
  }
}

pool_bernoulli_log_rng <- function(alpha, N){
  as.integer(stats::runif(length(alpha)) > (-expm1(pmin(-0.000000001,alpha)))^N)
}

log_lik_pool_bernoulli_log <- function(i, prep) {
  alpha <- prep$dpars$mu[, i]
  N <- prep$data$vint1[i]
  y <- prep$data$Y[i]
  pool_bernoulli_log_lpmf(y, alpha, N)
}

posterior_predict_pool_bernoulli_log <- function(i, prep, ...) {
  alpha <- prep$dpars$mu[, i]
  N <- prep$data$vint1[i]
  y <- prep$data$Y[i]
  pool_bernoulli_log_rng(alpha, N)
}

posterior_epred_pool_bernoulli_log <- function(prep) {
  alpha <- prep$dpars$mu
  N <- prep$data$vint1
  N <- matrix(N, nrow = nrow(alpha), ncol = ncol(alpha), byrow = TRUE)
  -expm1(N * log(-expm1(pmin(-0.000000001,alpha))))
}

#Post-processing functions for cloglog link

pool_bernoulli_cloglog_lpmf <- function(y, alpha, N){
  if(y){
    log(-expm1(- N * exp(alpha)))
  }else{
    - N * exp(alpha)
  }
}

pool_bernoulli_cloglog_rng <- function(alpha, N){
  as.integer(stats::runif(length(alpha)) > exp(- N * exp(alpha)))
}

log_lik_pool_bernoulli_cloglog <- function(i, prep) {
  alpha <- prep$dpars$mu[, i]
  N <- prep$data$vint1[i]
  y <- prep$data$Y[i]
  pool_bernoulli_cloglog_lpmf(y, alpha, N)
}

posterior_predict_pool_bernoulli_cloglog <- function(i, prep, ...) {
  alpha <- prep$dpars$mu[, i]
  N <- prep$data$vint1[i]
  y <- prep$data$Y[i]
  pool_bernoulli_cloglog_rng(alpha, N)
}

posterior_epred_pool_bernoulli_cloglog <- function(prep) {
  alpha <- prep$dpars$mu
  N <- prep$data$vint1
  N <- matrix(N, nrow = nrow(alpha), ncol = ncol(alpha), byrow = TRUE)
  - expm1(- N * exp(alpha))
}

##Post-processing functions for loglogit link

pool_bernoulli_loglogit_lpmf <- function(y, alpha, N){
  q <- alpha

  c <- 0.8
  K <- log(c);
  b <- 1/(1-c);
  a <- K - log(1-c) - K * b;

  #Pool negative probability
  l <- alpha < K
  q[l] <- (-expm1(alpha[l]))^N
  q[!l] <- exp(-log1p(exp(a + b * alpha[!l]))*N)

  #log pmf
  if(y){
    log1p(-q)
  }else{
    log(q)
  }
}

pool_bernoulli_loglogit_rng <- function(alpha, N){

  q <- alpha

  c <- 0.8
  K <- log(c);
  b <- 1/(1-c);
  a <- K * (1-b) - log(1-c);

  #Pool negative probability
  l <- alpha < K
  q[l] <- (-expm1(alpha[l]))^N[l]
  q[!l] <- exp(-log1p(exp(a + b * alpha[!l]))*N[!l])

  as.integer(stats::runif(length(alpha)) > q)
}

log_lik_pool_bernoulli_loglogit <- function(i, prep) {
  alpha <- prep$dpars$mu[, i]
  N <- prep$data$vint1[i]
  y <- prep$data$Y[i]
  pool_bernoulli_loglogit_lpmf(y, alpha, N)
}

posterior_predict_pool_bernoulli_loglogit <- function(i, prep, ...) {
  alpha <- prep$dpars$mu[, i]
  N <- prep$data$vint1[i]
  y <- prep$data$Y[i]
  pool_bernoulli_loglogit_rng(alpha, N)
}

posterior_epred_pool_bernoulli_loglogit <- function(prep) {
  alpha <- prep$dpars$mu
  N <- prep$data$vint1
  N <- matrix(N, nrow = nrow(alpha), ncol = ncol(alpha), byrow = TRUE)
  p <- alpha

  c <- 0.8
  K <- log(c);
  b <- 1/(1-c);
  a <- K - log(1-c) - K * b;

  #Pool positive probability
  l <- alpha < K
  p[l] <- -expm1(N[l] * log(-expm1(alpha[l])))
  p[!l] <- -expm1(-log1p(exp(a + b * alpha[!l]))*N[!l])

  p
}

## stancode for custom families

stancode_pool_bernoulli_logit <- "
    real pool_bernoulli_logit_lpmf(int y, real alpha, int N) {
      return bernoulli_lpmf(1-y | exp(log1m_inv_logit(alpha) * N));
    }
    int pool_bernoulli_logit_rng(real alpha, int N) {
      return 1-bernoulli_rng(exp(log1m_inv_logit(alpha) * N));
    }
    "
stancode_pool_bernoulli_log <- "
    real pool_bernoulli_log_lpmf(int y, real alpha, int N) {
      return bernoulli_lpmf(1-y | (-expm1(fmin(-0.000000001,alpha)))^N);
    }
    int pool_bernoulli_log_rng(real alpha, int N) {
      return 1-bernoulli_rng((-expm1(fmin(-0.000000001,alpha)))^N);
    }
    "
stancode_pool_bernoulli_loglogit <- "
    real pool_bernoulli_loglogit_lpmf(int y, real alpha, int N) {
      //real c;
      //real K;
      //real a;
      //real b;

      //c = 0.8
      //K = log(c);
      //b = 1/(1-c);
      //a = K - log(1-c) - K * b;

      //if(alpha < K){
      //  return bernoulli_lpmf(1-y | (-expm1(alpha))^N);
      //}else{
      //  return bernoulli_lpmf(1-y | exp(log1m_inv_logit(a + b * alpha ) * N));
      //}

      if(alpha < log(0.8)){
        return bernoulli_lpmf(1-y | (-expm1(alpha))^N);
      }else{
        return bernoulli_lpmf(1-y | exp(log1m_inv_logit(-log(0.2) - log(0.8) * 4 + 5 * alpha) * N));
      }
    }
    int pool_bernoulli_loglogit_rng(real alpha, int N) {
      if(alpha < log(0.8)){
        return 1-bernoulli_rng((-expm1(alpha))^N);
      }else{
        return 1-bernoulli_rng(exp(log1m_inv_logit(-log(0.2) - log(0.8) * 4 + 5 * alpha) * N));
      }

    }
    "
stancode_pool_bernoulli_cloglog <- "
    real pool_bernoulli_cloglog_lpmf(int y, real alpha, int N) {
      return bernoulli_lpmf(1-y | exp(- N * exp(alpha)));
    }
    int pool_bernoulli_cloglog_rng(real alpha, int N) {
      return 1-bernoulli_rng(exp(- N * exp(alpha)));
    }
    "


#custom families

pool_bernoulli_logit <- brms::custom_family(
  "pool_bernoulli_logit", dpars = c("mu"),
  links = c("identity"),
  type = "int", vars = "vint1[n]"
)

pool_bernoulli_log <- brms::custom_family(
  "pool_bernoulli_log", dpars = c("mu"),
  links = c("identity"),
  type = "int", vars = "vint1[n]"
)

pool_bernoulli_loglogit <- brms::custom_family(
  "pool_bernoulli_loglogit", dpars = c("mu"),
  links = c("identity"),
  type = "int", vars = "vint1[n]"
)

pool_bernoulli_cloglog <- brms::custom_family(
  "pool_bernoulli_cloglog", dpars = c("mu"),
  links = c("identity"),
  type = "int", vars = "vint1[n]"
)
