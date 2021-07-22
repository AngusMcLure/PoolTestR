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
#' @param link Link function. There are two options `'logit'` (logistic
#'   regression, the default) and `'cloglog'` (complementary log log
#'   regression).
#' @param prior The priors to be used for the regression parameters. Defaults to
#'   a non-informative (normal(0,100)) prior on linear coefficients and a
#'   zero-truncated student-t prior on the group effect standard deviations.
#'   Custom priors must \code{brmsprior} objects produced by
#'   [brms::set_prior()].
#' @param cores The number of CPU cores to be used. By default one core is used
#' @param ... Additional arguments to be passed to \code{brms::brms}.
#' @return An object of class \code{brms} with the regression outputs.
#'
#' @seealso [PoolTestR::PoolReg()], [PoolTestR::getPrevalence()]
#' @example examples/LogisticRegression.R



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
  if(link == 'logit'){
    PoolLogitResponse <- "
    real pool_bernoulli_logit_lpmf(int y, real alpha, int N) {
      return bernoulli_lpmf(1-y | exp(log1m_inv_logit(alpha) * N));
    }
    int pool_bernoulli_logit_rng(real alpha, int N) {
      return 1-bernoulli_rng(exp(log1m_inv_logit(alpha) * N));
    }
    "
    stanvars <- brms::stanvar(scode = PoolLogitResponse, block = "functions")

    pool_bernoulli_logit <- brms::custom_family(
      "pool_bernoulli_logit", dpars = c("mu"),
      links = c("identity"),
      type = "int", vars = "vint1[n]"
    )
    f <- formula
    f[[2]] <- str2lang(paste0(formula[[2]], " | vint(",PoolSizeName,")" ))
    family <- pool_bernoulli_logit
    bform <- brms::bf(f)

  }else if(link == "cloglog"){ ## FIX THIS SINCE IT CAN DONE MORE ELEGANTLY WITH  OFFSETS OR A CUSTOM FAMILY
    stanvars <- NULL
    family <- brms::bernoulli("identity")

    f1 <- stats::reformulate(paste0("inv_cloglog(log(",
                                        PoolSizeName,
                                        ")+ eta)"),
                                 response = formula[[2]])
    f2 <- formula
    f2[[2]] <- as.name("eta")
    bform <- brms::bf(f1,f2,nl = TRUE)
  } else{
    stop('Invalid link function. Options are logit or cloglog')
  }

  if(is.null(prior)){
    prior <- brms::set_prior("normal(0,100)", class = "b",
                             nlpar = switch(link,cloglog="eta",logit= ""))
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
  #model$stancode <- code
  return(model)
}

pool_bernoulli_logit_lpmf <- function(y, alpha, N){
  out <- y
  out[y==0] <- log1p(exp(alpha[y == 0])) * -N[y == 0]
  out[y==1] <- log1p(-(1+exp(alpha[y == 1]))^-N[y == 1])
  out
}

pool_bernoulli_logit_rng <- function(alpha, N){
  as.integer(stats::runif(length(alpha)) > exp(log1p(-stats::plogis(alpha)) * N))
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
  1 - exp(log1p(-stats::plogis(alpha)) * N)
}


