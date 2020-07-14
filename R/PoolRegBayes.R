#' Bayesian Mixed (or fixed) Effect Logistic Regression with Presence/Absence Tests on Pooled Samples
#'
#' It can be useful to do mixed effects logistic regression on the presence/absence results from
#' pooled samples, however one must adjust for the size of each pool to correctly
#' identify trends and associations.
#'
#' @export
#' @param data A \code{data.frame} with one row for each pooled sampled and columns for
#'             the size of the pool (i.e. the number of specimens / isolates / insects
#'             pooled to make that particular pool), the result of the test of the pool
#'             and any number of columns to be used as the dependent variables in the
#'             logistic regression
#' @param PoolSize The name of the column with number of specimens/isolates/insects in each pool
#' @param formula A \code{formula} of the kind used to define models in \code{lme4} or \code{brms}.
#'                The left-hand side should be the name of column with the result of each test on
#'                each pooled sample. The result must be stored with 1 indicating a positive test
#'                result and 0 indicating a negative test result.
#' @return An object of class \code{brms} with the regression outputs
#'
#' @example examples/LogisticRegression.R!!!



PoolRegBayes <- function (data, formula, PoolSize){
  PoolSize <- enquo(PoolSize)
  data <- rename(PoolSize = !! PoolSize)
  f1 <- formula
  f2 <- eta ~ 1 - (1-inv_logit(eta))^PoolSize

  f1[[2]] <- f2[[2]]
  f2[[2]] <- formula[[2]]

  bform <- bf(Result ~ 1 - (1-inv_logit(eta))^PoolSize,
              eta ~ Region + (1|Village/Site),
              nl = TRUE)

  nlprior <- c(prior(normal(0,2), nlpar = "eta"))

  fit <- brm(bform, family = bernoulli("identity"),
             data = data,
             prior = nlprior,
             cores = 4)

  return(fit)
}

