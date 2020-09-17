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
#'   \code{brms}, which are generalisation of the formulae used in \code{lm,glm}
#'   or \code{lme4}. The left-hand side of the formula should be the name of
#'   column in \code{data} with the result of the test on the pooled samples.
#'   The result must be stored with 1 indicating a positive test result and 0
#'   indicating a negative test result.
#' @param ... Arguments to be passed brms.
#' @return An object of class \code{brms} with the regression outputs.
#'
#' @example examples/LogisticRegression.R!!!



PoolRegBayes <- function (formula, data, poolSize, prior = NULL, cores = 4, ...){
  poolSize <- enquo(poolSize)
  AllVars <- all.vars(formula)

  if(!all(AllVars %in% colnames(data))){
    stop("formula contains variables that aren't in the data: ",
         paste(AllVars[!(AllVars %in% colnames(data))], collapse = ", "))
  }

  if(!(as_label(poolSize) %in% colnames(data))){
    stop("poolSize does not match any of the variable names in the the data provided.")
  }

  if("eta" %in% AllVars){
    stop("formula contains a variable called eta which is needed internally. ",
         "Please rename this variable and try again")
  }
  if(as_label(poolSize) %in% AllVars){
    stop("The size of the pools (",as_label(poolSize),")",
         "is included as a variable in the regression formula. ",
         "Are you sure this is what you meant to do?")
  }

  #Set up formula for the format needed for brms
  f1 <- reformulate(paste0("1 - (1-inv_logit(eta))^",as_label(poolSize)),response = formula[[2]])
  f2 <- formula
  f2[[2]] <- as.name("eta")

  bform <- bf(f1,f2,nl = TRUE)

  if(is.null(prior)){
    prior <- set_prior("normal(0,100)", class = "b", nlpar = "eta")
  }

  model <- brm(bform,
               family = bernoulli("identity"),
               data = data,
               cores = cores,
               prior = prior,
               ...)

  return(model)
}

