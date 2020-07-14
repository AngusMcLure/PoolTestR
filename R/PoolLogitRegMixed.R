#' Frequentist Mixed Effect Logistic Regression with Presence/Absence Tests on Pooled Samples
#'
#' It can be useful to do mixed effects logistic regression on the presence/absence results from
#' pooled samples, however one must adjust for the size of each pool to correctly
#' identify trends and associations. This can done using \code{glmer} from the \code{lme4}
#' package and the custom link function \code{\link{PoolLink}}, defined in this package.
#' \code{PoolLogitRegMixed} is provided as a more convenient way to fit the same model.
#' \code{PoolLogitReg} is similar but for the case with only population/fixed effects.
#' \code{PoolLogitRegMixed} should be used whenever there is some heirarchical structure
#' to te sampling frame, or in other cases where a model with group/random effects is appropriate.
#'
#' @export
#' @param data A \code{data.frame} with one row for each pooled sampled and columns for
#'             the size of the pool (i.e. the number of specimens / isolates / insects
#'             pooled to make that particular pool), the result of the test of the pool
#'             and any number of columns to be used as the dependent variables in the
#'             logistic regression
#' @param PoolSize The name of the column with number of specimens/isolates/insects in each pool
#' @param formula A \code{formula} of the kind used to define models in \code{lme4}. The left-hand
#'                side should be the name of column with the result of each test on each pooled
#'                sample. The result must be stored with 1 indicating a positive test result and
#'                0 indicating a negative test result.
#' @return An object of class \code{glm} with the regression outputs
#'
#' @example examples/LogisticRegression.R!!!



PoolLogitRegMixed <- function (data, formula, PoolSize){
  PoolSize <- enquo(PoolSize)
  PoolSize <- select(data,!! PoolSize)[,1]

  # This method of determining whether a formula has any random/mixed effects
  # is pretty much lifted straight from lme4
  if(!length(findbars(formula[[length(formula)]]))){
    warning("Model has no group/random effects. Using a generalised linear model instead")
    out <- glm(formula,
               family = binomial(PoolLink(PoolSize)),
               data = data)
  }else{
    out <- glmer(formula,
                 family = binomial(PoolLink(PoolSize)),
                 data = data)
  }

  return(out)
}

