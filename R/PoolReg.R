#' Frequentist Mixed or Fixed Effect Logistic Regression with Presence/Absence Tests on
#' Pooled Samples
#'
#' It can be useful to do mixed effects logistic regression on the
#' presence/absence results from pooled samples, however one must adjust for the
#' size of each pool to correctly identify trends and associations. This can
#' done using \code{glmer} from the \code{lme4} package and the custom link
#' function \code{\link{PoolLink}}, defined in this package.
#' \code{PoolLogitRegMixed} is provided as a more convenient way to fit the same
#' model. \code{PoolLogitReg} is similar but for the case with only
#' population/fixed effects. \code{PoolLogitRegMixed} should be used whenever
#' there is some heirarchical structure to te sampling frame, or in other cases
#' where a model with group/random effects is appropriate.
#'
#' @export
#' @param data A \code{data.frame} with one row for each pooled sampled and
#'   columns for the size of the pool (i.e. the number of specimens / isolates /
#'   insects pooled to make that particular pool), the result of the test of the
#'   pool and any number of columns to be used as the dependent variables in the
#'   logistic regression
#' @param poolSize The name of the column with number of
#'   specimens/isolates/insects in each pool
#' @param formula A \code{formula} of the kind used to define models in
#'   \code{lme4}, which are generalisation of the formulae used in \code{lm} or
#'   \code{glm} that allow for random/group effects. The left-hand side of the
#'   formula should be the name of column in \code{data} with the result of the
#'   test on the pooled samples. The result must be stored with 1 indicating a
#'   positive test result and 0 indicating a negative test result.
#' @return An object of class \code{glmerMod} (or \code{glm} if there are no
#'   random/group effects)
#'
#' @example examples/LogisticRegression.R!!!



PoolReg <- function (data, formula, poolSize){
  poolSize <- enquo(poolSize)

  AllVars <- all.vars(formula)

  if(!(as_label(poolSize) %in% colnames(data))){
    stop("poolSize does not match any of the variable names in the the data provided.")
  }
  poolSize <- select(data,!! poolSize)[,1]

  if(!all(AllVars %in% colnames(data))){
    stop("formula contains variables that aren't in the data: ",
         paste(AllVars[!(AllVars %in% colnames(data))], collapse = ", "))
  }
  if(as_label(poolSize) %in% AllVars){
    stop("The size of the pools (",as_label(poolSize),")",
         "is included as a variable in the regression formula. ",
         "Are you sure this is what you meant to do?")
  }

  # This method of determining whether a formula has any random/mixed effects
  # is pretty much lifted straight from lme4
  if(!length(findbars(formula[[length(formula)]]))){
    print("Model has no group/random effects. Using a fixed effect (glm)")
    out <- glm(formula,
               family = binomial(PoolLink(poolSize)),
               data = data)
  }else{
    print("Dected group/random effects. Using a mixed effect model (glmer)")
    out <- glmer(formula,
                 family = binomial(PoolLink(poolSize)),
                 data = data)
  }
  return(out)
}

