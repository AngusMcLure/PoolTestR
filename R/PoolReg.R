#' Frequentist Mixed or Fixed Effect Logistic Regression with Presence/Absence
#' Tests on Pooled Samples
#'
#' It can be useful to do mixed effects logistic regression on the
#' presence/absence results from pooled samples, however one must adjust for the
#' size of each pool to correctly identify trends and associations. This can
#' done by using a custom link function [PoolTestR::PoolLink()], defined in this
#' package, in conjunction with using \code{glm} from the \code{stats} package
#' (fixed effect models) or \code{glmer} from the \code{lme4} package (mixed
#' effect models).
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
#'   test on the pooled samples. The result must be encoded with 1 indicating a
#'   positive test result and 0 indicating a negative test result.
#' @param link Link function. There are two options `'logit'` (logistic
#'   regression, the default) and `'cloglog'` (complementary log log
#'   regression).
#' @param ... Arguments to be passed on to \code{stats::glm} or \code{lme4::glmer}
#' @return An object of class \code{glmerMod} (or \code{glm} if there are no
#'   random/group effects)
#'
#' @example examples/LogisticRegression.R



PoolReg <- function (formula, data, poolSize, link = 'logit',...){
  poolSize <- deparse(substitute(poolSize))

  AllVars <- all.vars(formula)

  if(!(poolSize %in% colnames(data))){
    stop("poolSize does not match any of the variable names in the the data provided.")
  }
  if(!all(AllVars %in% colnames(data))){
    stop("formula contains variables that aren't in the data: ",
         paste(AllVars[!(AllVars %in% colnames(data))], collapse = ", "))
  }
  if(poolSize %in% AllVars){
    stop("The size of the pools (",poolSize,")",
         "is included as a variable in the regression formula. ",
         "Are you sure this is what you meant to do?")
  }

  if(link == "cloglog"){
    formula <- stats::update(formula, formula(paste0("~. + offset(log(",poolSize,"))")))
  }

  # This method of determining whether a formula has any random/mixed effects
  # is pretty much lifted straight from lme4
  if(!length(lme4::findbars(formula[[length(formula)]]))){
    print("Model has no group/random effects. Using a fixed effect model (glm)")
    out <- switch(link,
                  logit = stats::glm(formula,
                                     family = stats::binomial(PoolLink(data[[poolSize]])),
                                     data = data,
                                     ...),
                  cloglog = stats::glm(formula,
                                       family = stats::binomial("cloglog"),
                                       data = data,
                                       ...),
                  stop('Invalid link function. Options are logit or cloglog'))

  }else{
    print("Detected group/random effects. Using a mixed effect model (glmer)")
    #control <- lme4::glmerControl(optCtrl=list(maxfun=1e5),
    #                              check.conv.grad = lme4::.makeCC("warning", tol = 2e-3, relTol = NULL))
    out <- switch(link,
                  logit =  lme4::glmer(formula,
                                       family = stats::binomial(PoolLink(data[[poolSize]])),
                                       data = data,
                                       #control = control,
                                       ...),
                  cloglog = lme4::glmer(formula,
                                        family = stats::binomial("cloglog"),
                                        data = data,
                                        #control = control,
                                        ...),
                  stop('Invalid link function. Options are logit or cloglog'))
  }
  attr(out, 'link') <- link
  attr(out,'PoolSizeName') <- poolSize
  return(out)
}

