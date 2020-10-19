#' Link Function for Logistic Regression with Presence/Absence Tests on Pooled Samples
#'
#' A custom link function for the \code{\link{binomial}} family to be used with \code{\link{glm}}
#' @export
#' @param PoolSize The number of specimens/isolates/insects in each pool. When used with glm, the
#'                 length must either be 1 if all the pools are the same size, but the same length
#'                 as the data otherwise
#' @return An object of class \code{link-glm}
#'
#' @example examples/LogisticRegression.R

#Turns out the pooled sampled regression is very similar to the problem of
#monitoring bird nests which is in fact THE EXAMPLE given in glm for adding a
#custom link function!

PoolLink <- function(PoolSize = 1)
{
  linkfun <- function(mu) stats::qlogis(1-(1-mu)^(1/PoolSize))
  linkinv <- function(eta) 1-(1-stats::plogis(eta))^PoolSize
  mu.eta  <- function(eta) PoolSize * (1-stats::plogis(eta))^(PoolSize-1) * stats::binomial()$mu.eta(eta)
  valideta <- function(eta) TRUE
  link <- "PooledTestsLogistic"
  structure(list(linkfun = linkfun, linkinv = linkinv,
                 mu.eta = mu.eta, valideta = valideta, name = link),
            class = "link-glm")
}
