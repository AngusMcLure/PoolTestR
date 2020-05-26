#' The 'PoolTestR' package.
#'
#' @description Tools for working with pooled samples.
#'     Currently in early stages.
#'     Inspired by PoolScreen.
#'     Uses rstan to do bayesian prevalence estimates
#'     Currently only has basic functionally reproducing PoolScreen functionality for R, however more features are planned or in the works:
#'     e.g. adjustments for imperfect test specificity/sensitivity;
#'     functions for helping with optimal experimental design;
#'     functions for inferring whether a disease has been locally eliminated from a series of pooled tests over time.
#'
#' @docType package
#' @name PoolTestR-package
#' @aliases PoolTestR
#' @useDynLib PoolTestR, .registration = TRUE
#' @import dplyr
#' @import methods
#' @import Rcpp
#' @import rstan
#' @importFrom stats qchisq uniroot quantile model.matrix quantile na.omit terms reformulate
#' @importFrom parallel detectCores
#' @importFrom tidyr unite
#' @importFrom vcd structable
#'
#' @references
#' Stan Development Team (2019). RStan: the R interface to Stan. R package version 2.19.2. https://mc-stan.org
#'
NULL
