#' @keywords internal
"_PACKAGE"

#' The 'PoolTestR' package.
#'
#' @description This is a package for working with presence/absence tests on pooled
#'              or grouped samples. The primary application is for estimating prevalence of
#'              a marker in a population based on the results of tests on pooled specimens.
#'              This sampling method is often employed in surveillance of rare conditions in
#'              humans or animals (e.g. livestock or xenomonitoring). The package was initially
#'              conceived as an R-based alternative to the xenomonitoring software,
#'              PoolScreen. However, it goes further, allowing for estimates of prevalence to
#'              adjusted for hierarchy in sampling frames, and perform flexible (mixed effect)
#'              regression analyses. The package is currently in early stages, however more
#'              features are planned or in the works: e.g. adjustments for imperfect test
#'              specificity/sensitivity, functions for helping with optimal experimental
#'              design, and functions for spatial modelling.
#' @name PoolTestR-package
#' @aliases PoolTestR
#' @useDynLib PoolTestR, .registration = TRUE
#' @import dplyr
#' @import methods
#' @import Rcpp
#' @import rstan
#' @importFrom rlang .data
#'
#' @references
#'
#' Angus McLure, Ben O'Neill, Helen Mayfield, Colleen Lau, Brady McPherson (2021). PoolTestR: An R package for estimating prevalence and regression modelling for molecular xenomonitoring and other applications with pooled samples. Environmental Modelling & Software, 145:105158. <DOI:10.1016/j.envsoft.2021.105158>
#'
#' Stan Development Team (2019). RStan: the R interface to Stan. R package version 2.19.2. https://mc-stan.org
#'
#' Paul-Christian Bürkner (2017). brms: An R Package for Bayesian Multilevel Models Using Stan. Journal of Statistical Software, 80(1), 1-28. <DOI:10.18637/jss.v080.i01>
NULL
