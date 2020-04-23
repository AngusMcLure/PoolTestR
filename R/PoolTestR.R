#' Estimation of prevalence based on results of tests on pooled samples
#'
#' @export
#' @param TestResult The name of column with the result of each test on each pooled sample
#' @param PoolSize The name of the column with number of bugs in each pool
#' @param ... Optional name(s) of columns with variables to group the data by. If ommitted uses the complete dataset of pooled sample results to calculate a single prevalence. If included prevalence is estimated spearately for each group defined by these columns
#' @param alpha The confidence level to be used for the confidence and credible intervals. Defaults to 0.5\% (i.e. 95\% intervals)
#' @param verbose Logical indicating whether to print progress to screen. Defaults to false (no printing to screen)
#' @return A data.frame with columns:
#' \itemize{
#'  \item{MLE (the Maximum Likleihood Estimate of prevelance)}
#'  \item{LR-CI Lower and LR-CI Upper (Lower and Upper Confidence intervals using the Likelihood Ratio method)}
#'  \item{Bayesian Posterior Expectation}
#'  \item{Bayesian CI Lower and Bayesian CI Upper}
#'  \item{Number of Pools}
#'  \item{Number Positive}
#' }
#' When there are no grouping variables (supplied in ...) then the dataframe has only one row with the prevalence estimates for the whole dataset.
#' When grouping varaibles are supplied, then there is a seperate row for each group.
#'



# Created by Angus McLure, Dec 2019.
# All rights reserved.

#Script for calculating infection prevalences from pooled samples of insects.
#Uses bayesian inference of a modified binomial model with a uniform [0, 1] prior on prevalence
#For comparison it also calculates the Maximum Likelihood Estimate and 95% confidence intervals using the Likelihood Ratio Method



PoolPrev <- function(data,TestResult,PoolSize,...,alpha=0.05,verbose = F){
  TestResult <- enquo(TestResult) #The name of column with the result of each test on each pooled sample
  PoolSize <- enquo(PoolSize) #The name of the column with number of bugs in each pool
  group_var <- enquos(...) #optional name(s) of columns with other variable to group by. If ommitted uses the complete dataset of pooled sample results to calculate a single prevalence

  if(length(group_var) == 0){ #if there are no grouping variables
    options(mc.cores = parallel::detectCores())
    sdata <- list(N = nrow(data),
                  Result = as.array(as.matrix(dplyr::select(data, !! TestResult))[,1]), #This seems a rather obscene way to select a column, but other more sensible methods have inexplicible errors when passed to rstan::sampling
                  PoolSize = as.array(as.matrix(dplyr::select(data, !! PoolSize))[,1])
                  )

    sfit <- sampling(stanmodels$BayesianPoolScreen,
                     data = sdata,
                     pars = c('p'),
                     chains = 4,
                     iter = 2000,
                     warmup = 1000,
                     refresh = ifelse(verbose,200,0))
    LogLikPrev = function(p,Result,PoolSize,goal=0){
      sum(log(Result + (-1)^Result * (1-p)^PoolSize)) - goal
    }

    # This is the log-likelihood difference used to calculate Likelihood ratio confidence intervals
    # Currently not used (as we are trying to reproduce the original PoolScreen's odd behaviour)
    # but we could uncomment below if we want users to supply confidence level (alpha)
    LogLikDiff <- qchisq(1-alpha, df = 1)/2

    out <- summary(sfit,probs = c(alpha/2,1-alpha/2))$summary["p",] %>% t() %>% as.data.frame()

    #Calculate the Maximum likelihood estimate -- this is exactly zero if all the pools are negative or positive
    if(any(as.logical(sdata$Result)) & !all(as.logical(sdata$Result))){ #if there is at least one positive and one negative result
      out$MLE <- optimizing(attr(sfit,"stanmodel"),sdata)$par["p"]
      out$`LR-CI Lower` <- uniroot(LogLikPrev,
                                   c(0,out$MLE),
                                   goal = LogLikPrev(out$MLE,sdata$Result,sdata$PoolSize) - LogLikDiff, # the version we would use if we let users supply confidence
                                   #goal = LogLikPrev(out$MLE,sdata$Result,sdata$PoolSize) - 2.51,
                                   Result= sdata$Result,
                                   PoolSize= sdata$PoolSize,
                                   tol = 1e-10)$root
      out$`LR-CI Upper` <- uniroot(LogLikPrev,
                                   c(out$MLE,1),
                                   goal = LogLikPrev(out$MLE,sdata$Result,sdata$PoolSize) - LogLikDiff, # the version we would use if we let users supply confidence
                                   #goal = LogLikPrev(out$MLE,sdata$Result,sdata$PoolSize) - 2.51, #Poolscreen uses the 2.51 value for a 95% confidence interval, which is the value one would use for 97.5% confidence interval (perhpas they thought they needed to make an adjustment for a 'two-sided' test?) For consistency I have reproduced it here
                                   Result= sdata$Result,
                                   PoolSize= sdata$PoolSize,
                                   tol = 1e-10)$root
    }else if(all(as.logical(sdata$Result))){ #If all tests are positive
      out$MLE <- 1
      out$`LR-CI Lower` <- uniroot(LogLikPrev,
                                   c(0,1),
                                   goal = -LogLikDiff, # the version we would use if we let users supply confidence
                                   #goal = -1.92, # When all results are positive, the original Poolscreen uses this (more expected) value for the logliklihood difference (1.92) for a 95% confidence interval
                                   Result= sdata$Result,
                                   PoolSize= sdata$PoolSize,
                                   tol = 1e-10)$root
      out$`LR-CI Upper` <- 1
    }else{ #if all tests are negative
      out$MLE <- 0
      out$`LR-CI Lower` <- 0
      out$`LR-CI Upper` <- uniroot(LogLikPrev,
                                   c(0,1),
                                   goal = -LogLikDiff, # the version we would use if we let users supply confidence
                                   #goal = -1.92, # When all results are negative, the original Poolscreen uses this (more expected) value for the logliklihood difference (1.92) for a 95% confidence interval
                                   Result= sdata$Result,
                                   PoolSize= sdata$PoolSize,
                                   tol = 1e-10)$root
    }
    out$`Number of Pools` <- sdata$N
    out$`Number Positive` <- sum(sdata$Result)

    out <- out %>%
      rename(`Bayesian Posterior Expectation` = mean) %>%
      rename(`Bayesian CI Lower` = paste0(as.character((alpha/2) * 100),'%')) %>%
      rename(`Bayesian CI Upper` = paste0(as.character((1-alpha/2) * 100),'%')) %>%
      dplyr::select(MLE, `LR-CI Lower`, `LR-CI Upper`,
                    `Bayesian Posterior Expectation`,
                    `Bayesian CI Lower`,`Bayesian CI Upper`,
                    `Number of Pools`, `Number Positive`)

    out
  }else{ #if there are grouping variables the function calls itself iteratively on each group
    out <- data %>%
      group_by(!!! group_var) %>%
      do(PoolPrev(.,!! TestResult,!! PoolSize,alpha=alpha,verbose = verbose)) %>%
      as.data.frame()
  }
  out
}

