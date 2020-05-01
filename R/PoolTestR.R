#' Estimation of prevalence based on presence/absence tests on pooled samples
#'
#' @export
#' @param data A \code{data.frame} with one row for each pooled sampled and columns for
#'             the size of the pool (i.e. the number of specimens / isolates / insects
#'             pooled to make that particular pool), the result of the test of the pool.
#'             It may also contain additional columns with additional information (e.g.
#'             location where pool was taken) which can optionally be used for splitting
#'             the data into smaller groups and calculting prevalence by group (e.g.
#'             calculating prevalence for each location)
#' @param TestResult The name of column with the result of each test on each pooled sample.
#'                   The result must be stored with 1 indicating a positive test result and
#'                   0 indicating a negative test result.
#' @param PoolSize The name of the column with number of specimens/isolates/insects in each pool
#' @param ... Optional name(s) of columns with variables to group the data by.
#'            If ommitted the complete dataset is used to estimate a single prevalence.
#'            If included prevalence is estimated spearately for each group defined by these columns
#' @param alpha The confidence level to be used for the confidence and credible intervals. Defaults to 0.5\% (i.e. 95\% intervals)
#' @param verbose Logical indicating whether to print progress to screen. Defaults to false (no printing to screen)
#' @return A \code{data.frame} with columns:
#' \itemize{
#'  \item{\code{MLE} (the Maximum Likleihood Estimate of prevelance)}
#'  \item{\code{LR-CI Lower} and \code{LR-CI Upper} (Lower and Upper Confidence intervals using the Likelihood Ratio method)}
#'  \item{\code{Bayesian Posterior Expectation}}
#'  \item{\code{Bayesian CI Lower} and \code{Bayesian CI Upper}}
#'  \item{\code{Number of Pools}}
#'  \item{\code{Number Positive}}
#' }
#' If grouping variables are provided in \code{...} there will be an additional column for each grouping variable.
#' When there are no grouping variables (supplied in \code{...}) then the dataframe has only one row with the prevalence estimates for the whole dataset.
#' When grouping variables are supplied, then there is a seperate row for each group.
#'
#' @examples
#' #Build a fake dataset with 1000 pools taken from 4 different locations and 3 different times
#' Data <- data.frame(Place = sample(c("A","B","C","D"),1000, replace = TRUE),
#'                    Date = sample(as.Date(c("2000/1/1","2001/1/1","2002/1/1")),1000, replace = TRUE),
#'                    NumInPool = sample(1:10,1000, replace = TRUE),
#'                    Result = sample(0:1,1000,replace = TRUE)
#'                    )
#' #Prevalence across the whole (fake) dataset
#' PoolPrev(Data, Result,NumInPool)
#' #Prevalence at each location
#' PoolPrev(Data, Result,NumInPool,Place)
#' #Prevalence for each time period
#' PoolPrev(Data, Result,NumInPool,Date)
#' #Prevalence for each combination of location and time period
#' PoolPrev(Data, Result,NumInPool,Place,Date)



PoolPrev <- function(data,TestResult,PoolSize,...,alpha=0.05,verbose = F){
  TestResult <- enquo(TestResult) #The name of column with the result of each test on each pooled sample
  PoolSize <- enquo(PoolSize) #The name of the column with number of bugs in each pool
  group_var <- enquos(...) #optional name(s) of columns with other variable to group by. If ommitted uses the complete dataset of pooled sample results to calculate a single prevalence

  if(length(group_var) == 0){ #if there are no grouping variables

    #Set number of cores to use (use all the cores! BUT when checking R packages they limit you to two cores)

    # chk <- Sys.getenv("_R_CHECK_LIMIT_CORES_", "")
    #
    # if (nzchar(chk) && chk == "TRUE") {
    #   # use 2 cores in CRAN/Travis/AppVeyor
    #   num_workers <- 2L
    # } else {
    #   # use all cores in devtools::test()
    #   num_workers <- parallel::detectCores()
    # }
    #options(mc.cores = num_workers)

    sdata <- list(N = nrow(data),
                  Result = as.array(as.matrix(dplyr::select(data, !! TestResult))[,1]), #This seems a rather obscene way to select a column, but other more sensible methods have inexplicible errors when passed to rstan::sampling
                  PoolSize = as.array(as.matrix(dplyr::select(data, !! PoolSize))[,1])
                  )

    sfit <- sampling(stanmodels$BayesianPoolScreen,
                     data = sdata,
                     pars = c('p'),
                     chains = 1,
                     iter = 2000,
                     warmup = 1000,
                     refresh = ifelse(verbose,200,0),
                     cores = 1)

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

      # 'optimizing' from stan actaully maximizes the joint posterior, not the likelihood,
      # but since we are using a uniform prior they are equivalent. However, with an alternate prior,
      # they would not be the same, so be careful!
      out$MLE <- optimizing(attr(sfit,"stanmodel"),sdata)$par["p"]
      out[,'LR-CI Lower'] <- uniroot(LogLikPrev,
                                   c(0,out$MLE),
                                   goal = LogLikPrev(out$MLE,sdata$Result,sdata$PoolSize) - LogLikDiff, # the version we would use if we let users supply confidence
                                   #goal = LogLikPrev(out$MLE,sdata$Result,sdata$PoolSize) - 2.51,
                                   Result= sdata$Result,
                                   PoolSize= sdata$PoolSize,
                                   tol = 1e-10)$root
      out[,'LR-CI Upper'] <- uniroot(LogLikPrev,
                                   c(out$MLE,1),
                                   goal = LogLikPrev(out$MLE,sdata$Result,sdata$PoolSize) - LogLikDiff, # the version we would use if we let users supply confidence
                                   #goal = LogLikPrev(out$MLE,sdata$Result,sdata$PoolSize) - 2.51, #Poolscreen uses the 2.51 value for a 95% confidence interval, which is the value one would use for 97.5% confidence interval (perhpas they thought they needed to make an adjustment for a 'two-sided' test?) For consistency I have reproduced it here
                                   Result= sdata$Result,
                                   PoolSize= sdata$PoolSize,
                                   tol = 1e-10)$root
    }else if(all(as.logical(sdata$Result))){ #If all tests are positive
      out$MLE <- 1
      out[,'LR-CI Lower'] <- uniroot(LogLikPrev,
                                   c(0,1),
                                   goal = -LogLikDiff, # the version we would use if we let users supply confidence
                                   #goal = -1.92, # When all results are positive, the original Poolscreen uses this (more expected) value for the logliklihood difference (1.92) for a 95% confidence interval
                                   Result= sdata$Result,
                                   PoolSize= sdata$PoolSize,
                                   tol = 1e-10)$root
      out[,'LR-CI Upper'] <- 1
    }else{ #if all tests are negative
      out$MLE <- 0
      out[,'LR-CI Lower'] <- 0
      out[,'LR-CI Upper'] <- uniroot(LogLikPrev,
                                   c(0,1),
                                   goal = -LogLikDiff, # the version we would use if we let users supply confidence
                                   #goal = -1.92, # When all results are negative, the original Poolscreen uses this (more expected) value for the logliklihood difference (1.92) for a 95% confidence interval
                                   Result= sdata$Result,
                                   PoolSize= sdata$PoolSize,
                                   tol = 1e-10)$root
    }
    out[,'Number of Pools'] <- sdata$N
    out[,'Number Positive'] <- sum(sdata$Result)

    out <- out %>%
      rename('Bayesian Posterior Expectation' = mean) %>%
      rename('Bayesian CI Lower' = paste0(as.character((alpha/2) * 100),'%')) %>%
      rename('Bayesian CI Upper' = paste0(as.character((1-alpha/2) * 100),'%')) %>%
      dplyr::select('MLE', 'LR-CI Lower', 'LR-CI Upper',
                    'Bayesian Posterior Expectation',
                    'Bayesian CI Lower','Bayesian CI Upper',
                    'Number of Pools', 'Number Positive')

    out
  }else{ #if there are grouping variables the function calls itself iteratively on each group
    out <- data %>%
      group_by(!!! group_var) %>%
      do(PoolPrev(.,!! TestResult,!! PoolSize,alpha=alpha,verbose = verbose)) %>%
      as.data.frame()
    cat("\n")
  }
  out
}

