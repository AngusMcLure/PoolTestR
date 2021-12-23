#' Estimation of prevalence based on presence/absence tests on pooled samples
#'
#' @export
#' @param data A \code{data.frame} with one row for each pooled sampled and
#'   columns for the size of the pool (i.e. the number of specimens / isolates /
#'   insects pooled to make that particular pool), the result of the test of the
#'   pool. It may also contain additional columns with additional information
#'   (e.g. location where pool was taken) which can optionally be used for
#'   stratifying the data into smaller groups and calculating prevalence by
#'   group (e.g. calculating prevalence for each location)
#' @param result The name of column with the result of each test on each pooled
#'   sample. The result must be stored with 1 indicating a positive test result
#'   and 0 indicating a negative test result.
#' @param poolSize The name of the column with number of
#'   specimens/isolates/insects in each pool
#' @param ... Optional name(s) of columns with variables to stratify the data
#'   by. If omitted the complete dataset is used to estimate a single
#'   prevalence. If included, prevalence is estimated separately for each group
#'   defined by these columns
#' @param prior.alpha,prior.beta,prior.absent The default prior for the
#'   prevalence is the uninformative Jeffrey's prior, however you can also
#'   specify a custom prior with a beta distribution (with parameters
#'   prior.alpha and prior.beta) modified to have a point mass of zero i.e.
#'   allowing for some prior probability that the true prevalence is exactly
#'   zero (prior.absent). Another popular uninformative choice is
#'   \code{prior.alpha = 1, prior.beta = 1, prior.absent = 0}, i.e. a uniform
#'   prior.
#' @param level Defines the confidence level to be used for the confidence and
#'   credible intervals. Defaults to 0.95 (i.e. 95\% intervals)
#' @param verbose Logical indicating whether to print progress to screen.
#'   Defaults to false (no printing to screen).
#' @param iter,warmup,chains MCMC options for passing onto the sampling
#'   routine. See \link[rstan]{stan} for details.
#' @param cores The number of CPU cores to be used. By default one core is used
#' @param control A named list of parameters to control the sampler's behaviour.
#'   Defaults to default values as defined in \link[rstan]{stan}, except for
#'   \code{adapt_delta} which is set to the more conservative value of 0.9. See
#'   \link[rstan]{stan} for details.
#' @return A \code{data.frame} with columns: \itemize{ \item{\code{PrevMLE} (the
#'   Maximum Likelihood Estimate of prevalence)} \item{\code{CILow} and
#'   \code{CIHigh} (Lower and Upper Confidence intervals using the Likelihood
#'   Ratio method)} \item{\code{Bayesian Posterior Expectation}}
#'   \item{\code{CrILow} and \code{CrIHigh}} \item{\code{Number of Pools}}
#'   \item{\code{Number Positive}} } If grouping variables are provided in
#'   \code{...} there will be an additional column for each grouping variable.
#'   When there are no grouping variables (supplied in \code{...}) then the
#'   dataframe has only one row with the prevalence estimates for the whole
#'   dataset. When grouping variables are supplied, then there is a separate row
#'   for each group.
#'
#' @example examples/Prevalence.R
#'      \code{\link{HierPoolPrev}},
#'      \code{\link{getPrevalence}}


PoolPrev <- function(data,result,poolSize,...,
                     prior.alpha = NULL, prior.beta = NULL, prior.absent = 0,
                     level = 0.95, verbose = FALSE,cores = NULL,
                     iter = 2000, warmup = iter/2,
                     chains = 4, control = list(adapt_delta = 0.9)){
  result <- dplyr::enquo(result) #The name of column with the result of each test on each pooled sample
  poolSize <- dplyr::enquo(poolSize) #The name of the column with number of bugs in each pool
  groupVar <- dplyr::enquos(...) #optional name(s) of columns with other variable to group by. If omitted uses the complete dataset of pooled sample results to calculate a single prevalence

  useJefferysPrior <- is.null(prior.alpha) & is.null(prior.beta)
  if(is.null(prior.alpha) != is.null(prior.beta)){
    stop("prior.alpha and prior.beta must either both be specified or both left blank. The latter uses the default Jeffrey's prior")
  }

  # log-likelihood difference used to calculate Likelihood ratio confidence intervals
  LogLikDiff <- stats::qchisq(level, df = 1)/2
  # log-likelihood function
  LogLikPrev = function(p,result,poolSize,goal=0){
    sum(log(result + (-1)^result * (1-p)^poolSize)) - goal
  }

  if(length(groupVar) == 0){ #if there are no grouping variables

    # Ideally I would like to:
    # Set number of cores to use (use all the cores! BUT when checking R
    # packages they limit you to two cores)
    # However, there appear to be some issues where running in parallel is a
    # lot slower sometimes. So I am setting 1 core as default, but keeping this
    # code here so I change later if I iron out parallel issues

    if(is.null(cores)){
      chk <- Sys.getenv("_R_CHECK_LIMIT_CORES_", "")
      if (nzchar(chk) && chk == "TRUE") {
        # use 2 cores in CRAN/Travis/AppVeyor
        cores <- 1L
      } else {
        cores <- 1L
      }
    }
    #if(!is.integer(cores)){stop("Number of cores must be numeric")}

    sdata <- list(N = nrow(data),
                  #Result = array(data$Result), #PERHAPS TRY REMOVING COLUMN NAMES?
                  Result = dplyr::select(data, !! result)[,1] %>% as.matrix %>% as.numeric %>% array, #This seems a rather obscene way to select a column, but other more sensible methods have inexplicible errors when passed to rstan::sampling
                  PoolSize = dplyr::select(data, !! poolSize)[,1] %>% as.matrix %>% array,
                  PriorAlpha = ifelse(is.null(prior.alpha),0,prior.alpha),
                  PriorBeta = ifelse(is.null(prior.beta),0,prior.beta),
                  JeffreysPrior = useJefferysPrior
    )

    #When prior is beta and all tests are negative Bayesian inference has an analytic solution. Otherwise we do MCMC

    #if any tests are positive or for the Jeffrey's prior case
    if(sum(sdata$Result) | useJefferysPrior){
      sfit <- rstan::sampling(stanmodels$BayesianPoolScreen,
                              data = sdata,
                              pars = c('p'),
                              chains = chains,
                              iter = iter,
                              warmup = warmup,
                              refresh = ifelse(verbose,200,0),
                              cores = cores,
                              control = control)
      sfit <- as.matrix(sfit)[,"p"]
    }

    #if there is at least one positive and one negative result
    if(any(as.logical(sdata$Result)) & !all(as.logical(sdata$Result))){
      out <- data.frame(mean = mean(sfit))
      out[,'CrILow'] <- stats::quantile(sfit,(1-level)/2)
      out[,'CrIHigh'] <- stats::quantile(sfit,(1+level)/2)
      out$ProbAbsent <- ifelse(prior.absent & !useJefferysPrior,0,NA)

      # calculate maximum likelihood estimate
      # 'optimizing' from stan actually maximizes the joint posterior, not the likelihood,
      # but if we use a uniform prior they are equivalent
      MLEdata <- sdata
      MLEdata$PriorAlpha <- 1
      MLEdata$PriorBeta  <- 1
      out$PrevMLE <- rstan::optimizing(stanmodels$BayesianPoolScreen,MLEdata)$par["p"]


      out[,'CILow'] <- stats::uniroot(LogLikPrev,
                                      c(0,out$PrevMLE),
                                      goal = LogLikPrev(out$PrevMLE,sdata$Result,sdata$PoolSize) - LogLikDiff, # the version we would use if we let users supply confidence
                                      #goal = LogLikPrev(out$PrevMLE,sdata$Result,sdata$PoolSize) - 2.51,
                                      result= sdata$Result,
                                      poolSize= sdata$PoolSize,
                                      tol = 1e-10)$root
      out[,'CIHigh'] <- stats::uniroot(LogLikPrev,
                                       c(out$PrevMLE,1),
                                       goal = LogLikPrev(out$PrevMLE,sdata$Result,sdata$PoolSize) - LogLikDiff, # the version we would use if we let users supply confidence
                                       #goal = LogLikPrev(out$PrevMLE,sdata$Result,sdata$PoolSize) - 2.51, #Poolscreen uses the 2.51 value for a 95% confidence interval, which is the value one would use for 97.5% confidence interval (perhpas they thought they needed to make an adjustment for a 'two-sided' test?) For consistency I have reproduced it here
                                       result= sdata$Result,
                                       poolSize= sdata$PoolSize,
                                       tol = 1e-10)$root
    }
    #If all tests are positive
    else if(all(as.logical(sdata$Result))){
      out <- data.frame(mean = mean(sfit))
      out[,'CrILow'] <- stats::quantile(sfit,1-level)
      out[,'CrIHigh'] <- 1
      out$ProbAbsent <- ifelse(prior.absent & !useJefferysPrior ,0,NA)
      out$PrevMLE <- 1
      out[,'CILow'] <- stats::uniroot(LogLikPrev,
                                      c(0,1),
                                      goal = -LogLikDiff, # the version we would use if we let users supply confidence
                                      #goal = -1.92, # When all results are positive, the original Poolscreen uses this (more expected) value for the logliklihood difference (1.92) for a 95% confidence interval
                                      result= sdata$Result,
                                      poolSize= sdata$PoolSize,
                                      tol = 1e-10)$root
      out[,'CIHigh'] <- 1
    }
    #if all tests are negative
    else{
      if(useJefferysPrior){
        out <- data.frame(mean = mean(sfit))
        out[,'CrILow'] <- 0
        out[,'CrIHigh'] <- stats::quantile(sfit,level)
        out[,'ProbAbsent'] <- NA
      }else{
        ProbAbsent <- 1/(1 + (1/prior.absent - 1) * beta(prior.alpha, prior.beta + sum(sdata$PoolSize))/beta(prior.alpha, prior.beta))

        #This is the quantile we need to extract from the posterior of the beta-binomial posterior dist to get the credible interval
        q <- (level - ProbAbsent)/(1 - ProbAbsent)

        out <- data.frame(mean = prior.alpha/(prior.alpha + prior.beta + sum(sdata$PoolSize))*(1-ProbAbsent))
        out[,'CrILow'] <- 0
        out[,'CrIHigh'] <- ifelse(q<0, #i.e. if the probability that the disease is absent exceeds the desired size of the credible interval
                                  0,
                                  stats::qbeta(q,prior.alpha, prior.beta + sum(sdata$PoolSize)))
        out[,'ProbAbsent'] <- ifelse(prior.absent,
                                     ProbAbsent,
                                     NA)
      }
      out$PrevMLE <- 0
      out[,'CILow'] <- 0
      out[,'CIHigh'] <- stats::uniroot(LogLikPrev,
                                       c(0,1),
                                       goal = -LogLikDiff, # the version we would use if we let users supply confidence
                                       #goal = -1.92, # When all results are negative, the original Poolscreen uses this (more expected) value for the logliklihood difference (1.92) for a 95% confidence interval
                                       result= sdata$Result,
                                       poolSize= sdata$PoolSize,
                                       tol = 1e-10)$root
    }

    out[,'NumberOfPools'] <- sdata$N
    out[,'NumberPositive'] <- sum(sdata$Result)

    out <- out %>%
      dplyr::rename('PrevBayes' = mean) %>%
      dplyr::select('PrevMLE',
                    'CILow', 'CIHigh',
                    'PrevBayes',
                    'CrILow','CrIHigh',
                    'ProbAbsent',
                    'NumberOfPools', 'NumberPositive')

    out
  }else{ #if there are stratifying variables the function calls itself iteratively on each stratum
    data <- data %>%
      dplyr::group_by(!!! groupVar)
    nGroups <- dplyr::n_groups(data)
    ProgBar <- progress::progress_bar$new(format = "[:bar] :current/:total (:percent)", total = nGroups)
    ProgBar$tick(-1)
    out <- data %>% dplyr::group_modify(function(x,...){
      ProgBar$tick(1)
      PoolPrev(x,!! result,!! poolSize,
               level=level,verbose = verbose,
               prior.alpha = prior.alpha,
               prior.beta = prior.beta,
               prior.absent = prior.absent,
               cores = cores,
               iter = iter,
               warmup = warmup,
               chains = chains,
               control = control)}) %>%
      as.data.frame()
    ProgBar$tick(1)
  }
  dplyr::tibble(out)
}

