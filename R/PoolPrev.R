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
#' @param bayesian Logical indicating whether Bayesian calculations should be
#'   calculated. If TRUE (the default) calculates frequentist and Bayesian
#'   estimates of prevalence, otherwise only calculates frequentist estimates
#'   (MLE and likelihood ratio confidence intervals).
#' @param prior Prior for prevalence, ignored if \code{bayesian == FALSE}. If
#'   NULL (the default) the prior for the prevalence is the uninformative
#'   Jeffrey's prior. The only alternative prior is a possibly zero-inflated
#'   beta distribution. Zero inflation allows for some prior (and posterior)
#'   probability that the marker of interest is totally absent from the
#'   population. The parameters for this are specified with a list with three
#'   numeric non-negative entries named alpha, beta, and absent. For instance, a
#'   uniform prior with no probability of true absence can be specified as
#'   \code{prior = list(alpha = 1, beta = 1, absent = 0}.
#' @param robust Logical. If \code{TRUE} (default), the point estimate of 
#'   prevalence is the posterior median. If \code{FALSE}, the posterior mean is
#'   used instead. Applies to Bayesian estimates only and therefore ignored if 
#'   \code{bayesian = FALSE}.
#' @param level Defines the confidence level to be used for the confidence and
#'   credible intervals. Defaults to 0.95 (i.e. 95\% intervals)
#' @param reproduce.poolscreen (defaults to FALSE). If TRUE this changes the way
#'   that likelihood ratio confidence intervals are computed to be somewhat
#'   wider and more closely match those returned by Poolscreen. We recommend
#'   using the default (FALSE). However setting to TRUE can help to make
#'   comparisons between PoolPrev and Poolscreen.
#' @param all.negative.pools The kind of point estimate and interval to use when
#'   all pools are negative (Bayesian estimates only). If \code{'zero'} 
#'   (default), uses 0 as the point estimate and lower bound for the interval 
#'   and \code{level} posterior quantile the upper bound of the interval. If 
#'   \code{'consistent'}, result is the same as for the case where at least one 
#'   pool is positive. Applies to Bayesian estimates only and therefore ignored 
#'   if \code{bayesian == FALSE}.
#' @param verbose Logical indicating whether to print progress to screen.
#'   Defaults to false (no printing to screen). Ignored if \code{bayesian ==
#'   FALSE}.
#' @param iter,warmup,chains MCMC options for passing onto the sampling routine.
#'   See \link[rstan]{stan} for details. Ignored if \code{bayesian == FALSE}.
#' @param cores The number of CPU cores to be used. By default one core is used.
#'   Ignored if \code{bayesian == FALSE}.
#' @param control A named list of parameters to control the sampler's behaviour.
#'   Defaults to default values as defined in \link[rstan]{stan}, except for
#'   \code{adapt_delta} which is set to the more conservative value of 0.98. See
#'   \link[rstan]{stan} for details. Ignored if \code{bayesian == FALSE}.
#' @return An object of class \code{PoolPrevOutput}, which inherits from 
#' class \code{tbl}. 
#' The output includes the following columns:
#'   \itemize{
#'     \item{\code{PrevMLE} -- (the Maximum Likelihood Estimate of prevalence)}
#'     \item{\code{CILow} and \code{CIHigh} - lower and upper confidence
#'           intervals using the likelihood
#'           ratio method}
#'      \item{\code{PrevBayes} -- the (Bayesian) posterior expectation. Omitted
#'            if \code{bayesian == FALSE}.}
#'      \item{\code{CrILow} and \code{CrIHigh} -- lower and upper bounds for
#'            credible intervals. Omitted if \code{bayesian == FALSE}.}
#'      \item{\code{ProbAbsent} -- the posterior probability that prevalence is
#'            exactly 0 (i.e. disease marker is absent). NA if using default
#'            Jeffrey's prior or if \code{prior$absent == 0}. Omitted if
#'            \code{bayesian == FALSE}.}
#'      \item{\code{NumberOfPools} -- number of pools}
#'      \item{\code{NumberPositive} -- the number of positive pools} }
#'
#'   If grouping variables are provided in \code{...} there will be an
#'   additional column for each grouping variable. When there are no grouping
#'   variables (supplied in \code{...}) then the output has only one row with
#'   the prevalence estimates for the whole dataset. When grouping variables are
#'   supplied, then there is a separate row for each group.
#'
#'   The custom print method summarises the output data frame by representing
#'   the prevalence and credible intervals as a single column in the form
#'   \code{"Prev (CLow - CHigh)"} where \code{Prev} is the prevalence,
#'   \code{CLow} is the lower confidence/credible interval and \code{CHigh} is
#'   the upper confidence/credible interval. In the print method, prevalence is
#'   represented as a percentage (i.e., per 100 units)
#'
#' @seealso \code{\link{HierPoolPrev}}, \code{\link{getPrevalence}}
#'
#' @example examples/Prevalence.R

PoolPrev <- function(data,result,poolSize,...,
                     bayesian = TRUE, prior = NULL,
                     robust = TRUE,
                     level = 0.95,
                     all.negative.pools = 'zero',
                     reproduce.poolscreen = FALSE,
                     verbose = FALSE, cores = NULL,
                     iter = 2000, warmup = iter/2,
                     chains = 4, control = list(adapt_delta = 0.98)){
  result <- enquo(result) #The name of column with the result of each test on each pooled sample
  poolSize <- enquo(poolSize) #The name of the column with number of bugs in each pool
  groupVar <- enquos(...) #optional name(s) of columns with other variable to group by. If omitted uses the complete dataset of pooled sample results to calculate a single prevalence
  
  useJefferysPrior <- is.null(prior)
  if(bayesian){
    if(!useJefferysPrior && (is.null(prior$alpha) || is.null(prior$beta) || is.null(prior$beta))){
      stop("If not using the default prior (NULL), prior$alpha, prior$beta, and prior$absent must all be specified.")
    }
    if(!useJefferysPrior && (length(prior$alpha) != 1 ||
                             length(prior$beta)  != 1 ||
                             !is.numeric(prior$alpha) ||
                             !is.numeric(prior$beta) ||
                             prior$alpha <= 0 ||
                             prior$beta  <= 0)){
      stop('If not using the default prior (NULL), prior$alpha and prior$beta must each be a single non-negative number.')
    }
    if(!useJefferysPrior && (length(prior$absent) != 1 ||
                             !is.numeric(prior$absent) ||
                             prior$absent < 0 ||
                             prior$absent >= 1)){
      stop('If not using the default prior (NULL), prior$absent must be a single number between 0 (no prior probability of absence) and 1.')
    }
  }
  
  #kind of function to use to get point estimate from posterior draws
  f_point <- if(robust){stats::median}else{mean}
  
  
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
    rplnull <- function(x,replacement){if(is.null(x)){replacement}else{x}}
    sdata <- list(N = nrow(data),
                  #Result = array(data$Result), #PERHAPS TRY REMOVING COLUMN NAMES?
                  Result = select(data, !! result)[,1] %>% as.matrix %>% as.numeric %>% array, #This seems a rather obscene way to select a column, but other more sensible methods have inexplicible errors when passed to rstan::sampling
                  PoolSize = select(data, !! poolSize)[,1] %>% as.matrix %>% array,
                  PriorAlpha = rplnull(prior$alpha,0),
                  PriorBeta = rplnull(prior$beta,0),
                  JeffreysPrior = useJefferysPrior
    )
    
    #When prior is beta and all tests are negative Bayesian inference has an analytic solution. Otherwise we do MCMC
    
    #if any tests are positive or for the Jeffrey's prior case
    if(bayesian & (sum(sdata$Result) | useJefferysPrior)){
      sfit <- rstan::sampling(stanmodels$PoolPrev,
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
    
    #initialise output object
    out <- tibble::tibble(NumberOfPools = sdata$N,
                          NumberPositive = sum(sdata$Result))
    
    #if there is at least one positive and one negative result
    if(any(as.logical(sdata$Result)) & !all(as.logical(sdata$Result))){
      
      # calculate maximum likelihood estimate
      # 'optimizing' from stan actually maximizes the joint posterior, not the likelihood,
      # but if we use a uniform prior on prevalence they are equivalent in this case
      MLEdata <- sdata
      MLEdata$PriorAlpha <- 1
      MLEdata$PriorBeta  <- 1
      MLEdata$JeffreysPrior  <- FALSE
      out$PrevMLE <- rstan::optimizing(stanmodels$PoolPrev,MLEdata)$par["p"]
      
      # log-likelihood difference used to calculate Likelihood ratio confidence intervals
      LogLikDiff <- stats::qchisq(if(reproduce.poolscreen){1 - (1 - level)/2}else{level}, df = 1)/2
      out$CILow <- stats::uniroot(LogLikPrev,
                                  c(0,out$PrevMLE),
                                  goal = LogLikPrev(out$PrevMLE,sdata$Result,sdata$PoolSize) - LogLikDiff,
                                  result= sdata$Result,
                                  poolSize= sdata$PoolSize,
                                  tol = 1e-10)$root
      out$CIHigh <- stats::uniroot(LogLikPrev,
                                   c(out$PrevMLE,1),
                                   goal = LogLikPrev(out$PrevMLE,sdata$Result,sdata$PoolSize) - LogLikDiff,
                                   result= sdata$Result,
                                   poolSize= sdata$PoolSize,
                                   tol = 1e-10)$root
      
      if(bayesian){
        out$PrevBayes <- f_point(sfit)
        out$CrILow <- stats::quantile(sfit,(1-level)/2)
        out$CrIHigh <- stats::quantile(sfit,(1+level)/2)
        out$ProbAbsent <- ifelse(!useJefferysPrior && prior$absent,0,NA)
      }
    }
    #If all tests are positive
    else if(all(as.logical(sdata$Result))){
      out$PrevMLE <- 1
      LogLikDiff <- stats::qchisq(level, df = 1)/2
      out$CILow <- stats::uniroot(LogLikPrev,
                                  c(0,1),
                                  goal = -LogLikDiff,
                                  result= sdata$Result,
                                  poolSize= sdata$PoolSize,
                                  tol = 1e-10)$root
      out$CIHigh <- 1
      
      if(bayesian){
        out$PrevBayes = f_point(sfit)
        out$CrILow <- stats::quantile(sfit,1-level)
        out$CrIHigh <- 1
        out$ProbAbsent <- ifelse(!useJefferysPrior && prior$absent,0,NA)
      }
      
    }
    #if all tests are negative
    else{
      out$PrevMLE <- 0
      out$CILow <- 0
      LogLikDiff <- stats::qchisq(level, df = 1)/2
      out$CIHigh <- stats::uniroot(LogLikPrev,
                                   c(0,1),
                                   goal = -LogLikDiff,
                                   result= sdata$Result,
                                   poolSize= sdata$PoolSize,
                                   tol = 1e-10)$root
      if(bayesian){
        if(useJefferysPrior){
          out$PrevBayes <- switch(all.negative.pools,
                                 'consistent' = f_point(sfit),
                                 'zero' = 0)
          out$CrILow <- 0
          out$CrIHigh <- stats::quantile(sfit,level)
          out$ProbAbsent <- NA
        }else{
          ProbAbsent <- 1/(1 + (1/prior$absent - 1) * beta(prior$alpha, prior$beta + sum(sdata$PoolSize))/beta(prior$alpha, prior$beta))
          #This is the quantile we need to extract from the posterior of the beta-binomial posterior dist to get the credible interval
          q <- (level - ProbAbsent)/(1 - ProbAbsent)
          out$PrevBayes <- switch(all.negative.pools,
                                 'consistent' = prior$alpha/(prior$alpha + prior$beta + sum(sdata$PoolSize))*(1-ProbAbsent),
                                 'zero' = 0)
          out$CrILow <- 0
          out$CrIHigh <- ifelse(q<0, #i.e. if the probability that the disease is absent exceeds the desired size of the credible interval
                                0,
                                stats::qbeta(q,prior$alpha, prior$beta + sum(sdata$PoolSize)))
          out$ProbAbsent <- ifelse(prior$absent,
                                   ProbAbsent,
                                   NA)
        }
      }
    }
    
    if(bayesian){
      out <- out %>%
        select('PrevMLE',
               'CILow', 'CIHigh',
               'PrevBayes',
               'CrILow','CrIHigh',
               'ProbAbsent',
               'NumberOfPools', 'NumberPositive')
    }else{
      out <- out %>%
        select('PrevMLE',
               'CILow', 'CIHigh',
               'NumberOfPools', 'NumberPositive')
    }
    
    
    
    out
  }else{ #if there are stratifying variables the function calls itself iteratively on each stratum
    data <- data %>%
      group_by(!!! groupVar)
    nGroups <- n_groups(data)
    ProgBar <- progress::progress_bar$new(format = "[:bar] :current/:total (:percent)", total = nGroups)
    ProgBar$tick(-1)
    out <- data %>% group_modify(function(x,...){
      ProgBar$tick(1)
      PoolPrev(x,!! result,!! poolSize,
               bayesian = bayesian,
               prior = prior,
               robust = robust,
               level = level,
               all.negative.pools = all.negative.pools,
               reproduce.poolscreen = reproduce.poolscreen,
               verbose = verbose,
               cores = cores,
               iter = iter,
               warmup = warmup,
               chains = chains,
               control = control)})
    ProgBar$tick(1)
  }
  ungroup(out) 
  
  out <- structure(
    out, 
    class = c("PoolPrevOutput", class(out))
  )
  out
}


#' Print method for HierPoolPrevOutput objects
#' S3 method
#' @param object An object of class "PoolPrevOutput" as returned by \code{PoolPrev()}.
#' @return A \code{data.frame} output by \code{PoolPrev}, in a human readable format
#' @seealso \code{\link{PoolPrev}}
#' @method print PoolPrevOutput
#' @export
#' @noRd
print.PoolPrevOutput <- function(x, ...) {
  # Reformat PoolPrevOutput into a human-readable data.frame
  formatted_output <- as.data.frame(
    ungroup(x) %>% 
      mutate(PrevMLE = paste0(" ",
                              format((.data$PrevMLE*100), digits = 2, nsmall = 2),
                              " (", 
                              format((.data$CILow*100), digits = 2, nsmall = 2),
                              " - ", 
                              format((.data$CIHigh*100), digits = 2, nsmall = 2),
                              ")"),
             .keep = "unused")  %>%
      rename("PrevMLE % " = "PrevMLE")
  )
  bayes_check <- "PrevBayes" %in% names(x)
  if (bayes_check == TRUE) {
    formatted_output <- as.data.frame(
      formatted_output %>% 
        mutate(PrevBayes = paste0(" ",
                                  format((.data$PrevBayes*100), digits = 2, nsmall = 2),
                                  " (", 
                                  format((.data$CrILow*100), digits = 2, nsmall = 2),
                                  " - ", 
                                  format((.data$CrIHigh*100), digits = 2, nsmall = 2),
                                  ")"),
               .keep = "unused")  %>%
        rename("PrevBayes % " = "PrevBayes")
    )
  }
  print(formatted_output)
  return(invisible(x))
}
