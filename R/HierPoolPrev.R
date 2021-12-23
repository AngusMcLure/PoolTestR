#' Estimation of prevalence based on presence/absence tests on pooled samples in
#' a hierarchical sampling frame
#'
#' @export
#' @param data A \code{data.frame} with one row for each pooled sampled and
#'   columns for the size of the pool (i.e. the number of specimens / isolates /
#'   insects pooled to make that particular pool), the result of the test of the
#'   pool. It may also contain additional columns with additional information
#'   (e.g. location where pool was taken) which can optionally be used for
#'   splitting the data into smaller groups and calculating prevalence by group
#'   (e.g. calculating prevalence for each location)
#' @param result The name of column with the result of each test on each pooled
#'   sample. The result must be stored with 1 indicating a positive test result
#'   and 0 indicating a negative test result.
#' @param poolSize The name of the column with number of
#'   specimens/isolates/insects in each pool
#' @param hierarchy The name of column(s) indicating the group membership. In a
#'   nested sampling design with multiple levels of grouping the lower-level
#'   groups must have names/numbers that differentiate them from all other
#'   groups at the same level. E.g. If sampling was performed at 200 sites
#'   across 10 villages (20 site per village), then there should be 200 unique
#'   names for the sites. If, for instance, the sites are instead numbered 1 to
#'   20 within each village, the village identifier (e.g. A, B, C...) should be
#'   combined with the site number to create unique identifiers for each site
#'   (e.g. A-1, A-2... for sites in village A and B-1, B-2... for the sites in
#'   village B etc.)
#' @param ... Optional name(s) of columns with variables to stratify the data by.
#'   If omitted the complete dataset is used to estimate a single prevalence.
#'   If included prevalence is estimated separately for each group defined by
#'   these columns
#' @param prior.alpha,prior.beta,prior.absent The prior on the prevalence in
#'   each group takes the form of beta distribution (with parameters alpha and
#'   beta). The default is \code{prior.alpha = prior.beta = 1/2}. Another popular
#'   uninformative choice is \code{prior.alpha = prior.beta = 1}, i.e. a uniform
#'   prior. \code{prior.absent} is included for consistency with \code{PoolPrev},
#'   but is currently ignored
#' @param hyper.prior.sd Scale for the half-Cauchy hyper-prior for standard deviations
#'  of random/group effect terms. Defaults to 2, which is weakly informative since
#'  it implies that 50% of random/group effects terms will be within a order of
#'  magnitude of each other, and 90% of random/group effects will be within four
#'  orders of magnitude of each other. Decrease if you think group differences are
#'  are smaller than this, and increase if you think group differences may often
#'  reasonably be larger than this
#' @param level The confidence level to be used for the confidence and credible
#'   intervals. Defaults to 0.95 (i.e. 95\% intervals)
#' @param verbose Logical indicating whether to print progress to screen.
#'   Defaults to false (no printing to screen)
#' @param cores The number of CPU cores to be used. By default one core is used
#' @param iter,warmup,chains MCMC options for passing onto the sampling
#'   routine. See \link[rstan]{stan} for details.
#' @param control A named list of parameters to control the sampler's behaviour.
#'   Defaults to default values as defined in \link[rstan]{stan}, except for
#'   \code{adapt_delta} which is set to the more conservative value of 0.9. See
#'   \link[rstan]{stan} for details.
#' @return A \code{data.frame} with columns: \itemize{ \item{\code{PrevMLE} (the
#'   Maximum Likelihood Estimate of prevalence)} \item{\code{CILow} and
#'   \code{CIHigh} (Lower and Upper Confidence intervals using the Likelihood
#'   Ratio method)} \item{\code{Bayesian Posterior Expectation}}
#'   \item{\code{CrILow} and \code{CrIHigh}} \item{\code{Number of Pools}}
#'   \item{\code{Number Positive}} } If stratifying variables are provided in
#'   \code{...} there will be an additional column for each stratifying variable.
#'   When there are no stratifying variables (supplied in \code{...}) then the
#'   output has only one row with the prevalence estimates for the whole
#'   dataset. When stratifying variables are supplied, then there is a separate row
#'   for each group.
#'
#' @example examples/HierPrevalence.R
#' @seealso
#'      \code{\link{PoolPrev}},
#'      \code{\link{getPrevalence}}


HierPoolPrev <- function(data,result,poolSize,hierarchy,...,
                         prior.alpha = 0.5, prior.beta = 0.5,
                         prior.absent = 0, hyper.prior.sd = 2,
                         level = 0.95, verbose = FALSE,cores = NULL,
                         iter = 2000, warmup = iter/2,
                         chains = 4, control = list(adapt_delta = 0.9)){
  result <- dplyr::enquo(result) #The name of column with the result of each test on each pooled sample
  poolSize <- dplyr::enquo(poolSize) #The name of the column with number of bugs in each pool
  groupVar <- dplyr::enquos(...) #optional name(s) of columns with other variable to group by. If omitted uses the complete dataset of pooled sample results to calculate a single prevalence

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

  if(length(groupVar) == 0){ #if there are no grouping variables

    #Make the model matrix for the group effects - there might be a simpler way of doing this...
    G <- data[,hierarchy,drop = FALSE] %>%
      dplyr::mutate_all(as.factor) %>%
      droplevels %>%
      dplyr::mutate_all(as.integer)
    NumGroups <- G %>% sapply(max)
    G <- t(t(G) + cumsum(NumGroups) - NumGroups)
    Z <- matrix(0,nrow = nrow(data), ncol = sum(NumGroups))
    for (n in 1:nrow(data)){
      Z[n,G[n,]] = 1
    }
    sdata <- list(N = nrow(data), #number of datapoints (pools)
                  L = length(hierarchy), #number of levels of hierarchy
                  NumGroups = array(NumGroups), #Number of groups at each level of hierarchy
                  TotalGroups = sum(NumGroups),
                  #Result = array(data$Result), #PERHAPS TRY REMOVING COLUMN NAMES?
                  Result = dplyr::select(data, !! result)[,1] %>% as.matrix %>% as.numeric %>% array, #This seems a rather obscene way to select a column, but other more sensible methods have inexplicible errors when passed to rstan::sampling
                  PoolSize = dplyr::select(data, !! poolSize)[,1] %>% as.matrix %>% array,
                  #G = G, #The group membership for each data point and level of hierarchy
                  Z = Z,
                  PriorAlpha = prior.alpha,
                  PriorBeta = prior.beta,
                  HyperpriorSD = hyper.prior.sd
                  )
    #return(sdata)
    sfit <- rstan::sampling(stanmodels$HierBayesianPoolScreen,
                            data = sdata,
                            pars = c('p'),
                            chains = chains,
                            iter = iter,
                            warmup = warmup,
                            refresh = ifelse(verbose,200,0),
                            cores = cores,
                            control = control)
    #return(sfit)
    sfit <- as.matrix(sfit)[,"p"]

    out <- data.frame(mean = mean(sfit))
    out[,'CrILow'] <- stats::quantile(sfit,(1-level)/2)
    out[,'CrIHigh'] <- stats::quantile(sfit,(1+level)/2)

    out[,'NumberOfPools'] <- sdata$N
    out[,'NumberPositive'] <- sum(sdata$Result)

    out <- out %>%
      dplyr::rename('PrevBayes' = mean) %>%
      dplyr::select('PrevBayes',
                    'CrILow','CrIHigh',
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
      HierPoolPrev(x,!! result,!! poolSize,
                   hierarchy,
                   prior.alpha = prior.alpha,
                   prior.beta = prior.beta,
                   prior.absent = prior.absent,
                   level = level,
                   verbose = verbose,
                   cores = cores,
                   iter = iter, warmup = warmup,
                   chains = chains, control = control)}) %>%
      as.data.frame()
    ProgBar$tick(1)
  }
  dplyr::tibble(out)
}

