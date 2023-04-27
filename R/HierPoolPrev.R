#' Estimation of prevalence based on presence/absence tests on pooled samples in
#' a hierarchical sampling frame. Uses an intercept-only random effects model to
#' model prevalence at population level. See PoolReg and PoolRegBayes for full
#' mixed-effect modelling
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
#' @param ... Optional name(s) of columns with variables to stratify the data
#'   by. If omitted the complete dataset is used to estimate a single
#'   prevalence. If included prevalence is estimated separately for each group
#'   defined by these columns
#' @param prior List of parameters specifying the parameters for the the priors
#'   on the population intercept and standard deviations of group-effect terms.
#'   The default value (NULL) uses the following parameters:
#'   \code{list(intercept = list(nu = 3, mu = 0, sigma = 4.0),
#'              group_sd  = list(nu = 3, mu = 0, sigma = 2.5),
#'              individual_sd = FALSE)}
#'   This models the prior of the linear scale intercept as t-distributed with
#'   parameters in `intercept` and the standard deviation of the group-level
#'   effects as truncated (non-negative) t-distribution. `individual_sd = FALSE`
#'   means that this prior is for the root-sum-square of group-effect standard
#'   deviations for models with multiple grouping levels. The default implies a
#'   prior on population prevalence that is approximately distributed as
#'   beta(0.5,0.5). To set custom priors, use the same nested list format. Any
#'   omitted parameters will be replaced with the default values and additional
#'   parameters ignored silently. For example, to change the parameters to be
#'   equal to the defaults for intercept-only random-effect model in
#'   PoolRegBayes you can use:
#'   \code{list(individual_sd = TRUE)},
#'   which puts a prior on each the standard deviations of each of group-level
#'   effects separately, but doesn't change the priors used.
#'
#' @param level The confidence level to be used for the confidence and credible
#'   intervals. Defaults to 0.95 (i.e. 95\% intervals)
#' @param verbose Logical indicating whether to print progress to screen.
#'   Defaults to false (no printing to screen)
#' @param cores The number of CPU cores to be used. By default one core is used
#' @param iter,warmup,chains MCMC options for passing onto the sampling routine.
#'   See \link[rstan]{stan} for details.
#' @param control A named list of parameters to control the sampler's behaviour.
#'   Defaults to default values as defined in \link[rstan]{stan}, except for
#'   \code{adapt_delta} which is set to the more conservative value of 0.9. See
#'   \link[rstan]{stan} for details.
#' @return A \code{data.frame} with columns:
#'   \itemize{\item{\code{PrevBayes} the (Bayesian) posterior expectation}
#'            \item{\code{CrILow} and \code{CrIHigh} -- lower and upper bounds
#'                  for credible intervals}
#'            \item{\code{NumberOfPools} -- number of pools}
#'            \item{\code{NumberPositive} -- the number of positive pools} }
#'   If grouping variables are provided in \code{...} there will be an
#'   additional column for each grouping variable. When there are no grouping
#'   variables (supplied in \code{...}) then the output has only one row with
#'   the prevalence estimates for the whole dataset. When grouping variables are
#'   supplied, then there is a separate row for each group.
#'
#' @example examples/HierPrevalence.R
#' @seealso \code{\link{PoolPrev}}, \code{\link{getPrevalence}}


HierPoolPrev <- function(data,result,poolSize,hierarchy,...,
                         prior = NULL,
                         level = 0.95, verbose = FALSE, cores = NULL,
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
    rplnull <- function(x,replacement){if(is.null(x)){replacement}else{x}}
    sdata <- list(N = nrow(data), #number of datapoints (pools)
                  L = length(hierarchy), #number of levels of hierarchy
                  NumGroups = array(NumGroups), #Number of groups at each level of hierarchy
                  TotalGroups = sum(NumGroups),
                  #Result = array(data$Result), #PERHAPS TRY REMOVING COLUMN NAMES?
                  Result = dplyr::select(data, !! result)[,1] %>% as.matrix %>% as.numeric %>% array, #This seems a rather obscene way to select a column, but other more sensible methods have inexplicible errors when passed to rstan::sampling
                  PoolSize = dplyr::select(data, !! poolSize)[,1] %>% as.matrix %>% array,
                  #G = G, #The group membership for each data point and level of hierarchy
                  Z = Z,
                  # Parameters for t-distributed priors for:
                    #  Intercept
                  InterceptNu    = rplnull(prior$intercept$nu,3),
                  InterceptMu    = rplnull(prior$intercept$mu,0),
                  InterceptSigma = rplnull(prior$intercept$sigma,4),
                    # Standard deviations of group-effects
                  GroupSDNu      = rplnull(prior$group_sd$nu,3),
                  GroupSDMu      = rplnull(prior$group_sd$mu,0),
                  GroupSDSigma   = rplnull(prior$group_sd$sigma,2.5)
                  )
    #return(sdata)
    model <- if(rplnull(prior$inidividual_sd,FALSE)){
      stanmodels$HierPoolPrevIndividualSD
      }else{
        stanmodels$HierPoolPrevTotalSD
      }
    sfit <- rstan::sampling(model,
                            data = sdata,
                            pars = c('Intercept', 'total_group_sd'),
                            chains = chains,
                            iter = iter,
                            warmup = warmup,
                            refresh = ifelse(verbose,200,0),
                            cores = cores,
                            control = control)
    #return(sfit)
    sfit <- as.matrix(sfit)[,c('Intercept', 'total_group_sd')] %>%
      as.data.frame %>%
      dplyr::rowwise() %>%
      dplyr::mutate(p = meanlinknormal(Intercept,total_group_sd,plogis))

    out <- data.frame(PrevBayes = mean(sfit$p))
    out[,'CrILow'] <- stats::quantile(sfit$p,(1-level)/2)
    out[,'CrIHigh'] <- stats::quantile(sfit$p,(1+level)/2)

    out[,'NumberOfPools'] <- sdata$N
    out[,'NumberPositive'] <- sum(sdata$Result)

    out <- out %>%
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
    out <- data %>%
      dplyr::group_modify(function(x,...){
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
                     chains = chains, control = control)
      }) %>%
      as.data.frame()
    ProgBar$tick(1)
  }
  dplyr::tibble(out)
}

