#' Predicting Prevalence from a Mixed or Fixed Effect Logistic Regression with
#' Presence/Absence Tests on Pooled Samples
#'
#' This function works somewhat like a \code{predict} or \code{fitted} generic
#' function returning the model predicted prevalence for a given set of data;
#' however, as the quantity of interest (prevalence) is neither on the response
#' or link scale we do not use either of these generic functions. Further, when
#' the model accounts for the hierarchical structure of the sampling frame (e.g.
#' Region/Village/Site), it is common to want to know the predicted values at
#' each level of sampling (e.g. Prevalence at each region, village or site) so
#' these are calculated automatically. Also to calculate population-level
#' prevalence from a mixed model, random/group effects need to marginalised out
#' to avoid biased estimates. This is performed automatically.
#'
#' @export
#' @param model An object returned by [PoolReg()] or [PoolRegBayes()]
#' @param ... Arguments passed to methods for each class
#' @param newdata The data for which prevalence needs to be estimated/predicted.
#'   If not provided, defaults to using the data used to train the model (i.e.
#'   returns the fitted values of the prevalence)
#' @param re.form A description of which random effects to include in the
#'   prediction. If omitted, an attempt is made to infer from model and data
#'   structure.
#' @param robust Logical. Option when model class is \code{brmsfit}. If 
#'   \code{TRUE} (default) the point estimate of prevalence is the posterior 
#'   median. If \code{FALSE}, the the posterior mean is used instead.
#' @param level Defines the confidence level to be used for the confidence and
#'   credible intervals. Defaults to 0.95 (i.e. 95\% intervals).
#' @param all.negative.pools The kind of point estimate and interval to use when
#'   all pools are negative. Typically ignored unless newdata is NULL. If 
#'   \code{'zero'} (default), uses 0 as the point estimate and lower bound for 
#'   the interval and \code{level} posterior quantile the upper bound of the 
#'   interval. If \code{'consistent'}, result is the same as for the case where 
#'   at least one pool is positive. 
#' @return A \code{list} with at least one field \code{PopulationEffects} and an
#'   additional field for every random/group effect variable. The field
#'   \code{PopulationEffects} contains a \code{data.frame} with the prevalence
#'   estimated based only the fixed/population effects. When the intercept is
#'   the only fixed/population effect, this is just the population mean
#'   (possibly adjusted for random/group effects). When there are group effects
#'   terms, \code{getPrevalence} attempts to order these with respect to
#'   'granularity' and extract the prevalence estimates for these random
#'   effects; e.g. if the random/group effects included are there to account for
#'   a hierarchical sampling frame with levels 'Village' and 'Site' with a
#'   formula like \code{Result ~ Cov1 + Cov2 + (1|Village) + (1|Site)}, then
#'   getPrevalence will be a list of three data frames: estimates for every
#'   combination of covariates, estimates for every combination of covariates
#'   and village, and estimates for every combination of covariates, village,
#'   and site.
#'
#' @details
#'
#' If \code{re.form} is omitted (probably the most common use case)
#' \code{getPrevalence} will test to see if there are any random effect terms in
#' the model formula extracted from the \code{model} object. If not, it just
#' returns the estimates based on population effects. If there are random
#' effects, it tests to see if the random effect variables form a nested
#' hierarchical structure in the data provided. If so, in addition to the
#' estimates based on population effects only, it will estimate at different
#' levels of the nested hierarchical structure in order of increasing
#' granularity. For manual control you can set to NA for population effects
#' only, or a one-sided formula specifying the form of the random effects to
#' include in estimates, or a list of such objects. Any random effects omitted
#' will be marginalised out. For automatically detected nested hierarchical
#' structures this means that higher level estimates marginalise over
#' lower-level random effect; in particular, population level estimates will
#' marginalise over all random effects.
#'
#' @seealso \code{\link{PoolReg}}, \code{\link{PoolRegBayes}}
#'
#' @example examples/LogisticRegression.R


getPrevalence <- function(model,...){
  UseMethod('getPrevalence')
}

#' @rdname getPrevalence
#' @export
getPrevalence.glm <- function(model, newdata = NULL, level = 0.95,...){
  # if newdata is not specified, use the data used to fit the model
  if(is.null(newdata)){
    newdata <- model$data
  }
  # extract the column name of the pool size variable
  PoolSizeName <- attr(model,'PoolSizeName')
  # define the inverse link function based on the link function used in the model
  invlink <- switch(attr(model,'link'),
                    logit = stats::plogis,
                    cloglog = cloglog_inv)
  
  # compute the t-distribution quantile at the specified level of confidence -- used to calculate confidence intervals later
  s <- stats::qt((1-level)/2, df = stats::df.residual(model), lower.tail = FALSE)
  
  # extract the terms used in the model formula
  PopTerms <- attr(stats::terms(model$formula),"term.labels")
  # select corresponding columns from newdata, remove duplicates
  PredData <- newdata[,PopTerms,drop = FALSE] %>% unique
  row.names(PredData) <- NULL
  # add a dummy PoolSizeName column (needed to keep stats::predict happy, but not actually used in computations)
  PredData[,PoolSizeName] <- 1
  # compute the point estimates and standard errors (on the link scale), and transform to prevalence/probability scale
  Prev <- stats::predict(model,
                         newdata = PredData,
                         type = "link",
                         se.fit = TRUE)[1:2] %>%
    as.data.frame()
  Prev <- cbind(PredData[,names(PredData) != PoolSizeName],
                data.frame(
                  # compute prevalence estimates and confidence interval on prevalence/probability scale
                  Estimate = invlink(Prev$fit),
                  CILow = invlink(Prev$fit - s * Prev$se.fit),
                  CIHigh = invlink(Prev$fit + s * Prev$se.fit)
                )
  )
  # return the estimated prevalence and its confidence interval as a named list
  predlist <- list(PopulationEffects = Prev)
  return(predlist)
}

#' @rdname getPrevalence
#' @export
getPrevalence.glmerMod <- function(model, newdata = NULL, re.form = NULL, all.negative.pools = 'zero',...){
  
  if(is.null(newdata)){
    newdata <- attr(model,'frame')
  }
  
  invlink <- switch(attr(model,"link"),
                    logit = stats::plogis,
                    cloglog = cloglog_inv)
  
  formula <- attr(model,'call')$formula
  PoolSizeName <- attr(model,'PoolSizeName')
  
  GroupEffectTerms <- getGroupEffectTerms(formula, newdata, re.form)
  PredData <- preparePredictionData(GroupEffectTerms, newdata, formula, PoolSizeName)
  # When PredDataSub$..allnegative == FALSE, at least one replicate for that set of parameters found prevalence (response = 1)
  # When PredDataSub$..allnegative == TRUE, all replicates for that set of parameters have response of 0
  
  predlist <- list()
  #Make predictions based on group effects
  for(nge in 1:length(GroupEffectTerms)){
    ge <- GroupEffectTerms[[nge]] # The formula for the group effect(s) at this level
    PredDataSub <- PredData[[nge]] # Subset of newdata to make predictions on (only unique combinations of inputs relevant to the level of random effects)
    
    # Linear predictor
    eta <- stats::predict(model,
                          type = 'link',
                          re.form = ge,
                          newdata = PredDataSub)
    
    # The random/group effect terms that we need to marginalise/integrate over
    maringal.ge.terms <- retermdiff(formula,ge)
    
    # Correlation matrices for each grouping variable
    correlations <- lme4::VarCorr(model, summary = FALSE)
    
    # The standard deviations of the random/group effects that are being integrated over
    # Note that the loop adds up the variances so we need to take square roots after
    sds <- vector('numeric', nrow(PredDataSub))
    for(mge in maringal.ge.terms){
      gn <- as.character(mge[3]) #name of grouping variable
      mm <- stats::model.matrix(stats::reformulate(as.character(mge[2])),PredDataSub)
      Sigma <- correlations[[gn]]
      # sds <- sds + diag(mm %*% Sigma %*% t(mm))
      sds <- sds + sapply(1:nrow(mm), # same as the commented out line above but uses far fewer computations since we only need the diagonal terms
                          function(n){d <- mm[n,,drop = FALSE];
                          d %*% Sigma %*% t(d)})
    }
    sds <- sqrt(sds)
    
    # `..zeroest` = TRUE when all.negative.pools == 'zero' AND PredDataSub$`..allnegative` == TRUE
    # Otherwise, `..zeroest` = FALSE
    # Remove `..allnegative` col from Prev (prevents replicate col after bind_cols)
    Prev <- data.frame(Estimate = Vectorize(meanlinknormal)(eta, sds, list(invlink)),
                       ..allnegative = PredDataSub$..allnegative) %>%
      rowwise() %>%
      mutate(..zeroest = (all.negative.pools == 'zero' && .data$..allnegative) ) %>%
      select(!('..allnegative')) 
    
    # Update `Estimate` col using `..zeroest` col (account for all.negative.pools input value)
    pred <- PredDataSub %>%
      bind_cols(Prev) %>%
      mutate(Estimate = ifelse(.data$..zeroest, 0, .data$Estimate)) %>% 
      select(-any_of(c('..zeroest', '..allnegative', PoolSizeName)))
    
    predlist <- c(predlist, list(pred))
  }
  
  names(predlist) <- names(GroupEffectTerms)
  
  return(predlist)
}

#' @rdname getPrevalence
#' @export
getPrevalence.brmsfit <- function(model, newdata = NULL, re.form = NULL,
                                  robust = TRUE, level = 0.95,
                                  all.negative.pools = 'zero',...){
  if (is.null(newdata)) {
    newdata <- model$data
  } else {
    all.negative.pools = 'zero'
  }
  
  formula <- model$formula$formula
  invlink <- switch(model$link,
                    logit = stats::plogis,
                    cloglog = cloglog_inv)
  
  PoolSizeName <- model$PoolSizeName
  
  GroupEffectTerms <- getGroupEffectTerms(formula, newdata, re.form)
  PredData <- preparePredictionData(GroupEffectTerms, newdata, formula, PoolSizeName)
  
  # Correlation matrices for each grouping variable (only for models with random effects)
  if (! is.null(lme4::findbars(formula)) ) {
    correlations <- lme4::VarCorr(model, summary = FALSE)
  }
  
  predlist <- list()
  # Make predictions based on group effects
  for (nge in 1:length(GroupEffectTerms) ) {
    ge <- GroupEffectTerms[[nge]] # The formula for the group effect(s) at this level
    PredDataSub <- PredData[[nge]] # Subset of newdata to make predictions on (only unique combinations of inputs relevant to the level of random effects)
    eta <- stats::fitted(model,
                         scale = 'linear',
                         re_formula = ge,
                         newdata = PredDataSub,
                         summary = FALSE)
    
    ndraw <- brms::ndraws(model) # Number of MCMC draws
    npoint <- ncol(eta) # Number of prediction points
    maringal.ge.terms <- retermdiff(formula,ge) # The random/group effect terms that we need to marginalise/integrate over
    
    # The standard deviations of the random/group effects that are being integrated over
    # Note that the loop adds up the VARIANCES so we need to take square roots after
    sds <- matrix(0,ndraw,npoint) # The standard deviations of the random/group effects that are being integrated over. Note that in the general case these are calculate by summing variances and then taking square roots, so we have to take square roots later
    for (mge in maringal.ge.terms) {
      gn <- as.character(mge[3]) # Name of grouping variable
      mm <- stats::model.matrix(stats::reformulate(as.character(mge[2])),PredDataSub) # Model matrix for random effects
      Sigma <- correlations[[gn]]$cov # Array with first dimension for number of MCMC draws. Each slice is the sampled covariance matrix for random effects for this group (gn)
      if(is.null(Sigma)){# For groups with only a single random effect, no covariance matrix is given so extract sd (and square) instead
        Sigma <- (correlations[[gn]]$sd)^2
        Sigma <- array(Sigma, dim = c(ndraw, 1, 1)) # Change for consistency with other case. By default dim would be c(ndraw, 1)
      }
      
      #sds <- sds + diag(mm %*% Sigma %*% t(mm))
      
      # The goal is to calculate like the above commented out code something like
      # this but for every draw from Sigma. Also we can avoid doing the full
      # matrix computation by iterating over rows of mm, since we only need the
      # diagonal
      for(n in 1:ndraw){
        sigma <- Sigma[n,,]
        for(m in 1:npoint){
          d <- mm[m,,drop = FALSE]
          sds[n,m] <- sds[n,m] + (d %*% sigma %*% t(d))
        }
      }
    }
    sds <- sqrt(sds)
    
    # Calculate prevalence from eta and sd (integrating over selected random effects)
    prev <- matrix(0,ndraw, npoint)
    for (n in 1:ndraw) {
      for (m in 1:npoint) {
        prev[n,m] <- meanlinknormal(eta[n,m], sds[n,m], invlink)
      }
    }
    
    pred <- PredDataSub %>%
      select(-all_of(PoolSizeName)) %>%
      rowwise() %>%
      mutate(..zeroest = (all.negative.pools == 'zero' && .data$..allnegative) )
    
    pred$prev <- t(prev)
    
    pred <- pred %>%
      mutate(Estimate = ifelse(.data$..zeroest,
                               0,
                               ifelse(robust,
                                      stats::median(.data$prev, na.rm = TRUE),
                                      mean(.data$prev, na.rm = TRUE))),
             CrILow = ifelse(.data$..zeroest,
                             0,
                             stats::quantile(.data$prev, 0.5 - level/2)),
             CrIHigh = ifelse(.data$..zeroest,
                              stats::quantile(.data$prev, level),
                              stats::quantile(.data$prev, 0.5 + level/2))) %>%
      select(-c("..allnegative","prev","..zeroest")) %>%
      ungroup()
    
    predlist <- c(predlist,list(pred))
  }
  
  names(predlist) <- names(GroupEffectTerms)
  
  return(predlist)
}

getGroupEffectTerms <- function(formula,d,re.form){
  # Get the the random/group effect terms names
  GroupVarNames <- getGroupVarNames(formula)
  # Number of grouping variables
  NGroupVars <- length(GroupVarNames)
  
  #set up default re.form list - and if an NA or a single formula wrap in a list
  switch(class(re.form),
         NULL = {
           if(!all(GroupVarNames %in% colnames(d))){
             stop('Cannot calculate random effects for variables ',
                  paste(setdiff(GroupVarNames, colnames(d)),collapse = ', '),
                  ' as these have not been provided in `newdata`')
           }
           
           if(NGroupVars >0){
             GroupVarNames <- orderByGranularity(d,GroupVarNames)
           }
           
           if(isNested(d,GroupVarNames)){
             group_effects <- c(list(PopulationEffects = NA),
                                orderedGroupTerms(formula,GroupVarNames))
           }else{
             group_effects <- list(PopulationEffects = NA)
           }
         },
         list = {group_effects <- re.form},
         logical = {group_effects <- list(PopulationEffects = NA)},
         formula = {group_effects <- list(re.form)},
         stop('re.form must be a list of random effect formulas, or NA (for no random effect terms)')
  )
  
  #checks that group effects are valid
  
  for(ge in group_effects){
    if(inherits(ge, 'formula')){
      #check that grouping variables also exist in original formula
      if(!all(getGroupVarNames(ge) %in% GroupVarNames)){
        stop('A grouping factor specified in re.form was not present in original model. Problematic random effect term:',ge)
      }
      #check that all random effect terms existed in original model
      if(length(retermdiff(ge, formula)) > 0){
        stop('A random effect term specified in re.form was not present in original model: ', retermdiff(ge, formula))
      }
    }else{
      if(!is.na(ge)){
        stop('re.form must be a list of random effect formulas, or NA (for no random effect terms)')
      }
    }
  }
  group_effects
}

preparePredictionData <- function(group_effects, data, formula, poolsizename){
  PredData <- list()
  response <- all.vars(stats::update(formula, . ~ 1))[1]
  for(ge in group_effects){
    #The variables used in these group effect(s)
    gev <- if(inherits(ge, 'formula')){
      all.vars(ge)
    }else{
      if(is.na(ge)){
        NULL
      }else{
        stop('re.form must be a list of random effect formulas, or NA (for no random effect terms)')
      }
    }
    
    #Population terms
    AllTerms <- setdiff(all.vars(formula), response)
    PopTerms <- setdiff(AllTerms,getGroupVarNames(formula))
    
    # Prepare data for prediction -- just want unique combinations of the 
    #   relevant variables
    PredDataSub <- data
    # Sets the pool size variable to 1 (not used in calculations but required to 
    #   keep stats::fitted happy)
    PredDataSub[,poolsizename] <- 1 
    
    # Grouping by poolsize (which are all 1) guarantees there is at least one 
    #   row after summarise
    PredDataSub <- PredDataSub %>%
      group_by(pick(c(PopTerms,gev,poolsizename))) 
    
    # Select unique combinations of predictor variables
    if(response %in% colnames(data)){
      # If the response is included in the dataset then additionally annotate 
      #   whether all responses are negative for the rows of each unique 
      #   combinations of predictors. 
      # Useful if all.negative.pools = 'zero' and newdata = NULL
      PredDataSub$..response <- PredDataSub[,response]
      # For each combination of parameters, check the number of positive 
      #   responses
      # The ..allnegative column is TRUE when all rows for that set of 
      #   parameters are 0 (i.e., no prevalence)
      PredDataSub <- summarise(PredDataSub,
                               ..allnegative = sum(.data$..response) == 0)
    }else{
      PredDataSub <- summarise(PredDataSub,
                               ..allnegative = NA)
    }
    
    PredDataSub <- ungroup(PredDataSub)
    
    PredData <- c(PredData, list(PredDataSub))
  }
  PredData
}

orderByGranularity <- function(df,Names = NULL){
  if(is.null(Names)) Names <- colnames(df)
  else if(is.character((Names)) & length(Names) == 0) return(Names)
  N <- length(Names)
  Levels <- rep(0,N)
  for(n in 1:N){
    Levels[n] <- length(unique(df[,Names[n]]))
  }
  Names[order(Levels)]
}

isNested <- function(df, Names){
  N <- length(Names)
  if(N %in% c(0,1)){
    return(TRUE)
  }
  Names <- rev(orderByGranularity(df,Names))
  
  for(n in 1:(N-1)){
    Levels <- unique(df[,Names[n]])
    for(level in Levels){
      if(length(unique(df[df[,Names[n]] == level,Names[n+1]])) > 1){
        return(FALSE)
      }
    }
  }
  return(TRUE)
}

getGroupVarNames <- function(formula){
  n <- length(formula)
  stringr::str_match_all(as.character(formula)[[n]],
                         "\\|([^\\|]*)\\)")[[1]][,2] %>%
    strsplit(split = "[\\/\\+\\:\\*]") %>%
    unlist %>%
    trimws %>%
    unique
}

orderedGroupTerms <- function(formula,vars){
  if(length(vars) == 0 ) return(NULL)
  GroupTerms <- lme4::findbars(formula)
  if(is.null(GroupTerms)) return(NULL)
  NumTerms <- length(GroupTerms)
  NumVars <- length(vars)
  
  FinestVar <- rep(length(vars), NumTerms)
  for(n in 1:NumTerms){
    TermVars <- intersect(all.vars(stats::reformulate(as.character(GroupTerms[n]))),vars)
    FinestVar[n] <- max(match(TermVars,vars))
  }
  
  out <- list()
  m <- 0
  for(n in sort(unique(FinestVar))){
    m <- m + 1
    out[[m]] <- stats::reformulate(paste0("(",as.character(GroupTerms[FinestVar <= n]),")"))
  }
  names(out) <- vars[sort(unique(FinestVar))]
  return(out)
}

#returns a list of random effect terms that are in f1 but are not in f2
retermdiff <- function(f1, f2){
  out <- setdiff(lme4::findbars(f1), lme4::findbars(f2))
  if(is.null(out)){
    out <- list()
  }
  out
}

# Function (not exported) to calculate the mean of a link-normal distributed r.v.
# i.e. the link function of the r.v. is normally distributed (e.g. logit-normal)
# This is called in order to marginalise over random effects e.g. when calculating
# population-level prevalence
meanlinknormal <- function(mu, sigma, invlink){
  if(sigma < 0){
    stop('sigma (sd of normal distribution) must be non-negative')
  } else if(sigma == 0){ #at zero sigma, the link-normal r.v. reduces to a point mass --- no integration required
    return(invlink(mu))
  } else{
    .mean <- try(stats::integrate(function(x){invlink(x) * stats::dnorm(x,mean = mu, sd = sigma)},
                                  lower = -Inf, upper = Inf, abs.tol =  -1)$value,
                 silent = TRUE)
    if(inherits(.mean,'try-error') || .mean * 10 < invlink(mu)){
      .mean <- try(stats::integrate(function(x){invlink(x) * stats::dnorm(x,mean = mu, sd = sigma)},
                                    lower = mu - sigma * 5, upper = mu + sigma * 5, abs.tol = -1)$value,
                   silent = TRUE)
      if(inherits(.mean,'try-error') || .mean * 10 < invlink(mu)){
        warning('integration failed for mu = ', mu, ' and sigma = ', sigma, ', with error: \n',attr(.mean, 'condition')$message, '\nResult will be NA')
        .mean <- NA
      }
    }
    return(.mean)
  }
}


meanvarlinknormal <- function(mu, sigma, invlink){
  .mean <- meanlinknormal(mu, sigma, invlink)
  .var <- meanlinknormal(mu, sigma, \(x){(invlink(x) - .mean)^2})
  return(c(mean = .mean, var = .var))
}

cloglog_inv <- function(x){-expm1(-exp(x))}

ICC <- function(mu, sigma, link, .mean = NULL, method = 'nested'){
  
  if(!is.character(link)){stop('link should be a name of a link function; either logit or cloglog')}
  
  invlink <- switch(link,
                    'logit' = stats::plogis,
                    'cloglog' = cloglog_inv,
                    stop(link, ' is not a supported link'))
  
  #sigma (sd of random effect on linear scale) are assumed ordered from biggest
  #group to smallest. Output ICCs are returned in the same order
  
  sigma_tot <- sqrt(sum(sigma^2))
  L <- length(sigma) # Number of levels in clustering
  
  #prevalence -- possible to provide to avoid recalculation if already calculated
  if(is.null(.mean)){
    .mean <- meanlinknormal(mu, sigma_tot, invlink) 
  }
  
  #variance of group-level prevalence at each level ...
  .var <- numeric(L)
  #...starting with lowest...
  .var[L] <- meanlinknormal(mu, sigma_tot, \(x){(invlink(x) - .mean)^2}) 
  if(L > 1){
    for(l in 1:(L-1)){ #...then the remaining levels
      sigma_l <- sqrt(sum(sigma[(l+1):L]^2)) # sqrt of total variance at lower levels
      sigma_h <- sqrt(sum(sigma[1:l]^2))     # sqrt of total variance at higher levels
      
      # if(method == 'triple'){
      #   sgm <- c(sigma_l, sigma_l, sigma_h)
      #   .var[l] <- cubature::hcubature(\(x){
      #     invlink(x[1] + x[3] + mu) *
      #       invlink(x[2] + x[3] + mu) *
      #       prod(stats::dnorm(x,sd = sgm))},
      #     lowerLimit = sgm * -10,
      #     upperLimit = sgm * 10
      #   )$integral - .mean^2
      # }
      if(method == 'nested'){
        .var[l] <- meanlinknormal(0, sigma_h,
                                  \(x){(Vectorize(meanlinknormal, 'mu')(mu + x, sigma_l,invlink) - .mean)^2})
      }
      if(method == 'approx'){
        # similar to 'nested' except the mean of link normal is approximated by
        # function related to the link with a scaling. Calculating the scaling
        # requires computing the mean link normal once, but avoids repeated
        # computations. This approximation is reasonable for logitnormal, but
        # pretty poor for cloglog. See test/ApproximatingMeanLinkNormal.R for a
        # more complicated approximation which works ok for cloglog and logit
        
        linkf <- switch(link,
                        'logit' = stats::qlogis,
                        stop(link, " is not a supported link for method = 'approx'"))                       
        
        scaling <- linkf(meanlinknormal(sigma_l, sigma_l, invlink))/sigma_l
        .var[l] <- meanlinknormal(0, sigma_h,
                                  \(x){(invlink((mu + x) * scaling) - .mean)^2})
      }
    }
  }
  icc <- .var/ (.mean * (1 - .mean))
  if(any(icc<0) | any(icc>1) | is.unsorted(icc)){ #check for numerical issues
    if(method == 'approx'){# if using the approx method try using
      return(ICC(mu, sigma, link, .mean, method = 'nested'))
    }else{
      wrng <- paste('There has been a numeric integration error.',
                    ' The calculated ICC =', paste(icc, collapse = ', '), ' for inputs',
                    ' mu = ', mu,
                    ' sigma = ', paste(sigma, collapse = ', '),
                    ' and link = ', link)
      if(any(icc<0) | any(icc>1)){
        wrng <- paste0(wrng, '. ICC must be 0<=ICC<=1.')
      }
      if(is.unsorted(icc)){
        wrng <- paste0(wrng, '. ICC must increase as you move from higher to lower sampling levels.')
      }
      warning(wrng)
    }
  }
  icc
}

