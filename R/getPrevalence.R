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
#' @param newdata The data for which prevalence needs to be estimated/predicted.
#'   If not provided, defaults to using the data used to train the model (i.e.
#'   returns the fitted values of the prevalence)
#' @param re.form A description of which random effects to include in the
#'   prediction. If omitted, getPrevalence automatically tests to see if there
#'   are any random effect terms. If not, it just returns the estimates based on
#'   population effects. If there are random effects, it tests to see if the
#'   random effect variables form a nested hierarchical structure. If so, in
#'   addition to the estimates based on population effects only, it will
#'   estimate at different levels of the nested hierarchical structure in order
#'   of increasing granularity. For manual control you can set to NA for
#'   population effects only, or a one-sided formula specifying the form of the
#'   random effects to include in estimates, or a list of such objects. Any
#'   random effects omitted will be marginalised out. For automatically detected
#'   nested hierarchical structures this means that higher level estimates
#'   marginalise over lower-level random effect; in particular, population level
#'   estimates will marginalise over all random effects.
#' @param robust Currently only relevant for brmsfit objects (returned by
#'   PoolRegBayes). If \code{FALSE} (default) the point estimate of prevalence
#'   is the mean over the posterior. If \code{TRUE}, the median over the
#'   posterior is used instead.
#' @param level Defines the confidence level to be used for the confidence and
#'   credible intervals. Defaults to 0.95 (i.e. 95\% intervals).
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
#' @seealso
#'   \code{\link{PoolReg}},
#'    \code{\link{PoolRegBayes}}
#'
#' @example examples/LogisticRegression.R

getPrevalence <- function(model, newdata = NULL, re.form = NULL, robust = FALSE, level = 0.95){
  #I should just make this a proper generic function
  out <- switch(class(model)[1],
                brmsfit = getPrevalence.brmsfit(model, newdata, re.form, robust, level),
                glm = getPrevalence.glm(model, newdata, level),
                glmerMod = getPrevalence.glmerMod(model, newdata, re.form),
                stop('The provided model must be the output of either PoolReg or PoolRegBayes'))
  out
}

getPrevalence.glm <- function(model, newdata = NULL, level = 0.95){
  # if newdata is not specified, use the data used to fit the model
  if(is.null(newdata)){
    newdata <- model$data
  }
  # extract the column name of the pool size variable
  PoolSizeName <- attr(model,'PoolSizeName')
  # define the inverse link function based on the link function used in the model
  invlink <- switch(attr(model,'link'),
                    logit = stats::plogis,
                    cloglog = function(x){-expm1(-exp(x))})

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


getPrevalence.glmerMod <- function(model, newdata = NULL, re.form = NULL){

  if(is.null(newdata)){
    newdata <- attr(model,'frame')
  }

  invlink <- switch(attr(model,"link"),
                    logit = stats::plogis,
                    cloglog = function(x){-expm1(-exp(x))})

  formula <- attr(model,'call')$formula
  PoolSizeName <- attr(model,'PoolSizeName')

  GroupEffectTerms <- getGroupEffectTerms(formula, newdata, re.form)
  PredData <- preparePredictionData(GroupEffectTerms, newdata, formula, PoolSizeName)

  predlist <- list()
  #Make predictions based on group effects
  for(nge in 1:length(GroupEffectTerms)){
    ge <- GroupEffectTerms[[nge]] #The formula for the group effect(s) at this level
    PredDataSub <- PredData[[nge]] #Subset of newdata to make preditions on (only unique combinations of inputs relevant to the level of random effects)

    #linear predictor
    eta <- stats::predict(model,
                          type = 'link',
                          re.form = ge,
                          newdata = PredDataSub)

    #the random/group effect terms that we need to marginalise/integrate over
    maringal.ge.terms <- retermdiff(formula,ge)

    #correlation matrices for each grouping variable
    correlations <- lme4::VarCorr(model, summary = FALSE)

    #the standard deviations of the random/group effects that are being integrated over
    #Note that the loop adds up the variances so we need to take square roots after
    sds <- vector('numeric', nrow(PredDataSub))
    for(mge in maringal.ge.terms){
      gn <- as.character(mge[3]) #name of grouping variable
      mm <- stats::model.matrix(stats::reformulate(as.character(mge[2])),PredDataSub)
      Sigma <- correlations[[gn]]
      #sds <- sds + diag(mm %*% Sigma %*% t(mm))
      sds <- sds + sapply(1:nrow(mm), #same as the commented out line above but uses far fewer computations since we only need the diagonal terms
                          function(n){d <- mm[n,,drop = FALSE];
                          d %*% Sigma %*% t(d)})
    }
    sds <- sqrt(sds)

    Prev <- data.frame(Estimate = Vectorize(meanlinknormal)(eta, sds, list(invlink)))

    pred <- cbind(PredDataSub[,!names(PredDataSub) %in% c("DummyVar", PoolSizeName), drop = FALSE],
                  Prev)
    predlist <- c(predlist, list(pred))
  }

  names(predlist) <- names(GroupEffectTerms)

  return(predlist)
}

getPrevalence.brmsfit <- function(model, newdata = NULL, re.form = NULL,
                                  robust = FALSE, level = 0.95){
  if(is.null(newdata)){
    newdata <- model$data
  }

  formula <- model$formula$formula
  invlink <- switch(model$link,
                    logit = stats::plogis,
                    cloglog = function(x){-expm1(-exp(x))})

  PoolSizeName <- model$PoolSizeName

  GroupEffectTerms <- getGroupEffectTerms(formula, newdata, re.form)
  PredData <- preparePredictionData(GroupEffectTerms, newdata, formula, PoolSizeName)

  #correlation matrices for each grouping variable (only for models with random effects)
  if(!is.null(lme4::findbars(formula))){
    correlations <- lme4::VarCorr(model, summary = FALSE)
  }

  predlist <- list()
  #Make predictions based on group effects
  for(nge in 1:length(GroupEffectTerms)){
    ge <- GroupEffectTerms[[nge]] #The formula for the group effect(s) at this level
    PredDataSub <- PredData[[nge]] #Subset of newdata to make predictions on (only unique combinations of inputs relevant to the level of random effects)
    eta <- stats::fitted(model,
                         scale = 'linear',
                         re_formula = ge,
                         newdata = PredDataSub,
                         summary = FALSE)

    ndraw <- brms::ndraws(model) #number of MCMC draws
    npoint <- ncol(eta) # number of prediction points
    #the random/group effect terms that we need to marginalise/integrate over
    maringal.ge.terms <- retermdiff(formula,ge) #the random/group effect terms that we need to marginalise/integrate over

    #the standard deviations of the random/group effects that are being integrated over
    #Note that the loop adds up the variances so we need to take square roots after
    sds <- matrix(0,ndraw,npoint) #the standard deviations of the random/group effects that are being integrated over. Note that in the general case these are calculate by summing variances and then taking square roots, so we have to take square roots later
    for(mge in maringal.ge.terms){
      gn <- as.character(mge[3]) #name of grouping variable
      mm <- stats::model.matrix(stats::reformulate(as.character(mge[2])),PredDataSub) #model matrix for random effects
      Sigma <- correlations[[gn]]$cov #array with first dimension for number of MCMC draws. Each slice is the sampled covariance matrix for random effects for this group (gn)
      if(is.null(Sigma)){#for groups with only a single random effect, no covariance matrix is given so extract sd (and square) instead
        Sigma <- (correlations[[gn]]$sd)^2
        Sigma <- array(Sigma, dim = c(ndraw, 1, 1)) #change for consistency with other case. By default dim would be c(ndraw, 1)
      }

      #sds <- sds + diag(mm %*% Sigma %*% t(mm)) # the goal is to calculate something like this but for every draw of Sigma. Also we can avoid doing the full matrix computation by iterating over rows of mm, since we only need to the diagonal
      for(n in 1:ndraw){
        sigma <- Sigma[n,,]
        for(m in 1:npoint){
          d <- mm[m,,drop = FALSE]
          sds[n,m] <- sds[n,m] + (d %*% sigma %*% t(d))
        }
      }
    }
    sds <- sqrt(sds)
    #calculate prevalence from eta and sd (integrating over selected random effects)
    prev <- matrix(0,ndraw, npoint)
    for(n in 1:ndraw){
      for(m in 1:npoint){
        prev[n,m] <- meanlinknormal(eta[n,m], sds[n,m], invlink)
      }
    }

    prev.interval <- prev %>%
      apply(2, function(x)stats::quantile(x, probs = 0.5 + c(-level,level)/2, na.rm = TRUE)) %>%
      t %>% as.data.frame() %>%
      stats::setNames(c("CrILow", "CrIHigh"))
    pred <- cbind(PredDataSub[,names(PredDataSub) != PoolSizeName, drop = FALSE],
                  Estimate = apply(prev, 2, ifelse(robust, stats::median, mean),na.rm = TRUE),
                  prev.interval)

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

preparePredictionData <- function(group_effects, d, formula, PoolSizeName){
  PredData <- list()
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
    AllTerms <- setdiff(all.vars(formula), all.vars(formula[[2]]))
    PopTerms <- setdiff(AllTerms,getGroupVarNames(formula))

    #Prepare data for prediction -- just want unique combinations of the relevant variables
    PredDataSub <-  d
    PredDataSub[,PoolSizeName] <- 1 #Sets PoolSize variables to 1 (not used in calculations but required to keep stats::fitted happy)
    PredDataSub <- PredDataSub[,unique(c(PopTerms,gev,PoolSizeName)),drop = FALSE] %>%
      unique
    rownames(PredDataSub) <- NULL
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
    invlink(mu)
  } else{
    integral <- try(stats::integrate(function(x){invlink(x) * stats::dnorm(x,mean = mu, sd = sigma)},
                                     lower = -Inf, upper = Inf)$value,
                    silent = TRUE)
    if(class(integral) == 'try-error'){

      integral <- try(stats::integrate(function(x){invlink(x) * stats::dnorm(x,mean = mu, sd = sigma)},
                                       lower = mu - sigma * 10, upper = mu + sigma * 10)$value,
                      silent = TRUE)
      if(class(integral) == 'try-error'){
        warning('integration failed for mu = ', mu, ' and sigma = ', sigma, ', with error: \n',attr(integral, 'condition')$message, '\nResult will be NA')
        integral <- NA
      }
    }
    integral
  }
}

