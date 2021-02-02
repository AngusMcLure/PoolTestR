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
#' these are calculated automatically.
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
#'   random effects to include in estimates, or a list of such objects.
#' @return A \code{list} with at least one field \code{PopulationEffects} and an
#'   additional field for every random/group effect variable. The field
#'   \code{PopulationEffects} contains a \code{data.frame} with the prevalence
#'   estimated based only the fixed/population effects. When the intercept is
#'   the only fixed/population effect, this is just the population mean (possibly
#'   adjusted for random/group effects). When there are group effects terms,
#'   \code{getPrevalence} attempts to order these with respect to 'granularity'
#'   and extract the prevalence estimates for these random effects; e.g. if the
#'   random/group effects included are there to account for a hierarchical
#'   sampling frame with levels 'Village' and 'Site' with a formula like
#'   \code{Result ~ Cov1 + Cov2 + (1|Village/Site)}, then getPrevalence will be
#'   a list of three data frames: estimates for every combination of covariates,
#'   estimates for every combination of covariates and village, and estimates
#'   for every combination of covariates, village, and site.
#'
#' @seealso [PoolReg()] and [PoolRegBayes()]
#' @example examples/LogisticRegression.R

getPrevalence <- function(model, newdata = NULL, re.form = NULL){
  out <- switch(class(model)[1],
                brmsfit = getPrevalence.brmsfit(model, newdata, re.form),
                glm = getPrevalence.glm(model, newdata),
                glmerMod = getPrevalence.glmerMod(model, newdata, re.form),
                stop('The provided model must be the output of either PoolReg or PoolRegBayes'))
  out
}

getPrevalence.glm <- function(model, newdata = NULL){
  if(is.null(newdata)){
    newdata <- model$data
  }
  PoolSizeName <- attr(model,'PoolSizeName')
  invlink <- switch(attr(model,'link'),
                    logit = stats::plogis,
                    cloglog = function(x){1-exp(-exp(x))})

  s <- stats::qt(0.025, df = stats::df.residual(model), lower.tail = FALSE)

  PopTerms <- attr(stats::terms(model$formula),"term.labels")
  PredData <- newdata[,PopTerms,drop = FALSE] %>% unique
  row.names(PredData) <- NULL
  PredData[,PoolSizeName] <- 1
  Prev <- stats::predict(model,
                         newdata = PredData,
                         type = "link",
                         se.fit = TRUE)[1:2] %>%
    as.data.frame()
  Prev <- cbind(PredData[,names(PredData) != PoolSizeName],
                data.frame(
                  Estimate = invlink(Prev$fit),
                  CILow = invlink(Prev$fit - s * Prev$se.fit),
                  CIHigh = invlink(Prev$fit + s * Prev$se.fit)
                )
  )
  predlist <- list(PopulationEffects = Prev)
  return(predlist)
}

getPrevalence.glmerMod <- function(model, newdata = NULL, re.form = NULL){

  if(is.null(newdata)){
    newdata <- attr(model,'frame')
  }

  invlink <- switch(attr(model,"link"),
                    logit = stats::plogis,
                    cloglog = function(x){1-exp(-exp(x))})

  formula <- attr(model,'call')$formula
  PoolSizeName <- attr(model,'PoolSizeName')

  #Get the the random/group effect terms names
  GroupVarNames <- getGroupVarNames(formula)
  #Order group terms in order of the number of unique values (i.e. coarsest first)
  GroupVarNames <- orderByGranularity(newdata,GroupVarNames)
  NGroupVars <- length(GroupVarNames)

  #set up default re.form list - and if an NA or a single formula wrap in a list
  switch(class(re.form),
         NULL = {
           if(isNested(newdata,GroupVarNames)){
             GroupTerms <- lme4::findbars(formula)
             re.form <- c(list(PopulationEffects = NA),
                          orderedGroupTerms(formula,GroupVarNames))
           }else{
             re.form <- list(PopulationEffects = NA)
           }
         },
         list = {},
         logical = {re.form <- list(PopulationEffects = NA)},
         formula = {re.form <- list(re.form)},
         stop('re.form must be a list of random effect formulas, NA (for no random effect terms)')
  )

  AllTerms <- setdiff(all.vars(formula), as.character(formula[[2]]))
  PopTerms <- setdiff(AllTerms,GroupVarNames)

  predlist <- list()
  #Make predictions based on group effects
  for(n in 1:length(re.form)){
    re <- re.form[[n]]
    if(inherits(re, 'formula')){
      SubGroupVarNames <- all.vars(re)
    }else{
      if(is.na(re)){
        SubGroupVarNames <- NULL
      }else{
        stop('re.form must be a list of random effect formulas or NA (for no random effect terms)')
      }
    }

    #We just want to predict for unique combinations of the relevant variables
    PredDataSub <-  newdata[,unique(c(PopTerms,SubGroupVarNames)),drop = FALSE] %>%
      dplyr::mutate(DummyVar = 1) %>% #guarantees that the PredDataSub is non-empty
      unique
    rownames(PredDataSub) <- NULL
    PredDataSub[,PoolSizeName] <- 1
    Prev <- stats::predict(model,
                           type = 'link',
                           re.form = re,
                           newdata = PredDataSub) %>%
      invlink() %>%
      as.data.frame
    colnames(Prev) <- "Estimate"
    predlist[[n]] <- cbind(PredDataSub[,!names(PredDataSub) %in% c("DummyVar", PoolSizeName), drop = FALSE],
                           Prev)
  }

  names(predlist) <- names(re.form)

  return(predlist)
}

getPrevalence.brmsfit <- function(model, newdata = NULL, re.form = NULL){
  if(is.null(newdata)){
    newdata <- model$data
  }
  formula <- model$formula$pforms$eta
  PoolSizeName <- model$PoolSizeName

  #Get the the random/group effect terms names
  GroupVarNames <- getGroupVarNames(formula)
  #Order group terms in order of the number of unique values (i.e. coarsest first)
  NGroupVars <- length(GroupVarNames)
  if(NGroupVars >0){
    GroupVarNames <- orderByGranularity(newdata,GroupVarNames)
  }

  #set up default re.form list - and if an NA or a single formula wrap in a list
  switch(class(re.form),
         NULL = {
           if(isNested(newdata,GroupVarNames)){
             GroupTerms <- lme4::findbars(formula)
             re.form <- c(list(PopulationEffects = NA),
                          orderedGroupTerms(formula,GroupVarNames))
           }else{
             re.form <- list(PopulationEffects = NA)
           }
         },
         list = {},
         logical = {re.form <- list(PopulationEffects = NA)},
         formula = {re.form <- list(re.form)},
         stop('re.form must be a list of random effect formulas, NA (for no random effect terms)')
  )

  AllTerms <- setdiff(all.vars(formula), as.character(formula[[2]]))
  PopTerms <- setdiff(AllTerms,GroupVarNames)

  predlist <- list()
  #Make predictions based on group effects
  for(n in 1:length(re.form)){
    re <- re.form[[n]]
    if(inherits(re, 'formula')){
      SubGroupVarNames <- all.vars(re)
    }else{
      if(is.na(re)){
        SubGroupVarNames <- NULL
      }else{
        stop('re.form must be a list of random effect formulas or NA (for no random effect terms)')
      }
    }

    #We just want to predict for unique combinations of the relevant variables
    PredDataSub <-  newdata[,unique(c(PopTerms,SubGroupVarNames)),drop = FALSE] %>%
      dplyr::mutate(DummyVar = 1) %>% #guarantees that the PredDataSub is non-empty
      unique
    rownames(PredDataSub) <- NULL
    PredDataSub[,PoolSizeName] <- 1
    Prev <- stats::fitted(model,
                         scale = 'response',
                         re_formula = re,
                         newdata = PredDataSub) %>%
      as.data.frame %>%
      dplyr::select(-dplyr::any_of(c("Est.Error"))) %>%
      stats::setNames(c("Estimate", "CrILow", "CrIHigh"))
    predlist[[n]] <- cbind(PredDataSub[,!names(PredDataSub) %in% c("DummyVar", PoolSizeName), drop = FALSE],
                           Prev)
  }

  names(predlist) <- names(re.form)

  return(predlist)
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
                         "\\|\\s*(.*?)\\s*\\)")[[1]][,2] %>%
    strsplit(split = "[\\/\\+\\:\\*]") %>%
    unlist %>%
    trimws
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

