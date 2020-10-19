#' Predicting Prevalence from a Mixed or Fixed Effect Logistic
#' Regression with Presence/Absence Tests on Pooled Samples
#'
#' This function works somewhat like a \code{predict} or \code{fitted} generic
#' function returning the model predicted prevalence for a given set of data;
#' however, as the quantity of interest (prevalence) is neither on the response
#' or link scale we do not use either of these generic functions. Further, when
#' the model accounts for the heirarchical structure of the sampling frame (e.g.
#' Region/Village/Site), it is common to want to know the predicted values at
#' each level of sampling (e.g. Prevalence at each region, village or site) so
#' these are calculated automatically.
#'
#' @export
#' @param model An object return by [PoolReg()] or [PoolRegBayes()]
#' @param data The data for which prevalence needs to be estimated/predicted. If
#'   not provided, defaults to using the data used to train the model (i.e.
#'   returns the fitted values of the prevalence)
#' @return A \code{list} with at least one field \code{PopulationEffects} and an
#'   additional field for every random/group effect variable. The field
#'   \code{PopulationEffects} contains a \code{data.frame} with the prevalence
#'   estimated based only the fixed/population effects. When the intercept is
#'   the only fixed/popultion effect, this is just the population mean (possibly
#'   adjusted for random/group effects). When there are group effects
#'
#' @seealso [PoolReg()] and [PoolRegBayes()]
#' @example examples/LogisticRegression.R

getPrevalence <- function(model, data = NULL){
  out <- switch(class(model)[1],
         brmsfit = getPrevalence.brmsfit(model, data),
         glm = getPrevalence.glm(model, data),
         glmerMod = getPrevalence.glmerMod(model, data),
         stop('The provided model must be the output of either PoolReg or PoolRegBayes'))
  out
}

getPrevalence.glm <- function(model, data = NULL){
  if(is.null(data)){
    data <- model$data
  }

  s <- stats::qt(0.025, df = stats::df.residual(model), lower.tail = FALSE)

  PopTerms <- attr(stats::terms(model$formula),"term.labels")
  PredData <- data[,PopTerms,drop = FALSE] %>% unique
  row.names(PredData) <- NULL
  Prev <- stats::predict(model,
                  newdata = PredData,
                  type = "link",
                  se.fit = TRUE)[1:2] %>%
    as.data.frame()
  Prev <- cbind(PredData,
                data.frame(
                  Estimate = stats::plogis(Prev$fit),
                  CILow = stats::plogis(Prev$fit - s * Prev$se.fit),
                  CIHigh = stats::plogis(Prev$fit + s * Prev$se.fit)
                  )
                )
  predlist <- list(PopulationEffects = Prev)
  return(predlist)
}

getPrevalence.glmerMod <- function(model, data = NULL){

  if(is.null(data)){
    data <- attr(model,'frame')
  }

  formula <- attr(model,'call')$formula

  GroupTermNames <- (as.character(formula)[[3]] %>%
                       stringr::str_match_all("\\|\\s*(.*?)\\s*\\)"))[[1]][,2] %>%
    strsplit(split = "\\/") %>%
    unlist

  GroupTerms <- lme4::findbars(formula)

  NGroupTerms <- length(GroupTermNames)

  AllTerms <- intersect(colnames(data),
                        unique(c(GroupTermNames, attr(stats::terms(formula),"term.labels"))))
  PopTerms <- setdiff(AllTerms,GroupTermNames)

  #Datapoints to make prediction at
  PredData <- data[,AllTerms, drop = FALSE] %>% unique

  #Make predictions based only on population effects - there might not be any in which case we feed in null data
  if(length(PopTerms)){
    PredDataSub <- PredData[,PopTerms, drop = FALSE] %>% unique()
  }else{
    PredDataSub <- data.frame(DummyVar = 1)
  }
  rownames(PredDataSub) <- NULL
  Prev <- stats::predict(model,
                 type = 'link',
                 re.form = NA,
                 newdata = PredDataSub) %>%
    stats::plogis() %>%
    as.data.frame
  colnames(Prev) <- c("Estimate")
  if(length(PopTerms)){
    predlist <- list(cbind(PredDataSub,Prev))
  }else{
    predlist <- list(cbind(PredDataSub[,!names(PredDataSub) == "DummyVar"],Prev))
  }

  #Make predictions based on group effects
  if(NGroupTerms){
    for(n in 1:NGroupTerms){
      PredDataSub <-  PredData[,c(PopTerms,GroupTermNames[1:n]),drop =F] %>% unique
      rownames(PredDataSub) <- NULL
      GroupEffectForm <- stats::reformulate(paste0("(",as.character(GroupTerms[(NGroupTerms-n+1):NGroupTerms]),")"))
      Prev = stats::predict(model,
                     type = 'link',
                     re.form = GroupEffectForm,
                     newdata = PredDataSub) %>%
        stats::plogis() %>%
        as.data.frame
      colnames(Prev) <- "Estimate"
      predlist <- c(predlist,
                    list(cbind(PredDataSub,Prev)))
    }
  }
  names(predlist) <- c("PopulationEffects", GroupTermNames)

  return(predlist)
}

getPrevalence.brmsfit <- function(model, data = NULL){
  if(is.null(data)){
    data <- model$data
  }
  formula <- model$formula$pforms$eta
  PoolSizeName <- sub(".*\\^", "",as.character(model$formula$formula)[[3]]) #The name of the pool size variable in data
  GroupTermNames <- (as.character(formula)[[3]] %>%
                       stringr::str_match_all("\\|\\s*(.*?)\\s*\\)"))[[1]][,2] %>%
    strsplit(split = "\\/") %>%
    unlist
  NGroupTerms <- length(GroupTermNames)

  AllTerms <- intersect(colnames(data),
                        unique(c(GroupTermNames, attr(stats::terms(formula),"term.labels"))))
  PopTerms <- setdiff(AllTerms,GroupTermNames)

  #Datapoints to make prediction at
  PredData <- data %>% dplyr::select_at(AllTerms) %>% unique

  #Make predictions based only on population effects - there might not be any in which case we feed in null data
  if(length(PopTerms)){
    PredDataSub <- PredData %>% dplyr::select_at(PopTerms) %>% unique()
  }else{
    PredDataSub <- data.frame(DummyVar = 1)
  }
  PredDataSub[,PoolSizeName] = 1 #Setting PoolSize to 1 means that the response scale is the same as the prevalence scale
  rownames(PredDataSub) <- NULL
  Prev <- stats::fitted(model,
                 scale = 'response',
                 re_formula = NA,
                 newdata = PredDataSub) %>%
    as.data.frame %>%
    dplyr::select(-dplyr::one_of(c("Est.Error","DummyVar",PoolSizeName))) %>%
    stats::setNames(c("Estimate", "CrILow", "CrIHigh"))

  predlist <- list(cbind(PredDataSub,Prev))

  #Make predictions based on group effects
  if(NGroupTerms){
    for(n in 1:NGroupTerms){
      PredDataSub <-  PredData[,c(PopTerms,GroupTermNames[1:n]),drop =F] %>% unique
      PredDataSub[,PoolSizeName] = 1 #Setting PoolSize to 1 means that the response scale is the same as the prevalence scale
      rownames(PredDataSub) <- NULL
      GroupEffectForm <- paste("(1|",GroupTermNames[1:n] %>% paste(collapse = "/"),")",sep = "") %>% stats::reformulate()
      Prev = stats::fitted(model,
                    scale = 'response',
                    re_formula = GroupEffectForm,
                    newdata = PredDataSub) %>%
        as.data.frame %>%
        dplyr::select(-dplyr::one_of(c("Est.Error",PoolSizeName))) %>%
        stats::setNames(c("Estimate", "CrILow", "CrIHigh"))
      predlist <- c(predlist,
                    list(cbind(PredDataSub,Prev)))
    }
  }
  names(predlist) <- c("PopulationEffects", GroupTermNames)

  return(predlist)
}






